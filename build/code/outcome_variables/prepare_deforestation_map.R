###########################################################################################################
#                                                                                                         #
#     Preparation of annual maps of forest loss where oil palm plantations stood in 2015                  #
#                                                                                                         #
#     Inputs: - georeferenced mills (from georeferencing works)                                           #
#             ---> IBS_UML_cs.dta                                                             #
#                                                                                                         #
#             - oil palm plantations; already processed 2015 map from Austin here                         #
#             ---> new_oilpalm_2015_WGS1984.tif                                                           #
#                                                                                                         #
#             - Global Forest Change rasters downloaded on internet.                                      #
#                                                                                                         #
#     Output: 17 rasters of oil palm-imputable deforestation defo_1.tif to defo_17.tif                    #
#                                                                                                         #
###########################################################################################################


#THIS SCRIPT'S STRUCTURE
############################################################################################################
  # DEFINE AREA OF INTEREST (AOI)
  # DOWNLOAD APPROPRIATE HANSEN DEFORESTATION DATA
  # THRESHOLD GFC DATA AT 25%, 50% and 75% PIXEL COVER
  # ALIGN PLANTATION MAPS ON FOREST LOSS MAPS
  # COMPUTE FOREST LOSS IMPUTABLE TO PALM OIL
  # PROJECT PALM-IMPUTABLE DEFORESTATION MAP
  # SPLIT THE SINGLE LAYER defo RASTER INTO ANNUAL LAYERS.
############################################################################################################

rm(list = ls())


### PACKAGES

# sf need to be installed from source for lwgeom te be installed from source. 
if (!require(sf)) install.packages("sf", source = TRUE)
#"plyr", 
neededPackages = c("dplyr", "raster", "foreign", "sp", "lwgeom", "rnaturalearth", "data.table",
                   "rgdal", "readstata13",
                   "rlist", "velox", "parallel", "foreach", "iterators", "doParallel", "readxl", "here")
allPackages    = c(neededPackages %in% installed.packages()[ , "Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
  missingIDX = which(allPackages == FALSE)
  needed     = neededPackages[missingIDX]
  lapply(needed, install.packages)
}

# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)
library(sf)

# install other packages not from source.
if (!require(devtools)) install.packages("devtools")
library(devtools)
  
  # package tictoc
  install_github("jabiru/tictoc")
  library(tictoc)

  #install.packages("sf", source = TRUE)
  #if (!require(devtools)) install.packages("devtools")
  #devtools::install_github("r-spatial/lwgeom")
  #library(lwgeom)

  #INSTALL GFC ANALYSIS
  # Install the snow package used to speed spatial analyses
  if (!require(snow)) install.packages("snow")
  library(snow)
  # Install Alex's gfcanalysis package
  if (!require(gfcanalysis)) install.packages('gfcanalysis')
  library(gfcanalysis)

  ### local working directory, just to shorten calls.
  setwd(here("build/input/outcome_variables"))
  
  
############################################################################################################
#   Rather than calling all relevant gfc tiles, mosaicing, and masking with catchment areas, we will first 
#   define an AOI corresponding to catchment areas (CAs) and load only Hansen's maps that cover them.     
#   BUT, the extract_gfc returns data for larger areas than the only AOI provided (we could see Malaysia).
#   We do it this way still. 
#
#   We don't use gfc_stats because we want to keep information at the pixel level and not at the aoi's in order 
#   to overlay it with plantations. 
############################################################################################################

  
##### DEFINE AREA OF INTEREST (AOI) #####
############################################################################################################

# read data.frame of cross-sectional mills with their coordinates. 
mills <- read.dta13(here("build/input/IBS_UML_cs.dta"))  

#turn into an sf object. 
mills <- st_as_sf(mills,	coords	=	c("lon",	"lat"), crs=4326)
class(mills) # "sf" "data.frame"

# set CRS and project
st_crs(mills) 
# EPSG 4326 - proj4string: "+proj=longlat +datum=WGS84 +no_defs" i.e. unprojected because of crs argument in st_as_sf above. 

#   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
#   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator. 
#   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is 
#   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0 
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

mills_prj <- st_transform(mills, crs = indonesian_crs) 
st_crs(mills_prj) # units are meters. 

#define big catchment areas to have a large AOI. 
mills_ca60 <- st_buffer(mills_prj, dist = 60000)

aois <- st_geometry(mills_ca60)


### 
# CODE TO extract_gfc USING to_UTM = FALSE ON A BOUNDING BOX OF CAs
AOI <- st_as_sfc(st_bbox(aois))

#give the aoi the same (unprojected) crs than the tiles (which extract_gfc would have done automatically anyway).

AOI_unprj <- st_transform(AOI, crs = 4326)

#convert the box to a SpatialPolygon object for compatibility with download_tiles methods. 
AOI_sp <- as(AOI_unprj, Class="Spatial")
rm(AOI_unprj, AOI, aois, mills_ca60, mills, mills_prj)
###


### 
#CODE IF ONE WANTED TO RATHER extract_gfc USING to_UTM = TRUE ON A DISSOLVED POLYGON OF CAs
#   AOI <- st_union(aois)
#   #Note: let us not unproject so that the extract_gfc can directly reproject its output onto the UTM of AOI. 
#
#   AOI_sp <- as(AOI, "Spatial")
#   rm(aois, mills, mills_prj)
###

#########################################################################################################################



##### DOWNLOAD APPROPRIATE HANSEN DEFORESTATION DATA #####
#########################################################################################################################
  
#define where all tiles are going to be stored
data_folder <- paste0(getwd(), "/GFC_tiles")

#Calculate tiles needed to cover the AOI
tiles <- calc_gfc_tiles(AOI_sp)
print(length(tiles)) # 12 

#download tiles, with all layers otherwise later extract does not work 
download_tiles(tiles, data_folder, images = c("treecover2000", "lossyear", "gain", "datamask"), dataset = "GFC-2018-v1.6")

# extract gfc data (can only extract all layers with default stack=change)
# to better understand extract_gfc see https://rdrr.io/cran/gfcanalysis/src/R/extract_gfc.R
extract_gfc(AOI_sp, data_folder, 
            stack = "change", 
            to_UTM = FALSE, 
            dataset = "GFC-2018-v1.6", 
            filename = "gfc_data.tif", 
            overwrite = TRUE )
###
# to extract and project in the same time (AOI_sp should be projected) but did not work, I don't know why. 
# extract_gfc(AOI_sp, data_folder, stack = "change", to_UTM = TRUE, 
# dataset = "GFC-2017-v1.5", filename = "gfc_data_prj.tif", overwrite = TRUE)
# defo_e <- raster("gfc_data_prj.tif")
# crs(defo_e)
###
########################################################################################################################


##### THRESHOLD the GFC data based on a specified percent cover threshold: 25%, 50% and 75% here. 
########################################################################################################################

# read the extracted layers
gfc_data <- brick("gfc_data.tif")

## Do it once by once
# 25%
forest_threshold <- 25
threshold_gfc(gfc_data, 
              forest_threshold=forest_threshold, 
              filename="gfc_data_25th.tif", 
              overwrite = TRUE )

# 50%
forest_threshold <- 50
threshold_gfc(gfc_data, 
              forest_threshold=forest_threshold, 
              filename="gfc_data_50th.tif", 
              overwrite = TRUE )
# 75%
forest_threshold <- 75
threshold_gfc(gfc_data, 
              forest_threshold=forest_threshold, 
              filename="gfc_data_75th.tif",
              overwrite = TRUE )

rm(gfc_data)


#extract lossyear layer.
gfc_data25 <- brick("gfc_data_25th.tif")
gfc_data50 <- brick("gfc_data_50th.tif")
gfc_data75 <- brick("gfc_data_75th.tif")

#names(gfc_data) #"gfc_data.1" "gfc_data.2" "gfc_data.3" "gfc_data.4"; .2 is lossyear (values going from 0 to 18). 
# command below flexibly selects the layer that has years running from 15 (we don't want earlier versions of GFC here) to adapt to future versions. 
loss25 <- gfc_data25[[which(gfc_data25@data@max > 15 & gfc_data25@data@max < 30)]]
loss50 <- gfc_data50[[which(gfc_data50@data@max > 15 & gfc_data50@data@max < 30)]]
loss75 <- gfc_data75[[which(gfc_data75@data@max > 15 & gfc_data75@data@max < 30)]]

#remove other layers
rm(gfc_data25)
rm(gfc_data50)
rm(gfc_data75)


##### ALIGN PLANTATION MAPS ON FOREST LOSS MAPS #####
########################################################################################################################

### 2000 plantations
# read plantation map 2000 in. 
po <- raster(here("build/input/PALMOIL/new_oilpalm_2000_WGS1984.tif"))

# po # resolution is 0.002277, 0.002277
# loss # resolution is 0.00025, 0.00025

# ALIGN PO ON LOSS: po is disaggregated and will match loss res, ext, and crs. Both are unprojected at this stage. 
projectRaster(from = po, to = loss25, 
              method = "ngb", 
              filename = "aligned_new_oilpalm_2000.tif", 
              overwrite = TRUE )  

rm(po)

### 2015 plantations
# read plantation map 2015 in. 
po <- raster(here("build/input/PALMOIL/new_oilpalm_2015_WGS1984.tif"))

# po # resolution is 0.002277, 0.002277
# loss # resolution is 0.00025, 0.00025

# ALIGN PO ON LOSS: po is disaggregated and will match loss res, ext, and crs. Both are unprojected at this stage. 
    projectRaster(from = po, to = loss, 
                          method = "ngb", 
                          filename = "aligned_new_oilpalm_2015.tif", 
                          overwrite = TRUE )  

rm(po)
########################################################################################################################


##### COMPUTE FOREST LOSS IMPUTABLE TO PALM OIL #####
########################################################################################################################
# We want to keep forest loss pixels only within 2015 plantations in order to induce forest conversion to plantation, 
# BUT outside 2000 plantations, in order not to count plantation renewals as forest conversion to plantation. 
# po maps are binary with 1 meaning plantation in 2015 (or 2000 resp.)) 
po2000 <- raster("aligned_new_oilpalm_2000.tif")
po2015 <- raster("aligned_new_oilpalm_2015.tif")

# For 25% treshold definition
f <- function(loss25, po2000, po2015) {loss25*po2015*(1-po2000)}
overlay(loss25, po2000, po2015, 
        fun = f, 
        filename = "defo_25th.tif", 
        overwrite = TRUE ) 

rm(loss25, f)

# For 50% treshold definition
f <- function(loss50, po2000, po2015) {loss50*po2015*(1-po2000)}
overlay(loss50, po2000, po2015, 
        fun = f, 
        filename = "defo_50th.tif", 
        overwrite = TRUE ) 

rm(loss50, f)


# For 75% treshold definition
f <- function(loss75, po2000, po2015) {loss75*po2015*(1-po2000)}
overlay(loss75, po2000, po2015, 
        fun = f, 
        filename = "defo_75th.tif", 
        overwrite = TRUE ) 

rm(loss75, f)

rm(po2000, po2015)
#################################################################################################################################



##### PROJECT PALM-IMPUTABLE DEFORESTATION MAP #####
#################################################################################################################################

# This is necessary because we will need to make computations on this map within mills' catchment *areas*. 
# If one does not project this map, then catchment areas all have different areas while being defined with a common buffer.

# For 25% threshold definition
defo <- raster("defo_25th.tif")
                                  
projectRaster(from = defo, 
              crs = indonesian_crs, 
              method = "ngb", 
              filename = "defo_25th_prj.tif", 
              overwrite = TRUE )

rm(defo)

# For 50% threshold definition
defo <- raster("defo_50th.tif")

projectRaster(from = defo, 
              crs = indonesian_crs, 
              method = "ngb", 
              filename = "defo_50th_prj.tif", 
              overwrite = TRUE )

rm(defo)


# For 75% threshold definition
defo <- raster("defo_75th.tif")

projectRaster(from = defo, 
              crs = indonesian_crs, 
              method = "ngb", 
              filename = "defo_75th_prj.tif", 
              overwrite = TRUE )

rm(defo)
#################################################################################################################################



##### SPLIT THE SINGLE LAYER defo RASTER INTO ANNUAL LAYERS. #####
#################################################################################################################################

# For 25% threshold definition 
defo <- raster("defo_25th_prj.tif")

years <- c(1:18)
for (t in 1:length(years)){ 
  annualrastername <- paste0("annual_maps/defo_25th_", years[t],".tif")
  calc(defo, fun = function(x){if_else(x == years[t], true = 1, false = 0)}, 
       filename = annualrastername, 
       overwrite = TRUE ) 
}

rm(annualrastername, defo)

# For 50% threshold definition 
defo <- raster("defo_50th_prj.tif")

years <- c(1:18)
for (t in 1:length(years)){ 
  annualrastername <- paste0("annual_maps/defo_50th_", years[t],".tif")
  calc(defo, fun = function(x){if_else(x == years[t], true = 1, false = 0)}, 
       filename = annualrastername, 
       overwrite = TRUE ) 
}

rm(annualrastername, defo)

# For 75% threshold definition 
defo <- raster("defo_75th_prj.tif")

years <- c(1:18)
for (t in 1:length(years)){ 
  annualrastername <- paste0("annual_maps/defo_75th_", years[t],".tif")
  calc(defo, fun = function(x){if_else(x == years[t], true = 1, false = 0)}, 
       filename = annualrastername, 
       overwrite = TRUE ) 
}

rm(annualrastername, defo)



#################################################################################################################################


##### AGGREGATE THE PIXELS TO A GIVEN PARCEL SIZE. #####
#################################################################################################################################
    # Since in each annual map, original pixels are either 1 or 0 valued (conversion from forest to op plantation remotely sensed 
    # for that pixel that year or not) and each pixel is the same area, the mean value of a group of pixels is 
    # the ratio of the area deforested over the parcel area. This is refered to as the percentage of deforestation. 


#### define parcel size ####
PS <- 10000

years <- seq(from = 1998, to = 2015, by = 1)

threshold <- 25
while(threshold < 100){
  for(t in 1:length(years)){
    processname <- paste0("./annual_maps/defo_",threshold,"th_", t,".tif")
    
    #create unique filepath for temp directory
    dir.create(file.path(paste0(processname,"_Tmp")), showWarnings = FALSE)
    #set temp directory
    rasterOptions(tmpdir=file.path(paste0(processname,"_Tmp")))
    
    
    ## Mask operation in order to keep only data in the area of interest
    
    # read in the indonesia wide raster of deforestation of year t, threshold th, computed in prepare_deforestation_maps.R 
    annual_defo <- raster(processname)
    
    # aggregate it from the 30m cells to PSm cells with mean function. 
    raster::aggregate(annual_defo, fact = c(PS/res(annual_defo)[1], PS/res(annual_defo)[2]), 
                      expand = FALSE, 
                      fun = mean,
                      filename = paste0("./annual_parcels/parcels_",PS/1000,"km_",threshold, "th_", t,".tif"),
                      overwrite = TRUE)

    
    ## Deal with memory and stockage issues: 
    rm(annual_defo)
    #removes entire temp directory without affecting other running processes (but there should be no temp file now)
    unlink(file.path(paste0(processname,"_Tmp")), recursive = TRUE)
    
  }
  
  
  #read in the 18 layers and brick them
  layers_paths <- list.files(path = "./annual_parcels", pattern = paste0("parcels_",PS/1000,"km_",threshold, "th_"), full.names = TRUE) %>% as.list()
  #layers_paths <- list.files(path = "./annual_maps", pattern = paste0("defo_",threshold, "th_"), full.names = TRUE) %>% as.list()
  parcels_brick <- layers_paths %>% brick()
  
  #write the brick
  writeRaster(parcels_brick, 
              filename = paste0("./bricked_parcels/parcels_",PS/1000,"km_",threshold,"th.tif"), 
              overwrite = TRUE)
  
  
  threshold <- threshold + 25
}











































### MASK ? is not useful because reclassifying NAs to 0s does not make the file lighter. 
# (and croping more is not possible.) 

