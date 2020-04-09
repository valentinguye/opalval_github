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
  # PACKAGES, WD, OBJECTS
  # DEFINE AREA OF INTEREST
  # DOWNLOAD APPROPRIATE HANSEN DEFORESTATION DATA
  # THRESHOLD GFC DATA AT 25%, 50% and 75% PIXEL COVER
  # ALIGN PLANTATION MAPS ON FOREST LOSS MAPS
  # COMPUTE FOREST LOSS IMPUTABLE TO PALM OIL
  # PROJECT PALM-IMPUTABLE DEFORESTATION MAP
  # SPLIT THE SINGLE LAYER defo RASTER INTO ANNUAL LAYERS.
############################################################################################################

rm(list = ls())

##### PACKAGES, WD, OBJECTS #####

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

### local working directory, just to shorten calls, using here. 
setwd(here("build/input/outcome_variables"))
  

### INDONESIAN CRS 
  #   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
  #   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator. 
  #   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is 
  #   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
  #   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0 
  indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  

rasterOptions(progress = "window", 
              timer = TRUE)
  
##### DEFINE AREA OF INTEREST (AOI) #####
############################################################################################################

# read data.frame of cross-sectional mills with their coordinates. 
mills <- read.dta13(here("build/input/IBS_UML_cs.dta"))  

#turn into an sf object. 
mills <- st_as_sf(mills,	coords	=	c("lon",	"lat"), crs=4326)
# keep only the geometry, we do not need mills attributes here. 
mills <- st_geometry(mills)
# set CRS and project
mills_prj <- st_transform(mills, crs = indonesian_crs) 
st_crs(mills_prj) # units are meters. 

#define big catchment areas to have a large AOI. 
mills_ca <- st_buffer(mills_prj, dist = 60000)

# work with squares rather than with circles
for(i in 1:length(mills_ca)){
  mills_ca[i] <- st_as_sfc(st_bbox(mills_ca[i]))
}
# and dissolve them in one polygon aoi <- st_union(st_geometry(mills_ca))

# rather use the bbox.
aoi <- st_as_sfc(st_bbox(mills_ca))

# unproject to use extract_gfc with to_UTM = FALSE
aoi <- st_transform(aoi, crs = 4326)
#convert the box to a SpatialPolygon object for compatibility with download_tiles methods. 
aoi_sp <- as(aoi, "Spatial")


rm(mills, mills_prj, mills_ca, aoi)





##### DOWNLOAD APPROPRIATE HANSEN DEFORESTATION DATA #####
#########################################################################################################################
  
#define where all tiles are going to be stored
data_folder <- paste0(getwd(), "/GFC_tiles")

#Calculate tiles needed to cover the AOI
tiles <- calc_gfc_tiles(aoi_sp)
length(tiles) # 11 (the upper right tile is not needed with the union and not the bbox of all CAs) 

# version of GFC used here. 
gfc_version <- "GFC-2018-v1.6" 
# //!\\ script is not written flexibly to adjust for other versions of GFC. One should check every "18" entries for instance. 

#download tiles - with all layers otherwise later extract_gfc does not work 
download_tiles(tiles, data_folder, images = c("treecover2000", "lossyear", "gain", "datamask"), dataset = gfc_version)

# extract gfc data (can only extract all layers with default stack=change)
# to better understand extract_gfc see https://rdrr.io/cran/gfcanalysis/src/R/extract_gfc.R
extract_gfc(aoi_sp, data_folder, 
            stack = "change", 
            to_UTM = FALSE, 
            dataset = gfc_version, 
            filename = "gfc_data.tif",
            overwrite = TRUE )
# extract télécharge les tiles qui couvrent notre AOI
# et ensuite pour ces tiles là il n'y a aucun NA, dans le résultat du extract, même 
# pour les pixels en dehors de l'AOI. En revanche il y a des NAs
# dans la partie du raster couverte pas un tile qui ne couvre pas l'AOI. 
# donc si on prend un bbox comme aoi on n'a pas de NA. 
# faire ça par île ? 


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
# 30%
forest_threshold <- 30
threshold_gfc(gfc_data, 
              forest_threshold=forest_threshold, 
              filename="gfc_data_30th.tif", 
              overwrite = TRUE )

# 60%
forest_threshold <- 60
threshold_gfc(gfc_data, 
              forest_threshold=forest_threshold, 
              filename="gfc_data_60th.tif", 
              overwrite = TRUE )
# 90%
forest_threshold <- 90
threshold_gfc(gfc_data, 
              forest_threshold=forest_threshold, 
              filename="gfc_data_90th.tif",
              overwrite = TRUE )

rm(gfc_data)


## extract lossyear layer.
# names(gfc_data) #"gfc_data.1" "gfc_data.2" "gfc_data.3" "gfc_data.4"; .2 is lossyear (values going from 0 to 18). 
# command below flexibly selects the layer that has years running from 15 (we don't want earlier versions of GFC here) to adapt to future versions (up to 2040...) 

# 30% 
gfc_data30 <- brick("gfc_data_30th.tif")
loss30 <- gfc_data30[[which(gfc_data30@data@max > 15 & gfc_data30@data@max < 40)]]
rm(gfc_data30)

# 60%
gfc_data60 <- brick("gfc_data_60th.tif")
loss60 <- gfc_data60[[which(gfc_data60@data@max > 15 & gfc_data60@data@max < 40)]]
rm(gfc_data60)

# 90%
gfc_data90 <- brick("gfc_data_90th.tif")
loss90 <- gfc_data90[[which(gfc_data90@data@max > 15 & gfc_data90@data@max < 40)]]
rm(gfc_data90)




##### ALIGN PLANTATION MAPS ON FOREST LOSS MAPS #####
########################################################################################################################
# ALIGN PO ON LOSS: po is disaggregated and will match loss res, ext, and crs. Both are unprojected at this stage. 

### 2000 plantations
po2000 <- raster(here("build/input/PALMOIL/new_oilpalm_2000_WGS1984.tif"))
# po # resolution is 0.002277, 0.002277
# loss # resolution is 0.00030, 0.00025
projectRaster(from = po2000, to = loss30, 
              method = "ngb", 
              filename = "aligned_new_oilpalm_2000.tif", 
              datatype = "INT1U",
              overwrite = TRUE )  

rm(po2000)

### 2015 plantations
po2015 <- raster(here("build/input/PALMOIL/new_oilpalm_2015_WGS1984.tif"))
# po # resolution is 0.002277, 0.002277
# loss # resolution is 0.00025, 0.00025
projectRaster(from = po2015, to = loss30, 
              method = "ngb", 
              filename = "aligned_new_oilpalm_2015.tif", 
              datatype = "INT1U",
              overwrite = TRUE )  

rm(po2015)
########################################################################################################################


##### OVERLAY FOREST LOSS AND OIL PALM PLANTATIONS #####
########################################################################################################################
# We want to keep forest loss pixels only within 2015 plantations in order to induce forest conversion to plantation, 
# BUT outside 2000 plantations, in order not to count plantation renewals as forest conversion to plantation. 
# po maps are binary with 1 meaning plantation in 2015 (or 2000 resp.)) 
po2000 <- raster("aligned_new_oilpalm_2000.tif")
po2015 <- raster("aligned_new_oilpalm_2015.tif")

# overlay function 
f <- function(rs){rs[[1]]*rs[[3]]*(1-rs[[2]])}
# multiplies a cell of forest loss (rs[[1]]) by 0 if it is not a plantation in 2015 (rs[[3]]) or if it is a plantation in 2000 (rs[[2]])

## For 30% treshold definition
rs <- stack(loss30, po2000, po2015)

# run the computation in parallel with clusterR, as cells are processed one by one independently. 
beginCluster() # uses by default detectedCores() - 1 
clusterR(rs, 
         fun = calc, # note we use calc but this is equivalent to using overlay
         args = list(f),
         filename = "lucfp_30th.tif",
         datatype = "INT1U",
         overwrite = TRUE )

lucfp30 <- raster("lucfp_30th.tif")
MAIS PK RECLASSIFIER FORCEMENT ? les NA sont là pour une raison, ils ne remplacent pas forcément des zeros
clusterR(lucfp30, 
         fun = reclassify, 
         args = list(rcl = cbind(NA,0)),
         filename = "lucfp_30th.tif",
         datatype = "INT1U",
         overwrite = TRUE )

endCluster()

rm(loss30, rs, lucfp30)

## For 60% treshold definition
rs <- stack(loss60, po2000, po2015)

beginCluster()
clusterR(rs, 
         fun = calc,
         args = list(f),
         filename = "lucfp_60th.tif",
         datatype = "INT1U",
         overwrite = TRUE )

lucfp60 <- raster("lucfp_60th.tif")

clusterR(lucfp60, 
         fun = reclassify, 
         args = list(rcl = cbind(NA,0)),
         filename = "lucfp_60th.tif",
         datatype = "INT1U",
         overwrite = TRUE )
endCluster()

rm(loss60, rs, lucfp60)


## For 90% treshold definition
rs <- stack(loss90, po2000, po2015)

beginCluster()
clusterR(rs, 
         fun = calc,
         args = list(f),
         filename = "lucfp_90th.tif",
         datatype = "INT1U",
         overwrite = TRUE )

lucfp90 <- raster("lucfp_90th.tif")

clusterR(lucfp90, 
         fun = reclassify, 
         args = list(rcl = cbind(NA,0)),
         filename = "lucfp_90th.tif",
         datatype = "INT1U",
         overwrite = TRUE )
endCluster()

rm(loss90, rs, lucfp90)

rm(po2000, po2015)
#################################################################################################################################



##### PROJECT PALM-IMPUTABLE DEFORESTATION MAP #####
#################################################################################################################################

# This is necessary because we will need to make computations on this map within mills' catchment *areas*. 
# If one does not project this map, then catchment areas all have different areas while being defined with a common buffer.

# For 30% threshold definition
lucfp <- raster("lucfp_30th.tif")
                                  
projectRaster(from = lucfp, 
              crs = indonesian_crs, 
              method = "ngb", 
              filename = "lucfp_30th_prj.tif", 
              datatype = "INT1U",
              overwrite = TRUE )

rm(lucfp)

# For 60% threshold definition
lucfp <- raster("lucfp_60th.tif")

projectRaster(from = lucfp, 
              crs = indonesian_crs, 
              method = "ngb", 
              filename = "lucfp_60th_prj.tif", 
              datatype = "INT1U",
              overwrite = TRUE )

rm(lucfp)


# For 90% threshold definition
lucfp <- raster("lucfp_90th.tif")

projectRaster(from = lucfp, 
              crs = indonesian_crs, 
              method = "ngb", 
              filename = "lucfp_90th_prj.tif", 
              datatype = "INT1U",
              overwrite = TRUE )

rm(lucfp)
#################################################################################################################################



##### SPLIT THE SINGLE LAYER defo RASTER INTO ANNUAL LAYERS. #####
#################################################################################################################################

years <- seq(from = 2001, to = 2018, by = 1)

## For 30% threshold definition 
lucfp <- raster("lucfp_30th_prj.tif")

for (t in 1:length(years)){ 
  annualrastername <- paste0("annual_maps/lucfp_30th_", years[t],".tif")
  calc(lucfp, fun = function(x){if_else(x == t, true = 1, false = 0)}, 
       filename = annualrastername, 
       datatype = "INT1U",
       overwrite = TRUE ) 
}

rm(annualrastername, lucfp)


## For 60% threshold definition 
lucfp <- raster("lucfp_60th_prj.tif")

for (t in 1:length(years)){ 
  annualrastername <- paste0("annual_maps/lucfp_60th_", years[t],".tif")
  calc(lucfp, fun = function(x){if_else(x == t, true = 1, false = 0)}, 
       filename = annualrastername, 
       datatype = "INT1U",
       overwrite = TRUE ) 
}

rm(annualrastername, lucfp)


## For 90% threshold definition 
lucfp <- raster("lucfp_90th_prj.tif")

for (t in 1:length(years)){ 
  annualrastername <- paste0("annual_maps/lucfp_90th_", years[t],".tif")
  calc(lucfp, fun = function(x){if_else(x == t, true = 1, false = 0)}, 
       filename = annualrastername, 
       datatype = "INT1U",
       overwrite = TRUE ) 
}

rm(annualrastername, lucfp)

#################################################################################################################################



##### AGGREGATE THE PIXELS TO A GIVEN PARCEL SIZE. #####
#################################################################################################################################


#### FOR 3km x 3km PARCELS ####
PS <- 3000
# just PS and run all the code from here to get the lucfp datasets for a different parcel size. 
  
annual_aggregate <- function(time, threshold){
    ## Define which process (year and threshold) we are in: 
    processname <- file.path(paste0("./annual_maps/lucfp_",threshold,"th_", years[time],".tif"))
    #create unique filepath for temp directory
    dir.create(paste0(processname,"_Tmp"), showWarnings = FALSE)
    # #set temp directory
    rasterOptions(tmpdir=file.path(paste0(processname,"_Tmp")))
    # read in the indonesia wide raster of lucfp at a given time and for a given threshold. 
    annual_defo <- raster(processname)
    ## Aggregation operation
    # aggregate it from the 30m cells to PSm cells with mean function. 
    raster::aggregate(annual_defo, fact = c(PS/res(annual_defo)[1], PS/res(annual_defo)[2]), 
                      expand = FALSE, 
                      fun = sum,
                      filename = paste0("./annual_parcels/parcels_",PS/1000,"km_",threshold,"th_",time,".tif"),
                      datatype = "INT4U", # because the sum may go up to ~ 10 000 with PS = 3000, 
                      # but to more than 65k with PS = 10000 so INT4U will be necessary; 
                      overwrite = TRUE)
    ## Deal with memory and stockage issues: 
    #removes entire temp directory without affecting other running processes (but there should be no temp file now)
    unlink(file.path(paste0(processname,"_Tmp")), recursive = TRUE)
    #unlink(file.path(tmpDir()), recursive = TRUE)
    ## return the path to this parcels file 
    return(file.path(paste0("./annual_parcels/parcels_",PS/1000,"km_",threshold,"th_",time,".tif")))
}
  
### 2 register loop function - not in parallel here
aggregate <- function(th){
    #registerDoParallel(cores = detected_cores) 
    
    # the loop has arguments to define how the results of the workers should be combined, and to give "workers" (CPUs) 
    # the objects and packages they need to run the function. 
    foreach(t = 1:length(years), 
            # .combine combine the outputs as a mere character list (by default)
            #.inorder = FALSE, # we don't care that the results be combine in the same order they were submitted
            #.multicombine = TRUE,
            .export = c("annual_aggregate", "years", "PS"), 
            .packages = c("raster")) %do% 
      annual_aggregate(time = t, threshold = th)  
}
  
  
### 3. run the function to compute the RasterBrick object of 18 annual layers for each forest definition threshold 

## 30%
# run the computation, that writes the layers and return a list of their paths 
rasterlist <- aggregate(th = 30)

# brick the layers together
parcels_brick <- brick(rasterlist)

# write it
writeRaster(parcels_brick, 
            filename = paste0("./bricked_parcels/parcels_",PS/1000,"km_30th.tif"),
            datatype = "INT4U",
            overwrite = TRUE)
rm(parcels_brick)


## 60% 
# run the computation, that writes the layers and return a list of their paths 
rasterlist <- aggregate(th = 60)

# brick the layers together
parcels_brick <- brick(rasterlist)

# write it
writeRaster(parcels_brick, 
            filename = paste0("./bricked_parcels/parcels_",PS/1000,"km_60th.tif"),
            datatype = "INT4U",
            overwrite = TRUE)
rm(parcels_brick)

## 90%
# run the computation, that writes the layers and return a list of their paths 
rasterlist <- aggregate(th = 90)

# brick the layers together
parcels_brick <- brick(rasterlist)

# write it
writeRaster(parcels_brick, 
            filename = paste0("./bricked_parcels/parcels_",PS/1000,"km_90th.tif"),
            datatype = "INT4U",
            overwrite = TRUE)
rm(parcels_brick)




##### convert to dataframe. ##### 
#################################################################################################





# we do not mask with water mask from gfc because there are many other reasons why 
# deforestation cannot occur (cities, mountains) and these are taken into account in FE 
# and the distributional effect of this on our outcome variabel will be captured 
# by the poisson model. 

# Note also that For Sumatra at least, the northern part of the most north CA is not covered 
# by Austin data. Those NAs are simply not converted to the dataframe. This is not an 
# issue because the unit of observation is not the CA (one of which would not be the same size)
# but the parcel. In other words we don't bother that some CA are not fully observed in our maps. 

# we do not reclassify NAs to 0 after the overlay between loss and plantation maps 
# because these NAs are turned to 0 in the split anyway. They are only at the margin (i.e. they could be trimed)
#lucfp30 <- raster("lucfp_30th.tif")
# clusterR(lucfp30, 
#          fun = reclassify, 
#          args = list(rcl = cbind(NA,0)),
#          filename = "lucfp_30th.tif",
#          datatype = "INT1U",
#          overwrite = TRUE )





#  brick after a normal aggregation (no foreach)
# # read in the 18 layers and brick them
# layers_paths <- list.files(path = "./annual_parcels", pattern = paste0("parcels_",PS/1000,"km_",threshold, "th_"), full.names = TRUE) %>% as.list()
#   #layers_paths <- list.files(path = "./annual_maps", pattern = paste0("defo_",threshold, "th_"), full.names = TRUE) %>% as.list()
#   parcels_brick <- layers_paths %>% brick()
#   
#   #write the brick
#   writeRaster(parcels_brick, 
#               filename = paste0("./bricked_parcels/parcels_",PS/1000,"km_",threshold,"th.tif"), 
#               overwrite = TRUE)
#   
#   
#   rm(parcels_brick)



# rationale if aggregation is with mean: 
  # Since in each annual map, original pixels are either 1 or 0 valued (conversion from forest to op plantation remotely sensed 
  # for that pixel that year or not) and each pixel is the same area, the mean value of a group of pixels is 
  # the ratio of the area deforested over the parcel area. This is refered to as the percentage of deforestation. 



  ############################################################################################################
  #   Rather than calling all relevant gfc tiles, mosaicing, and masking with catchment areas, we will first 
  #   define an AOI corresponding to catchment areas (CAs) and load only Hansen's maps that cover them.     
  #   BUT, the extract_gfc returns data for larger areas than the only AOI provided (we could see Malaysia).
  #   We do it this way still. 
  #
  #   We don't use gfc_stats because we want to keep information at the pixel level and not at the aoi's in order 
  #   to overlay it with plantations. 
  ############################################################################################################


### MASK ? is not useful because reclassifying NAs to 0s does not make the file lighter. 
# (and croping more is not possible.) 

