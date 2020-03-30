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

# risque de mal se passer avec l'écritute en fonction
rasterOptions(chunksize = 1e+9,
              timer = TRUE)


############################################################################################################

#################################################################
# Write prepare_deforestation script as a function of island 
# either Sumatra-Java, Kalimantan, Sulawesi and Papua-Maluku (?)
#################################################################

# note on rm in function: removes object in the function "frame" i.e. environment withou
# having to specify something (I tested it) 

#island <- "Sumatra"
prepare_deforestation <- function(island, PS){

##### DEFINE AREA OF INTEREST (AOI) #####
############################################################################################################

# read data.frame of cross-sectional mills with their coordinates. 
mills <- read.dta13(here("build/input/IBS_UML_cs.dta"))  

mills <- mills[mills$island_name == island,]

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
# rather use a BBOX
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
            filename = paste0("gfc_data_",island,".tif"),
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
gfc_data <- brick(paste0("gfc_data_",island,".tif"))

## Do it once by once
# 30%
th <- 30
while(th < 100){
  threshold_gfc(gfc_data, 
                forest_threshold=th, 
                filename=paste0("gfc_data_",island,"_",th,"th.tif"), 
                overwrite = TRUE )
  th <- th + 30
}

# Do not remove gfc_data yet because we need it to align plantation maps
#rm(gfc_data)



##### ALIGN PLANTATION MAPS ON FOREST LOSS MAPS #####
########################################################################################################################
# ALIGN PO ON LOSS: po is disaggregated and will match loss res, ext, and crs. Both are unprojected at this stage. 
# po # resolution is 0.002277, 0.002277
# loss # resolution is 0.00030, 0.00025
# first crop po maps to the current aoi (island bbox) to match res.
# then projectRaster to match res - disaggregate before is not necessary (yields the same result)

### 2000 plantations
po2000 <- raster(here("build/input/PALMOIL/new_oilpalm_2000_WGS1984.tif"))
# match extent
crop(po2000, y = gfc_data, 
             filename = paste0("./oilpalm_2000_",island,"_croped.tif"), 
             datatype = "INT1U",
             overwrite = TRUE)
# 3 minutes, not even printed

# match resolution.
po2000 <- raster(paste0("./oilpalm_2000_",island,"_croped.tif"))
projectRaster(from = po2000, to = gfc_data, 
              method = "ngb", 
              filename = paste0("./oilpalm_2000_",island,"_aligned.tif"), 
              datatype = "INT1U",
              overwrite = TRUE )  
rm(po2000)
# 18700 seconds

### 2015 plantations
po2015 <- raster(here("build/input/PALMOIL/new_oilpalm_2015_WGS1984.tif"))
# match extent
crop(po2015, y = gfc_data, 
     filename = paste0("./oilpalm_2015_",island,"_croped.tif"), 
     datatype = "INT1U",
     overwrite = TRUE)
# less than a minute, not even printed

# match resolution
po2015 <- raster(paste0("./oilpalm_2015_",island,"_croped.tif"))
projectRaster(from = po2015, to = gfc_data, 
              method = "ngb", 
              filename = paste0("./oilpalm_2015_",island,"_aligned.tif"), 
              datatype = "INT1U",
              overwrite = TRUE )  
# 9436 seconds

rm(po2015)

rm(gfc_data)
########################################################################################################################


##### OVERLAY FOREST LOSS AND OIL PALM PLANTATIONS #####
########################################################################################################################
# We want to keep forest loss pixels only within 2015 plantations in order to induce forest conversion to plantation, 
# BUT outside 2000 plantations, in order not to count plantation renewals as forest conversion to plantation. 
# po maps are binary with 1 meaning plantation in 2015 (or 2000 resp.)) 
po2000 <- raster(paste0("./oilpalm_2000_",island,"_aligned.tif"))
po2015 <- raster(paste0("./oilpalm_2015_",island,"_aligned.tif"))

# overlay function 
f <- function(rs){rs[[1]]*(1-rs[[2]])*rs[[3]]}
# multiplies a cell of forest loss (rs[[1]]) by 0 if it it is a plantation in 2000 (rs[[2]]) or if is not a plantation in 2015 (rs[[3]]) 

## For th% treshold definition
th <- 30
while(th < 100){
  # call the loss layer for threshold th 
  thed_gfc_data <- brick(paste0("gfc_data_",island,"_",th,"th.tif"))
  # select the loss layer
  loss <- thed_gfc_data[[which(thed_gfc_data@data@max > 15 & thed_gfc_data@data@max < 40)]]
  # remove useless other stack of gfc layers
  rm(thed_gfc_data)
  # stack loss with plantation maps 
  rs <- stack(loss, po2000, po2015)

  # run the computation in parallel with clusterR, as cells are processed one by one independently. 
  beginCluster() # uses by default detectedCores() - 1 
  clusterR(rs, 
           fun = calc, # note we use calc but this is equivalent to using overlay
           args = list(f),
           filename = paste0("lucfp_",island,"_",th,"th.tif"),
           datatype = "INT1U",
           overwrite = TRUE )

  endCluster()

  rm(loss)  
  
  th <- th + 30
}
# ~ 4500 seconds / threshold 

rm(po2000, po2015)


#################################################################################################################################

##### PROJECT PALM-IMPUTABLE DEFORESTATION MAP #####
#################################################################################################################################

# This is necessary because we will need to make computations on this map within mills' catchment *areas*. 
# If one does not project this map, then catchment areas all have different areas while being defined with a common buffer.

th <- 30
while(th < 100){
  lucfp <- raster(paste0("lucfp_",island,"_",th,"th.tif"))

  projectRaster(from = lucfp, 
                crs = indonesian_crs, 
                method = "ngb", 
                filename = paste0("lucfp_",island,"_",th,"th_prj.tif"), 
                datatype = "INT1U",
                overwrite = TRUE )
  
  rm(lucfp)

  th <- th + 30
}
# 13571 seconds
# 12459 seconds
# 11565 seconds
#################################################################################################################################



##### SPLIT THE SINGLE LAYER defo RASTER INTO ANNUAL LAYERS. #####
#################################################################################################################################

years <- seq(from = 2001, to = 2018, by = 1)

# use foreach rather than for loop
annual_split <- function(time, threshold){
  # define process
  process <- file.path(paste0("lucfp_",island,"_",threshold,"th_prj.tif"))
  # #set temp directory
  dir.create(paste0(process,"_Tmp"), showWarnings = FALSE)
  rasterOptions(tmpdir=file.path(paste0(process,"_Tmp")))
  
  lucfp_prj <- raster(process)
  
  calc(lucfp_prj, 
       fun = function(x){if_else(x == time, true = 1, false = 0)}, 
       filename = paste0("./annual_maps/lucfp_",island,"_",threshold,"th_", years[time],".tif"),
       datatype = "INT1U",
       overwrite = TRUE )
  
  unlink(file.path(paste0(process,"_Tmp")), recursive = TRUE)
}

split <- function(th, detected_cores){
  registerDoParallel(cores = detected_cores) 
  # the loop has arguments to define how the results of the workers should be combined, and to give "workers" (CPUs) 
  # the objects and packages they need to run the function. 
  foreach(t = 1:length(years), 
          # .combine combine the outputs as a mere character list (by default)
          .inorder = FALSE, # we don't care that the results be combine in the same order they were submitted
          .multicombine = TRUE,
          .export = c("annual_split", "years"), 
          .packages = c("dplyr", "raster")
  ) %dopar%  annual_split(time = t, threshold = th)
} 
  
th <- 30
while(th < 100){
  split(th, detectCores() - 1) # ~500 seconds / annual layer
  th <- th + 30
}



#################################################################################################################################



##### AGGREGATE THE PIXELS TO A GIVEN PARCEL SIZE. #####
#################################################################################################################################

years <- seq(from = 2001, to = 2018, by = 1)

## write aggregating function to be executed in parallel for each annual layer 
annual_aggregate <- function(time, threshold){
  ## Define which process (year and threshold) we are in: 
  processname <- file.path(paste0("./annual_maps/lucfp_",island,"_",threshold,"th_", years[time],".tif"))
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
                    na.rm = FALSE, # if FALSE, aggregations at margins that use NA (if there are NA at margin)
                    # are discarded because the sum would be spurious as it would count all NA as 0s while 
                    # it is not necessary the case. 
                    filename = paste0("./annual_parcels/parcels_",island,"_",PS/1000,"km_",threshold,"th_",years[time],".tif"),
                    datatype = "INT4U", # because the sum may go up to ~ 10 000 with PS = 3000, 
                    # but to more than 65k with PS = 10000 so INT4U will be necessary; 
                    overwrite = TRUE)
  ## Deal with memory and stockage issues: 
  #removes entire temp directory without affecting other running processes (but there should be no temp file now)
  unlink(file.path(paste0(processname,"_Tmp")), recursive = TRUE)
  #unlink(file.path(tmpDir()), recursive = TRUE)
  ## return the path to this parcels file 
  return(file.path(paste0("./annual_parcels/parcels_",island,"_",PS/1000,"km_",threshold,"th_",years[time],".tif")))
}

### 2 register loop function - not in parallel here
aggregate <- function(th, detected_cores){
  registerDoParallel(cores = detected_cores) 
  
  # the loop has arguments to define how the results of the workers should be combined, and to give "workers" (CPUs) 
  # the objects and packages they need to run the function. 
  foreach(t = 1:length(years), 
          # .combine combine the outputs as a mere character list (by default)
          .inorder = FALSE, # we don't care that the results be combine in the same order they were submitted
          .multicombine = TRUE,
          .export = c("annual_aggregate", "years"), 
          .packages = c("raster")) %dopar% 
    annual_aggregate(time = t, threshold = th)  
}


### 3. run the function to compute the RasterBrick object of 18 annual layers for each forest definition threshold 
th <- 30 
while(th < 100){
  # run the computation, that writes the layers and return a list of their paths 
  rasterlist <- aggregate(th, detectCores() - 1)
  # brick the layers together
  parcels_brick <- brick(rasterlist)
  # write it
  writeRaster(parcels_brick, 
              filename = paste0("./bricked_parcels/parcels_",island,"_",PS/1000,"km_",th,"th.tif"),
              datatype = "INT4U",
              overwrite = TRUE)
  rm(parcels_brick)
  th <- th + 30
}

return(print("end"))
}

tic()
prepare_deforestation("Sumatra", PS = 3000)
toc()

##### convert to dataframe. ##### 
#################################################################################################

#### Function description 
# raster_to_df converts the raster bricks of annual layers of parcels to a panel dataframe.
# It does that in parallel for each threshold definition. 
# The tasks that are executed are:  
  ## 1. masking the brick of parcels of a given size (PS) on a given island with the maximal CA of mills on that island; 
  ## 2. selecting only the parcels that are within a given catchment radius. 
  ## 3. reshaping the values in these parcels to a long format panel dataframe

raster_to_df <- function(island, PS, catchment_radius, ncores){
  
## write the function to be executed in parallel for each threshold definition
threshold_raster_to_df <- function(threshold){
  
  years <- seq(from = 2001, to = 2018, by = 1)
    
  ## 1. Masking. 
  # Probably more efficient as the st_is_within does not need to be executed over all Indonesian cells but only those within the largest catchment_radius. 
  
  ## Make the mask  
  mills <- read.dta13(here("build/input/IBS_UML_cs.dta"))  
  mills <- mills[mills$island_name == island,]
  #turn into an sf object. 
  mills <- st_as_sf(mills,	coords	=	c("lon",	"lat"), crs=4326)
  # keep only the geometry, we do not need mills attributes here. 
  mills <- st_geometry(mills)
  # set CRS and project
  mills_prj <- st_transform(mills, crs = indonesian_crs) 
  #define big catchment areas to have a large AOI. 
  mills_ca <- st_buffer(mills_prj, dist = 60000)
  # work with squares rather than with circles
  for(i in 1:length(mills_ca)){
    mills_ca[i] <- st_as_sfc(st_bbox(mills_ca[i]))
  }
  total_ca <- st_union(st_geometry(mills_ca))
  # coerce to a SpatialPolygon
  total_ca_sp <- as(total_ca, "Spatial")
  # keep mills_prj we need it below
  rm(total_ca, mills_ca, mills) 
  
  ## Mask 
  parcels_brick <- brick(paste0("./bricked_parcels/parcels_",island,"_",PS/1000,"km_",threshold,"th.tif"))
    
  mask(x = parcels_brick, mask = total_ca_sp,
        filename = paste0("./bricked_parcels/m_parcels_",island,"_",PS/1000,"km_",threshold,"th.tif"),
        datatype = "INT4U", 
        overwrite = TRUE)
    

  ## 2. Selecting parcels within a given distance to a mill at least one year
  # (i.e. the parcel is present in the dataframe in all years even if it is within say 50km of a mill only since 2014)

  ## Turn the masked raster to a sf dataframe
  parcels_brick <- brick(paste0("./bricked_parcels/m_parcels_",island,"_",PS/1000,"km_",threshold,"th.tif"))
  m.df_wide <- raster::as.data.frame(parcels_brick, na.rm = TRUE, xy = TRUE, centroids = TRUE)
  m.df_wide <- m.df_wide %>% dplyr::rename(lon = x, lat = y)  
  m.df_wide <- st_as_sf(m.df_wide, coords = c("lon", "lat"), remove = FALSE, crs = indonesian_crs) 


  ## Remove here parcels that are not within the catchment area of a given size (defined by catchment radius) 
  # coordinates of all mills (crs is indonesian crs, unit is meter)
  mills_coord <- mills_prj %>% st_geometry()
  within <- st_is_within_distance(m.df_wide, mills_coord, dist = catchment_radius)
  m.df_wide <- m.df_wide %>% dplyr::filter(lengths(within) >0)
  m.df_wide <- m.df_wide %>% st_drop_geometry()


  ### 3. Reshaping to long format
  m.df_wide$parcel_id <- c(1:nrow(m.df_wide))
  
  # vector of the names in the wide format of our time varying variables 
  varying_vars <- paste0("m_parcels_",island,"_",PS/1000,"km_",threshold,"th.", seq(from = 1, to = 18))

  m.df <- reshape(m.df_wide, 
                  varying = varying_vars, 
                  v.names = paste0("pixelcount_",threshold,"th"), 
                  timevar = "year",
                  idvar = c("parcel_id", "lon", "lat"),
                  ids = "parcel_id",
                  direction = "long",
                  sep = ".",
                  new.row.names = seq(from = 1, to = nrow(m.df_wide)*length(years), by = 1)
  )
  
  # replace the indices from the raster::as.data.frame with actual years. 
  m.df <- mutate(m.df, year = years[year])
  rm(varying_vars)
  m.df <- setorder(m.df, parcel_id, year)
  
  saveRDS(m.df, 
          file = paste0("./dataframes/panel_",island,"_",PS/1000,"km_",catchment_radius/1000,"CR_",threshold,"th.Rdata"))

}
  
thresholdS <- seq(from = 30, to = 90, by = 30)
  
registerDoParallel(cores = ncores) 
# the loop has arguments to define how the results of the workers should be combined, and to give "workers" (CPUs) 
# the objects and packages they need to run the function. 
foreach(th = in(thresholdS), 
        # .combine combine the outputs as a mere character list (by default)
        .inorder = FALSE, # we don't care that the results be combine in the same order they were submitted
        .multicombine = TRUE,
        .export = c("threshold_raster_to_df", "indonesian_crs"), 
        .packages = c("raster")
        ) %dopar% threshold_raster_to_df(threshold = th)  
}

# Convert rasters to dataframes for different sizes of catchment areas (10km, 30km and 50km)
CR <- 10000 # i.e. 10km radius
while(CR < 60000){
raster_to_df(island = "Sumatra", 
             PS = 3000, 
             catchment_radius = CR, 
             ncores = detectCores() - 1)
CR <- CR + 20000
}
#######

