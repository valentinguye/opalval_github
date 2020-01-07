###########################################################################################################
#                                                                                                         #
#     Computation of deforestation imputable to oil palms around georeferenced mills                      #
#                                                                                                         #
#     Inputs: - georeferenced mills (from georeferencing works)                                           #
#             ---> IBS_mills_geolocalized.dta                                                             #
#                                                                                                         #
#             - 17 rasters of oil palm-imputable deforestation defo_1.tif to defo_17.tif from             #
#               code prepare_deforestation_map.R                                                          #
#                                                                                                         #
#     Output: A data frame called heilmayr_desa_defo with annually deforested areas in                    #
#             different kinds of buffers                                                                  #
#             around palm oil mills.                                                                      #
#                                                                                                         #
###########################################################################################################
# will require Heilmayr's master mill list as well, in order to accound for as many mills as possible when defining 
# net buffers and computing the indicator the indicator of influence. 

#THIS SCRIPT'S STRUCTURE
############################################################################################################
# EXTRACT FOR SIMPLE BUFFERS
#   - define the simple buffer polygons
#   - extract 
# EXTRACT FOR NET BUFFERS (EXCLUSIVE PARTS OF SIMPLE BUFFERS)
#   - define the polygons
#   - extract
# COMPUTE AN INDICATOR OF INFLUENCE OVER CATCHMENT AREA 

############################################################################################################

rm(list = ls())

# PACKAGES
install.packages("sf", source = TRUE)
library(sf)

neededPackages = c("plyr", "dplyr", "tidyr", "readxl","foreign", "data.table", "readstata13", "here",
                   "rgdal", "raster", "velox","sp", "lwgeom", "rnaturalearth", 
                   "rlist", "parallel", "foreach", "iterators", "doParallel" )

allPackages    = c(neededPackages %in% installed.packages()[ , "Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
  missingIDX = which(allPackages == FALSE)
  needed     = neededPackages[missingIDX]
  lapply(needed, install.packages)
}
# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)

# no CRAN packages
if (!require(devtools)) install.packages("devtools")
# package tictoc
devtools::install_github("jabiru/tictoc")
library(tictoc)


### LOCAL WORKING DIRECTORY, just to shorten calls.
setwd(here("/build/input/outcome_variables"))


##### PREPARE MILL POINTS #####
############################################################################################################

# read data.frame of cross-sectional mills with their coordinates. 
mills <- read.dta13(here("/build/input/mill_geolocalization/IBS_mills_geolocalized.dta"))  

#turn into an sf object. 
mills <- st_as_sf(mills,	coords	=	c("lon",	"lat"), crs=4326)

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

rm(mills)
###########################################################################################################


##### PREPARE ONE POLYGON OF MILLS' TOTAL INFLUENCE #####
############################################################################################################

#the parcel size is defined here, in meters. 
PS <- 10000

# that is the mask of influence area of all mills from IBS over the country. 
#(note the buffer size is note related to the choice of the parcel size. 
# The +PS adds a buffer for the expand = *FALSE* argument: it's decided to make the parcels go *beyond* the 40 kms through the aggregation. 
# the maximum distance a parcel might go beyond is less than the parcel size PS.
# With expand = FALSE, a parcel that does not gather enough input cells to be ~10000m long is not created (it's NA) in the output
# But those which do gather enough small cells will go beyond the 40kms and within the 40km+PSm)

mills_coord <- st_geometry(mills_prj)
mills_ca40 <- st_buffer(mills_prj, dist = 40000+PS) 
for(i in 1:nrow(mills_ca40)){
  mills_ca40$geometry[i] <- st_as_sfc(st_bbox(mills_ca40$geometry[i]))
}
total_ca40 <- st_union(st_geometry(mills_ca40))
total_ca40 <- st_as_sf(total_ca40)
total_ca40_sp <- as(total_ca40, "Spatial")

# A parallel computing set up has been used to speed things up. ( ///!!!\\\ )
# it has 3 steps: 
# 1. Build a function that will be executed for each year in parallel. 
#     This function, individually, aggregates one map for one year and one threshold. 
#     Its input is a RasterLayer file
#     Its output is a RasterLayer file

# 2. Register (i.e. build) the parallel function that executes the annual_aggregate over each year.
#     If one runs this function, the aggregation is made for all years, but only one given threshold.
#     Its inputs are the 18 raster layers for one threshold definition. 
#     Its output is a RasterStack

# 3. Execute the parallel function within a loop over thresholds. 
#     The inputs of the loop are the 3 RasterBrick of 18 layers. 
#     The output of the loop is what we want: 3 data frames in wide format (each column is a year)


### 1. build the function that will be called in the foreach loop: 

annual_aggregate <- function(t, threshold){
  
  ## Define which process we are in: 
  processname <- paste0("./annual_maps/defo_",threshold,"th_", years[t],".tif")
  
  #create unique filepath for temp directory
  dir.create(file.path(paste0(processname,"_Tmp")), showWarnings = FALSE)
  #set temp directory
  rasterOptions(tmpdir=file.path(paste0(processname,"_Tmp")))
  
  
  ## read in the indonesia wide raster of deforestation of year t, threshold th, computed in prepare_deforestation_maps.R 
  annual_defo <- raster(processname)
  
  
  ## mask with the mill influence polygon. 
  mask(annual_defo, total_ca40_sp, 
       filename = paste0("./annual_maps/defo_",threshold,"th_",years[t],"_masked.tif"),
       overwrite = TRUE)
  
  rm(annual_defo)
  
  ## read in the masked raster 
  maskedrastername <- paste0("./annual_maps/defo_",threshold,"th_",years[t],"_masked.tif")
  annual_defo_masked <- raster(maskedrastername)
  
  ## aggregate it from the 30m cells to PSm cells with mean function. 
  raster::aggregate(annual_defo_masked, fact = c(PS/res(annual_defo_masked)[1], PS/res(annual_defo_masked)[2]), 
                    expand = FALSE, 
                    fun = mean,
                    filename = paste0("./annual_parcels/parcels_", threshold, "th_", t, ".tif"),
                    overwrite = TRUE)
  
  ## Deal with memory and stockage issues: 
  rm(annual_defo_masked)
  file.remove(paste0("./annual_maps/defo_",threshold,"th_",years[t],"_masked.tif"))
  
  #removes entire temp directory without affecting other running processes
  file.remove(file.path(paste0(processname,"_Tmp")))
  
  ## read it in to return
  parcelsname <- paste0("./annual_parcels/parcels_", threshold, "th_", t, ".tif")
  parcels <- raster(parcelsname)
  return(parcels)
}

### 2. build the parallel-looping function
aggregate_parallel <- function(detected_cores, th){
  
  registerDoParallel(cores = detected_cores) 
  
  # the loop has arguments to define how the results of the workers should be combined, and to give "workers" (CPUs) 
  # the objects and packages they need to run the function. 
  foreach(t = 1:length(years), 
          .combine = raster::stack,
          .export = c("annual_aggregate", "years", "PS", "total_ca40_sp"), 
          .packages = c("sp", "raster")) %dopar% 
    annual_aggregate(t, threshold = th) # the function that is parallelly applied to different years. 
}



### 3. run it for each forest definition (threshold 25, 50 or 75). 
years <- c(1:18)
th <- 25
while(th < 100){
  # compute the RasterBrick object of 18 annual layers for this threshold
  brick(aggregate_parallel(detected_cores = detectCores(), th = th), 
        filename = paste0("./bricked_parcels/parcels_", th, "th.tif"), 
        overwrite = TRUE)
  th <- th + 25
}

####################################################################################################"
#####                         TEST                   #######

# make the smaller fileS for the test. Don't need to do it again. 
mills_ca40_sp <- as(mills_ca40, "Spatial")

annualrastername <- "./annual_maps/defo_50th_10.tif"
annual_defo <- raster(annualrastername)
test <- crop(annual_defo, y = mills_ca40_sp[1,], filename = "./test/defo_50th_10_test.tif", overwrite = TRUE)
plot(test)

annualrastername <- "./annual_maps/defo_50th_11.tif"
annual_defo <- raster(annualrastername)
test <- crop(annual_defo, y = mills_ca40_sp[1,], filename = "./test/defo_50th_11_test.tif", overwrite = TRUE)
plot(test)

rm(test)
###################################################################################################"

years <- c(1:18)
t <- 10
threshold <- 50
th <- 50
### 1. build the function that will be called in the foreach loop: 

annual_aggregate <- function(t, threshold){
  
  ## Define which process we are in: 
  processname <- paste0("./test/defo_",threshold,"th_", years[t],"_test",".tif")
  
  #create unique filepath for temp directory
  dir.create(file.path(paste0(processname,"_Tmp")), showWarnings = FALSE)
  #set temp directory
  rasterOptions(tmpdir=file.path(paste0(processname,"_Tmp")))
  
  
  ## read in the indonesia wide raster of deforestation of year t, threshold th, computed in prepare_deforestation_maps.R 
  annual_defo <- raster(processname)
  
  #mask it with the mill influence polygon. 
  mask(annual_defo, total_ca40_sp, 
       filename = paste0("./test/defo_",threshold,"th_", years[t],"_masked.tif"),
       overwrite = TRUE)
  rm(annual_defo)
  
  # read in the masked raster 
  maskedrastername <- paste0("./test/defo_",threshold,"th_", years[t],"_masked.tif")
  annual_defo_masked <- raster(maskedrastername)
  
  #aggregate it from the 30m cells to PSm cells with mean function. 
  raster::aggregate(annual_defo_masked, fact = c(PS/res(annual_defo_masked)[1], PS/res(annual_defo_masked)[2]), 
                    expand = FALSE, 
                    fun = mean,
                    filename = paste0("./test/parcels_", threshold, "th_", t, ".tif"),
                    overwrite = TRUE)
  rm(annual_defo_masked)
  
  
  #removes entire temp directory without affecting other running processes
  file.remove(file.path(paste0(processname,"_Tmp")))
  
  parcelsname <- paste0("./test/parcels_", threshold, "th_", t, ".tif")
  parcels <- raster(parcelsname)
  return(parcels)
}

### 2. build the parallel-looping function
aggregate_parallel <- function(cores, th){
  
  registerDoParallel(cores = c) 
  
  # the loop has arguments to define how the results of the workers should be combined, and to give "workers" (CPUs) 
  # the objects and packages they need to run the function. 
  foreach(t = 10:11, .combine = raster::stack,
          .export = c("annual_aggregate", "years", "PS", "total_ca40_sp"), 
          .packages = c("sp", "raster")) %dopar% 
    annual_aggregate(t, threshold = th) # the function that is parallelly applied to different years. 
}
showTmpFiles()
length(years)


### 3. run it for each forest definition (threshold 25, 50 or 75). 
th <- 25
while(th < 100){
  # compute the RasterStack object of 18 annual layers for this threshold
  brick(aggregate_parallel(c = detectCores() - 1, th = th), 
        filename = paste0("./test/bricked_parcels/parcels_", th, "th.tif"), 
        overwrite = TRUE)
  th <- th + 25
}
showTmpFiles()


### Convert to a data frame.
testbrick <- brick(paste0("./test/bricked_parcels/parcels_", th, "th.tif"))
testbrick
df_wide <- raster::as.data.frame(testbrick)
df_wide$id <- c(1:nrow(df_wide))
df <- pivot_longer(df_wide, 
                   cols = -id, 
                   names_to = "year",
                   names_prefix = "parcels_50th.",
                   values_to = "defo_pct",
                   ) 
head(df)
str(df)

############################################ END OF TEST ############################################################

#################### Draft code ##############################
### Draft code ### 
# create a raster from scratch, of resolution the plot size we want for our analysis. 
#let's say 4 square km. 
# this a raster with the defined resolution, but over the whole bbox (extent) of total_ca40
r <- raster(total_ca40, res = PS, crs = indonesian_crs)

# mask outside the influence 
values(r) <- 1
mask <- as(total_ca40, "Spatial")
rp <- mask(r, mask, updatevalue = NA)
rm(r)

# make annual layers.
years <- c(1:18)
for(t in 1:length(years)){
  annualrastername <- paste0("./annual_parcels/parcels_", years[t],".tif")
  writeRaster(rp, 
              filename = annualrastername)
}
#########################################################


