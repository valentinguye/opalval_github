###########################################################################################################
#                                                                                                         #
#     Computation of deforestation imputable to oil palms around georeferenced mills                      #
#                                                                                                         #
#     Inputs: - georeferenced mills (from georeferencing works)                                           #
#             ---> IBS_UML_cs.dta                                                             #
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

neededPackages = c("data.table","plyr", "dplyr", "tidyr", "readxl","foreign", "data.table", "readstata13", "here",
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
setwd(here("build/input/outcome_variables"))

#### define parcel size ####
PS <- 10000

#### define buffer size for the catchment areas  ####
BS <- 40000

#### define projection #### 
#   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
#   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator. 
#   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is 
#   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0 
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"


years <- seq(from = 1998, to = 2015, by = 1)

##### PREPARE MILL POINTS #####
############################################################################################################

# read data.frame of cross-sectional mills with their coordinates. 
mills <- read.dta13(here("/build/input/IBS_UML_cs.dta"))  

#turn into an sf object. 
mills <- st_as_sf(mills,	coords	=	c("lon",	"lat"), crs=4326)
# and project
mills_prj <- st_transform(mills, crs = indonesian_crs) 

rm(mills)
###########################################################################################################


##### PREPARE ONE POLYGON OF MILLS' TOTAL INFLUENCE #####
############################################################################################################

# that is the mask of influence area of all mills from IBS over the country. 
#(note the buffer size is note related to the choice of the parcel size. 
# The +PS adds a buffer for the expand = *FALSE* argument in the aggregate function: 
# it's decided to make the parcels go *beyond* the 40 kms through the aggregation. 
# the strict maximum distance a parcel might go beyond is the parcel size PS.
# With expand = FALSE, a parcel that does not gather enough input cells to be ~10000m long is not created (it's NA) in the output. 
# This makes sure that all parcels have a deforestation rate computed with the same amount of information. 
# But those which do gather enough small cells will go beyond the 40kms and within the 40km+PSm)

mills_coord <- st_geometry(mills_prj)
# here, we define the catchment area invariably at 50 km, the maximum buffer we will consider, so that the deforestation aggregation 
# at the parcel level is done once and for all. The BS variable will be used later on, when selecting what parcels to keep. 
# here we are not at parcel selection yet but in the forge of all possibly useful parcels. 
mills_ca <- st_buffer(mills_prj, dist = 50+PS) 
for(i in 1:nrow(mills_ca)){
  mills_ca$geometry[i] <- st_as_sfc(st_bbox(mills_ca$geometry[i]))
}
total_ca <- st_union(st_geometry(mills_ca))
total_ca <- st_as_sf(total_ca)
total_ca_sp <- as(total_ca, "Spatial")

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
  
  ## Define which process (year and threshold) we are in: 
  processname <- paste0("./annual_maps/defo_",threshold,"th_", t,".tif")
  
  #create unique filepath for temp directory
  dir.create(file.path(paste0(processname,"_Tmp")), showWarnings = FALSE)
  #set temp directory
  rasterOptions(tmpdir=file.path(paste0(processname,"_Tmp")))
  
  
  ## Mask operation in order to keep only data in the area of interest
  
  # read in the indonesia wide raster of deforestation of year t, threshold th, computed in prepare_deforestation_maps.R 
  annual_defo <- raster(processname)
  
  # mask with the mill influence polygon. 
  raster::mask(annual_defo, total_ca_sp, 
       filename = paste0("./annual_maps/defo_",PS/1000,"km_",threshold,"th_",t,"_masked.tif"),
       overwrite = TRUE)
  
  rm(annual_defo)
  
  
  ## Aggregation operation
  
  # read in the masked raster 
  maskedrastername <- paste0("./annual_maps/defo_",PS/1000,"km_",threshold,"th_",t,"_masked.tif")
  annual_defo_masked <- raster(maskedrastername)
  
  # aggregate it from the 30m cells to PSm cells with mean function. 
  raster::aggregate(annual_defo_masked, fact = c(PS/res(annual_defo_masked)[1], PS/res(annual_defo_masked)[2]), 
                    expand = FALSE, 
                    fun = mean,
                    filename = paste0("./annual_parcels/parcels_",PS/1000,"km_",threshold, "th_", t,".tif"),
                    overwrite = TRUE)
  # Since in each annual map, original pixels are either 1 or 0 valued (conversion from forest to op plantation remotely sensed 
  # for that pixel that year or not) and each pixel is the same area, the mean value of a group of pixels is 
  # the ratio of the area deforested over the parcel area. This is refered to as the percentage of deforestation. 
  
  ## Deal with memory and stockage issues: 
  rm(annual_defo_masked)
  file.remove(paste0("./annual_maps/defo_",PS/1000,"km_",threshold,"th_",t,"_masked.tif"))
  #removes entire temp directory without affecting other running processes (but there should be no temp file now)
  unlink(file.path(paste0(processname,"_Tmp")), recursive = TRUE)
  
  
  ## return the path to this parcels file 
  return(paste0("./annual_parcels/parcels_",PS/1000,"km_",threshold, "th_", t,".tif"))
  
}


### 2. build the parallel-looping function
aggregate_parallel <- function(detected_cores, th){
  
  registerDoParallel(cores = detected_cores) 
  
  # the loop has arguments to define how the results of the workers should be combined, and to give "workers" (CPUs) 
  # the objects and packages they need to run the function. 
  foreach(t = 1:length(years), 
          # .combine = raster::stack, combine the outputs as a mere character list (by default)
          .multicombine = TRUE,
          .export = c("annual_aggregate", "years", "PS", "BS", "total_ca_sp"), 
          .packages = c("sp", "raster")) %dopar% 
    annual_aggregate(t, threshold = th) # the function that is parallelly applied to different years. 
}



### 3. run it for each forest definition (threshold 25, 50 or 75). 

th <- 25
while(th < 100){
  # compute the RasterBrick object of 18 annual layers for this threshold
  tic()
  #read in the 18 layers and brick them
  parcels_brick <- brick(aggregate_parallel(detected_cores = detectCores(), th = th))
  
  #write the brick
  writeRaster(parcels_brick, 
        filename = paste0("./bricked_parcels/parcels_",PS/1000,"km_",th,"th.tif"), 
        overwrite = TRUE)
  toc()
  
  #remove the brick object from memory
  rm(parcels_brick)

  th <- th + 25
}

##############################################################################################



##############################################################################################
##### Convert to a data frame. #####

th <- 25
while(th < 100){
  
  # read the brick for forest definition th
  parcels_brick <- brick(paste0("./bricked_parcels/parcels_",PS/1000,"km_",th,"th.tif"))
  
  # turn it to a data frame
  df_wide <- raster::as.data.frame(parcels_brick, na.rm = TRUE, xy = TRUE, centroids = TRUE)
  df_wide <- df_wide %>% dplyr::rename(lon = x, lat = y)
  
  
#### Remove here parcels that are not within the catchment area. ####
  
  # Such parcels are here because we set a larger mask than the strict union of 40 km buffers.
  # Indeed, we wanted to be sure to have no 30m cell missing (masked) in the aggregation - even for the parcels 
  # aggregated at the border of catchment area.
  df_wide <- st_as_sf(df_wide, coords = c("lon", "lat"), remove = FALSE, crs = indonesian_crs)
  # So here we are making the choice of our catchment area size 
  within <- st_is_within_distance(df_wide, mills_coord, dist = BS)
  df_wide <- df_wide %>% dplyr::filter(lengths(within) >0)
  df_wide <- df_wide %>% st_drop_geometry()

## reshape to long format
  df_wide$parcel_id <- c(1:nrow(df_wide))
  
  # vector of the names in the wide format of our time varying variables 
  # varying_vars <- paste0("parcels_",PS/1000,"km_",th,"th.", seq(from = 1, to = 2))
  varying_vars <- paste0("parcels_",PS/1000,"km_",th,"th.", seq(from = 1, to = 18))
  
  df <- reshape(df_wide, 
                varying = varying_vars, 
                v.names = "defo_pct", 
                timevar = "year",
                idvar = c("parcel_id", "lon", "lat"),
                ids = "parcel_id",
                direction = "long",
                sep = ".",
                new.row.names = seq(from = 1, to = nrow(df_wide)*length(years), by = 1)
                )
  
  # replace the indices from the raster::as.data.frame with actual years. 
  df <- mutate(df, year = years[year])
  
  rm(varying_vars)
  
  df <- setorder(df, parcel_id, year)
  
  saveRDS(df, file = paste0("./panel_parcels_",PS/1000,"km_",BS/1000,"CA_",th,"th.Rdata"))
}

#df <- pivot_longer(df_wide, 
 #                  cols = -id, 
  #                 names_to = "year",
   #                names_prefix = paste0("parcels_",th,"th."),
    #               values_to = "defo_pct",
#) 


#ddf <- readRDS(file = paste0("./panel_parcels_",PS/1000,"km_",th,"th.Rdata"))
#ddf <- st_as_sf(ddf, coords = c("x","y"))























####################################################################################################"
####################################################################################################"
#####                         TEST                   #######

# make the smaller fileS for the test. Don't need to do it again. 
mills_ca_sp <- as(mills_ca, "Spatial")

annualrastername <- "./annual_maps/defo_50th_10.tif"
annual_defo <- raster(annualrastername)
test <- crop(annual_defo, y = mills_ca_sp[1,], filename = "./test/defo_50th_10_test.tif", overwrite = TRUE)
plot(test)

annualrastername <- "./annual_maps/defo_50th_11.tif"
annual_defo <- raster(annualrastername)
test <- crop(annual_defo, y = mills_ca_sp[1,], filename = "./test/defo_50th_11_test.tif", overwrite = TRUE)
plot(test)

rm(test)
###################################################################################################"

years <- seq(from = 1998, to = 2015, by = 1)
t <- 10
threshold <- 50
th <- 50



### 1. build the function that will be called in the foreach loop: 

annual_aggregate <- function(t, threshold){
  
  ## Define which process we are in: 
  processname <- paste0("./test/defo_",threshold,"th_", t,"_test",".tif")
  
  #create unique filepath for temp directory
  dir.create(file.path(paste0(processname,"_Tmp")), showWarnings = FALSE)
  #set temp directory
  rasterOptions(tmpdir=file.path(paste0(processname,"_Tmp")))
  
  
  ## read in the indonesia wide raster of deforestation of year t, threshold th, computed in prepare_deforestation_maps.R 
  annual_defo <- raster(processname)
  
  #mask it with the mill influence polygon. 
  mask(annual_defo, total_ca_sp, 
       filename = paste0("./test/defo_",threshold,"th_", t,"_masked.tif"),
       overwrite = TRUE)
  rm(annual_defo)
  
  # read in the masked raster 
  maskedrastername <- paste0("./test/defo_",threshold,"th_", t,"_masked.tif")
  annual_defo_masked <- raster(maskedrastername)
  
  #aggregate it from the 30m cells to PSm cells with mean function. 
  raster::aggregate(annual_defo_masked, fact = c(PS/res(annual_defo_masked)[1], PS/res(annual_defo_masked)[2]), 
                    expand = FALSE, 
                    fun = mean,
                    filename = paste0("./test/parcels_", threshold, "th_", t, ".tif"),
                    overwrite = TRUE)
  rm(annual_defo_masked)
  
  
  #removes entire temp directory without affecting other running processes
 # unlink(file.path(paste0(processname,"_Tmp")), recursive = TRUE)
  
  
  return(paste0("./test/parcels_", threshold, "th_", t, ".tif"))
}

### 2. build the parallel-looping function
aggregate_parallel <- function(detected_cores, th){
  
  registerDoParallel(cores = detected_cores) 
  
  # the loop has arguments to define how the results of the workers should be combined, and to give "workers" (CPUs) 
  # the objects and packages they need to run the function. 
  foreach(t = 10:11, 
          #.combine = ,
          #.combine = list.files,
          .multicombine = F,
          .export = c("annual_aggregate", "years", "PS", "total_ca_sp"), 
          .packages = c("sp", "raster")) %dopar% 
    annual_aggregate(t, threshold = th) # the function that is parallelly applied to different years. 
}


### 3. run it for each forest definition (threshold 25, 50 or 75). 
th <- 50

th <- 25
while(th < 100){
  # compute the RasterStack object of 18 annual layers for this threshold
  tic()
  #read in the 18 layers and brick them
  parcels_brick <- brick(aggregate_parallel(detected_cores = 2, th = th))
  
  #write the brick
  writeRaster(parcels_brick, 
              filename = paste0("./test/bricked_parcels/parcels_", th, "th.tif"), 
              overwrite = TRUE)
  toc()
  th <- th + 25
}
showTmpFiles()
li <- 
s <- brick(list("./test/parcels_50th_10.tif", "./test/parcels_50th_11.tif"))

writeRaster(s, filename = paste0("./test/bricked_parcels/parcels_", th, "th.tif"),
            overwrite = TRUE) 
d <- brick(paste0("./test/bricked_parcels/parcels_", th, "th.tif"))
writeRaster(d, filename = paste0("./test/bricked_parcels/parcels_", th, "th_bricked.tif"),
            overwrite = TRUE)


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
### ATTENTION? ICI CE N'EST PAS UN PB, MAIS DANS LE VRAI CAS IL Y AURA ENCORE TOUTES LES CASES DANS LE MASK (NA DONC)
### ET ON NE VEUT PAS LES GARDER QUAND ON PASSE AU DATAFRAME.

############################################ END OF TEST ############################################################

#########################################################


