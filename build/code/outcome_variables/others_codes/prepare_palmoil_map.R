######################################################################
#                                                                    #
#   Pal oil remote sensing data on Desa level                        #
#                                                                    #
#   Input: Palm oil remote sensing raw data                          #
#   Output: Palm oil remote sensing processed data                   #   
#        This file has !(a larger extent and) values in every cell   #
######################################################################
######################################################################
# LOAD OR INSTALL NECESSARY PACKAGES 

rm(list = ls())
# List all packages needed for session
neededPackages = c("dplyr", "raster", "sf", "foreign", "sp", "data.table",
                   "GISTools", "rgdal", "RColorBrewer", "plyr")
allPackages    = c(neededPackages %in% installed.packages()[ , "Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
  missingIDX = which(allPackages == FALSE)
  needed     = neededPackages[missingIDX]
  lapply(needed, install.packages)
}

# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)

#######################################################################

# Set up working directory 
file.path(PROJHOME, "/build/code", "Prepare_remote_sensing_palmoil.R")

######################################################################
######################################################################
### LOAD NECESSARY PALM OIL REMOTE SENSING DATA 

# Load palm oil remote sensing data (raster)
tif.all.names     <- list.files(path = "build/input/PALMOIL/IIASA_indo_oilpalm_map", pattern = ".tif")
other.names       <- list.files(path = "build/input/PALMOIL/IIASA_indo_oilpalm_map", pattern = ".tif.")
tif.names         <- tif.all.names[!(tif.all.names %in% other.names)]
data.file         <- list()

for (i in 1: length(tif.names)){
  
  file.path      <- paste0("build/input/PALMOIL/IIASA_indo_oilpalm_map/", tif.names[i])
  data.file[[i]] <- raster(file.path)
  
  # Change name of raster to identify year
  names(data.file[[i]]) <- tif.names[i]
}

# Check CRS of rasters
crs(data.file[[1]]) #WGS84

##################################################################
### PREPARE AND EXPORT DATA SET

# Transform raster layers such that all cells have a value (0 or 1)
for (i in 1:length(data.file)){
  
  # Raster value
  val.raster <- getValues(data.file[[i]])
  
  # Find empty cells
  IND.empty <- is.na(val.raster)
  
  # Create binary vector
  new.value             <- rep(1, length(val.raster))
  new.value[IND.empty]  <- 0
  
  # Replace original raster values with binary cell vector
  data.file[[i]] <-  setValues(data.file[[i]], values = new.value)
  
  # Extend raster to exactly match the extent of district and desa shapefile, new cells have value 0
  #extent_indo    <- extent(94.5,141.5,-11.5,6.5)
  #data.file[[i]] <- raster::extend(data.file[[i]], y=extent_indo, value=0)
  
  # Save raster files
  #name.vec <- paste0("build/output/data/", "new_" ,names(data.file[[i]]))
  #writeRaster(x = data.file[[i]], filename = name.vec, overwrite=TRUE) 
  #rm(val.raster, IND.empty, new.value)
  
}

##################################################################