######################################################################
#                                                                    #
#   Palm oil remote sensing data on Desa level                        #
#                                                                    #
#   Input: Crosswalk Desa, palm oil remote sensing data (processed)  #
#          desa 2015 shapefile                                       #
#   Output: Palm oil data on desa level                              #
#           + optionally plots                                       #
#   Dependecy: "Prepare_remote_sensing_palmoil.R"                    #
#              (creates processed remote sensing data)               #
######################################################################
######################################################################
# LOAD OR INSTALL NECESSARY PACKAGES 

rm(list = ls())
# List all packages needed for session
neededPackages = c("dplyr", "raster", "sf", "foreign", "sp", "data.table",
                   "GISTools", "rgdal", "RColorBrewer", "plyr", "tmap",
                   "rlist", "velox")
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
file.path(PROJHOME, "/build/code", "Desa_palmoil_remote_sensing.R")

######################################################################
######################################################################
### LOAD NECESSARY DATA 

# Load desa shapefile 
shp_all      <- as(st_zm(st_read("build/input/AREA/podes_bps2014.shp", 
                                 stringsAsFactors = FALSE)), Class = "Spatial")

# Transform to use sf package
desa_shp_all <- st_as_sf(shp_all)

# Check CRS
st_crs(desa_shp_all) #WGS84

# Load in desa 2000-2014 crosswalk
crosswalk_desa       <- read.csv("build/input/desa_crosswalk_2000_2014.csv")
crosswalk_desa[,"X"] <- NULL #

# Prepare to load palm oil remote sensing data (raster)
years             <- c(1995,2000,2005,2010,2015)
# Note: Files will be loaded one by one due to size

# Check CRS of rasters
# checked: WGS84

# Check CRS of desa shapefile
st_crs(desa_shp_all) #WGS84

##################################################################
### PREPARE DATA SETS

# Match desa 2013 codes with desa shapefile codes from 2013
IND.code.match           <- match(as.character(desa_shp_all$ID2013), crosswalk_desa$Id2013)

# Map Desa codes from 2000 onto shape file
desa_shp_all$desa_code_2000  <- crosswalk_desa[IND.code.match,]$Id2000
desa_shp_all$desa_name_2000  <- crosswalk_desa[IND.code.match,]$Nm2000

# Transform shapefile to spatial class
desa_sp <- as(st_zm(desa_shp_all), Class = "Spatial")

# Transform desa codes to characters
desa_sp@data[["desa_code_2000"]] <- as.character(desa_sp@data[["desa_code_2000"]])

# Add ID field to shapefile
desa_sp@data$ID<-c(1:length(desa_sp@data[,1]))

##################################################################
### Calculate mean statistics from raster for each polygon
## Steps: Load in palm oil shapefiles and extract means
# Note: Must run "Prepare_remote_sensing_palmoil.R" beforehand
stats_palm <-list()

for (i in 1:length(years)){
  
  file.path      <- paste0("build/output/data/new_oilpalm_", years[i], "_WGS1984.tif")
  data.file      <- raster(file.path)
  
  # Change name of raster to identify year
  names(data.file) <-  paste0("palmoil_", years[i])
  
  # Create velox raster files (and crop to desa_sp extent
  velox.raster <- velox(x = data.file, extent = extent(desa_sp))
  
  # Extract mean palm oil density per polygon
  stats_palm[[i]] <- velox.raster$extract(desa_sp, fun=mean)
  
  rm(file.path,data.file,velox.raster)
}

# Unlist and merge palm oil data
palm.oil.vec           <- as.data.frame(list.cbind(stats_palm))
vec.names.data         <- paste0("palmoil_", years)         
colnames(palm.oil.vec) <- vec.names.data
palm.oil.vec$ID        <- 1:nrow(palm.oil.vec)
desa_sp@data           <- plyr::join(desa_sp@data, palm.oil.vec, by="ID")

rm(stats_palm, palm.oil.vec)

# Replace empty cells with 0 in mean palm oil variables
# Note: Empty cells/missing values are caused by the fact that the oil palm raster file's 
# extent (latitude) is smaller than the desa shapefile
replace_zero                     <- function(y){replace(y, is.na(y), 0)}
desa_sp@data[vec.names.data]     <- apply(desa_sp@data[vec.names.data], 
                                          2, replace_zero)

# Calculate area in sqkm for each desa
desa_sp@data$arem_sqkm <- as.numeric(st_area(st_as_sf(desa_sp)))/1000000#in km^2

# Define area per desa with palm oil in sqkm
desa_sp@data <- desa_sp@data %>% mutate(
  palm_oil_1995_PO_area_95	= palmoil_1995*arem_sqkm,
  palm_oil_2000_PO_area_00	= palmoil_2000*arem_sqkm,
  palm_oil_2005_PO_area_05	= palmoil_2005*arem_sqkm,
  palm_oil_2010_PO_area_10	= palmoil_2010*arem_sqkm,
  palm_oil_2015_PO_area_15	= palmoil_2015*arem_sqkm  )

# Transform data for export
desa_export <- st_as_sf(desa_sp)
desa_export <- st_set_geometry(desa_export, NULL) # drop geometry column

# Export as shapefile
writeOGR(obj=desa_sp, dsn="build/output/data/desa_2014_palmoil.shp",
         layer = "desa_sp", driver="ESRI Shapefile")  

# Export as csv file
write.csv(x = desa_export, file = "build/output/data/desa_2014_palmoil.csv")

##################################################################
### Calculate mean statistics from raster for collapsed polygons from base year 2000

# Add ID to observations with missing baseline code (to avoid that all NAs are collapsed into one shape)
vec_new_code                <- paste0(desa_sp@data$ID2013, "_new")
desa_sp@data$desa_code_2000 <- ifelse(is.na(desa_sp@data$desa_code_2000),
                               vec_new_code, desa_sp@data$desa_code_2000 ) 

# Collapse polygons back to year 2000
# Note: Adjust variable name ("desa_code_2000") if desa_sp is loaded from existing file
desa_sp_2000 <- raster::aggregate(desa_sp, by = "desa_code_2000") 

## Add information on area of base year desas
area_info <- desa_export %>% 
  group_by(desa_code_2000) %>%
  dplyr::summarize(arem_sqkm = sum(arem_sqkm))

desa_sp_2000@data <- dplyr::left_join(desa_sp_2000@data, 
                                          area_info, by= "desa_code_2000")
# Add ID field to shapefile
desa_sp_2000@data$ID<-c(1:length(desa_sp_2000@data[,1]))

# Calculate mean palm oil statistics
stats_palm_2000 <-list()

# Calculate mean statistics
for (i in 1:length(years)){
  
  file.path      <- paste0("build/output/data/new_oilpalm_", years[i], "_WGS1984.tif")
  data.file      <- raster(file.path)
  
  # Change name of raster to identify year
  names(data.file) <-  paste0("palmoil_", years[i])
  
  # Create velox raster files (and crop to desa_sp extent
  velox.raster <- velox(x = data.file, extent = extent(desa_sp))
  
  # Extract mean palm oil density per polygon
  stats_palm_2000[[i]]  <- velox.raster$extract(desa_sp_2000, fun=mean)
  
  rm(file.path,data.file,velox.raster)
}


# Unlist and merge palm oil data for base year 2000
palm.oil.vec.2000           <- as.data.frame(list.cbind(stats_palm_2000))
colnames(palm.oil.vec.2000) <- vec.names.data
palm.oil.vec.2000$ID        <- 1:nrow(palm.oil.vec.2000)
desa_sp_2000@data       <- plyr::join(desa_sp_2000@data, palm.oil.vec.2000, by="ID")

rm(stats_palm_2000, palm.oil.vec.2000)

# Replace empty cells with 0 in mean palm oil variables
# Note: Empty cells/missing values are caused by the fact that the oil palm raster file's 
# extent (latitude) is smaller than the desa shapefile
desa_sp_2000@data[vec.names.data]  <- apply(desa_sp_2000@data[vec.names.data], 
                                                2, replace_zero)

# Define area per desa with palm oil in sqkm
desa_sp_2000@data <- desa_sp_2000@data %>% 
  mutate(
    palm_oil_1995_PO_area_95	= palmoil_1995*arem_sqkm,
    palm_oil_2000_PO_area_00	= palmoil_2000*arem_sqkm,
    palm_oil_2005_PO_area_05	= palmoil_2005*arem_sqkm,
    palm_oil_2010_PO_area_10	= palmoil_2010*arem_sqkm,
    palm_oil_2015_PO_area_15	= palmoil_2015*arem_sqkm  )

# Match back desa 2000 name
name.vec.match                     <- match(desa_sp_2000@data$desa_code_2000, crosswalk_desa$Id2000) 
desa_sp_2000@data$desa_name_2000   <- crosswalk_desa[name.vec.match,]$Nm2000

# Transform data for export
desa_export_2000 <- st_as_sf(desa_sp_2000)
desa_export_2000 <- st_set_geometry(desa_export_2000, NULL) # drop geometry column

# Export as shapefile
writeOGR(obj=desa_sp_2000, dsn="build/output/data/desa_2014_palm_base2000.shp", 
         layer = "desa_sp_2000", driver="ESRI Shapefile")  

# Export as csv file
write.csv(x = desa_export_2000, file = "build/output/data/desa_2014_palm_base2000.csv")

##################################################################
# ### Plotting: Compare for visual impression
# 
# # For plotting, uncomment line below
# # Use "ctrl + shift + c" for multi-line (un)commenting
# # Note: If you do not use a loaded desa_sp file, column names must be adjusted
# 
# # Use desa 2015 map for visualization
# desa_sp                   <- readOGR( dsn = "build/output/data/desa_2014_palmoil.shp",
#                                      layer = "desa_2014_palmoil",
#                                      stringsAsFactors = FALSE)
# 
# # Create bounding box and a sample
# sample_a              <- as(extent(c(97,101,1,5)), "SpatialPolygons")
# proj4string(sample_a) <- CRS(proj4string(desa_sp))
# sample_a              <- raster::intersect(desa_sp, sample_a)
# 
# # Transform 2015 oil palm raster data to polygons for comparison
# sample_a_2015         <- raster::intersect(data.file[[5]], sample_a) # take 2015 palm oil raster
# sample_a_2015         <- rasterToPolygons(sample_a_2015, fun=NULL, n=4,
#                                           na.rm=TRUE, digits=12, dissolve=FALSE)
# 
# # Map 1: Shows palmoil remote estmiated mean for each desa
# (map1 <-  tm_shape(shp = sample_a) +
#           tm_polygons(col = "pl_2015", convert2density = FALSE, # add coloured polygons
#                 palette = "Greens",
#                 title = "Palm oil mean",
#                 breaks = seq(from = 0, to = 1, by = 0.25),
#                 border.alpha = 1) +  # set to zero to make borders transparent
#           tm_legend(position=c(0,0), frame=FALSE))
# 
# 
# # Map 2: Shows orginal 2015 palm oil raster
# ## takes quite long
# (map2 <-   tm_shape(shp = sample_a_2015)  +
#            tm_polygons(col = "palmoil_2015", alpha = 0.5, border.col = "grey", border.alpha = 0.1,
#                        palette = "Greens"))
# 
# tmap_save(map1, filename = "palm_oil_2015_1.png", width = 2000, height = 1500)
# tmap_save(map2, filename = "palm_oil_2015_2.png", width = 2000, height = 1500)
