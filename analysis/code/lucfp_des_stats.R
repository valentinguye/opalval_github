### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#               LUCFP DESCRIPTIVE STATISTICS 


rm(list = ls())

##### 0. PACKAGES, WD, OBJECTS #####

### PACKAGES ###

# sf need to be installed from source for lwgeom te be installed from source. 
if (!require(sf)) install.packages("sf", source = TRUE)
#"plyr", 
neededPackages = c("dplyr", "data.table", "rlist", "here",
                   "foreign", "readxl", "readstata13",
                   "raster", "rgdal", "velox", "sp", "lwgeom", "rnaturalearth", 
                   "ggplot2", "leaflet", "htmltools",
                   "parallel", "foreach", "iterators", "doParallel")

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

# Install the snow package used to speed spatial analyses
if (!require(snow)) install.packages("snow")
library(snow)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### WORKING DIRECTORY ### 
setwd(here("analysis/input"))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### RASTER OPTIONS ### 
rasterOptions(chunksize = 1e+9,
              timer = TRUE)

### INDONESIAN CRS ### 
#   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
#   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator.
#   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is
#   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"




provinces <- st_read("IDN_adm/IDN_adm1.shp")
provinces <- dplyr::select(provinces, NAME_1)

provinces$island_name <- NA

provinces$island_name[provinces$NAME_1 == "Aceh" |
                       provinces$NAME_1 == "Bangka-Belitung" |
                       provinces$NAME_1 == "Bengkulu" |
                       provinces$NAME_1 == "Jambi" |
                       provinces$NAME_1 == "Kepulauan Riau" |
                       provinces$NAME_1 == "Lampung" |
                       provinces$NAME_1 == "Riau" |
                       provinces$NAME_1 == "Sumatera Barat" |
                       provinces$NAME_1 == "Sumatera Selatan" |
                       provinces$NAME_1 == "Sumatera Utara" ] <- "Sumatra"

provinces$island_name[provinces$NAME_1 == "Kalimantan Barat" |
                       provinces$NAME_1 == "Kalimantan Selatan" |
                       provinces$NAME_1 == "Kalimantan Tengah" |
                       provinces$NAME_1 == "Kalimantan Timur" |
                       provinces$NAME_1 == "Kalimantan Utara" ] <- "Kalimantan"

provinces$island_name[provinces$NAME_1 == "Papua" |
                      provinces$NAME_1 == "Irian Jaya Barat" ] <- "Papua"

island_sf <- provinces[!is.na(provinces$island_name),c("island_name", "geometry")]

IslandS <- c("Sumatra", "Kalimantan", "Papua")
for(Island in IslandS){
  island_sf$geometry[island_sf$island_name == Island] <- st_union(island_sf$geometry[island_sf$island_name == Island])
}

island_sf <- island_sf[!duplicated(island_sf$island_name),]

island_sf_prj <- st_transform(island_sf, crs = indonesian_crs)


#### Forest cover in 2000 #### 
make_stats_fc2000 <- function(island){
  ### Prepare UML mills
  uml <- read_xlsx(here("build/input/mill_geolocalization/mills_20200129.xlsx"))
  uml$latitude <- as.numeric(uml$latitude)
  uml$longitude <- as.numeric(uml$longitude)
  uml$lat <- uml$latitude
  uml$lon <- uml$longitude
  uml <- st_as_sf(uml,	coords	=	c("longitude",	"latitude"), crs = 4326)
  uml_prj <- st_transform(uml, crs = indonesian_crs)
  
  #define big catchment areas to have a large AOI.
  uml_ca <- list()
  CR <- 10
  while(CR < 60){
    uml_ca[[CR]] <- st_buffer(uml_prj, dist = CR*1000)
    
    # # work with squares rather than with circles
    # for(i in 1:length(uml_ca[[CR]])){
    #   uml_ca[[CR]][i] <- st_as_sfc(st_bbox(uml_ca[[CR]][i]))
    # }
    
    
    uml_ca[[CR]] <- st_union(st_geometry(uml_ca[[CR]]))
    
    # keep only the part of this total catchment area that is on our island of interest
    uml_ca[[CR]] <- st_intersection(x = uml_ca[[CR]], 
                                    y = island_sf_prj[island_sf_prj$island_name == island,])
    
    uml_ca[[CR]] <- st_transform(uml_ca[[CR]], crs = 4326)

    # coerce to a SpatialPolygon
    uml_ca[[CR]] <- uml_ca[[CR]] %>% st_geometry()
    #uml_ca[[CR]] <- as(uml_ca[[CR]], "Spatial")
    
    CR <- CR + 20
  }
  

  island_sf[island_sf$island_name == island,"geometry"] %>% plot() 
  uml_ca[[50]] %>% plot(add = T, col = "blue")
  uml_ca[[10]] %>% plot(add = T, col = "red")
  

### Prepare IBS_UML mills
ibs <- read.dta13(here("build/input/IBS_UML_cs.dta"))
ibs <- st_as_sf(ibs,	coords	=	c("lon",	"lat"), crs=4326)
ibs <- st_geometry(ibs)

#define big catchment areas to have a large AOI.
ibs_ca50 <- st_buffer(ibs, dist = 50000)
ibs_ca30 <- st_buffer(ibs, dist = 30000)
ibs_ca10 <- st_buffer(ibs, dist = 10000)

# work with squares rather than with circles
for(i in 1:length(ibs_ca)){
  ibs_ca[i] <- st_as_sfc(st_bbox(ibs_ca[i]))
}
total_ca <- st_union(st_geometry(mills_ca))
# coerce to a SpatialPolygon
total_ca_sp <- as(total_ca, "Spatial")

island <- "Sumatra" 
th <- 30

while(th < 100){
  thed_gfc_data <- brick(paste0(here("build/input/outcome_variables/gfc_data_"),island,"_",th,"th.tif"))
  # select the forestcover layer (band 1)
  fc2000 <- thed_gfc_data[[1]] 
  # remove useless other stack of gfc layers
  rm(thed_gfc_data)  

  
  # Unique cell area
  # once projected to indonesian_crs, the gfc raster has resolution(27.8 ; 27.6)
  cell_area <- 27.7

  # the output is the total area covered with forest in 2000 in the polygon of Sumatra. 
  sum_fc2000 <- raster::extract(fc2000, sumatra, fun = sum, na.rm = TRUE)
  
  
  # then we repeat this but with UML dissolved polygon and with ibs_uml dissolved polygon.  
  
  # compute the size of each cell with area(fc2000) 
  # puis regarder la proportion de 1 
  
  
  th <- th + 30
}



}









### VISUALIZATION

# provinces <- cbind(provinces, st_coordinates(st_centroid(provinces)))
# ggplot(data = provinces) +
#   geom_sf() +
#   geom_sf(data = provinces, fill = NA) + 
#   geom_text(data = provinces, aes(X, Y, label = NAME_1))

## Map building

# info you want the map to display
#provinces$popup <- provinces$NAME_1
# MAP
# provinces %>% 
#   leaflet() %>% 
#   addTiles()%>%
#   addProviderTiles(providers$Esri.WorldImagery, group ="ESRI") %>%
#   addPolygons(opacity = 0.5, color = "red", weight = 2, fill = FALSE, popup = ~provinces$NAME_1)#%>%
#   #addAwesomeMarkers(data = provinces, icon = icons, popup = ~provinces$NAME_1)
# 


## What we want is to get an integer of the area of grid cells = 1 in this fc2000 
## Two options, 
# 1. either compute cell specific area (but for now area(fc2000) returns a raster with only 0)
# 2. or approximate with a unique cell area. 

# 1. each cell has its area for value
# area(fc2000,
#      filename = "cell_area.tif", 
#      datatype = "INT2U", 
#      overwrite = TRUE)
# cell_area <- raster("cell_area.tif")
# # each cell has its area for value if it is covered with forest
# # cell_area could also be just replace by the average cell_area in the region (~30m2)
# rs <- stack(fc2000, cell_area)
# fc2000_area <- calc(fc2000, 
#                     fun = function(x){x*cell_area})


