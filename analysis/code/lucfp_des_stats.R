### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#               LUCFP DESCRIPTIVE STATISTICS 


rm(list = ls())

##### 0. PACKAGES, WD, OBJECTS #####

### PACKAGES ###

# sf need to be installed from source for lwgeom te be installed from source. 
if (!require(sf)) install.packages("sf", source = TRUE)
#"plyr", 
neededPackages = c("dplyr", "tidyr", "data.table", "rlist", "stringr", "sjmisc", "here", 
                   "foreign", "readxl", "readstata13", 
                   "knitr", "kableExtra",
                   "raster", "rgdal", "velox", "sp", "lwgeom", "rnaturalearth", "gfcanalysis",
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

#INSTALL GFC ANALYSIS
# Install the snow package used to speed spatial analyses
if (!require(snow)) install.packages("snow")
library(snow)
# Install Alex's gfcanalysis package
if (!require(gfcanalysis)) install.packages('gfcanalysis')
library(gfcanalysis)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### WORKING DIRECTORY ### 
setwd(here("analysis/input"))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


### INDONESIAN CRS ### 
#   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
#   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator.
#   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is
#   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

##### Prepare polygons of three Indonesian islands of interest #####
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

## export to GEE 
st_write(island_sf, "island_sf", driver = "ESRI Shapefile", delete_dsn = TRUE)



##### Prepare polygons of mill catchment radius #####
# We would like a multipolygon Simple feature collection with X features and 1 field
# The one field describes the feature (i.e. polygon).
# There is one feature for each CR for each mill group (IBS or UML) within each island (so 3*2*3 = 18 features). 

# Create empty list for shapes and shape descriptions
shape_des <- list()
mill_cr <- list()

# Fill the lists with shapes and shape descriptions (names)
IslandS <- c("Sumatra", "Kalimantan", "Papua")
catchment_radius <- c(10, 30, 50)
for(island in IslandS){
  ### Island lists
  shape_des[[match(island, IslandS)]] <- list()
  mill_cr[[match(island, IslandS)]] <- list()
  
  ## Prepare UML mills
  uml <- read_xlsx(here("build/input/mill_geolocalization/mills_20200129.xlsx"))
  uml$latitude <- as.numeric(uml$latitude)
  uml$longitude <- as.numeric(uml$longitude)
  uml$lat <- uml$latitude
  uml$lon <- uml$longitude
  uml <- st_as_sf(uml,	coords	=	c("longitude",	"latitude"), crs = 4326)
  uml <- st_geometry(uml)
  uml_prj <- st_transform(uml, crs = indonesian_crs)
  
  ## UML lists
  shape_des[[match(island, IslandS)]][[1]] <- list()
  mill_cr[[match(island, IslandS)]][[1]] <- list()
  
  for(CR in catchment_radius){
    ## create union of individual catchment areas.
    # buffer
    shape <- st_buffer(uml_prj, dist = CR*1000)
    # work with squares rather than with circles
    # for(i in 1:length(shape)){
    #   shape[i] <- st_as_sfc(st_bbox(shape[i]))
    # }
    shape <- sapply(shape, FUN = function(x){st_as_sfc(st_bbox(x))}) %>% st_sfc(crs = indonesian_crs)

    shape <- st_union(shape)
    # keep only the part of this total catchment area that is on our island of interest
    shape <- st_intersection(x = shape, y = island_sf_prj[island_sf_prj$island_name == island,])
    shape <- st_transform(shape, crs = 4326)
    
    # add name and geometry to lists
    shape_des[[match(island, IslandS)]][[1]][[match(CR, catchment_radius)]] <- paste0(island,"_uml_",CR,"km")
    mill_cr[[match(island, IslandS)]][[1]][[match(CR, catchment_radius)]] <-  shape %>% st_geometry()
  }  
  
  ## Prepare IBS mills
  ibs <- read.dta13(here("build/input/IBS_UML_cs.dta"))
  ibs <- st_as_sf(ibs,	coords	=	c("lon",	"lat"), crs=4326)
  ibs <- st_geometry(ibs)
  ibs_prj <- st_transform(ibs, crs = indonesian_crs)
  
  ## IBS lists
  shape_des[[match(island, IslandS)]][[2]] <- list()
  mill_cr[[match(island, IslandS)]][[2]] <- list()
  
  for(CR in catchment_radius){
    ## create union of individual catchment areas.
    # buffer
    shape <- st_buffer(ibs_prj, dist = CR*1000)
    # work with squares rather than with circles
    # for(i in 1:length(shape)){
    #   shape[i] <- st_as_sfc(st_bbox(shape[i]))
    # }
    shape <- sapply(shape, FUN = function(x){st_as_sfc(st_bbox(x))}) %>% st_sfc(crs = indonesian_crs)
    
    shape <- st_union(shape)
    
    # keep only the part of this total catchment area that is on our island of interest
    shape <- st_intersection(x = shape, y = island_sf_prj[island_sf_prj$island_name == island,])
    shape <- st_transform(shape, crs = 4326)
    
    # add name and geometry to lists
    shape_des[[match(island, IslandS)]][[2]][[match(CR, catchment_radius)]] <- paste0(island,"_ibs_",CR,"km")
    mill_cr[[match(island, IslandS)]][[2]][[match(CR, catchment_radius)]] <-  shape %>% st_geometry()
    }    
}

# flatten the lists
shape_des <- unlist(shape_des)
mill_cru <- unlist(mill_cr, recursive = FALSE) 
mill_cru <- unlist(mill_cru, recursive = FALSE) 
mill_cru <- unlist(mill_cru, recursive = FALSE)

# make the sf object
mill_cr_sfc <- st_as_sfc(mill_cru)
mill_cr_sf <- st_sf(mill_cr_sfc, crs = 4326)
mill_cr_sf$shape_des <- shape_des

# export to GEE 
st_write(mill_cr_sf, "mill_cr_sf_bbox", 
         driver = "ESRI Shapefile", 
         delete_dsn = TRUE, 
         overwrite = TRUE)



##### Forest cover in 2000 ##### 

### Make an ordered data frame that is easily tabulized afterwards. 

fc2000_list <- list()
th <- 30
while(th < 100){
  # Stack island sums and mill_cr sums
  fc2000_islands <- st_read(paste0("C:/Users/GUYE/Google Drive/opal/GEE_outputs/FC2000_",th,"th_island_ha/FC2000_",th,"th_island_ha.shp"))
  fc2000_islands <- fc2000_islands %>% st_drop_geometry()
  names(fc2000_islands)[names(fc2000_islands) == "islnd_n"] <- "shape_des"

  fc2000_mill_cr <- st_read(paste0("C:/Users/GUYE/Google Drive/opal/GEE_outputs/FC2000_",th,"th_mill_cr_bbox_ha/FC2000_",th,"th_mill_cr_bbox_ha.shp"))
  fc2000_mill_cr <- fc2000_mill_cr %>% st_drop_geometry()
  
  fc2000_th <- rbind(fc2000_islands, fc2000_mill_cr) 
  
  # convert the sum value from hectare to million hectare 
  fc2000_th$sum <- fc2000_th$sum/1e6
  fc2000_th$sum <- fc2000_th$sum %>% round(digits = 2)

  # change the one column name
  names(fc2000_th)[names(fc2000_th) == "sum"] <- paste0(th,"%")
  
  # order the data frame in the way we want the output table. 
  # For this, one needs row names
  row.names(fc2000_th) <- fc2000_th$shape_des
  fc2000_th <- fc2000_th[c("Sumatra_ibs_10km",
                           "Sumatra_ibs_30km",
                           "Sumatra_ibs_50km",
                           "Sumatra_uml_10km",
                           "Sumatra_uml_30km",
                           "Sumatra_uml_50km",
                           "Sumatra",
                           "Kalimantan_ibs_10km",
                           "Kalimantan_ibs_30km",
                           "Kalimantan_ibs_50km",
                           "Kalimantan_uml_10km",
                           "Kalimantan_uml_30km",
                           "Kalimantan_uml_50km",
                           "Kalimantan",
                           "Papua_ibs_10km",
                           "Papua_ibs_30km",
                           "Papua_ibs_50km",
                           "Papua_uml_10km",
                           "Papua_uml_30km",
                           "Papua_uml_50km",
                           "Papua"), ]
                         
  fc2000_list[[th/30]] <- fc2000_th
  
  th <- th + 30  
} 

fc2000 <- cbind(fc2000_list[[1]], fc2000_list[[2]], fc2000_list[[3]])
fc2000 <- fc2000[,c("30%", "60%", "90%")]

# simplifying row names makes them not unique, which is not accepted in a data frame but is in a matrix. 
  fc2000 <- as.matrix(fc2000)
  
  row.names(fc2000) <- str_replace(string = row.names(fc2000), 
                                      pattern = "Sumatra_", 
                                      replacement = "")
  row.names(fc2000) <- str_replace(string = row.names(fc2000), 
                                      pattern = "Sumatra", 
                                      replacement = "Total island")
  row.names(fc2000) <- str_replace(string = row.names(fc2000), 
                                      pattern = "Kalimantan_", 
                                      replacement = "")
  row.names(fc2000) <- str_replace(string = row.names(fc2000), 
                                      pattern = "Kalimantan", 
                                      replacement = "Total island")
  row.names(fc2000) <- str_replace(string = row.names(fc2000), 
                                      pattern = "Papua_", 
                                      replacement = "")
  row.names(fc2000) <- str_replace(string = row.names(fc2000), 
                                      pattern = "Papua", 
                                      replacement = "Total island")
  
  for(KM in c(10, 30, 50)){
    row.names(fc2000) <- str_replace(string = row.names(fc2000), 
                                        pattern = paste0("ibs_",KM,"km"), 
                                        replacement = paste0("Within ",KM,"km of an IBS mill"))
    
  
    row.names(fc2000) <- str_replace(string = row.names(fc2000), 
                                        pattern = paste0("uml_",KM,"km"), 
                                        replacement = paste0("Within ",KM,"km of an UML mill"))
  }



##### ACCUMULATED LUCFP #####
tic()
rasterOptions(maxmemory = 1e10, chunksize = 1e9)

island_sf <- st_read("island_sf")
names(island_sf)[names(island_sf) == "islnd_n"] <- "shape_des"
island_sf_prj <- st_transform(island_sf, crs = indonesian_crs)

mill_cr_sf <- st_read("mill_cr_sf_bbox")
mill_cr_sf_prj <- st_transform(mill_cr_sf, crs = indonesian_crs)

all_shapes <- rbind(island_sf_prj, mill_cr_sf_prj)
all_shapes$shape_des <- as.character(all_shapes$shape_des)

### Extract LUCFP for all shapes of interest (in R and not GEE here because not too slow because already aggregated)
IslandS <- c("Sumatra", "Kalimantan", "Papua")
#island <- "Sumatra"
 
lucfp_thresholds <- list()

parcel_size <- 3000
th <- 30
while(th < 100){
  
  lucfp_islands<- list()
  
  for(island in IslandS){
    brick_lucfp <- brick(here(paste0("build/input/outcome_variables/bricked_parcels/parcels_",island,"_",parcel_size/1000,"km_",th,"th.tif")))
    
    # remove layers of years aftr 2015
    brick_lucfp <- raster::subset(brick_lucfp, c(1:15))
    # Add up annual aggregated LUCFP
    accu_lucfp <- calc(brick_lucfp, fun = sum, na.rm = TRUE)
    
    # select shapes of that island 
    shapes_in_island <- all_shapes[str_contains(x = island, 
                                                pattern = all_shapes$shape_des, 
                                                switch = TRUE),]
    
    # make velox object
    v.accu_lucfp <- velox(accu_lucfp)
    
    lucfp_islands[[match(island, IslandS)]] <- matrix(nrow = nrow(shapes_in_island),
                                                      ncol = 2)
    colnames(lucfp_islands[[match(island, IslandS)]]) <- c("shape_des", "sum")

    for(i in 1:nrow(shapes_in_island)){
      lucfp_islands[[match(island, IslandS)]][i,"shape_des"] <- shapes_in_island$shape_des[i]
      lucfp_islands[[match(island, IslandS)]][i, "sum"] <- v.accu_lucfp$extract(shapes_in_island[i,"geometry"],
                                       fun = function(x)sum(x, na.rm = TRUE))
    }
  }
  # stack the extracted values from shapes of three islands
  lucfp_th <- rbind(lucfp_islands[[1]], lucfp_islands[[2]], lucfp_islands[[3]]) %>% as.data.frame()
  
  # "de-factor" variables
  lucfp_th$shape_des <-lucfp_th$shape_des %>% as.character() 
  lucfp_th$sum <- lucfp_th$sum %>% as.character %>% as.numeric()

  # convert the sum value from number of pixels of resolution 27.7m (i.e; 767.29 square meters) to million hectare 
  pixel_area <- (27.7^2)/(1e10) # 1e10 is the convertion factor between a square meter and a MILLION hectare.  
  lucfp_th$sum <- lucfp_th$sum*pixel_area
  lucfp_th$sum <- lucfp_th$sum %>% round(digits = 2)
  
  # change the one column name
  names(lucfp_th)[names(lucfp_th) == "sum"] <- paste0(th,"%")
  
  # order the data frame in the way we want the output table. 
  # For this, one needs row names
  row.names(lucfp_th) <- lucfp_th$shape_des
  lucfp_th <- lucfp_th[c("Sumatra_ibs_10km",
                           "Sumatra_ibs_30km",
                           "Sumatra_ibs_50km",
                           "Sumatra_uml_10km",
                           "Sumatra_uml_30km",
                           "Sumatra_uml_50km",
                           "Sumatra",
                           "Kalimantan_ibs_10km",
                           "Kalimantan_ibs_30km",
                           "Kalimantan_ibs_50km",
                           "Kalimantan_uml_10km",
                           "Kalimantan_uml_30km",
                           "Kalimantan_uml_50km",
                           "Kalimantan",
                           "Papua_ibs_10km",
                           "Papua_ibs_30km",
                           "Papua_ibs_50km",
                           "Papua_uml_10km",
                           "Papua_uml_30km",
                           "Papua_uml_50km",
                           "Papua"), ]
  
  lucfp_thresholds[[th/30]] <- lucfp_th
  th <- th + 30                         
}

# column bind the three dataframes  
lucfp <- cbind(lucfp_thresholds[[1]], lucfp_thresholds[[2]], lucfp_thresholds[[3]])
# remove the shape_des columns
lucfp <- lucfp[,c("30%", "60%", "90%")]

# simplifying row names makes them not unique, which is not accepted in a data frame but is in a matrix. 
lucfp <- as.matrix(lucfp)

row.names(lucfp) <- str_replace(string = row.names(lucfp), 
                                 pattern = "Sumatra_", 
                                 replacement = "")
row.names(lucfp) <- str_replace(string = row.names(lucfp), 
                                 pattern = "Sumatra", 
                                 replacement = "Total island")
row.names(lucfp) <- str_replace(string = row.names(lucfp), 
                                 pattern = "Kalimantan_", 
                                 replacement = "")
row.names(lucfp) <- str_replace(string = row.names(lucfp), 
                                 pattern = "Kalimantan", 
                                 replacement = "Total island")
row.names(lucfp) <- str_replace(string = row.names(lucfp), 
                                 pattern = "Papua_", 
                                 replacement = "")
row.names(lucfp) <- str_replace(string = row.names(lucfp), 
                                 pattern = "Papua", 
                                 replacement = "Total island")

for(KM in c(10, 30, 50)){
  row.names(lucfp) <- str_replace(string = row.names(lucfp), 
                                   pattern = paste0("ibs_",KM,"km"), 
                                   replacement = paste0("Within ",KM,"km of an IBS mill"))
  
  
  row.names(lucfp) <- str_replace(string = row.names(lucfp), 
                                   pattern = paste0("uml_",KM,"km"), 
                                   replacement = paste0("Within ",KM,"km of an UML mill"))
}


lucfp
toc() # 1571s. 


##### Print the LateX table code #####
options(knitr.table.format = "latex") 

LU_stat_des <- cbind(fc2000, lucfp)
colnames(LU_stat_des) <- NULL

kable(LU_stat_des, booktabs = T, align = "r") %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  add_header_above(c("Canopy closure:" = 1, 
                     "30%" = 1, "60%" = 1, "90%" = 1, 
                     "30%" = 1, "60%" = 1, "90%" = 1), 
                   align = "r", 
                   strikeout = F) %>% 
  add_header_above(c(" ", 
                     "Forest cover \n in 2000, Mha" = 3, 
                     "Accumulated LUCFP \n 2001-2015, Mha" = 3), 
                   bold = T, 
                   align = "c") %>% 
  pack_rows(index = c("Sumatra" = 7, 
                      "Kalimantan" = 7,
                      "Papua" = 7))  %>%
  column_spec(column = c(2:7),
              width = "3em")













fc2000_islands <- list()
th <- 30
while(th < 100){
  fc2000_islands[[th/30]] <- st_read(paste0("C:/Users/GUYE/Google Drive/opal/GEE_outputs/FC2000_",th,"th_island_ha/FC2000_",th,"th_island_ha.shp"))
  names(fc2000_islands[[th/30]])[names(fc2000_islands[[th/30]]) == "islnd_n"] <- "island_and_threshold"
  fc2000_islands[[th/30]]$island_and_threshold <- paste0(fc2000_islands[[th/30]]$island_and_threshold,"_",th,"th")
  
  th <- th + 30 
}

fc2000 <- rbind(fc2000_islands[[1]],fc2000_islands[[2]],fc2000_islands[[3]])



fc2000_islands30 <- st_read("C:/Users/GUYE/Google Drive/opal/GEE_outputs/FC2000_30th_island_ha/FC2000_30th_island_ha.shp")
fc2000_islands60 <- st_read("C:/Users/GUYE/Google Drive/opal/GEE_outputs/FC2000_60th_island_ha/FC2000_60th_island_ha.shp")
fc2000_islands90 <- st_read("C:/Users/GUYE/Google Drive/opal/GEE_outputs/FC2000_90th_island_ha/FC2000_90th_island_ha.shp")














### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# island <- "Sumatra"
# threshold <- 30
make_cr_polygons_uml <- function(island){
  ### Prepare UML mills
  uml <- read_xlsx(here("build/input/mill_geolocalization/mills_20200129.xlsx"))
  uml$latitude <- as.numeric(uml$latitude)
  uml$longitude <- as.numeric(uml$longitude)
  uml$lat <- uml$latitude
  uml$lon <- uml$longitude
  uml <- st_as_sf(uml,	coords	=	c("longitude",	"latitude"), crs = 4326)
  uml_prj <- st_transform(uml, crs = indonesian_crs)
  
  uml_cr <- list()
  CR <- 10
  while(CR < 60){
    uml_cr[[CR]] <- st_buffer(uml_prj, dist = CR*1000)
    
    # # work with squares rather than with circles
    # for(i in 1:length(uml_cr[[CR]])){
    #   uml_cr[[CR]][i] <- st_as_sfc(st_bbox(uml_cr[[CR]][i]))
    # }
    
    
    uml_cr[[CR]] <- st_union(st_geometry(uml_cr[[CR]]))
    
    # keep only the part of this total catchment area that is on our island of interest
    uml_cr[[CR]] <- st_intersection(x = uml_cr[[CR]], 
                                    y = island_sf_prj[island_sf_prj$island_name == island,])
    
    uml_cr[[CR]] <- st_transform(uml_cr[[CR]], crs = 4326)

    # coerce to a SpatialPolygon
    uml_cr[[CR]] <- uml_cr[[CR]] %>% st_geometry()
    #uml_cr[[CR]] <- as(uml_cr[[CR]], "Spatial")
    
    CR <- CR + 20
  }
  
return(uml_cr)  
}  

  # island_sf[island_sf$island_name == island,"geometry"] %>% plot() 
  # uml_cr[[50]] %>% plot(add = T, col = "blue")
  # uml_cr[[10]] %>% plot(add = T, col = "red")
  
make_cr_polygons_ibs <- function(island){
  ### Prepare IBS_UML mills
  ibs <- read.dta13(here("build/input/IBS_UML_cs.dta"))
  ibs <- st_as_sf(ibs,	coords	=	c("lon",	"lat"), crs=4326)
  ibs <- st_geometry(ibs)
  ibs_prj <- st_transform(ibs, crs = indonesian_crs)
  
  ibs_cr <- list()
  CR <- 10
  while(CR < 60){
    ibs_cr[[CR]] <- st_buffer(ibs_prj, dist = CR*1000)
    
    ibs_cr[[CR]] <- st_union(st_geometry(ibs_cr[[CR]]))
    
    # keep only the part of this total catchment area that is on our island of interest
    ibs_cr[[CR]] <- st_intersection(x = ibs_cr[[CR]], 
                                    y = island_sf_prj[island_sf_prj$island_name == island,])
    
    ibs_cr[[CR]] <- st_transform(ibs_cr[[CR]], crs = 4326)
    
    # coerce to a SpatialPolygon
    ibs_cr[[CR]] <- ibs_cr[[CR]] %>% st_geometry()
    #ibs_cr[[CR]] <- as(ibs_cr[[CR]], "Spatial")
    
    CR <- CR + 20
  }
  
return(ibs_cr)
}



make_cr_polygons_uml("Sumatra")




















make_stats_fc2000 <- function(island, threshold){

  ### Extract forest cover 2000 pixels in different polygons
  areas_fc2000 <- matrix(nrow = 3, 
                         ncol = 3, 
                         dimnames = list(c("10km_catchment_radius", "30km_catchment_radius", "50km_catchment_radius"),
                                         c("area_in_island", "area_in_uml_cr", "area_in_ibs_cr")))

  thed_gfc_data <- brick(paste0(here("build/input/outcome_variables/gfc_data_"),island,"_",threshold,"th.tif"))
  # select the forestcover layer (band 1)
  fc2000 <- thed_gfc_data[[1]] 
  # remove useless other stack of gfc layers
  #rm(thed_gfc_data)  
  
  
  ## compute area of forest cover on the island
  # areas_fc2000[,"area_in_island"] <- raster::extract(fc2000, 
  #                                                    island_sf[island_sf$island_name == island,"geometry"],
  #                                                    fun = sum, 
  #                                                    na.rm = TRUE, 
  #                                                    progress = "text")
  
  aoi_island <- island_sf[island_sf$island_name == island,"geometry"] %>% as("Spatial")
  #aoi_island$label <- island
  
  rasterOptions(progress = "text")
  
  # 60k seconds and a 9Gb tmp file (with chunksize 2e+9). 
  stat_list <- gfc_stats(aoi = aoi_island, 
                         thed_gfc_data, 
                         dataset = "GFC-2018-v1.6")
  saveRDS(stat_list, 
          paste0(here("analysis/output/gfc_stats_output_"),island,"_",threshold,"th.Rdata"))
  
  print("area_in_island done")
  print(Sys.time())
  
  CR <- 10
  while(CR < 60){  
    ## compute area of forest cover within catchment radius of all UML mills
    areas_fc2000[paste0(CR,"km_catchment_radius"), "area_in_uml_cr"] <- raster::extract(fc2000,
                                                                                        uml_cr[[CR]], 
                                                                                        fun = sum, 
                                                                                        na.rm = TRUE, 
                                                                                        progress = "text")
    
    print(paste0(CR, "CR UML done"))
    print(Sys.time())
    
    ## compute area of forest cover within catchment radius of IBS_UML mills only
    areas_fc2000[paste0(CR,"km_catchment_radius"), "area_in_ibs_cr"] <- raster::extract(fc2000,
                                                                                        ibs_cr[[CR]], 
                                                                                        fun = sum, 
                                                                                        na.rm = TRUE, 
                                                                                        progress = "text")
    print(paste0(CR, "CR IBS done"))
    print(Sys.time())
    
    CR <- CR + 20
    }
  
  rm(fc_2000, uml_cr, ibs_cr)
  # at this point, each value in areas_fc2000 is the count of pixels covered with forest. 
  # This value is converted to an area by approximating each pixel's area with a unique value (in m2): 27.7
  # (because once projected to indonesian_crs, the gfc raster has resolution(27.8 ; 27.6))
  areas_fc2000 <- areas_fc2000*27.7
  
  saveRDS(areas_fc2000, paste0(here("analysis/output/areas_fc2000_"),island, "_",threshold,"th.Rdata"))
  
  return(areas_fc2000)
}

tic()
make_stats_fc2000(island = "Sumatra", threshold = 30)
toc()







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


