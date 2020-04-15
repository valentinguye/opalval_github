### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Program to compute the number of UML mills each parcel can reach within 10, 30 and 50km. 
# 
#   Inputs: UML most complete version
#           --> UML_valentin_imputed_est_year.dta
# 
#           parcel panel from previous step (i.e. making weighted averages)
#           --> pattern: wa_panel_parcels_ ; for each parcel_size and catchment_radius combination
# 
#   Outputs: parcel panel with 3 new columns: the parcel and time varying numbers of 
#             UML mills reachable within 10, 30 and 50km. 
#           --> pattern panel_parcels_reachable_uml_ ; for each parcel_size and catchment_radius combination
# 
# 

rm(list = ls())

# PACKAGES
#install.packages("sf", source = TRUE)
library(sf)

neededPackages = c("tidyverse","data.table", "readxl","foreign", "data.table", "readstata13", "here",
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
setwd(here("/build/input"))


#### Define projection ####
#   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
#   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator. 
#   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is 
#   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0 
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

years <- seq(1998, 2015, 1)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# read the most complete version of UML we have. 
uml <- read.dta13(here("/build/output/UML_valentin_imputed_est_year.dta"))
uml <- uml[!is.na(uml$lat),]
uml <- st_as_sf(uml, coords = c("lon", "lat"), crs = 4326)
uml <- st_transform(uml, crs = indonesian_crs)


make_n_reachable_uml <- function(parcel_size, catchment_radius){
  
  # read the parcel panel
  parcels <- readRDS(here(paste0("/build/output/wa_panel_parcels_",
                                 parcel_size/1000,"km_",
                                 catchment_radius/1000,"CR.rds")))
  
  # keep only a cross section and variables needed
  parcels_centro <- parcels_centro[parcels_centro$year == 1998, c("parcel_id", "lat", "lon")]
  
  # turn it into a sf object (lon lat are already expressed in indonesian crs)
  parcels_centro <- st_as_sf(parcels_centro, coords = c("lon", "lat"), remove = T, crs = indonesian_crs)
  
  CR <- 10000
  while(CR < 60000){
    parcels$newv <- rep(0, nrow(parcels))

    for(t in 1:length(years)){
      
      present_uml <- uml[uml$est_year_imp <= years[t] | is.na(uml$est_year_imp),]
      
      annual_reachable_uml <- st_is_within_distance(parcels_centro, present_uml, dist = CR)
      parcels[parcels$year == years[t], "newv"] <- lengths(annual_reachable_uml) 
    
    } 
    
  colnames(parcels)[colnames(parcels) == "newv"] <- paste0("n_reachable_uml_",CR,"km")
  
  CR <- CR + 20000
  }

  return(parcels)
}

parcel_size <- 3000
catchment_radius <- 10000
while(catchment_radius < 60000){
  
  make_n_reachable_uml(parcel_size, catchment_radius) %>% 
    saveRDS(paste0("explanatory_variables/panel_parcels_reachable_uml_",
                   parcel_size/1000,"km_",
                   catchment_radius/1000,"CR.rds"))
  
  catchment_radius <- catchment_radius + 20000
}





# parcels_centro_tmp <- readRDS(paste0("./outcome_variables/dataframes/panel_Indonesia_",parcel_size/1000,"km_",catchment_radius/1000,"CR.rds"))
# # keep only one cross-section
# parcels_centro_tmp <- filter(parcels_centro_tmp, year == 2001)
# parcels_centro_tmp <- dplyr::select(parcels_centro_tmp, parcel_id, lat, lon)
# parcels_centro_tmp <- dplyr::arrange(parcels_centro_tmp, parcel_id)
# 
# 
# make_n_reachable_uml <- function(parcel_size, catchment_radius){
#   
#   # read the parcel panel
#   parcels <- readRDS(here(paste0("/build/output/wa_panel_parcels_",
#                                  parcel_size/1000,"km_",
#                                  catchment_radius/1000,"CR.rds")))
#   
#   parcels_centro <- left_join(x = parcels, y = parcels_centro_tmp,  by = "parcel_id")
# 









