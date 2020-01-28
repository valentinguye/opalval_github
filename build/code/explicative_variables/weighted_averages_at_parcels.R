# This script uses parcels already built in build_parcels_df.R
# This data frame is a 2001 - 2018 panel of parcels covered by the 40km catchment areas of Indonesian mills. 
# It has information on different measures of forest conversion, for each parcel in each year. 
# Now we would like to add explanatory variables to these records. 

# The main steps are: 
# Take one set of parcels - their geographic characteristics are constant. 
# Have it as an sf obect with point data (coordinates of centroids)

# import the panel of IBS geolocalized mills. 

# Add to the sf dataframe parcel object 18 list columns (for years 1998 to 2015). Each row has one list element in each of these list columns.
# Each of these elements is a dataframe with one record for each mill that is less than 40 km from the element's point. 

# Each of the records within each of the 18 annual elements of every parcels is an IBS mill, with its attributes from IBS_mills_final
# + its distance to this particular parcel's centroid. 
# + attribute specific weights based on this distance and the distance of other reachable mills that have a non missing value for this attribute.   
# + its weighted value for each variable of interest. 

# Add to the sf dataframe 18*K columns (with K explanatory variables of interest). Each row has the sum of the column with weighted values for this variable in the annual element

rm(list = ls())

# PACKAGES
#install.packages("sf", source = TRUE)
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
setwd(here("/build/input/outcome_variables"))


#### define parcel size ####
PS <- 10000

#### Define projection ####
#   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
#   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator. 
#   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is 
#   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0 
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"


th <- 25 
year <- seq(from = 1998, to = 2015, by = 1)

## read in the parcels of one arbitrary year (say year one of deforestation: 2001)
#while(th < 100){
assign(x = paste0("parcels_",th,"th"), value = raster(paste0("./annual_parcels/parcels_",th, "th_", 1,".tif")))
#th <- th + 25
#}

# turn it into a sf object
parcels_25th_sf <- rasterToPoints(parcels_25th, spatial = TRUE) %>% st_as_sf()
#parcels_50th <- rasterToPoints(parcels_50th, spatial = TRUE) %>% st_as_sf()
#parcels_75th <- rasterToPoints(parcels_75th, spatial = TRUE) %>% st_as_sf()

# append an id variable
parcels_25th_sf$parcel_id <- seq(from = 1, to = nrow(parcels_25th_sf), by = 1)
#parcels_50th$parcel_id <- seq(from = 1, to = nrow(parcels_50th), by = 1)
#parcels_75th$parcel_id <- seq(from = 1, to = nrow(parcels_75th), by = 1)

# get rid of the land use variable 
parcels_25th_sf <- dplyr::select(parcels_25th_sf, -parcels_25th_1)


#### PREPARE IBS DATA #### 

ibs <- read.dta13(here("/build/output/IBS_mills_final.dta"))  

# keep only geolocalized mills
ibs <- ibs[is.na(ibs$lat) == FALSE,]
length(unique(ibs$firm_id))

# keep only some variables to fasten computations for now
ibs <- ibs[,c("firm_id", "year", "lon", "lat")]
# split the panel into sf cross sections 
class(ibs$year)
ibs_cs <- lapply(year, FUN = function(x) ibs[ibs$year == x,]) 
ibs_cs <- lapply(ibs_cs, FUN = st_as_sf, coords =  c("lon", "lat"), remove = FALSE, crs = 4326)
ibs_cs <- lapply(ibs_cs, FUN = st_transform, crs = indonesian_crs)

## Attribute to each parcel centroid the sf data frame of reachable mills 

# this is a data frame of pairs of parcel and mill points that are within distance of 40km
# ***the geometry kept is from x ***
d <- st_join(x = parcels_25th_sf, y = ibs_cs[[1]], join = st_is_within_distance, dist = 40000, left = F)
#b <- st_join(x =ibs_cs[[1]], y =  parcels_25th_sf, join = st_is_within_distance, dist = 40000, left = F)

# nest the sets of reachable mills within each parcel row.
d <- st_set_geometry(d, NULL)
parcels_25th_sf <- nest_join(parcels_25th_sf, d, by = "parcel_id", keep = F, name = "reachable") # keep = T garde parcel_id dans les df nested. 
rm(d)

# select non empty reachable data frames (data frames of reachable mills)
s <- sapply(parcels_25th_sf$reachable, FUN = nrow)>0
# turn nested tibbles into sf data frames. 
parcels_25th_sf$reachable[s] <- lapply(parcels_25th_sf$reachable[s], 
                                        FUN = st_as_sf, 
                                        coords =  c("lon", "lat"), 
                                        remove = FALSE, 
                                        crs = 4326)

parcels_25th_sf$reachable[s] <- lapply(parcels_25th_sf$reachable[s], 
  FUN = st_transform, 
  crs = indonesian_crs)

test <- parcels_25th_sf
# make a distance column in the reachable data frames 
tic()
for(i in parcels_25th_sf$parcel_id[s]){
parcels_25th_sf$reachable[parcels_25th_sf$parcel_id == i][[1]] <- mutate(parcels_25th_sf[parcels_25th_sf$parcel_id == i, "reachable"][[1]], 
                                                        distance = st_distance(x = parcels_25th_sf[parcels_25th_sf$parcel_id == i, "geometry"], 
                                                                     y = parcels_25th_sf[parcels_25th_sf$parcel_id == i, "reachable"][[1]]$geometry,
                                                                     by_element = TRUE) %>% as.numeric()
          )
}
toc()

test$reachable[test$parcel_id == i][[1]] <- mutate(test[test$parcel_id == i, "reachable"][[1]], 
                                                      distance = 1)
all.equal(test$reachable[test$parcel_id == i][[1]], test[test$parcel_id == i, "reachable"][[1]])

a <- st_distance(x = test[test$parcel_id == i, "geometry"], y = test[test$parcel_id == i, "reachable"][[1]]$geometry, by_element = T)
b<- st_distance(x = test[test$parcel_id == i, "geometry"], y = test[test$parcel_id == i, "reachable"][[1]]$geometry, by_element = F)
test$reachable[[test$parcel_id == i]] %>% class()
test[test$parcel_id == i, "reachable"] %>% class()
test$reachable[s] <- lapply(test$reachable[s], 
                            FUN = mutate, 
                            distance = st_distance, 
                              y = test[] 
                            )



test$reachable[s][[1]]$geometry %>% st_crs()
parcels_25th_sf$reachable[sapply(parcels_25th_sf$reachable, FUN = nrow)>0][[1]] %>% st_transform(crs = indonesian_crs)

parcels_25th_sf$reachable[sapply(parcels_25th_sf$reachable, FUN = nrow)>0] %>% sapply(FUN = nrow)[sapply(FUN = nrow)==0]
parcels_25th_sf$reachable[sapply(parcels_25th_sf$reachable, FUN = nrow)>0][[1]] %>% st_crs()
# OK COMME TEL CA FONCTIONNE. MAINTENANT IL FAUT LAPPLY st_as_sf SUR TOUS LES DATA FRAMES DE test$reachable. 
# donc ça marche quand on enlève la colonne geometry de d
# mais on en a encore besoin 
# quand on selectionne en cliquand (View) on voit que les données sont bien là 
# donc ca doit être un pb de comment on les appelle ou de liste dans une liste


# en revanche appeler plus loin fonctionne 

# euh et il n'y aurait pas un pb si test[19, "geometry"] et test$reachable[[19]]$geometry appellent le même point... 
# ca c'est les coordonnées de la parcelle 19	c(-1890840.82402193, 516386.446157133) 

# avec le st_join, on ne garde que ces coordonnées, celles des parcelles. 
# on peut essayer soit de reconstruire une column geometry avec lon et lat
# soit d'inverser x et y dans le st_join
# ok ça c'est résolu si on utilise b 
# mais on a toujours le pb attempt to select less than one element. 

d[d$firm_id == 1765,] 
d %>% st_geometry() %>% plot()
ibs_cs[[1]][ibs_cs[[1]]$firm_id == 1765,] %>% st_geometry %>% plot(add  = TRUE)
test$y[[1]]

ibs_cs[[1]] %>% st_crs()


plot(parcels_25th_sf)
ibs_cs[[1]] %>% st_geometry() %>% plot(add = T)

ibs_cs[[1]][,"cpo_price_imp1"] %>% plot()

parcels_25th$parcels_25th_1 %>% st_crs()
parcels_25th%>% plot()


d[[]] %>% class()
class(d)
st_set_crs()


ibs_1998 %>% st_geometry() %>% plot()
plot(st_geometry(ibs_1998))




for(t in 1:18){
  assign(x = paste0("ibs_",year[t]), value = ibs[ibs$year == year[t],] %>% st_as_sf(coords = c("lon", "lat"), crs = 4326))
}









































