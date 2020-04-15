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


#### define parcel size ####
PS <- 10000

#### define buffer size for the catchment areas  ####
BS <- 40000

#### Define projection ####
#   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
#   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator. 
#   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is 
#   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0 
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"



years <- seq(from = 1998, to = 2015, by = 1)

## read in the parcels of one arbitrary year (say year one of deforestation: 2001), for one arbitrary threshold (say 25%)
# indeed, we only want to allocate economic variables to spatial units (parcels) but don't care yet about land use change there. 
# In other words, it's easier to join later economic and land use attributes of these same parcels, rather than compute economic attributes
# again for every deforestation year and definition. 

# parcels_template <- raster("./outcome_variables/annual_parcels/parcels_25th_1.tif")
# 
# 
# parcels_template <- rasterToPoints(parcels_template, spatial = TRUE) %>% st_as_sf()
# 
# # append an id variable
# parcels_template$parcel_id <- seq(from = 1, to = nrow(parcels_template), by = 1)
# 
# # get rid of the land use variable 
# parcels_template <- dplyr::select(parcels_template, parcel_id)


####################################
# rather import the parcels from the panel saved in build_parcels_df.R.
parcels_centro <- readRDS(paste0("./outcome_variables/panel_parcels_",PS/1000,"km_",BS/1000,"CA_","25th.Rdata"))
# keep only one cross-section
parcels_centro <- filter(parcels_centro, year == 1998)
# turn it into a sf object
parcels_centro <- st_as_sf(parcels_centro, coords = c("lon", "lat"), remove = FALSE, crs = indonesian_crs)
parcels_centro <- dplyr::select(parcels_centro, parcel_id, geometry)

####################################


#### PREPARE IBS DATA #### 

ibs <- read.dta13(here("/build/output/IBS_panel_final.dta"))  

# keep only geolocalized mills
ibs <- ibs[is.na(ibs$lat) == FALSE,]
length(unique(ibs$firm_id))

# keep only some variables to fasten computations for now
      ibs <- ibs[, c("firm_id", "year", "trase_code", "uml_id", "mill_name", "parent_co", "lat", "lon",
                     "min_year","est_year",  "startYear", "max_year", 
                     "ffb_price_imp1", "ffb_price_imp2", "in_ton_ffb_imp1", "in_ton_ffb_imp2", "in_val_ffb_imp1", "in_val_ffb_imp2",
                     "cpo_price_imp1","cpo_price_imp2", "out_ton_cpo_imp1", "out_ton_cpo_imp2", "out_val_cpo_imp1", "out_val_cpo_imp2",
                     "prex_cpo_imp1", "prex_cpo_imp2",
                     "pko_price_imp1","pko_price_imp2", "out_ton_pko_imp1", "out_ton_pko_imp2", "out_val_pko_imp1", "out_val_pko_imp2",
                     "prex_pko_imp1", "prex_pko_imp2",
                     "export_pct_imp", "revenue_total", "workers_total_imp3",
                     "pct_own_cent_gov_imp", "pct_own_loc_gov_imp", "pct_own_nat_priv_imp", "pct_own_for_imp", 
                     "iv2_imp1", "iv2_imp2", "iv3_imp1", "iv3_imp2", "iv4_imp1", "iv4_imp2"               
                     )]
      
            # we don't keep the logs because we don't want to compute means of logs, but logs of means. 
            # "ffb_price_imp1_ln", "ffb_price_imp2_ln", "cpo_price_imp1_ln", "cpo_price_imp2_ln",        
            #           "pko_price_imp1_ln", "pko_price_imp2_ln", "out_val_cpo_imp1_ln", "out_val_cpo_imp2_ln", "out_val_pko_imp1_ln",      
            #          "out_val_pko_imp2_ln", "revenue_total_ln" 
                     

# split the panel into sf cross sections 
class(ibs$year)
ibs_cs <- lapply(years, FUN = function(x) ibs[ibs$year == x,]) 
ibs_cs <- lapply(ibs_cs, FUN = st_as_sf, coords =  c("lon", "lat"), remove = FALSE, crs = 4326)
ibs_cs <- lapply(ibs_cs, FUN = st_transform, crs = indonesian_crs)


#### BUILD FUNCTION TO COMPUTE CROSS SECTIONAL WEIGHTED AVERAGES AT PARCELS ####
# note on addressing geometries: the two first of the following calls are equivalent; however, the third is different 
# parcels$geometry[parcels$parcel_id == i] 
# parcels[["geometry"]][parcels$parcel_id == i]
# parcels[parcels$parcel_id == i, "geometry"]

#t <- 1
make_cs_w_averages <- function(t){
#let's not be year specific in this function, and we will rename and append everything after. 

## Attribute to each parcel centroid the sf data frame of reachable mills 

# this is a data frame of pairs of parcel and year t mill points that are within distance of 40km
# ***the geometry kept is from x ***
d <- st_join(x = parcels_centro, y = ibs_cs[[t]], join = st_is_within_distance, dist = BS, left = F)

# nest the sets of reachable mills within each parcel row.
# they need to be no sf object for that. 
d <- st_set_geometry(d, NULL)
parcels <- nest_join(parcels_centro, d, 
                             by = "parcel_id", 
                             keep = T, # keep = T garde parcel_id dans les df nested. 
                             name = "reachable") %>% st_as_sf() # (bc the nest_join removes the sf class)
rm(d)

# select non empty reachable nested data frames (data frames of reachable mills) - programing purpose
s <- sapply(parcels$reachable, FUN = nrow)>0

# compute the number of reachable mills at each parcel - informative purpose
parcels[,"n_reachable_ibs"] <- sapply(parcels$reachable, 
                                  FUN = nrow)

# turn (non-empty) nested tibbles into sf data frames. 
parcels$reachable[s] <- lapply(parcels$reachable[s], 
                                        FUN = st_as_sf, 
                                        coords =  c("lon", "lat"), 
                                        remove = FALSE, 
                                        crs = 4326)

parcels$reachable[s] <- lapply(parcels$reachable[s], 
  FUN = st_transform, 
  crs = indonesian_crs)


# make a distance column in the reachable data frames 
for(i in parcels$parcel_id[s]){
parcels$reachable[parcels$parcel_id == i][[1]] <- mutate(parcels$reachable[parcels$parcel_id == i][[1]], 
                                                        distance = st_distance(x = parcels$geometry[parcels$parcel_id == i], 
                                                                     y = parcels$reachable[parcels$parcel_id == i][[1]]$geometry,
                                                                     by_element = TRUE) %>% as.numeric())
}

# make the inverse of distance column
for(i in parcels$parcel_id[s]){
  parcels$reachable[parcels$parcel_id == i][[1]] <- mutate(parcels$reachable[parcels$parcel_id == i][[1]], 
                                                           w = 1/distance)
}
# 7845 s. for distance and w columns within one year

 # Define the variables of interest we want to compute the weighted averages of. 
#variables <- c("cpo_price_imp1","cpo_price_imp2", "prex_cpo_imp1", "prex_cpo_imp2")
variables <- c("min_year", "startYear", "max_year",
                "ffb_price_imp1", "ffb_price_imp2", "in_ton_ffb_imp1", "in_ton_ffb_imp2", "in_val_ffb_imp1", "in_val_ffb_imp2",
                "cpo_price_imp1","cpo_price_imp2", "out_ton_cpo_imp1", "out_ton_cpo_imp2", "out_val_cpo_imp1", "out_val_cpo_imp2",
                "prex_cpo_imp1", "prex_cpo_imp2",
                "pko_price_imp1","pko_price_imp2", "out_ton_pko_imp1", "out_ton_pko_imp2", "out_val_pko_imp1", "out_val_pko_imp2",
                "prex_pko_imp1", "prex_pko_imp2",
                "export_pct_imp", "revenue_total", "workers_total_imp3",
                "pct_own_cent_gov_imp", "pct_own_loc_gov_imp", "pct_own_nat_priv_imp", "pct_own_for_imp", 
                "iv2_imp1", "iv2_imp2", "iv3_imp1", "iv3_imp2", "iv4_imp1", "iv4_imp2")

# make the variable specific sum of the inverse of distance over all the reachable mills that have no missing on this variable.
for(voi in variables){
  for(i in parcels$parcel_id[s]){
    # vector selecting mills to count in the weight standardization for each variable of interest
    voi_missing <- parcels$reachable[parcels$parcel_id == i][[1]][,voi] %>% st_drop_geometry() %>% is.na() %>% as.vector() 
    
    # create the column for the sum of invert distances to mills that don't have a missing on this particular voi. 
    parcels$reachable[parcels$parcel_id == i][[1]] <- mutate(parcels$reachable[parcels$parcel_id == i][[1]], 
                                                     !!as.symbol(paste0("sum_w_",voi)) := sum(w[!voi_missing])) 
    # the weird syntax !! and := is to dynamically assign parameter names in dyplr version >= 0.7.
    
    # and just make the information of that column only available for those mills. 
    parcels$reachable[parcels$parcel_id == i][[1]][voi_missing, paste0("sum_w_",voi)] <- NA
    }
}

# make the standardized weights. They are variable specific too. 
for(voi in variables){
  for(i in parcels$parcel_id[s]){
    parcels$reachable[parcels$parcel_id == i][[1]] <- mutate(parcels$reachable[parcels$parcel_id == i][[1]], 
                                                             !!as.symbol(paste0("std_w_", voi)) := w/!!as.symbol(paste0("sum_w_", voi))) 
                                                             # this ratio indeed is NA if sum_w_voi is NA
  }
} 

# the two loops below build the column in parcels in which every cell is the weighted average of voi 
# (the first one computes an intermediate column in reachable in which each mill gets the product of its weight and its attribute)
for(voi in variables){
  for(i in parcels$parcel_id[s]){
    parcels$reachable[parcels$parcel_id == i][[1]] <- mutate(parcels$reachable[parcels$parcel_id == i][[1]], 
                                                             !!as.symbol(paste0("w_var_", voi)) := !!as.symbol(paste0("std_w_", voi))*(!!as.symbol(voi)))
  }
}
# (the second one makes the sum of these weighted terms and hence computes the weighted means
# its makes sure that parcels whose reachable mills are all NA on a voi don't get a 0 but a NA for weighted mean)
for(voi in variables){
  parcels[s,paste0("wa_", voi)] <- sapply(parcels$reachable[s], 
                                          FUN = function(x) x[,paste0("w_var_",voi)] 
                                                            %>% st_drop_geometry() 
                                                            %>% is.na()
                                                            %>% all() 
                                                            %>% ifelse(yes = NA, no = sum(st_drop_geometry(x[,paste0("w_var_",voi)]),na.rm = T)))
}

# remove now useless columns from parcels
parcels <- st_drop_geometry(parcels)

parcels <- mutate(parcels, 
                  reachable = NULL)

# give year specific variable names to the variables built in this function 
names(parcels) <- names(parcels) %>% paste0(".", years[t])

return(parcels)
}

# tic()
# t <- 1
# assign(x = paste0("parcels_", years[t]), value = make_cs_w_averages(t)) 
# toc()


### Build the parallel-looping function ####

w_averages_parallel <- function(detected_cores){
  
  registerDoParallel(cores = detected_cores) 
  
  # the loop has arguments to define how the results of the workers should be combined, and to give "workers" (CPUs) 
  # the objects and packages they need to run the function. 
  foreach(t = 1:length(years), 
          .combine = cbind,
          #.multicombine = TRUE not necessary because with cbind the default multicombine is TRUE anyways
          .export = c("make_cs_w_averages", "parcels_centro", "ibs_cs", "years", "BS", "indonesian_crs"), 
          .packages = c("sf", "raster", "tidyverse")) %dopar% 
    make_cs_w_averages(t) # the function that is parallelly applied to different years. 
}

### And run it
tic()
wide_parcels <- w_averages_parallel(detectCores())
toc()

# manage repetitions of parcel_id variables over years
wide_parcels$parcel_id <- seq(from = 1, to = nrow(wide_parcels), by = 1)
wide_parcels <- dplyr::select(wide_parcels, parcel_id, everything())
wide_parcels <- dplyr::select(wide_parcels, -starts_with("parcel_id."))  

saveRDS(wide_parcels, file = paste0("./wa_wide_panel_parcels_",PS/1000,"km_",BS/1000,"CA.Rdata"))

# reshape to long 
varying_vars <- wide_parcels %>% dplyr::select(-parcel_id) %>% colnames()

long_parcels <- reshape(wide_parcels, 
                varying = varying_vars, 
                #v.names =  
                timevar = "year",
                idvar = "parcel_id",
                ids = "parcel_id",
                direction = "long",
                sep = ".")


rm(varying_vars)

long_parcels <- dplyr::arrange(long_parcels, parcel_id, year)

saveRDS(long_parcels, file = paste0("./wa_panel_parcels_",PS/1000,"km_",BS/1000, "CA.Rdata"))




#### n_reachable_full ####
# BUT IT WOULD BE BETTER TO HAVE THIS COMPUTED FOR ALL KNOWN MILLS.
# This can be done outside of the make_cs_w_averages function
# Not difficult but we need to know when they appear in the UML 
# see with Sebi how he did for that. 




################################################################################################################
#### TEST ZONE ####

#long panel and parcels in it that never have a reachable mill though... 
lp <- readRDS(paste0("./wa_panel_parcels_",PS/1000,"km_",BS/1000, "CA.Rdata"))

grp_unreached <- plyr::ddply(lp, "parcel_id", summarise, 
                       unreached = sum(n_reachable_ibs) == 0)

lp <- merge(lp, grp_unreached, by = "parcel_id")
lp$parcel_id[lp$unreached == FALSE] %>% unique() %>% length()

geop <- merge(parcels_template, grp_unreached, by = "parcel_id")

geop$parcel_id[geop$unreached == TRUE] %>% unique() %>% length()
geop_out <- geop[geop$unreached == TRUE,]
geop_out %>% class()
geop_out %>% ggplot() + geom_sf()


wp <- readRDS(paste0("./wa_wide_panel_parcels_",PS/1000,"km_th.Rdata"))

s_wp <- dplyr::select(wp, parcel_id, starts_with("n_reachable_ibs"))

### plot tests
time_means <- plyr::ddply(lp, "parcel_id", summarise, 
                          tm_n_reachable_mills = mean(n_reachable_ibs, na.rm=T),
                            tm_cpo_price_imp1 = mean(wa_cpo_price_imp1, na.rm=T),
                            tm_cpo_price_imp2 = mean(wa_cpo_price_imp2, na.rm=T),
                          tm_cpo_prex_imp1 = mean(wa_prex_cpo_imp1, na.rm=T),
                          tm_cpo_prex_imp2 = mean(wa_prex_cpo_imp2, na.rm=T))

time_means <- merge(parcels_centro, time_means, by = "parcel_id")
lp <- left_join(long_parcels, parcels_centro, by = "parcel_id") 
lp <- lp %>% st_as_sf()
plot(time_means[,"tm_cpo_price_imp1"])
plot(time_means[,"tm_n_reachable_mills"])

plot(lp[lp$year == 1998,"wa_cpo_price_imp1"])
plot(lp[lp$year == 2014 & lp$parcel_id < 100,"wa_cpo_price_imp1"])

plot(lp[lp$year == 1998,"n_reachable_ibs"])
plot(lp[lp$year == 2015,"n_reachable_ibs"])

names(lp)
plot()
lp <- unique(lp)

ggplot() + geom_sf(data = parcels[,], col = 'red')  

geom_sf(data = total_ca40)+ 



long_parcels$parcel_id[long_parcels$unreached == FALSE] %>% unique() %>% length()

parcels$parcel_id[long_parcels$unreached == FALSE] %>% unique() %>% length()


buffers_total_50km <- total_ca40
plot(total_ca40, add = TRUE)
plot(mills_ca40)
plot(parcels[parcels$parcel_id == 1:19,], add = T, col = "red")

parcels_template[1:19,] %>% st_geometry() %>% plot(col = "red")
parcels_template[1:19,] %>% plot()

fp <- parcels[1:19,] %>% st_geometry()

ggplot() + geom_sf(data = total_ca40)+ geom_sf(data = parcels[,], col = 'red')  



mapping = aes(total_ca40$x)

total_ca40 %>% plot()

parcels %>% class()

total_ca40 %>% st_crs()














