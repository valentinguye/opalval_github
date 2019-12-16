######################################################################
#                                                                    #
#   Match IBS mill dataset with Heilmayr's                           #
#                                                                    #
#   Input:  - IBS mills, each of the year it has the most recent     #
#             valid desa id, with the corresponding geometries       #
#             --> IBSmills_desageom.Rdata                            #
#                                                                    #
#           - Heilmayr georeferenced mills                           #
#             --> traseMills_capEstyear.xlsx                         #
#                                                                    #
#   Output: several subsets of IBS mills, depending on how they      #
#           matched with Heilmayr's.                                 #
#   
######################################################################
######################################################################
rm(list = ls())
       
                                                         
### LOAD OR INSTALL NECESSARY PACKAGES 
# List all packages needed for session"foreign",
neededPackages = c("dplyr", "readxl", "foreign", "readstata13", "here",
                   "sf", "rgdal") 

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


### LOCAL WORKING DIRECTORY, just to shorten calls, relative to project root wd. 
setwd(here("/build/input/mill_geolocalization"))
#######################################################################



# read Heilmayr mills
heilmayr <- read_excel("traseMills_capEstyear.xlsx")
# read IBS mills
ibs <- readRDS(lwd,"IBSmills_desageom.Rdata")

########################################### MATCH IBS AND HEILMAYR MILLS ################################################
## Indonesian CRS
#   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
#   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator. 
#   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is 
#   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0 
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
#indonesian_crs <- 4326
#prepare the sfc of ibs mills and their most recently known villages. 
ibs <- filter(ibs, is.na(geom)== F)   
ibs_na <- filter(ibs, is.na(geom))
ibs <- st_as_sf(ibs, crs = indonesian_crs)
ibs <- st_transform(ibs, crs = indonesian_crs)

# prepare heilmayr dataset
heilmayr <- dplyr::select(heilmayr, trase_code, parent_co, mill_name, est_year, latitude, longitude)
heilmayr$latitude <- as.numeric(heilmayr$latitude)
heilmayr$longitude <- as.numeric(heilmayr$longitude)
heilmayr$lat <- heilmayr$latitude
heilmayr$lon <- heilmayr$longitude
heilmayr <- st_as_sf(heilmayr,	coords	=	c("longitude",	"latitude"), crs = indonesian_crs)
heilmayr <- st_transform(heilmayr, crs = indonesian_crs)



#match 
ibs_j <- st_join(x = ibs, y = heilmayr, join = st_contains, left = T) # is equivalent to ibs_j <- st_join(x = ibs, y = heilmayr) bc default is st_intersect.

#plot(st_geometry(ibs[ibs$firm_id == 1761,]))
#plot(st_geometry(heilmayr[heilmayr$mill_name == "PT. Socfin - Seumanyam",]), add = TRUE, col = "red")


########################################### IDENTIFY DIFFERENTLY MATCHED SUBSETS ######################################################

no_nas <- ibs_j[is.na(ibs_j$lon)==F,] # 355 non-missings
nrow(no_nas[unique(no_nas$firm_id),])
# 245 ibs mills are in a village where at least one Heilmayr mill stands. 

pot_otm <- no_nas[!(duplicated(no_nas$firm_id) | duplicated(no_nas$firm_id, fromLast = TRUE)), ] # 173
# these are the 173 potential one-to-many matches (the ibs mill is the sole mill from ibs mills (we know the desa of) to be in this village and 
# there may be several Heilmayr mills in this same village.). 
# Among them, there are:
  oto <- pot_otm[!(duplicated(pot_otm$mill_name) | duplicated(pot_otm$mill_name, fromLast = TRUE)), ] 
  # 132 IBS mills we know the desa of that are the only ibs mills in it while there is only one Heilmayr mill in this village.  
  
  otm <- pot_otm[duplicated(pot_otm$mill_name) | duplicated(pot_otm$mill_name, fromLast = TRUE), ]
  # 41 IBS mills with identified desa in which they are the only ibs mills, but where there is more than one Heilmayr mill. 
  nrow(distinct(otm, lat, lon)) # 27 Heilmayr mills are involved.

du <- no_nas[(duplicated(no_nas$firm_id) | duplicated(no_nas$firm_id, fromLast = TRUE)), ] 
# these are the 72 ibs mills with identified desa that are in the same desa with at least another ibs one.
# These villages with many ibs may encompass either one or many Heilmayr mills. 

noto <- rbind(otm, du)
# these are the 113 ibs mills (41 + 72) that have an identified desa and are either m:m, o:m or m:o with Heilmayr mills. 

total_potential <- no_nas[!duplicated(no_nas$lon),]
# these are the 256 mills of which we know the coordinates that are within the villages of IBS mills. 
# Note: that two mills have exactly the same name but have completely different coordinates. 
#total_potential[total_potential$mill_name == "PT. Usaha Sawit Mandiri",]


## So, the maximum number of pre-2011 ibs mills we can hope to geolocalize with this desa matching technique is 256. 
## The minimum number is 132 (unless we double-check them with workers and some don't match). 


#### Those that have a desa polygon but match with no Heilmayr mills (592)
# It is useless to try to find them manually with the directory number of workers and Heilmayr's list. 
# we can still find their names with directories and google-search them, and/or directly spot them manually within their villages. 
ibs_unref <- left_join(x = ibs, y = st_set_geometry(oto[,c("firm_id","lat")], NULL), by = "firm_id")
ibs_unref <- filter(ibs_unref, is.na(ibs_unref$lat))

ibs_unref <- left_join(x = ibs_unref, y = st_set_geometry(noto[,c("firm_id","lon")], NULL), by = "firm_id")
ibs_unref <- filter(ibs_unref, is.na(ibs_unref$lon))

ibs_unref <-dplyr::select(ibs_unref, -lon, -lat)
# (the use of "lat" and "lon" was just an arbitrary choice of non-empty variables to flag oto and noto resp.)

# They are in 455 different polygons, of which 359 do not intersect with another one
# (intersections but not equal likely when there is a split and a mill is associated with the polygon of the village before, and one or more 
#other mills are associated with the villages after the split.)
#ibs_unref$grp = sapply(st_equals(ibs_unref), max)
#ibs_unref <- arrange(ibs_unref, grp, firm_id)
#nrow(ibs_unref[unique(ibs_unref$grp),])

#unique_unref <-ibs_unref[unique(ibs_unref$grp),]

#ibs_unref$grp_int = sapply(st_intersects(ibs_unref), max)
#nrow(ibs_unref[unique(ibs_unref$grp_int),])

#plot(st_geometry(ibs_unref[ibs_unref$grp == 1 | ibs_unref$grp == 355,]), add = F)
#plot(st_geometry(ibs_unref[ibs_unref$grp == 355,]), add = F)

# also, this is 473 different year_desa_id, meaning that there are cases where two mills have equal polygons but no equal year_desa_id: these are the 
# cases of two mills being associated to the same polygon but with desa_ids from different years. 
#ibs_unref$year_desa_id <- paste(ibs_unref$year, ibs_unref$desa_id, sep = "")
#nrow(ibs_unref[unique(ibs_unref$year_desa_id),])



######################################### EXPORT THEM ACCORDINGLY ##############################################################################

#### Those that have no desa polygon though they had an un-flagged desa_id (76)
# they return to stata to get merged with the panel and geolocalized manually. 
ibs_na <-dplyr::select(ibs_na, firm_id)
write.dta(ibs_na, "pre2011_bad_desa_id.dta")


#### Those that have a desa polygon but match with no Heilmayr mills (592) out_ton_cpo_imp2, district_name, village_name,
ibs_unref$desa_id <- as.character(ibs_unref$desa_id)
st_crs(ibs_unref) <- 4326 
ibs_unref <- st_transform(ibs_unref, crs = 4326)

## For R 
saveRDS(ibs_unref, file = "ibs_unref.Rdata") 

## For GEE
ibs_unref <-dplyr::select(ibs_unref, firm_id, year, min_year, max_year, industry_code, out_ton_cpo_imp2,
                    workers_total_imp3, avg_out_ton_cpo_imp2, last_out_ton_cpo_imp2, avg_cpo_price_imp2, desa_id, geom)

st_write(ibs_unref, "ibs_unref", driver = "ESRI Shapefile", delete_dsn = TRUE)
#st_write(ibs_unref,"unreferenced_mill_desa.gpkg", driver = "GPKG", delete_dsn = TRUE)

## For Stata 
ibs_unref2 <-dplyr::select(ibs_unref, firm_id)
write.dta(st_set_geometry(ibs_unref2, NULL), "ibs_unref.dta")


#### Those who matched (oto & noto)
## oto
oto <- st_set_geometry(oto, NULL)
oto <-dplyr::select(oto, firm_id, parent_co, mill_name, est_year, lat, lon)
write.dta(oto, "oto.dta")

## noto 
# sort by desa geometry to ease manual matching
noto$grp = sapply(st_intersects(noto), max)
noto <- arrange(noto, grp, firm_id)
noto <- st_set_geometry(noto, NULL)
noto <-dplyr::select(noto, firm_id, parent_co, mill_name, est_year, lat, lon, grp)
write.dta(noto, "noto.dta")

## heilmayr (for GEE)
st_crs(heilmayr) <- 4326
heilmayr <- st_transform(heilmayr, crs = 4326)
heilmayr <-dplyr::select(heilmayr, -lat, -lon)
st_write(heilmayr, "heilmayr_list", driver = "ESRI Shapefile", delete_dsn = TRUE)

