######################################################################
#                                                                    #
#   Give IBS mills a desa geometry                                   #
#                                                                    #
#   Input:  - IBS mills, each of the year it has the most recent     #
#             valid desa id                                          #
#             --> IBSmills_valid_desa.dta                            #
#                                                                    #
#           - crosswalked annual village shapefiles and 2010 map:    #
#             --> desa_1998_crosswalked.shp to 2009                  #
#             --> indo_by_desa_2010.shp                              #
#                                                                    #
#                                                                    #
#   Output: IBS mill with the smallest desa geometry possible        #
#           --> IBSmills_desageom.Rdata                              #
#   
######################################################################
######################################################################
rm(list = ls())


# LOAD OR INSTALL NECESSARY PACKAGES 
# List all packages needed for session
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

### LOCAL WORKING DIRECTORY, just to shorten calls.
setwd(here("/build/input/mill_geolocalization"))

# read IBS mills
ibs_desa <- read.dta13("IBSmills_valid_desa.dta")


################################# ASSOCIATE DESA GEOMETRIES TO IBS DESA_ID ###########################################
#templates:
ibs_desa[,"geom"] <- c(1:nrow(ibs_desa))
#annual_n_badcodes <- data.frame(year = c(1998:2010))
#annual_n_badcodes$n_obs <- c(1998:2010)


t <- 1998 
while(t < 2010){
  #read year t desa shapefile
  annualpolypath <- paste0("village_shapefiles/desa_",t,"_crosswalked.shp")
  annual_desa_poly <- st_read(annualpolypath)
  
  # give the column in "using" the same name as in master 
  names(annual_desa_poly)[names(annual_desa_poly) == paste0("d__",t)] <- "desa_id"
  
  mills_t <- filter(ibs_desa, year == t)
  
  #bc <- 0 
  for(i in mills_t$firm_id){
    # desa code of the mill from year t. 
    desa_of_i <- mills_t[mills_t$firm_id == i, "desa_id"]
    
    # some desa codes are not found in village maps (holes in crosswalk probably, or misreported desa codes in ibs*)
    if(nrow(annual_desa_poly[annual_desa_poly$desa_id == desa_of_i, "geometry"])!= 0){
      # give the corresponding desa geometry. 
        ibs_desa[ibs_desa$firm_id == i, "geom"] <- annual_desa_poly[annual_desa_poly$desa_id == desa_of_i, "geometry"] 

    } else {
      ibs_desa[ibs_desa$firm_id == i, "geom"] <- NA
      
      #bc <- bc + 1
    }
  }
  #accounts for mismatches peryear
  #annual_n_badcodes[annual_n_badcodes$year == t, "n_obs"] <- bc
  rm(annual_desa_poly)
  t <- t+1
}  


# 2010 
desa_map_2010 <- st_read("village_shapefiles/desa_map_2010/indo_by_desa_2010.shp")
names(desa_map_2010)[names(desa_map_2010) == "IDSP2010"] <- "desa_id"
mills_t <- filter(ibs_desa, year == 2010)
bc <- 0 
ndu_desa <- 0
#ibs_desa$from_du_desa <- c(1:nrow(ibs_desa))*0
for(i in mills_t$firm_id){
  # desa code of the mill from year t. 
  desa_of_i <- mills_t[mills_t$firm_id == i, "desa_id"]
  
  # All desa codes are found in village maps (no holes issue since this is the actual map of 2010 - not crosswalked). But pb is that 
  # there are duplicates in IDSP2010 so that several polygons may match with one desa_id. 
  if(nrow(desa_map_2010[desa_map_2010$desa_id == desa_of_i, "geometry"]) == 1){
    # give the corresponding desa geometry. 
    ibs_desa[ibs_desa$firm_id == i, "geom"] <- desa_map_2010[desa_map_2010$desa_id == desa_of_i, "geometry"] 
    
  } else if(nrow(desa_map_2010[desa_map_2010$desa_id == desa_of_i, "geometry"]) > 1){
    ibs_desa[ibs_desa$firm_id == i, "geom"] <- NA
    ndu_desa <- ndu_desa + 1
    #ibs_desa[ibs_desa$firm_id == i, "from_du_desa"] <- 1
  } else if(nrow(desa_map_2010[desa_map_2010$desa_id == desa_of_i, "geometry"]) == 0){
    ibs_desa[ibs_desa$firm_id == i, "geom"] <- NA
    bc <- bc + 1
  }
}
#ibs_desa[ibs_desa$from_du_desa == 1,]
#accounts for mismatches peryear
#annual_n_badcodes[annual_n_badcodes$year == 2010, "n_obs"] <- bc

# * we don't try to match these desa codes with n-1 or n-2 years of village maps because there is a risk that these codes correspond to 
# different areas in other years.

saveRDS(ibs_desa, file = "IBSmills_desageom.Rdata")

