######################################################################
#                                                                    #
#   Give a desa code to each of georeferenced mills                  #
#                                                                    #
#   Input:  - annual desa shapefiles
#             --> desa_shp_all.Rdata      
#
#           - heilmayr georeferenced mills
#             --> traseMills_capEstyear.xlsx
#                                                                    #
#   Output: georeferenced mills with the desa code they belong to.   #   
#                                                                    #
#   
######################################################################
######################################################################

# LOAD OR INSTALL NECESSARY PACKAGES 

rm(list = ls())
# List all packages needed for session
neededPackages = c("dplyr", "readxl", "sf", "foreign", "readstata13") 
                 
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
#file.path(PROJHOME, "/build/code", "Prepare_remote_sensing_palmoil.R")


######################################################################
######################################################################

# for now we read the initial heilmayr dataset
heilmayr <- read_excel("C:/Users/guyv/ownCloud/opalval/build/input/traseMills_capEstyear.xlsx")
# but eventually it is meant to read heilmayr_desa_defo

# read in the desa polygons
load("C:/Users/guyv/ownCloud/opalval/build/input/desa_shp_all.Rdata")

# keep only desa code and corresponding geometry. 
desa_geo <- desa_shp_all[,c("ID2013", "geometry")]
class(desa_geo)
rm(desa_shp_all)

#keep only main variables from heilmayr's mill cross-section
heilmayr <- select(heilmayr, parent_co, mill_name, est_year, latitude, longitude)

# make it a spatial feature object. 
heilmayr <- st_as_sf(heilmayr,	coords	=	c("longitude",	"latitude"))
class(heilmayr)

#Give both polygons and mill points the same projection
st_crs(desa_geo)
st_crs(heilmayr) <- st_crs(desa_geo)
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
desa_geo <- st_transform(desa_geo, crs = indonesian_crs)
heilmayr <- st_transform(heilmayr, crs = indonesian_crs)


#Create a logical matrice with element [i,j] TRUE when geometry of x(i) is within geometry of y(j)
?st_within
within_prj <- st_within(heilmayr, desa_geo, sparse = FALSE)
str(within_prj)
#change from logical to integers
within_prj <- within_prj*1
str(within_prj)

#remove geometry from desa_geo
desa_code_2013 <- st_set_geometry(desa_geo, NULL)
rm(desa_geo)
#convert from data.frame to matrix
desa_code_2013 <- as.matrix(desa_code_2013)
class(desa_code_2013)

# convert desa codes from character to numeric
desa_code_2013 <- as.numeric(desa_code_2013)

#matrix multiplication to select desa_codes that encompass a mill, i.e. desa gives for each mill the code of the desa it was in in 2013. 
desa <- within_prj%*%desa_code_2013
str(desa)

#merge with heilmayr information
heilmayr_desa <- cbind(heilmayr, desa)

## Analyse of the matrix. 

  # no mill is within no desa nor several desa. 
  r <- rowSums(within_prj)
  r[r!=1]
  
  summary(desa)
  
  # but several mills can be in the same desa. 
  length(unique(desa)) # 962 mills are alone in their desa.
  same_desa_mills <- heilmayr_desa[duplicated(desa),] # these are the 121 mills that are in the same desa as at least one other mill. 
  desa_millS <- desa_code_2013[colSums(within_prj)>1] # these are the 93 desa in which there are several mills. 
  
  # on pourrait aussi ploter les clusters (ceux qui ne sont pas uniques dans leur desa)
  rm(r, same_desa_mills, desa_millS, within_prj)
## 
  

# load IBS data 
ibs <- read_excel("C:/Users/guyv/ownCloud/opalval/build/input/IBS_1998_cleaned.xls")

# merge   
 # "By default the data frames are merged on the columns with names they both have"
 # So 
# prepare heilmayr_desa for merging
heilmayr_desa <- st_set_geometry(heilmayr_desa, NULL)
heilmayr_desa <- rename(heilmayr_desa, desa_id = desa)
#coertion of desa_id to numeric is not possible in ibs because there are some non numeric characters in there, 
# so one solution is to use desa_id as a character for the key of the merge. 
heilmayr_desa[,"desa_id"] <- as.character(heilmayr_desa[,"desa_id"])

#MERGE
ibs_desa <- merge(x = ibs, y = heilmayr_desa, by = "desa_id", all = TRUE )  
rm(ibs)
  

  
  
  
  
  
  
#Export and save
write.dta(heilmayr_desa, "C:/Users/guyv/ownCloud/opalval/build/input/heilmayr_desa.dta")
save(heilmayr_desa, file = "C:/Users/guyv/ownCloud/opalval/build/input/heilmayr_desa.Rdata")
save(heilmayr_desa, file = "C:/Users/guyv/ownCloud/opalval/build/input/heilmayr_desa_v2.Rdata", version = 2)




#Export data with firm_id year and desa_code to stata.
load("C:/Users/guyv/ownCloud/opalval/build/input/firms_desa2000.Rdata")
dat.firms <- select(dat.firms, firm_id, year, desa_id)
write.dta(dat.firms, "C:/Users/guyv/ownCloud/opalval/build/input/firms_desa_selected.dta")

IBS_desa2000 <- read.dta13("C:/Users/guyv/ownCloud/opal (2)/build/temp/IBS/IBS_desa2000.dta")
IBS_base_prelu <- read.dta13("C:/Users/guyv/ownCloud/opalval/build/input/mill_geolocalization/IBS_base_prelu.dta")

dif <- IBS_base_prelu[IBS_base_prelu$desa_id != IBS_desa2000$desa_id]

IBS_base_prelu <- filter(IBS_base_prelu, year >1999 & year < 2011)                   

nrow(IBS_base_prelu)                      
colnames(IBS_desa2000)

nrow(IBS_base_prelu[is.na(IBS_base_prelu$desa_id),]) #0
nrow(IBS_desa2000[is.na(IBS_desa2000$desa_id),]) #0

nrow(IBS_desa2000[IBS_desa2000$desa_id == "",]) #0
nrow(IBS_base_prelu[IBS_base_prelu$desa_id == "" & IBS_base_prelu$year<2011 & IBS_base_prelu$year>1997,]) #64163

nrow(IBS_desa2000) # 257439
nrow(IBS_base_prelu[IBS_base_prelu$year > 1997 & IBS_base_prelu$year < 2011,]) #300902

#Smaller desa_map
#aceh <- desa_shp_all$PROVINSI == "ACEH"
#desa_shp_aceh <- desa_shp_all[aceh,]
#st_crs(desa_shp_aceh)
#desa_shp_aceh_prj <- st_transform(desa_shp_aceh, crs = 23845)

#plot(st_geometry(desa_shp_aceh_prj))
#plot(st_geometry(coordinates_prj), col = "red", add = TRUE)