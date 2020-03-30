
rm(list = ls())

# PACKAGES
#install.packages("sf", source = TRUE)
library(sf)

neededPackages = c("plyr", "dplyr", "readxl","foreign", "data.table", "readstata13", "here",
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

fc <- readRDS("idn_plant_sf.Rdata")

fc$species_simp <- as.character(fc$species_simp)
fc$source <- as.character(fc$source)


for(name in fc$source %>% unique()){
  fc$species_simp[fc$source == name] %>%  unique() %>% print()
}

for(name in fc$source %>% unique()){
  fc[fc$source == name] %>% st_union %>% st_area()/(10000*1e6) %>%  print()
}

fc_op <- fc %>%  dplyr::filter(species_simp == "Oil Palm" | species_simp == "Oil Palm Mix" )

fc_op %>%  st_geometry() %>%  plot()

fc_op %>% st_union() %>% st_area()



peter <- raster("tree_plantations")

#fc_s <- dplyr::select(fc, final_id, iso, org_name, org_code, final_code)

fc$species_simp[source == "Austin et al. (2017)"] %>% unique()



fc$size[fc$org_name == "oil palm, kemen"] %>% unique()

fc$year %>% unique()
fc$org_name %>% unique()
fc$final_code %>% unique()
fc$source %>% unique()
fc$common_name %>% unique()
fc$species_simp[fc$org_name == "oil palm, kemen"] %>% unique()









# The input file geodatabase
fgdb <- "sdpt.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="idn_plant")

# Determine the FC extent, projection, and attribute information
summary(fc)

fc_sf <- st_as_sf(fc)
class(fc_sf)

saveRDS(fc_sf, "idn_plant_sf.Rdata")

# View the feature class
plot(fc)








