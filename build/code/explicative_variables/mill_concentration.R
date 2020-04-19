### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Make variables of mills' concentration



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


# IBS YEARS
years <- seq(from = 1998, to = 2015, by = 1)


# parallel over CR
parallel_mill_concentration <- function(ncores){
  
  ### Sequence over which to execute the task 
  CRs <- c(1e4, 3e4, 5e4) # 10, 30, 50km
  
  ### read the input to the task
  # has been done beforehand because it is not specific to function variables
  
  ### Define the task 
  catchment_radius_mill_concentration <- function(catchment_radius){
    ## prepare UML mills 
    # read the most complete version of UML we have. 
    uml <- read.dta13(here("/build/output/UML_valentin_imputed_est_year.dta"))
    uml <- uml[!is.na(uml$lat),]
    uml <- st_as_sf(uml, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    uml <- st_transform(uml, crs = indonesian_crs)
    uml <- st_buffer(uml, dist = catchment_radius)
    
    # make square buffers rather than circles, to avoid geometry issues later
    st_geometry(uml) <- sapply(st_geometry(uml), 
                               FUN = function(x){st_as_sfc(st_bbox(x))}) %>% 
                        st_as_sfc(crs = indonesian_crs)
    
    # set buffer area to standardize weights later 
    total_buffer_area <- st_area(uml$geometry[1]) %>% as.numeric()
    
    # Make template column for the variable we will build.  
    uml$concentration <- rep(NA, nrow(uml))
    colnames(uml)[colnames(uml)=="concentration"] <- paste0("concentration_",catchment_radius/1000)
    
    # split to cross sections of ALL (UML) mills present each year
    # ///!!!\\\ missing establishment years are assumed to be older than 1998
    uml_cs <- lapply(years, FUN = function(x) uml[uml$est_year_imp <= x | is.na(uml$est_year_imp),]) 
    
    # Each element of uml_cs is an sf dataframe of buffers of uml mill present (already estabished, or assumed so) in the corresponding year. 
    
    for(t in 1:length(years)){
      
      buffer_uml_cs <- uml_cs[[t]] %>% st_geometry
     
      # for each mill, compute its annual number of competing mills. 
        uml_cs[[t]][,paste0("concentration_",catchment_radius/1000)] <- sapply(1:length(buffer_uml_cs),
                                                                          function(i){
          
          # First identify mills which buffers intersect with i
          intersecters_id <- st_intersects(x = buffer_uml_cs[i], 
                                           y = buffer_uml_cs,
                                           sparse = T) %>% unlist()
          # do not select i 
          intersecters_id <- intersecters_id[!is.element(intersecters_id, i)]
      
          # intersecters_id are row indexes in buffer_uml_cs, they do NOT correspond to row.names (which are row indexes from uml)
          intersect_with_i <- buffer_uml_cs[intersecters_id]
          
          # Get the areas of each of the overlaps between other mills and mill i's buffer. 
          # Note that because areas over which several mill buffers overlap are counted once for 
          # each overlapping mill, such areas receive a higher weight, proportional to how many mills have potential influence over it. 
      
          areas <- lapply(1:length(intersect_with_i), 
                          FUN = function(j){
                                  st_intersection(buffer_uml_cs[i], 
                                                  intersect_with_i[j]) %>% 
                                  st_area() %>% 
                                  as.numeric()
                                  }) 
          # Divide by the total buffer area for each overlapping mill buffer to be weighted by a relative and not an absolute area.
          # uml_cs[[t]][i, "concentration"] <- areas %>% unlist() %>% sum()/total_buffer_area
          # This value can be interpreted as the number of mill buffers that cover 100% of one mill's buffer. 
          # Or equivalently as the share of buffer area that is overlaped by another mill. 
          areas %>% unlist() %>% sum()/total_buffer_area
        }) 
    }# closes the loop over years. 
    
    # sf class not needed anymore  
    uml_cs <- lapply(uml_cs, FUN = st_drop_geometry)
    # group annual cross sections to a long format panel of UML mills with time varying concentration variable
    uml_panel <- bind_rows(uml_cs, .id = "year")
    uml_panel <- dplyr::mutate(uml_panel,
                               year = years[as.numeric(year)])
    uml_panel <- dplyr::select(uml_panel, trase_code, uml_id, everything())
    
    saveRDS(uml_panel, paste0("./explanatory_variables/temp_mill_concentration_",
                              catchment_radius/1000,"km.rds"))
    
    # return(uml_panel)
  }
  
  ### Register cluster
  registerDoParallel(cores = ncores) 
  
  ### Define foreach object 
  foreach(catchment_radius = CRs, 
          #.combine = list by default, which is what we want
          # .multicombine = TRUE, # not necessary if .combine = cbind because then the default multicombine is TRUE anyways
          .inorder = FALSE, # we don't care that the results be combined in the same order they were submitted
          .export = c("indonesian_crs", "years"), 
          .packages = c("sf", "tidyverse", "here", "readstata13"),
          .verbose = TRUE) %dopar% 
    catchment_radius_mill_concentration(catchment_radius) # the function that is parallelly applied to different catchment_radius  
}# closes parallel_w_averages


tic()
parallel_mill_concentration(detectCores() - 1)
toc()

# NEXT STEPS WHEN THIS IS COMPLETED
# 
uml_panel_cr_paths <- list.files(path = file.path("./explanatory_variables"),
                                  pattern = "temp_mill_concentration_",
                                  full.names = TRUE)

uml_panel_cr_list <- lapply(uml_panel_cr_paths,
                            FUN = function(x){readRDS(x)})
# 1. Join the new colomn of each one. 
uml_panel <- uml_panel_cr_list[[1]]
uml_panel <- inner_join(uml_panel, 
                        uml_panel_cr_list[[2]][,c("trase_code", "year", "concentration_30")], 
                        by = c("trase_code", "year"))
uml_panel <- inner_join(uml_panel, 
                        uml_panel_cr_list[[3]][,c("trase_code", "year", "concentration_50")], 
                        by = c("trase_code", "year"))# saveRDS(uml_panel, here("build/output/UML_panel_valentin.rds"))

uml_panel[uml_panel$uml_id == "", "uml_id"] <- NA
uml_panel[uml_panel$parent_co == "", "parent_co"] <- NA
uml_panel[uml_panel$mill_name == "", "mill_name"] <- NA

write.dta(uml_panel, here("build/output/UML_panel_valentin.dta"))

# uml_panel[uml_panel$trase_code=="M-01200",]

# 2. then go to Stata add.variabes.do and see if there are other variables to merge with IBS_UML
# 
# 3. Then save this, it is IBS_UML_panel_final. 
#
# 4. Use it as the input to wa_at_parcels. Run this script
#
# 5. Finally run 
 
# 
# 
# 



# then we bind_rows(uml_cs, .id = "year") 
# and then arrange year column

# when panel format is done, 
# nest all this within a function of catchment_radius. 
# write it in parallel because this is a bit long (~20 min each)
# or we could write the loop over i in apply shape... 
# on peut essayer juste en modifiant la dernière ligne, celle de l'attribution, et 
# enlever la partie gauche de la flèche, et assigner directement le résultat de toutle 
# sapply (instead of for(i in...)) au vecteur uml_cs[[t]]$concentration






##### NOTES #####
# PREPARE IBS DATA # is removed because we rather build the concentration variable for all 
# UML mills, and not only IBS mills, in case this is useful to have the information for the
# larger group at some point. 

# ibs <- read.dta13(here("/build/output/IBS_UML_panel_final.dta"))  
# 
# # keep only geolocalized mills
# ibs <- ibs[!is.na(ibs$lat),]
# length(unique(ibs$firm_id))
# 
# # keep only the variables needed for this script's purpose. 
# ibs <- ibs[, c("firm_id", "year", "lat", "lon",
#                "min_year","est_year", "est_year_imp", "max_year")]
# 
# # add the concentration variable column
# ibs$concentration <- rep(0, nrow(ibs))
# ibs <- st_as_sf(ibs, coords =  c("lon", "lat"), crs = 4326)
# ibs <- st_transform(ibs, crs = indonesian_crs)
# ibs <- st_buffer(ibs, dist = catchment_radius)
# # make square buffers rather than circles, to avoid geometry issues later
# st_geometry(ibs) <- sapply(st_geometry(ibs), 
#                            FUN = function(x){st_as_sfc(st_bbox(x))}) %>% st_as_sfc()
# 
# # split to cross sections of IBS mills present each year
# ibs_cs <- lapply(years, FUN = function(x) ibs[ibs$year == x,]) 
# 
# 



#   # ST_INTERSECTION FOR NUMBER OF POLYGONS OVERLAPPING ON EACH AREA
#   # when called with a missing y, the sf method for st_intersection (...) two fields are added
#   # n.overlaps with the number of overlapping features in x, 
#   # and a list-column origins with indexes of all overlapping features.
#   
#   ## in order to get these two useful fields, we use st_intersection on a single sf object. 
#   
#   # First identify mills which buffers intersect with i * see note below *
#   intersecters_id <- st_intersects(x = buffer_uml_cs$geometry[i], 
#                                    y = buffer_uml_cs$geometry,
#                                    sparse = T) %>% unlist()
#   
#   intersect_with_i <- buffer_uml_cs[intersecters,]
#   
#   overlap_on_i <- st_intersection(intersect_with_i)
#   
#   
#   
#   # keep only the polygons that actually overlap i (otherwise output of st_intersection also has
#   # the overlaps of those buffers outside i. 
#   # i.e. we filter for polygons that have i as $origins - all those that have number 1.  
#   actual_overl <- c(1:length(overlap_on_i$origins))
#   for(k in 1:length(overlap_on_i$origins)){
#     actual_overl[k] <-  is.element(1, overlap_on_i$origins[[k]]) 
#   }
#   overlap_on_i <- overlap_on_i[actual_overl == TRUE,]
#   rm(actual_overl)
#   
#   # puis on calcule la somme des aires
#   
#   weighted_areas <- as.numeric(st_area(overlap_on_i$geometry))*(1/overlap_on_i$n.overlap)
#   heilmayr_desa_defo[i, exclu_index] <- (1/as.numeric(st_area(light_buffers[[km/10]][i,])))*sum(weighted_areas)
#   
#   rm(ids, intersect_with_i_in_t, prec, overlap_on_i, weighted_areas)
#   
# }


