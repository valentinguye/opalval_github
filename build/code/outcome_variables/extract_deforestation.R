###########################################################################################################
#                                                                                                         #
#     Computation of deforestation imputable to oil palms around georeferenced mills                      #
#                                                                                                         #
#     Inputs: - georeferenced mills (from georeferencing works)                                           #
#             ---> IBS_mills_geolocalized.dta                                                             #
#                                                                                                         #
#             - 17 rasters of oil palm-imputable deforestation defo_1.tif to defo_17.tif from             #
#               code prepare_deforestation_map.R                                                          #
#                                                                                                         #
#     Output: A data frame called heilmayr_desa_defo with annually deforested areas in                    #
#             different kinds of buffers                                                                  #
#             around palm oil mills.                                                                      #
#                                                                                                         #
###########################################################################################################
# will require Heilmayr's master mill list as well, in order to accound for as many mills as possible when defining 
# net buffers and computing the indicator the indicator of influence. 

#THIS SCRIPT'S STRUCTURE
############################################################################################################
# EXTRACT FOR SIMPLE BUFFERS
#   - define the simple buffer polygons
#   - extract 
# EXTRACT FOR NET BUFFERS (EXCLUSIVE PARTS OF SIMPLE BUFFERS)
#   - define the polygons
#   - extract
# COMPUTE AN INDICATOR OF INFLUENCE OVER CATCHMENT AREA 

############################################################################################################

rm(list = ls())

# PACKAGES
install.packages("sf", source = TRUE)
library(sf)

neededPackages = c("plyr", "dplyr", "readxl","foreign", "data.table", "readstata13", "here",
                   "rgdal", "raster", "velox","sp", "lwgeom", "rnaturalearth", 
                   "rlist", "parallel", "foreach", "iterators", "doParallel", )

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


##### PREPARE MILL POINTS #####
############################################################################################################

# read data.frame of cross-sectional mills with their coordinates. 
mills <- read.dta13("C:/Users/guyv/ownCloud/opalval/build/temp/mill_geolocalization/IBS_mills_geolocalized.dta")  

#turn into an sf object. 
mills <- st_as_sf(mills,	coords	=	c("lon",	"lat"), crs=4326)

# set CRS and project
st_crs(mills) 
# EPSG 4326 - proj4string: "+proj=longlat +datum=WGS84 +no_defs" i.e. unprojected because of crs argument in st_as_sf above. 

#   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
#   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator. 
#   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is 
#   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0 
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

mills_prj <- st_transform(mills, crs = indonesian_crs) 

rm(mills)
###########################################################################################################

# convert the mills data to a data frame: this will be the output object. 
mills_defo <- st_drop_geometry(mills_prj)



##### SIMPLE BUFFERS ##### 
#################################################################################################################################

#### CA DEFINITION ####

# The catchment areas are defined as buffers of radius 10 to 50 kilometers around mills.

# We will need projected mills' coordinates. 
mills_coord <- st_geometry(mills_prj)

simple_buffers <- list()
km <- 10
while(km < 60){
  simple_buffers[[km/10]] <- st_buffer(mills_coord, dist = km*1000) # units are meters. 
  km <- km + 10
}
rm(mills_coord)
# simple_buffers[[k]] is k-th element of a list. It is a sf data.frame. Hence simple_buffers[[1]][1] is 
# first column (+ associated geometry) of first element of the list of simple buffers. 

#### EXTRACTION ####

# Annual deforested areas are computed for the simple buffers

# A parallel computing set up has been used to speed things up. ( ///!!!\\\ )
# it has 3 steps: 


### 1. build the function that will be called in the foreach loop : 
annual_extract <- function(t, kilometers){
  
  # read in the raster of year t
  annualrastername <- paste0("defo_", years[t],".tif")
  annual_defo <- raster(annualrastername)
  
  # column name 
  simple_buffer_percentage <- paste0("pct_defo_simplebuf_",kilometers, "km_",t)
  #and size 
  heilmayr_desa_defo[simple_buffer_percentage] <- c(1:length(simple_buffers_sp))
  
  for(i in 1:length(simple_buffers_sp)){
    
    # crop it to the extent of CA i 
    single_annual_defo <- crop(annual_defo, extent(simple_buffers_sp[i,])) # this creates a tmpfile
    single_annual_defo <- velox(single_annual_defo) 
    
    # extract proportion of CA that is deforested. 
    heilmayr_desa_defo[i, simple_buffer_percentage] <- single_annual_defo$extract(simple_buffers_sp[i,], fun = function(x) mean(x, na.rm = TRUE))
    
    rm(single_annual_defo) 
    removeTmpFiles(h = 0)# removes the temporary file created by crop
  }
  rm(annual_defo, annualrastername)
  
  # compute the corresponding area
  simple_buffer_area <- paste0("area_defo_simplebuf_",kilometers, "km_",t)
  heilmayr_desa_defo[simple_buffer_area] <- buffer_area*heilmayr_desa_defo[simple_buffer_percentage]
  
  return(heilmayr_desa_defo[,c(simple_buffer_percentage,simple_buffer_area)])
}


### 2. build the parallel-looping function
years <- c(1:17)
extract_parallel <- function(cores, km){
  
  registerDoParallel(cores = cores) 
  
  # the loop has arguments to define how the results of the workers should be combined, and to give "workers" (CPUs) 
  # the objects and packages they need to run the function. 
  foreach(t = 1:length(years), .combine = cbind,
          .export = c("annual_extract", "heilmayr_desa_defo", "years", "simple_buffers_sp", "buffer_area"), 
          .packages = c("sp", "raster", "velox")) %dopar% 
    annual_extract(t, kilometers = km) # the function that is parallelly applied to different years. 
}


### 3. run it for each buffer size
km <- 10 
while(km < 60){
  #These objects are only dependent on km. 
  simple_buffers_sp <- as(simple_buffers[[km/10]], "Spatial") # Convert polygons of interest as SpatialPolygons.
  
  buffer_area <- as.numeric(st_area(simple_buffers[[km/10]][1])) # Compute area (only valid for CA = simple_buffers (being a list))
  
  # attach the output columns to the base mill dataframe. 
  heilmayr_desa_defo <- cbind(heilmayr_desa_defo, extract_parallel(cores = detectCores() - 1, km = km))
  
  km <- km + 10
}

saveRDS(heilmayr_desa_defo, file = "heilmayr_desa_defo.Rdata") 
save(heilmayr_desa_defo, file = "heilmayr_desa_defo_alt.Rdata")
write.xlsx(heilmayr_desa_defo, "heilmayr_desa_defo.xlsx", col.names = TRUE)

#################################################################################################################################



##### NET BUFFERS ##### 
#################################################################################################################################

# i.e. parts of CAs in which plantations belong to no other CA (hence buffers net of overlaps)


#### CA DEFINITION #### 

### create polygons for buffers of different radiuses (similar to just above but keeps the non geometric information - the est_year in particular)
simple_buffers <- list()
km <- 10
#listnames <- c(1:9)
while(km < 100){
  simple_buffers[[km/10]] <- st_buffer(mills_prj, dist = km*1000)
  #listnames[km/10]<- paste0("of",km, "km" )
  km <- km + 10
}

### create structure for annual net_buffers storage. 
years <- c(1:17)
net_buffers <- list()
km <- 10
while(km < 60){
  net_buffers[[km/10]] <- list(1:length(years)) # each of 5 slots of net_buffers is a vector of 17 elements
  
  for(t in 1:length(years)){
    net_buffers[[km/10]][[t]] <- simple_buffers[[km/10]]  # each of these 17 elements is a sf dataframe of 1083 buffer polygons.
  }
  km <- km + 10 
}


### fill in this structure with the net buffer polygons calculated for 5 first buffer sizes and 
#annually excluding overlapping buffers of mills that were not already there
#mills_coord <- st_geometry(mills_prj)
km <- 10
e <- st_sfc(st_polygon())
while(km < 60){
  for(t in 1:length(years)){
    # identify mills (buffers) that were existing in each year. 
    existing <- simple_buffers[[km/10]]$est_year <= 2000 + t | is.na(simple_buffers[[km/10]]$est_year) # ///!!!\\\ ASSUMPTION MADE here: mill was here from the begining when NA. 
    
    for(i in 1:nrow(heilmayr_desa_prj)){     
      #
      buffer <- simple_buffers[[km/10]][i,] # see note below
      others <- st_union(st_geometry(simple_buffers[[km/10]][-i,][existing,]))
      net_buffer <- st_difference(x = st_geometry(buffer), y = st_geometry(others))
      
      
      if(length(net_buffer) == 1){
        
        #st_geometry(net_buffers[[km/10]][[t]][i,]) <- net_buffer
        st_set_geometry(net_buffers[[km/10]][[t]][i,], value = net_buffer)
      } else{
        #point_poly <- st_polygon(list(st_coordinates(mills_coord[i])))
        #point_sfc_POLY <- st_sfc(point_poly, crs = indonesian_crs)
        #st_geometry(net_buffers[[km/10]][[t]][i,]) <- st_transform(point_sfc_POLY, crs = indonesian_crs)
        #rm(point_poly, point_sfc_POLY)
        st_geometry(net_buffers[[km/10]][[t]][i,]) <- e
      }
      
      rm(buffer, others, net_buffer)
    }
    
    rm(existing)
  }
  km <- km + 10
}
rm(e)

save(net_buffers, file = "C:/Users/guyv/ownCloud/opalval/build/temp/net_buffers_alt.Rdata")
saveRDS(net_buffers, file = "C:/Users/guyv/ownCloud/opalval/build/temp/net_buffers.Rdata")


# Note : in each year all firms are selected for their net CAs to be computed, irrespective of whether they appeared later or not. 
# Hence leaving selection of useful data to a later stage (and giving unnecessary work to the extraction below). 
# Practical reason for this : the extraction requires the template heilmayr_desa_defo and the polygons collection to have the same number of rows.
# "workflow" reason: let's not exclude the possibility that a mill is given a year of appearance t here but actually appears earlier in IBS data. 

##### EXTRACTION #####


#load net_buffers geometries computed above
load("C:/Users/guyv/ownCloud/opalval/build/temp/net_buffers_alt.Rdata")

# for memory reasons, add the columns to the "empty version" rather than to the version with 170 columns
# of simple buffers. 
heilmayr_desa_defo <- st_drop_geometry(heilmayr_desa_prj)

# A parallel computing set up has been used to speed things up. ( ///!!!\\\ )
# it has 3 steps: 


### 1. build the function that will be called in the foreach loop (it's different than the function used on simple buffers) 
annual_extract <- function(t, kilometers){
  
  # read in the raster of year t
  annualrastername <- paste0("defo_", years[t],".tif")
  annual_defo <- raster(annualrastername)
  
  # column names 
  net_buffer_percentage <- paste0("pct_defo_netbuf_",kilometers, "km_",t)
  net_buffer_area <- paste0("area_defo_netbuf_",kilometers, "km_",t)
  
  #and sizes 
  heilmayr_desa_defo[net_buffer_percentage] <- c(1:nrow(heilmayr_desa_defo))
  heilmayr_desa_defo[net_buffer_area] <- c(1:nrow(heilmayr_desa_defo))
  
  # and content
  for(i in 1:3){
    #nrow(heilmayr_desa_defo)
    #distinguish cases when the net buffer is not empty from cases of emptiness. 
    if(length(net_buffers[[kilometers/10]][[t]][[i,"geometry"]]) > 0 ){
      # use an sp object
      #net_buffers_sp <- as(st_make_valid(net_buffers[[kilometers/10]][[t]][i,]), "Spatial")
      net_buffers_sp <- as(net_buffers[[kilometers/10]][[t]][i,], "Spatial")
      
      # crop it to the extent of CA i and velox it 
      single_annual_defo <- crop(annual_defo, extent(net_buffers_sp)) # this creates a tmpfile
      single_annual_defo <- velox(single_annual_defo) 
      
      # extract proportion of CA that is deforested. 
      heilmayr_desa_defo[i, net_buffer_percentage] <- single_annual_defo$extract(net_buffers_sp, fun = function(x) mean(x, na.rm = TRUE))
      
      # compute the area of the corresponding polygon 
      buffer_area <- as.numeric(st_area(net_buffers[[kilometers/10]][[t]][i,]))
      heilmayr_desa_defo[i, net_buffer_area] <- buffer_area*heilmayr_desa_defo[i, net_buffer_percentage]
      
      rm(single_annual_defo, buffer_area)
      removeTmpFiles(h = 0)# removes the temporary file created by crop
      
    } else {
      heilmayr_desa_defo[i, net_buffer_percentage] <- NA
      heilmayr_desa_defo[i, net_buffer_area] <- NA 
    }
    
  }
  rm(annual_defo, annualrastername)
  
  return(heilmayr_desa_defo[,c(net_buffer_percentage, net_buffer_area)])
}

### 2. build the parallel-looping function
years <- c(1:17)
extract_parallel <- function(cores, km){
  
  registerDoParallel(cores = cores)
  # the loop
  foreach(t = 1:length(years), .combine = cbind,
          .export = c("annual_extract", "heilmayr_desa_defo", "years", "net_buffers"), 
          .packages = c("sf", "sp", "raster", "velox")) %dopar% 
    annual_extract(t, kilometers = km) # the function that is parallelly applied to different years. 
}

### 3. run it for each buffer size

km <- 10 
while(km < 60){
  heilmayr_desa_defo <- cbind(heilmayr_desa_defo, extract_parallel(cores = detectCores() - 1, km = km))
  
  km <- km + 10
}

saveRDS(heilmayr_desa_defo, file = "heilmayr_desa_defo_net.Rdata")
save(heilmayr_desa_defo, file = "heilmayr_desa_defo_net_alt.Rdata")
write.xlsx(heilmayr_desa_defo, file = "heilmayr_desa_defo_net_alt.xlsx", col.names = TRUE)

# Now merge the two data frames. 
# remove first columns of net
#cbind it to heilmayr_desa_defo



## NOW IF WE WANTED TO COMPUTE AN INDICATOR OF HOW MUCH OVERLAPPED CAs ARE EVERY YEAR. 
# we can compute the number of mills whose CA intersect. 
# compute the proportion of exclusive area. 
# one solution is the share of area weighted  average of number of other mills overlapping. because it's not 
# the same to be overlapped by one or by 8 others
# SO THE FORMULA IS: sum(n, n=[0,N], sha_n*(1/n+1)) 
# where sha_n is the share of CA that is shared with n other CAs.
# use st_intersection ? 

# j'ai l'impression qu'on peut tout faire avec st_intersection: 
# chaue element de la list-colonne origins est un integer de length diff?rent, selon le nombre de 
# polygons qui se superpose pour la ligne en question. 
# length(i$origins[[k]]) = n+1
# mais ce length c'est aussi la valeur de n.overlaps en fait ^^ 
# ou k correspond ? l'une des geometries output de st_intersection, soit un polygon sans overlap, soit 
# avec 1, 2, 3 .. overlaps. 
# autrement dit les weights peuvent ?tre 1/length(.)

#pour chaque CA i
# il faut r?ussir ? s?lectionner tous les polygons qui ont "i" en origins. 
# puis grouper ceux qui ont le m?me nombre d'overlaps. ou meme pas d'ailleurs en fait
# on peut juste "d?composer" sha_n en sha_n_1 sha_n_2 etc. i.e. les proportions du buffer total
# des diff?rentes parties qui sont partag?es par n autres. 

# une mani?re de s?lectioner ce serait, pour chaque CA, de faire la liste des CAs qui l'intersectent. 
# et faire st_intersection parmi cette liste. 
# l'output i devrait toujours avoir 1 dans l'origins, 
# NOP il y a aussi toutes les intersections entre les "autres" "en-dehors" le CA d'int?r?t. 

# puis on calcule la somme des st_area(i[r,])/st_area(simple_buffer[[km/10]][k,])*1/i[r, n.overlaps]

# RESTE A REGLER QUE ST_INTERSECTION NE FONCTIONNE PAS POUR CERTAINES GEO.


#A workaround on this appears to be to call st_make_valid AFTER you set the precision:
# p2 <- readRDS("D:/p2.rds") %>% st_sfc() %>% st_set_precision(1000000) %>% lwgeom::st_make_valid()
# peut regler le pb des slivers mais pas des geom vides. 

# see also https://www.r-spatial.org/r/2017/03/19/invalid.html#empty-geometries


### create the usual polygons for buffers of different radiuses (similar to just above but keeps the non geometric information - the est_year in particular)
simple_buffers <- list()
km <- 10
while(km < 100){
  simple_buffers[[km/10]] <- st_buffer(heilmayr_desa_prj, dist = km*1000)
  km <- km + 10
}

#annual exclusivity indexes are stored in the main data.frame 
load("C:/Users/guyv/ownCloud/opalval/build/temp/heilmayr_desa_defo_alt.Rdata")

years <- c(1:17)
light_buffers <- list()
km <- 10
while(km < 60){
  # lighter version of simple_buffers
  light_buffers[[km/10]] <- st_sf(st_geometry(simple_buffers[[km/10]]))
  
  for(t in 1:length(years)){
    # filter mills actually existing each year. 
    existing <- simple_buffers[[km/10]]$est_year <= 2000 + t | is.na(simple_buffers[[km/10]]$est_year) # ///!!!\\\ ASSUMPTION MADE here: mill was here from the begining when NA. 
    
    # new columns names
    exclu_index <- paste0("exclusivity_index",km, "km_",t)
    
    #and sizes 
    heilmayr_desa_defo[,exclu_index] <- c(1:nrow(heilmayr_desa_defo))
    
    for(i in 1:nrow(heilmayr_desa_defo)){
      
      if(existing[i] == TRUE){
        #identify polygons that intersect with i * see note below *
        ids <- unlist(st_intersects(x = light_buffers[[km/10]][i,], y = light_buffers[[km/10]][existing,], sparse = TRUE))
        
        # gather them 
        intersect_with_i_in_t <- light_buffers[[km/10]][existing,][ids,]
        
        #compute the intersection information
        #intersect_with_i_in_t <- st_buffer(intersect_with_i_in_t, dist = 0)
        #intersect_with_i_in_t <- intersect_with_i_in_t %>% st_set_precision(1e7) %>% lwgeom::st_make_valid()
        # set the degree of precision that works when there are invalid geom. 
        prec <- 10
        while(prec < 1e10){
          try(overlaps_on_i <- st_set_precision(intersect_with_i_in_t, prec) %>% st_intersection(), silent = FALSE
          )
          try(
            if(length(overlaps_on_i) == 3){break}, silent = TRUE
          )
          prec <- prec*10
        }
        
        prec <- 10
        while(prec < 1e10){
          try(overlaps_on_i <- st_set_precision(intersect_with_i_in_t, prec) 
              %>% st_intersection())
          #%>%  st_collection_extract("POLYGON") 
          try(
            if(length(overlaps_on_i) == 3){break}
          )
          prec <- prec*10
        }
        
        # keep only the polygons that actually overlap i (otherwise output of st_intersection also has 
        # overlaps "outside" i. 
        # i.e. we filter for polygons that have i as $origins - all those that have number 1.  
        actual_overl <- c(1:length(overlaps_on_i$origins))
        for(k in 1:length(overlaps_on_i$origins)){
          actual_overl[k] <-  is.element(1, overlaps_on_i$origins[[k]]) 
        }
        overlaps_on_i <- overlaps_on_i[actual_overl == TRUE,]
        rm(actual_overl)
        
        # puis on calcule la somme des aires
        weighted_areas <- as.numeric(st_area(overlaps_on_i$geometry))*(1/overlaps_on_i$n.overlaps)
        heilmayr_desa_defo[i, exclu_index] <- (1/as.numeric(st_area(light_buffers[[km/10]][i,])))*sum(weighted_areas)
        
        rm(ids, intersect_with_i_in_t, prec, overlaps_on_i, weighted_areas)
        
      } else {
        heilmayr_desa_defo[i, exclu_index] <- NA
      }
    }
    
    rm(existing, exclu_index)
  }
  
  km <- km + 10
}




tol <- 1
while(tol < 100){
  try(overlaps_on_i <- st_snap(intersect_with_i_in_t, intersect_with_i_in_t, tolerance = tol) 
      %>% st_intersection())
  #%>%  st_collection_extract("POLYGON") 
  try(
    if(length(overlaps_on_i) == 3){break}
  )
  tol <- tol + 1
}


plot(st_geometry(exind[[km/10]][[t]][1,]))
plot(st_geometry(exind[[km/10]][[t]][existing,][unlist(intersect_with_i_in_t),]), add = TRUE)

y <- exind[[km/10]][[t]][existing,]
simple_buffers[[km/10]][i,]



for(i in 1:nrow(heilmayr_desa_prj)){     
  #
  buffer <- simple_buffers[[km/10]][i,] # see note below
  others <- st_union(st_geometry(simple_buffers[[km/10]][-i,][existing,]))
  net_buffer <- st_difference(x = st_geometry(buffer), y = st_geometry(others))
  
}  

### NOTE  
# LES POLYGONS CORRESPONDANT SONT CEUX DONT CES NUM?ROS SONT LES INDICES *DANS exind[[km/10]][[t]][existing,]*
# i.e. les valeurs du sparse predicate sont les positions
# DANS Y (donc 1:488 pour t < 2004 par ex.) des polygons qui intersectent. 
# donc quand on utilise ces positions pour filtrer dans l'ensemble des mills on s?lectionne d'autres poly
# cela vient de la mani?re dont st_intersects works.

# exemple canonique internet 
pol = st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
b = st_sfc(pol, pol + c(.8, .2), pol + c(.2, .8))
par(mar = rep(0, 4))
plot(b, col = NA)
i = st_intersection(st_sf(b))
par(mar = rep(0, 4))
cl = sf.colors(3, categorical = TRUE)
plot(b)
plot(i[i$n.overlaps == 3,2], col = cl[1], add = TRUE)
