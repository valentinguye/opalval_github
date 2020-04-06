### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 
# 
#       BUILDING DATAFRAMES OF OUR OUTCOME VARIABLE: THE LAND USE CHANGE FROM FOREST TO PLANTATION (LUCFP)
# 
#   Inputs:   - georeferenced mills (from georeferencing works)                                           
#             ---> IBS_UML_cs.dta                                                             
#                                                                                                         
#             - 2000 and 2015 oil palm plantations (Austin et al. 2017) for Sumatra, Kalimantan and Papua,
#               pre-processed in prepare_palmoil_map.R                         
#             ---> new_oilpalm_2000_WGS1984.tif 
#             ---> new_oilpalm_2015_WGS1984.tif 
#                                                                                                         
#             (- Global Forest Change (Hansen et al. 2013) tiles downloaded from internet)                                      
# 
#   Outputs:  panel dataframes of 2001-2018 parcels,  
#             for whole Indonesia (Sumatra, Kalimantan, Papua stacked), 
#             for 3 forest definitions (30, 60, 90 percent forest cover).
#             
#             One such dataframe for each combination of parcel size (only 3x3km for now) and catchment radius (10, 30, 50km)
#             ---> panel_Indonesia_3km_10CR.Rdata 
#             ---> panel_Indonesia_3km_30CR.Rdata 
#             ---> panel_Indonesia_3km_50CR.Rdata
# 
#   
#   Actions:  This script consists of mainly three functions.  
#             0. load needed packages; set working directory; set raster options; define the crs used throughout the script. 
#                 /// !!! \\\ the chunksize and maxmemory raster options should be set accordingly with the machine used,  
#                             considering that this script executes parallel functions using parallel::detectCores() - 1
#                             For instance, here we set chunksize to 1Go so that our 3 working cores processed 3Go together. 
#
#             1. prepare_pixel_lucfp(island)
# 
#             2. aggregate_lucfp(island, parcel_size)
# 
#             3. to_panel_within_CR(island, parcel_size, catchment_radius)
#
#             Finally, functions are run and there outputs are merged across islands and forest thresholds.   
# 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 



rm(list = ls())

##### 0. PACKAGES, WD, OBJECTS #####

### PACKAGES ###

# sf need to be installed from source for lwgeom te be installed from source. 
if (!require(sf)) install.packages("sf", source = TRUE)
#"plyr", 
neededPackages = c("dplyr", "raster", "foreign", "sp", "lwgeom", "rnaturalearth", "data.table",
                   "rgdal", "readstata13",
                   "rlist", "velox", "parallel", "foreach", "iterators", "doParallel", "readxl", "here")
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

#install.packages("sf", source = TRUE)
#if (!require(devtools)) install.packages("devtools")
#devtools::install_github("r-spatial/lwgeom")
#library(lwgeom)

#INSTALL GFC ANALYSIS
# Install the snow package used to speed spatial analyses
if (!require(snow)) install.packages("snow")
library(snow)
# Install Alex's gfcanalysis package
if (!require(gfcanalysis)) install.packages('gfcanalysis')
library(gfcanalysis)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### WORKING DIRECTORY ### 
setwd(here("build/input/outcome_variables"))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### RASTER OPTIONS ### 
rasterOptions(chunksize = 1e+9,
              timer = TRUE)

### INDONESIAN CRS ### 
#   Following http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/map%20coordinate%20systems/how%20to%20choose%20a%20projection.htm
#   the Cylindrical Equal Area projection seems appropriate for Indonesia extending east-west along equator.
#   According to https://spatialreference.org/ref/sr-org/8287/ the Proj4 is
#   +proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#   which we center at Indonesian longitude with lat_ts = 0 and lon_0 = 115.0
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


##### 1. PREPARE 30m PIXEL-LEVEL MAPS OF LUCFP ##### 

prepare_pixel_lucfp <- function(island){
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  #### Define area of interest (AOI) ####
  
    # read data.frame of cross-sectional mills with their coordinates.
    mills <- read.dta13(here("build/input/IBS_UML_cs.dta"))

    mills <- mills[mills$island_name == island,]

    #turn into an sf object.
    mills <- st_as_sf(mills,	coords	=	c("lon",	"lat"), crs=4326)
    # keep only the geometry, we do not need mills attributes here.
    mills <- st_geometry(mills)
    # set CRS and project
    mills_prj <- st_transform(mills, crs = indonesian_crs)
    st_crs(mills_prj) # units are meters.

    #define big catchment areas to have a large AOI.
    mills_ca <- st_buffer(mills_prj, dist = 60000)

    # work with squares rather than with circles
    for(i in 1:length(mills_ca)){
      mills_ca[i] <- st_as_sfc(st_bbox(mills_ca[i]))
    }

    # and dissolve them in one polygon aoi <- st_union(st_geometry(mills_ca))
    # rather use a BBOX
    aoi <- st_as_sfc(st_bbox(mills_ca))

    # unproject to use extract_gfc with to_UTM = FALSE
    aoi <- st_transform(aoi, crs = 4326)
    #convert the box to a SpatialPolygon object for compatibility with download_tiles methods.
    aoi_sp <- as(aoi, "Spatial")


    rm(mills, mills_prj, mills_ca, aoi)
    # note on rm in function: removes object in the function "frame" i.e. environment without
    # having to specify something (I tested it)

    
    
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
    #### Download GFC data ####

    #define where all tiles are going to be stored
    data_folder <- paste0(getwd(), "/GFC_tiles")

    # Calculate tiles needed to cover the AOI
    tiles <- calc_gfc_tiles(aoi_sp)
    # length(tiles) 

    # version of GFC used here.
    gfc_version <- "GFC-2018-v1.6"
    # //!\\ this script is not written flexibly to adjust for other versions of GFC. One should check every "18" entries in this script for instance. 

    #download tiles - with all layers otherwise later extract_gfc does not work
    download_tiles(tiles, 
                   output_folder = data_folder, 
                   images = c("treecover2000", "lossyear", "gain", "datamask"), 
                   dataset = gfc_version)

    rm(tiles)
    
    # extract gfc data (can only extract all layers with default stack=change)
    # to better understand extract_gfc see https://rdrr.io/cran/gfcanalysis/src/R/extract_gfc.R
    extract_gfc(aoi_sp, data_folder,
                stack = "change",
                to_UTM = FALSE,
                dataset = gfc_version,
                filename = paste0("gfc_data_",island,".tif"),
                overwrite = TRUE )


    
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
    #### Threshold GFC data based on a specified percent cover threshold: 30%, 60% and 90% here. ####

    ### Function description
    # Computes (in particular) a forest loss layer based on what pixel-level canopy cover percentage is the threshold
    # between forest and non-forest state in 2000.
    # The task is parallelly executed for 3 different threshold values.
    parallel_threshold_gfc <- function(ncores){

      ## sequence over which to execute the task
      thresholdS <- seq(from = 30, to = 90, by = 30)

      ## read the input to the task (rather than calling it again within each task)
      gfc_data <- brick(paste0("gfc_data_",island,".tif"))

      ## define the task
      # function threshold_gfc is already defined in package gfc_analysis

      ## register cluster
      registerDoParallel(cores = ncores)

      ## define foreach object
      foreach(th = thresholdS,
              # .combine combine the outputs as a mere character list (by default)
              .inorder = FALSE, # we don't care that the results be combine in the same order they were submitted
              .multicombine = TRUE,
              .export = c("island"),
              .packages = c("raster", "gfcanalysis", "rgdal")
      ) %dopar% threshold_gfc(gfc_data,
                              forest_threshold=th,
                              filename=paste0("gfc_data_",island,"_",th,"th.tif"),
                              overwrite = TRUE )
      }

    #### Execute it
    parallel_threshold_gfc(detectCores() - 1)
    
    
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  #### Align plantation maps on GFC maps ####
    
  # po maps are disaggregated and will match loss res, ext, and crs. Both are unprojected at this stage.
  # po resolution is 0.002277, 0.002277
  # loss resolution is 0.00030, 0.00025
  # first crop po maps to the current aoi (island bbox) to match res.
  # then projectRaster to match res - disaggregate before is not necessary (yields the same result)
  
  ### read gfc_data, the target of the align operations
  gfc_data <- brick(paste0("gfc_data_",island,".tif"))
  
  ### 2000 plantations
  po2000 <- raster(here("build/input/PALMOIL/new_oilpalm_2000_WGS1984.tif"))
  
  ## match extent
  crop(po2000, y = gfc_data,
       filename = paste0("./oilpalm_2000_",island,"_croped.tif"),
       datatype = "INT1U",
       overwrite = TRUE)
  # 3 minutes, not even printed
  
  ## match resolution.
  po2000 <- raster(paste0("./oilpalm_2000_",island,"_croped.tif"))
  # we run it within a cluster because according to ?clusterR
  # "projectRaster has a build-in capacity for clustering that is automatically used if beginCluster() has been called."
  #beginCluster()
  projectRaster(from = po2000, to = gfc_data,
                method = "ngb",
                filename = paste0("./oilpalm_2000_",island,"_aligned.tif"),
                datatype = "INT1U",
                overwrite = TRUE )
  #endCluster()
  rm(po2000)
  # 18700 seconds
  
  ### 2015 plantations
  po2015 <- raster(here("build/input/PALMOIL/new_oilpalm_2015_WGS1984.tif"))
  
  ## match extent
  crop(po2015, y = gfc_data,
       filename = paste0("./oilpalm_2015_",island,"_croped.tif"),
       datatype = "INT1U",
       overwrite = TRUE)

  ## match resolution
  po2015 <- raster(paste0("./oilpalm_2015_",island,"_croped.tif"))
  # beginCluster()
  projectRaster(from = po2015, to = gfc_data,
                method = "ngb",
                filename = paste0("./oilpalm_2015_",island,"_aligned.tif"),
                datatype = "INT1U",
                overwrite = TRUE )
  # endCluster()
  # 9436 seconds
  rm(po2015)
  rm(gfc_data)
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  #### Overlay forest loss and oil palm plantation maps ####
  
  # We want to keep forest loss pixels only within 2015 plantations in order to induce forest conversion to plantation,
  # BUT outside 2000 plantations, in order not to count plantation renewals as forest conversion to plantation.
  # po maps are binary with 1 meaning plantation in 2015 (or 2000 resp.))
  
  po2000 <- raster(paste0("./oilpalm_2000_",island,"_aligned.tif"))
  po2015 <- raster(paste0("./oilpalm_2015_",island,"_aligned.tif"))
  
  # overlay function
  overlay_maps <- function(rs){rs[[1]]*(1-rs[[2]])*rs[[3]]}
  # multiplies a cell of forest loss (rs[[1]]) by 0 if it it is a plantation in 2000 (rs[[2]]) or if is not a plantation in 2015 (rs[[3]])
  
  ### For each threshold, overlay forest loss map with plantations maps in a clusterR setting 
  th <- 30
  while(th < 100){
    # call the loss layer for threshold th
    thed_gfc_data <- brick(paste0("gfc_data_",island,"_",th,"th.tif"))
    # select the loss layer
    loss <- thed_gfc_data[[which(thed_gfc_data@data@max > 15 & thed_gfc_data@data@max < 40)]]
    # remove useless other stack of gfc layers
    rm(thed_gfc_data)
    
    # stack loss with plantation maps (necessary for clusterR)
    rs <- stack(loss, po2000, po2015)
    # run the computation in parallel with clusterR, as cells are processed one by one independently.
    beginCluster() # uses by default detectedCores() - 1
    clusterR(rs,
             fun = calc, # note we use calc but this is equivalent to using overlay 
             # (but more appropriate to the input being a stack)
             args = list(overlay_maps),
             filename = paste0("lucfp_",island,"_",th,"th.tif"),
             datatype = "INT1U",
             overwrite = TRUE )
    endCluster()
    rm(loss)
    th <- th + 30
  }
  # ~ 4500 seconds / threshold
  rm(po2000, po2015, overlay_maps)
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  #### Project LUCFP ####

  # This is necessary because we will need to make computations on this map within mills' catchment *areas*.
  # If one does not project this map, then catchment areas all have different areas while being defined with a common buffer.
  
  ### Function description
  # Project to Indonesian crs, in parallel, for each threshold definition. 
  parallel_project_lucfp <- function(ncores){
    
    ## sequence over which to execute the task
    thresholdS <- seq(from = 30, to = 90, by = 30)

    ## read the input to the task
    # it is task specific

    ## define the task
    project_lucfp <- function(threshold){
      lucfp <- raster(paste0("lucfp_",island,"_",threshold,"th.tif"))
      projectRaster(from = lucfp,
                    crs = indonesian_crs,
                    method = "ngb",
                    filename = paste0("lucfp_",island,"_",threshold,"th_prj.tif"),
                    datatype = "INT1U",
                    overwrite = TRUE )
    }

    ## register cluster
    registerDoParallel(cores = ncores)

    ## define foreach object
    foreach(th = thresholdS,
            # .combine combine the outputs as a mere character list (by default)
            .inorder = FALSE, # we don't care that the results be combine in the same order they were submitted
            .multicombine = TRUE,
            .export = c("island", "indonesian_crs"),
            .packages = c("raster", "rgdal")
            ) %dopar% project_lucfp(threshold = th)
  }

  ### Execute it
  parallel_project_lucfp(detectCores() - 1)
    
    
  # 13571 seconds
  # 12459 seconds
  # 11565 seconds
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  #### Split LUCFP to annual layers ####

  ### Function description
  # parallel_split has for input the single lucfp layer where each pixel has a value corresponding to the year when a lucfp event occured;
  # it outputs annual layers in each of which pixels are either 1 if a lucfp event occured that year, and 0 else.
  # the tasks are year specific and independent across years, therefore they are executed parallely over years.
  parallel_split <- function(th, ncores){
    
    ## sequence over which to execute the task.
    # We attribute the tasks to CPU "workers" at the annual level and not at the threshold level.
    # Hence, if a worker is done with its annual task before the others it can move on to the next one and workers' labor is maximized wrt.
    # attributing tasks at the threshold level.
    years <- seq(from = 2001, to = 2018, by = 1)
    
    ## read the input to the task
    # is done within each task because it is each time different here.
    
    ## define the task
    annual_split <- function(time, threshold){
      # define process
      process <- file.path(paste0("lucfp_",island,"_",threshold,"th_prj.tif"))
      # #set temp directory
      dir.create(paste0(process,"_Tmp"), showWarnings = FALSE)
      rasterOptions(tmpdir=file.path(paste0(process,"_Tmp")))
      # read in annual raster layer
      lucfp_prj <- raster(process)
      # split it into annual binary layers
      calc(lucfp_prj,
           fun = function(x){if_else(x == time, true = 1, false = 0)},
           filename = paste0("./annual_maps/lucfp_",island,"_",threshold,"th_", years[time],".tif"),
           datatype = "INT1U",
           overwrite = TRUE )
      # remove process temporary files
      unlink(file.path(paste0(process,"_Tmp")), recursive = TRUE)
    }
    
    ## register cluster
    registerDoParallel(cores = ncores)
    
    ## define foreach object.
    foreach(t = 1:length(years),
            # .combine combine the outputs as a mere character list (by default)
            .inorder = FALSE, # we don't care that the results be combine in the same order they were submitted
            .multicombine = TRUE,
            .export = c("annual_split", "years", "island"),
            .packages = c("dplyr", "raster", "rgdal")
    ) %dopar%  annual_split(time = t, threshold = th)
  }
  
  ### Execute it for each forest definition
  th <- 30
  while(th < 100){
    parallel_split(th, detectCores() - 1) # ~500 seconds / annual layer
    th <- th + 30
  }
  return(print("end"))
}



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

##### 2. AGGREGATE THE PIXELS TO A GIVEN PARCEL SIZE. #####

aggregate_lucfp <- function(island, parcel_size){
  
  ### Function description
  # The function has for inputs annual layers of lucfp events at the pixel level.
  # It aggregates these pixels to a parcel size defined by parcel_size (in meters).
  # The aggregation operation is the sum of the pixel lucfp events.
  # Each annual aggregation is tasked in parallel.
  parallel_aggregate <- function(th, ncores){
    
    ## sequence over which to execute the task.
    # We attribute the tasks to CPU "workers" at the annual level and not at the threshold level.
    # Hence, if a worker is done with its annual task before the others it can move on to the next one and workers' labor is maximized wrt.
    # attributing tasks at the threshold level.
    years <- seq(from = 2001, to = 2018, by = 1)
    
    ## read the input to the task
    # is done within each task because it is each time different here.
    
    ## define the task
    annual_aggregate <- function(time, threshold){
      # Define which process (year and threshold) we are in:
      processname <- file.path(paste0("./annual_maps/lucfp_",island,"_",threshold,"th_", years[time],".tif"))
      #create process-specific temp directory
      dir.create(paste0(processname,"_Tmp"), showWarnings = FALSE)
      #set temp directory
      rasterOptions(tmpdir=file.path(paste0(processname,"_Tmp")))
      # read in the indonesia wide raster of lucfp at a given time and for a given threshold.
      annual_defo <- raster(processname)
      # aggregate it from the 30m cells to parcel_sizem cells with mean function.
      raster::aggregate(annual_defo, fact = c(parcel_size/res(annual_defo)[1], parcel_size/res(annual_defo)[2]),
                        expand = FALSE,
                        fun = sum,
                        na.rm = FALSE, # NA cells are in margins, see the NOTES part. If FALSE, aggregations at margins that use NA 
                        # are discarded because the sum would be spurious as it would count all NA as 0s while it is not necessary the case.
                        filename = paste0("./annual_parcels/parcels_",island,"_",parcel_size/1000,"km_",threshold,"th_",years[time],".tif"),
                        datatype = "INT4U", # because the sum may go up to ~ 10 000 with parcel_size = 3000,
                        # but to more than 65k with parcel_size = 10000 so INT4U will be necessary;
                        overwrite = TRUE)
      #removes entire temp directory without affecting other running processes (but there should be no temp file now)
      unlink(file.path(paste0(processname,"_Tmp")), recursive = TRUE)
      #unlink(file.path(tmpDir()), recursive = TRUE)
      # return the path to this parcels file
      return(file.path(paste0("./annual_parcels/parcels_",island,"_",parcel_size/1000,"km_",threshold,"th_",years[time],".tif")))
    }
    
    ## register cluster
    registerDoParallel(cores = ncores)
    
    ##  define foreach object.
    foreach(t = 1:length(years),
            # .combine combine the outputs as a mere character list (by default)
            .inorder = FALSE, # we don't care that the results be combine in the same order they were submitted
            .multicombine = TRUE,
            .export = c("annual_aggregate", "years", "island", "parcel_size"),
            .packages = c("raster", "rgdal")
    ) %dopar% annual_aggregate(time = t, threshold = th)
  }
  
  ### Execute the function to compute the RasterBrick object of 18 annual layers for each forest definition threshold
  th <- 30
  while(th < 100){
    # run the computation, that writes the layers and return a list of their paths
    rasterlist <- parallel_aggregate(th, detectCores() - 1)
    # brick the layers together
    parcels_brick <- brick(rasterlist)
    # write it
    writeRaster(parcels_brick,
                filename = paste0("./bricked_parcels/parcels_",island,"_",parcel_size/1000,"km_",th,"th.tif"),
                datatype = "INT4U",
                overwrite = TRUE)
    rm(rasterlist, parcels_brick)
    th <- th + 30
  }
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


##### 3. CONVERT TO A DATAFRAME THE PARCELS WITHIN A GIVEN CATCHMENT RADIUS  #####

to_panel_within_CR <- function(island, parcel_size, catchment_radius){
  
  ### Function description
  # threshold_raster_to_df converts the raster bricks of annual layers of parcels to a panel dataframe.
  # This is executed for each threshold, iteratively and not in parallel (not necessary because quite fast)
  # The tasks are:
  # 1. masking the brick of parcels of a given size (parcel_size) on a given island with the maximal CA of mills on that island;
  # 2. selecting only the parcels that are within a given catchment radius.
  # 3. reshaping the values in these parcels to a long format panel dataframe
  threshold_raster_to_df <- function(threshold){
    
    years <- seq(from = 2001, to = 2018, by = 1)
    
    ## 1. Masking.
    # Probably more efficient as the st_is_within does not need to be executed over all Indonesian cells but only those within the largest catchment_radius.
    
    # Make the mask
    mills <- read.dta13(here("build/input/IBS_UML_cs.dta"))
    mills <- mills[mills$island_name == island,]
    #turn into an sf object.
    mills <- st_as_sf(mills,	coords	=	c("lon",	"lat"), crs=4326)
    # keep only the geometry, we do not need mills attributes here.
    mills <- st_geometry(mills)
    # set CRS and project
    mills_prj <- st_transform(mills, crs = indonesian_crs)
    #define big catchment areas to have a large AOI.
    mills_ca <- st_buffer(mills_prj, dist = 60000)
    # work with squares rather than with circles
    for(i in 1:length(mills_ca)){
      mills_ca[i] <- st_as_sfc(st_bbox(mills_ca[i]))
    }
    total_ca <- st_union(st_geometry(mills_ca))
    # coerce to a SpatialPolygon
    total_ca_sp <- as(total_ca, "Spatial")
    # keep mills_prj we need it below
    rm(total_ca, mills_ca, mills)
    
    # Mask
    parcels_brick <- brick(paste0("./bricked_parcels/parcels_",island,"_",parcel_size/1000,"km_",threshold,"th.tif"))
    mask(x = parcels_brick, mask = total_ca_sp,
         filename = paste0("./bricked_parcels/m_parcels_",island,"_",parcel_size/1000,"km_",threshold,"th.tif"),
         datatype = "INT4U",
         overwrite = TRUE)
    rm(parcels_brick, total_ca_sp)
    
    
    ## 2. Selecting parcels within a given distance to a mill at least one year
    # (i.e. the parcel is present in the dataframe in all years even if it is within say 50km of a mill only since 2014)
    
    # Turn the masked raster to a sf dataframe
    parcels_brick <- brick(paste0("./bricked_parcels/m_parcels_",island,"_",parcel_size/1000,"km_",threshold,"th.tif"))
    m.df_wide <- raster::as.data.frame(parcels_brick, na.rm = TRUE, xy = TRUE, centroids = TRUE)
    m.df_wide <- m.df_wide %>% dplyr::rename(lon = x, lat = y)
    m.df_wide <- st_as_sf(m.df_wide, coords = c("lon", "lat"), remove = FALSE, crs = indonesian_crs)
    
    # Remove here parcels that are not within the catchment area of a given size (defined by catchment radius)
    # coordinates of all mills (crs is indonesian crs, unit is meter)
    within <- st_is_within_distance(m.df_wide, mills_prj, dist = catchment_radius)
    m.df_wide <- m.df_wide %>% dplyr::filter(lengths(within) >0)
    m.df_wide <- m.df_wide %>% st_drop_geometry()
    
    rm(within, parcels_brick)
    
    
    ## 3. Reshaping to long format
    # make parcel id
    island_id <- if(island == "Sumatra"){1} else if(island == "Kalimantan"){2} else if (island == "Papua"){3}
    m.df_wide$parcel_id <- paste0(island_id, c(1:nrow(m.df_wide))) %>% as.numeric()
    # vector of the names in the wide format of our time varying variables
    varying_vars <- paste0("m_parcels_",island,"_",parcel_size/1000,"km_",threshold,"th.", seq(from = 1, to = 18))
    m.df <- stats::reshape(m.df_wide,
                           varying = varying_vars,
                           v.names = paste0("pixelcount_",threshold,"th"),
                           sep = ".",
                           timevar = "year",
                           idvar = c("parcel_id"), # don't put "lon" and "lat" in there, otherwise memory issue (see https://r.789695.n4.nabble.com/reshape-makes-R-run-out-of-memory-PR-14121-td955889.html)
                           ids = "parcel_id",
                           direction = "long",
                           new.row.names = seq(from = 1, to = nrow(m.df_wide)*length(years), by = 1))
    
    rm(varying_vars, m.df_wide)
    # replace the indices from the raster::as.data.frame with actual years.
    m.df <- mutate(m.df, year = years[year])
   
    m.df <- setorder(m.df, parcel_id, year)
    saveRDS(m.df,
            file = paste0("./dataframes/panel_",island,"_",parcel_size/1000,"km_",catchment_radius/1000,"CR_",threshold,"th.Rdata"))
  }

  ### Execute it
  threshold <- 30
  while(threshold < 100){
    threshold_raster_to_df(threshold)
    threshold <- threshold + 30
  }
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

##### EXECUTE FUNCTIONS AND MERGE THE OUTPUTS #####

#### Execute the functions ####
# Only if their outputs have not been already computed

### Prepare a 30m pixel map of lucfp for that Island
# Choose an island between "Sumatra", "Kalimantan" or "Papua"
Island <- "Papua"

if(!file.exists(paste0("./annual_maps/lucfp_",Island,"_90th_2018.tif"))){
  
  prepare_pixel_lucfp(Island)
}

### Aggregate this Island map to a chosen parcel size (3km, 6km and 9km for instance)
PS <- 3000

if(!file.exists(paste0("./bricked_parcels/parcels_",Island,"_",PS/1000,"km_90th.tif"))){
  
  aggregate_lucfp(island = Island,
                  parcel_size = PS)
}

### For that Island and for each aggregation factor, extract panels of parcels within different catchment area sizes 
# (radius of 10km, 30km and 50km)
CR <- 10000 # i.e. 10km radius

if(!file.exists(paste0("./dataframes/panel_",Island,"_",PS/1000,"km_",CR/1000,"CR_90th.Rdata"))){

  while(CR < 60000){

    to_panel_within_CR(island = Island,
                          parcel_size = PS,
                          catchment_radius = CR)
    CR <- CR + 20000
  }
}

#### Gather the lucfp variables for each parcel_size and catchment_radius combinations. ####
PS <- 3000  
CR <- 10000 # i.e. 10km radius

while(CR < 60000){
  
  # For each Island, join columns of lucfp variable for different forest thresholds. 
  df_list <- list()
  IslandS <- c("Sumatra", "Kalimantan", "Papua")
  for(Island in IslandS){

    df30 <- readRDS(paste0("./dataframes/panel_",Island,"_",PS/1000,"km_",CR/1000,"CR_30th.Rdata"))
    df60 <- readRDS(paste0("./dataframes/panel_",Island,"_",PS/1000,"km_",CR/1000,"CR_60th.Rdata"))
    df90 <- readRDS(paste0("./dataframes/panel_",Island,"_",PS/1000,"km_",CR/1000,"CR_90th.Rdata"))
    
    df60 <- dplyr::select(df60, -lon, -lat)
    df <- inner_join(df30, df60, by = c("parcel_id", "year"))
    df90 <- dplyr::select(df90, -lon, -lat)
    
    df_list[[match(Island, IslandS)]] <- inner_join(df, df90, by = c("parcel_id", "year"))
  }
  
  # stack the three Islands together
  indo_df <- rbind(df_list[[1]], df_list[[2]], df_list[[3]])
  
  saveRDS(indo_df, paste0("./dataframes/panel_Indonesia_",PS/1000,"km_",CR/1000,"CR.Rdata"))
  
  rm(indo_df, df_list)
  CR <- CR + 20000
}




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

##### NOTES #####
# Not necessarily relevant 

### ON EXTRACT_GFC
# extract télécharge les tiles qui couvrent notre AOI
# et ensuite pour ces tiles là il n'y a aucun NA, dans le résultat du extract, même
# pour les pixels en dehors de l'AOI. En revanche il y a des NAs
# dans la partie du raster couverte pas un tile qui ne couvre pas l'AOI.
# donc si on prend un bbox comme aoi on n'a pas de NA.

# to extract and project in the same time (AOI_sp should be projected) but did not work, I don't know why.
# extract_gfc(AOI_sp, data_folder, stack = "change", to_UTM = TRUE,
# dataset = "GFC-2017-v1.5", filename = "gfc_data_prj.tif", overwrite = TRUE)
# defo_e <- raster("gfc_data_prj.tif")
# crs(defo_e)

#   We don't use gfc_stats because we want to keep information at the pixel level and not at the aoi's in order 
#   to overlay it with plantations.




### GFC mask 
# we do not mask with water mask from gfc because there are many other reasons why 
# deforestation cannot occur (cities, mountains) and these are taken into account in FE 
# and the distributional effect of this on our outcome variabel will be captured 
# by the poisson model. 

# Note also that For Sumatra at least, the northern part of the most north CA is not covered 
# by Austin data. Those NAs are simply not converted to the dataframe. This is not an 
# issue because the unit of observation is not the CA (one of which would not be the same size)
# but the parcel. In other words we don't bother that some CA are not fully observed in our maps. 




### NAs
# The forest loss and aligned plantations maps don't have NAs
# But once overlaid, some NAs are produced at the margins (because they don't perfectly align)
# These NAs are not removed (trimmed) or reclassified. 
# They disappear when 
#lucfp30 <- raster("lucfp_30th.tif")
# clusterR(lucfp30, 
#          fun = reclassify, 
#          args = list(rcl = cbind(NA,0)),
#          filename = "lucfp_30th.tif",
#          datatype = "INT1U",
#          overwrite = TRUE )




#  brick after a normal aggregation (no foreach)
# # read in the 18 layers and brick them
# layers_paths <- list.files(path = "./annual_parcels", pattern = paste0("parcels_",PS/1000,"km_",threshold, "th_"), full.names = TRUE) %>% as.list()
#   #layers_paths <- list.files(path = "./annual_maps", pattern = paste0("defo_",threshold, "th_"), full.names = TRUE) %>% as.list()
#   parcels_brick <- layers_paths %>% brick()
#   
#   #write the brick
#   writeRaster(parcels_brick, 
#               filename = paste0("./bricked_parcels/parcels_",PS/1000,"km_",threshold,"th.tif"), 
#               overwrite = TRUE)
#   rm(parcels_brick)



### rationale if aggregation is with mean: 
# Since in each annual map, original pixels are either 1 or 0 valued (conversion from forest to op plantation remotely sensed 
# for that pixel that year or not) and each pixel is the same area, the mean value of a group of pixels is 
# the ratio of the area deforested over the parcel area. This is refered to as the percentage of deforestation. 



### MASK ? is not useful because reclassifying NAs to 0s does not make the file lighter (NA is stored in a large value 
# see NAvalue() and ?datatype)

