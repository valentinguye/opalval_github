# OK : ALL RASTER ALGEBRAS SHOULD BE MADE WITH A FILENAME OPTION SPECIFIED SO THAT NO USE OF TEMP FILES IS NEEDED.
# because some times there is not enough memory in R for this and we get the warnings. 

############## code for extracting on individual CAs #########################################
#test avant de looper sur la première CA
#filter line 1: polygon of CA 1. 
aoi1 <- mill_ca40[1,]
#give the aoi the same (unprojected) crs than the tiles. 
aoi1_unprj <- st_transform(aoi1, crs = 4326)
#convert to a SpatialPolygon object for compatibility with download_tiles methods. 
aoi1_unprj_sp <- as(aoi1_unprj, "Spatial")
#extract and read 
gfc_aoi1 <- extract_gfc(aoi1_unprj_sp, data_folder, stack = "change", to_UTM = FALSE, dataset = "GFC-2017-v1.5", 
                        filename = "aoi1.tif", overwrite=TRUE)

#extract loss layer
gfc_aoi1_loss <- gfc_aoi1$lossyear

#repeat for aoi2
#filter line 2: polygon of CA 2. 
aoi2 <- mill_ca40[2,]
#give the aoi the same (unprojected) crs than the tiles. 
aoi2_unprj <- st_transform(aoi2, crs = 4326)
#convert to a SpatialPolygon object for compatibility with download_tiles methods. 
aoi2_unprj_sp <- as(aoi2_unprj, "Spatial")
#extract and read 
gfc_aoi2 <- extract_gfc(aoi2_unprj_sp, data_folder, stack = "change", to_UTM = FALSE, dataset = "GFC-2017-v1.5", 
                        filename = "aoi2.tif", overwrite=TRUE)

#extract loss layer
gfc_aoi2_loss <- gfc_aoi2$lossyear

#merge with precedent layers. 
m<-mosaic(gfc_aoi1_loss, gfc_aoi2_loss, fun = mean)
plot(m)
file.size("aoi1.tif")
object.size(gfc_aoi1)
object.size(m)
plot(gfc_aoi1_loss)
plot(gfc_aoi2_loss, add=TRUE)

plot(gfc_aoi2_loss) 




#equalize crs
aoi1_unprj <- st_transform(aoi1, crs= crs(gfc_aoi1))
plot(gfc_aoi1_loss)
plot(st_geometry(aoi1_unprj), add=TRUE)
crs(gfc_aoi1)
#the zone for which data are extracted is the bbox (square) of circle CA. 



### MASK ? is not useful because reclassifying NAs to 0s does not make the file lighter. 
aoi1 <- heilmayr_ca100[1,]
aoi1 <- st_geometry(aoi1)
aoi1 <- as(aoi1, "Spatial")
defo1 <- crop(defo, aoi1, filename = "defo1.tif", overwrite = TRUE)
rm(defo1)
defo1 <- raster("defo1.tif")

defo1_mask <- mask(defo1, mask =aoi1)
defo1_m <- mask(defo1, mask =aoi1)



object.size(defo1_m) == object.size(defo1_mask)

defo1_m[is.na(defo1_m[])] <- 0 

defo1_m[is.na(defo1_m)]
defo1_mask[is.na(defo1_mask)]
defo1_m == defo1_mask

defo1_m
defo1_mask
rm(defo1, defo1_m, defo1_m_0, defo1_mask)

plot(defo1_mask)
plot(aoi1, add = TRUE)

plot(defo1_m)

object.size(defo1)
object.size(defo1_m)
################################################################################################################################

# code for smaller area (2)
#define the smaller area
aoi1 <- heilmayr_ca100[1,]
st_crs(aoi1)
aoi1_unprj <- st_transform(aoi1, crs=crs(loss, asText=TRUE))
st_crs(aoi1_unprj)


#just mask
aoi1_unprj_sp <- as(aoi1_unprj, "Spatial")
loss_mask1 <- mask(po, mask = aoi1_unprj_sp)
plot(loss_mask1)

# just crop
aoi1_unprj_bbox <- c(99.860066, 100.690995, 1.633717, 2.473283)

loss_aoi1 <- crop(loss, y = aoi1_unprj_bbox)
loss_aoi1

po_aoi1 <- crop(po, y = aoi1_unprj_bbox)
po_aoi1

#velox po_aoi1
velox_po1 <- velox(x = po_aoi1)
plot(velox_po1)

#align it
po1_align <- projectRaster(from = po_aoi1, to = loss_aoi1, 
                           method = "ngb", 
                           filename = "po1_aligned_new_oilpalm_2015.tif") 


plot(loss_aoi1)
plot(st_geometry(aoi1), alpha = 0.8, add = TRUE)
plot(po_aoi1, alpha = 0.5, add=TRUE)


plot(st_geometry(aoi1))

loss_aoi1
po_aoi1




# project them 
loss_aoi1_prj <- projectRaster(from = loss_aoi1, 
                               crs = "+proj=tmerc +lat_0=0 +lon_0=139.5 +k=0.9999 +x_0=200000 +y_0=1500000 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", 
                               method = "ngb") #computation time : appx. 15 s. 


po_aoi1_prj <- projectRaster(from = po_aoi1, to = loss_aoi1_prj, 
                             method = "ngb" , 
                             filename = "po_aoi1_prj.tif") # computation time : 10 sec




# give them same resolution otherwise overlay does not work. Try after projection first. 
#define levels of disaggregation 
disagg_fact <- c(327/35.9, 325/35.7)
po_aoi1_prj <- disaggregate(po_aoi1_prj, disagg_fact, method = "")

loss_aoi1_prj
po_aoi1_prj
aoi1

# try mask
loss_aoi1_ca <- mask(loss_aoi1_prj, mask = aoi1)
po_aoi1_ca <- mask(po_aoi1_prj, mask = aoi1)
plot(loss_aoi1_ca)
plot(po_aoi1_ca, alpha = 0.5, add = TRUE)

f <- function(loss_aoi1_ca, po_aoi1_ca) {loss_aoi1_ca*po_aoi1_ca}
defo <- overlay(loss_aoi1_ca, po_aoi1_ca, fun = f)
plot(defo) 
# CA MAAAAAAAARCHE 





#### ANNUAL DEFORESTED AREA ####
# store the extracted information in a dataframe 
#heilmayr_desa_defo <- st_drop_geometry(heilmayr_desa_prj)
heilmayr_desa_defo <- readRDS("heilmayr_desa_defo.Rdata")
years <- c(1:17)

## EXTRACT ON SIMPLE BUFFERS
km <- 20 
tic()
while(km < 60){
  #Convert polygons of interest as SpatialPolygons. 
  simple_buffers_sp <- as(simple_buffers[[km/10]], "Spatial") 
  #Compute area of the CA 
  buffer_area <- as.numeric(st_area(simple_buffers[[km/10]][1])) # they all have the same so take the area of the first one.
  
  for(t in 1:length(years)){
    #read in the raster of year t
    annualrastername <- paste0("defo_", years[t],".tif")
    annual_defo <- raster(annualrastername)
    
    #column name 
    simple_buffer_percentage <- paste0("pct_defo_simplebuf_",km, "km_",t)
    
    
    #and size 
    heilmayr_desa_defo[simple_buffer_percentage] <- c(1:length(simple_buffers_sp))
    
    for(i in 1:length(simple_buffers_sp)){
      
      # crop it to the extent of CA i 
      single_annual_defo <- crop(annual_defo, extent(simple_buffers_sp[i,])) # this creates a tmpfile
      single_annual_defo <- velox(single_annual_defo) 
      
      #extract proportion of CA that is deforested. 
      heilmayr_desa_defo[i, simple_buffer_percentage] <- single_annual_defo$extract(simple_buffers_sp[i,], fun = function(x) mean(x, na.rm = TRUE))
      
      rm(single_annual_defo) 
      removeTmpFiles(h = 0)# removes the temporary file created by crop
    }
    
    #compute corresponding area. 
    simple_buffer_area <- paste0("area_defo_simplebuf_",km, "km_",t)
    heilmayr_desa_defo[simple_buffer_area] <- buffer_area*heilmayr_desa_defo[simple_buffer_percentage]
    
    rm(annual_defo, annualrastername)
  } 
  
  rm(buffer_area, simple_buffers_sp)
  
  km <- km + 10 
  
}
toc()