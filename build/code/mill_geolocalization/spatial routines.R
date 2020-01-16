
rm(list = ls())
# PACKAGES
neededPackages = c("leaflet", "htmltools", "readxl", "dplyr", "tidyverse", "raster", "sf", "foreign", "sp", "rnaturalearth", "data.table",
                   "rgdal", "leaflet", "readstata13", "ggspatial",
                   "rlist", "velox", "parallel", "foreach", "iterators", "doParallel", "xlsx") # + lwgeom
allPackages    = c(neededPackages %in% installed.packages()[ , "Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
  missingIDX = which(allPackages == FALSE)
  needed     = neededPackages[missingIDX]
  lapply(needed, install.packages)
}

# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)


library(leaflet)
library(htmltools)


setwd("C:/Users/GUYE/Desktop/opalval/build/input/mill_geolocalization")



indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

### Edition of pictures of deforestation and pictures of price spatial heterogeneity, for one cross-section, say 2010
ibs <- read.dta13("C:/Users/GUYE/Desktop/opalval/build/output/IBS_mills_final.dta")
ibs_geo <- ibs[!is.na(ibs$lat),]
nrow(unique(ibs_geo[,"firm_id"]))

ibs_geo$X <- ibs_geo$lon
ibs_geo$Y <- ibs_geo$lat
ibs_geo <- st_as_sf(ibs_geo, coords = c("lon", "lat"), crs = 4326)
ibs_geo_prj <- st_transform(ibs_geo, crs = indonesian_crs )

ibs_geo_prj <- st_buffer(ibs_geo_prj, dist = 20000)
plot(ibs_geo_prj[ibs_geo_prj$year==2013,"cpo_price_imp1"])

ibs_geo <- st_transform(ibs_geo_prj, crs = 4326)
plot(ibs_geo[ibs_geo$year==2013,"cpo_price_imp2"])

bins <- seq(from = 0, to = 300, by = 50)
pal <- colorBin("inferno", domain = ibs_geo$ffb_price_imp2, bins = bins)
ibs_geo[ibs_geo$year==2003,"ffb_price_imp2"]%>% 
  leaflet() %>% 
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldImagery, group ="ESRI") %>%
  addPolygons(opacity = 0.2, color = "white",
              fillOpacity = 0.8, fillColor = ~pal(ffb_price_imp2),  
              weight = 1, noClip = FALSE) %>% 
  addLegend(pal = pal, values = ~ffb_price_imp2, opacity = 0.7,
             title = "Mills' 20km catchment areas and <br/> their 2003 input mean unit values <br/> (2010 USD/ton CPO)", position = "topright")
            

max(ibs_geo$ffb_price_imp2, na.rm = TRUE)
RdYlBu
"viridis", "magma", "inferno", or "plasma".

fill = FALSE,

ibs_geo2010_prj$district_name == "Kab. Asahan"



sb <- list()
km <- 10
#listnames <- c(1:9)
while(km < 70){
  sb[[km/10]] <- st_buffer(ibs_geo_prj, dist = km*1000)
  #sb[[km/10]] <- st_transform(sb[[km/10]], crs = 4326)
  km <- km + 10
}


class(sb[[2]])
ggplot(data = sb[[2]]) + 
  geom_sf()+
  geom_text(data = sb[[2]], aes(x = X, y = Y, label = firm_id)) +
  coord_sf(xlim = c(112, 113), ylim = c(-3, -2.5), expand = FALSE)

plot(st_geometry(simple_buffers[[1]][1:3,]))
plot(st_geometry(ibs_20km[1:3,]))

ibs_20km <- st_buffer(ibs_prj, dist = 20000)
ibs_10km <- st_buffer(ibs_prj, dist = 10000)
ibs_20km <- st_transform(ibs_20km, crs = 4326)
ibs_10km <- st_transform(ibs_10km, crs = 4326)

plot(st_geometry(ibs_20km[1,]))












#################################
ibs <- read.dta13("IBS_mills_geolocalized.dta")
ibs$X <- ibs$lon
ibs$Y <- ibs$lat
ibs <- st_as_sf(ibs, coords = c("lon", "lat"), crs = 4326)
ibs_prj <- st_transform(ibs, crs = indonesian_crs )

ibst <- st_buffer(ibs_prj, dist = 500)

m <- st_intersects(ibst)
ibst <- st_transform(ibst, crs = 4326)
n <- m[lengths(m)>1]

ibs_unref <- st_read("ibs_unref")
ibs_unref <- st_transform(ibs_unref, crs = 4326)

# UML DATA
uml <- read_excel("traseMills_capEstyear.xlsx") 
uml <- dplyr::select(uml, trase_code, parent_co, mill_name, est_year, latitude, longitude)
uml$latitude <- as.numeric(uml$latitude)
uml$longitude <- as.numeric(uml$longitude)
uml$lat <- uml$latitude
uml$lon <- uml$longitude
uml <- st_as_sf(uml,	coords	=	c("longitude",	"latitude"), crs = 4326)
uml <- st_transform(uml, crs = 4326)

# LEAFLET ESRI WORLD IMAGERY REQUIRES GEODESIC COORDINATES.
# st_transform alone IS NOT SUFFICIENT, ONE NEEDS TO FIRST CHANGE SRC. 
st_crs(ibs_unref) <- 4326 
ibs_unref <- st_transform(ibs_unref, crs = 4326)
st_crs(heilmayr) <- 4326
heilmayr <- st_transform(heilmayr, crs = 4326)


#### MAP STUFF FOR UNREF CHECKS
rm(list = ls())
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

heilmayr <- read_excel("traseMills_capEstyear.xlsx")
ibs_unref <- readRDS("ibs_unref.Rdata")
# prepare heilmayr dataset
heilmayr <- select(heilmayr, trase_code, parent_co, mill_name, est_year, latitude, longitude)
heilmayr$latitude <- as.numeric(heilmayr$latitude)
heilmayr$longitude <- as.numeric(heilmayr$longitude)
heilmayr$lat <- heilmayr$latitude
heilmayr$lon <- heilmayr$longitude
heilmayr <- st_as_sf(heilmayr,	coords	=	c("longitude",	"latitude"), crs = 4326)
heilmayr <- st_transform(heilmayr, crs = 4326)



# if you wan't to see only those villages that are reported with the exact same shape by more than one IBS observation. 
# and project before 
st_crs(ibs_unref) <- indonesian_crs 
ibs_unref <- st_transform(ibs_unref, crs = indonesian_crs)
ibs_unref <- arrange(ibs_unref, firm_id)
#ibs_unref$grp_eq = sapply(st_equals(ibs_unref), max)
#ibs_unref <- arrange(ibs_unref, grp_eq, firm_id)
#ibs_unref_eq <- ibs_unref[(duplicated(ibs_unref$grp_eq) | duplicated(ibs_unref$grp_eq, fromLast = TRUE)), ]
#nrow(ibs_unref[!(duplicated(ibs_unref$grp_eq) | duplicated(ibs_unref$grp_eq, fromLast = TRUE)), ])

# let's only look at those who intersect rather than those who are really equal. 
ibs_unref$grp_int = sapply(st_intersects(ibs_unref), max)
ibs_unref_int <- ibs_unref[(duplicated(ibs_unref$grp_int) | duplicated(ibs_unref$grp_int, fromLast = TRUE)), ]


#re project to geodesic for maping
st_crs(ibs_unref) <- 4326 
ibs_unref <- st_transform(ibs_unref, crs = 4326)
#st_crs(ibs_unref_eq) <- 4326 
#ibs_unref_eq <- st_transform(ibs_unref_eq, crs = 4326)
st_crs(ibs_unref_int) <- 4326 
ibs_unref_int <- st_transform(ibs_unref_int, crs = 4326)

#ibs_unref_eq$popup <- paste(ibs_unref_eq$firm_id, "<br/>",
#                        "isic: ", ibs_unref_eq$industry_code, "<br/>", 
#                         ibs_unref_eq$grp_eq
#)

ibs_unref_int$popup <- paste(ibs_unref_int$firm_id, "<br/>",
                            "isic: ", ibs_unref_int$industry_code, "<br/>", 
                            ibs_unref_int$grp_int
)

ibs_unref$n_int <- duplicated(ibs_unref$grp_int)

ibs_unref_int[ibs_unref_int$grp_int == 379,]
plot(st_geometry(ibs_unref_int[ibs_unref_int$firm_id == 55743,]), add = T, col = "red")
st_equals_exact(ibs_unref_int[ibs_unref_int$firm_id == 55742,], ibs_unref_int[ibs_unref_int$firm_id == 55743,], sparse = F)

# info you want the map to display
heilmayr$popup <-paste("trase_code: ", heilmayr$trase_code, "<br/>",
                       "Mill name: ", heilmayr$mill_name, "<br/>",
                       "Parent: ",  heilmayr$parent_co, "<br/>",
                       heilmayr$lat, heilmayr$lon
)

ibs_unref$popup <- paste(ibs_unref$firm_id, "<br/>",
                        "isic: ", ibs_unref$industry_code, "<br/>",
                        "average ffb input (ton): ", round(ibs_unref$avg_in_ton_ffb_imp2), "<br/>",
                        "average cpo input (ton): ", round(ibs_unref$avg_in_tot_ton_cpo_imp2), "<br/>",
                        "average cpo output (ton): ", round(ibs_unref$avg_out_ton_cpo_imp2), "<br/>",
                        "most recent cpo output (ton)","(",ibs_unref$max_year, "): ", round(ibs_unref$last_out_ton_cpo_imp2), "<br/>",
                        "average cpo unit value (2010 USD/ton): ", round(ibs_unref$avg_cpo_price_imp2)
)
# pimp the markers a bit 
icons <- awesomeIcons(icon = "industry", library = "fa", markerColor = "lightgray"
)
  
  
ibs_unref%>% 
  leaflet() %>% 
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldImagery, group ="ESRI") %>%
  addPolygons(opacity = 0.5, color = "red", weight = 2, fill = FALSE, popup = ~ibs_unref$popup)%>%
  addAwesomeMarkers(data = heilmayr, icon = icons, popup = ~heilmayr$popup)
  


a <- paste("x","y","z")
b<-  string.break.line(a)
  
  
sb <- list()
km <- 10
#listnames <- c(1:9)
while(km < 70){
  sb[[km/10]] <- st_buffer(ibs_prj, dist = km*1000)
  #sb[[km/10]] <- st_transform(sb[[km/10]], crs = 4326)
  km <- km + 10
}

# CHECK THAT DEFO WAS COMPUTED FOR A LARGER AOI THAN THE BBOX OF MILLS. 
defo <- raster("C:/Users/guyv/ownCloud/opalval/build/input/deforestation/defo_prj.tif")
extent(defo)
extent(sb[[6]])

plot(extent(defo), col = "green")
plot(extent(sb[[6]]), col = "red", add = T)
st_bbox(sb[[6]])

defo <- crop(defo, sb[[2]][sb[[2]]$firm_id==70271,])

defo <- calc(defo, fun = function(x){2000+x})
  
plot(defo, col = topo.colors(n = 18, rev = T))
plot(st_geometry(sb[[2]]), add = TRUE)


class(sb[[2]])
ggplot(data = sb[[2]]) + 
  geom_sf()+
  geom_text(data = sb[[2]], aes(x = X, y = Y, label = firm_id)) +
    coord_sf(xlim = c(112, 113), ylim = c(-3, -2.5), expand = FALSE)

plot(st_geometry(simple_buffers[[1]][1:3,]))
plot(st_geometry(ibs_20km[1:3,]))

ibs_20km <- st_buffer(ibs_prj, dist = 20000)
ibs_10km <- st_buffer(ibs_prj, dist = 10000)
ibs_20km <- st_transform(ibs_20km, crs = 4326)
ibs_10km <- st_transform(ibs_10km, crs = 4326)

plot(st_geometry(ibs_20km[1,]))
plot(st_geometry(ibs_5km[1,]), add = T)


ibs[ibs$firm_id==71526,]




sb[[1]] %>% 
  leaflet() %>% 
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldImagery, group ="ESRI")%>%
  addPolygons(opacity = 0.5, color = "red", weight = 2, fill = FALSE)%>% 
  addMarkers(lng = 112.5096 , lat = -2.783615)

[ibs_20km$firm_id == 71526 & ibs_20km$year == 2015,]
options = leafletOptions(crs = epsg3857)

leaflet()%>%
plot(st_geometry(ibs_20km))

[ibs$firm_id==68247,]))

plot(st_geometry(ibs_desa))




  


ibs[ibs$firm_id==55727 & ibs$year == 2005, ibs$lat]
plot(st_geometry(ibs[ibs$firm_id==55727 & ibs$year == 2005,]), add = F)


m <- leaflet(ibs_desa) %>% addProviderTiles("Esri.WorldImagery")
class(m)