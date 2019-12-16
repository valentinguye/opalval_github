
rm(list = ls())
# List all packages needed for session"foreign",
neededPackages = c("dplyr", "readxl", "sf", "rgdal", "foreign", "leaflet", "htmltools") 

allPackages    = c(neededPackages %in% installed.packages()[ , "Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
  missingIDX = which(allPackages == FALSE)
  needed     = neededPackages[missingIDX]
  lapply(needed, install.packages)
}

# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)



setwd("./build/input/mill_geolocalization")


## read in data 
# Universal Mill List
uml <- read_excel("traseMills_capEstyear.xlsx") 
# IBS observations that report a village in which none of the referenced mills are. 
ibs_unref <- readRDS("ibs_unref.Rdata")

## prepare uml dataset
uml <- select(uml, trase_code, parent_co, mill_name, est_year, latitude, longitude)
uml$latitude <- as.numeric(uml$latitude)
uml$longitude <- as.numeric(uml$longitude)
uml$lat <- uml$latitude
uml$lon <- uml$longitude
uml <- st_as_sf(uml,	coords	=	c("longitude",	"latitude"), crs = 4326)
uml <- st_transform(uml, crs = 4326)

## Identify intersecting villages. 
st_crs(ibs_unref) <- indonesian_crs 
ibs_unref <- st_transform(ibs_unref, crs = indonesian_crs)
# let's only look at those who intersect rather than those who are really equal. 
ibs_unref <- arrange(ibs_unref, firm_id)
ibs_unref$grp_int = sapply(st_intersects(ibs_unref), max)
ibs_unref_int <- ibs_unref[(duplicated(ibs_unref$grp_int) | duplicated(ibs_unref$grp_int, fromLast = TRUE)), ]
#re project to geodesic for maping
st_crs(ibs_unref) <- 4326 
ibs_unref <- st_transform(ibs_unref, crs = 4326)
st_crs(ibs_unref_int) <- 4326 
ibs_unref_int <- st_transform(ibs_unref_int, crs = 4326)


## Map building

# info you want the map to display
uml$popup <-paste("trase_code: ", uml$trase_code, "<br/>",
                       "Mill name: ", uml$mill_name, "<br/>",
                       "Parent: ",  uml$parent_co, "<br/>",
                       uml$lat, uml$lon
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

# MAP
ibs_unref%>% 
  leaflet() %>% 
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldImagery, group ="ESRI") %>%
  #addPolygons(opacity = 0.5, color = "red", weight = 2, fill = FALSE, popup = ~ibs_unref$popup)%>%
  addAwesomeMarkers(data = uml, icon = icons, popup = ~uml$popup)

