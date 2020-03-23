# Create interactive maps of Norfolk Island sites
rm(list = ls()) # remove everything stored in environment

# load libraries
library(leaflet)

# load data
loggers <- read.csv("Data/Norfolk_Island_2020_March_GPS_coordinates_instruments.csv", head=T, stringsAsFactors=F)
surveys <- read.csv("Data/Norfolk_Island_2020_March_GPS_coordinates_coral_surveys.csv", head=T, stringsAsFactors=F) 

# subset survey data to sites already done
# surveys_completed <- surveys[grep("EB01|EB02|EB03|SB01|SB03|SB04|SB05|SB06|SB08|SB09", surveys$GPS_Name ),]
surveys_completed <- subset(surveys, !is.na(Bearing))

# subset logger data to unique pointns
TG <- subset(loggers, Instrument == "Tide_gauge")
FM <- subset(loggers, Instrument == "Flow_meter" | Instrument == "Tide_gauge" & !is.na(Site))
TL <- subset(loggers, Instrument == "Temperature_logger" & !is.na(Site))

# Add scale bar that adjusts with zoom function
addScaleBar = function(map,
                       position = c('topright', 'bottomright', 'bottomleft', 'topleft'),
                       options = scaleBarOptions()) {
  
  options = c(options, list(position = match.arg(position)))
  invokeMethod(map, getMapData(map), 'addScaleBar', options)
}

scaleBarOptions = function(maxWidth = 100, metric = TRUE, imperial = TRUE,
                           updateWhenIdle = TRUE) {
  list(maxWidth=maxWidth, metric=metric, imperial=imperial,
       updateWhenIdle=updateWhenIdle)
}

# survey map
survey.Map <- leaflet() %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=surveys_completed, lat = ~Latitude, lng = ~Longitude, col='red') # , radius=0.8
addScaleBar(survey.Map)

# tide gauge map
TG.Map <- leaflet() %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=TG, lat = ~Latitude, lng = ~Longitude, col='orange')
addScaleBar(TG.Map)

# flow meter map
FM.Map <- leaflet() %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=FM, lat = ~Latitude, lng = ~Longitude, col='lightblue')
addScaleBar(FM.Map)

# temperature logger map
TL.Map <- leaflet() %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=TL, lat = ~Latitude, lng = ~Longitude, col='yellow')
addScaleBar(TL.Map)
