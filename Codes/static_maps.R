# Create static maps of Norfolk Island sites
rm(list = ls()) # remove everything stored in environment

# load libraries
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggrepel)
library(grid)

# set google api
source("C:/Users/Jeremy/Box Sync/R_functions/Google_maps_API_token.R")

# load data
surveys <- read.csv("Data/Norfolk_Island_2020_March_GPS_coordinates_coral_surveys.csv", head=T, stringsAsFactors=F) 
loggers <- read.csv("Data/Norfolk_Island_2020_March_GPS_coordinates_instruments_for_mapping.csv", head=T, stringsAsFactors=F)
sampling <- read.csv("Data/Norfolk_Island_2020_March_GPS_coordinates_water_and_sediment_sampling.csv", head=T, stringsAsFactors=F)
outflows <- read.csv("Data/Norfolk_Island_2020_March_GPS_coordinates_outflow_sites.csv", head=T, stringsAsFactors=F)

# subset survey data to sites already done
surveys_completed <- subset(surveys, !is.na(Bearing))

# get basemap 
NI_basemap <- get_googlemap(center = c(167.9598, -29.06026), maptype = "satellite"
                            , source = "google", zoom = 15
                            , style='feature:all|element:labels|visibility:off')

# map coral health transects -----------------------------------------------
ggmap(NI_basemap) +
  scale_y_continuous(limits = c(-29.063, -29.0575)) +
  scale_x_continuous(limits = c(167.9542, 167.964)) +
  geom_point(data = surveys_completed, mapping = aes(x = Longitude, y = Latitude), colour = 'white') +
  geom_label_repel(aes(x = Longitude, y = Latitude, label = GPS_Name)
                   , min.segment.length = 0.001
                   , nudge_x = 0.0005, nudge_y = 0.0005
                   , data = surveys_completed, segment.color = 'white') + 
  ylab("Latitude") +
  xlab("Longitude") +
  geom_text(aes(x = 167.9571, y = -29.0625, label = 'Coral health transects'), colour = I("white"), fontface='bold', size = 8)

# map loggers -------------------------------------------------------
ggmap(NI_basemap) +
  scale_y_continuous(limits = c(-29.063, -29.0575)) +
  scale_x_continuous(limits = c(167.9542, 167.964)) +
  geom_point(data = loggers, mapping = aes(x = Longitude, y = Latitude
                                           , shape=Instrument, size = 1.5)
             , colour = 'white') +
  geom_label_repel(aes(x = Longitude, y = Latitude, label = Site)
                   , min.segment.length = 0.001
                   , nudge_x = 0.0005, nudge_y = 0.0005
                   , data = loggers, segment.color = 'white', fill='white') +
  ylab("Latitude") +
  xlab("Longitude") +
  geom_text(aes(x = 167.956, y = -29.0625, label = 'Loggers'), colour = I("white"), fontface='bold', size = 8) +
  theme_nothing(legend=TRUE) + theme(legend.position = "left")
