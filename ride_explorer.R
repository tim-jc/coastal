# ride explorer
# use this script to add new rides to the coastal dataset

# libraries
library(tidyverse)
library(leaflet)
library(stravR)

# clear the memory
rm(list=ls(all = TRUE))

source("coastal_vis_functions.R")
source("config.R")


# Load ride ---------------------------------------------------------------

ride_id <- 15709407056

ride_data <- tbl(con, "streams") %>% filter(strava_id == ride_id) %>% collect()

max(ride_data$time)


# Draw track, and adjust --------------------------------------------------

# Adjust time filter to crop ride to correct size. Add start and end time
# values to coastal_activities in functions script

leaflet() %>% 
  addTiles() %>% 
  add_track(ride_data %>% filter(time >= 2310,
                                 time <= 8000),
            track_colour = "blue")

