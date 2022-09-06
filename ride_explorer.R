# ride explorer
# use this script to add new rides to the coastal dataset

# libraries
library(tidyverse)
library(leaflet)

# clear the memory
rm(list=ls(all = TRUE))

source("coastal_vis_functions.R")


# Load ride ---------------------------------------------------------------

ride_id <- 3057187825

ride_data <- tbl(con, "ride_streams") %>% filter(id == ride_id) %>% collect()

max(ride_data$time)


# Draw track, and adjust --------------------------------------------------

# Adjust time filter to crop ride to correct size. Add start and end time
# values to coastal_activities in functions script

leaflet() %>% 
  addTiles() %>% 
  add_track(ride_data %>% filter(time >= 0,
                                 time <= 60000),
            track_colour = "blue")

