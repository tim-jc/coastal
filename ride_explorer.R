# ride explorer
# use this script to add new rides to the coastal dataset

# libraries
library(tidyverse)
library(leaflet)
library(stravR)

# clear the memory
rm(list=ls(all = TRUE))

source("config.R")
source("R/load.R")


# Load ride ---------------------------------------------------------------

ride_id <- 19088138409

ride_data <- load_activity_stream(ride_id) %>%
  dplyr::rename(time = time_seconds)

max(ride_data$time)


# Draw track, and adjust --------------------------------------------------

# Adjust time filter to crop ride to correct size. Add start and end time
# values to data/coastal_activities.R

leaflet() %>% 
  addTiles() %>% 
  add_track(
    ride_data %>% filter(time >= 29643, time <= 29645),
    track_colour = "blue"
  )


ride_data %>% filter(time >= 27000) %>% pull(time) %>% min()
