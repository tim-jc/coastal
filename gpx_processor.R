# gpx_processor
# use this script to review and crop raw.gpx files and export as .csv

# libraries
library(tidyverse)
library(leaflet)
library(magrittr)
library(revgeo)

# clear the memory
rm(list=ls(all = TRUE))

# source functions
source("coastal_vis_functions.R")

# 1 - load .gpx file ------------------------------------------------------

input_gpx <- gpx_to_df("gps_data/battlesbridge_rochester.gpx")

# 2 - review with leaflet -------------------------------------------------

leaflet() %>% 
  addTiles() %>% 
  add_track(input_gpx, track_colour = "red") # %>% 
  # add_track(read_csv("gps_data/csv_files/boston_hunstaton.csv")) # add existing track from another ride to compare if needed

# Start / end time

input_gpx$time %>% min()
input_gpx$time %>% max()


# 3 - adjust data ---------------------------------------------------------

# reverse geocode
# crop data by filtering on time field if needed
# add direction (cw = clockwise, acw = anti-clockwise)
# add ride_string in starttown_finishtown format
# add riders in initials|initials format (e.g. TC|SB|DA)

ride_string <- "battlesbridge_rochester"
ride_direction <- "cw"
strava_link_string <- "https://www.strava.com/activities/5906287061"
riders_string <- "TC|SB"

output_gpx <- input_gpx %>%
  filter(time > "2021-09-04T07:22:55Z" & time < "2021-09-04T19:15:46Z") %>%
  mutate(direction = ride_direction,
         ride = ride_string,
         riders = riders_string, 
         strava_link = strava_link_string)

# checked cropped ride
leaflet() %>% 
  addTiles() %>% 
  add_track(output_gpx, track_colour = "red")


# 4 - export csv ----------------------------------------------------------

write.csv(output_gpx, str_c("gps_data/csv_files/",ride_string,".csv"), row.names = F)
