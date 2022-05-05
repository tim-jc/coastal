# Strava scraper
# use this script to add new rides to the coastal dataset

# libraries
library(tidyverse)
library(lubridate)
library(leaflet)
library(rStrava)
library(DBI)

# clear the memory
rm(list=ls(all = TRUE))

source("coastal_vis_functions.R")

# Define master table of activities. New activities to be added here, in geographical order
coastal_activities <- tribble(
  ~strava_id, ~from, ~to, ~ride_direction, ~riders, ~ride_start_time, ~ride_end_time,
  7081454837, "harrapool", "strathcarron", "cw", "TC|SB|WR", 0, 10442,
  7076591578, "acharacle", "harrapool", "cw", "TC|SB|WR", 0, 26453,
  7072272567, "oban", "acharacle", "cw", "TC|SB|WR", 0, 34095,
  7066002617, "tarbert", "oban", "cw", "TC|SB|WR", 0, 33371,
  7060605792, "whiting bay", "tarbert", "cw", "TC|SB|WR", 0, 33382,
  7055062883, "ardrossan", "whiting bay", "cw", "TC|SB|WR", 7920, 16788,
  # Carlisle -> Glasgow adventure here
  6193006840, "seascale", "carlisle", "cw", "TC|SB|WR", 0, 26200,
  6188924719, "lancaster", "seascale", "cw", "TC|SB|WR", 0, 42273,
  6184233328, "chester", "lancaster", "cw", "TC|SB|WR", 3959, 43692,
  # Wales adventure here
  5836688186, "washford", "bristol", "cw", "TC|SB|DA|TS|WR", 0, 27310,
  5831004889, "tintagel", "washford", "cw", "TC|SB|DA|TS|WR", 0, 48232,
  5824943588, "penzance", "tintagel", "cw", "TC|SB|DA|TS|WR", 0, 39186,
  5560406484, "penzance", "looe", "acw", "TC|SB|DA", 0, 51318,
  5564763338, "looe", "exmouth", "acw", "TC|SB|DA", 0, 44658,
  5575822827, "exmouth", "bournemouth", "acw", "TC|SB|DA", 0, 45082,
  # Bournmouth -> Bognor
  1250873973, "folkestone", "bognor", "cw", "TC|SB",  0, 41420,
  1250104735, "rochester", "folkestone", "cw", "TC|SB", 10425, 40297,
  5906287061, "battlesbridge", "rochester", "cw", "TC|SB", 475, 42125, 
  177822252, "maldon", "battlesbridge", "cw", "TC", 13000, 25175,
  31856491, "maldon", "clacton", "acw", "TC", 12280, 23817,
  33329545, "clacton", "manningtree", "acw", "TC", 4700, 18254,
  754726575, "woodbridge", "manningtree", "cw", "TC|AH",  0, 11150,
  1323036676, "orford", "woodbridge", "acw", "TC", 0, 10000,
  428214670, "snape", "orford", "cw", "TC", 2350, 4000,
  233045883, "southwold", "snape", "cw", "TC|SB", 7175, 20430,
  753905298, "hunstanton", "southwold", "cw", "TC|AH" , 0, 35020,
  870993393, "boston", "hunstanton", "cw", "TC", 16250, 28975,
  4049860168, "boston", "hull", "acw", "TC|SB|DA", 205, 33323,
  4055608848, "hull", "staithes", "acw", "TC|SB|DA", 0, 35523,
  4058882682, "staithes", "newcastle", "acw", "TC|SB|DA", 0, 24371
) %>% 
  mutate(ride_name = str_glue("{from} -> {to}"),
         strava_link = str_glue("https://www.strava.com/activities/{strava_id}"),
         strava_data = map(strava_id, ~get_activity(.x, stoken)),
         ride_start = map_chr(strava_data, "start_date"),
         ride_start = str_replace_all(ride_start, "T|Z", " ") %>% as.POSIXct(),
         start_date = as.Date(ride_start),
         marker_popup = str_c(ride_name, "<br>",
                              format(ride_start, "%d-%b-%y"), "<br>",
                              riders, "<br>",
                              "<a href=", strava_link, " target=\"_blank\">Strava")) %>% 
 select(-strava_data)

# Connect to SQLite DB, retrieve rides already loaded
con <- dbConnect(RSQLite::SQLite(), "coastal.db")
rides_loaded <- dbGetQuery(con, "SELECT DISTINCT strava_id FROM ride_streams")
rides_to_load <- coastal_activities %>% anti_join(rides_loaded) %>% pull(strava_id)

# Get stream data for all activities
coastal_activity_streams <- get_activity_list(stoken) %>% compile_activities() %>% filter(id %in% rides_to_load) %>% get_activity_streams(stoken)

# Create dataset to append to SQLite DB
data_to_append <- coastal_activities %>% 
  inner_join(coastal_activity_streams, by = c("strava_id" = "id")) %>% 
  filter(time >= ride_start_time,
         time <= ride_end_time) 

# Write dataset
dbWriteTable(con, "ride_streams", data_to_append, append = T)
