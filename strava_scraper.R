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

# Add new activities to master table in functions script before scraping strava data
coastal_activities <- coastal_activities %>% 
  mutate(strava_link = str_glue("https://www.strava.com/activities/{strava_id}"),
         strava_data = map(strava_id, ~get_activity(.x, stoken)),
         ride_start = map_chr(strava_data, "start_date"),
         ride_start = str_replace_all(ride_start, "T|Z", " ") %>% as.POSIXct(),
         start_date = as.Date(ride_start),
         riders_pretty = str_replace_all(riders, "\\|", ", "),
         marker_popup = str_c(ride_name, "<br>",
                              format(ride_start, "%d-%b-%y"), "<br>",
                              riders_pretty, "<br>",
                              "<a href=", strava_link, " target=\"_blank\">Strava")) %>% 
 select(-strava_data)

# Connect to SQLite DB, retrieve rides already loaded, export backup
con <- dbConnect(RSQLite::SQLite(), "coastal.db")
ride_streams <- dbReadTable(con, "ride_streams")
write_csv(ride_streams,
          str_glue("ride_streams_backup_{Sys.Date()}.csv"))

# Get stream data for all activities
coastal_activity_streams <- get_activity_list(stoken) %>% compile_activities() %>% filter(id %in% coastal_activities$strava_id) %>% get_activity_streams(stoken)

# Create dataset to load to SQLite DB
data_to_load <- coastal_activities %>% 
  inner_join(coastal_activity_streams, by = c("strava_id" = "id")) %>% 
  filter(time >= ride_start_time,
         time <= ride_end_time) 

# Write dataset
dbWriteTable(con, "ride_streams", data_to_load, overwrite = T)
