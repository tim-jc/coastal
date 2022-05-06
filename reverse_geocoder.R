# reverse geocoder
# use this script to add location data to the csv files
# bit of a nightmare because the reverse geocode crashes after a few calls
# so need to write out to SQLite DB so data isn't lost
# Restarting R when the counter hits 275 to prevent a force quit

# libraries
library(tidyverse)
library(magrittr)
library(leaflet)
library(DBI)

# clear the memory
rm(list=ls(all = TRUE))

source("coastal_vis_functions.R")

# bing api key - these refresh annually. If one isn't working, try the other! Otherwise the photon API is unlimited, but a bit less detailed.
hca_account_key <- "AmmKMFzPTOsRpX1M8orYkCmkTA_A-0q9UB980GE16xIY0mQ0UYYmX_IX49Hs_UqQ"
gmail_account_key <- "AvF_1ohpw9KtMUOazP0qqIY6-1MVT8YWyUQqQktYT-oPml7J-mjzL4mTyhedkIZQ" 

# Get set of coded lat / lon and streams data from SQLite DB
ride_streams <- dbReadTable(con, "ride_streams")
coded <- dbReadTable(con, "geocodes")

# Load set of uncoded lat / lon
to_code <- ride_streams %>% 
  left_join(coded) %>% 
  filter(is.na(location_string))

# view points to be coded
leaflet() %>% 
  addTiles() %>% 
  add_track(to_code, track_colour = "red")

# Shuffle the dataset order to ensure it doesn't always attempt to code uncodeable locations first
set.seed(82)
rows <- sample(nrow(to_code))
to_code <- to_code[rows,] %>%
  mutate(id = seq(1,nrow(.),1))

for (j in 1:nrow(to_code)) {
  
  lat <- to_code$lat[to_code$id == j]
  lng <- to_code$lng[to_code$id == j]
  
  # location_string <-  revgeo::revgeo(lng, lat)
  location_string <-  revgeo::revgeo(lng, lat, provider = "bing", API = hca_account_key) %>% unlist()
  
  print(paste(j, location_string))
  
  out <- tibble(lng, lat, location_string) %>% 
    mutate(has_postcode = str_detect(location_string, "[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}"))
  
  if(out$has_postcode) {
    out <- out %>% select(-has_postcode)
    dbWriteTable(con, "geocodes", out, append = T)
  }
  
  if(j > 200) {
    .rs.restartR()
  }
  
}
