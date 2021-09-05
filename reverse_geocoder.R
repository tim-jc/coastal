# reverse geocoder
# use this script to add location data to the csv files
# bit of a nightmare because the reverse geocode crashes after about 900 calls
# so need to write out to csv so data isn't lost
# Restarting R when the counter hits 800 to prevent a force quit

# libraries
library(tidyverse)
library(magrittr)
library(leaflet)

source("coastal_vis_functions.R")

# bing api key - these refresh annually. If one isn't working, try the other! Otherwise the photon API is unlimited, but a bit less detailed.
hca_account_key <- "AmmKMFzPTOsRpX1M8orYkCmkTA_A-0q9UB980GE16xIY0mQ0UYYmX_IX49Hs_UqQ"
gmail_account_key <- "AvF_1ohpw9KtMUOazP0qqIY6-1MVT8YWyUQqQktYT-oPml7J-mjzL4mTyhedkIZQ" 

to_code <- read_csv("csv/locations_to_map.csv") %>% 
  select(lat, lon) %>% 
  distinct() 
  
coded <- read_csv("csv/reverse_geocoding.csv") %>% 
  distinct() %>% 
  mutate(has_postcode = str_detect(location_string, "[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}")) %>% 
  filter(has_postcode) %>% 
  select(-has_postcode)

write_csv(coded, "csv/reverse_geocoding.csv")

to_code %<>%
  anti_join(coded) 

write_csv(to_code, "csv/locations_to_map.csv")

# view points to be coded
leaflet() %>% 
  addTiles() %>% 
  add_track(to_code)

# Shuffle the dataset order to ensure it doesn't always attempt to code uncodeable locations first
set.seed(82)
rows <- sample(nrow(to_code))
to_code <- to_code[rows,] %>% 
  mutate(id = seq(1,nrow(.),1))

for (j in 1:nrow(to_code)) {
    
    lat <- to_code$lat[to_code$id == j]
    lon <- to_code$lon[to_code$id == j]
    
    # location_str <-  revgeo::revgeo(lon, lat)
    location_str <-  revgeo::revgeo(lon, lat, provider = "bing", API = hca_account_key)
    
    print(paste(j, location_str))
    
    out <- data.frame(lon, lat, location_str)
    
    write_csv(out, "csv/reverse_geocoding.csv", append = T)
    
    if(j > 800) {
      .rs.restartR()
    }
    
}

