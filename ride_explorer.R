# ride explorer
# use this script to add new rides to the coastal dataset

source("R/load.R")
load_coastal_packages()
con <- connect_coastal_database()


# Load ride ---------------------------------------------------------------

ride_id <- 19088138409

ride_data <- load_activity_stream(ride_id, con) %>%
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
