# ride explorer
# use this script to add new rides to the coastal dataset

source("R/load.R")
load_coastal_packages()
con <- connect_coastal_database()


# Load ride ---------------------------------------------------------------

ride_id <- 15562928333

ride_data <- load_activity_stream(ride_id, con)

range(ride_data$sample_index)


# Draw track, and adjust --------------------------------------------------

# Adjust the sample-index filter to crop the ride to the correct section. Add
# the inclusive start and end sample indices to data/coastal_activities.R.
# Keep time_seconds visible as contextual information only.

leaflet() %>%
  addTiles() %>%
  add_track(
    ride_data %>% filter(sample_index >= 1000, sample_index <= 1100),
    track_colour = "blue"
  )

ride_data %>%
  filter(sample_index >= 1000, sample_index <= 1100) %>%
  select(sample_index, time_seconds, latitude, longitude)
