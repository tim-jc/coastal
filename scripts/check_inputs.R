library(tidyverse)
library(DBI)
library(lubridate)

source("config.R")
source("R/load.R")

validate_coastal_activities(connection = con, check_db = TRUE)
validate_ferries()

coastal_data <- load_coastal_data(
  validate_db = TRUE,
  include_images = FALSE,
  include_position_extremities = FALSE,
  export_rider_traces = FALSE
)

full_dataset <- coastal_data$full_dataset
rides_index <- coastal_data$rides_index

if (nrow(full_dataset) == 0) {
  stop("No coastal stream rows loaded.", call. = FALSE)
}

if (nrow(rides_index) != nrow(coastal_activities)) {
  stop(
    stringr::str_glue(
      "Expected {nrow(coastal_activities)} ride summary rows, found {nrow(rides_index)}."
    ),
    call. = FALSE
  )
}

missing_distance <- rides_index %>%
  dplyr::filter(is.na(distance_whole_ride_miles))

if (nrow(missing_distance) > 0) {
  stop(
    stringr::str_glue(
      "Missing whole-ride distance for {nrow(missing_distance)} ride summary rows."
    ),
    call. = FALSE
  )
}

message("Input check passed.")
message(stringr::str_glue("Activities: {dplyr::n_distinct(coastal_activities$activity_id)}"))
message(stringr::str_glue("Ride segments: {nrow(coastal_activities)}"))
message(stringr::str_glue("Stream rows after cropping: {nrow(full_dataset)}"))
message(stringr::str_glue("Ride summary rows: {nrow(rides_index)}"))
