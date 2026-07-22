source("R/load.R")
load_coastal_packages(c(coastal_packages, "DBI"))
con <- connect_coastal_database()

on.exit({
  if (exists("con", inherits = TRUE) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
}, add = TRUE)

validate_coastal_activities(connection = con, check_db = TRUE)
validate_ferries()

coastal_data <- load_coastal_data(
  con,
  validate_db = TRUE,
  include_images = FALSE,
  include_position_extremities = FALSE,
  export_rider_traces = FALSE
)

coastal_streams <- coastal_data$coastal_streams
rides_index <- coastal_data$rides_index

if (nrow(coastal_streams) == 0) {
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
  dplyr::filter(is.na(overall_distance_miles))

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
message(stringr::str_glue("Stream rows after cropping: {nrow(coastal_streams)}"))
message(stringr::str_glue("Ride summary rows: {nrow(rides_index)}"))

if (exists("con", inherits = TRUE) && DBI::dbIsValid(con)) {
  DBI::dbDisconnect(con)
}
