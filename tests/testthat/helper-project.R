source(file.path("..", "..", "R", "load.R"))
load_coastal_packages()

make_test_activities <- function() {
  dplyr::tibble(
    activity_id = bit64::as.integer64(1),
    from = "Start",
    to = "Finish",
    ride_direction = "cw",
    riders = "TC|SB",
    ride_start_time = 0,
    ride_end_time = 20,
    ride_name = "Start -> Finish",
    riders_pretty = "TC, SB",
    coastal_segment_id = 1L
  )
}

make_test_streams <- function() {
  dplyr::tibble(
    activity_id = bit64::as.integer64(rep(1, 3)),
    sample_index = 1:3,
    time_seconds = c(0, 10, 20),
    distance = c(0, 100, 200),
    latitude = c(51, 51.001, 51.002),
    longitude = c(-1, -1.001, -1.002),
    lat = latitude,
    lng = longitude,
    altitude = c(10, 15, 13),
    velocity_smooth = 5,
    heartrate = 120,
    cadence = 80,
    watts = 150,
    temp = 15,
    moving = TRUE,
    grade_smooth = 0
  )
}

make_test_activity_records <- function() {
  dplyr::tibble(
    activity_id = bit64::as.integer64(1),
    start_date_local = as.POSIXct("2026-01-01 09:00:00", tz = "UTC"),
    distance_metres = 200,
    strava_link = "https://www.strava.com/activities/1"
  )
}
