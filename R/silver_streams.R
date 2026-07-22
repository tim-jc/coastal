select_stream_columns <- function(streams) {
  streams %>%
    dplyr::select(
      activity_id,
      sample_index,
      time_seconds,
      distance = distance_metres,
      latitude,
      longitude,
      lat = latitude,
      lng = longitude,
      altitude = altitude_metres,
      velocity_smooth = velocity_smooth_metres_per_second,
      heartrate = heartrate_bpm,
      cadence = cadence_rpm,
      watts,
      temp = temperature_celsius,
      moving = is_moving,
      grade_smooth = grade_smooth_percent
    )
}

load_activity_stream <- function(ride_id, connection) {
  silver_tbl("activity_streams", connection) %>%
    dplyr::filter(activity_id == !!bit64::as.integer64(ride_id)) %>%
    select_stream_columns() %>%
    dplyr::collect()
}

load_stream_records <- function(connection, activity_ids) {
  silver_tbl("activity_streams", connection) %>%
    dplyr::filter(activity_id %in% activity_ids) %>%
    select_stream_columns() %>%
    dplyr::collect()
}

load_activity_records <- function(connection, activity_ids) {
  silver_tbl("activities", connection) %>%
    dplyr::select(
      activity_id,
      start_date_local = start_datetime_local,
      distance_metres
    ) %>%
    dplyr::filter(activity_id %in% activity_ids) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      strava_link = paste0("https://www.strava.com/activities/", activity_id)
    )
}

build_coastal_rides <- function(stream_records, activities = coastal_activities) {
  stream_records %>%
    dplyr::inner_join(
      activities,
      by = "activity_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(time_seconds >= ride_start_time, time_seconds <= ride_end_time)
}

build_activity_rides <- function(stream_records, activities = coastal_activities) {
  activity_metadata <- activities %>%
    dplyr::distinct(activity_id, .keep_all = TRUE)

  stream_records %>%
    dplyr::inner_join(
      activity_metadata,
      by = "activity_id"
    )
}

get_coastal_rides <- function(connection, activities = coastal_activities) {
  activity_ids <- activities$activity_id
  load_stream_records(connection, activity_ids) %>%
    build_coastal_rides(activities)
}

get_activity_rides <- function(connection, activities = coastal_activities) {
  activity_ids <- activities$activity_id
  load_stream_records(connection, activity_ids) %>%
    build_activity_rides(activities)
}

new_silver_data_source <- function(connection) {
  list(
    load_activities = function(activity_ids) {
      load_activity_records(connection, activity_ids)
    },
    load_streams = function(activity_ids) {
      load_stream_records(connection, activity_ids)
    }
  )
}
