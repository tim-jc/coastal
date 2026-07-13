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

load_activity_stream <- function(ride_id, connection = con) {
  silver_tbl("activity_streams", connection) %>%
    dplyr::filter(activity_id == !!bit64::as.integer64(ride_id)) %>%
    select_stream_columns() %>%
    dplyr::collect()
}

get_coastal_rides <- function(connection = con, activities = coastal_activities) {
  activity_ids <- activities$activity_id

  ride_streams <- silver_tbl("activity_streams", connection) %>%
    dplyr::filter(activity_id %in% activity_ids) %>%
    select_stream_columns() %>%
    dplyr::collect()

  ride_streams %>%
    dplyr::inner_join(
      activities,
      by = "activity_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(time_seconds >= ride_start_time, time_seconds <= ride_end_time)
}

get_activity_rides <- function(connection = con, activities = coastal_activities) {
  activity_ids <- activities$activity_id
  activity_metadata <- activities %>%
    dplyr::distinct(activity_id, .keep_all = TRUE)

  silver_tbl("activity_streams", connection) %>%
    dplyr::filter(activity_id %in% activity_ids) %>%
    select_stream_columns() %>%
    dplyr::collect() %>%
    dplyr::inner_join(
      activity_metadata,
      by = "activity_id"
    )
}
