load_gps_data <- function(connection = con, activities = coastal_activities) {
  activity_ids <- activities$activity_id

  activity_list <- silver_tbl("activities", connection) |>
    dplyr::mutate(
      strava_link = paste0("https://www.strava.com/activities/", activity_id)
    ) |>
    dplyr::select(
      activity_id,
      start_date_local = start_datetime_local,
      strava_link
    ) |>
    dplyr::filter(activity_id %in% activity_ids) |>
    dplyr::collect()

  ride_streams <- get_coastal_rides(connection, activities) |>
    dplyr::inner_join(activity_list, by = "activity_id") |>
    dplyr::mutate(
      sort_time = if_else(ride_direction == "cw", -time_seconds, time_seconds),
      ride_name = factor(ride_name, levels = activities$ride_name, ordered = T),
      start_date_local = as.POSIXct(start_date_local),
      time_of_day = start_date_local + seconds(time_seconds),
      yr = lubridate::year(start_date_local),
      marker_popup = str_c(
        ride_name,
        "<br>",
        format(start_date_local, "%d-%b-%y"),
        "<br>",
        riders_pretty,
        "<br>",
        "<a href=",
        strava_link,
        " target=\"_blank\">Strava"
      )
    ) |>
    arrange(ride_name, sort_time)

  ride_streams <- ride_streams |>
    dplyr::group_by(activity_id) |>
    dplyr::mutate(
      prev_lng = lag(lng),
      prev_lat = lag(lat),
      prev_alt = lag(altitude),
      dist_since_prev = geosphere::distHaversine(
        cbind(prev_lng, prev_lat),
        cbind(lng, lat)
      ),
      climb_since_prev = if_else(altitude > prev_alt, altitude - prev_alt, 0)
    ) |>
    ungroup()

  return(ride_streams)
}

create_summary <- function(
  full_dataset,
  connection = con,
  activity_ids = coastal_ids
) {
  df <- full_dataset |>
    dplyr::group_by(
      ride_name,
      start_date_local,
      yr,
      riders,
      activity_id,
      from,
      to,
      strava_link,
      ride_direction,
      marker_popup
    ) |>
    dplyr::summarise(
      start_time = min(time_seconds),
      finish_time = max(time_seconds),
      start_lon = lng[which.min(time_seconds)],
      start_lat = lat[which.min(time_seconds)],
      distance_miles = sum(dist_since_prev, na.rm = T) * metres_to_miles,
      elevation_metres = sum(climb_since_prev, na.rm = T),
      dist_per_elev = distance_miles / elevation_metres,
      .groups = "drop"
    ) |>
    distinct() |>
    arrange(start_date_local, start_time) |>
    dplyr::mutate(
      start_date = as.Date(start_date_local),
      is_latest_ride = start_date == max(start_date),
      ride_prev_day = start_date == lag(start_date) |
        start_date == lag(start_date) + days(1),
      is_new_adventure = case_when(!ride_prev_day | is.na(ride_prev_day) ~ T)
    ) |>
    replace_na(list(is_new_adventure = F)) |>
    dplyr::mutate(
      adventure_id = cumsum(is_new_adventure),
      is_latest_adventure = adventure_id == max(adventure_id[is_latest_ride])
    ) |>
    dplyr::select(-c(ride_prev_day, is_new_adventure))

  activity_data <- silver_tbl("activities", connection) |>
    dplyr::mutate(
      distance_whole_ride_miles = distance_metres * metres_to_miles
    ) |>
    dplyr::select(activity_id, distance_whole_ride_miles) |>
    dplyr::filter(activity_id %in% activity_ids) |>
    dplyr::collect()

  df <- df |>
    dplyr::left_join(activity_data, by = "activity_id") |>
    dplyr::mutate(
      coastal_percentage = distance_miles / distance_whole_ride_miles,
      coastal_percentage = if_else(
        coastal_percentage > 1,
        1,
        coastal_percentage
      )
    )

  return(df)
}

load_coastal_data <- function(
  connection = con,
  activities = coastal_activities,
  ferries_data = ferries,
  validate_db = FALSE,
  include_images = TRUE,
  include_position_extremities = TRUE,
  export_rider_traces = FALSE
) {
  validate_coastal_activities(
    activities,
    connection = connection,
    check_db = validate_db
  )
  validate_ferries(ferries_data, activities)

  full_dataset <- load_gps_data(connection, activities)
  rides_index <- create_summary(
    full_dataset,
    connection,
    activities$activity_id
  )
  rider_ids <- extract_riders(activities)
  image_metadata <- if (include_images) get_image_metadata() else NULL
  position_extremities <- if (include_position_extremities) {
    get_position_extremities(full_dataset)
  } else {
    NULL
  }

  if (export_rider_traces) {
    purrr::walk(rider_ids, ~ export_rider_maps(.x, full_dataset))
  }

  list(
    full_dataset = full_dataset,
    rides_index = rides_index,
    image_metadata = image_metadata,
    position_extremities = position_extremities,
    riders = rider_ids,
    activities = activities,
    ferries = ferries_data
  )
}
