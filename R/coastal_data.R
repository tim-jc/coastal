enrich_activity_streams <- function(
  ride_streams,
  activity_records,
  activities = coastal_activities,
  coastal = FALSE
) {
  enriched <- ride_streams |>
    dplyr::inner_join(
      dplyr::select(activity_records, activity_id, start_date_local, strava_link),
      by = "activity_id"
    ) |>
    dplyr::mutate(
      sort_time = dplyr::if_else(ride_direction == "cw", -time_seconds, time_seconds),
      ride_name = factor(ride_name, levels = activities$ride_name, ordered = TRUE),
      start_date_local = as.POSIXct(start_date_local),
      time_of_day = start_date_local + lubridate::seconds(time_seconds),
      yr = lubridate::year(start_date_local),
      marker_popup = stringr::str_c(
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
    )

  if (coastal) {
    enriched <- enriched |>
      dplyr::arrange(ride_name, sort_time) |>
      dplyr::group_by(activity_id, coastal_segment_id)
  } else {
    enriched <- enriched |>
      dplyr::arrange(activity_id, time_seconds) |>
      dplyr::group_by(activity_id)
  }

  enriched |>
    dplyr::mutate(
      prev_lng = dplyr::lag(lng),
      prev_lat = dplyr::lag(lat),
      prev_alt = dplyr::lag(altitude),
      dist_since_prev = geosphere::distHaversine(
        cbind(prev_lng, prev_lat),
        cbind(lng, lat)
      ),
      climb_since_prev = dplyr::if_else(
        altitude > prev_alt,
        altitude - prev_alt,
        0
      )
    ) |>
    dplyr::ungroup()
}

load_coastal_streams <- function(connection, activities = coastal_activities) {
  activity_ids <- activities$activity_id
  activity_records <- load_activity_records(connection, activity_ids)
  stream_records <- load_stream_records(connection, activity_ids)

  stream_records |>
    build_coastal_rides(activities) |>
    enrich_activity_streams(activity_records, activities, coastal = TRUE)
}

load_activity_streams <- function(
  connection,
  activities = coastal_activities
) {
  activity_ids <- activities$activity_id
  activity_records <- load_activity_records(connection, activity_ids)
  stream_records <- load_stream_records(connection, activity_ids)

  stream_records |>
    build_activity_rides(activities) |>
    enrich_activity_streams(activity_records, activities)
}

create_summary <- function(
  coastal_streams,
  activity_streams,
  activity_records,
  activity_ids = coastal_ids
) {
  df <- coastal_streams |>
    dplyr::group_by(
      coastal_segment_id,
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
      coastal_distance_miles = sum(dist_since_prev, na.rm = T) * metres_to_miles,
      coastal_elevation_metres = sum(climb_since_prev, na.rm = T),
      dist_per_elev = coastal_distance_miles / coastal_elevation_metres,
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

  activity_distances <- activity_records |>
    dplyr::mutate(
      overall_distance_miles = distance_metres * metres_to_miles
    ) |>
    dplyr::select(activity_id, overall_distance_miles) |>
    dplyr::filter(activity_id %in% activity_ids)

  activity_metrics <- activity_streams |>
    dplyr::group_by(activity_id) |>
    dplyr::summarise(
      overall_stream_distance_miles = max(distance, na.rm = TRUE) * metres_to_miles,
      overall_elevation_metres = sum(climb_since_prev, na.rm = TRUE),
      overall_start_time = min(time_seconds, na.rm = TRUE),
      overall_finish_time = max(time_seconds, na.rm = TRUE),
      overall_moving_seconds = overall_finish_time - overall_start_time,
      .groups = "drop"
    )

  df <- df |>
    dplyr::left_join(activity_distances, by = "activity_id") |>
    dplyr::left_join(activity_metrics, by = "activity_id") |>
    dplyr::mutate(
      overall_distance_miles = dplyr::coalesce(
        overall_distance_miles,
        overall_stream_distance_miles
      ),
      coastal_percentage = coastal_distance_miles / overall_distance_miles,
      coastal_percentage = if_else(
        coastal_percentage > 1,
        1,
        coastal_percentage
      )
    )

  return(df)
}

load_coastal_data <- function(
  connection = NULL,
  activities = coastal_activities,
  ferries_data = ferries,
  validate_db = FALSE,
  include_images = TRUE,
  include_position_extremities = TRUE,
  export_rider_traces = FALSE,
  data_source = NULL
) {
  if (validate_db && is.null(connection)) {
    stop("A database connection is required when validate_db is TRUE.", call. = FALSE)
  }

  if (is.null(data_source)) {
    if (is.null(connection)) {
      stop("Provide either connection or data_source.", call. = FALSE)
    }

    data_source <- new_silver_data_source(connection)
  }

  required_loaders <- c("load_activities", "load_streams")
  missing_loaders <- required_loaders[!vapply(
    required_loaders,
    function(name) is.function(data_source[[name]]),
    logical(1)
  )]

  if (length(missing_loaders) > 0) {
    stop(
      "data_source is missing loader functions: ",
      paste(missing_loaders, collapse = ", "),
      call. = FALSE
    )
  }

  validate_coastal_activities(
    activities,
    connection = connection,
    check_db = validate_db
  )
  validate_ferries(ferries_data, activities)

  activity_ids <- activities$activity_id
  activity_records <- data_source$load_activities(activity_ids)
  stream_records <- data_source$load_streams(activity_ids)
  coastal_streams <- stream_records |>
    build_coastal_rides(activities) |>
    enrich_activity_streams(activity_records, activities, coastal = TRUE)
  activity_streams <- stream_records |>
    build_activity_rides(activities) |>
    enrich_activity_streams(activity_records, activities)
  rides_index <- create_summary(
    coastal_streams,
    activity_streams,
    activity_records,
    activities$activity_id
  )
  rider_ids <- extract_riders(activities)
  image_metadata <- if (include_images) get_image_metadata() else NULL
  position_extremities <- if (include_position_extremities) {
    get_position_extremities(coastal_streams)
  } else {
    NULL
  }

  if (export_rider_traces) {
    purrr::walk(rider_ids, ~ export_rider_maps(.x, coastal_streams))
  }

  list(
    coastal_streams = coastal_streams,
    activity_streams = activity_streams,
    rides_index = rides_index,
    image_metadata = image_metadata,
    position_extremities = position_extremities,
    riders = rider_ids,
    activities = activities,
    ferries = ferries_data
  )
}
