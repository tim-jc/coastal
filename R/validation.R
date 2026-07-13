validate_coastal_activities <- function(
    activities = coastal_activities,
    connection = NULL,
    check_db = !is.null(connection)
) {
  required_columns <- c(
    "activity_id", "from", "to", "ride_direction", "riders",
    "ride_start_time", "ride_end_time", "ride_name", "riders_pretty"
  )
  errors <- character()
  missing_columns <- setdiff(required_columns, names(activities))

  if (length(missing_columns) > 0) {
    errors <- c(errors, stringr::str_glue("Missing columns: {paste(missing_columns, collapse = ', ')}"))
  }

  if (length(errors) == 0) {
    empty_text <- activities %>%
      dplyr::filter(
        is.na(from) | from == "" |
          is.na(to) | to == "" |
          is.na(riders) | riders == ""
      )

    invalid_direction <- activities %>%
      dplyr::filter(!ride_direction %in% c("cw", "acw"))

    invalid_time_window <- activities %>%
      dplyr::filter(
        is.na(ride_start_time) | is.na(ride_end_time) |
          ride_start_time < 0 | ride_end_time < 0 |
          ride_start_time > ride_end_time
      )

    duplicated_segments <- activities %>%
      dplyr::count(activity_id, from, to, ride_start_time, ride_end_time) %>%
      dplyr::filter(n > 1)

    if (any(is.na(activities$activity_id))) {
      errors <- c(errors, "At least one activity_id is missing or invalid.")
    }

    if (nrow(empty_text) > 0) {
      errors <- c(errors, stringr::str_glue("{nrow(empty_text)} rows have missing from/to/riders values."))
    }

    if (nrow(invalid_direction) > 0) {
      errors <- c(errors, stringr::str_glue("{nrow(invalid_direction)} rows have invalid ride_direction values."))
    }

    if (nrow(invalid_time_window) > 0) {
      errors <- c(errors, stringr::str_glue("{nrow(invalid_time_window)} rows have invalid crop windows."))
    }

    if (nrow(duplicated_segments) > 0) {
      errors <- c(errors, stringr::str_glue("{nrow(duplicated_segments)} duplicate activity segment rows found."))
    }
  }

  if (length(errors) == 0 && check_db) {
    activity_ids <- unique(activities$activity_id)

    silver_activity_ids <- silver_tbl("activities", connection) %>%
      dplyr::select(activity_id) %>%
      dplyr::filter(activity_id %in% activity_ids) %>%
      dplyr::distinct() %>%
      dplyr::collect()

    missing_activities <- setdiff(
      as.character(activity_ids),
      as.character(silver_activity_ids$activity_id)
    )

    if (length(missing_activities) > 0) {
      errors <- c(errors, stringr::str_glue(
        "Activities missing from silver activities: {paste(missing_activities, collapse = ', ')}"
      ))
    }

    stream_times <- silver_tbl("activity_streams", connection) %>%
      dplyr::select(activity_id, time_seconds) %>%
      dplyr::filter(activity_id %in% activity_ids) %>%
      dplyr::collect()

    missing_streams <- setdiff(
      as.character(activity_ids),
      as.character(unique(stream_times$activity_id))
    )

    if (length(missing_streams) > 0) {
      errors <- c(errors, stringr::str_glue(
        "Activities missing from silver activity_streams: {paste(missing_streams, collapse = ', ')}"
      ))
    }

    cropped_counts <- activities %>%
      dplyr::mutate(segment_id = dplyr::row_number()) %>%
      dplyr::select(segment_id, activity_id, ride_start_time, ride_end_time) %>%
      dplyr::inner_join(
        stream_times,
        by = "activity_id",
        relationship = "many-to-many"
      ) %>%
      dplyr::filter(time_seconds >= ride_start_time, time_seconds <= ride_end_time) %>%
      dplyr::count(segment_id, name = "stream_rows")

    missing_cropped_segments <- setdiff(seq_len(nrow(activities)), cropped_counts$segment_id)

    if (length(missing_cropped_segments) > 0) {
      bad_segments <- activities[missing_cropped_segments, ] %>%
        dplyr::transmute(segment = stringr::str_glue("{activity_id} ({from} -> {to})"))

      errors <- c(errors, stringr::str_glue(
        "No stream rows inside crop windows for: {paste(bad_segments$segment, collapse = ', ')}"
      ))
    }
  }

  if (length(errors) > 0) {
    stop(
      paste(c("Invalid coastal activities metadata:", paste0("- ", errors)), collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(activities)
}

validate_ferries <- function(ferries_data = ferries, activities = coastal_activities) {
  required_columns <- c("ferry", "activity_id", "lat", "lng")
  errors <- character()
  missing_columns <- setdiff(required_columns, names(ferries_data))

  if (length(missing_columns) > 0) {
    errors <- c(errors, stringr::str_glue("Missing columns: {paste(missing_columns, collapse = ', ')}"))
  }

  if (length(errors) == 0) {
    missing_values <- ferries_data %>%
      dplyr::filter(is.na(ferry) | ferry == "" | is.na(activity_id) | is.na(lat) | is.na(lng))

    unknown_activities <- setdiff(
      as.character(ferries_data$activity_id),
      as.character(unique(activities$activity_id))
    )

    if (nrow(missing_values) > 0) {
      errors <- c(errors, stringr::str_glue("{nrow(missing_values)} ferry rows have missing values."))
    }

    if (length(unknown_activities) > 0) {
      errors <- c(errors, stringr::str_glue(
        "Ferries reference unknown activities: {paste(unknown_activities, collapse = ', ')}"
      ))
    }
  }

  if (length(errors) > 0) {
    stop(
      paste(c("Invalid ferries metadata:", paste0("- ", errors)), collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(ferries_data)
}
