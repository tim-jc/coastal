find_project_root <- function() {
  path <- normalizePath(getwd(), mustWork = FALSE)

  repeat {
    if (
      file.exists(file.path(path, "data", "coastal_activities.R")) &&
        file.exists(file.path(path, "R", "load.R"))
    ) {
      return(path)
    }

    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not find the Coastal project root.", call. = FALSE)
    }
    path <- parent
  }
}

legacy_enrich_activity_streams <- function(
  ride_streams,
  activity_records,
  activities,
  coastal = FALSE
) {
  enriched <- ride_streams |>
    dplyr::inner_join(
      dplyr::select(activity_records, activity_id, start_date_local, strava_link),
      by = "activity_id"
    ) |>
    dplyr::mutate(
      sort_time = dplyr::if_else(
        ride_direction == "cw",
        -time_seconds,
        time_seconds
      ),
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

directional_sample_measurements <- function(cropped_streams) {
  cropped_streams |>
    dplyr::mutate(
      directional_order = dplyr::if_else(
        ride_direction == "cw",
        -sample_index,
        sample_index
      )
    ) |>
    dplyr::arrange(coastal_segment_id, directional_order) |>
    dplyr::group_by(coastal_segment_id) |>
    dplyr::mutate(
      previous_lng = dplyr::lag(lng),
      previous_lat = dplyr::lag(lat),
      previous_altitude = dplyr::lag(altitude),
      directional_distance_metres = geosphere::distHaversine(
        cbind(previous_lng, previous_lat),
        cbind(lng, lat)
      ),
      directional_ascent_metres = dplyr::if_else(
        altitude > previous_altitude,
        altitude - previous_altitude,
        0
      )
    ) |>
    dplyr::summarise(
      directional_distance_miles = sum(
        directional_distance_metres,
        na.rm = TRUE
      ) * metres_to_miles,
      directional_ascent_metres = sum(
        directional_ascent_metres,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

section_stream_diagnostics <- function(cropped_streams) {
  cropped_streams |>
    dplyr::arrange(coastal_segment_id, sample_index) |>
    dplyr::group_by(coastal_segment_id) |>
    dplyr::summarise(
      selected_observations = dplyr::n(),
      missing_sample_indices = sum(is.na(sample_index)),
      duplicate_sample_indices = sum(duplicated(sample_index[!is.na(sample_index)])),
      missing_timestamps = sum(is.na(time_seconds)),
      duplicate_timestamp_rows = sum(
        !is.na(time_seconds) &
          (duplicated(time_seconds) | duplicated(time_seconds, fromLast = TRUE))
      ),
      nonmonotonic_timestamp_steps = {
        valid_times <- time_seconds[!is.na(time_seconds)]
        if (length(valid_times) < 2L) 0L else sum(diff(valid_times) < 0)
      },
      missing_coordinates = sum(is.na(lat) | is.na(lng)),
      invalid_coordinates = sum(
        !is.na(lat) & !is.na(lng) &
          (!is.finite(lat) | !is.finite(lng) |
            lat < -90 | lat > 90 | lng < -180 | lng > 180)
      ),
      missing_altitudes = sum(is.na(altitude)),
      sample_interval_contiguous = {
        indices <- sample_index[!is.na(sample_index)]
        length(indices) <= 1L || all(diff(indices) == 1L)
      },
      .groups = "drop"
    )
}

rank_values <- function(data, value, rank_name = "rank") {
  value <- rlang::ensym(value)
  data |>
    dplyr::arrange(dplyr::desc(!!value), coastal_segment_id) |>
    dplyr::mutate(!!rank_name := dplyr::row_number())
}

calculate_xp_positions <- function(rides_index) {
  mile_equivalent_climb <- min(rides_index$dist_per_elev)

  rides_index |>
    dplyr::select(riders, coastal_distance_miles, coastal_elevation_metres) |>
    dplyr::mutate(rider = stringr::str_split(riders, "\\|")) |>
    tidyr::unnest(rider) |>
    dplyr::mutate(rider = stringr::str_remove_all(rider, "\\(|\\)")) |>
    dplyr::group_by(rider) |>
    dplyr::summarise(
      total_distance_miles = sum(coastal_distance_miles),
      total_ascent_metres = sum(coastal_elevation_metres),
      distance_xp = total_distance_miles * xp_unit,
      elevation_xp = total_ascent_metres * mile_equivalent_climb * xp_unit,
      total_xp = distance_xp + elevation_xp,
      .groups = "drop"
    ) |>
    tidyr::crossing(xp_levels) |>
    dplyr::filter(total_xp <= next_xp, total_xp > xp) |>
    dplyr::transmute(
      rider,
      total_distance_miles,
      total_ascent_metres,
      distance_xp,
      elevation_xp,
      total_xp,
      xp_level,
      level_progress = (total_xp - xp) / (next_xp - xp)
    ) |>
    dplyr::arrange(dplyr::desc(total_xp), rider) |>
    dplyr::mutate(xp_rank = dplyr::row_number())
}

compare_named_metrics <- function(legacy, corrected, key, metrics) {
  legacy_values <- legacy |>
    dplyr::select(dplyr::all_of(c(key, metrics))) |>
    dplyr::rename_with(
      function(name) paste0("legacy_", name),
      dplyr::all_of(metrics)
    )
  corrected_values <- corrected |>
    dplyr::select(dplyr::all_of(c(key, metrics))) |>
    dplyr::rename_with(
      function(name) paste0("corrected_", name),
      dplyr::all_of(metrics)
    )

  dplyr::full_join(legacy_values, corrected_values, by = key)
}

reconcile_measurement_ordering <- function(
  activities,
  activity_records,
  stream_records
) {
  cropped_records <- build_coastal_rides(stream_records, activities)
  full_records <- build_activity_rides(stream_records, activities)

  legacy_coastal <- legacy_enrich_activity_streams(
    cropped_records,
    activity_records,
    activities,
    coastal = TRUE
  )
  legacy_full <- legacy_enrich_activity_streams(
    full_records,
    activity_records,
    activities,
    coastal = FALSE
  )
  corrected_coastal <- enrich_activity_streams(
    cropped_records,
    activity_records,
    activities,
    coastal = TRUE
  )
  corrected_full <- enrich_activity_streams(
    full_records,
    activity_records,
    activities,
    coastal = FALSE
  )

  legacy_rides <- create_summary(
    legacy_coastal,
    legacy_full,
    activity_records,
    activities$activity_id
  )
  corrected_rides <- create_summary(
    corrected_coastal,
    corrected_full,
    activity_records,
    activities$activity_id
  )

  diagnostic_values <- section_stream_diagnostics(cropped_records)
  directional_values <- directional_sample_measurements(cropped_records)
  section_keys <- c(
    "coastal_segment_id", "activity_id", "ride_name", "ride_direction",
    "ride_start_sample_index", "ride_end_sample_index"
  )
  sections <- legacy_rides |>
    dplyr::select(
      coastal_segment_id,
      legacy_distance_miles = coastal_distance_miles,
      legacy_ascent_metres = coastal_elevation_metres
    ) |>
    dplyr::inner_join(
      corrected_rides |>
        dplyr::select(
          coastal_segment_id,
          activity_id,
          ride_name,
          ride_direction,
          corrected_distance_miles = coastal_distance_miles,
          corrected_ascent_metres = coastal_elevation_metres
        ),
      by = "coastal_segment_id"
    ) |>
    dplyr::left_join(
      activities |>
        dplyr::select(
          coastal_segment_id,
          ride_start_sample_index,
          ride_end_sample_index
        ),
      by = "coastal_segment_id"
    ) |>
    dplyr::left_join(directional_values, by = "coastal_segment_id") |>
    dplyr::left_join(diagnostic_values, by = "coastal_segment_id") |>
    dplyr::mutate(
      activity_id = as.character(activity_id),
      distance_difference_miles = corrected_distance_miles - legacy_distance_miles,
      ascent_difference_metres = corrected_ascent_metres - legacy_ascent_metres,
      ascent_percentage_difference = dplyr::if_else(
        legacy_ascent_metres == 0,
        NA_real_,
        100 * ascent_difference_metres / legacy_ascent_metres
      ),
      timestamp_distance_effect_miles = legacy_distance_miles - directional_distance_miles,
      timestamp_ascent_effect_metres = legacy_ascent_metres - directional_ascent_metres,
      change_classification = dplyr::case_when(
        abs(timestamp_distance_effect_miles) > 1e-10 |
          abs(timestamp_ascent_effect_metres) > 1e-10 ~ "TIMESTAMP_ORDER_EFFECT",
        abs(distance_difference_miles) > 1e-10 |
          abs(ascent_difference_metres) > 1e-10 ~ "DIRECTION_REVERSAL_ONLY",
        TRUE ~ "NO_MEASUREMENT_CHANGE"
      )
    ) |>
    dplyr::select(
      dplyr::all_of(section_keys),
      selected_observations,
      legacy_distance_miles,
      corrected_distance_miles,
      distance_difference_miles,
      legacy_ascent_metres,
      corrected_ascent_metres,
      ascent_difference_metres,
      ascent_percentage_difference,
      change_classification,
      timestamp_distance_effect_miles,
      timestamp_ascent_effect_metres,
      dplyr::everything()
    ) |>
    dplyr::arrange(coastal_segment_id)

  legacy_riders <- format_rider_leaderboard(legacy_rides)
  corrected_riders <- format_rider_leaderboard(corrected_rides)
  riders <- compare_named_metrics(
    legacy_riders,
    corrected_riders,
    "rider",
    c("coastal_elevation_metres", "biggest_climb_metres")
  ) |>
    dplyr::mutate(
      ascent_difference_metres = corrected_coastal_elevation_metres -
        legacy_coastal_elevation_metres,
      biggest_climb_difference_metres = corrected_biggest_climb_metres -
        legacy_biggest_climb_metres
    )

  legacy_adventures <- format_adventure_rankings(legacy_rides)
  corrected_adventures <- format_adventure_rankings(corrected_rides)
  adventure_key <- c("adventure", "start_date", "end_date")
  adventures <- compare_named_metrics(
    legacy_adventures,
    corrected_adventures,
    adventure_key,
    c("overall_elevation_metres", "coastal_elevation_metres")
  ) |>
    dplyr::mutate(
      overall_ascent_difference_metres = corrected_overall_elevation_metres -
        legacy_overall_elevation_metres,
      coastal_ascent_difference_metres = corrected_coastal_elevation_metres -
        legacy_coastal_elevation_metres
    )

  legacy_whole <- legacy_rides |>
    dplyr::distinct(activity_id, .keep_all = TRUE) |>
    dplyr::transmute(
      activity_id = as.character(activity_id),
      legacy_overall_elevation_metres = overall_elevation_metres
    )
  whole_activities <- corrected_rides |>
    dplyr::distinct(activity_id, .keep_all = TRUE) |>
    dplyr::transmute(
      activity_id = as.character(activity_id),
      corrected_overall_elevation_metres = overall_elevation_metres
    ) |>
    dplyr::full_join(legacy_whole, by = "activity_id") |>
    dplyr::mutate(
      elevation_difference_metres = corrected_overall_elevation_metres -
        legacy_overall_elevation_metres
    )

  biggest_climbs <- rank_values(
    legacy_rides |>
      dplyr::select(coastal_segment_id, ride_name, coastal_elevation_metres),
    coastal_elevation_metres,
    "legacy_rank"
  ) |>
    dplyr::rename(legacy_ascent_metres = coastal_elevation_metres) |>
    dplyr::inner_join(
      rank_values(
        corrected_rides |>
          dplyr::select(coastal_segment_id, corrected_ascent_metres = coastal_elevation_metres),
        corrected_ascent_metres,
        "corrected_rank"
      ),
      by = "coastal_segment_id"
    ) |>
    dplyr::mutate(
      ascent_difference_metres = corrected_ascent_metres - legacy_ascent_metres,
      rank_change = legacy_rank - corrected_rank
    ) |>
    dplyr::arrange(corrected_rank)

  climb_per_mile <- legacy_rides |>
    dplyr::transmute(
      coastal_segment_id,
      ride_name,
      legacy_climb_per_mile = coastal_elevation_metres / coastal_distance_miles
    ) |>
    dplyr::inner_join(
      corrected_rides |>
        dplyr::transmute(
          coastal_segment_id,
          corrected_climb_per_mile = coastal_elevation_metres /
            coastal_distance_miles
        ),
      by = "coastal_segment_id"
    ) |>
    dplyr::mutate(
      climb_per_mile_difference = corrected_climb_per_mile - legacy_climb_per_mile
    )

  annual <- legacy_rides |>
    dplyr::group_by(yr) |>
    dplyr::summarise(
      legacy_coastal_ascent_metres = sum(coastal_elevation_metres),
      .groups = "drop"
    ) |>
    dplyr::full_join(
      corrected_rides |>
        dplyr::group_by(yr) |>
        dplyr::summarise(
          corrected_coastal_ascent_metres = sum(coastal_elevation_metres),
          .groups = "drop"
        ),
      by = "yr"
    ) |>
    dplyr::mutate(
      ascent_difference_metres = corrected_coastal_ascent_metres -
        legacy_coastal_ascent_metres
    )

  xp <- compare_named_metrics(
    calculate_xp_positions(legacy_rides),
    calculate_xp_positions(corrected_rides),
    "rider",
    c(
      "total_ascent_metres", "elevation_xp", "total_xp", "xp_level",
      "level_progress", "xp_rank"
    )
  ) |>
    dplyr::mutate(
      ascent_difference_metres = corrected_total_ascent_metres -
        legacy_total_ascent_metres,
      total_xp_difference = corrected_total_xp - legacy_total_xp,
      level_change = corrected_xp_level - legacy_xp_level,
      rank_change = legacy_xp_rank - corrected_xp_rank
    )

  summary <- tibble::tibble(
    metric = c(
      "curated_sections",
      "sections_with_changed_ascent",
      "sections_with_changed_distance",
      "legacy_total_coastal_distance_miles",
      "corrected_total_coastal_distance_miles",
      "total_coastal_distance_difference_miles",
      "legacy_total_coastal_ascent_metres",
      "corrected_total_coastal_ascent_metres",
      "total_net_ascent_difference_metres",
      "largest_positive_ascent_difference_metres",
      "largest_negative_ascent_difference_metres",
      "sections_with_duplicate_timestamps",
      "sections_with_nonmonotonic_timestamps",
      "sections_with_timestamp_order_effect",
      "sections_with_missing_or_invalid_observations"
    ),
    value = c(
      nrow(sections),
      sum(abs(sections$ascent_difference_metres) > 1e-10),
      sum(abs(sections$distance_difference_miles) > 1e-10),
      sum(sections$legacy_distance_miles),
      sum(sections$corrected_distance_miles),
      sum(sections$distance_difference_miles),
      sum(sections$legacy_ascent_metres),
      sum(sections$corrected_ascent_metres),
      sum(sections$ascent_difference_metres),
      max(sections$ascent_difference_metres),
      min(sections$ascent_difference_metres),
      sum(sections$duplicate_timestamp_rows > 0L),
      sum(sections$nonmonotonic_timestamp_steps > 0L),
      sum(sections$change_classification == "TIMESTAMP_ORDER_EFFECT"),
      sum(
        sections$missing_sample_indices > 0L |
          sections$duplicate_sample_indices > 0L |
          sections$missing_timestamps > 0L |
          sections$missing_coordinates > 0L |
          sections$invalid_coordinates > 0L |
          sections$missing_altitudes > 0L
      )
    )
  )

  list(
    summary = summary,
    sections = sections,
    riders = riders,
    adventures = adventures,
    whole_activities = whole_activities,
    biggest_climbs = biggest_climbs,
    climb_per_mile = climb_per_mile,
    annual = annual,
    xp = xp
  )
}

write_reconciliation_outputs <- function(reconciliation, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  purrr::iwalk(reconciliation, function(data, name) {
    utils::write.csv(
      data,
      file.path(output_dir, paste0(name, ".csv")),
      row.names = FALSE,
      na = ""
    )
  })
}

print_reconciliation_summary <- function(reconciliation, output_dir = NULL) {
  cat("\nCoastal measurement-ordering reconciliation\n")
  print(reconciliation$summary, n = Inf)

  cat("\nLargest absolute section ascent differences\n")
  reconciliation$sections |>
    dplyr::arrange(dplyr::desc(abs(ascent_difference_metres))) |>
    dplyr::select(
      coastal_segment_id,
      ride_name,
      ride_direction,
      legacy_ascent_metres,
      corrected_ascent_metres,
      ascent_difference_metres,
      change_classification
    ) |>
    utils::head(10L) |>
    print(n = 10L)

  if (!is.null(output_dir)) {
    cat("\nCSV output directory:\n", normalizePath(output_dir), "\n", sep = "")
  }
}

parse_reconciliation_args <- function(args) {
  if ("--help" %in% args) return(list(help = TRUE, output_dir = NULL))

  output_args <- grep("^--output-dir=", args, value = TRUE)
  unknown <- args[!grepl("^--output-dir=", args)]
  if (length(unknown) > 0L || length(output_args) > 1L) {
    stop(
      paste(
        "Usage: Rscript scripts/reconcile_measurement_ordering.R",
        "[--output-dir=PATH]"
      ),
      call. = FALSE
    )
  }

  output_dir <- if (length(output_args) == 1L) {
    sub("^--output-dir=", "", output_args)
  } else {
    file.path("outputs", "measurement_reconciliation")
  }
  if (!nzchar(output_dir)) stop("--output-dir requires a path.", call. = FALSE)

  list(help = FALSE, output_dir = output_dir)
}

main <- function() {
  args <- parse_reconciliation_args(commandArgs(trailingOnly = TRUE))
  if (args$help) {
    cat(
      "Usage: Rscript scripts/reconcile_measurement_ordering.R ",
      "[--output-dir=PATH]\n",
      sep = ""
    )
    return(invisible(NULL))
  }

  project_root <- find_project_root()
  source(file.path(project_root, "R", "load.R"))
  load_coastal_packages(c(coastal_packages, "DBI"))

  output_dir <- args$output_dir
  if (!grepl("^/", output_dir)) output_dir <- file.path(project_root, output_dir)
  if (!startsWith(
    normalizePath(output_dir, mustWork = FALSE),
    paste0(normalizePath(file.path(project_root, "outputs"), mustWork = FALSE), "/")
  )) {
    stop("Reconciliation output must be below the ignored outputs/ directory.", call. = FALSE)
  }

  connection <- connect_coastal_database()
  on.exit({
    if (DBI::dbIsValid(connection)) DBI::dbDisconnect(connection)
  }, add = TRUE)

  activity_ids <- unique(coastal_activities$activity_id)
  activity_records <- load_activity_records(connection, activity_ids)
  stream_records <- load_stream_records(connection, activity_ids)
  reconciliation <- reconcile_measurement_ordering(
    coastal_activities,
    activity_records,
    stream_records
  )

  if (nrow(reconciliation$sections) != nrow(coastal_activities)) {
    stop(
      stringr::str_glue(
        "Expected {nrow(coastal_activities)} sections but reconciled {nrow(reconciliation$sections)}."
      ),
      call. = FALSE
    )
  }
  if (any(reconciliation$sections$selected_observations == 0L)) {
    stop("At least one curated section selected no observations.", call. = FALSE)
  }

  write_reconciliation_outputs(reconciliation, output_dir)
  print_reconciliation_summary(reconciliation, output_dir)
  invisible(reconciliation)
}

if (sys.nframe() == 0L) main()
