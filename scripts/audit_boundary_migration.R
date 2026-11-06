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

boundary_match_type <- function(match_count) {
  if (match_count == 0L) return("UNMATCHED")
  if (match_count == 1L) return("UNIQUE")
  "DUPLICATED"
}

max_repeated_timestamp_run <- function(time_seconds) {
  if (length(time_seconds) == 0L) return(0L)
  values <- ifelse(is.na(time_seconds), "<NA>", as.character(time_seconds))
  max(rle(values)$lengths)
}

stream_content_fingerprint <- function(activity_streams) {
  if (nrow(activity_streams) == 0L) return(NA_character_)

  excluded <- c(
    "activity_id",
    "raw_stream_retrieved_at",
    "transformed_at"
  )
  content_columns <- setdiff(names(activity_streams), excluded)
  content <- activity_streams[order(activity_streams$sample_index), content_columns, drop = FALSE]
  rownames(content) <- NULL
  digest::digest(content, algo = "sha256", serialize = TRUE)
}

format_lineage_value <- function(values) {
  values <- values[!is.na(values)]
  if (length(values) == 0L) return(NA_character_)
  format(max(values), tz = "UTC", usetz = TRUE)
}

audit_boundary_section <- function(section, activity_streams) {
  activity_streams <- activity_streams[order(activity_streams$sample_index), , drop = FALSE]
  start_time <- section$ride_start_time[[1]]
  end_time <- section$ride_end_time[[1]]
  invalid_boundary <- is.na(start_time) || is.na(end_time) || start_time > end_time

  start_match_count <- if (is.na(start_time)) 0L else {
    sum(activity_streams$time_seconds == start_time, na.rm = TRUE)
  }
  end_match_count <- if (is.na(end_time)) 0L else {
    sum(activity_streams$time_seconds == end_time, na.rm = TRUE)
  }
  timestamp_selected <- if (invalid_boundary) {
    activity_streams[0, , drop = FALSE]
  } else {
    activity_streams[
      !is.na(activity_streams$time_seconds) &
        activity_streams$time_seconds >= start_time &
        activity_streams$time_seconds <= end_time,
      ,
      drop = FALSE
    ]
  }

  sample_index_nulls <- sum(is.na(activity_streams$sample_index))
  non_null_indices <- activity_streams$sample_index[!is.na(activity_streams$sample_index)]
  sample_index_duplicates <- sum(duplicated(non_null_indices))
  valid_times <- activity_streams$time_seconds[!is.na(activity_streams$time_seconds)]
  nonmonotonic_steps <- if (length(valid_times) < 2L) {
    0L
  } else {
    sum(diff(valid_times) < 0)
  }

  proposed_start <- proposed_end <- NA_integer_
  index_selected <- activity_streams[0, , drop = FALSE]
  exact_ordered_match <- FALSE
  numerically_contiguous <- FALSE

  if (nrow(timestamp_selected) > 0L && !any(is.na(timestamp_selected$sample_index))) {
    proposed_start <- min(timestamp_selected$sample_index)
    proposed_end <- max(timestamp_selected$sample_index)
    index_selected <- activity_streams[
      !is.na(activity_streams$sample_index) &
        activity_streams$sample_index >= proposed_start &
        activity_streams$sample_index <= proposed_end,
      ,
      drop = FALSE
    ]
    index_selected <- index_selected[order(index_selected$sample_index), , drop = FALSE]

    timestamp_indices <- as.integer(timestamp_selected$sample_index)
    index_indices <- as.integer(index_selected$sample_index)
    exact_ordered_match <- identical(timestamp_indices, index_indices)
    numerically_contiguous <- length(timestamp_indices) <= 1L ||
      all(diff(timestamp_indices) == 1L)
  }

  if (nrow(activity_streams) == 0L) {
    status <- "UNRESOLVED"
    reason <- "NO_STREAM_ROWS"
  } else if (invalid_boundary) {
    status <- "UNRESOLVED"
    reason <- "INVALID_TIMESTAMP_BOUNDARY"
  } else if (nrow(timestamp_selected) == 0L) {
    status <- "UNRESOLVED"
    reason <- "NO_TIMESTAMP_SELECTED_ROWS"
  } else if (sample_index_nulls > 0L || any(is.na(timestamp_selected$sample_index))) {
    status <- "UNRESOLVED"
    reason <- "MISSING_SAMPLE_INDEX"
  } else if (sample_index_duplicates > 0L) {
    status <- "UNRESOLVED"
    reason <- "DUPLICATE_SAMPLE_INDEX"
  } else if (!exact_ordered_match) {
    status <- "REQUIRES_REVIEW"
    reason <- "INDEX_INTERVAL_MEMBERSHIP_DIFFERS"
  } else if (!numerically_contiguous) {
    status <- "REQUIRES_REVIEW"
    reason <- "SAMPLE_INDEX_GAPS"
  } else {
    status <- "EXACT_MATCH"
    reason <- "IDENTICAL_ORDERED_MEMBERSHIP"
  }

  route <- paste(section$from[[1]], section$to[[1]], sep = " -> ")
  tibble::tibble(
    coastal_segment_id = as.integer(section$coastal_segment_id[[1]]),
    activity_id = as.character(section$activity_id[[1]]),
    route = route,
    existing_start_time = start_time,
    existing_end_time = end_time,
    proposed_start_sample_index = proposed_start,
    proposed_end_sample_index = proposed_end,
    timestamp_selected_rows = nrow(timestamp_selected),
    index_selected_rows = nrow(index_selected),
    exact_ordered_sample_index_match = exact_ordered_match,
    proposed_interval_contiguous = exact_ordered_match && numerically_contiguous,
    start_timestamp_match_count = as.integer(start_match_count),
    start_timestamp_match_type = boundary_match_type(start_match_count),
    end_timestamp_match_count = as.integer(end_match_count),
    end_timestamp_match_type = boundary_match_type(end_match_count),
    activity_stream_rows = nrow(activity_streams),
    activity_has_duplicate_timestamps = anyDuplicated(valid_times) > 0L,
    maximum_repeated_timestamp_run = max_repeated_timestamp_run(
      activity_streams$time_seconds
    ),
    nonmonotonic_timestamp_steps = as.integer(nonmonotonic_steps),
    sample_index_nulls = as.integer(sample_index_nulls),
    sample_index_duplicates = as.integer(sample_index_duplicates),
    stream_content_sha256 = stream_content_fingerprint(activity_streams),
    raw_stream_retrieved_at = if ("raw_stream_retrieved_at" %in% names(activity_streams)) {
      format_lineage_value(activity_streams$raw_stream_retrieved_at)
    } else {
      NA_character_
    },
    transformed_at = if ("transformed_at" %in% names(activity_streams)) {
      format_lineage_value(activity_streams$transformed_at)
    } else {
      NA_character_
    },
    status = status,
    status_reason = reason
  )
}

audit_boundary_migration <- function(activities, streams) {
  required_activity_columns <- c(
    "activity_id",
    "from",
    "to",
    "ride_start_time",
    "ride_end_time",
    "coastal_segment_id"
  )
  required_stream_columns <- c("activity_id", "sample_index", "time_seconds")

  missing_activity_columns <- setdiff(required_activity_columns, names(activities))
  missing_stream_columns <- setdiff(required_stream_columns, names(streams))
  if (length(missing_activity_columns) > 0L) {
    stop(
      "Activities are missing columns: ",
      paste(missing_activity_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(missing_stream_columns) > 0L) {
    stop(
      "Streams are missing columns: ",
      paste(missing_stream_columns, collapse = ", "),
      call. = FALSE
    )
  }

  stream_ids <- as.character(streams$activity_id)
  reports <- lapply(seq_len(nrow(activities)), function(row_number) {
    section <- activities[row_number, , drop = FALSE]
    activity_id <- as.character(section$activity_id[[1]])
    audit_boundary_section(section, streams[stream_ids == activity_id, , drop = FALSE])
  })

  dplyr::bind_rows(reports) |>
    dplyr::arrange(coastal_segment_id)
}

parse_audit_args <- function(args) {
  if ("--help" %in% args) {
    return(list(help = TRUE, output = NULL))
  }

  output_args <- grep("^--output=", args, value = TRUE)
  unknown <- args[!grepl("^--output=", args)]
  if (length(unknown) > 0L || length(output_args) > 1L) {
    stop("Usage: Rscript scripts/audit_boundary_migration.R [--output=PATH]", call. = FALSE)
  }

  output <- if (length(output_args) == 1L) sub("^--output=", "", output_args) else NULL
  if (!is.null(output) && !nzchar(output)) {
    stop("--output requires a non-empty path.", call. = FALSE)
  }

  list(
    help = FALSE,
    output = output
  )
}

load_audit_streams <- function(connection, activity_ids) {
  silver_tbl("activity_streams", connection) |>
    dplyr::filter(activity_id %in% activity_ids) |>
    dplyr::select(
      activity_id,
      sample_index,
      time_seconds,
      distance_metres,
      latitude,
      longitude,
      altitude_metres,
      velocity_smooth_metres_per_second,
      heartrate_bpm,
      cadence_rpm,
      watts,
      temperature_celsius,
      is_moving,
      grade_smooth_percent,
      raw_stream_retrieved_at,
      transformed_at
    ) |>
    dplyr::collect()
}

print_audit_report <- function(audit) {
  cat("\nBoundary migration audit summary\n")
  print(audit |> dplyr::count(status, status_reason), n = Inf, width = Inf)

  flagged <- audit |>
    dplyr::filter(
      status != "EXACT_MATCH" |
        start_timestamp_match_type != "UNIQUE" |
        end_timestamp_match_type != "UNIQUE" |
        activity_has_duplicate_timestamps |
        nonmonotonic_timestamp_steps > 0L
    )
  cat("\nFlagged sections and boundary diagnostics\n")
  if (nrow(flagged) == 0L) {
    cat("None\n")
  } else {
    print(flagged, n = Inf, width = Inf)
  }

  cat("\nAll sections\n")
  print(audit, n = Inf, width = Inf)
}

main <- function() {
  args <- parse_audit_args(commandArgs(trailingOnly = TRUE))
  if (args$help) {
    cat("Usage: Rscript scripts/audit_boundary_migration.R [--output=PATH]\n")
    return(invisible(NULL))
  }

  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required to fingerprint stream content.", call. = FALSE)
  }

  project_root <- find_project_root()
  source(file.path(project_root, "R", "load.R"))
  load_coastal_packages(c(coastal_packages, "DBI"))

  connection <- connect_coastal_database()
  on.exit({
    if (DBI::dbIsValid(connection)) DBI::dbDisconnect(connection)
  }, add = TRUE)

  streams <- load_audit_streams(connection, unique(coastal_activities$activity_id))
  audit <- audit_boundary_migration(coastal_activities, streams)
  print_audit_report(audit)

  if (!is.null(args$output)) {
    output_path <- args$output
    if (!grepl("^/", output_path)) output_path <- file.path(project_root, output_path)
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(audit, output_path, row.names = FALSE, na = "")
    message("Audit report written to: ", normalizePath(output_path, mustWork = TRUE))
  }

  invisible(audit)
}

if (sys.nframe() == 0L) main()
