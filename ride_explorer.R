# Ride explorer
# Use this script interactively to choose sample-index boundaries for a new
# coastal section. Edit only the four values in "Selection settings".

source("R/load.R")
load_coastal_packages(c(coastal_packages, "DBI"))


# Selection settings ------------------------------------------------------

ride_id <- 5906287061
start_sample_index <- 27837
end_sample_index <- 27839
context_samples <- 5L


# Load and validate ride --------------------------------------------------

con <- connect_coastal_database()
ride_data <- tryCatch(
  load_activity_stream(ride_id, con),
  finally = {
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  }
) |>
  dplyr::arrange(sample_index)

required_columns <- c(
  "sample_index",
  "time_seconds",
  "lat",
  "lng",
  "distance",
  "altitude"
)
missing_columns <- setdiff(required_columns, names(ride_data))

if (length(missing_columns) > 0L) {
  stop(
    "Activity stream is missing columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

if (nrow(ride_data) == 0L) {
  stop("No Silver stream rows found for activity ", ride_id, ".", call. = FALSE)
}

if (anyNA(ride_data$sample_index)) {
  stop("Activity stream contains missing sample indices.", call. = FALSE)
}

if (anyDuplicated(ride_data$sample_index)) {
  stop("Activity stream contains duplicate sample indices.", call. = FALSE)
}

if (nrow(ride_data) > 1L && any(diff(ride_data$sample_index) != 1L)) {
  stop("Activity stream contains non-contiguous sample indices.", call. = FALSE)
}

if (
  length(context_samples) != 1L ||
    is.na(context_samples) ||
    context_samples < 0L ||
    context_samples != as.integer(context_samples)
) {
  stop("Context samples must be a single non-negative integer.", call. = FALSE)
}

sample_range <- range(ride_data$sample_index)
activity_summary <- tibble::tibble(
  activity_id = as.character(ride_id),
  sample_count = nrow(ride_data),
  first_sample_index = sample_range[[1]],
  last_sample_index = sample_range[[2]],
  first_time_seconds = min(ride_data$time_seconds, na.rm = TRUE),
  last_time_seconds = max(ride_data$time_seconds, na.rm = TRUE),
  distance_miles = max(ride_data$distance, na.rm = TRUE) * metres_to_miles
)

message(
  "Activity ",
  ride_id,
  ": ",
  nrow(ride_data),
  " samples (",
  sample_range[[1]],
  "-",
  sample_range[[2]],
  ")."
)
print(activity_summary)

overview_stride <- max(1L, ceiling(nrow(ride_data) / 75L))
overview_samples <- ride_data |>
  dplyr::filter(
    (sample_index - sample_range[[1]]) %% overview_stride == 0L |
      sample_index %in% sample_range
  ) |>
  dplyr::filter(!is.na(lat), !is.na(lng))

overview_map <- leaflet() |>
  addTiles() |>
  add_track(ride_data, track_colour = "#66727C") |>
  addCircleMarkers(
    data = overview_samples,
    lng = ~lng,
    lat = ~lat,
    radius = 4,
    color = "#006D77",
    fillColor = "#FFFFFF",
    fillOpacity = 1,
    weight = 2,
    label = ~ paste0("Sample ", sample_index),
    popup = ~ paste0(
      "Sample: ",
      sample_index,
      "<br>Elapsed: ",
      time_seconds,
      " seconds",
      "<br>Distance: ",
      round(distance * metres_to_miles, 1),
      " miles"
    )
  )

start_missing <- length(start_sample_index) == 1L && is.na(start_sample_index)
end_missing <- length(end_sample_index) == 1L && is.na(end_sample_index)

if (xor(start_missing, end_missing)) {
  stop(
    "Set both start_sample_index and end_sample_index, or leave both as NA. ",
    "Activity range: ",
    sample_range[[1]],
    "-",
    sample_range[[2]],
    ".",
    call. = FALSE
  )
}

if (start_missing && end_missing) {
  message(
    "Overview mode: inspect overview_map, choose both boundaries, then rerun."
  )
  crop_map <- NULL
  start_context_map <- NULL
  end_context_map <- NULL
  endpoint_samples <- NULL
  print(overview_map)
} else {
  if (
    length(start_sample_index) != 1L ||
      length(end_sample_index) != 1L ||
      start_sample_index != as.integer(start_sample_index) ||
      end_sample_index != as.integer(end_sample_index)
  ) {
    stop("Start and end sample indices must be single integers.", call. = FALSE)
  }

  if (start_sample_index > end_sample_index) {
    stop("Start sample index must not exceed end sample index.", call. = FALSE)
  }

  if (start_sample_index < sample_range[[1]]) {
    stop(
      "Start sample index ",
      start_sample_index,
      " is below the activity minimum ",
      sample_range[[1]],
      ".",
      call. = FALSE
    )
  }

  if (end_sample_index > sample_range[[2]]) {
    stop(
      "End sample index ",
      end_sample_index,
      " exceeds the activity maximum ",
      sample_range[[2]],
      " by ",
      end_sample_index - sample_range[[2]],
      ".",
      call. = FALSE
    )
  }

  selected_samples <- ride_data |>
    dplyr::filter(
      sample_index >= start_sample_index,
      sample_index <= end_sample_index
    )

  expected_selected_rows <- end_sample_index - start_sample_index + 1L
  if (nrow(selected_samples) != expected_selected_rows) {
    stop("The selected sample-index interval is incomplete.", call. = FALSE)
  }

  if (anyNA(selected_samples$lat) || anyNA(selected_samples$lng)) {
    stop(
      "The selected interval contains missing GPS coordinates.",
      call. = FALSE
    )
  }

  endpoint_samples <- selected_samples |>
    dplyr::filter(sample_index %in% c(start_sample_index, end_sample_index)) |>
    dplyr::mutate(
      boundary = dplyr::case_when(
        start_sample_index == end_sample_index ~ "Start and end",
        sample_index == start_sample_index ~ "Start",
        TRUE ~ "End"
      ),
      boundary_color = dplyr::case_when(
        boundary == "Start" ~ "#2E7D32",
        boundary == "End" ~ "#C62828",
        TRUE ~ "#6A1B9A"
      )
    ) |>
    dplyr::select(
      boundary,
      boundary_color,
      sample_index,
      time_seconds,
      lat,
      lng,
      distance,
      altitude
    )

  message(
    "Selection mode: ",
    start_sample_index,
    "-",
    end_sample_index,
    " (",
    nrow(selected_samples),
    " samples)."
  )
  print(endpoint_samples |> dplyr::select(-boundary_color))

  crop_map <- leaflet() |>
    addTiles() |>
    add_track(ride_data, track_colour = "#9AA4AD") |>
    add_track(selected_samples, track_colour = "#006D77") |>
    addCircleMarkers(
      data = endpoint_samples,
      lng = ~lng,
      lat = ~lat,
      radius = 7,
      color = ~boundary_color,
      fillColor = ~boundary_color,
      fillOpacity = 1,
      label = ~ paste0(boundary, ": sample ", sample_index)
    )

  build_context_map <- function(boundary_index, boundary_name) {
    context_start <- max(sample_range[[1]], boundary_index - context_samples)
    context_end <- min(sample_range[[2]], boundary_index + context_samples)
    context_data <- ride_data |>
      dplyr::filter(sample_index >= context_start, sample_index <= context_end)
    context_markers <- context_data |>
      dplyr::filter(!is.na(lat), !is.na(lng))
    selected_context <- context_data |>
      dplyr::filter(
        sample_index >= start_sample_index,
        sample_index <= end_sample_index
      )
    boundary_sample <- endpoint_samples |>
      dplyr::filter(boundary == boundary_name)

    map <- leaflet() |>
      addTiles() |>
      add_track(context_data, track_colour = "#9AA4AD")

    if (nrow(selected_context) > 0L) {
      map <- map |>
        add_track(selected_context, track_colour = "#006D77")
    }

    map |>
      addCircleMarkers(
        data = context_markers,
        lng = ~lng,
        lat = ~lat,
        radius = 4,
        color = "#D50032",
        fillOpacity = 0.9
      ) |>
      addLabelOnlyMarkers(
        data = context_markers,
        lng = ~lng,
        lat = ~lat,
        label = ~ as.character(sample_index),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          offset = c(0, -8),
          style = list(
            "font-weight" = "bold",
            "font-size" = "12px"
          )
        )
      ) |>
      addCircleMarkers(
        data = boundary_sample,
        lng = ~lng,
        lat = ~lat,
        radius = 7,
        color = ~boundary_color,
        fillColor = ~boundary_color,
        fillOpacity = 1,
        label = ~ paste0(boundary, ": sample ", sample_index)
      )
  }

  if (start_sample_index == end_sample_index) {
    start_context_map <- build_context_map(start_sample_index, "Start and end")
    end_context_map <- start_context_map
  } else {
    start_context_map <- build_context_map(start_sample_index, "Start")
    end_context_map <- build_context_map(end_sample_index, "End")
  }

  print(crop_map)
}

# Objects available for interactive inspection:
# overview_map, activity_summary, overview_samples
# crop_map, start_context_map, end_context_map, endpoint_samples
