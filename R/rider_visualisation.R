rider_visualisation_defaults <- list(
  background_colour = "#273c75",
  map_colour = "#192a56",
  foreground_colour = "#FFFFFF",
  a2_width_mm = 420,
  a2_height_mm = 594,
  frame_width_mm = 400,
  frame_height_mm = 500,
  dpi = 300
)

available_font_family <- function(
  preferred = c("Avenir", "Arial", "sans"),
  font_provider = NULL
) {
  installed <- tryCatch(
    suppressWarnings({
      if (!is.null(font_provider)) {
        font_provider()
      } else if (requireNamespace("extrafont", quietly = TRUE)) {
        extrafont::fonts()
      } else if (requireNamespace("systemfonts", quietly = TRUE)) {
        systemfonts::system_fonts()$family
      } else {
        character()
      }
    }),
    error = function(error) character()
  )
  installed <- unique(as.character(installed))
  match <- preferred[preferred %in% installed]
  if (length(match) > 0) match[[1]] else tail(preferred, 1)
}

rider_membership <- function(rider_values, rider) {
  if (length(rider) != 1 || is.na(rider) || !nzchar(rider)) {
    stop("rider must be one non-empty value.", call. = FALSE)
  }

  vapply(
    rider_values,
    function(value) {
      if (is.na(value) || !nzchar(value)) {
        return(FALSE)
      }

      members <- strsplit(value, "|", fixed = TRUE)[[1]]
      members <- stringr::str_remove(members, "^\\(")
      members <- stringr::str_remove(members, "\\)$")
      rider %in% members
    },
    logical(1)
  )
}

filter_rider_rows <- function(data, rider) {
  if (!"riders" %in% names(data)) {
    stop("data must contain a riders column.", call. = FALSE)
  }

  data[rider_membership(data$riders, rider), , drop = FALSE]
}

summarise_rider_years <- function(rider_index) {
  if (nrow(rider_index) == 0) {
    stop("Cannot summarise a rider with no ride rows.", call. = FALSE)
  }

  rider_index |>
    dplyr::mutate(yr = as.character(yr)) |>
    dplyr::group_by(yr) |>
    dplyr::summarise(
      total_miles = sum(coastal_distance_miles, na.rm = TRUE),
      total_climb = sum(coastal_elevation_metres, na.rm = TRUE),
      .groups = "drop"
    )
}

rider_plot_theme <- function(
  font_family,
  background_colour = rider_visualisation_defaults$background_colour
) {
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(
      colour = rider_visualisation_defaults$foreground_colour,
      size = 20,
      family = font_family
    ),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = background_colour),
    plot.background = ggplot2::element_rect(
      fill = background_colour,
      colour = NA
    ),
    plot.margin = ggplot2::margin(t = 0, l = 0, b = 2, r = 2, "cm")
  )
}

create_metric_plot <- function(
  summary_data,
  metric,
  unit,
  font_family,
  symbol_font_family = font_family
) {
  metric <- rlang::ensym(metric)
  metric_name <- rlang::as_string(metric)
  values <- summary_data[[metric_name]]

  if (length(values) == 0 || all(is.na(values))) {
    stop("Metric has no values to plot: ", metric_name, call. = FALSE)
  }

  max_value <- as.integer(max(values, na.rm = TRUE))
  plot_data <- dplyr::bind_rows(
    summary_data,
    dplyr::tibble(
      yr = "",
      total_miles = NA_real_,
      total_climb = NA_real_
    )
  )
  label_data <- dplyr::tibble(
    yr = "",
    value = max_value,
    label = stringr::str_c(max_value, "\n", unit)
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = yr, y = !!metric)) +
    ggplot2::geom_segment(
      x = 2,
      xend = nrow(plot_data),
      y = max_value,
      yend = max_value,
      linetype = "dashed",
      colour = rider_visualisation_defaults$foreground_colour
    ) +
    ggplot2::geom_text(
      data = summary_data,
      label = "\u2699",
      size = 10,
      colour = rider_visualisation_defaults$foreground_colour,
      family = symbol_font_family
    ) +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = yr, y = value, label = label),
      inherit.aes = FALSE,
      colour = rider_visualisation_defaults$foreground_colour,
      fontface = "italic",
      family = font_family,
      size = 6,
      hjust = 1
    ) +
    ggplot2::coord_flip() +
    ggplot2::expand_limits(y = c(50, max(50, max_value * 1.1))) +
    rider_plot_theme(font_family)
}

create_rider_map_plot <- function(
  rider_streams,
  uk_outline_map,
  background_colour = rider_visualisation_defaults$background_colour
) {
  if (nrow(rider_streams) == 0) {
    stop("Cannot create a map for a rider with no coastal stream rows.", call. = FALSE)
  }

  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = uk_outline_map,
      ggplot2::aes(x = long, y = lat, group = group),
      fill = rider_visualisation_defaults$map_colour
    ) +
    ggplot2::geom_point(
      data = rider_streams,
      ggplot2::aes(x = lng, y = lat),
      colour = rider_visualisation_defaults$foreground_colour,
      size = 0.1,
      alpha = 0.9
    ) +
    ggplot2::coord_map() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = background_colour),
      plot.background = ggplot2::element_rect(
        fill = background_colour,
        colour = NA
      )
    )
}

format_rider_subtitle <- function(rider_index, summary_data) {
  dates <- as.Date(rider_index$start_date_local)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    stop("Rider rows contain no valid start dates.", call. = FALSE)
  }

  stringr::str_glue(
    "{format(min(dates), '%B %Y')} to {format(max(dates), '%B %Y')}\n",
    "{as.integer(sum(summary_data$total_miles, na.rm = TRUE))} miles ridden\n",
    "{as.integer(sum(summary_data$total_climb, na.rm = TRUE))} metres climbed"
  )
}

create_rider_visualisation <- function(
  rider,
  coastal_streams,
  rides_index,
  uk_outline_map,
  font_family = available_font_family(),
  symbol_font_family = available_font_family(c("Apple Symbols", font_family)),
  dimensions = rider_visualisation_defaults
) {
  rider_streams <- filter_rider_rows(coastal_streams, rider)
  rider_index <- filter_rider_rows(rides_index, rider)

  if (nrow(rider_streams) == 0 || nrow(rider_index) == 0) {
    warning("Skipping rider with incomplete visualisation data: ", rider)
    return(NULL)
  }

  summary_data <- summarise_rider_years(rider_index)
  map_plot <- create_rider_map_plot(rider_streams, uk_outline_map)
  miles_plot <- create_metric_plot(
    summary_data,
    total_miles,
    "miles",
    font_family,
    symbol_font_family
  )
  climb_plot <- create_metric_plot(
    summary_data,
    total_climb,
    "metres",
    font_family,
    symbol_font_family
  )
  background <- ggplot2::ggplot() +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(
        fill = dimensions$background_colour,
        colour = NA
      )
    )
  x_val <- (1 - dimensions$frame_width_mm / dimensions$a2_width_mm) / 2
  y_val <- (1 - dimensions$frame_height_mm / dimensions$a2_height_mm) / 2

  cowplot::ggdraw(background) +
    cowplot::draw_plot(map_plot, width = 0.8, height = 0.8, x = 0.1, y = 0.07) +
    cowplot::draw_plot(
      miles_plot,
      x = x_val + 0.05,
      y = 0.1,
      width = 0.275,
      height = 0.2
    ) +
    cowplot::draw_plot(
      climb_plot,
      x = x_val + 0.65,
      y = 0.6,
      width = 0.275,
      height = 0.2
    ) +
    cowplot::draw_text(
      "COASTING",
      x = x_val + 0.01,
      y = (1 - y_val) * 0.97,
      size = 80,
      family = font_family,
      colour = dimensions$foreground_colour,
      fontface = "bold",
      hjust = 0
    ) +
    cowplot::draw_text(
      format_rider_subtitle(rider_index, summary_data),
      x = x_val + 0.01,
      y = (1 - y_val) * 0.915,
      size = 20,
      family = font_family,
      colour = dimensions$foreground_colour,
      hjust = 0
    )
}

rider_output_filename <- function(rider, output_dir) {
  safe_rider <- stringr::str_replace_all(rider, "[^A-Za-z0-9_-]+", "_")
  file.path(output_dir, stringr::str_glue("coastal_vis_{safe_rider}.png"))
}

export_rider_visualisation <- function(
  plot,
  rider,
  output_dir,
  dimensions = rider_visualisation_defaults
) {
  if (is.null(plot)) {
    return(invisible(NULL))
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- rider_output_filename(rider, output_dir)
  ggplot2::ggsave(
    filename = output_file,
    plot = plot,
    device = "png",
    width = dimensions$a2_width_mm,
    height = dimensions$a2_height_mm,
    units = "mm",
    dpi = dimensions$dpi
  )
  invisible(output_file)
}
