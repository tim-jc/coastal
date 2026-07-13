draw_cumulative_progress_plot <- function(rides_index) {
  progress <- rides_index |>
    dplyr::arrange(start_date_local) |>
    dplyr::mutate(
      cumulative_miles = cumsum(coastal_distance_miles),
      text_label = str_glue(
        "{ride_name}
{format(start_date, '%d %b %Y')}
Coastal miles: {round(coastal_distance_miles, 1)}
Project total: {round(cumulative_miles, 1)}"
      )
    )

  plot <- progress |>
    ggplot(aes(
      x = start_date,
      y = cumulative_miles,
      text = text_label
    )) +
    geom_step(colour = phiets_navy, linewidth = 1) +
    geom_point(colour = phiets_red, size = 2) +
    labs(x = "", y = "Cumulative coastal miles") +
    theme_minimal() +
    theme(
      axis.text = element_text(colour = phiets_navy),
      axis.title = element_text(colour = phiets_navy),
      panel.grid.minor = element_blank()
    )

  ggplotly(plot, tooltip = "text")
}

draw_monthly_activity_plot <- function(rides_index) {
  monthly_activity <- rides_index |>
    dplyr::mutate(
      month = lubridate::month(start_date, label = TRUE, abbr = TRUE)
    ) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      ride_days = dplyr::n_distinct(start_date),
      coastal_miles = sum(coastal_distance_miles, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      text_label = str_glue(
        "{month}
Ride days: {ride_days}
Coastal miles: {round(coastal_miles, 1)}"
      )
    )

  plot <- monthly_activity |>
    ggplot(aes(x = month, y = coastal_miles, text = text_label)) +
    geom_col(fill = phiets_red, colour = phiets_red, alpha = 0.25) +
    labs(x = "", y = "Coastal miles") +
    theme_minimal() +
    theme(
      axis.text = element_text(colour = phiets_navy),
      axis.title = element_text(colour = phiets_navy),
      panel.grid.minor = element_blank()
    )

  ggplotly(plot, tooltip = "text")
}

draw_rider_contribution_plot <- function(rides_index) {
  rider_totals <- format_rider_leaderboard(rides_index) |>
    dplyr::arrange(dplyr::desc(coastal_miles)) |>
    dplyr::mutate(
      rider = factor(rider, levels = rev(rider)),
      text_label = str_glue(
        "{rider}
Coastal miles: {round(coastal_miles, 1)}
Ride days: {ride_days}
Coastal climb: {round(coastal_elevation_metres, 0)}m"
      )
    )

  plot <- rider_totals |>
    ggplot(aes(x = rider, y = coastal_miles, text = text_label)) +
    geom_col(fill = phiets_red, colour = phiets_red, alpha = 0.25) +
    coord_flip() +
    labs(x = "", y = "Coastal miles") +
    theme_minimal() +
    theme(
      axis.text = element_text(colour = phiets_navy),
      axis.title = element_text(colour = phiets_navy),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )

  ggplotly(plot, tooltip = "text")
}

format_rider_leaderboard <- function(rides_index) {
  rides_index |>
    dplyr::select(
      adventure_id,
      start_date,
      riders,
      coastal_distance_miles,
      coastal_elevation_metres
    ) |>
    dplyr::mutate(rider = stringr::str_split(riders, "\\|")) |>
    tidyr::unnest(rider) |>
    dplyr::mutate(rider = stringr::str_remove_all(rider, "\\(|\\)")) |>
    dplyr::group_by(rider) |>
    dplyr::summarise(
      adventures = dplyr::n_distinct(adventure_id),
      ride_days = dplyr::n_distinct(start_date),
      coastal_miles = sum(coastal_distance_miles, na.rm = TRUE),
      longest_day_miles = max(coastal_distance_miles, na.rm = TRUE),
      biggest_climb_metres = max(coastal_elevation_metres, na.rm = TRUE),
      coastal_elevation_metres = sum(coastal_elevation_metres, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(coastal_miles)) |>
    dplyr::select(
      rider,
      adventures,
      ride_days,
      coastal_miles,
      coastal_elevation_metres,
      longest_day_miles,
      biggest_climb_metres
    )
}

get_longest_day_activity <- function(rides_index) {
  rides_index |>
    dplyr::mutate(
      coastal_distance_miles = if_else(
        coastal_distance_miles > overall_distance_miles,
        overall_distance_miles,
        coastal_distance_miles
      )
    ) |>
    dplyr::slice_max(coastal_distance_miles, n = 1, with_ties = FALSE)
}

get_biggest_climb_activity <- function(rides_index) {
  rides_index |>
    dplyr::slice_max(coastal_elevation_metres, n = 1, with_ties = FALSE)
}

format_hardest_days_table <- function(rides_index, n = 12) {
  rides_index |>
    dplyr::mutate(
      ride_name_link = str_c(
        "<a href=\"",
        strava_link,
        "\" target=\"_blank\">",
        ride_name,
        "</a>"
      ),
      ride_date = as.Date(start_date_local),
      riders_display = purrr::map_chr(riders, format_rider_list),
      moving_seconds = finish_time - start_time,
      moving_time = format_duration_hours(moving_seconds),
      climb_per_mile = coastal_elevation_metres / coastal_distance_miles,
      coastal_distance_miles = if_else(
        coastal_distance_miles > overall_distance_miles,
        overall_distance_miles,
        coastal_distance_miles
      )
    ) |>
    dplyr::arrange(
      dplyr::desc(coastal_distance_miles),
      dplyr::desc(coastal_elevation_metres)
    ) |>
    dplyr::slice_head(n = n) |>
    dplyr::select(
      ride_date,
      ride_name_link,
      riders = riders_display,
      coastal_distance_miles,
      coastal_elevation_metres,
      climb_per_mile,
      moving_seconds,
      moving_time
    )
}

format_adventure_rankings <- function(rides_index) {
  rides_index |>
    dplyr::arrange(start_date_local, start_time) |>
    dplyr::group_by(adventure_id) |>
    dplyr::summarise(
      adventure = str_glue(
        "{stringr::str_to_title(dplyr::first(from))} -> {stringr::str_to_title(dplyr::last(to))}"
      ),
      adventure_start_date = min(start_date),
      adventure_end_date = max(start_date),
      days = dplyr::n_distinct(start_date),
      activities = dplyr::n_distinct(activity_id),
      overall_miles = sum(
        overall_distance_miles[!duplicated(activity_id)],
        na.rm = TRUE
      ),
      coastal_miles = sum(coastal_distance_miles, na.rm = TRUE),
      overall_elevation_metres = sum(
        overall_elevation_metres[!duplicated(activity_id)],
        na.rm = TRUE
      ),
      coastal_elevation_metres = sum(coastal_elevation_metres, na.rm = TRUE),
      riders = format_rider_list(riders),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(coastal_miles)) |>
    dplyr::rename(
      start_date = adventure_start_date,
      end_date = adventure_end_date
    ) |>
    dplyr::select(-adventure_id)
}
