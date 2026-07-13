draw_xp_plot <- function(rides_index) {
  mile_equivalent_climb <- min(rides_index$dist_per_elev)

  rider_xp <- rides_index %>%
    dplyr::select(riders, distance_miles, elevation_metres) %>%
    dplyr::mutate(rider = stringr::str_split(riders, "\\|")) %>%
    tidyr::unnest(cols = "rider") %>%
    dplyr::mutate(rider = stringr::str_remove_all(rider, "\\(|\\)")) %>%
    dplyr::group_by(rider) %>%
    dplyr::summarise(
      total_dist = sum(distance_miles),
      total_elev = sum(elevation_metres),
      dist_xp = total_dist * xp_unit,
      elev_xp = total_elev * mile_equivalent_climb * xp_unit,
      total_xp = dist_xp + elev_xp
    ) %>%
    crossing(xp_levels) %>%
    filter(total_xp <= next_xp, total_xp > xp) %>%
    dplyr::mutate(
      xp_window = next_xp - xp,
      percent_level_complete = (total_xp - xp) / xp_window,
      text_label = str_glue(
        "Level {xp_level} ({floor(total_xp)} XP)
                                 {round(total_dist,1)} miles ridden
                                 {round(total_elev,1)} metres climbed"
      ),
      rider = factor(rider),
      rider = fct_reorder(rider, total_xp),
      img_path = str_c(docs_folder_path, "images/rider_maps/", rider, ".png"),
      img_path_b64 = map_chr(img_path, ~ base64enc::dataURI(file = .x))
    )

  xp_plot <- rider_xp %>%
    ggplot(aes(
      x = rider,
      y = percent_level_complete,
      text = text_label,
      customdata = img_path_b64
    )) +
    geom_segment(
      aes(x = rider, y = 0, xend = rider, yend = 1),
      size = 4,
      colour = "grey85"
    ) +
    geom_point(aes(x = rider, y = 1), size = 3.5, colour = "grey85") +
    geom_point(aes(x = rider, y = 0), size = 3.5, colour = phiets_red) +
    geom_segment(
      aes(x = rider, y = 0, xend = rider, yend = percent_level_complete),
      size = 4,
      colour = phiets_red
    ) +
    geom_point(size = 10, colour = phiets_navy) +
    geom_point(size = 7, colour = phiets_red) +
    geom_text(aes(label = xp_level)) +
    geom_text(
      aes(y = 1.25, label = str_glue("{floor(total_xp)} XP")),
      hjust = 0,
      colour = phiets_navy
    ) +
    coord_flip() +
    scale_y_continuous(lim = c(0, 1.5)) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(colour = phiets_navy),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(x = "", y = "")

  xp_plot <- ggplotly(xp_plot, tooltip = "text")

  xp_plot <- xp_plot %>%
    htmlwidgets::onRender(
      "
    function(el, x) {
      // when hovering over an element, do something
      el.on('plotly_hover', function(d) {
            // extract image location
        image_location = d.points[0].customdata;

        // define image to be shown
        var img = {
          // location of image
          source: image_location,
          // position of image overlap on plot
          x: 0.8,
          y: 0.5,
          sizex: 0.5,
          sizey: 0.5,
          xref: 'paper',
          yref: 'paper',
          border: '5px solid #555'
        };

        // show image
        Plotly.relayout(el.id, {
            images: [img]
        });
      })
          // remove image when unhovering
      .on('plotly_unhover', function(d){
            // show image and annotation
        Plotly.relayout(el.id, {
            images: null
        });
          })
        }"
    )

  return(xp_plot)
}

draw_miles_by_hour_plot <- function(full_dataset) {
  mbh_plot <- full_dataset %>%
    dplyr::mutate(
      hour_of_day = hour(time_of_day),
      hour_text = if_else(
        hour_of_day < 10,
        str_c("0", hour_of_day),
        as.character(hour_of_day)
      )
    ) %>%
    dplyr::group_by(hour_of_day, hour_text) %>%
    dplyr::summarise(
      dist_ridden_in_hr_mi = sum(dist_since_prev, na.rm = T) * metres_to_miles
    ) %>%
    dplyr::mutate(
      text_label = str_glue(
        "From: {hour_text}00 to {hour_text}59
                                 Miles ridden: {round(dist_ridden_in_hr_mi, digits = 1)}"
      )
    ) %>%
    ggplot(aes(x = hour_of_day, y = dist_ridden_in_hr_mi, text = text_label)) +
    geom_col(fill = phiets_red, colour = phiets_red, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1, 24, 1)) +
    labs(x = "\nHour of day", y = "Distance ridden /miles\n") +
    theme_minimal() +
    theme(
      axis.text = element_text(colour = phiets_navy),
      axis.title = element_text(colour = phiets_navy),
      panel.grid = element_blank()
    )

  mbh_plot <- ggplotly(mbh_plot, tooltip = "text")

  return(mbh_plot)
}

draw_miles_climb_plot <- function(rides_index) {
  dist_climb_yr <- rides_index %>%
    dplyr::group_by(yr) %>%
    dplyr::summarise(
      tot_miles = sum(distance_miles) %>% round(0),
      tot_elev = sum(elevation_metres) %>% round(0),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      yr = as.character(yr),
      elevation_scaled = tot_elev / 10
    ) %>%
    tidyr::pivot_longer(
      c(tot_miles, elevation_scaled),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric_label = dplyr::case_when(
        metric == "tot_miles" ~ "Coastal miles",
        metric == "elevation_scaled" ~ "Elevation (10m units)"
      ),
      text_label = dplyr::case_when(
        metric == "tot_miles" ~ str_glue("{yr}\nCoastal miles: {value}"),
        metric == "elevation_scaled" ~ str_glue("{yr}\nElevation: {tot_elev}m")
      )
    )

  mc_plot <- dist_climb_yr %>%
    ggplot(aes(
      x = yr,
      y = value,
      fill = metric_label,
      colour = metric_label,
      text = text_label
    )) +
    geom_col(position = "dodge", alpha = 0.25) +
    scale_fill_manual(values = c(phiets_red, phiets_navy), name = NULL) +
    scale_colour_manual(values = c(phiets_red, phiets_navy), name = NULL) +
    labs(x = "", y = "", fill = NULL, colour = NULL) +
    theme_minimal() +
    theme(
      axis.text = element_text(colour = phiets_navy),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

  mc_plot <- ggplotly(mc_plot, tooltip = "text") |>
    layout(legend = list(title = list(text = "")))

  return(mc_plot)
}
