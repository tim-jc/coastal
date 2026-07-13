get_latest_adventure <- function(rides_index) {
  rides_index |>
    dplyr::filter(is_latest_adventure) |>
    dplyr::arrange(start_date_local)
}

format_adventure_date_range <- function(adventure_rides) {
  adventure_dates <- range(adventure_rides$start_date, na.rm = TRUE)

  if (adventure_dates[[1]] == adventure_dates[[2]]) {
    return(format(adventure_dates[[1]], "%d %b %Y"))
  }

  str_glue(
    "{format(adventure_dates[[1]], '%d %b %Y')} to {format(adventure_dates[[2]], '%d %b %Y')}"
  )
}

format_duration_hours <- function(seconds) {
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)

  str_glue("{hours}h {stringr::str_pad(minutes, 2, pad = '0')}m")
}

summarise_adventure <- function(adventure_rides, adventure_photos = NULL) {
  first_ride <- dplyr::first(adventure_rides$ride_name)
  last_ride <- dplyr::last(adventure_rides$ride_name)
  adventure_title <- if (first_ride == last_ride) {
    first_ride
  } else {
    str_glue("{first_ride} to {last_ride}")
  }

  riders <- format_rider_list(adventure_rides$riders)

  dplyr::tibble(
    title = adventure_title,
    date_range = format_adventure_date_range(adventure_rides),
    days = dplyr::n_distinct(adventure_rides$start_date),
    activities = dplyr::n_distinct(adventure_rides$activity_id),
    distance_miles = sum(adventure_rides$distance_miles, na.rm = TRUE),
    elevation_metres = sum(adventure_rides$elevation_metres, na.rm = TRUE),
    moving_seconds = sum(
      adventure_rides$finish_time - adventure_rides$start_time,
      na.rm = TRUE
    ),
    riders = riders,
    photo_count = if (is.null(adventure_photos)) 0 else nrow(adventure_photos)
  )
}

filter_adventure_images <- function(image_metadata, adventure_rides) {
  if (is.null(image_metadata) || nrow(image_metadata) == 0) {
    return(image_metadata)
  }

  image_metadata |>
    dplyr::filter(image_date %in% adventure_rides$start_date) |>
    dplyr::arrange(image_date, FileName)
}

format_adventure_day_table <- function(adventure_rides) {
  adventure_rides |>
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
      moving_time = format_duration_hours(finish_time - start_time),
      distance_miles = if_else(
        distance_miles > distance_whole_ride_miles,
        distance_whole_ride_miles,
        distance_miles
      )
    ) |>
    dplyr::select(
      ride_date,
      ride_name_link,
      riders = riders_display,
      distance_whole_ride_miles,
      distance_miles,
      elevation_metres,
      moving_time,
      coastal_percentage
    )
}

render_adventure_photo_gallery <- function(adventure_photos, max_images = 18) {
  if (is.null(adventure_photos) || nrow(adventure_photos) == 0) {
    return(htmltools::p("No geotagged photos found for this adventure."))
  }

  photos <- adventure_photos |>
    dplyr::slice_head(n = max_images)

  photo_tags <- purrr::pmap(
    list(photos$image_source, photos$image_description, photos$image_date),
    function(image_source, image_description, image_date) {
      caption <- dplyr::coalesce(image_description, as.character(image_date))

      htmltools::tags$a(
        href = image_source,
        target = "_blank",
        class = "adventure-photo",
        htmltools::tags$img(src = image_source, alt = caption),
        htmltools::tags$span(caption)
      )
    }
  )

  htmltools::tagList(
    htmltools::tags$style(
      "
      .adventure-photo-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
        gap: 10px;
      }
      .adventure-photo {
        color: #0C2340;
        display: block;
        text-decoration: none;
      }
      .adventure-photo img {
        aspect-ratio: 4 / 3;
        display: block;
        object-fit: cover;
        width: 100%;
      }
      .adventure-photo span {
        display: block;
        font-size: 11px;
        line-height: 1.25;
        margin-top: 4px;
      }
      "
    ),
    htmltools::tags$div(class = "adventure-photo-grid", photo_tags)
  )
}
