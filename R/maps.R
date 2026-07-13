draw_map <- function(
  map_type,
  coastal_streams,
  rides_index,
  image_metadata,
  ferries_data = ferries,
  activity_streams = NULL
) {
  map <- leaflet() %>%
    addTiles(
      'https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png',
      attribution = paste(
        '&copy; <a href="https://openstreetmap.org">OpenStreetMap</a> contributors',
        '&copy; <a href="https://cartodb.com/attributions">CartoDB</a>'
      )
    ) %>%
    addControlGPS(
      options = gpsOptions(
        position = "topleft",
        activate = TRUE,
        autoCenter = TRUE,
        maxZoom = 60,
        setView = TRUE
      )
    )

  if (map_type == "all") {
    ride_names <- coastal_streams$ride_name %>% unique()
    images <- image_metadata
    ferry_markers <- ferries_data
  }

  if (map_type == "latest") {
    ride_names <- rides_index$ride_name[rides_index$is_latest_adventure]
    latest_activity_ids <- as.character(
      rides_index$activity_id[rides_index$is_latest_adventure]
    )
    images <- image_metadata %>%
      filter(
        image_date %in% rides_index$start_date[rides_index$is_latest_adventure]
      )
    ferry_markers <- ferries_data %>%
      filter(
        as.character(activity_id) %in%
          latest_activity_ids
      )
  }

  has_activity_context <- map_type == "latest" && !is.null(activity_streams)

  if (has_activity_context) {
    for (latest_activity_id in latest_activity_ids) {
      activity_track_data <- activity_streams %>%
        filter(as.character(activity_id) == latest_activity_id)

      map <- map %>%
        leaflet::addPolylines(
          data = activity_track_data,
          lng = ~lng,
          lat = ~lat,
          opacity = 0.65,
          weight = 2,
          color = phiets_navy,
          dashArray = "7, 7",
          group = "Whole activity route"
        )
    }
  }

  for (i in ride_names) {
    track_data <- coastal_streams %>% filter(ride_name == i)
    track_group <- if_else(
      map_type == "latest",
      "Coastal section",
      "Coastal route"
    )

    map <- map %>%
      leaflet::addPolylines(
        data = track_data,
        lng = ~lng,
        lat = ~lat,
        opacity = 0.85,
        weight = if_else(map_type == "latest", 2, 1.5),
        color = phiets_navy,
        group = track_group
      )
  }

  map <- map %>%
    addAwesomeMarkers(
      data = rides_index %>% filter(ride_name %in% ride_names),
      lng = ~start_lon,
      lat = ~start_lat,
      popup = ~marker_popup,
      label = ~ride_name,
      icon = section_start_icon,
      clusterOptions = markerClusterOptions(),
      group = "Ride start points"
    ) %>%
    addAwesomeMarkers(
      data = images,
      lng = ~GPSLongitude,
      lat = ~GPSLatitude,
      popup = ~marker_popup,
      icon = photo_icon,
      clusterOptions = markerClusterOptions(),
      group = "Photos"
    )

  if (nrow(ferry_markers) > 0) {
    overlay_groups <- c(
      "Coastal route",
      "Photos",
      "Ride start points",
      "Ferries"
    )
    if (map_type == "latest") {
      overlay_groups <- c(
        "Coastal section",
        "Photos",
        "Ride start points",
        "Ferries"
      )
    }
    if (has_activity_context) {
      overlay_groups <- c("Whole activity route", overlay_groups)
    }

    map <- map %>%
      addAwesomeMarkers(
        data = ferry_markers,
        lng = ~lng,
        lat = ~lat,
        popup = ~ferry,
        label = ~ferry,
        icon = ferry_icon,
        clusterOptions = markerClusterOptions(),
        group = "Ferries"
      ) %>%
      addLayersControl(
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = F)
      )
  } else {
    overlay_groups <- c("Coastal route", "Photos", "Ride start points")
    if (map_type == "latest") {
      overlay_groups <- c("Coastal section", "Photos", "Ride start points")
    }
    if (has_activity_context) {
      overlay_groups <- c("Whole activity route", overlay_groups)
    }

    map <- map %>%
      addLayersControl(
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = F)
      )
  }

  if (map_type == "latest") {
    bounds_data <- if (has_activity_context) {
      activity_streams %>%
        filter(as.character(activity_id) %in% latest_activity_ids)
    } else {
      coastal_streams %>% filter(ride_name %in% ride_names)
    }

    if (nrow(bounds_data) > 0) {
      map <- map %>%
        leaflet::fitBounds(
          lng1 = min(bounds_data$lng, na.rm = TRUE),
          lat1 = min(bounds_data$lat, na.rm = TRUE),
          lng2 = max(bounds_data$lng, na.rm = TRUE),
          lat2 = max(bounds_data$lat, na.rm = TRUE)
        )
    }
  }

  return(map)
}

export_rider_maps <- function(rider, coastal_streams) {
  output_dir <- "docs/images/rider_maps"

  uk_outline_map <- map_data(
    map = "worldHires",
    region = c("UK", "Isle of Man", "Isle of Wight", "Wales:Anglesey"),
    xlim = c(-11, 3),
    ylim = c(49.9, 58.5)
  )

  rider_dataset <- coastal_streams %>%
    filter(str_detect(riders, rider)) %>%
    dplyr::select(activity_id, lat, lng) %>%
    dplyr::mutate(lat = round(lat, 2), lng = round(lng, 2)) %>%
    distinct()

  ggplot() +
    geom_polygon(
      data = uk_outline_map,
      aes(x = long, y = lat, group = group),
      fill = "#D3D3D3"
    ) +
    geom_point(
      data = rider_dataset,
      aes(x = lng, y = lat),
      colour = phiets_navy,
      size = 0.005,
      alpha = 0.9
    ) +
    coord_map(xlim = c(-8, 1.5), ylim = c(50, 59)) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF", colour = NA)
    )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    file.path(output_dir, str_c(rider, ".png")),
    device = "png",
    width = 1000,
    height = 1500,
    units = "px"
  )
}
