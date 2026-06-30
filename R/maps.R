draw_map <- function(
  map_type,
  full_dataset,
  rides_index,
  image_metadata,
  ferries_data = ferries
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
    ride_names <- full_dataset$ride_name %>% unique()
    images <- image_metadata
    ferry_markers <- ferries_data
  }

  if (map_type == "latest") {
    ride_names <- rides_index$ride_name[rides_index$is_latest_adventure]
    images <- image_metadata %>%
      filter(
        image_date %in% rides_index$start_date[rides_index$is_latest_adventure]
      )
    ferry_markers <- ferries_data %>%
      filter(
        activity_id %in%
          rides_index$activity_id[rides_index$is_latest_adventure]
      )
  }

  for (i in ride_names) {
    map <- map %>%
      add_track(
        position_tbl = full_dataset %>% filter(ride_name == i),
        track_colour = phiets_navy
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
        overlayGroups = c("Photos", "Ride start points", "Ferries"),
        options = layersControlOptions(collapsed = F)
      )
  } else {
    map <- map %>%
      addLayersControl(
        overlayGroups = c("Photos", "Ride start points"),
        options = layersControlOptions(collapsed = F)
      )
  }

  return(map)
}

export_rider_maps <- function(rider, full_dataset) {
  uk_outline_map <- map_data(
    map = "worldHires",
    region = c("UK", "Isle of Man", "Isle of Wight", "Wales:Anglesey"),
    xlim = c(-11, 3),
    ylim = c(49.9, 58.5)
  )

  rider_dataset <- full_dataset %>%
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

  ggsave(
    str_c("docs/images/rider_maps/", rider, ".png"),
    device = "png",
    width = 1000,
    height = 1500,
    units = "px"
  )
}
