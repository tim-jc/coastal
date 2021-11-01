# coastal vis functions / values

# Establish Strava connection ---------------------------------------------

app_name <- 'coastal_datavis' # chosen by user
app_client_id  <- '53709' # an integer, assigned by Strava
app_secret <- '4f5778009ce59f130203ef927694029d518ea3ff' # an alphanumeric secret, assigned by Strava

# create the authentication token - only do this once to cache the token in the working directory
# stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all", cache = T))

# once token created / cached in working directoty, use the below to load it
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])

# Define values -----------------------------------------------------------

# Order rides
ride_levels <-  c("seascale_carlisle","lancaster_seascale","chester_lancaster","washford_bristol","tintagel_washford","penzance_tintagel","penzance_looe","looe_exmouth","exmouth_bournemouth",
                  "folkestone_bognor", "rochester_folkestone", "battlesbridge_rochester", "maldon_battlesbridge", "maldon_clacton", "clacton_manningtree",
                  "woodbridge_manningtree", "orford_woodbridge", "snape_orford","southwold_snape", "hunstanton_southwold",
                  "boston_hunstaton", "boston_hull", "hull_staithes", "staithes_newcastle")

xp_unit <- 15

xp_levels <- tibble(xp = c(0, 1000, 2000, 3000, 4000, 5000, 7000, 10000, 13000, 16000, 19000,23000,28000,33000,38000,44000,50000,
                           56000,262000,70000,78000,88000,94000,100000,110000,121000,130000,140000,150000,170000,180000,190000,200000,220000,
                           230000,250000,260000,280000,290000,310000,330000,340000,360000,380000,400000,420000,440000,460000,480000,500000)
                    ) %>% 
  mutate(xp_level = seq_along(xp),
         next_xp = lead(xp))

phiets_navy <- "#0C2340"
phiets_red <- "#D50032"

section_start_icon <- makeAwesomeIcon(icon = "fa-play", library = "fa", markerColor = "white", iconColor = phiets_navy)
photo_icon <- makeAwesomeIcon(icon = "fa-camera", library = "fa", markerColor = "white", iconColor = phiets_red)

# Functions ---------------------------------------------------------------

gpx_to_df <- function(file_path) {
  
  # read gpx file and convert to df
  df <- plotKML::readGPX(file_path)
  df <- df$tracks[[1]][[1]] %>% as.tibble()
  df <- df %>% mutate(ele = as.numeric(ele))
  
  # calculate distance in miles and climb in meters between each point
  df <- df %>% 
    mutate(prev_lon = lag(lon),
           prev_lat = lag(lat),
           prev_ele = lag(ele),
           longlat1 = map2(lon, lat, function(x,y) c(x,y)),
           longlat2 = map2(prev_lon, prev_lat, function(x,y) c(x,y)),
           dist_since_prev = map2(longlat1, longlat2, function(x,y) geosphere::distm(x,y)) %>% unlist(),
           climb_since_prev = if_else(ele > prev_ele, ele - prev_ele, 0),
           file_name = str_remove(file_path, "gps_data/")) %>% 
    select(-matches("^longlat"))
  
}

add_track <- function(leaflet_obj, gpx_df, track_colour = phiets_navy) {
  
  latitude <- gpx_df$lat
  longitude <- gpx_df$lon
  
  leaflet_obj %>% 
    addPolylines(lat = latitude, lng = longitude, opacity = 0.5, weight = 2, color = track_colour)

}

create_summary <- function() {
  
  df <- full_dataset %>% 
    group_by(file_name, ride, riders, strava_link, direction) %>% 
    summarise(start_datetime = min(time),
              finish_datetime = max(time),
              start_lon = lon[which(time == start_datetime)],
              start_lat = lat[which(time == start_datetime)],
              distance_miles = sum(dist_since_prev, na.rm = T) * 0.0006213,
              elevation_metres = sum(climb_since_prev, na.rm = T),
              dist_per_elev = distance_miles / elevation_metres,
              yr = lubridate::year(start_datetime)) %>% 
    ungroup() %>% 
    arrange(start_datetime) %>% 
    mutate(start_date = as.Date(start_datetime),
           is_latest_ride = start_datetime == max(start_datetime),
           ride_prev_day = start_date == lag(start_date) + days(1),
           ride_next_day = start_date == lead(start_date) - days(1),
           is_new_adventure = case_when(!ride_prev_day | is.na(ride_prev_day) ~ T),
           ride_pretty = str_replace(ride, "_", " -> ") %>% str_to_title(),
           riders_pretty = str_replace_all(riders, "\\|", ", "),
           marker_popup = str_c(ride_pretty, "<br>",
                                format(start_datetime, "%d-%b-%y"), "<br>",
                                riders_pretty, "<br>",
                                "<a href=", strava_link, " target=\"_blank\">Strava")) %>% 
    replace_na(list(is_new_adventure = F)) %>% 
    mutate(adventure_id = cumsum(is_new_adventure),
           is_latest_adventure = adventure_id == adventure_id[which(is_latest_ride)]) %>% 
    select(-c(ride_prev_day, ride_next_day, is_new_adventure))
  
  return(df)
  
}

load_gps_data <- function(file_location = NA_character_) {

   full_dataset <- tibble()
    
   if (!is.na(file_location)) {
    
    old_wd <- getwd()
    setwd(file_location)
    
  }
  
  file_names <- list.files()

  for (j in file_names) {

      df_load <- readr::read_csv(j)
      df_load <- df_load %>% select(-extensions)
      
      # Make sure all rides are going in the same direction
      if (df_load$direction == "cw") {
        
        df_load <- df_load %>% arrange(desc(time))
        
      }
      
      full_dataset <- bind_rows(full_dataset,
                                df_load)

  }
  
  if (!is.na(file_location)) {
    setwd(old_wd)
  }
  
  full_dataset <- full_dataset %>% 
    left_join(read_csv("csv/reverse_geocoding.csv"), by = c("lon", "lat")) %>% 
    mutate(postcode = str_extract(location_string, "[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}"),
           town = str_remove(location_string, ", [A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}.*") %>% str_extract("([^,]+$)") %>% str_trim(),
           ride = factor(ride, levels = ride_levels, ordered = T)) %>% 
    arrange(ride) %>% 
    left_join(read_csv("csv/open_postcode_elevation.csv"), by = "postcode") %>% 
    mutate(id = seq(1,nrow(.),1))
  
  # write out bound dataset
  assign("full_dataset", full_dataset, envir = .GlobalEnv)
  
}

draw_xp_plot <- function() {
  
  mile_equivalent_climb <- min(rides_index$dist_per_elev)
  
  rider_xp <- rides_index %>% 
    select(riders, distance_miles, elevation_metres) %>% 
    mutate(rider = str_split(riders, "\\|")) %>% 
    unnest(cols = "rider") %>% 
    group_by(rider) %>% 
    summarise(total_dist = sum(distance_miles),
              total_elev = sum(elevation_metres),
              dist_xp = total_dist * xp_unit,
              elev_xp = total_elev * mile_equivalent_climb * xp_unit,
              total_xp = dist_xp + elev_xp) %>% 
    crossing(xp_levels) %>% 
    filter(total_xp <= next_xp,
           total_xp > xp) %>% 
    mutate(xp_window = next_xp - xp,
           percent_level_complete = (total_xp - xp) / xp_window,
           text_label = str_glue("Level {xp_level} ({floor(total_xp)} XP)"),
           rider = factor(rider),
           rider = fct_reorder(rider, total_xp))
  
  xp_plot <- rider_xp %>% 
    ggplot(aes(x = rider, y = percent_level_complete, text = text_label)) +
    # Background bar
    geom_segment(aes(x = rider, y = 0, xend = rider, yend = 1), size = 4, colour = "grey85") +
    geom_point(aes(x = rider, y = 1), size = 3.5, colour = "grey85") +
    # XP bar
    geom_point(aes(x = rider, y = 0), size = 3.5, colour = phiets_red) +
    geom_segment(aes(x = rider, y = 0, xend = rider, yend = percent_level_complete), size = 4, colour = phiets_red) +
    geom_point(size = 10, colour = phiets_navy) +
    geom_point(size = 7, colour = phiets_red) +
    geom_text(aes(label = xp_level)) +
    geom_text(aes(y = 1.25, label = str_glue("{floor(total_xp)} XP")), hjust = 0, colour = phiets_navy) +
    coord_flip() +
    scale_y_continuous(lim = c(0, 1.5)) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(colour = phiets_navy),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    labs(x = "",
         y = "")
  
  xp_plot <- ggplotly(xp_plot, tooltip = "text")
  
  return(xp_plot)
  
}

draw_time_plot <- function() {
  
  time_plot <- full_dataset %>% 
    mutate(gpx_point_time = update(time, day = 1, month = 1, year = 2020)) %>% 
    ggplot(aes(x = gpx_point_time)) +
    geom_freqpoly(binwidth = 300, colour = phiets_red) + 
    scale_x_datetime(labels = function(x) format(x, "%H:%M"), breaks = "2 hour") +
    labs(x = "",
         y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(colour = phiets_navy),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())
  
  time_plot <- ggplotly(time_plot)
  
  time_plot <- style(time_plot, hoverinfo = "none")
  
  return(time_plot)
  
}

draw_map <- function(map_type) {
  
  map <- leaflet() %>% 
    addTiles('https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png',
             attribution = paste(
               '&copy; <a href="https://openstreetmap.org">OpenStreetMap</a> contributors',
               '&copy; <a href="https://cartodb.com/attributions">CartoDB</a>'
             ))
  
  if(map_type == "all") {
    
    ride_names <- full_dataset$ride %>% unique()
    
    images <- image_metadata
    
  }
  
  if(map_type == "latest") {
    
    ride_names <- rides_index %>% filter(is_latest_adventure) %>% pull(ride)
    
    images <- image_metadata %>% filter(image_date %in% (rides_index %>% filter(is_latest_adventure) %>% pull(start_datetime) %>% as.Date()))
    
  }
  
  for(ride_name in ride_names) {
    
    map <- map %>% add_track(full_dataset %>% filter(ride == ride_name))
    
  }
  
  map <- map %>% 
    addAwesomeMarkers(data = rides_index %>% filter(ride %in% ride_names), lng = ~start_lon, lat = ~start_lat, popup = ~marker_popup, label = ~ride_pretty, icon = section_start_icon, clusterOptions = markerClusterOptions()) %>% 
    addAwesomeMarkers(data = images %>% filter(), lng = ~GPSLongitude, lat = ~GPSLatitude, popup = ~marker_popup, icon = photo_icon, clusterOptions = markerClusterOptions())
  
  return(map)
  
}

get_image_metadata <- function() {
  
  exifr::read_exif("docs/images",
            args = c("-FileName","-GPSLatitude", "-GPSLongitude", "-DateTimeOriginal", "-ImageDescription", "-Description", "-Caption-Abstract"),
            recursive = T) %>% 
    mutate(image_date = str_sub(DateTimeOriginal, 1, 10) %>% ymd(),
           image_source = str_c("https://raw.githubusercontent.com/tim-jc/coastal/master/docs/images/",FileName),
           image_description = coalesce(ImageDescription, Description, `Caption-Abstract`),
           marker_popup = str_c("<a href=\"", image_source, "\" target=\"_blank\">",
                           "<img src=\"",image_source, "\" style=\"width:230px;height:300px;object-fit:cover;\"><br>",  
                           image_description)
    )
  
}

get_coord_valuebox <- function(pos_needed) {
  
  if(pos_needed == "N") {
    df <- full_dataset %>% filter(lat == max(lat))
    icon_str <- "fa-arrow-up"
  }
  
  if(pos_needed == "S") {
    df <- full_dataset %>% filter(lat == min(lat))
    icon_str <- "fa-arrow-down"
  }
  
  if(pos_needed == "E") {
    df <- full_dataset %>% filter(lon == max(lon))
    icon_str <- "fa-arrow-right"
  }
  
  if(pos_needed == "W") {
    df <- full_dataset %>% filter(lon == min(lon))
    icon_str <- "fa-arrow-left"
  }
  
  link_str <- str_glue("https://www.google.com/maps/place/{df$lat}N+{if_else(df$lon>0,str_c(df$lon,\"E\"),str_c(0 - df$lon,\"W\"))}")
  
  valueBox(df$town, icon = icon_str, color = "grey25", href = link_str)
}