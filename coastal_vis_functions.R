# coastal vis functions / values

# Define values -----------------------------------------------------------

# Source local DB config
source("config.R")

# Master table of activities. New activities to be added here, in geographical order
coastal_activities <- tribble(
  ~strava_id, ~from, ~to, ~ride_direction, ~riders, ~ride_start_time, ~ride_end_time,
  7081454837, "harrapool", "strathcarron", "cw", "TC|SB|WR", 0, 10442,
  7076591578, "acharacle", "harrapool", "cw", "TC|SB|WR", 0, 26453,
  7072272567, "oban", "acharacle", "cw", "TC|SB|WR", 0, 34095,
  7066002617, "tarbert", "oban", "cw", "TC|SB|WR", 244, 33371,
  7060605792, "whiting bay", "tarbert", "cw", "TC|SB|WR", 0, 32870,
  7055062883, "ardrossan", "whiting bay", "cw", "TC|SB|WR", 7920, 16788,
  7753830579, "ayr", "glasgow", "cw", "TC|SB|WR|ML", 0, 24570,
  7749140338, "newton stewart", "ayr", "cw", "TC|SB|WR|ML", 120, 36170,
  7742970954, "carlisle", "newton stewart", "cw", "TC|SB|WR|ML", 476, 30851,
  6193006840, "seascale", "carlisle", "cw", "TC|SB|WR", 0, 26200,
  6188924719, "lancaster", "seascale", "cw", "TC|SB|WR", 40, 42273,
  6184233328, "chester", "lancaster", "cw", "TC|SB|WR", 3959, 43595,
  # Wales north
  8992413973, "newport", "machynlleth", "cw", "TC|WR|TS|ML", 832, 32960, 
  8985824154, "tenby", "newport", "cw", "TC|SB|WR|TS|ML", 0, 36655, 
  8978887674, "swansea", "tenby", "cw", "TC|SB|WR|TS|ML", 321, 33120,
  8972919601, "bristol", "swansea", "cw", "TC|SB|WR|TS|ML", 2189, 33945,
  5836688186, "washford", "bristol", "cw", "TC|SB|DA|TS|WR", 0, 27308,
  5831004889, "tintagel", "washford", "cw", "TC|SB|DA|TS|WR", 0, 48232,
  5824943588, "penzance", "tintagel", "cw", "TC|SB|DA|TS|WR", 0, 39186,
  5560406484, "penzance", "looe", "acw", "TC|SB|DA", 0, 51100,
  5564763338, "looe", "exmouth", "acw", "TC|SB|DA", 69, 44658,
  5575822827, "exmouth", "bournemouth", "acw", "TC|SB|DA", 0, 45040,
  8848979854, "havant", "bournemouth", "cw", "TC|SB|ML", 0, 34603,
  # Havant -> Bognor
  7403080498, "cowes", "cowes", "acw", "TC|SB",  0, 18211,
  1250873973, "folkestone", "bognor", "cw", "TC|SB",  0, 41420,
  1250104735, "culmers", "folkestone", "cw", "TC|SB", 16545, 40297,
  7709542211, "rochester", "culmers", "cw", "TC", 7920, 27166,
  5906287061, "rochford", "rochester", "cw", "TC|SB", 3902, 42125, 
  9641255722, "maldon", "rochford", "cw", "TC|WR|TS|SR", 64, 11243,
  9634786328, "aldeburgh", "maldon", "cw", "TC|WR|TS|SR", 3753, 41694,
  9628132541, "southwold", "aldeburgh", "cw", "TC|WR|TS|SR", 5551, 17544,
  753905298, "hunstanton", "southwold", "cw", "TC|AH" , 0, 35015,
  870993393, "boston", "hunstanton", "cw", "TC", 16250, 28975,
  4049860168, "boston", "hull", "acw", "TC|SB|DA", 205, 33323,
  4055608848, "hull", "staithes", "acw", "TC|SB|DA", 0, 35523,
  4058882682, "staithes", "newcastle", "acw", "TC|SB|DA", 0, 21970
  # Newcastle -> Inverness
  # NC500
  ) %>% 
  mutate(ride_name = str_glue("{str_to_title(from)} -> {str_to_title(to)}"), 
         riders_pretty = str_replace_all(riders, "\\|", ", "))

coastal_ids <- coastal_activities$strava_id

xp_unit <- 15

xp_levels <- tibble(xp = c(0, 1000, 2000, 3000, 4000, 5000, 7000, 10000, 13000, 16000, 19000,23000,28000,33000,38000,44000,50000,
                           56000,62000,70000,78000,88000,94000,100000,110000,121000,130000,140000,150000,170000,180000,190000,200000,220000,
                           230000,250000,260000,280000,290000,310000,330000,340000,360000,380000,400000,420000,440000,460000,480000,500000)
) %>% 
  mutate(xp_level = seq_along(xp),
         next_xp = lead(xp))

riders <- coastal_activities %>% mutate(riders = str_split(riders,"\\|")) %>% unnest(riders) %>% pull(riders) %>% unique()

phiets_navy <- "#0C2340"
phiets_red <- "#D50032"

section_start_icon <- makeAwesomeIcon(icon = "fa-play", library = "fa", markerColor = "white", iconColor = phiets_navy)
photo_icon <- makeAwesomeIcon(icon = "fa-camera", library = "fa", markerColor = "white", iconColor = phiets_red)

#github docs folder file path
docs_folder_path <- "https://raw.githubusercontent.com/tim-jc/coastal/master/docs/"

# Functions ---------------------------------------------------------------

get_coastal_rides <- function() {
  
  ride_streams <- tbl(con, "streams") %>% filter(strava_id %in% coastal_ids) %>% collect()
  
  ride_streams <- ride_streams %>% 
    inner_join(coastal_activities, by = "strava_id") %>% 
    filter(time >= ride_start_time,
           time <= ride_end_time) 
  
  return(ride_streams)
  
}

load_gps_data <- function() {
  
  # Connect to SQLite DB, pull down geocode data and activity list
  # data from the activity list table for coastal rides to go here
  geocodes <- tbl(con, "geocodes") %>% collect()
  
  activity_list <- tbl(con, "activities") %>% select(strava_id, ride_start, strava_link) %>% filter(strava_id %in% coastal_ids) %>% collect()
  
  # Arrange dataframe
  ride_streams <- get_coastal_rides() %>%  
    inner_join(activity_list, by = "strava_id") %>%
    mutate(sort_time = if_else(ride_direction == "cw", -time, time),
           ride_name = factor(ride_name, levels = coastal_activities$ride_name, ordered = T),
           ride_start = as.POSIXct(ride_start),
           time_of_day = ride_start + seconds(time),
           yr = lubridate::year(ride_start),
           marker_popup = str_c(ride_name, "<br>",
                                format(ride_start, "%d-%b-%y"), "<br>",
                                riders_pretty, "<br>",
                                "<a href=", strava_link, " target=\"_blank\">Strava")) %>% 
    arrange(ride_name, sort_time) %>% 
    left_join(geocodes, by = c("lng", "lat")) %>% 
    mutate(postcode = str_extract(location_string, "[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}"),
           town = str_remove(location_string, ", [A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}.*") %>% str_extract("([^,]+$)") %>% str_trim(),
    ) %>% 
    left_join(read_csv("csv/open_postcode_elevation.csv"), by = "postcode") %>% 
    mutate(point_id = seq(1,nrow(.),1))
  
  ride_streams <- ride_streams %>% 
    group_by(strava_id) %>% 
    mutate(prev_lng = lag(lng),
           prev_lat = lag(lat),
           prev_alt = lag(altitude),
           longlat1 = map2(lng, lat, function(x,y) c(x,y)),
           longlat2 = map2(prev_lng, prev_lat, function(x,y) c(x,y)),
           dist_since_prev = map2(longlat1, longlat2, function(x,y) geosphere::distm(x,y)) %>% unlist(),
           climb_since_prev = if_else(altitude > prev_alt, altitude - prev_alt, 0)) %>% 
    select(-matches("^longlat")) %>% 
    ungroup()
  
  return(ride_streams)
  
}

create_summary <- function() {
  
  df <- full_dataset %>% 
    group_by(ride_name, ride_start, yr, riders, strava_id, strava_link, ride_direction, marker_popup) %>% 
    summarise(start_time = min(time),
              finish_time = max(time),
              start_lon = lng[which(time == start_time)],
              start_lat = lat[which(time == start_time)],
              distance_miles = sum(dist_since_prev, na.rm = T) * 0.0006213,
              elevation_metres = sum(climb_since_prev, na.rm = T),
              dist_per_elev = distance_miles / elevation_metres) %>% 
    distinct() %>% 
    ungroup() %>% 
    arrange(ride_start) %>% 
    mutate(start_date = as.Date(ride_start),
           is_latest_ride = start_date == max(start_date),
           ride_prev_day = start_date == lag(start_date) + days(1),
           ride_next_day = start_date == lead(start_date) - days(1),
           is_new_adventure = case_when(!ride_prev_day | is.na(ride_prev_day) ~ T)) %>% 
    replace_na(list(is_new_adventure = F)) %>% 
    mutate(adventure_id = cumsum(is_new_adventure),
           is_latest_adventure = adventure_id == adventure_id[which(is_latest_ride)]) %>% 
    select(-c(ride_prev_day, ride_next_day, is_new_adventure))
  
  return(df)
  
}

draw_map <- function(map_type) {
  
  map <- leaflet() %>% 
    addTiles('https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png',
             attribution = paste(
               '&copy; <a href="https://openstreetmap.org">OpenStreetMap</a> contributors',
               '&copy; <a href="https://cartodb.com/attributions">CartoDB</a>'
             )) %>% 
    addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                       autoCenter = TRUE, maxZoom = 60, 
                                       setView = TRUE)) 
  
  if(map_type == "all") {
    
    ride_names <- full_dataset$ride_name %>% unique()
    
    images <- image_metadata
    
  }
  
  if(map_type == "latest") {
    
    ride_names <- rides_index %>% filter(is_latest_adventure) %>% pull(ride_name)
    
    images <- image_metadata %>% filter(image_date %in% (rides_index %>% filter(is_latest_adventure) %>% pull(start_date)))
    
  }
  
  for(i in ride_names) {
    
    map <- map %>% add_track(position_tbl = full_dataset %>% filter(ride_name == i), track_colour = phiets_navy)
    
  }
  
  map <- map %>% 
    addAwesomeMarkers(data = rides_index %>% filter(ride_name %in% ride_names), lng = ~start_lon, lat = ~start_lat, popup = ~marker_popup, label = ~ride_name, icon = section_start_icon, clusterOptions = markerClusterOptions(), group = "Ride start points") %>% 
    addAwesomeMarkers(data = images %>% filter(), lng = ~GPSLongitude, lat = ~GPSLatitude, popup = ~marker_popup, icon = photo_icon, clusterOptions = markerClusterOptions(), group = "Photos") %>% 
    addLayersControl(overlayGroups = c("Photos", "Ride start points"),
                     options = layersControlOptions(collapsed = F))
  
  return(map)
  
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
           rider = fct_reorder(rider, total_xp),
           img_path = str_c(docs_folder_path,rider,".png"), # path to hover image of each riders trace
           img_path_b64 = map_chr(img_path, ~base64enc::dataURI(file = .x))) # image path needs to be base64 encoded
  
  xp_plot <- rider_xp %>% 
    ggplot(aes(x = rider, y = percent_level_complete, text = text_label, customdata = img_path_b64)) +
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
  
  xp_plot <- xp_plot %>% htmlwidgets::onRender("
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
        }")
  
  return(xp_plot)
  
}

draw_miles_by_hour_plot <- function() {
  
  mbh_plot <- full_dataset %>% 
    mutate(hour_of_day = hour(time_of_day),
           hour_text = if_else(hour_of_day < 10, str_c("0",hour_of_day), as.character(hour_of_day))) %>% 
    group_by(hour_of_day, hour_text) %>% 
    summarise(dist_ridden_in_hr_mi = sum(dist_since_prev, na.rm = T) * 0.0006213) %>% 
    mutate(text_label = str_glue("From: {hour_text}00 to {hour_text}59
                                 Miles ridden: {round(dist_ridden_in_hr_mi, digits = 1)}")) %>% 
    ggplot(aes(x = hour_of_day, y = dist_ridden_in_hr_mi, text = text_label)) +
    geom_col(fill = phiets_red, colour = phiets_red, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1,24,1)) +
    labs(x = "\nHour of day",
         y = "Distance ridden /miles\n") +
    theme_minimal() +
    theme(axis.text = element_text(colour = phiets_navy),
          axis.title = element_text(colour = phiets_navy),
          panel.grid = element_blank()) 
  
  mbh_plot <- ggplotly(mbh_plot, tooltip = "text")
  
  return(mbh_plot)
  
}

draw_miles_climb_plot <- function() {
  
  dist_climb_yr <- rides_index %>% 
    group_by(yr) %>% 
    summarise(tot_miles = sum(distance_miles) %>% round(0),
              tot_elev = sum(elevation_metres) %>% round(0)) %>% 
    mutate(yr = as.character(yr),
           adjusted_elev = -tot_elev/10) %>% 
    pivot_longer(-c(yr, tot_elev))
  
  mc_plot <- dist_climb_yr %>% 
    ggplot(aes(x = yr, y = value, colour = name)) +
    geom_segment(aes(xend = yr, yend = 0)) +
    geom_text(y = 0, aes(label = yr), colour = phiets_navy, nudge_x = 0.25) +
    geom_text(data = dist_climb_yr %>% filter(name == "adjusted_elev"), aes(label = str_c(tot_elev, "m")), nudge_y = -150) +
    geom_text(data = dist_climb_yr %>% filter(name == "tot_miles"), aes(label = str_c(value, "mi")), nudge_y = 150) +
    geom_point(size = 3) +
    coord_flip() +
    scale_colour_manual(values = c(phiets_navy,phiets_red)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")
  
  mc_plot <- ggplotly(mc_plot, tooltip = "text")
  
  return(mc_plot)
  
}

get_image_metadata <- function() {
  
  exifr::read_exif("docs/images",
                   args = c("-FileName","-GPSLatitude", "-GPSLongitude", "-DateTimeOriginal", "-ImageDescription", "-Description", "-Caption-Abstract"),
                   recursive = T) %>% 
    mutate(image_date = str_sub(DateTimeOriginal, 1, 10) %>% ymd(),
           image_source = str_c(docs_folder_path,"images/",FileName),
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
    df <- full_dataset %>% filter(lng == max(lng))
    icon_str <- "fa-arrow-right"
  }
  
  if(pos_needed == "W") {
    df <- full_dataset %>% filter(lng == min(lng))
    icon_str <- "fa-arrow-left"
  }
  
  link_str <- str_glue("https://www.google.com/maps/place/{df$lat}N+{if_else(df$lng>0,str_c(df$lng,\"E\"),str_c(0 - df$lng,\"W\"))}")
  
  valueBox(df$town, icon = icon_str, color = "#EDF0F1", href = link_str)
}

export_rider_maps <- function(rider) {
  
  uk_outline_map <-  map_data(map = "worldHires", region = c("UK", "Isle of Man", "Isle of Wight", "Wales:Anglesey"), xlim=c(-11,3), ylim=c(49.9,58.5))
  
  rider_dataset <- full_dataset %>% 
    filter(str_detect(riders, rider)) %>% 
    select(strava_id, lat, lng) %>% 
    mutate(lat = round(lat, 2),
           lng = round(lng, 2)) %>% 
    distinct()
  
  ggplot() + 
    geom_polygon(data = uk_outline_map, aes(x = long, y = lat, group = group), fill = "#D3D3D3") +
    geom_point(data = rider_dataset, aes(x = lng, y = lat),  colour = phiets_navy, size = 0.005, alpha = 0.9) +
    coord_map(xlim = c(-8,1.5), ylim = c(50, 59)) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "#FFFFFF"),
          plot.background = element_rect(fill = "#FFFFFF", colour = NA))
  
  ggsave(str_c("docs/",rider,".png"), device = "png", width = 1000, height = 1500, units = "px")
  
}