# coastal vis functions / values

# Define values -----------------------------------------------------------

# Source local DB config
source("config.R")

# Table of ferries and locations
ferries <- dplyr::tribble(
  ~ferry, ~activity_id, ~latitude, ~longitude,
  "Tilbury -> Gravesend", 5906287061, 51.448386, 0.371552,
  "Sandbanks", 5575822827, 50.681738, -1.949315,
  "Dartmouth -> Kingswear", 5564763338, 50.348742, -3.575669,
  "Salcombe", 5564763338, 50.235731, -3.765182,
  "Torpoint -> Plymouth", 5564763338, 50.375810, -4.188294,
  "Fowey -> Polruan", 5560406484, 50.331343, -4.635739,
  "Falmouth -> St Mawes", 5560406484, 50.154367, -5.036136,
  "Padstow", 5824943588, 50.543438, -4.930992,
  "Ardrossan -> Brodick", 7055062883, 55.607871, -4.969103,
  "Lochranza -> Claonaig", 7060605792, 55.733197, -5.339097,
  "Oban -> Craignure", 7072272567, 56.440869, -5.583040,
  "Tobermory -> Kilchoan", 7072272567, 56.658632, -6.066250,
  "Mallaig -> Armadale", 7076591578, 57.039403, -5.852108,
  "East Cowes -> West Cowes", 7403080498, 50.757738, -1.291578,
  "Hayling Island", 8848979854, 50.796715, -1.027668,
  "Portsmouth -> Gosport", 8848979854, 50.795424, -1.112938,
  "Warsash -> Hamble", 8848979854, 50.855839, -1.310089,
  "Mudeford", 8848979854, 50.721583, -1.743585,
  "Ferryside -> Llansteffan", 8978887674, 51.770486, -4.377032,
  "Butley", 9634786328, 52.080330, 1.490391,
  "Bawdsey -> Felixstowe", 9634786328, 51.990132, 1.392646,
  "Felixstowe -> Harwich", 9634786328, 51.945829, 1.304259,
  "Brightlingsea -> Mersea Island", 9634786328, 51.800945, 1.011537,
  "Burnham YH -> Essex Marina", 9641255722, 51.623854, 0.803652,
  "Itchenor", 15095197221, 50.808844, -0.865772, 
  "Ardmhòr -> Eriskay", 15720148185, 57.045633, -7.361339,
  "Berneray -> Leverburgh", 15720148185, 57.710011, -7.045988
) %>% dplyr::mutate(ferry = str_glue("{ferry} Ferry"))

# Master table of activities. New activities to be added here, in geographical order
# Riders in brackets did similar rides and are awarded XP etc
coastal_activities <- dplyr::tribble(
  ~activity_id, ~from, ~to, ~ride_direction, ~riders, ~ride_start_time, ~ride_end_time,
  15731925300, "tarbert", "stornoway", "cw", "TC|TS", 65, 30683,
  15720148185, "ardmhòr", "tarbert", "cw", "TC|TS", 1260, 45165,
  15709407056, "vatersay", "eolaigearraidh", "cw", "TC|TS", 2310, 8000,
  7081454837, "harrapool", "strathcarron", "cw", "TC|SB|WR", 0, 10442,
  7076591578, "acharacle", "harrapool", "cw", "TC|SB|WR", 0, 26453,
  7072272567, "kilchoan", "acharacle", "cw", "TC|SB|WR", 27732, 34095,
  7072272567, "craignure", "tobermory", "cw", "TC|SB|WR", 5760, 23100,
  7066002617, "tarbert", "oban", "cw", "TC|SB|WR", 244, 33371,
  7060605792, "whiting bay", "tarbert", "cw", "TC|SB|WR", 0, 32870,
  7055062883, "brodick", "whiting bay", "cw", "TC|SB|WR", 14800, 16788,
  7753830579, "ayr", "glasgow", "cw", "TC|SB|WR|ML", 0, 24570,
  7749140338, "newton stewart", "ayr", "cw", "TC|SB|WR|ML", 120, 36170,
  7742970954, "carlisle", "newton stewart", "cw", "TC|SB|WR|ML", 476, 30851,
  6193006840, "seascale", "carlisle", "cw", "TC|SB|WR", 0, 26200,
  6188924719, "lancaster", "seascale", "cw", "TC|SB|WR", 40, 42273,
  6184233328, "chester", "lancaster", "cw", "TC|SB|WR", 3959, 43595,
  # <activity_id>, "kinmel bay", "chester", "cw", "TC|SB", <start_time>, <end_time>,
  11622261830, "glan-yr-afon", "kinmel bay", "cw", "TC|SB|WR|TS|ML", 220, 18414,
  11614847759, "nefyn", "glan-yr-afon", "cw", "TC|SB|WR|TS|ML", 21, 50465,
  11605843542, "machynlleth", "nefyn", "cw", "TC|SB|WR|TS|ML", 2410, 36293,
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
  8848979854, "havant", "bournemouth", "cw", "TC|SB|ML", 75, 34603,
  15095197221, "havant", "ferring", "cw", "TC|TS", 152, 17993,
  7403080498, "cowes", "cowes", "acw", "TC|SB",  0, 18211,
  1250873973, "folkestone", "ferring", "cw", "TC|SB",  0, 37692,
  1250104735, "culmers", "folkestone", "cw", "TC|SB", 16545, 40297,
  7709542211, "rochester", "culmers", "cw", "TC", 7920, 27166,
  5906287061, "rochford", "rochester", "cw", "TC|SB", 3902, 42125, 
  9641255722, "maldon", "rochford", "cw", "TC|WR|TS|SR", 64, 11243,
  9634786328, "aldeburgh", "maldon", "cw", "TC|WR|TS|SR", 3753, 41694,
  9628132541, "southwold", "aldeburgh", "cw", "TC|WR|TS|SR", 5551, 17544,
  753905298, "hunstanton", "southwold", "cw", "TC|AH", 382, 35015,
  14294527868, "boston", "hunstanton", "cw", "TC|WR|TS", 3095, 35725,
  18241293474, "boston", "mablethorpe", "acw", "TC|WR|TS|ML|(SB)|(DA)", 11235, 28775,
  18255965742, "mablethorpe", "withernsea", "acw", "TC|WR|TS|ML", 1925, 41715,
  18264839802, "withernsea", "aldbrough", "acw", "TC|WR|TS|ML", 185, 3385,
  4055608848, "aldbrough", "staithes", "acw", "TC|SB|DA", 2320, 35523,
  4058882682, "staithes", "newcastle", "acw", "TC|SB|DA", 0, 21970
  # Newcastle -> Inverness
  # NC500
) %>% 
  dplyr::mutate(ride_name = str_glue("{str_to_title(from)} -> {str_to_title(to)}"), 
                riders_pretty = str_replace_all(riders, "\\|", ", "),
                activity_id = bit64::as.integer64(activity_id))

coastal_ids <- coastal_activities$activity_id

xp_unit <- 15

xp_levels <- dplyr::tibble(xp = c(0, 1000, 2000, 3000, 4000, 5000, 7000, 10000, 13000, 16000, 19000,23000,28000,33000,38000,44000,50000,
                                  56000,62000,70000,78000,88000,94000,100000,110000,121000,130000,140000,150000,170000,180000,190000,200000,220000,
                                  230000,250000,260000,280000,290000,310000,330000,340000,360000,380000,400000,420000,440000,460000,480000,500000)
) %>% 
  dplyr::mutate(xp_level = seq_along(xp),
                next_xp = lead(xp))

riders <- coastal_activities %>% dplyr::mutate(riders = str_split(riders,"\\|")) %>% tidyr::unnest(riders) %>% dplyr::pull(riders) %>% stringr::str_remove_all("\\(|\\)") %>% unique()

phiets_navy <- "#0C2340"
phiets_red <- "#D50032"

section_start_icon <- leaflet::makeAwesomeIcon(icon = "fa-play", library = "fa", markerColor = "white", iconColor = phiets_navy)
photo_icon <- leaflet::makeAwesomeIcon(icon = "fa-camera", library = "fa", markerColor = "white", iconColor = phiets_red)
ferry_icon <- leaflet::makeAwesomeIcon(icon = "fa-ship", library = "fa", markerColor = "white", iconColor = phiets_navy)

#github docs folder file path
docs_folder_path <- "https://raw.githubusercontent.com/tim-jc/coastal/master/docs/"

# factor to convert metres to miles
metres_to_miles <- 0.0006213

# Functions ---------------------------------------------------------------

get_coastal_rides <- function() {
  
  ride_streams <- tbl(con, "activity_streams") %>% filter(activity_id %in% coastal_ids) %>% collect()
  
  ride_streams <- ride_streams %>% 
    dplyr::inner_join(coastal_activities, by = "activity_id") %>% 
    dplyr::filter(time >= ride_start_time,
                  time <= ride_end_time) 
  
  return(ride_streams)
  
}

load_gps_data <- function() {
  
  # Connect to DB, pull down geocode data and activity list
  # data from the activity list table for coastal rides to go here
  # geocodes <- tbl(con, "geocodes") %>% collect()
  
  activity_list <- tbl(con, "activities") %>%
    dplyr::mutate(strava_link = paste0("https://www.strava.com/activities/19045845136/segments/",activity_id)) %>% 
    dplyr::select(activity_id, start_date_local, strava_link) %>% filter(activity_id %in% coastal_ids) %>% collect()
  
  # Arrange dataframe
  ride_streams <- get_coastal_rides() %>%  
    inner_join(activity_list, by = "activity_id") %>%
    dplyr::mutate(sort_time = if_else(ride_direction == "cw", -time, time),
                  ride_name = factor(ride_name, levels = coastal_activities$ride_name, ordered = T),
                  start_date_local = as.POSIXct(start_date_local),
                  time_of_day = start_date_local + seconds(time),
                  yr = lubridate::year(start_date_local),
                  marker_popup = str_c(ride_name, "<br>",
                                       format(start_date_local, "%d-%b-%y"), "<br>",
                                       riders_pretty, "<br>",
                                       "<a href=", strava_link, " target=\"_blank\">Strava")) %>% 
    arrange(ride_name, sort_time) %>% 
    # left_join(geocodes, by = c("longitude", "latitude")) %>% 
    dplyr::mutate(postcode = str_extract(location_string, "[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}"),
                  town = str_remove(location_string, ", [A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}.*") %>% str_extract("([^,]+$)") %>% str_trim(),
    ) %>% 
    left_join(read_csv("csv/open_postcode_elevation.csv"), by = "postcode") %>% 
    dplyr::mutate(point_id = seq(1,nrow(.),1))
  
  ride_streams <- ride_streams %>% 
    dplyr::group_by(activity_id) %>% 
    dplyr::mutate(prev_lng = lag(longitude),
                  prev_lat = lag(latitude),
                  prev_alt = lag(altitude),
                  longlat1 = map2(longitude, latitude, function(x,y) c(x,y)),
                  longlat2 = map2(prev_lng, prev_lat, function(x,y) c(x,y)),
                  dist_since_prev = map2(longlat1, longlat2, function(x,y) geosphere::distm(x,y)) %>% unlist(),
                  climb_since_prev = if_else(altitude > prev_alt, altitude - prev_alt, 0)) %>% 
    dplyr::select(-matches("^longlat")) %>% 
    ungroup()
  
  return(ride_streams)
  
}

create_summary <- function() {
  
  df <- full_dataset %>% 
    dplyr::group_by(ride_name, start_date_local, yr, riders, activity_id, strava_link, ride_direction, marker_popup) %>% 
    dplyr::summarise(start_time = min(time),
                     finish_time = max(time),
                     start_lon = longitude[which(time == start_time)],
                     start_lat = latitude[which(time == start_time)],
                     distance_miles = sum(dist_since_prev, na.rm = T) * metres_to_miles,
                     elevation_metres = sum(climb_since_prev, na.rm = T),
                     dist_per_elev = distance_miles / elevation_metres) %>% 
    distinct() %>% 
    ungroup() %>% 
    arrange(start_date_local) %>% 
    dplyr::mutate(start_date = as.Date(start_date_local),
                  is_latest_ride = start_date == max(start_date),
                  ride_prev_day = start_date == lag(start_date) + days(1),
                  ride_next_day = start_date == lead(start_date) - days(1),
                  is_new_adventure = case_when(!ride_prev_day | is.na(ride_prev_day) ~ T)) %>% 
    replace_na(list(is_new_adventure = F)) %>% 
    dplyr::mutate(adventure_id = cumsum(is_new_adventure),
                  is_latest_adventure = adventure_id == adventure_id[which(is_latest_ride)]) %>% 
    dplyr::select(-c(ride_prev_day, ride_next_day, is_new_adventure))
  
  activity_data <- tbl(con, "activities") %>%
    dplyr::mutate(distance_whole_ride_miles = distance * metres_to_miles) %>%
    dplyr::select(activity_id, distance_whole_ride_miles) %>%
    dplyr::filter(activity_id %in% coastal_ids) %>%
    collect()
  
  df <- df %>% 
    left_join(activity_data) %>% 
    dplyr::mutate(coastal_percentage = distance_miles / distance_whole_ride_miles,
                  coastal_percentage = if_else(coastal_percentage > 1, 1, coastal_percentage))
  
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
    
    ride_names <- rides_index$ride_name[rides_index$is_latest_adventure]
    
    images <- image_metadata %>% filter(image_date %in% rides_index$start_date[rides_index$is_latest_adventure])
    
    ferries <- ferries %>% filter(activity_id %in% rides_index$activity_id[rides_index$is_latest_adventure])
    
    
  }
  
  for(i in ride_names) {
    
    map <- map %>% add_track(position_tbl = full_dataset %>% filter(ride_name == i), track_colour = phiets_navy)
    
  }
  
  map <- map %>% 
    addAwesomeMarkers(data = rides_index %>% filter(ride_name %in% ride_names), lng = ~start_lon, lat = ~start_lat, popup = ~marker_popup, label = ~ride_name, icon = section_start_icon, clusterOptions = markerClusterOptions(), group = "Ride start points") %>% 
    addAwesomeMarkers(data = images, lng = ~GPSLongitude, lat = ~GPSLatitude, popup = ~marker_popup, icon = photo_icon, clusterOptions = markerClusterOptions(), group = "Photos")
  
  # handle an adventure with no ferries
  if(nrow(ferries) > 0) {
    map <- map %>% 
      addAwesomeMarkers(data = ferries, lng = ~lng, lat = ~latitude, popup = ~ferry, label = ~ferry, icon = ferry_icon, clusterOptions = markerClusterOptions(), group = "Ferries") %>% 
      addLayersControl(overlayGroups = c("Photos", "Ride start points", "Ferries"),
                       options = layersControlOptions(collapsed = F))
  } else {
    map <- map %>% 
      addLayersControl(overlayGroups = c("Photos", "Ride start points"),
                       options = layersControlOptions(collapsed = F))
  }
  
  
  return(map)
  
}

draw_xp_plot <- function() {
  
  mile_equivalent_climb <- min(rides_index$dist_per_elev)
  
  rider_xp <- rides_index %>% 
    dplyr::select(riders, distance_miles, elevation_metres) %>% 
    dplyr::mutate(rider = stringr::str_split(riders, "\\|")) %>% 
    tidyr::unnest(cols = "rider") %>% 
    dplyr::mutate(rider = stringr::str_remove_all(rider, "\\(|\\)")) %>% 
    dplyr::group_by(rider) %>% 
    dplyr::summarise(total_dist = sum(distance_miles),
                     total_elev = sum(elevation_metres),
                     dist_xp = total_dist * xp_unit,
                     elev_xp = total_elev * mile_equivalent_climb * xp_unit,
                     total_xp = dist_xp + elev_xp) %>% 
    crossing(xp_levels) %>% 
    filter(total_xp <= next_xp,
           total_xp > xp) %>% 
    dplyr::mutate(xp_window = next_xp - xp,
                  percent_level_complete = (total_xp - xp) / xp_window,
                  text_label = str_glue("Level {xp_level} ({floor(total_xp)} XP)
                                 {round(total_dist,1)} miles ridden
                                 {round(total_elev,1)} metres climbed"),
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
    dplyr::mutate(hour_of_day = hour(time_of_day),
                  hour_text = if_else(hour_of_day < 10, str_c("0",hour_of_day), as.character(hour_of_day))) %>% 
    dplyr::group_by(hour_of_day, hour_text) %>% 
    dplyr::summarise(dist_ridden_in_hr_mi = sum(dist_since_prev, na.rm = T) * metres_to_miles) %>% 
    dplyr::mutate(text_label = str_glue("From: {hour_text}00 to {hour_text}59
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
    dplyr::group_by(yr) %>% 
    dplyr::summarise(tot_miles = sum(distance_miles) %>% round(0),
                     tot_elev = sum(elevation_metres) %>% round(0)) %>% 
    dplyr::mutate(yr = as.character(yr),
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
    dplyr::mutate(GPSLongitude = as.numeric(GPSLongitude),
                  image_date = str_sub(DateTimeOriginal, 1, 10) %>% ymd(),
                  image_source = str_c(docs_folder_path,"images/",FileName),
                  image_description = coalesce(ImageDescription, Description, `Caption-Abstract`),
                  marker_popup = str_c("<a href=\"", image_source, "\" target=\"_blank\">",
                                       "<img src=\"",image_source, "\" style=\"width:230px;height:300px;object-fit:cover;\"><br>",  
                                       image_description)
    )
  
}

get_coord_valuebox <- function(pos_needed) {
  
  positions <- position_extremities %>% 
    filter(extremity == pos_needed) %>% 
    dplyr::mutate(city_name = case_when(!is.na(village) ~ village,
                                        !is.na(town) ~ town)) %>% 
    slice_head(n = 1)
  
  if(pos_needed == "N") {
    icon_str <- "fa-arrow-up"
  }
  
  if(pos_needed == "S") {
    icon_str <- "fa-arrow-down"
  }
  
  if(pos_needed == "E") {
    icon_str <- "fa-arrow-right"
  }
  
  if(pos_needed == "W") {
    icon_str <- "fa-arrow-left"
  }
  
  link_str <- str_glue("https://www.google.com/maps/place/{positions$latitude}N+{if_else(positions$longitude>0,str_c(positions$longitude,\"E\"),str_c(0 - positions$longitude,\"W\"))}")
  vb <- valueBox(positions$city_name, icon = icon_str, color = "#EDF0F1", href = link_str)
  return(vb)
  
}

export_rider_maps <- function(rider) {
  
  uk_outline_map <-  map_data(map = "worldHires", region = c("UK", "Isle of Man", "Isle of Wight", "Wales:Anglesey"), xlim=c(-11,3), ylim=c(49.9,58.5))
  
  rider_dataset <- full_dataset %>% 
    filter(str_detect(riders, rider)) %>% 
    dplyr::select(activity_id, latitude, longitude) %>% 
    dplyr::mutate(latitude = round(latitude, 2),
                  longitude = round(longitude, 2)) %>% 
    distinct()
  
  ggplot() + 
    geom_polygon(data = uk_outline_map, aes(x = long, y = latitude, group = group), fill = "#D3D3D3") +
    geom_point(data = rider_dataset, aes(x = longitude, y = latitude),  colour = phiets_navy, size = 0.005, alpha = 0.9) +
    coord_map(xlim = c(-8,1.5), ylim = c(50, 59)) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "#FFFFFF"),
          plot.background = element_rect(fill = "#FFFFFF", colour = NA))
  
  ggsave(str_c("docs/",rider,".png"), device = "png", width = 1000, height = 1500, units = "px")
  
}