# coastal vis functions / values

# Order rides
ride_levels <-  c("washford_bristol","tintagel_washford","penzance_tintagel","penzance_looe","looe_exmouth","exmouth_bournemouth",
                  "folkestone_bognor", "london_folkestone", "maldon_battlesbridge", "maldon_clacton", "clacton_manningtree",
                  "woodbridge_manningtree", "orford_woodbridge", "snape_orford","southwold_snape", "hunstanton_southwold",
                  "boston_hunstaton", "boston_hull", "hull_staithes", "staithes_newcastle")

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

add_track <- function(leaflet_obj, gpx_df) {
  
  latitude <- gpx_df$lat
  longitude <- gpx_df$lon
  
  leaflet_obj %>% 
    addPolylines(lat = latitude, lng = longitude, opacity = 1, weight = 3, color = "red")

}

create_summary <- function() {
  
  df <- full_dataset %>% 
    group_by(file_name, ride, direction) %>% 
    summarise(start_datetime = min(time),
              finish_datetime = max(time),
              distance_miles = sum(dist_since_prev, na.rm = T) * 0.0006213,
              elevation_metres = sum(climb_since_prev, na.rm = T),
              yr = lubridate::year(start_datetime))
  
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
    left_join(read_csv("reverse_geocoding.csv"), by = c("lon", "lat")) %>% 
    mutate(postcode = str_extract(location_string, "[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}"),
           town = str_remove(location_string, ", [A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}.*") %>% str_extract("([^,]+$)") %>% str_trim(),
           ride = factor(ride, levels = ride_levels, ordered = T)) %>% 
    arrange(ride) %>% 
    left_join(read_csv("open_postcode_elevation.csv"), by = "postcode") %>% 
    mutate(id = seq(1,nrow(.),1))
  
  # write out bound dataset
  assign("full_dataset", full_dataset, envir = .GlobalEnv)
  
}
