get_coord_valuebox <- function(pos_needed, position_extremities) {
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

  link_str <- str_glue("https://www.google.com/maps/place/{positions$lat}N+{if_else(positions$lng>0,str_c(positions$lng,\"E\"),str_c(0 - positions$lng,\"W\"))}")
  vb <- valueBox(positions$city_name, icon = icon_str, color = "#EDF0F1", href = link_str)
  return(vb)
}
