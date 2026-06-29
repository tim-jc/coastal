get_position_extremities <- function(full_dataset) {
  full_dataset %>%
    dplyr::select(lat, lng) %>%
    dplyr::mutate(extremity = dplyr::case_when(
      lat == max(lat) ~ "N",
      lat == min(lat) ~ "S",
      lng == max(lng) ~ "E",
      lng == min(lng) ~ "W"
    )) %>%
    dplyr::filter(!is.na(extremity)) %>%
    tidygeocoder::reverse_geocode(long = lng, lat = lat, full_results = T)
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
