get_position_extremities <- function(coastal_streams) {
  coastal_streams %>%
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
  images_dir <- file.path(coastal_project_root, "docs", "images")
  image_files <- list.files(
    images_dir,
    pattern = "[.](jpe?g|png)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  image_files <- image_files[!grepl("(^|/)rider_maps/", image_files)]

  if (length(image_files) == 0) {
    return(
      tibble::tibble(
        GPSLatitude = numeric(),
        GPSLongitude = numeric(),
        image_date = as.Date(character()),
        image_source = character(),
        image_description = character(),
        marker_popup = character()
      )
    )
  }

  exifr::read_exif(
    image_files,
    args = c(
      "-FileName",
      "-GPSLatitude",
      "-GPSLongitude",
      "-DateTimeOriginal",
      "-ImageDescription",
      "-Description",
      "-Caption-Abstract"
    ),
    recursive = FALSE
  ) %>%
    dplyr::mutate(
      GPSLatitude = as.numeric(GPSLatitude),
      GPSLongitude = as.numeric(GPSLongitude),
      image_date = str_sub(DateTimeOriginal, 1, 10) %>% ymd(),
      image_source = str_c(docs_folder_path, "images/", FileName),
      image_description = coalesce(ImageDescription, Description, `Caption-Abstract`),
      marker_popup = str_c(
        "<a href=\"", image_source, "\" target=\"_blank\">",
        "<img src=\"", image_source, "\" style=\"width:230px;height:300px;object-fit:cover;\"><br>",
        image_description
      )
    ) %>%
    dplyr::filter(!is.na(GPSLatitude), !is.na(GPSLongitude))
}
