source("R/load.R")

main <- function(
  output_dir = file.path(coastal_project_root, "outputs", "visualisations"),
  show_preview = interactive()
) {
  load_coastal_packages(c(coastal_packages, "extrafont", "cowplot", "DBI"))
  suppressWarnings(try(extrafont::loadfonts(quiet = TRUE), silent = TRUE))

  con <- connect_coastal_database()
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)

  coastal_data <- load_coastal_data(
    con,
    include_images = FALSE,
    include_position_extremities = FALSE
  )
  coastal_streams <- coastal_data$coastal_streams
  rides_index <- coastal_data$rides_index
  riders <- coastal_data$riders
  uk_outline_map <- ggplot2::map_data(
    map = "worldHires",
    region = c("UK", "Isle of Man", "Isle of Wight", "Wales:Anglesey"),
    xlim = c(-11, 3),
    ylim = c(49.9, 58.5)
  )

  if (show_preview) {
    print(
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        stravR::add_track(coastal_streams)
    )
  }

  output_files <- purrr::map_chr(riders, function(rider) {
    plot <- create_rider_visualisation(
      rider,
      coastal_streams,
      rides_index,
      uk_outline_map
    )
    output_file <- export_rider_visualisation(plot, rider, output_dir)
    if (is.null(output_file)) NA_character_ else output_file
  })
  output_files <- output_files[!is.na(output_files)]

  message("Exported ", length(output_files), " rider visualisation(s) to ", output_dir)
  invisible(output_files)
}

if (sys.nframe() == 0) {
  main()
}
