coastal_packages <- c(
  "tidyverse",
  "leaflet",
  "leaflet.extras",
  "plotly",
  "lubridate",
  "mapdata",
  "stravR"
)

load_coastal_packages <- function(packages = coastal_packages) {
  missing_packages <- packages[!vapply(
    packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )]

  if (length(missing_packages) > 0) {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  suppressPackageStartupMessages(
    invisible(lapply(packages, library, character.only = TRUE))
  )
}
