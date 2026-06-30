# Coastal metadata and shared constants.

source("data/ferries.R")
source("data/coastal_activities.R")

coastal_ids <- coastal_activities$activity_id

xp_unit <- 15

xp_levels <- dplyr::tibble(
  xp = c(
    0,
    1000,
    2000,
    3000,
    4000,
    5000,
    7000,
    10000,
    13000,
    16000,
    19000,
    23000,
    28000,
    33000,
    38000,
    44000,
    50000,
    56000,
    62000,
    70000,
    78000,
    88000,
    94000,
    100000,
    110000,
    121000,
    130000,
    140000,
    150000,
    170000,
    180000,
    190000,
    200000,
    220000,
    230000,
    250000,
    260000,
    280000,
    290000,
    310000,
    330000,
    340000,
    360000,
    380000,
    400000,
    420000,
    440000,
    460000,
    480000,
    500000
  )
) |>
  dplyr::mutate(xp_level = seq_along(xp), next_xp = lead(xp))

extract_riders <- function(activities = coastal_activities) {
  activities |>
    dplyr::mutate(riders = stringr::str_split(riders, "\\|")) |>
    tidyr::unnest(riders) |>
    dplyr::pull(riders) |>
    stringr::str_remove_all("\\(|\\)") |>
    unique()
}

riders <- extract_riders(coastal_activities)

phiets_navy <- "#0C2340"
phiets_red <- "#D50032"

section_start_icon <- leaflet::makeAwesomeIcon(
  icon = "fa-play",
  library = "fa",
  markerColor = "white",
  iconColor = phiets_navy
)
photo_icon <- leaflet::makeAwesomeIcon(
  icon = "fa-camera",
  library = "fa",
  markerColor = "white",
  iconColor = phiets_red
)
ferry_icon <- leaflet::makeAwesomeIcon(
  icon = "fa-ship",
  library = "fa",
  markerColor = "white",
  iconColor = phiets_navy
)

docs_folder_path <- "https://raw.githubusercontent.com/tim-jc/coastal/master/docs/"

metres_to_miles <- 0.0006213
