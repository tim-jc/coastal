test_that("Leaflet controls use scoped Field Notebook presentation", {
  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    coastal_css,
    "#dashboard-container .leaflet-container .leaflet-bar",
    fixed = TRUE
  )
  expect_match(
    coastal_css,
    "#dashboard-container .leaflet-container .leaflet-control-layers",
    fixed = TRUE
  )
  expect_match(
    coastal_css,
    "#dashboard-container .leaflet-container .leaflet-control-gps",
    fixed = TRUE
  )
  expect_match(
    coastal_css,
    "#dashboard-container .leaflet-container .leaflet-popup-content-wrapper",
    fixed = TRUE
  )
  expect_match(
    coastal_css,
    "#dashboard-container .leaflet-container .leaflet-control-attribution",
    fixed = TRUE
  )
  expect_match(coastal_css, "background: var(--ptg-paper)", fixed = TRUE)
  expect_match(coastal_css, "border: 1px solid var(--ptg-line)", fixed = TRUE)
  expect_match(coastal_css, "outline: 3px solid var(--ptg-focus)", fixed = TRUE)

  expect_false(grepl("^\\.leaflet-", coastal_css))
  expect_false(grepl("overflow-x: hidden", coastal_css, fixed = TRUE))
})

test_that("Leaflet attribution and popup content remain visible", {
  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  maps_source <- paste(
    readLines(file.path(coastal_project_root, "R", "maps.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(coastal_css, "color: var(--ptg-muted)", fixed = TRUE)
  expect_false(grepl("leaflet-control-attribution.*display: none", coastal_css))
  expect_false(grepl("leaflet-control-attribution.*visibility: hidden", coastal_css))
  expect_match(maps_source, "OpenStreetMap", fixed = TRUE)
  expect_match(maps_source, "CartoDB", fixed = TRUE)
  expect_match(maps_source, "popup = ~marker_popup", fixed = TRUE)
  expect_match(maps_source, "popup = ~ferry", fixed = TRUE)
})
