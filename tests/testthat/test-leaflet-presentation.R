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

test_that("Leaflet layer inputs and cluster sizes use scoped Field Notebook colours", {
  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  markercluster_js <- paste(
    readLines(
      file.path(
        coastal_project_root,
        "index_files",
        "leaflet-markercluster-1.0.5",
        "leaflet.markercluster.js"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(coastal_css, "accent-color: var(--ptg-sage)", fixed = TRUE)
  expect_match(coastal_css, ".marker-cluster-small", fixed = TRUE)
  expect_match(coastal_css, ".marker-cluster-medium", fixed = TRUE)
  expect_match(coastal_css, ".marker-cluster-large", fixed = TRUE)
  expect_match(coastal_css, "box-shadow: inset 0 0 0 3px var(--ptg-sage)", fixed = TRUE)
  expect_match(coastal_css, "box-shadow: inset 0 0 0 3px var(--ptg-copper)", fixed = TRUE)
  expect_match(coastal_css, "box-shadow: inset 0 0 0 3px var(--ptg-ink)", fixed = TRUE)

  expect_match(markercluster_js, '10>t?"small":100>t?"medium":"large"', fixed = TRUE)
  expect_match(markercluster_js, "maxClusterRadius:80", fixed = TRUE)
})
