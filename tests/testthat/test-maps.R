test_that("missing CARTO key gives an actionable, non-secret error", {
  withr::local_envvar(CARTO_BASEMAP_API_KEY = NA)

  expect_error(
    carto_basemap_api_key(),
    "CARTO_BASEMAP_API_KEY",
    fixed = TRUE
  )
})

test_that("CARTO tile URL preserves Voyager and includes the configured key", {
  withr::local_envvar(CARTO_BASEMAP_API_KEY = "test-carto-key")

  expect_identical(
    carto_basemap_tile_url(),
    paste0(
      "https://{s}.basemaps.cartocdn.com/",
      "rastertiles/voyager_labels_under/{z}/{x}/{y}.png?key=test-carto-key"
    )
  )
})

test_that("CARTO configuration does not print the configured key", {
  withr::local_envvar(CARTO_BASEMAP_API_KEY = "test-carto-key")

  output <- capture.output(api_key <- carto_basemap_api_key())

  expect_identical(output, character())
  expect_identical(api_key, "test-carto-key")
})
