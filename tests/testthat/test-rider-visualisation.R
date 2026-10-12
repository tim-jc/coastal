test_that("rider filtering matches complete pipe-delimited values", {
  rows <- dplyr::tibble(
    riders = c("TC|SB", "(TC)|WR", "TCC|DA", NA_character_),
    value = 1:4
  )

  result <- filter_rider_rows(rows, "TC")

  expect_equal(result$value, c(1L, 2L))
})

test_that("font selection falls back when enumeration fails", {
  broken_provider <- function() stop("font enumeration failed")

  expect_identical(
    available_font_family(c("Avenir", "sans"), broken_provider),
    "sans"
  )
})

test_that("yearly rider summaries tolerate missing metrics", {
  rider_index <- dplyr::tibble(
    yr = c(2025, 2025, 2026),
    coastal_distance_miles = c(10, NA, 20),
    coastal_elevation_metres = c(100, 50, NA)
  )

  result <- summarise_rider_years(rider_index)

  expect_equal(result$total_miles, c(10, 20))
  expect_equal(result$total_climb, c(150, 0))
})

test_that("metric plots reserve space below the units label", {
  summary_data <- dplyr::tibble(
    yr = as.character(2016:2026),
    total_miles = seq(100, 1100, by = 100),
    total_climb = seq(1000, 11000, by = 1000)
  )

  plot <- create_metric_plot(
    summary_data,
    total_miles,
    "miles",
    font_family = "sans",
    symbol_font_family = "sans"
  )
  built <- ggplot2::ggplot_build(plot)
  discrete_range <- built$layout$panel_params[[1]]$y.range

  expect_lt(discrete_range[[1]], 0)
  expect_s3_class(plot, "ggplot")
})

test_that("empty rider data is rejected or skipped clearly", {
  expect_error(
    summarise_rider_years(dplyr::tibble()),
    "no ride rows"
  )

  empty_streams <- dplyr::tibble(riders = character())
  empty_index <- dplyr::tibble(riders = character())
  expect_warning(
    expect_null(create_rider_visualisation(
      "TC",
      empty_streams,
      empty_index,
      dplyr::tibble()
    )),
    "Skipping rider"
  )
})

test_that("visualisation assembly returns an editable ggplot object", {
  streams <- make_test_streams() |>
    dplyr::mutate(riders = "TC|SB")
  rides <- dplyr::tibble(
    riders = "TC|SB",
    yr = 2026,
    coastal_distance_miles = 12,
    coastal_elevation_metres = 345,
    start_date_local = as.POSIXct("2026-06-01 09:00:00", tz = "UTC")
  )
  outline <- dplyr::tibble(
    long = c(-2, 0, 0, -2),
    lat = c(50, 50, 53, 53),
    group = 1L
  )

  result <- create_rider_visualisation(
    "TC",
    streams,
    rides,
    outline,
    font_family = "sans",
    symbol_font_family = "sans"
  )

  expect_s3_class(result, "ggplot")
})

test_that("export writes the explicitly supplied plot to a safe filename", {
  output_dir <- withr::local_tempdir()
  plot <- ggplot2::ggplot(
    dplyr::tibble(x = 1, y = 1),
    ggplot2::aes(x, y)
  ) + ggplot2::geom_point()
  dimensions <- utils::modifyList(
    rider_visualisation_defaults,
    list(a2_width_mm = 20, a2_height_mm = 20, dpi = 72)
  )

  output_file <- export_rider_visualisation(
    plot,
    "TC / test",
    output_dir,
    dimensions
  )

  expect_true(file.exists(output_file))
  expect_identical(basename(output_file), "coastal_vis_TC_test.png")
})
