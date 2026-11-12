make_chart_palette_rides <- function() {
  dplyr::tibble(
    adventure_id = c(1L, 1L, 2L, 2L),
    start_date = as.Date(c("2023-04-01", "2023-05-02", "2024-04-03", "2024-06-04")),
    start_date_local = as.POSIXct(
      c("2023-04-01", "2023-05-02", "2024-04-03", "2024-06-04"),
      tz = "UTC"
    ),
    ride_name = c("A -> B", "B -> C", "C -> D", "D -> E"),
    riders = c("TC|SB", "TC", "SB|WR", "TC|WR"),
    coastal_distance_miles = c(20, 30, 40, 50),
    coastal_elevation_metres = c(200, 300, 400, 500),
    yr = c(2023L, 2023L, 2024L, 2024L)
  )
}

make_chart_palette_streams <- function() {
  dplyr::tibble(
    time_of_day = as.POSIXct(
      c("2024-01-01 08:00:00", "2024-01-01 09:00:00", "2024-01-01 10:00:00"),
      tz = "UTC"
    ),
    dist_since_prev = c(1000, 2000, 3000)
  )
}

test_that("the approved Coastal analytical palette is explicit", {
  expect_identical(
    coastal_chart_palette,
    c(
      distance = "#9B582E",
      climb = "#172630",
      distribution = "#667C73"
    )
  )
  expect_identical(
    unname(coastal_chart_fill(coastal_chart_palette)),
    c("#9B582E47", "#17263047", "#667C7347")
  )
})

test_that("cumulative progress uses ink line and terracotta markers", {
  built <- plotly::plotly_build(
    draw_cumulative_progress_plot(make_chart_palette_rides())
  )

  expect_identical(built$x$data[[1]]$line$color, "rgba(23,38,48,1)")
  expect_identical(built$x$data[[2]]$marker$color, "rgba(155,88,46,1)")
})

test_that("single-series bar charts use opaque outlines and 28 percent fills", {
  charts <- list(
    contributors = draw_rider_contribution_plot(make_chart_palette_rides()),
    seasonality = draw_monthly_activity_plot(make_chart_palette_rides()),
    hourly = draw_miles_by_hour_plot(make_chart_palette_streams())
  )
  expected <- list(
    contributors = c("rgba(102,124,115,", "rgba(102,124,115,1)"),
    seasonality = c("rgba(155,88,46,", "rgba(155,88,46,1)"),
    hourly = c("rgba(102,124,115,", "rgba(102,124,115,1)")
  )

  for (name in names(charts)) {
    trace <- plotly::plotly_build(charts[[name]])$x$data[[1]]
    expect_true(startsWith(trace$marker$color, expected[[name]][[1]]))
    expect_equal(
      as.numeric(sub(".*,(.*)\\)$", "\\1", trace$marker$color)),
      .28,
      tolerance = .01
    )
    expect_identical(trace$marker$line$color, expected[[name]][[2]])
  }
})

test_that("annual distance and climb retain trace order with approved colours", {
  built <- plotly::plotly_build(draw_miles_climb_plot(make_chart_palette_rides()))

  expect_identical(
    vapply(built$x$data, `[[`, character(1), "name"),
    c("Coastal climb (10m units)", "Coastal miles")
  )
  fill_colours <- vapply(
    built$x$data,
    function(trace) trace$marker$color,
    character(1)
  )
  expect_true(startsWith(fill_colours[[1]], "rgba(23,38,48,"))
  expect_true(startsWith(fill_colours[[2]], "rgba(155,88,46,"))
  expect_equal(
    as.numeric(sub(".*,(.*)\\)$", "\\1", fill_colours)),
    c(.28, .28),
    tolerance = .01
  )
  expect_identical(
    vapply(built$x$data, function(trace) trace$marker$line$color, character(1)),
    c("rgba(23,38,48,1)", "rgba(155,88,46,1)")
  )
})
