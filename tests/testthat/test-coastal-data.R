test_that("coastal rides are cropped without re-querying", {
  activities <- make_test_activities()
  activities$ride_start_sample_index <- 2L
  activities$ride_end_sample_index <- 2L

  result <- build_coastal_rides(make_test_streams(), activities)

  expect_equal(result$time_seconds, 10)
  expect_equal(result$coastal_segment_id, 1L)
})

test_that("sample-index crop boundaries are inclusive", {
  activities <- make_test_activities()
  activities$ride_start_sample_index <- 1L
  activities$ride_end_sample_index <- 2L

  result <- build_coastal_rides(make_test_streams(), activities)

  expect_equal(result$sample_index, 1:2)
})

test_that("physical measurements use recorded sample order", {
  result <- enrich_activity_streams(
    build_coastal_rides(make_test_streams(), make_test_activities()),
    make_test_activity_records(),
    make_test_activities(),
    coastal = TRUE
  )

  expect_equal(result$sample_index, 3:1)
  expect_equal(sum(result$climb_since_prev, na.rm = TRUE), 5)
  expect_equal(
    result$climb_since_prev[match(2L, result$sample_index)],
    5
  )
})

test_that("geographic reversal does not reverse physical measurements", {
  clockwise <- make_test_activities()
  anticlockwise <- clockwise
  anticlockwise$ride_direction <- "acw"

  clockwise_result <- enrich_activity_streams(
    build_coastal_rides(make_test_streams(), clockwise),
    make_test_activity_records(),
    clockwise,
    coastal = TRUE
  )
  anticlockwise_result <- enrich_activity_streams(
    build_coastal_rides(make_test_streams(), anticlockwise),
    make_test_activity_records(),
    anticlockwise,
    coastal = TRUE
  )

  expect_equal(clockwise_result$sample_index, 3:1)
  expect_equal(anticlockwise_result$sample_index, 1:3)
  expect_equal(
    sum(clockwise_result$climb_since_prev, na.rm = TRUE),
    sum(anticlockwise_result$climb_since_prev, na.rm = TRUE)
  )
  expect_equal(
    sum(clockwise_result$dist_since_prev, na.rm = TRUE),
    sum(anticlockwise_result$dist_since_prev, na.rm = TRUE)
  )
})

test_that("duplicate and non-monotonic timestamps do not determine ordering", {
  streams <- make_test_streams()[c(3, 1, 2), ]
  streams$time_seconds <- c(10, 10, 5)

  result <- enrich_activity_streams(
    build_coastal_rides(streams, make_test_activities()),
    make_test_activity_records(),
    make_test_activities(),
    coastal = TRUE
  )

  expect_equal(result$sample_index, 3:1)
  expect_equal(sum(result$climb_since_prev, na.rm = TRUE), 5)
})

test_that("corrected climb reaches ride and adventure summaries", {
  activities <- make_test_activities()
  coastal_streams <- enrich_activity_streams(
    build_coastal_rides(make_test_streams(), activities),
    make_test_activity_records(),
    activities,
    coastal = TRUE
  )
  activity_streams <- enrich_activity_streams(
    build_activity_rides(make_test_streams(), activities),
    make_test_activity_records(),
    activities,
    coastal = FALSE
  )
  rides_index <- create_summary(
    coastal_streams,
    activity_streams,
    make_test_activity_records(),
    activities$activity_id
  )

  expect_equal(rides_index$coastal_elevation_metres, 5)
  expect_equal(rides_index$overall_elevation_metres, 5)
  expect_equal(
    format_rider_leaderboard(rides_index)$coastal_elevation_metres,
    c(5, 5)
  )
  expect_equal(
    summarise_adventure(rides_index)$coastal_elevation_metres,
    5
  )
})

test_that("load_coastal_data calls each data-source loader once", {
  calls <- new.env(parent = emptyenv())
  calls$activities <- 0L
  calls$streams <- 0L

  data_source <- list(
    load_activities = function(activity_ids) {
      calls$activities <- calls$activities + 1L
      make_test_activity_records()
    },
    load_streams = function(activity_ids) {
      calls$streams <- calls$streams + 1L
      make_test_streams()
    }
  )
  empty_ferries <- dplyr::tibble(
    ferry = character(),
    activity_id = bit64::integer64(),
    lat = double(),
    lng = double()
  )

  result <- load_coastal_data(
    activities = make_test_activities(),
    ferries_data = empty_ferries,
    include_images = FALSE,
    include_position_extremities = FALSE,
    data_source = data_source
  )

  expect_equal(calls$activities, 1L)
  expect_equal(calls$streams, 1L)
  expect_equal(nrow(result$coastal_streams), 3L)
  expect_equal(nrow(result$rides_index), 1L)
})

test_that("database validation requires a real connection", {
  expect_error(
    load_coastal_data(data_source = list(), validate_db = TRUE),
    "database connection is required"
  )
})
