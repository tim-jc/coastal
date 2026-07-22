test_that("coastal rides are cropped without re-querying", {
  activities <- make_test_activities()
  activities$ride_start_time <- 5
  activities$ride_end_time <- 15

  result <- build_coastal_rides(make_test_streams(), activities)

  expect_equal(result$time_seconds, 10)
  expect_equal(result$coastal_segment_id, 1L)
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
