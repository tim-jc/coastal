test_that("valid coastal metadata is accepted", {
  activities <- make_test_activities()
  expect_invisible(validate_coastal_activities(activities, check_db = FALSE))
})

test_that("invalid crop windows are rejected", {
  activities <- make_test_activities()
  activities$ride_start_time <- 30

  expect_error(
    validate_coastal_activities(activities, check_db = FALSE),
    "invalid crop windows"
  )
})

test_that("ferries must reference a known activity", {
  ferries_data <- dplyr::tibble(
    ferry = "Test Ferry",
    activity_id = bit64::as.integer64(99),
    lat = 51,
    lng = -1
  )

  expect_error(
    validate_ferries(ferries_data, make_test_activities()),
    "unknown activities"
  )
})
