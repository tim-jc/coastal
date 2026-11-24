source(
  file.path(
    coastal_project_root,
    "scripts",
    "reconcile_measurement_ordering.R"
  ),
  local = TRUE
)

run_test_reconciliation <- function(activities = make_test_activities(), streams = make_test_streams()) {
  reconcile_measurement_ordering(
    activities,
    make_test_activity_records(),
    streams
  )
}

test_that("reconciliation reproduces legacy and corrected clockwise ascent", {
  reconciliation <- run_test_reconciliation()
  section <- reconciliation$sections

  expect_equal(section$legacy_ascent_metres, 2)
  expect_equal(section$corrected_ascent_metres, 5)
  expect_equal(section$ascent_difference_metres, 3)
  expect_identical(section$change_classification, "DIRECTION_REVERSAL_ONLY")
})

test_that("anticlockwise measurements remain in recorded order", {
  activities <- make_test_activities()
  activities$ride_direction <- "acw"
  section <- run_test_reconciliation(activities)$sections

  expect_equal(section$legacy_ascent_metres, 5)
  expect_equal(section$corrected_ascent_metres, 5)
  expect_equal(section$ascent_difference_metres, 0)
  expect_identical(section$change_classification, "NO_MEASUREMENT_CHANGE")
})

test_that("simple reversal preserves distance", {
  section <- run_test_reconciliation()$sections

  expect_equal(
    section$legacy_distance_miles,
    section$corrected_distance_miles,
    tolerance = 1e-12
  )
  expect_equal(section$distance_difference_miles, 0, tolerance = 1e-12)
})

test_that("duplicate and non-monotonic timestamps are diagnosed", {
  streams <- make_test_streams()[c(3, 1, 2), ]
  streams$time_seconds <- c(10, 10, 5)
  section <- run_test_reconciliation(streams = streams)$sections

  expect_gt(section$duplicate_timestamp_rows, 0L)
  expect_gt(section$nonmonotonic_timestamp_steps, 0L)
  expect_identical(section$change_classification, "TIMESTAMP_ORDER_EFFECT")
  expect_equal(section$corrected_ascent_metres, 5)
})

test_that("reconciliation preserves inclusive curated boundaries", {
  activities <- make_test_activities()
  activities$ride_start_sample_index <- 1L
  activities$ride_end_sample_index <- 2L
  section <- run_test_reconciliation(activities)$sections

  expect_equal(section$selected_observations, 2L)
  expect_equal(section$ride_start_sample_index, 1L)
  expect_equal(section$ride_end_sample_index, 2L)
})

test_that("corrected ascent propagates through reconciliation outputs", {
  reconciliation <- run_test_reconciliation()

  expect_equal(reconciliation$riders$corrected_coastal_elevation_metres, c(5, 5))
  expect_equal(reconciliation$adventures$corrected_coastal_elevation_metres, 5)
  expect_equal(reconciliation$annual$corrected_coastal_ascent_metres, 5)
  expect_equal(reconciliation$biggest_climbs$corrected_ascent_metres, 5)
  expect_equal(reconciliation$xp$corrected_total_ascent_metres, c(5, 5))
})
