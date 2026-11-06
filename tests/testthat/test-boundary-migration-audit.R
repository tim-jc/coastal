source(
  file.path(coastal_project_root, "scripts", "audit_boundary_migration.R"),
  local = TRUE
)

make_audit_section <- function(start_time, end_time) {
  dplyr::tibble(
    activity_id = bit64::as.integer64(1),
    from = "Start",
    to = "Finish",
    ride_start_time = start_time,
    ride_end_time = end_time,
    coastal_segment_id = 1L
  )
}

make_audit_streams <- function(indices, times) {
  dplyr::tibble(
    activity_id = bit64::as.integer64(rep(1, length(indices))),
    sample_index = indices,
    time_seconds = times
  )
}

test_that("duplicated boundary timestamps preserve every selected observation", {
  audit <- audit_boundary_migration(
    make_audit_section(10, 10),
    make_audit_streams(1:5, c(0, 10, 10, 10, 20))
  )

  expect_identical(audit$status, "EXACT_MATCH")
  expect_identical(audit$proposed_start_sample_index, 2L)
  expect_identical(audit$proposed_end_sample_index, 4L)
  expect_identical(audit$timestamp_selected_rows, 3L)
  expect_identical(audit$start_timestamp_match_type, "DUPLICATED")
  expect_true(audit$exact_ordered_sample_index_match)
})

test_that("unmatched timestamp boundaries use the observations actually selected", {
  audit <- audit_boundary_migration(
    make_audit_section(5, 15),
    make_audit_streams(1:3, c(0, 10, 20))
  )

  expect_identical(audit$status, "EXACT_MATCH")
  expect_identical(audit$proposed_start_sample_index, 2L)
  expect_identical(audit$proposed_end_sample_index, 2L)
  expect_identical(audit$start_timestamp_match_type, "UNMATCHED")
  expect_identical(audit$end_timestamp_match_type, "UNMATCHED")
})

test_that("non-monotonic timestamps that create disjoint membership require review", {
  audit <- audit_boundary_migration(
    make_audit_section(10, 20),
    make_audit_streams(1:5, c(0, 10, 30, 20, 40))
  )

  expect_identical(audit$status, "REQUIRES_REVIEW")
  expect_identical(audit$status_reason, "INDEX_INTERVAL_MEMBERSHIP_DIFFERS")
  expect_false(audit$exact_ordered_sample_index_match)
  expect_identical(audit$nonmonotonic_timestamp_steps, 1L)
})

test_that("sample-index gaps require review even when membership matches", {
  audit <- audit_boundary_migration(
    make_audit_section(10, 20),
    make_audit_streams(c(1L, 2L, 4L, 5L), c(0, 10, 20, 30))
  )

  expect_identical(audit$status, "REQUIRES_REVIEW")
  expect_identical(audit$status_reason, "SAMPLE_INDEX_GAPS")
  expect_true(audit$exact_ordered_sample_index_match)
  expect_false(audit$proposed_interval_contiguous)
})

test_that("missing timestamp selections remain unresolved", {
  audit <- audit_boundary_migration(
    make_audit_section(40, 50),
    make_audit_streams(1:3, c(0, 10, 20))
  )

  expect_identical(audit$status, "UNRESOLVED")
  expect_identical(audit$status_reason, "NO_TIMESTAMP_SELECTED_ROWS")
  expect_true(is.na(audit$proposed_start_sample_index))
})

test_that("duplicate canonical sample indices remain unresolved", {
  audit <- audit_boundary_migration(
    make_audit_section(0, 20),
    make_audit_streams(c(1L, 2L, 2L), c(0, 10, 20))
  )

  expect_identical(audit$status, "UNRESOLVED")
  expect_identical(audit$status_reason, "DUPLICATE_SAMPLE_INDEX")
})

test_that("invalid timestamp boundaries remain unresolved without selecting NA rows", {
  audit <- audit_boundary_migration(
    make_audit_section(NA_real_, 20),
    make_audit_streams(1:3, c(0, 10, 20))
  )

  expect_identical(audit$status, "UNRESOLVED")
  expect_identical(audit$status_reason, "INVALID_TIMESTAMP_BOUNDARY")
  expect_identical(audit$timestamp_selected_rows, 0L)
})
