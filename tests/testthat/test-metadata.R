test_that("rider lists are normalized, deduplicated, and sorted", {
  expect_identical(format_rider_list("SB|TC|(SB)|WR"), "TC, SB, WR")
})

test_that("empty rider lists return missing text", {
  expect_true(is.na(format_rider_list(NA_character_)))
})
