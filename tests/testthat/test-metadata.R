test_that("rider lists are normalized, deduplicated, and sorted", {
  expect_identical(format_rider_list("SB|TC|(SB)|WR"), "TC, SB, WR")
})

test_that("empty rider lists return missing text", {
  expect_true(is.na(format_rider_list(NA_character_)))
})

test_that("map POI icons use distinct Field Notebook colours", {
  expect_identical(
    coastal_marker_palette,
    c(photo = "#9B582E", ferry = "#667C73")
  )
  expect_identical(photo_icon$icon, "fa-camera")
  expect_identical(photo_icon$markerColor, "white")
  expect_identical(photo_icon$iconColor, coastal_marker_palette[["photo"]])
  expect_identical(ferry_icon$icon, "fa-ship")
  expect_identical(ferry_icon$markerColor, "white")
  expect_identical(ferry_icon$iconColor, coastal_marker_palette[["ferry"]])
  expect_false(identical(photo_icon$iconColor, ferry_icon$iconColor))
})
