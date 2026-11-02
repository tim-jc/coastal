test_that("photo marker popups preserve the natural image aspect ratio", {
  popup <- build_image_marker_popup(
    "https://example.com/coast.jpeg",
    "Peel Beach"
  )

  expect_match(popup, "class=\"coastal-photo-popup\"", fixed = TRUE)
  expect_match(popup, "class=\"coastal-photo-caption\"", fixed = TRUE)
  expect_false(grepl("style=", popup, fixed = TRUE))
  expect_false(grepl("width:230px", popup, fixed = TRUE))
  expect_false(grepl("height:300px", popup, fixed = TRUE))
})

test_that("photo marker popups are accessible and safely escaped", {
  popup <- build_image_marker_popup(
    "https://example.com/coast.jpeg?a=1&b=2",
    "Beach <sunset> & sea"
  )

  expect_match(popup, "loading=\"lazy\"", fixed = TRUE)
  expect_match(popup, "decoding=\"async\"", fixed = TRUE)
  expect_match(popup, "rel=\"noopener noreferrer\"", fixed = TRUE)
  expect_match(popup, "Beach &lt;sunset&gt; &amp; sea", fixed = TRUE)
  expect_match(popup, "a=1&amp;b=2", fixed = TRUE)
})

test_that("photo marker popups provide a fallback caption", {
  popup <- build_image_marker_popup("https://example.com/coast.jpeg", NA_character_)

  expect_match(popup, "Coastal photograph", fixed = TRUE)
})

test_that("conflict-style image names are recognisable", {
  files <- c("IMG_4342.jpeg", "IMG_4342 2.jpeg", "IMG_4352 (1) 2.jpeg")
  conflicts <- grepl(" [0-9]+[.](jpe?g|png)$", files, ignore.case = TRUE)

  expect_identical(conflicts, c(FALSE, TRUE, TRUE))
})
