test_that("module loading does not create a database connection", {
  expect_false(exists("con", envir = globalenv(), inherits = FALSE))
  expect_true(is.function(connect_coastal_database))
  expect_true(is.function(load_coastal_packages))
})
