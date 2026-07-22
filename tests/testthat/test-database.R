test_that("database configuration reports missing required values", {
  withr::local_envvar(c(
    CYCLING_PLATFORM_DB_NAME = NA,
    DB_NAME = NA
  ))

  expect_error(coastal_database_config(), "CYCLING_PLATFORM_DB_NAME")
})

test_that("a connection carries its silver schema explicitly", {
  connection <- structure(list(), coastal.silver_schema = "silver_test")
  expect_identical(coastal_connection_schema(connection), "silver_test")
})
