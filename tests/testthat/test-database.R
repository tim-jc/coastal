test_that("database configuration reports missing required values", {
  withr::local_envvar(c(
    MARIADB_HOST = NA
  ))

  expect_error(coastal_database_config(), "MARIADB_HOST")
})

test_that("database configuration uses the shared MariaDB contract", {
  withr::local_envvar(c(
    CYCLING_PLATFORM_SILVER_SCHEMA = "silver_test",
    MARIADB_HOST = "db.example.test",
    MARIADB_PORT = "3307",
    MARIADB_USER = "test_user",
    MARIADB_PASSWORD = "test_password"
  ))

  config <- coastal_database_config()

  expect_identical(
    names(config),
    c("schema", "host", "port", "username", "password")
  )
  expect_identical(config$schema, "silver_test")
  expect_identical(config$host, "db.example.test")
  expect_identical(config$port, 3307L)
  expect_identical(config$username, "test_user")
})

test_that("a connection carries its silver schema explicitly", {
  connection <- structure(list(), coastal.silver_schema = "silver_test")
  expect_identical(coastal_connection_schema(connection), "silver_test")
})
