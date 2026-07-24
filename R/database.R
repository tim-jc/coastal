coastal_env_value <- function(name, fallback = NULL, required = TRUE) {
  value <- Sys.getenv(name, unset = NA_character_)

  if (is.na(value) || identical(value, "")) {
    value <- fallback
  }

  if (required && (is.null(value) || is.na(value) || identical(value, ""))) {
    stop("Missing required environment variable: ", name, call. = FALSE)
  }

  value
}

coastal_database_config <- function() {
  list(
    schema = coastal_env_value(
      "CYCLING_PLATFORM_SILVER_SCHEMA"
    ),
    host = coastal_env_value(
      "MARIADB_HOST"
    ),
    port = as.integer(coastal_env_value(
      "MARIADB_PORT"
    )),
    username = coastal_env_value(
      "MARIADB_USER"
    ),
    password = coastal_env_value(
      "MARIADB_PASSWORD"
    )
  )
}

connect_coastal_database <- function(config = coastal_database_config()) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RMariaDB", quietly = TRUE)) {
    stop("Packages 'DBI' and 'RMariaDB' are required for database access.", call. = FALSE)
  }

  connection <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = config$host,
    port = config$port,
    username = config$username,
    password = config$password
  )
  attr(connection, "coastal.silver_schema") <- config$schema
  connection
}

coastal_connection_schema <- function(connection) {
  schema <- attr(connection, "coastal.silver_schema", exact = TRUE)

  if (is.null(schema) || !nzchar(schema)) {
    stop(
      "The database connection has no silver schema. Create it with connect_coastal_database().",
      call. = FALSE
    )
  }

  schema
}

silver_tbl <- function(table, connection, schema = coastal_connection_schema(connection)) {
  dplyr::tbl(connection, DBI::Id(schema = schema, table = table))
}
