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
  database_name <- coastal_env_value(
    "CYCLING_PLATFORM_DB_NAME",
    fallback = coastal_env_value("DB_NAME", required = FALSE)
  )

  list(
    dbname = database_name,
    schema = coastal_env_value(
      "CYCLING_PLATFORM_SILVER_SCHEMA",
      fallback = database_name
    ),
    host = coastal_env_value(
      "CYCLING_PLATFORM_DB_HOST",
      fallback = coastal_env_value("DB_HOST", required = FALSE)
    ),
    port = as.integer(coastal_env_value(
      "CYCLING_PLATFORM_DB_PORT",
      fallback = coastal_env_value("DB_PORT", required = FALSE)
    )),
    user = coastal_env_value(
      "CYCLING_PLATFORM_DB_USER",
      fallback = coastal_env_value("DB_USER", required = FALSE)
    ),
    password = coastal_env_value(
      "CYCLING_PLATFORM_DB_PASSWORD",
      fallback = coastal_env_value("DB_PASSWORD", required = FALSE)
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
    dbname = config$dbname,
    host = config$host,
    port = config$port,
    user = config$user,
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
