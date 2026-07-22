find_project_root <- function() {
  path <- normalizePath(getwd(), mustWork = FALSE)

  repeat {
    if (
      file.exists(file.path(path, "index.Rmd")) &&
        file.exists(file.path(path, "R", "load.R"))
    ) {
      return(path)
    }

    parent <- dirname(path)

    if (identical(parent, path)) {
      stop("Could not find project root from current working directory.", call. = FALSE)
    }

    path <- parent
  }
}

parse_output_arg <- function(args, project_root) {
  output_arg <- "index.html"

  if (length(args) > 0) {
    output_args <- grep("^(--output=|[^-])", args, value = TRUE)

    if (length(output_args) > 0) {
      output_arg <- sub("^--output=", "", output_args[[1]])
    }
  }

  if (!grepl("^/", output_arg)) {
    output_arg <- file.path(project_root, output_arg)
  }

  output_dir <- normalizePath(dirname(output_arg), mustWork = FALSE)
  file.path(output_dir, basename(output_arg))
}

is_valid_db_connection <- function(object) {
  tryCatch(
    DBI::dbIsValid(object),
    error = function(e) FALSE
  )
}

disconnect_env_connections <- function(env) {
  object_names <- ls(env, all.names = TRUE)
  objects <- mget(object_names, envir = env, inherits = FALSE)
  connections <- objects[vapply(objects, is_valid_db_connection, logical(1))]

  if (length(connections) == 0) {
    return(invisible(FALSE))
  }

  invisible(lapply(connections, DBI::dbDisconnect))
  invisible(TRUE)
}

disconnect_render_connections <- function(render_env) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  environments <- list(render_env, globalenv())

  if (requireNamespace("knitr", quietly = TRUE)) {
    environments <- c(list(knitr::knit_global()), environments)
  }

  closed <- vapply(environments, disconnect_env_connections, logical(1))

  if (!any(closed)) {
    return(invisible(FALSE))
  }

  message("Database connection closed.")
  invisible(TRUE)
}

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("Package 'rmarkdown' is required to render the dashboard.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
project_root <- find_project_root()
output_path <- parse_output_arg(args, project_root)
output_dir <- dirname(output_path)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

setwd(project_root)

message("Rendering index.Rmd")
message("Output: ", output_path)

render_env <- new.env(parent = globalenv())
on.exit(disconnect_render_connections(render_env), add = TRUE)

rmarkdown::render(
  input = file.path(project_root, "index.Rmd"),
  output_file = output_path,
  envir = render_env
)

message("Dashboard render complete.")
disconnect_render_connections(render_env)
