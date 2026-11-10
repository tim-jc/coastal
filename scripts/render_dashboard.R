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

dependency_path_for <- function(html_path) {
  stem <- tools::file_path_sans_ext(basename(html_path))
  file.path(dirname(html_path), paste0(stem, "_files"))
}

copy_file_contents <- function(from, to) {
  if (!file.exists(from) || dir.exists(from)) {
    stop("Source artifact is not a file: ", from, call. = FALSE)
  }

  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  source_size <- unname(file.info(from)$size)
  input <- file(from, open = "rb")
  output <- file(to, open = "wb")

  on.exit({
    if (!is.null(input)) close(input)
    if (!is.null(output)) close(output)
  }, add = TRUE)

  repeat {
    chunk <- readBin(input, what = "raw", n = 1024L * 1024L)
    if (length(chunk) == 0L) break
    writeBin(chunk, output, useBytes = TRUE)
  }

  close(input)
  input <- NULL
  close(output)
  output <- NULL

  if (!identical(unname(file.info(to)$size), source_size)) {
    stop("Copied artifact has an unexpected size: ", to, call. = FALSE)
  }

  invisible(to)
}

relative_files <- function(path) {
  if (!dir.exists(path)) return(character())

  list.files(
    path,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = FALSE
  )
}

sync_directory_contents <- function(from, to) {
  if (!dir.exists(from)) {
    stop("Source artifact directory does not exist: ", from, call. = FALSE)
  }

  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  source_files <- relative_files(from)

  for (relative_path in source_files) {
    copy_file_contents(
      file.path(from, relative_path),
      file.path(to, relative_path)
    )
  }

  stale_files <- setdiff(relative_files(to), source_files)
  if (length(stale_files) > 0L) {
    unlink(file.path(to, stale_files), force = TRUE)
  }

  invisible(to)
}

replace_file <- function(from, to) {
  # Rewrite the existing file rather than renaming over it. Cloud-backed file
  # providers can interpret inode replacement as a conflict and restore the old
  # dashboard under a duplicate name such as "index 2.html".
  copy_file_contents(from, to)
  unlink(from, force = TRUE)
  invisible(to)
}

install_rendered_dashboard <- function(staged_html, output_path) {
  staged_dependencies <- dependency_path_for(staged_html)
  output_dependencies <- dependency_path_for(output_path)

  if (!file.exists(staged_html)) {
    stop("Render completed without creating the expected HTML file.", call. = FALSE)
  }

  if (!dir.exists(staged_dependencies) || length(list.files(staged_dependencies)) == 0) {
    stop("Render completed without creating the expected dependency directory.", call. = FALSE)
  }

  html <- paste(readLines(staged_html, warn = FALSE), collapse = "\n")
  dependency_reference <- paste0(basename(staged_dependencies), "/")

  if (!grepl(dependency_reference, html, fixed = TRUE)) {
    stop("Rendered HTML does not reference its dependency directory.", call. = FALSE)
  }

  backup_dir <- tempfile(".coastal-render-backup-", tmpdir = dirname(output_path))
  dir.create(backup_dir)
  on.exit(unlink(backup_dir, recursive = TRUE, force = TRUE), add = TRUE)

  old_html <- file.path(backup_dir, basename(output_path))
  old_dependencies <- file.path(backup_dir, basename(output_dependencies))
  had_html <- file.exists(output_path)
  had_dependencies <- dir.exists(output_dependencies)

  if (had_html) {
    if (!file.copy(output_path, old_html, overwrite = TRUE)) {
      stop("Could not back up existing dashboard at ", output_path, call. = FALSE)
    }
  }

  if (had_dependencies) {
    dir.create(old_dependencies)
    sync_directory_contents(output_dependencies, old_dependencies)
  } else {
    dir.create(output_dependencies)
  }

  installed <- FALSE
  on.exit({
    if (!installed) {
      if (had_html && file.exists(old_html)) {
        copy_file_contents(old_html, output_path)
      } else if (file.exists(output_path)) {
        unlink(output_path, force = TRUE)
      }
      if (had_dependencies && dir.exists(old_dependencies)) {
        sync_directory_contents(old_dependencies, output_dependencies)
      } else if (dir.exists(output_dependencies)) {
        unlink(output_dependencies, recursive = TRUE, force = TRUE)
      }
    }
  }, add = TRUE)

  replace_file(staged_html, output_path)
  sync_directory_contents(staged_dependencies, output_dependencies)
  installed <- TRUE
  invisible(output_path)
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

main <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render the dashboard.", call. = FALSE)
  }

  args <- commandArgs(trailingOnly = TRUE)
  project_root <- find_project_root()
  output_path <- parse_output_arg(args, project_root)
  output_dir <- dirname(output_path)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  setwd(project_root)

  message("Rendering index.Rmd")
  message("Output: ", output_path)

  staging_dir <- tempfile(".coastal-render-", tmpdir = output_dir)
  dir.create(staging_dir)
  on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)
  staged_output <- file.path(staging_dir, basename(output_path))

  render_env <- new.env(parent = globalenv())
  on.exit(disconnect_render_connections(render_env), add = TRUE)

  rmarkdown::render(
    input = file.path(project_root, "index.Rmd"),
    output_file = staged_output,
    envir = render_env
  )

  install_rendered_dashboard(staged_output, output_path)
  message("Dashboard render complete.")
  disconnect_render_connections(render_env)
}

main()
