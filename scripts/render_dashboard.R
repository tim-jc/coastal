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

is_publish_target <- function(output_path, project_root) {
  identical(
    normalizePath(output_path, mustWork = FALSE),
    normalizePath(file.path(project_root, "index.html"), mustWork = FALSE)
  )
}

run_git <- function(args) {
  status <- system2("git", args)

  if (!identical(status, 0L)) {
    stop(
      paste("Git command failed:", paste(c("git", args), collapse = " ")),
      call. = FALSE
    )
  }
}

publish_dashboard <- function(project_root, output_path) {
  if (!is_publish_target(output_path, project_root)) {
    message("Preview render only; skipping git publish.")
    return(invisible(FALSE))
  }

  run_git(c("add", "--", "index.html"))

  has_staged_changes <- !identical(
    system2(
      "git",
      c("diff", "--cached", "--quiet", "--", "index.html"),
      stdout = FALSE,
      stderr = FALSE
    ),
    0L
  )

  if (!has_staged_changes) {
    message("No rendered index.html changes to publish.")
    return(invisible(FALSE))
  }

  run_git(c("commit", "-m", "Publish rendered dashboard"))
  run_git("push")

  message("Published rendered dashboard.")
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

rmarkdown::render(
  input = file.path(project_root, "index.Rmd"),
  output_file = output_path,
  envir = new.env(parent = globalenv())
)

message("Dashboard render complete.")
publish_dashboard(project_root, output_path)
