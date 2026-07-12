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
    output_arg <- sub("^--output=", "", args[[1]])
  }

  if (!grepl("^/", output_arg)) {
    output_arg <- file.path(project_root, output_arg)
  }

  output_dir <- normalizePath(dirname(output_arg), mustWork = FALSE)
  file.path(output_dir, basename(output_arg))
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
