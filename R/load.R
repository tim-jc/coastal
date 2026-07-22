coastal_find_project_root <- function() {
  candidates <- character()

  if (requireNamespace("here", quietly = TRUE)) {
    candidates <- c(candidates, normalizePath(here::here(), mustWork = FALSE))
  }

  load_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)

  if (!is.null(load_file) && nzchar(load_file)) {
    candidates <- c(
      candidates,
      dirname(dirname(normalizePath(load_file, mustWork = FALSE)))
    )
  }

  path <- normalizePath(getwd(), mustWork = FALSE)

  repeat {
    candidates <- c(candidates, path)
    parent <- dirname(path)

    if (identical(parent, path)) {
      break
    }

    path <- parent
  }

  candidates <- unique(candidates[nzchar(candidates)])
  has_project_files <- function(path) {
    file.exists(file.path(path, "R", "load.R")) &&
      file.exists(file.path(path, "data", "coastal_activities.R"))
  }

  matches <- candidates[vapply(candidates, has_project_files, logical(1))]

  if (length(matches) > 0) {
    return(matches[[1]])
  }

  stop(
    paste(
      "Could not find coastal project root.",
      paste0("Working directory: ", getwd()),
      "Expected to find R/load.R and data/coastal_activities.R.",
      sep = "\n"
    ),
    call. = FALSE
  )
}

coastal_project_root <- coastal_find_project_root()
options(coastal.project_root = coastal_project_root)

source(file.path(coastal_project_root, "R", "dependencies.R"))
source(file.path(coastal_project_root, "R", "database.R"))
source(file.path(coastal_project_root, "R", "metadata.R"))
source(file.path(coastal_project_root, "R", "validation.R"))
source(file.path(coastal_project_root, "R", "silver_streams.R"))
source(file.path(coastal_project_root, "R", "geo_images.R"))
source(file.path(coastal_project_root, "R", "maps.R"))
source(file.path(coastal_project_root, "R", "plots.R"))
source(file.path(coastal_project_root, "R", "valueboxes.R"))
source(file.path(coastal_project_root, "R", "adventure_detail.R"))
source(file.path(coastal_project_root, "R", "stats.R"))
source(file.path(coastal_project_root, "R", "coastal_data.R"))
