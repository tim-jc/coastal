find_project_root <- function() {
  path <- normalizePath(getwd(), mustWork = FALSE)

  repeat {
    if (file.exists(file.path(path, "index.Rmd")) &&
        file.exists(file.path(path, "R", "load.R"))) {
      return(path)
    }

    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not find project root from current working directory.", call. = FALSE)
    }
    path <- parent
  }
}

run_git <- function(args, project_root) {
  status <- system2("git", c("-C", shQuote(project_root), args))

  if (!identical(status, 0L)) {
    stop(
      paste("Git command failed:", paste(c("git", args), collapse = " ")),
      call. = FALSE
    )
  }
}

project_root <- find_project_root()
dashboard <- file.path(project_root, "index.html")
dependency_dir <- file.path(project_root, "index_files")
dashboard_artifacts <- c("index.html", "index_files")

if (!file.exists(dashboard)) {
  stop("Missing index.html. Render the dashboard before publishing.", call. = FALSE)
}

if (!dir.exists(dependency_dir)) {
  stop(
    "Missing index_files. Render the non-self-contained dashboard before publishing.",
    call. = FALSE
  )
}

duplicate_dependencies <- list.files(
  dependency_dir,
  pattern = " [0-9]+\\.[^/]+$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(duplicate_dependencies) > 0) {
  stop(
    paste0(
      "Refusing to publish index_files containing duplicate-suffixed assets. ",
      "Run scripts/render_dashboard.R to rebuild it cleanly."
    ),
    call. = FALSE
  )
}

run_git(c("add", "-A", "--", dashboard_artifacts), project_root)
has_staged_changes <- !identical(
  system2(
    "git",
    c(
      "-C",
      shQuote(project_root),
      "diff",
      "--cached",
      "--quiet",
      "--",
      dashboard_artifacts
    ),
    stdout = FALSE,
    stderr = FALSE
  ),
  0L
)

if (!has_staged_changes) {
  message("No rendered index.html changes to publish.")
  quit(save = "no", status = 0L)
}

run_git(c("commit", "-m", shQuote("Publish rendered dashboard")), project_root)
run_git("push", project_root)
message("Published rendered dashboard.")
