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

if (!file.exists(dashboard)) {
  stop("Missing index.html. Render the dashboard before publishing.", call. = FALSE)
}

run_git(c("add", "--", "index.html"), project_root)
has_staged_changes <- !identical(
  system2(
    "git",
    c("-C", shQuote(project_root), "diff", "--cached", "--quiet", "--", "index.html"),
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
