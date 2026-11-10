load_render_dashboard_functions <- function() {
  expressions <- parse(
    file.path(coastal_project_root, "scripts", "render_dashboard.R")
  )
  env <- new.env(parent = globalenv())

  for (expression in head(expressions, -1L)) {
    eval(expression, envir = env)
  }

  env
}

file_inode <- function(path) {
  as.integer(system2("stat", c("-f", "%i", path), stdout = TRUE))
}

test_that("rendered favicon declaration identifies its PNG correctly", {
  render <- load_render_dashboard_functions()
  html <- withr::local_tempfile(fileext = ".html")
  writeLines(
    c(
      "<head>",
      '<link rel="icon" href="assets/identity/favicon-32.png">',
      "</head>"
    ),
    html
  )

  render$fix_png_favicon_declaration(html)
  rendered_html <- paste(readLines(html), collapse = "\n")

  expect_match(
    rendered_html,
    '<link rel="icon" type="image/png" sizes="32x32" href="assets/identity/favicon-32.png">',
    fixed = TRUE
  )
  expect_length(
    stringr::str_extract_all(rendered_html, '<link rel="icon"')[[1]],
    1L
  )
})

test_that("render installation updates canonical artifacts in place", {
  skip_if_not(identical(Sys.info()[["sysname"]], "Darwin"))
  render <- load_render_dashboard_functions()
  root <- withr::local_tempdir()

  output <- file.path(root, "index.html")
  writeLines("old dashboard", output)
  dependencies <- file.path(root, "index_files")
  dir.create(dependencies)
  shared_asset <- file.path(dependencies, "shared.js")
  writeLines("old shared asset", shared_asset)
  writeLines("stale asset", file.path(dependencies, "stale.js"))

  html_inode <- file_inode(output)
  asset_inode <- file_inode(shared_asset)

  staging <- file.path(root, "staging")
  dir.create(staging)
  staged_output <- file.path(staging, "index.html")
  writeLines(
    '<script src="index_files/shared.js"></script>',
    staged_output
  )
  staged_dependencies <- file.path(staging, "index_files")
  dir.create(staged_dependencies)
  writeLines("new shared asset", file.path(staged_dependencies, "shared.js"))

  render$install_rendered_dashboard(staged_output, output)

  expect_identical(file_inode(output), html_inode)
  expect_identical(file_inode(shared_asset), asset_inode)
  expect_match(readLines(output), "index_files/shared.js", fixed = TRUE)
  expect_identical(readLines(shared_asset), "new shared asset")
  expect_false(file.exists(file.path(dependencies, "stale.js")))
  expect_false(file.exists(file.path(root, "index 2.html")))
})
