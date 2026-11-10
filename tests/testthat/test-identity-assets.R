test_that("local Past the Grid identity assets are complete", {
  identity_files <- c(
    "assets/identity/tokens.css",
    "assets/identity/symbol.svg",
    "assets/identity/favicon.svg",
    "assets/identity/favicon-32.png",
    "assets/identity/PROVENANCE.md",
    "assets/coastal.css",
    "assets/coastal-shell.html"
  )

  expect_true(all(file.exists(file.path(coastal_project_root, identity_files))))
})

test_that("dashboard uses local identity assets", {
  dashboard_source <- paste(
    readLines(file.path(coastal_project_root, "index.Rmd"), warn = FALSE),
    collapse = "\n"
  )
  shell_source <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal-shell.html"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(dashboard_source, "assets/identity/favicon-32.png", fixed = TRUE)
  expect_match(dashboard_source, "assets/identity/tokens.css", fixed = TRUE)
  expect_match(dashboard_source, "assets/coastal.css", fixed = TRUE)
  expect_match(
    shell_source,
    'symbol.src = ["assets", "identity", "symbol.svg"].join("/")',
    fixed = TRUE
  )
  expect_false(grepl('src="/Users/', shell_source, fixed = TRUE))
  expect_match(shell_source, "https://pastthegrid.uk", fixed = TRUE)
  expect_false(grepl("https://pastthegrid.uk/.+\\.(css|svg)", shell_source))
})

test_that("publisher includes and validates identity assets", {
  publish_source <- paste(
    readLines(
      file.path(coastal_project_root, "scripts", "publish_dashboard.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(publish_source, '"assets"', fixed = TRUE)
  expect_match(publish_source, "missing_identity_assets", fixed = TRUE)
})
