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

test_that("mobile navigation uses the framework icon slot without a solid fill", {
  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    coastal_css,
    ".navbar-inverse .navbar-toggle > .icon-bar:last-child",
    fixed = TRUE
  )
  expect_match(coastal_css, "background-color: transparent !important", fixed = TRUE)
  expect_match(coastal_css, "M4 7h22M4 15h22M4 23h22", fixed = TRUE)
  expect_false(grepl(".navbar-toggle .icon-bar {\n  background-color: var(--ptg-ink)", coastal_css, fixed = TRUE))
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
