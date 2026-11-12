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

test_that("table containment spans the full stacked-layout breakpoint", {
  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(coastal_css, "@media (max-width: 767px)", fixed = TRUE)
  expect_false(grepl("@media (max-width: 575.98px)", coastal_css, fixed = TRUE))
  expect_match(coastal_css, "#latest-adventure .dashboard-column", fixed = TRUE)
  expect_match(coastal_css, "#rides .dashboard-column", fixed = TRUE)
  expect_match(coastal_css, "#latest-adventure .dataTables_scrollBody", fixed = TRUE)
  expect_match(coastal_css, "#rides .dataTables_scrollBody", fixed = TRUE)
  expect_match(coastal_css, "overflow-x: auto !important", fixed = TRUE)
  expect_false(grepl("body {\n  overflow-x: hidden", coastal_css, fixed = TRUE))
  expect_false(grepl("html {\n  overflow-x: hidden", coastal_css, fixed = TRUE))
})

test_that("table containment hands off to the desktop layout at 768px", {
  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(coastal_css, "@media (max-width: 767px)", fixed = TRUE)
  expect_match(
    coastal_css,
    "@media (min-width: 768px) and (max-width: 859.98px)",
    fixed = TRUE
  )
  expect_match(coastal_css, "#maps #row-2 > .value-box", fixed = TRUE)
})

test_that("DataTables use the Field Notebook presentation without changing behaviour", {
  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(coastal_css, "#dashboard-container table.dataTable", fixed = TRUE)
  expect_match(coastal_css, "background: var(--ptg-paper)", fixed = TRUE)
  expect_match(coastal_css, "border-bottom: 1px solid var(--ptg-line)", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container table.dataTable thead th", fixed = TRUE)
  expect_match(coastal_css, "background: var(--ptg-surface)", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container table.dataTable tbody td", fixed = TRUE)
  expect_match(coastal_css, ".dataTables_filter input:focus", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .dataTables_scrollBody", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container div.dts .dataTables_scrollBody table", fixed = TRUE)

  expect_false(grepl("overflow-x: hidden", coastal_css, fixed = TRUE))
})

test_that("desktop map extremity boxes may shrink within their row", {
  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    coastal_css,
    "@media (min-width: 768px) and (max-width: 859.98px)",
    fixed = TRUE
  )
  expect_match(coastal_css, "#maps #row-2 > .value-box", fixed = TRUE)
  expect_match(
    coastal_css,
    "#maps #row-2 > .value-box {\n    flex: 1 1 auto !important;\n    min-width: 0;\n  }",
    fixed = TRUE
  )
})

test_that("dashboard surfaces and tabs use scoped Field Notebook tokens", {
  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(coastal_css, "html body {\n  background: var(--ptg-paper);", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .chart-wrapper", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .chart-title", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .chart-stage", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .nav-tabs-custom > .nav-tabs", fixed = TRUE)
  expect_match(coastal_css, "border-top-color: var(--ptg-copper)", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .nav-tabs-custom > .tab-content", fixed = TRUE)

  identity_start <- regexpr("#dashboard-container,", coastal_css, fixed = TRUE)
  identity_end <- regexpr("\n#dashboard-container .value-box {", coastal_css, fixed = TRUE)
  identity_rules <- substr(coastal_css, identity_start, identity_end - 1L)
  expect_false(grepl("!important", identity_rules, fixed = TRUE))
})

test_that("value boxes use scoped Field Notebook metric styling", {
  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(coastal_css, "#dashboard-container .value-box {", fixed = TRUE)
  expect_match(coastal_css, "background-color: var(--ptg-surface) !important", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .value-box .value", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .value-box .caption", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .value-box .icon i", fixed = TRUE)
  expect_match(coastal_css, "font-size: 24px", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .value-box.linked-value:hover", fixed = TRUE)
  expect_match(coastal_css, "#dashboard-container .coastal-value-box-link:focus-visible", fixed = TRUE)
  expect_match(coastal_css, "@media (max-width: 767px)", fixed = TRUE)
  expect_false(grepl("overflow-x: hidden", coastal_css, fixed = TRUE))
})

test_that("linked value boxes are upgraded to native links", {
  shell_source <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal-shell.html"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(shell_source, "span.value-output[data-href]", fixed = TRUE)
  expect_match(shell_source, 'document.createElement("a")', fixed = TRUE)
  expect_match(shell_source, 'link.className = "coastal-value-box-link"', fixed = TRUE)
  expect_match(shell_source, 'link.target = "_blank"', fixed = TRUE)
  expect_match(shell_source, 'link.rel = "noopener noreferrer"', fixed = TRUE)
  expect_match(shell_source, 'link.setAttribute("aria-label"', fixed = TRUE)
  expect_match(shell_source, '.off("click.value-box")', fixed = TRUE)
  expect_false(grepl('setAttribute("tabindex"', shell_source, fixed = TRUE))
  expect_false(grepl('addEventListener("keydown"', shell_source, fixed = TRUE))
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
