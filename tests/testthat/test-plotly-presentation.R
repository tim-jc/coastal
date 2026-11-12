test_that("Field Notebook Plotly styling preserves analytical trace properties", {
  make_widget <- function() {
    plotly::plot_ly(
      x = c(1, 2),
      y = c(3, 4),
      type = "scatter",
      mode = "lines+markers",
      text = c("first", "second"),
      hoverinfo = "text",
      line = list(color = phiets_navy, width = 3),
      marker = list(color = phiets_red, size = 7)
    ) |>
      plotly::layout(
        xaxis = list(type = "linear", range = c(0, 3), title = "Distance"),
        yaxis = list(type = "linear", range = c(0, 5), title = "Miles")
      )
  }

  before <- plotly::plotly_build(make_widget())
  after <- plotly::plotly_build(field_notebook_plotly(make_widget()))

  expect_length(after$x$data, length(before$x$data))
  expect_identical(after$x$data[[1]]$type, before$x$data[[1]]$type)
  expect_identical(after$x$data[[1]]$mode, before$x$data[[1]]$mode)
  expect_identical(after$x$data[[1]]$line, before$x$data[[1]]$line)
  expect_identical(after$x$data[[1]]$marker, before$x$data[[1]]$marker)
  expect_identical(after$x$data[[1]]$text, before$x$data[[1]]$text)
  expect_identical(after$x$data[[1]]$hoverinfo, before$x$data[[1]]$hoverinfo)
  expect_identical(after$x$layout$xaxis$type, before$x$layout$xaxis$type)
  expect_identical(after$x$layout$xaxis$range, before$x$layout$xaxis$range)
  expect_identical(after$x$layout$yaxis$type, before$x$layout$yaxis$type)
  expect_identical(after$x$layout$yaxis$range, before$x$layout$yaxis$range)
})

test_that("Field Notebook Plotly styling sets shared presentation attributes", {
  widget <- plotly::plot_ly(x = 1:2, y = 2:3) |>
    field_notebook_plotly() |>
    plotly::plotly_build()
  layout <- widget$x$layout

  expect_identical(layout$paper_bgcolor, "#F1EFE7")
  expect_identical(layout$plot_bgcolor, "#F1EFE7")
  expect_identical(layout$font$color, "#172630")
  expect_identical(layout$xaxis$gridcolor, "#C8CDC2")
  expect_identical(layout$yaxis$linecolor, "#667C73")
  expect_identical(layout$legend$font$color, "#52635C")
  expect_identical(layout$hoverlabel$bgcolor, "#172630")
  expect_identical(layout$hoverlabel$font$color, "#F1EFE7")
})

test_that("all five Stats charts use the shared Plotly presentation helper", {
  sources <- paste(
    c(
      readLines(file.path(coastal_project_root, "R", "stats.R"), warn = FALSE),
      readLines(file.path(coastal_project_root, "R", "plots.R"), warn = FALSE)
    ),
    collapse = "\n"
  )

  helper_calls <- gregexpr("field_notebook_plotly()", sources, fixed = TRUE)[[1]]
  expect_length(helper_calls[helper_calls > 0], 5L)

  coastal_css <- paste(
    readLines(
      file.path(coastal_project_root, "assets", "coastal.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(coastal_css, "#stats .plotly .modebar", fixed = TRUE)
  expect_match(coastal_css, ".modebar-btn:focus-visible", fixed = TRUE)
})
