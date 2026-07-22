if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required to run tests.", call. = FALSE)
}

testthat::test_dir("tests/testthat", reporter = "summary")
