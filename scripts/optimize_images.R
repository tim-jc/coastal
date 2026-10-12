find_project_root <- function() {
  path <- normalizePath(getwd(), mustWork = FALSE)

  repeat {
    if (file.exists(file.path(path, "index.Rmd")) && dir.exists(file.path(path, "R"))) {
      return(path)
    }

    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not find project root from current working directory.", call. = FALSE)
    }
    path <- parent
  }
}

parse_args <- function(args, project_root) {
  value_for <- function(prefix, default) {
    match <- grep(paste0("^", prefix, "="), args, value = TRUE)
    if (length(match) == 0) return(default)
    sub(paste0("^", prefix, "="), "", match[[1]])
  }

  input_dir <- value_for("--input-dir", file.path(project_root, "docs", "images"))
  if (!grepl("^/", input_dir)) input_dir <- file.path(project_root, input_dir)

  list(
    apply = "--apply" %in% args,
    input_dir = normalizePath(input_dir, mustWork = FALSE),
    max_dimension = as.integer(value_for("--max-dimension", "2560")),
    quality = as.integer(value_for("--quality", "82"))
  )
}

read_image_info <- function(path) {
  suppressWarnings(
    suppressMessages(
      exifr::read_exif(
        path,
        args = c(
          "-ImageWidth",
          "-ImageHeight",
          "-GPSLatitude",
          "-GPSLongitude",
          "-DateTimeOriginal",
          "-ImageDescription",
          "-Description",
          "-Caption-Abstract"
        ),
        recursive = FALSE
      )
    )
  )
}

metadata_preserved <- function(before, after) {
  fields <- c("GPSLatitude", "GPSLongitude", "DateTimeOriginal")

  for (field in fields) {
    if (!field %in% names(before)) next
    old_value <- before[[field]][[1]]
    if (is.na(old_value) || identical(as.character(old_value), "")) next
    if (!field %in% names(after)) return(FALSE)

    new_value <- after[[field]][[1]]
    if (is.na(new_value)) return(FALSE)

    if (field %in% c("GPSLatitude", "GPSLongitude")) {
      if (!isTRUE(all.equal(as.numeric(old_value), as.numeric(new_value), tolerance = 1e-7))) {
        return(FALSE)
      }
    } else if (!identical(as.character(old_value), as.character(new_value))) {
      return(FALSE)
    }
  }

  description_fields <- c("ImageDescription", "Description", "Caption-Abstract")
  description_values <- function(info) {
    present <- intersect(description_fields, names(info))
    values <- unlist(info[1, present, drop = FALSE], use.names = FALSE)
    unique(as.character(values[!is.na(values) & nzchar(as.character(values))]))
  }

  old_descriptions <- description_values(before)
  new_descriptions <- description_values(after)
  if (!all(old_descriptions %in% new_descriptions)) return(FALSE)

  TRUE
}

replace_file <- function(source, replacement) {
  backup <- tempfile(paste0(".", basename(source), "-backup-"), tmpdir = dirname(source))

  if (!file.rename(source, backup)) {
    stop("Could not back up image before replacement: ", source, call. = FALSE)
  }

  installed <- FALSE
  on.exit({
    if (!installed) {
      if (file.exists(source)) unlink(source, force = TRUE)
      file.rename(backup, source)
    }
  }, add = TRUE)

  if (!file.rename(replacement, source)) {
    stop("Could not install optimized image: ", source, call. = FALSE)
  }

  installed <- TRUE
  unlink(backup, force = TRUE)
  invisible(source)
}

prepare_optimized_image <- function(path, max_dimension, quality, staging_dir) {
  before <- read_image_info(path)
  width <- as.numeric(before$ImageWidth[[1]])
  height <- as.numeric(before$ImageHeight[[1]])

  if (max(width, height) <= max_dimension) {
    return(list(status = "within-limit", before = file.info(path)$size, after = file.info(path)$size))
  }

  candidate <- file.path(staging_dir, basename(path))

  if (!file.copy(path, candidate, overwrite = TRUE)) {
    stop("Could not create temporary image for: ", path, call. = FALSE)
  }

  status <- system2(
    "sips",
    c("-Z", max_dimension, "-s", "formatOptions", quality, shQuote(candidate)),
    stdout = FALSE,
    stderr = FALSE
  )

  if (!identical(status, 0L)) {
    stop("Image optimization failed for: ", path, call. = FALSE)
  }

  after <- read_image_info(candidate)
  if (max(as.numeric(after$ImageWidth[[1]]), as.numeric(after$ImageHeight[[1]])) > max_dimension) {
    stop("Optimized image exceeds the requested dimensions: ", path, call. = FALSE)
  }
  if (!metadata_preserved(before, after)) {
    stop("Required image metadata was not preserved: ", path, call. = FALSE)
  }

  old_size <- file.info(path)$size
  new_size <- file.info(candidate)$size
  if (new_size >= old_size) {
    unlink(candidate, force = TRUE)
    return(list(status = "not-smaller", before = old_size, after = old_size))
  }

  list(
    status = "prepared",
    before = old_size,
    after = new_size,
    source = path,
    candidate = candidate
  )
}

if (!requireNamespace("exifr", quietly = TRUE)) {
  stop("Package 'exifr' is required to verify image metadata.", call. = FALSE)
}
if (!nzchar(Sys.which("sips"))) {
  stop("This script currently requires macOS 'sips' for metadata-safe resizing.", call. = FALSE)
}

project_root <- find_project_root()
config <- parse_args(commandArgs(trailingOnly = TRUE), project_root)

if (!dir.exists(config$input_dir)) {
  stop("Image directory does not exist: ", config$input_dir, call. = FALSE)
}
if (is.na(config$max_dimension) || config$max_dimension < 640) {
  stop("--max-dimension must be an integer of at least 640.", call. = FALSE)
}
if (is.na(config$quality) || config$quality < 1 || config$quality > 100) {
  stop("--quality must be an integer from 1 to 100.", call. = FALSE)
}

images <- list.files(
  config$input_dir,
  pattern = "[.](jpe?g)$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

dimensions <- lapply(images, read_image_info)
needs_resize <- vapply(
  dimensions,
  function(info) max(as.numeric(info$ImageWidth[[1]]), as.numeric(info$ImageHeight[[1]])) > config$max_dimension,
  logical(1)
)
candidates <- images[needs_resize]

message("Images found: ", length(images))
message("Images above ", config$max_dimension, " px: ", length(candidates))
message("Current size: ", format(sum(file.info(images)$size), big.mark = ","), " bytes")

if (!config$apply) {
  message("Dry run only. Re-run with --apply to optimize these images.")
  quit(save = "no", status = 0L)
}

staging_dir <- tempfile(".coastal-image-optimization-", tmpdir = config$input_dir)
dir.create(staging_dir)
on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)

message("Preparing and validating all candidates before replacing originals...")
results <- lapply(candidates, function(path) {
  result <- prepare_optimized_image(
    path,
    config$max_dimension,
    config$quality,
    staging_dir
  )
  message(
    basename(path), ": ", result$status, " (",
    format(result$before, big.mark = ","), " -> ",
    format(result$after, big.mark = ","), " bytes)"
  )
  result
})

prepared <- Filter(function(result) identical(result$status, "prepared"), results)
invisible(lapply(prepared, function(result) {
  replace_file(result$source, result$candidate)
}))

bytes_before <- sum(vapply(results, `[[`, numeric(1), "before"))
bytes_after <- sum(vapply(results, `[[`, numeric(1), "after"))
message("Optimization complete.")
message("Candidate size: ", format(bytes_before, big.mark = ","), " -> ", format(bytes_after, big.mark = ","), " bytes")
message("Saved: ", format(bytes_before - bytes_after, big.mark = ","), " bytes")
