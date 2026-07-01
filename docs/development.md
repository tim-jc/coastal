# Development

## Module Layout

Project code is split by responsibility under `R/`.

| File | Responsibility |
| --- | --- |
| `R/load.R` | sources all modules in dependency order |
| `R/metadata.R` | loads metadata and defines constants |
| `R/validation.R` | validates coastal activity and ferry metadata |
| `R/silver_streams.R` | loads and maps silver activity stream columns |
| `R/coastal_data.R` | builds the core dataset and summaries |
| `R/geo_images.R` | handles geocoded extremities and image metadata |
| `R/maps.R` | renders leaflet maps and rider trace PNGs |
| `R/plots.R` | renders dashboard plots |
| `R/valueboxes.R` | renders coordinate value boxes |

Project entry points should load:

```r
source("R/load.R")
```

`R/load.R` loads project packages and sources `config.R` automatically when `con` and `silver_tbl()` are not already loaded. Existing scripts may still source `config.R` explicitly first.

## Local Configuration

`config.R` is untracked and reads environment variables from `.Renviron`.

Use `.Renviron.example` as the template. Do not commit real credentials.

## Metadata Files

The hand-maintained project data lives in:

```text
data/coastal_activities.R
data/ferries.R
```

These files are tracked because they are part of the app logic, not generated data.

## Checks

Parse R files:

```r
Rscript -e "files <- c(list.files('R', full.names = TRUE), 'scripts/check_inputs.R', 'ride_explorer.R', 'coastal_visualisation.R', 'reverse_geocoder.R'); invisible(lapply(files, parse))"
```

Parse R Markdown chunks:

```r
Rscript -e "out <- knitr::purl('index.Rmd', output = tempfile(fileext = '.R'), quiet = TRUE); invisible(parse(file = out))"
```

Validate silver inputs:

```r
Rscript scripts/check_inputs.R
```

The input check requires database access and should fail while silver `activity_streams` is empty.

## Generated Files

Generated files include:

- `index.html`
- rider PNGs under `docs/`
- print/export image outputs such as `coastal_vis_*.png`

Review generated diffs before committing them.
