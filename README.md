# Coastal

Coastal builds a cycling dashboard for the coastal ride project. It consumes ride activity and stream data from the `cycling_platform_silver` layer, combines it with hand-maintained coastal segment metadata, and renders the public dashboard to root `index.html`.

The generated root `index.html` file is the GitHub Pages publishing artifact. Do not commit regenerated dashboard output while validation is failing.

## Current Status

The project is currently blocked on an upstream silver streams issue: `cycling_platform_silver.activity_streams` is expected to contain stream rows for the coastal activity IDs, but the most recent smoke check found it empty.

Before publishing:

```r
Rscript scripts/check_inputs.R
```

## Entry Points

- `index.Rmd`: renders the flexdashboard and writes root `index.html`.
- `ride_explorer.R`: inspects one activity stream to determine coastal crop windows.
- `scripts/check_inputs.R`: validates local metadata against the silver tables.
- `coastal_visualisation.R`: creates static rider visualisations.
- `reverse_geocoder.R`: supports postcode/location enrichment.

## Project Layout

- `R/load.R`: loads all project modules in dependency order.
- `R/metadata.R`: loads metadata and shared constants.
- `R/validation.R`: validates ride and ferry metadata.
- `R/silver_streams.R`: owns silver stream column selection and stream loading.
- `R/coastal_data.R`: builds the core coastal dataset and ride summaries.
- `R/geo_images.R`: handles position extremities and image metadata.
- `R/maps.R`: renders maps and rider trace PNGs.
- `R/plots.R`: renders dashboard plots.
- `R/valueboxes.R`: renders coordinate value boxes.
- `data/coastal_activities.R`: hand-maintained coastal ride segment metadata.
- `data/ferries.R`: hand-maintained ferry marker metadata.

Use this in project entry points:

```r
source("R/load.R")
```

`R/load.R` loads project packages and sources `config.R` automatically when `con` and `silver_tbl()` are not already loaded.

## Local Configuration

`config.R` reads database settings from environment variables. Keep secrets in `.Renviron`, which is ignored by git. Start from `.Renviron.example`.

Required variables:

```text
CYCLING_PLATFORM_DB_NAME=cycling_platform_silver
CYCLING_PLATFORM_SILVER_SCHEMA=cycling_platform_silver
CYCLING_PLATFORM_DB_HOST=192.168.1.94
CYCLING_PLATFORM_DB_PORT=3306
CYCLING_PLATFORM_DB_USER=your_user
CYCLING_PLATFORM_DB_PASSWORD=your_password
```

Legacy `DB_*` variable names are still accepted as fallbacks.

## Add A Ride

1. Add the ride segment to `data/coastal_activities.R` in geographic order.
2. Use `ride_explorer.R` to inspect the silver activity stream and choose `ride_start_time` / `ride_end_time`.
3. Add ferry markers to `data/ferries.R` if relevant.
4. Run `Rscript scripts/check_inputs.R`.
5. Run `Rscript scripts/render_dashboard.R`.
6. Review generated changes, especially root `index.html` and rider PNGs.
7. Commit source and generated publishing output only when validation passes.

## Publishing

Run `Rscript scripts/render_dashboard.R` to render `index.Rmd` to root `index.html`; pushing that file publishes the dashboard.

Do not publish while `scripts/check_inputs.R` fails. In the current state, publishing should wait until the silver `activity_streams` table is populated.

## Static Checks

Parse the project R files without querying the database:

```r
Rscript -e "files <- c(list.files('R', full.names = TRUE), 'scripts/check_inputs.R', 'ride_explorer.R', 'coastal_visualisation.R', 'reverse_geocoder.R'); invisible(lapply(files, parse))"
```

Check the R Markdown chunks:

```r
Rscript -e "out <- knitr::purl('index.Rmd', output = tempfile(fileext = '.R'), quiet = TRUE); invisible(parse(file = out))"
```

## More Documentation

- `docs/data-contract.md`
- `docs/ride-maintenance.md`
- `docs/development.md`
- `docs/publishing.md`
