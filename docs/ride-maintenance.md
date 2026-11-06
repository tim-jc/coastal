# Ride Maintenance

This is the workflow for adding or updating a coastal ride segment.

## 1. Add Metadata

Edit:

```text
data/coastal_activities.R
```

Add the segment in geographic order with:

- `activity_id`
- `from`
- `to`
- `ride_direction`: `cw` or `acw`
- `riders`: pipe-delimited initials, for example `TC|SB|WR`
- `ride_start_sample_index`
- `ride_end_sample_index`

Riders in parentheses, for example `(SB)`, represent similar rides that count for XP-style summaries.

## 2. Find Crop Windows

Use:

```r
ride_explorer.R
```

Set `ride_id`, `start_sample_index`, `end_sample_index`, and `context_samples` in the selection-settings block. The script validates and orders the Silver stream, closes its database connection after loading, and creates:

- `crop_map`: the complete activity in grey with the candidate Coastal section highlighted;
- `sample_map`: labelled observations around both boundaries;
- `endpoint_samples`: the boundary indices, timestamps, coordinates, distance and altitude.

Copy the inclusive start/end indices into `ride_start_sample_index` and `ride_end_sample_index`. `time_seconds` is displayed for context but is not a boundary identifier.

The explorer uses the same silver stream loader as the dashboard:

```r
load_activity_stream(ride_id, con)
```

## 3. Add Ferries

If the ride includes ferries, edit:

```text
data/ferries.R
```

Each ferry marker needs:

- `ferry`
- `activity_id`
- `lat`
- `lng`

## 4. Validate

Run:

```r
Rscript scripts/check_inputs.R
```

This validates metadata against the silver `activities` and `activity_streams` tables.

## 5. Render

Render:

```text
index.Rmd
```

This updates:

```text
index.html
```

It may also regenerate rider trace PNGs under `docs/`.

## 6. Publish

After reviewing the generated output, run:

```r
Rscript scripts/publish_dashboard.R
```

This commits and pushes only root `index.html`. Publish only after validation passes.

To validate, render, and publish in one command instead, run:

```sh
scripts/render_and_publish.sh
```
