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
- `ride_start_time`
- `ride_end_time`

Riders in parentheses, for example `(SB)`, represent similar rides that count for XP-style summaries.

## 2. Find Crop Windows

Use:

```r
ride_explorer.R
```

Set `ride_id`, inspect the stream on the map, and choose the `ride_start_time` / `ride_end_time` values to crop to the coastal segment.

The explorer uses the same silver stream loader as the dashboard:

```r
load_activity_stream()
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
docs/index.html
```

It may also regenerate rider trace PNGs under `docs/`.

## 6. Publish

Commit generated dashboard output only after validation passes. `docs/index.html` is served by GitHub Pages, so pushing it publishes the dashboard.
