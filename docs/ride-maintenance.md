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

The legacy `ride_start_time` and `ride_end_time` values are temporarily retained on migrated rows as rollback evidence. They are neither validated nor used to trim streams. New sections should use `NA_integer_` for these legacy fields during the transition.

## 2. Find Crop Windows

Use:

```r
ride_explorer.R
```

Set `ride_id`, inspect the stream on the map, and choose the inclusive `ride_start_sample_index` / `ride_end_sample_index` values that identify the coastal segment. `time_seconds` is displayed for context but is not a boundary identifier.

The explorer uses the same silver stream loader as the dashboard:

```r
load_activity_stream(ride_id, con)
```

## Boundary Migration Audit

Before replacing timestamp crop windows with Silver sample-index boundaries, run the read-only audit:

```sh
Rscript scripts/audit_boundary_migration.R
```

To retain a local CSV report, provide an explicit output path:

```sh
Rscript scripts/audit_boundary_migration.R \
  --output=outputs/audits/coastal_boundary_migration.csv
```

The audit queries canonical Silver streams but performs no database writes and does not modify `data/coastal_activities.R`. It classifies every configured section as `EXACT_MATCH`, `REQUIRES_REVIEW`, or `UNRESOLVED`.

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
