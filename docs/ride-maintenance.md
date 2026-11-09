# Ride Maintenance

This is the workflow for adding or updating a coastal ride segment.

## 1. Discover Boundaries

Open the standalone Activity Inspector from the sibling `cycling-analytics` repository. Launch and usage instructions are in its [Activity Inspector guide](https://github.com/tim-jc/cycling-analytics/blob/main/docs/activity-inspector.md).

1. Load the relevant Strava activity ID.
2. Inspect the full route and select the coastal section.
3. Refine the canonical start and end sample indices.
4. Copy the inclusive boundaries.

The Activity Inspector is read-only: it helps discover boundaries but does not update Coastal. `cycling-platform` owns the canonical Silver activity and stream data.

## 2. Update Coastal Metadata

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

Add the segment manually in geographic order using the boundaries copied from the Activity Inspector. Coastal's `data/coastal_activities.R` is the authoritative source for section boundaries and its calculations and published outputs consume those definitions. Sample-index boundaries are inclusive; timestamps are not boundary identifiers.

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
