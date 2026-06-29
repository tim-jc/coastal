# Data Contract

The coastal project consumes data from the silver layer of `cycling-platform`.

## Source Schema

Default schema:

```text
cycling_platform_silver
```

The schema can be overridden locally with:

```text
CYCLING_PLATFORM_SILVER_SCHEMA
```

## Tables

### `activities`

Required by:

- `load_gps_data()`
- `create_summary()`
- `scripts/check_inputs.R`

Required columns:

| Silver column | Project use |
| --- | --- |
| `activity_id` | joins to coastal metadata |
| `start_datetime_local` | renamed to `start_date_local` |
| `distance_metres` | converted to `distance_whole_ride_miles` |

### `activity_streams`

Required by:

- `load_activity_stream()`
- `get_coastal_rides()`
- `scripts/check_inputs.R`

Required columns:

| Silver column | Project alias |
| --- | --- |
| `activity_id` | `activity_id` |
| `sample_index` | `sample_index` |
| `time_seconds` | `time_seconds` |
| `distance_metres` | `distance` |
| `latitude` | `latitude`, `lat` |
| `longitude` | `longitude`, `lng` |
| `altitude_metres` | `altitude` |
| `velocity_smooth_metres_per_second` | `velocity_smooth` |
| `heartrate_bpm` | `heartrate` |
| `cadence_rpm` | `cadence` |
| `watts` | `watts` |
| `temperature_celsius` | `temp` |
| `is_moving` | `moving` |
| `grade_smooth_percent` | `grade_smooth` |

## Invariants

`scripts/check_inputs.R` expects:

- every `activity_id` in `data/coastal_activities.R` exists in silver `activities`;
- every `activity_id` has rows in silver `activity_streams`;
- every coastal crop window returns at least one stream row;
- every ride summary has whole-ride distance from silver `activities`;
- ferry markers reference known coastal activity IDs.

## Current Known Blocker

The silver `activity_streams` table is currently expected by the project, but the most recent check found it empty. Until the upstream silver stream bug is fixed, `scripts/check_inputs.R` should fail and generated dashboard output should not be published.
