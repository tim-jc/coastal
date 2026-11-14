# Data Contract

The coastal project consumes activity summaries and canonical activity-stream samples from the Silver layer of `cycling-platform`.

## Source Schema

Default schema:

```text
cycling_platform_silver
```

The schema can be overridden locally with:

```text
CYCLING_PLATFORM_SILVER_SCHEMA
```

MariaDB connectivity uses `MARIADB_HOST`, `MARIADB_PORT`, `MARIADB_USER`, and
`MARIADB_PASSWORD`. Queries are schema-qualified, so Coastal does not set a
default database name.

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

- `load_activity_stream(ride_id, connection)`
- `get_coastal_rides(connection)`
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
- every inclusive sample-index crop window returns at least one stream row;
- every ride summary has whole-ride distance from silver `activities`;
- ferry markers reference known coastal activity IDs.

## Validation Failure Interpretation

`Rscript scripts/check_inputs.R` is the authoritative pre-render validation. Its failures should be interpreted according to the condition reported:

- **Missing activity records:** one or more Coastal `activity_id` values are absent from Silver `activities`.
- **Missing stream records:** one or more Coastal activities have no rows in Silver `activity_streams`.
- **Empty cropped results:** stream rows exist, but an inclusive Coastal sample-index boundary selects no observations.
- **Database connectivity failure:** Coastal could not reach or authenticate to MariaDB, so table existence and population were not evaluated.

These conditions are not interchangeable. In particular, a connection failure or a missing Coastal activity does not establish that the complete Silver stream table is empty.

## Resolved Historical Incident

In June 2026, Coastal recorded an upstream blocker after its input check found no usable Silver stream rows for the configured Coastal activities. The Platform Silver stream repair/backfill and Coastal migration were completed in July 2026. This incident is retained as historical context and is not a current operational-status claim.
