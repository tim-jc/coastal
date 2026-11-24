# Measurement-ordering correction acceptance

## Status

Accepted by the owner on 24 November 2026 after production reconciliation and
local dashboard visual review.

The correction calculates physical distance and ascent in ascending canonical
Silver `sample_index` order. It applies `cw` or `acw` ordering only after those
measurements have been attached to observations, preserving the curated map
trace assembly without allowing display direction to govern physical metrics.

The implementation is in `R/coastal_data.R::enrich_activity_streams()`. The
section boundaries and manual Coastal selection policy are unchanged.

## Production reconciliation

The owner ran `scripts/reconcile_measurement_ordering.R` locally against the
existing read-only Silver inputs for all 48 curated sections. Both legacy and
corrected calculations used the same observations.

The accepted results were:

| Measure | Result |
| --- | ---: |
| Curated sections evaluated | 48 |
| Sections with changed ascent | 38 |
| Sections with changed distance | 0 |
| Legacy coastal distance | 3,869.5737 miles |
| Corrected coastal distance | 3,869.5737 miles |
| Legacy coastal ascent | 67,439.4 metres |
| Corrected coastal ascent | 67,657.4 metres |
| Net ascent change | +218.0 metres (+0.32%) |
| Largest section increase | +160.0 metres |
| Largest section decrease | -123.0 metres |

No selected section contained missing or invalid required observations. The
selected intervals contained no duplicate timestamps, non-monotonic timestamps
or timestamp-order measurement effects. All selected sample intervals were
contiguous.

The owner reviewed the resulting dashboard locally and accepted the displayed
measurement changes.

## XP reconciliation

The XP benchmark remained `Newport -> Machynlleth` before and after correction.

| Measure | Legacy | Corrected |
| --- | ---: | ---: |
| Benchmark ascent | 2,703.2 m | 2,658.0 m |
| Benchmark elevation per mile | 31.9060 m/mi | 31.3725 m/mi |
| Elevation XP multiplier | 0.4701305 XP/m | 0.4781252 XP/m |

All eight rider XP changes were reproduced from the corrected ascent totals and
the recalculated multiplier. Distance XP was unchanged. No rider changed XP
level or ordering. The owner confirmed that this retrospective recalibration is
intentional and accepted; its methodology is documented in
[`xp-methodology.md`](xp-methodology.md).

## Retained validation evidence

- `scripts/reconcile_measurement_ordering.R` provides the reproducible read-only
  production comparison.
- `tests/testthat/test-measurement-reconciliation.R` verifies legacy and corrected
  calculations with synthetic clockwise, anticlockwise, repeated-timestamp and
  non-monotonic-timestamp cases.
- `tests/testthat/test-coastal-data.R` verifies recorded-order measurements,
  display reversal, distance invariance, inclusive boundaries and downstream
  summary propagation.

Reconciliation CSVs remain local under `outputs/measurement_reconciliation/`.
They are ignored and are not published or committed as acceptance artifacts.

## Deferred work

The correction is accepted without the non-blocking items recorded in
[`backlog.md`](backlog.md). None changes the accepted measurement methodology.
