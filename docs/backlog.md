# Backlog

These items are non-blocking follow-up work. They are not part of the accepted
measurement-ordering correction.

## Section start-marker sample selection

**Status:** Deferred, non-blocking.

Section start markers currently select the observation with the minimum
`time_seconds`. Investigate selecting the canonical first recorded observation
by `sample_index` instead, including repeated and non-monotonic timestamp cases.
Preserve curated boundaries, trace direction and marker presentation.

## XP above the final level threshold

**Status:** Deferred, non-blocking.

The final configured XP threshold has no following interval. A rider above that
threshold therefore produces no row for the current XP plot. Decide the intended
terminal-level presentation before changing threshold or plot behaviour.

## XP resilience to invalid measurement inputs

**Status:** Deferred, non-blocking.

XP calibration and rider aggregation currently expect complete, finite section
distance and ascent values. Investigate explicit validation and failure behaviour
for missing, zero, non-finite or otherwise invalid measurements. Do not silently
drop evidence or change the accepted dynamic calibration methodology.
