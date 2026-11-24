# Rider XP methodology

## Purpose

Rider XP is a light-hearted measure of participation in the Coastal project. It
rewards both coastal distance and climbing. It is not intended to measure
athletic performance or compare ride difficulty scientifically.

The owner intends XP to evolve with the geography of the project. The earliest
sections were relatively flat. Later, hillier sections may establish a new
distance-to-climbing equivalence and retrospectively recalibrate the climbing XP
earned from every included section.

## Implemented calculation

The calculation uses the current rows in `rides_index`, one row per curated
coastal section. For section `s`:

```text
distance_s = coastal distance in miles
climb_s    = coastal ascent in metres
ratio_s    = distance_s / climb_s, in miles per metre
```

The calibration ratio is the smallest section ratio:

```text
calibration = min(distance_s / climb_s)
```

For positive distance and climb, selecting the smallest miles-per-metre ratio is
equivalent to selecting the section with the greatest metres climbed per mile.
That section is the benchmark.

The fixed distance rate is:

```text
xp_unit = 15 XP per mile
```

Each rider's totals are calculated as:

```text
distance_xp  = rider_coastal_miles × 15
elevation_xp = rider_coastal_ascent_metres × calibration × 15
total_xp     = distance_xp + elevation_xp
```

On the benchmark section itself, `calibration = distance / climb`. Its two XP
components are consequently equal:

```text
distance XP  = distance × 15
elevation XP = climb × (distance / climb) × 15
             = distance × 15
```

Calculations use unrounded numeric values. The XP plot displays total XP with
`floor()` and displays distance and ascent rounded to one decimal place.

## Rider attribution

The `riders` field in `data/coastal_activities.R` contains pipe-delimited rider
initials. Every listed rider receives the section's complete coastal distance and
ascent; values are not divided among participants.

Parentheses are removed before aggregation. A parenthesised rider therefore also
receives the complete XP contribution, consistent with the Coastal convention
that a similar ride counts for XP.

## Dynamic recalibration

The benchmark is recalculated from the complete current set of curated sections
whenever the dashboard data is built. It is not stored as a fixed constant.

When a newly included section is hillier than the existing benchmark:

1. Its lower miles-per-metre ratio becomes the new calibration.
2. Only riders listed for that section receive its new distance and ascent.
3. The elevation XP for every rider's existing ascent is recalculated with the
   new calibration.
4. Historical total XP and level progress may therefore change retrospectively.

This retrospective movement is intentional. Removing or correcting a benchmark
section can likewise change the calibration in either direction.

### Worked example

Assume the current benchmark is 20 coastal miles with 1,000 metres of ascent:

```text
calibration = 20 / 1,000 = 0.02 miles per metre
elevation multiplier = 0.02 × 15 = 0.3 XP per metre
```

For that benchmark section:

```text
distance XP  = 20 × 15 = 300 XP
elevation XP = 1,000 × 0.3 = 300 XP
```

A rider with 100 total coastal miles and 2,000 total metres of ascent would have:

```text
distance XP  = 100 × 15 = 1,500 XP
elevation XP = 2,000 × 0.3 = 600 XP
total XP     = 2,100 XP
```

If a later section establishes a smaller miles-per-metre ratio, the elevation
multiplier increases and the 600 XP historical climbing contribution is
recalculated.

## Levels and ordering

Level thresholds are defined explicitly in `R/metadata.R`. Level numbers are the
threshold row numbers, beginning at level 1 for the interval above zero XP.

The implementation selects the row where:

```text
total_xp <= next_threshold and total_xp > current_threshold
```

Level progress is:

```text
(total_xp - current_threshold) /
  (next_threshold - current_threshold)
```

An XP total exactly equal to a threshold remains at 100% of the preceding level;
it moves to the next level only when it exceeds that threshold. The final
configured threshold has no following interval, so totals above it do not produce
an XP-plot row.

The XP plot orders riders by unrounded `total_xp`. It does not calculate or
display a numeric rank. Other rider tables have their own ordering and should not
be interpreted as the XP ranking.

## Implementation references and assumptions

- `R/plots.R::draw_xp_plot()` owns the calibration, rider aggregation and XP
  formula.
- `R/metadata.R` owns `xp_unit` and the level thresholds.
- `R/coastal_data.R::create_summary()` supplies coastal distance, ascent and the
  section ratio used by the XP calculation.
- `data/coastal_activities.R` owns section inclusion and rider attribution.

The current implementation expects every included section to provide usable,
non-missing distance and ascent values. The calibration and rider sums do not use
`na.rm = TRUE`; missing values can therefore prevent a valid XP result. This is
an implementation constraint, not a different scoring policy.
