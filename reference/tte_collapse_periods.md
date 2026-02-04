# Collapse time intervals to coarser periods

Aggregates fine-grained longitudinal data (e.g., weekly) into coarser
time periods (e.g., 4-week periods) for target trial emulation. This
reduces data size and can improve model stability.

## Usage

``` r
tte_collapse_periods(
  data,
  id_var,
  time_var,
  period_width = 4L,
  first_cols = NULL,
  last_cols = NULL,
  max_cols = NULL,
  sum_cols = NULL
)
```

## Arguments

- data:

  A data.table in counting-process format with one row per fine-grained
  time unit (e.g., week).

- id_var:

  Character, name of the trial/person identifier column.

- time_var:

  Character, name of the time index column (e.g., week number within
  trial, starting at 0).

- period_width:

  Integer, number of time units per period (default: 4).

- first_cols:

  Character vector, columns to aggregate using first value (e.g.,
  baseline characteristics, confounders).

- last_cols:

  Character vector, columns to aggregate using last value (e.g.,
  time-varying exposure status).

- max_cols:

  Character vector, columns to aggregate using max (e.g., event
  indicators - any event in period = 1).

- sum_cols:

  Character vector, columns to aggregate using sum (e.g., counts,
  default: NULL).

## Value

A data.table with one row per period per trial, containing:

- tstart:

  Period start time (period \* period_width)

- tstop:

  Period end time (period \* period_width + period_width)

- Aggregated columns:

  As specified by first_cols, last_cols, max_cols, sum_cols

## Details

This function implements a common preprocessing step in target trial
emulation where weekly registry data is collapsed to 4-week (monthly)
periods. This:

- Reduces dataset size by ~4x (for 4-week periods)

- Improves computational efficiency

- Can improve model stability with sparse events

The aggregation rules are:

- `first_cols`: Take first value in period (for time-invariant or
  baseline values)

- `last_cols`: Take last value in period (for time-varying status at
  period end)

- `max_cols`: Take maximum (for event indicators: any event in period
  counts as event)

- `sum_cols`: Take sum (for counts)

## See also

Other tte_data_prep:
[`tte_time_to_event()`](https://papadopoulos-lab.github.io/swereg/reference/tte_time_to_event.md)

## Examples

``` r
library(data.table)
# Weekly data for 2 trials, 8 weeks each
dt <- data.table(
  trial_id = rep(1:2, each = 8),
  week = rep(0:7, 2),
  exposed = c(rep(TRUE, 8), rep(FALSE, 8)),
  age = rep(c(55, 60), each = 8),
  event = c(0,0,0,1,0,0,0,0, 0,0,0,0,0,0,1,0)
)
# Collapse to 4-week periods
result <- tte_collapse_periods(
  dt,
  id_var = "trial_id",
  time_var = "week",
  period_width = 4,
  first_cols = c("age"),
  last_cols = c("exposed"),
  max_cols = c("event")
)
```
