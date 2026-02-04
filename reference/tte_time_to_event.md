# Calculate time to first event for each trial

Computes the time (tstop) of the first period in which an event occurs
for each trial. This is a common preprocessing step for survival
analysis in target trial emulation.

## Usage

``` r
tte_time_to_event(
  data,
  id_var,
  event_cols,
  tstop_var = "tstop",
  prefix = "weeks_to_"
)
```

## Arguments

- data:

  A data.table in counting-process format.

- id_var:

  Character, name of the trial identifier column.

- event_cols:

  Character vector, names of event indicator columns (binary: 1 = event
  occurred, 0 = no event).

- tstop_var:

  Character, name of the period end time column (default: "tstop").

- prefix:

  Character, prefix for output columns (default: "weeks_to\_").

## Value

The input data.table with added columns `prefix + event_col` for each
event column. Each contains the tstop of the first period with an event,
or NA if no event occurred.

## Details

For each trial and each event type, this function finds the first period
where the event indicator equals 1, and returns the tstop (end time) of
that period. This represents "time to event" in discrete-time survival
analysis.

The result is trial-level (constant within each trial) and is typically
used for:

- Determining censoring times

- Creating survival outcomes

- Defining analysis endpoints

## See also

Other tte_data_prep:
[`tte_collapse_periods()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse_periods.md)

## Examples

``` r
library(data.table)
dt <- data.table(
  trial_id = rep(1:3, each = 4),
  tstart = rep(c(0, 4, 8, 12), 3),
  tstop = rep(c(4, 8, 12, 16), 3),
  death = c(0,0,1,0, 0,0,0,0, 0,1,0,0),
  hosp = c(0,1,0,0, 0,0,0,1, 0,0,0,0)
)
result <- tte_time_to_event(dt, "trial_id", c("death", "hosp"))
# Trial 1: weeks_to_death = 12, weeks_to_hosp = 8
# Trial 2: weeks_to_death = NA, weeks_to_hosp = 16
# Trial 3: weeks_to_death = 8, weeks_to_hosp = NA
```
