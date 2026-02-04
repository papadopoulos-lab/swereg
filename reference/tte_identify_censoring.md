# Identify protocol deviation and loss to follow-up for per-protocol analysis

Detects informative censoring events in longitudinal data for target
trial emulation. Protocol deviation occurs when treatment status changes
from baseline assignment. Loss to follow-up occurs when follow-up ends
earlier than expected (e.g., death, emigration) without administrative
censoring.

## Usage

``` r
tte_identify_censoring(
  data,
  exposure_var,
  baseline_exposure_var,
  id_var = "trial_id",
  tstart_var = "tstart",
  tstop_var = "tstop",
  follow_up_var = NULL,
  admin_censor_var = NULL
)
```

## Arguments

- data:

  A data.table in counting-process format (one row per person per time
  period).

- exposure_var:

  Character, name of the time-varying exposure column (current treatment
  status at each period).

- baseline_exposure_var:

  Character, name of the baseline treatment assignment column.

- id_var:

  Character, name of the trial identifier column (default: "trial_id").

- tstart_var:

  Character, name of the period start time column (default: "tstart").

- tstop_var:

  Character, name of the period end time column (default: "tstop").

- follow_up_var:

  Character, name of the expected follow-up duration column. If NULL,
  loss to follow-up is not calculated (default: NULL).

- admin_censor_var:

  Character, name of a column indicating the administrative censoring
  boundary (e.g., study end date). If NULL, administrative censoring is
  not distinguished from loss to follow-up (default: NULL).

## Value

The input data.table with added columns:

- protocol_deviated:

  Logical, TRUE if current exposure differs from baseline assignment or
  is missing (NA)

- weeks_to_protocol_deviation:

  Integer, tstop of first period with protocol deviation (NA if never
  deviated)

- weeks_to_loss:

  Integer, max tstop if loss to follow-up occurred (NA otherwise). Only
  calculated if \`follow_up_var\` is provided.

- censored:

  Logical, TRUE if protocol_deviated or lost to follow-up

## Details

In per-protocol analysis, participants are censored when they deviate
from their assigned treatment. This function identifies:

\*\*Protocol deviation:\*\*

- Exposed at baseline who stop/switch treatment

- Unexposed at baseline who start treatment

- Missing exposure data (cannot confirm adherence)

\*\*Loss to follow-up:\*\*

- Follow-up ended before expected duration

- No protocol deviation occurred

- Not due to administrative censoring (study end)

These censoring events are typically informative (related to prognosis)
and require IPCW-PP adjustment for valid per-protocol effect estimation.

## See also

[`tte_calculate_ipcw`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md)

Other tte_weights:
[`tte_calculate_ipcw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md),
[`tte_calculate_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipw.md),
[`tte_combine_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_combine_weights.md),
[`tte_truncate_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate_weights.md)

## Examples

``` r
library(data.table)
# Create example counting-process data
dt <- data.table(
  trial_id = rep(1:3, each = 4),
  tstart = rep(0:3, 3),
  tstop = rep(1:4, 3),
  baseline_exposed = rep(c(TRUE, TRUE, FALSE), each = 4),
  current_exposed = c(TRUE, TRUE, FALSE, FALSE,  # Person 1 deviates at week 3
                      TRUE, TRUE, TRUE, TRUE,    # Person 2 adheres
                      FALSE, FALSE, TRUE, TRUE)  # Person 3 deviates at week 3
)
result <- tte_identify_censoring(
  data = dt,
  exposure_var = "current_exposed",
  baseline_exposure_var = "baseline_exposed"
)
```
