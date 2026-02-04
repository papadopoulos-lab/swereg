# Prepare outcome-specific data for per-protocol analysis

S7 method that combines all outcome-specific preparation into one step:
computing event times, censoring times, filtering data, and creating
indicators for IPCW analysis.

## Usage

``` r
tte_prepare_outcome(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object with trial level data.

- ...:

  Method arguments: \`outcome\` (character, must be in
  design@outcome_vars).

## Value

The modified \[TTETrial\] object (for chaining).

## Details

This method requires \`data_level == "trial"\` and can only be run once
per trial object because it deletes rows.

The method computes four \`weeks_to_X\` values per trial: -
\`weeks_to_event\`: tstop of first period where outcome == 1 -
\`weeks_to_protocol_deviation\`: tstop of first period with treatment
deviation - \`weeks_to_admin_end\`: weeks from entry isoyearweek to
admin_censor_isoyearweek - \`weeks_to_loss\`: max tstop if trial ended
before any planned stop

All \`weeks_to_X\` variables are aligned to period boundaries (multiples
of the collapsed period width). This ensures consistent behavior in
\`pmin()\` comparisons. \`weeks_to_admin_end\` is rounded DOWN to the
nearest period boundary, meaning partial periods at study end are
excluded (conservative approach). A warning is issued if any trials
entered less than one period before the administrative end date (these
trials have no complete periods and will be dropped).

Then computes: - \`censor_week\`: pmin of the four weeks_to values -
\`event\`: 1 if tstop == weeks_to_event, else 0 -
\`censor_this_period\`: 1 if tstop == weeks_to_protocol_deviation OR
tstop == weeks_to_loss, else 0

Data is filtered to rows where tstop \<= censor_week.

## See also

\[tte_ipcw_pp()\] for the next step in the per-protocol workflow

Other tte_methods:
[`tte_collapse()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse.md),
[`tte_enroll()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enroll.md),
[`tte_extract()`](https://papadopoulos-lab.github.io/swereg/reference/tte_extract.md),
[`tte_ipcw_pp()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipcw_pp.md),
[`tte_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipw.md),
[`tte_irr()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr.md),
[`tte_km()`](https://papadopoulos-lab.github.io/swereg/reference/tte_km.md),
[`tte_rates()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates.md),
[`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md),
[`tte_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_summary.md),
[`tte_table1()`](https://papadopoulos-lab.github.io/swereg/reference/tte_table1.md),
[`tte_truncate()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate.md),
[`tte_weight_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weight_summary.md),
[`tte_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load trial object and prepare for specific outcome
trial <- qs::qread("trial_with_ipw.qs") |>
  tte_prepare_outcome(outcome = "death") |>
  tte_ipcw(censoring_var = "censor_this_period") |>
  tte_weights() |>
  tte_truncate()
} # }
```
