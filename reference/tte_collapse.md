# Collapse time intervals to coarser periods

S7 method that aggregates fine-grained longitudinal data into coarser
time periods. Wraps \[tte_collapse_periods()\].

## Usage

``` r
tte_collapse(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object with trial level data.

- ...:

  Method arguments: \`period_width\` (integer, default 4), \`time_var\`
  (character or NULL; defaults to "trial_week" if it exists from
  \[tte_enroll()\], otherwise uses \`tstop_var\`), \`first_cols\`,
  \`last_cols\`, \`max_cols\`, \`sum_cols\` (character vectors for
  additional aggregation columns).

## Value

The modified \[TTETrial\] object (for chaining).

## Details

This method requires \`data_level == "trial"\`.

Column aggregation is inferred from the design: - \`confounder_vars\`,
\`exposure_var\`, \`person_id_var\` -\> first (baseline values) -
\`time_exposure_var\` -\> last (current status) - \`outcome_vars\` -\>
max (any event in period) - \`isoyearweek\` -\> first (auto-included
when \`admin_censor_isoyearweek\` is set)

The method also creates a \`person_weeks\` column (tstop - tstart).

## See also

\[tte_enroll()\] for the preceding step, \[tte_collapse_periods()\] for
the underlying function

Other tte_methods:
[`tte_enroll()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enroll.md),
[`tte_extract()`](https://papadopoulos-lab.github.io/swereg/reference/tte_extract.md),
[`tte_ipcw_pp()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipcw_pp.md),
[`tte_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipw.md),
[`tte_irr()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr.md),
[`tte_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr_combine.md),
[`tte_km()`](https://papadopoulos-lab.github.io/swereg/reference/tte_km.md),
[`tte_prepare_outcome()`](https://papadopoulos-lab.github.io/swereg/reference/tte_prepare_outcome.md),
[`tte_rates()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates.md),
[`tte_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates_combine.md),
[`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md),
[`tte_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_summary.md),
[`tte_table1()`](https://papadopoulos-lab.github.io/swereg/reference/tte_table1.md),
[`tte_truncate()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate.md),
[`tte_weight_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weight_summary.md),
[`tte_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
trial <- tte_trial(data, design) |>
  tte_collapse(period_width = 4)
} # }
```
