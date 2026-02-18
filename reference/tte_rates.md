# Calculate events, person-years, and rates by exposure group

S7 method that computes weighted events, person-years, and incidence
rates per 100,000 person-years, stratified by exposure group.

## Usage

``` r
tte_rates(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object with trial level data.

- ...:

  Method arguments: \`weight_col\` (character, required) - the column
  name containing per-protocol weights (e.g., "weight_pp_trunc").

## Value

A data.table with columns:

- baseline_exposed:

  Exposure group (TRUE/FALSE)

- n_trials:

  Number of unique trials

- events_weighted:

  Weighted sum of events

- py_weighted:

  Weighted person-years (person_weeks / 52.25)

- rate_per_100000py:

  Incidence rate per 100,000 person-years

The result carries attributes \`swereg_type = "rates"\` and
\`exposure_var\` (the exposure column name), used by
\[tte_rates_combine()\].

## Details

This method requires \`data_level == "trial"\`.

## See also

Other tte_methods:
[`tte_collapse()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse.md),
[`tte_enroll()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enroll.md),
[`tte_extract()`](https://papadopoulos-lab.github.io/swereg/reference/tte_extract.md),
[`tte_ipcw_pp()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipcw_pp.md),
[`tte_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipw.md),
[`tte_irr()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr.md),
[`tte_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr_combine.md),
[`tte_km()`](https://papadopoulos-lab.github.io/swereg/reference/tte_km.md),
[`tte_prepare_outcome()`](https://papadopoulos-lab.github.io/swereg/reference/tte_prepare_outcome.md),
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
table2 <- trial |> tte_rates(weight_col = "weight_pp_trunc")
} # }
```
