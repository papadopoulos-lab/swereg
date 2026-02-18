# Extract the data.table from a trial object

Returns the processed data.table from a \[TTETrial\] object.

## Usage

``` r
tte_extract(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object.

- ...:

  Additional arguments (unused).

## Value

A data.table with the processed trial data.

## See also

Other tte_methods:
[`tte_collapse()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse.md),
[`tte_enroll()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enroll.md),
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
analysis_data <- trial |>
  tte_ipw() |>
  tte_extract()
} # }
```
