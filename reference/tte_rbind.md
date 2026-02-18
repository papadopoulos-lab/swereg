# Combine multiple trial objects

Combines multiple \[TTETrial\] objects by row-binding their data. Used
for batched processing where data is too large to fit in memory at once.

## Usage

``` r
tte_rbind(trials)
```

## Arguments

- trials:

  A list of \[TTETrial\] objects to combine.

## Value

A new \[TTETrial\] object with combined data.

## Details

All trials must have the same design and data_level. The combined trial
inherits: - The design and data_level from the first trial - The
intersection of steps_completed from all trials - The union of
weight_cols from all trials

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
[`tte_rates()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates.md),
[`tte_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates_combine.md),
[`tte_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_summary.md),
[`tte_table1()`](https://papadopoulos-lab.github.io/swereg/reference/tte_table1.md),
[`tte_truncate()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate.md),
[`tte_weight_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weight_summary.md),
[`tte_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Process batches through enrollment
trials <- lapply(files, function(f) {
  tte_trial(load_data(f), design) |>
    tte_enroll(ratio = 2)
})

# Combine for collapse and weighting
combined <- tte_rbind(trials) |>
  tte_collapse(period_width = 4) |>
  tte_ipw()
} # }
```
