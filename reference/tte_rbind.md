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
[`tte_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr_combine.md),
[`tte_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates_combine.md)

## Examples

``` r
if (FALSE) { # \dontrun{
trials <- lapply(files, function(f) {
  tte_trial(load_data(f), design)$enroll(ratio = 2)
})
combined <- tte_rbind(trials)
combined$collapse(period_width = 4)$ipw()
} # }
```
