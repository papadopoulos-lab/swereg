# Combine multiple enrollment objects

Combines multiple \[TTEEnrollment\] objects by row-binding their data.
Used for batched processing where data is too large to fit in memory at
once.

## Usage

``` r
tteenrollment_rbind(trials)
```

## Arguments

- trials:

  A list of \[TTEEnrollment\] objects to combine.

## Value

A new \[TTEEnrollment\] object with combined data.

## Details

All trials must have the same design and data_level. The combined trial
inherits: - The design and data_level from the first trial - The
intersection of steps_completed from all trials - The union of
weight_cols from all trials

## See also

Other tte_methods:
[`tteenrollment_combined_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_combined_combine.md),
[`tteenrollment_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_irr_combine.md),
[`tteenrollment_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rates_combine.md)

## Examples

``` r
if (FALSE) { # \dontrun{
trials <- lapply(files, function(f) {
  TTEEnrollment$new(load_data(f), design, ratio = 2)
})
combined <- tteenrollment_rbind(trials)
combined$s2_ipw()
} # }
```
