# Combine and format multiple irr outputs into a publication-ready table

Combine and format multiple irr outputs into a publication-ready table

## Usage

``` r
tteenrollment_irr_combine(
  results,
  slot,
  descriptions = NULL,
  conf_level = 0.95
)
```

## Arguments

- results:

  Named list of per-ETT result lists.

- slot:

  Character scalar: name of the slot with `$irr()` output.

- descriptions:

  Optional named character vector mapping ett_id to descriptions.

- conf_level:

  Numeric(1) strictly between 0 and 1, the level the `$irr()` intervals
  in `slot` were computed at. The interval column header states it, so
  `0.9` gives `90% CI`. The function does not recompute the interval.
  Default: 0.95.

## Value

A data.table with formatted IRR estimates.

## See also

Other tte_methods:
[`tteenrollment_combined_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_combined_combine.md),
[`tteenrollment_fill_aggregates()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_aggregates.md),
[`tteenrollment_fill_followup_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_followup_confounders.md),
[`tteenrollment_fill_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_summary.md),
[`tteenrollment_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rates_combine.md),
[`tteenrollment_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rbind.md)
