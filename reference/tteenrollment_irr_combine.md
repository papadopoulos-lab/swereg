# Combine and format multiple irr outputs into a publication-ready table

Combine and format multiple irr outputs into a publication-ready table

## Usage

``` r
tteenrollment_irr_combine(results, slot, descriptions = NULL)
```

## Arguments

- results:

  Named list of per-ETT result lists.

- slot:

  Character scalar: name of the slot with \`\$irr()\` output.

- descriptions:

  Optional named character vector mapping ett_id to descriptions.

## Value

A data.table with formatted IRR estimates.

## See also

Other tte_methods:
[`tteenrollment_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rates_combine.md),
[`tteenrollment_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rbind.md)
