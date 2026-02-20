# Combine and format multiple irr outputs into a publication-ready table

Combine and format multiple irr outputs into a publication-ready table

## Usage

``` r
tte_irr_combine(results, slot, descriptions = NULL)
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
[`tte_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates_combine.md),
[`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md)
