# Combine and format multiple rates outputs into a publication-ready table

Combine and format multiple rates outputs into a publication-ready table

## Usage

``` r
tte_rates_combine(results, slot, descriptions = NULL)
```

## Arguments

- results:

  Named list of per-ETT result lists.

- slot:

  Character scalar: name of the slot with \`\$rates()\` output.

- descriptions:

  Optional named character vector mapping ett_id to descriptions.

## Value

A data.table in wide format.

## See also

Other tte_methods:
[`tte_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr_combine.md),
[`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md)
