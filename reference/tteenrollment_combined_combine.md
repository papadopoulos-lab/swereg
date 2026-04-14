# Combine rates + IRR outputs into a single wide publication-ready table

Calls \[tteenrollment_rates_combine()\] and
\[tteenrollment_irr_combine()\] with shared \`descriptions\`, then
left-joins on \`ett_id\` so that each row carries per-arm event counts,
person-years, rates, and the incidence rate ratio (with 95

## Usage

``` r
tteenrollment_combined_combine(
  results,
  rates_slot,
  irr_slot,
  descriptions = NULL
)
```

## Arguments

- results:

  Named list of per-ETT result lists.

- rates_slot:

  Character scalar, name of the slot with \`\$rates()\` output (e.g.
  \`"rates_pp_trunc"\`).

- irr_slot:

  Character scalar, name of the slot with \`\$irr()\` output (e.g.
  \`"irr_pp_trunc"\`).

- descriptions:

  Optional named character vector mapping \`ett_id\` to descriptions.

## Value

A wide \`data.table\` with one row per ETT.

## Details

The returned data.table still uses the generic
\`\_Exposed\`/\`\_Unexposed\` column suffixes from
\[tteenrollment_rates_combine()\]. The workbook writer in
\`.write_combined_rates_irr()\` applies \`.rename_exposure_columns()\`
afterwards when the featured ETTs share a single enrollment.

## See also

Other tte_methods:
[`tteenrollment_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_irr_combine.md),
[`tteenrollment_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rates_combine.md),
[`tteenrollment_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rbind.md)
