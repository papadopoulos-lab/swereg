# Combine and format multiple tte_rates() outputs into a publication-ready table

Extracts \[tte_rates()\] outputs from a named results list and returns a
formatted wide data.table with events, person-years, and rates by
exposure.

## Usage

``` r
tte_rates_combine(results, slot, descriptions = NULL)
```

## Arguments

- results:

  Named list of per-ETT result lists. Each element must contain a slot
  named by \`slot\` holding a \[tte_rates()\] output. Names are used as
  \`ett_id\` values.

- slot:

  Character scalar: name of the slot within each result that contains
  the \[tte_rates()\] output (e.g., \`"table2"\`).

- descriptions:

  Optional named character vector mapping ett_id to descriptions (e.g.,
  \`c(ETT01 = "Psychosis, 52w, 40-44")\`). If provided, a
  \`description\` column is included in the output.

## Value

A data.table in wide format with columns: \`ett_id\`, \`description\`
(if provided), \`events_weighted_Exposed\`,
\`events_weighted_Unexposed\`, \`py_weighted_Exposed\`,
\`py_weighted_Unexposed\`, \`rate_per_100000py_Exposed\`,
\`rate_per_100000py_Unexposed\`.

Formatting: events to 1 decimal place, person-years as comma-separated
integers, rates to 1 decimal place.

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
[`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md),
[`tte_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_summary.md),
[`tte_table1()`](https://papadopoulos-lab.github.io/swereg/reference/tte_table1.md),
[`tte_truncate()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate.md),
[`tte_weight_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weight_summary.md),
[`tte_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ett_desc <- setNames(ett$description, ett$ett_id)
table2 <- tte_rates_combine(results, "table2", ett_desc)
} # }
```
