# Combine and format multiple tte_irr() outputs into a publication-ready table

Extracts \[tte_irr()\] outputs from a named results list and returns a
formatted data.table with IRR estimates, confidence intervals, and
p-values. Prints a message listing any ETTs with convergence warnings.

## Usage

``` r
tte_irr_combine(results, slot, descriptions = NULL)
```

## Arguments

- results:

  Named list of per-ETT result lists. Each element must contain a slot
  named by \`slot\` holding a \[tte_irr()\] output. Names are used as
  \`ett_id\` values.

- slot:

  Character scalar: name of the slot within each result that contains
  the \[tte_irr()\] output (e.g., \`"table3"\`).

- descriptions:

  Optional named character vector mapping ett_id to descriptions. If
  provided, a \`description\` column is included.

## Value

A data.table with columns: \`ett_id\`, \`description\` (if provided),
\`IRR (constant)\`, \`95 \`IRR (flexible)\`, \`95

IRR values are formatted to 2 decimal places with "#" appended if the
model had convergence warnings. CIs are formatted as "lower-upper"
(2dp). P-values use \[format.pval()\] with 3 significant digits.

## See also

Other tte_methods:
[`tte_collapse()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse.md),
[`tte_enroll()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enroll.md),
[`tte_extract()`](https://papadopoulos-lab.github.io/swereg/reference/tte_extract.md),
[`tte_ipcw_pp()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipcw_pp.md),
[`tte_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipw.md),
[`tte_irr()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr.md),
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
ett_desc <- setNames(ett$description, ett$ett_id)
table3 <- tte_irr_combine(results, "table3", ett_desc)
} # }
```
