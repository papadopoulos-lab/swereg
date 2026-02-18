# Generate baseline characteristics table

S7 method that creates a baseline characteristics table, either weighted
(using IPW) or unweighted. Wraps \[tableone::CreateTableOne()\] for
unweighted tables and \[tableone::svyCreateTableOne()\] for weighted
tables.

## Usage

``` r
tte_table1(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object with trial level data.

- ...:

  Method arguments: \`ipw_col\` (character or NULL, default NULL). If
  NULL, creates an unweighted table. If specified, creates a weighted
  table using the specified column as survey weights.

## Value

A tableone object with baseline characteristics by exposure group.

## Details

This method requires \`data_level == "trial"\`.

The table is stratified by the exposure variable from the design.
Baseline data is extracted as the first row per trial (where tstart ==
0).

Variables included are the \`confounder_vars\` from the design. The
table includes NA counts and an overall column.

## See also

\[tableone::CreateTableOne()\], \[tableone::svyCreateTableOne()\]

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
[`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md),
[`tte_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_summary.md),
[`tte_truncate()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate.md),
[`tte_weight_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weight_summary.md),
[`tte_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Unweighted table
table1_unweighted <- trial |> tte_table1()

# IPW-weighted table
table1_weighted <- trial |> tte_table1(ipw_col = "ipw_trunc")
} # }
```
