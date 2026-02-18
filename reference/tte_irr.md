# Fit Poisson models and extract incidence rate ratios

S7 method that fits quasipoisson regression models and extracts IRR
estimates. Fits two models: (1) constant hazard, (2) flexible hazard
with natural splines. Uses survey-weighted GLM with cluster-robust
standard errors.

## Usage

``` r
tte_irr(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object with trial level data.

- ...:

  Method arguments: \`weight_col\` (character, required) - the column
  name containing per-protocol weights.

## Value

A data.table with one row containing: \`IRR_const\` (IRR from constant
hazard model), \`IRR_const_lower\` and \`IRR_const_upper\` (95% CI),
\`IRR_const_pvalue\`, \`IRR_flex\` (IRR from flexible hazard model),
\`IRR_flex_lower\` and \`IRR_flex_upper\` (95% CI), \`IRR_flex_pvalue\`,
\`warn_const\` and \`warn_flex\` (logical flags for convergence
warnings).

The result carries attribute \`swereg_type = "irr"\`, used by
\[tte_irr_combine()\].

## Details

This method requires \`data_level == "trial"\`.

The constant hazard model assumes a constant baseline hazard over time.
The flexible hazard model uses natural splines (df=3) to allow
non-constant baseline hazard, testing whether the constant hazard
assumption is reasonable.

Both models use quasipoisson family to account for overdispersion and
offset(log(person_weeks)) for the person-time denominator.

## See also

Other tte_methods:
[`tte_collapse()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse.md),
[`tte_enroll()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enroll.md),
[`tte_extract()`](https://papadopoulos-lab.github.io/swereg/reference/tte_extract.md),
[`tte_ipcw_pp()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipcw_pp.md),
[`tte_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipw.md),
[`tte_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr_combine.md),
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
irr <- trial |> tte_irr(weight_col = "weight_pp_trunc")
cat("IRR:", irr$IRR_const, "(", irr$IRR_const_lower, "-", irr$IRR_const_upper, ")\n")
} # }
```
