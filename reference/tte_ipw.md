# Calculate inverse probability of treatment weights

S7 method that calculates IPW for baseline confounding adjustment. Wraps
\[tte_calculate_ipw()\].

## Usage

``` r
tte_ipw(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object with trial level data.

- ...:

  Method arguments: \`stabilize\` (logical, default TRUE).

## Value

The modified \[TTETrial\] object (for chaining).

## Details

This method requires \`data_level == "trial"\`.

\*\*Problem:\*\* Exposed and unexposed groups may differ in baseline
characteristics (e.g., age, education) that also affect the outcome.
This confounds the treatment-outcome relationship.

\*\*Solution:\*\* Reweight the sample so that confounders are balanced
across groups. People who are unlikely to be exposed (given their
characteristics) but ARE exposed get upweighted; people likely to be
exposed who ARE exposed get downweighted. This creates a
"pseudo-population" where treatment is independent of confounders.

\*\*Notation:\*\*

- A = treatment (exposed vs unexposed)

- L = confounders (age, education, etc.)

- P(A) = marginal probability of treatment (overall

- P(A\|L) = probability of treatment given confounders (propensity
  score)

\*\*Stabilized weights:\*\*

- Exposed: P(A) / P(A\|L) - upweight if unlikely to be exposed

- Unexposed: (1-P(A)) / (1-P(A\|L)) - upweight if likely to be exposed

Unlike IPCW-PP (see \[tte_ipcw_pp()\]), the marginal probability here is
computed across everyone (not within exposure groups), because we're
modeling treatment assignment, not censoring.

IPW is calculated at baseline (tstart == 0) and then merged back to all
time periods. This ensures baseline confounding adjustment is applied
consistently across follow-up.

## See also

\[tte_calculate_ipw()\] for the underlying function

Other tte_methods:
[`tte_collapse()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse.md),
[`tte_enroll()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enroll.md),
[`tte_extract()`](https://papadopoulos-lab.github.io/swereg/reference/tte_extract.md),
[`tte_ipcw_pp()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipcw_pp.md),
[`tte_irr()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr.md),
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
trial <- tte_trial(data, design) |>
  tte_collapse(period_width = 4) |>
  tte_ipw(stabilize = TRUE)
} # }
```
