# Calculate inverse probability of censoring weights for per-protocol analysis

S7 method that calculates IPCW-PP for informative censoring adjustment
in per-protocol analysis. Wraps \[tte_calculate_ipcw()\].

## Usage

``` r
tte_ipcw_pp(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object with trial level data. Must have a censoring
  indicator column from \[tte_prepare_outcome()\].

- ...:

  Method arguments: \`separate_by_exposure\` (logical, default TRUE),
  \`use_gam\` (logical, default TRUE), \`censoring_var\` (character or
  NULL). If \`censoring_var\` is NULL (default), uses
  "censor_this_period" from \[tte_prepare_outcome()\].

## Value

The modified \[TTETrial\] object (for chaining).

## Details

This method requires \`data_level == "trial"\`.

\*\*Problem:\*\* In a per-protocol analysis, we censor people when they
deviate from their assigned treatment or are lost to follow-up
(death/emigration). Both types of censoring may be related to
prognosis - sicker people might be more likely to start treatment or to
die. This "informative censoring" biases results.

\*\*Solution:\*\* At each time point, model the probability of being
censored given confounders. Upweight people who were likely to be
censored but weren't, creating a pseudo-population where censoring is
independent of confounders.

\*\*How it works (at each time period, separately for
exposed/unexposed):\*\*

1.  Conditional probability: P(remain \| confounders) - predicted from
    model

2.  Marginal probability: P(remain) - average within exposure group at
    that time

3.  Cumulative conditional = product of conditional probs up to this
    time (person's predicted probability of still being in study)

4.  Cumulative marginal = product of marginal probs up to this time
    (expected if censoring depended only on time and exposure, not
    confounders)

5.  Stabilized IPCW-PP = cumulative marginal / cumulative conditional
    (upweights people who "should have" been censored but weren't)

Note: This function must be run separately for each outcome because
censoring times differ by outcome (the event stops follow-up).

We fit separate models for exposed and unexposed because the hazard of
censoring (protocol deviation + loss to follow-up) may differ by
treatment. For example, unexposed individuals may be more likely to
start treatment if they develop symptoms, while exposed may be more
likely to discontinue due to side effects. GAM with smooth term s(tstop)
allows flexible, non-parametric time trends in the hazard of censoring.

## See also

\[tte_calculate_ipcw()\] for the underlying function,
\[tte_prepare_outcome()\] for outcome-specific preparation,
\[tte_ipw()\] for baseline confounding adjustment

Other tte_methods:
[`tte_collapse()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse.md),
[`tte_enroll()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enroll.md),
[`tte_extract()`](https://papadopoulos-lab.github.io/swereg/reference/tte_extract.md),
[`tte_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipw.md),
[`tte_irr()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr.md),
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
trial <- trial |>
  tte_prepare_outcome(outcome = "death") |>
  tte_ipcw_pp(use_gam = TRUE)
} # }
```
