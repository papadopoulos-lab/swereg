# Calculate inverse probability of censoring weights (IPCW-PP)

Calculates time-varying stabilized inverse probability of censoring
weights for per-protocol analysis in target trial emulation. IPCW-PP
adjusts for informative censoring due to protocol deviation or loss to
follow-up.

## Usage

``` r
tte_calculate_ipcw(
  data,
  exposure_var,
  censoring_var,
  confounder_vars,
  id_var = "trial_id",
  tstart_var = "tstart",
  tstop_var = "tstop",
  separate_by_exposure = TRUE,
  use_gam = TRUE
)
```

## Arguments

- data:

  A data.table in counting-process format (one row per person per time
  period). Should contain a censoring indicator column.

- exposure_var:

  Character, name of the baseline exposure column.

- censoring_var:

  Character, name of the binary censoring indicator column (1 = censored
  at end of this period, 0 = not censored).

- confounder_vars:

  Character vector of confounder variable names for the censoring model.

- id_var:

  Character, name of the trial identifier column (default: "trial_id").

- tstart_var:

  Character, name of the period start time column (default: "tstart").

- tstop_var:

  Character, name of the period end time column (default: "tstop").

- separate_by_exposure:

  Logical, whether to fit separate censoring models for each exposure
  stratum (default: TRUE). Recommended because censoring hazards often
  differ by treatment.

- use_gam:

  Logical, whether to use GAM with smooth time term instead of plain GLM
  (default: TRUE). GAM allows flexible, non-parametric time trends in
  the censoring hazard.

## Value

The input data.table with added columns:

- p_censor:

  Predicted probability of censoring this period

- p_uncensored:

  1 - p_censor

- cum_p_uncensored:

  Cumulative product of p_uncensored over time

- marginal_p:

  Marginal probability of remaining uncensored at each time point
  (within exposure stratum if \`separate_by_exposure = TRUE\`)

- cum_marginal:

  Cumulative product of marginal_p over time

- ipcw_pp:

  Stabilized IPCW-PP for per-protocol analysis = cum_marginal /
  cum_p_uncensored

## Details

IPCW-PP addresses informative censoring in per-protocol analysis. When
people deviate from assigned treatment or are lost to follow-up, this
censoring may be related to prognosis (sicker people may be more likely
to change treatment or die). IPCW-PP upweights people who were likely to
be censored but weren't, creating a pseudo-population where censoring is
independent of measured confounders.

\*\*Algorithm:\*\*

1.  Fit a model for P(censored \| confounders, time)

2.  Calculate conditional probability of remaining: p_uncensored = 1 -
    p_censor

3.  Cumulative conditional: cum_p_uncensored = cumulative product over
    time

4.  Marginal probability: average p_uncensored at each time (within
    exposure)

5.  Cumulative marginal: cumulative product of marginal probabilities

6.  Stabilized IPCW-PP = cum_marginal / cum_p_uncensored

Separate models by exposure are recommended because the hazard of
censoring often differs by treatment (e.g., treated patients may
discontinue due to side effects while untreated may start treatment if
symptoms worsen).

GAM with smooth time term is recommended because the censoring hazard
may vary non-linearly over time.

## See also

[`tte_calculate_ipw`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipw.md),
[`tte_identify_censoring`](https://papadopoulos-lab.github.io/swereg/reference/tte_identify_censoring.md),
[`tte_combine_weights`](https://papadopoulos-lab.github.io/swereg/reference/tte_combine_weights.md)

Other tte_weights:
[`tte_calculate_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipw.md),
[`tte_combine_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_combine_weights.md),
[`tte_identify_censoring()`](https://papadopoulos-lab.github.io/swereg/reference/tte_identify_censoring.md),
[`tte_truncate_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
# Assuming data is prepared with censoring indicators
result <- tte_calculate_ipcw(
  data = counting_process_data,
  exposure_var = "baseline_exposed",
  censoring_var = "censor_this_period",
  confounder_vars = c("age_cat", "education"),
  use_gam = TRUE
)
} # }
```
