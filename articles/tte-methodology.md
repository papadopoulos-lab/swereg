# TTE methodology: mapping to reference papers

## Overview

This vignette maps the swereg target trial emulation (TTE)
implementation to five reference papers that define the methodological
foundation. It documents which methods are implemented, which are not,
and the rationale for design choices.

## Reference papers

1.  **Hernán & Robins (2016)** — “Using big data to emulate a target
    trial when a randomized trial is not available.” *Am J Epidemiol.*
    Theoretical framework defining the four emulation failures and their
    solutions.

2.  **Hernán et al. (2008)** — “Observational analyses of the effect of
    combined estrogen-progestin therapy on coronary heart disease.”
    *Epidemiology.* The original “sequence of nested trials” paper.
    NHS/WHI hormone therapy example.

3.  **Danaei et al. (2013)** — “Statins and coronary heart disease
    events.” *Epidemiology.* Most detailed methods paper covering ITT,
    per-protocol, and as-treated analyses with SAS code.

4.  **Caniglia et al. (2023)** — “Emulating target trials in pregnancy.”
    *Am J Epidemiol.* Discusses enrollment period granularity and
    residual immortal time bias.

5.  **Cashin et al. (2025)** — “TARGET Statement.” *JAMA.* 21-item
    checklist for transparent reporting of target trial emulations.

## Method mapping

### Enrollment and trial construction

| Method                    | Paper                     | swereg implementation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|:--------------------------|:--------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Sequence of nested trials | Hernán 2008, Danaei 2013  | `TTEEnrollment$new(..., ratio=)`: Enrollment assesses eligibility and treatment status weekly, then collapses `period_width` consecutive weeks into one enrollment period. Each enrollment period opens exactly one trial. Initiation in any week of an enrollment period assigns the person to the trial of that period. Time zero is a landmark: the first week after the enrollment period closes. A person enters the trial only if they reach that week under observation and free of every enrollment outcome. See [`vignette("tte-timing")`](https://papadopoulos-lab.github.io/swereg/articles/tte-timing.md).     |
| Cloning + censoring       | Hernán 2016               | Not used. Our comparator draw in each enrollment period is an alternative to cloning that is computationally more efficient for large registries. The draw is incidence density sampling, stratified by the `period_width`-week enrollment period, and it reads no other variable. It forms no matched set.                                                                                                                                                                                                                                                                                                                |
| Grace period              | Hernán 2016 (Section 4.4) | **Not implemented.** A true grace period (initiation allowed within X weeks of assignment without deviation) requires cloning + censoring + weighting. `period_width` provides slack only for the *timing of initiation at enrollment*. Treatment that starts in any week of the enrollment period assigns the person to its trial. Follow-up opens at time zero, the first week after the period closes. Deviation thereafter censors at the start of the first discordant week beyond the arm’s tolerance. See [`vignette("tte-nomenclature")`](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.md). |

### Weighting methods

| Method                          | Paper                     | swereg implementation                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|:--------------------------------|:--------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Baseline IPW (propensity score) | Hernán 2008, Danaei 2013  | `$s2_ipw()`: Logistic regression P(A=1 \| L_baseline), stabilized by default. L_baseline is the enrollment-period snapshot `.tte_entry__<v>`, read at the recruiting week.                                                                                                                                                                                                                                                                                                                        |
| Time-varying IPW (as-treated)   | Danaei 2013 (Section 4.3) | **Not implemented.** Requires P(A_t \| A\_{t-1}, L_t).                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| IPCW for per-protocol           | Danaei 2013 (Section 4.2) | `$s4_prepare_for_analysis(estimand = "pp")`: censoring at protocol deviation and at loss of observation. `ipcw_pp` is the product of three factors, each fitted in each arm by default. The loss model fits on the rows without an outcome. The deviation model fits on the rows without an outcome that were not lost. A logistic model for a deviation at time zero fits on every person-trial. The two row-level models are GAM or GLM. Combined weight: `analysis_weight_pp = ipw × ipcw_pp`. |
| Baseline IPW for ITT            | Hernán & Robins           | `$s4_prepare_for_analysis(estimand = "itt")`: no switch censoring, no IPCW; analysis weight = baseline `ipw_trunc`.                                                                                                                                                                                                                                                                                                                                                                               |
| Weight stabilization            | Danaei 2013               | IPW: stabilized with marginal treatment probability. IPCW-PP: each row-level numerator is a second model on the same risk set, with the follow-up-time term (`tstart`) and nothing else. The time-zero numerator is the arm’s proportion that deviates at time zero (see note below).                                                                                                                                                                                                             |
| Weight truncation               | Danaei 2013               | `$s3_truncate_weights()`: Winsorization at 1st/99th percentiles by default.                                                                                                                                                                                                                                                                                                                                                                                                                       |

**Note on analysis types**: swereg supports both **per-protocol** and
**intention-to-treat** estimands, selected with
`$s4_prepare_for_analysis(estimand = "pp" | "itt")`. Per-protocol
censors follow-up at treatment switching and corrects the resulting
informative censoring with IPCW (analysis weight
`analysis_weight_pp[_trunc]`). Intention-to-treat keeps follow-up
through switching, with no switch censoring and no IPCW, and weights on
the baseline IPW alone (`ipw_trunc`). The production pipeline builds
both analysis files per ETT and reports both IRRs and both risk
differences side by side. As-treated analysis (time-varying IPW) is not
implemented.

Follow-up can end for five reasons; only treatment switching is handled
differently between the two estimands:

| Reason follow-up ends                                                             | Per-protocol                                                                                        | Intention-to-treat                                                                  |
|:----------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------------------|
| Outcome event                                                                     | ends, counts as event                                                                               | same                                                                                |
| Outcome event AND switch in the same follow-up interval                           | counts as event only when it falls before the first discordant week beyond tolerance (since 27.1.1) | counts as event                                                                     |
| Treatment switch (protocol deviation)                                             | censors, IPCW-corrected                                                                             | **ignored** (not censored)                                                          |
| Deviation at time zero                                                            | no follow-up; the time-zero factor of the IPCW weights the rest of the arm                          | **ignored** (not censored)                                                          |
| Loss of observation (the record ends early, or a week after time zero has no row) | censors, IPCW-corrected; an outcome at or after a missing week never counts                         | censors, treated as independent; an outcome at or after a missing week never counts |
| Administrative end (study cutoff)                                                 | censors                                                                                             | same                                                                                |
| Follow-up horizon                                                                 | censors                                                                                             | same                                                                                |

Per-protocol weight = `ipw × ipcw_pp`; intention-to-treat weight =
baseline `ipw` only.

**Note on IPCW stabilization**: Danaei (2013) describes stabilized IPCW
weights with a numerator conditioned on baseline covariates. That
requires the same covariates in the outcome model. swereg’s outcome
model holds no confounders. Each row-level numerator is therefore a
second model, fitted on the same risk set, that carries the
follow-up-time term (`tstart`) only. A numerator covariate must also be
in the outcome model (Su et al. 2024, text after eq. 3), and a
calendar-period spline is not in the span of the outcome model’s trial
and follow-up terms. See
[`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md)
section 1.8.9.

### Outcome models

| Method                                     | Paper                                                                                                                                                                                                                    | swereg implementation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|:-------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Cox proportional hazards                   | Hernán 2008 (ITT)                                                                                                                                                                                                        | Not directly. `$survival_curve()` provides weighted discrete-time survival curves from the panel (ITT via baseline IPW, or PP via a time-varying weight).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| Pooled logistic regression                 | Danaei 2013, Hernán 2008 (IPW)                                                                                                                                                                                           | `$irr()`: Weighted Poisson regression with `survey::svyglm(family = quasipoisson)`, computationally equivalent.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Flexible baseline hazard                   | Danaei 2013 (“month of follow-up and squared terms”)                                                                                                                                                                     | `$irr()`: a term in `tstart`, the time since time zero at the interval start. It is `splines::ns(tstart, df = 3)` with ≥4 distinct values, `factor(tstart)` with 2-3, and absent with 1. `tstart` is read rather than the clipped `tstop`, which would move the event rows on the time axis.                                                                                                                                                                                                                                                                                                                                                                                                                              |
| Trial as covariate                         | Caniglia 2023, Danaei 2013, Su 2024                                                                                                                                                                                      | The outcome model of `$irr()`, `$irr_by_subgroup()` and `$effect_modification_test()` includes the trial, `enrollment_period_id`: `splines::ns(enrollment_period_id, df = 3)` with ≥4 trials, a factor with 2-3, and no term with 1. The loss and deviation denominators instead include `period_id`, the calendar period of each follow-up interval. It takes the same thresholds, plus a penalised spline for ≥10 under the default GAM. Their numerators include neither. [`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md) section 1.8.9 gives the reason for each.                                                                                                      |
| Robust variance                            | Hernán 2008, Danaei 2013                                                                                                                                                                                                 | `survey::svydesign(ids = ~person_id_var)` provides person-level clustered standard errors.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Heterogeneity test                         | Hernán 2008, Danaei 2013                                                                                                                                                                                                 | `$heterogeneity_test()`: joint Wald test of the interaction of treatment with `splines::ns(enrollment_period_id, df = min(3, n - 1))`, where `n` is the number of trials. The model also holds the `tstart` term.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Risk difference and number needed to treat | None of the five papers. The NNTB and NNTH labels and the reciprocal interval come from Altman (1998, *BMJ*). Altman reports an interval that crosses zero as running through infinity; swereg reports no interval there | `$risk_difference()`: the cause-specific risk difference at each distinct stop time, from weighted discrete-time survival in each arm. Death censors follow-up; there is no competing-risk model. The interval is a percentile bootstrap that resamples persons, with one resample shared by both arms and the weights held fixed. The number needed to treat is `-1/rd`, labelled NNTB or NNTH by the sign of `rd`, with an interval only when the risk-difference interval excludes zero. `$s3_analyze()` runs it for both estimands. See [`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md) sections 1.6 (Causal contrasts), 1.8.5 (Absolute scale) and 1.8.6 (Inference). |

### IRR approximates HR

For rare events (typical in registry-based TTE studies), the incidence
rate ratio from Poisson regression approximates the hazard ratio from
Cox regression (Thompson 1977). The quasipoisson family in `svyglm`
additionally accounts for overdispersion from survey weights. This
approach scales to large registry datasets where
[`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html)
would be computationally prohibitive.

## TARGET checklist

The TARGET Statement (Cashin et al., JAMA 2025) provides a 21-item
reporting checklist. Use `plan$print_target_checklist()` to generate a
pre-populated checklist that maps the 21 TARGET items to swereg
configuration, showing what’s auto-populated from the spec and what
needs manual reporting.

## What is not implemented

| Method                                  | Reason                                                                                                                                                            |
|:----------------------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| As-treated analysis (time-varying IPW)  | Requires different modeling framework for time-varying treatment weights. May be added in future versions.                                                        |
| Log-binomial models (risk ratios)       | Caniglia (2023) uses these for pregnancy outcomes. Users can fit these externally on `$extract()` data.                                                           |
| Baseline-conditional IPCW stabilization | Would require the baseline covariates in the outcome model, which has none here. The row-level numerator models carry the follow-up-time term and no confounders. |
