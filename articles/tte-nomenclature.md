# Target trial emulation nomenclature

## Overview

This vignette defines the terms used throughout the swereg target trial
emulation (TTE) system. It serves as a quick reference for anyone
reading or writing code that uses `TTEDesign`, `TTEEnrollment`, or
`TTEPlan`.

## Data levels

| Term                 | Meaning                                                                                                                                                                                                                                                 |
|:---------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **skeleton**         | Person-week panel created by [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md) and enriched with registry data. One row per person per ISO week. Input to the TTE pipeline. Stored as batched `.qs2` files. |
| **person-week**      | Synonym for skeleton-level data before enrollment. The `data_level` of a `TTEEnrollment` starts as `"person_week"`.                                                                                                                                     |
| **trial**            | After enrollment via `TTEEnrollment$new(..., ratio = )`, data is expanded to trial panels: one row per person per trial per time period. `data_level` becomes `"trial"`.                                                                                |
| **counting-process** | The trial-level data uses counting-process format with `tstart`/`tstop` columns (Andersen-Gill style), suitable for time-varying Cox models and weighted Poisson regression.                                                                            |

## Classes

| Class               | Role                                                                                                                                                                                 |
|:--------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **`TTEDesign`**     | Column name mappings that define the trial schema: person ID, treatment, outcome, confounder, and time variables. Created once via `TTEDesign$new()` and reused across the workflow. |
| **`TTEEnrollment`** | Enrollment data container (data.table + design + workflow state). Methods modify in-place via R6 reference semantics and return `invisible(self)` for `$`-chaining.                  |
| **`TTEPlan`**       | Builder for trial generation. Holds the ETT grid, skeleton file paths, and per-ETT design parameters. Orchestrates Loop 1 via `$s1_generate_enrollments_and_ipw()`.                  |

## ETT grid

| Term                            | Meaning                                                                                                                                                                                                                           |
|:--------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **ETT** (Emulated Target Trial) | One combination of outcome × follow-up duration × age group. Each ETT produces one analysis-ready dataset. Corresponds to one row in `plan$ett`.                                                                                  |
| **enrollment_id**               | Groups ETTs that share the same trial panels. ETTs within an `enrollment_id` have the same age group and the same design parameters (confounders, treatment, eligibility). They differ only in outcome and/or follow-up duration. |
| **ett_id**                      | Unique identifier for a single ETT (e.g., `"ETT01"`). Auto-assigned sequentially by `$add_one_ett()`.                                                                                                                             |
| **enrollment_spec**             | The metadata list returned by `plan$enrollment_spec(i)`. Contains `design` (`TTEDesign`), `enrollment_id`, `age_range`, and `n_threads`. Used internally by the two-pass Loop 1 workers.                                          |

## Two-loop architecture

### Loop 1: enrollment + IPW

One iteration per `enrollment_id`. Run by
`plan$s1_generate_enrollments_and_ipw()`:

    skeleton files ──(parallel worker subprocesses)──► enroll (band-based draw + collapse)
      ──► rbind ──► impute ──► IPW + truncate ──► save

Produces two files per enrollment_id:

- **file_raw** — post-enrollment, pre-imputation
- **file_imp** — post-imputation + IPW (input to Loop 2)

### Loop 2: per-ETT outcome weighting

One iteration per ETT. Runs sequentially in the main process:

    load file_imp ──► $s4_prepare_for_analysis() ──► save file_analysis

`$s4_prepare_for_analysis()` combines outcome preparation and IPCW-PP
into one call. It prepares outcome data, calculates IPCW-PP, combines
weights (`ipw × ipcw_pp` → `analysis_weight_pp`), truncates, and drops
intermediate IPCW columns.

## Weights

| Term                                                                   | Meaning                                                                                                                                                             |
|:-----------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **IPW** (Inverse Probability of treatment Weighting)                   | Baseline confounding adjustment. Computed once per enrollment_id in Loop 1 via `$s2_ipw()`.                                                                         |
| **IPCW-PP** (Inverse Probability of Censoring Weighting, Per-Protocol) | Time-varying weight for per-protocol analysis. Accounts for treatment switching and loss to follow-up. Computed per ETT in Loop 2 via `$s4_prepare_for_analysis()`. |
| **analysis_weight_pp**                                                 | Final combined weight (`ipw × ipcw_pp`), truncated. Created automatically by `$s4_prepare_for_analysis()`.                                                          |
| **truncation**                                                         | Winsorization of extreme weights at the 1st and 99th percentiles (by default) to reduce variance. Applied via `$s3_truncate_weights()`.                             |

## File naming

All output files live in the project-specific data directory.

| Column in `plan$ett` | Pattern                            | When created          |
|:---------------------|:-----------------------------------|:----------------------|
| `file_raw`           | `{prefix}_raw_{enrollment_id}.qs2` | Loop 1 (intermediate) |
| `file_imp`           | `{prefix}_imp_{enrollment_id}.qs2` | Loop 1 (output)       |
| `file_analysis`      | `{prefix}_analysis_{ett_id}.qs2`   | Loop 2 (output)       |

## Variable prefixes

| Prefix | Convention                                                                                                                                                                                                                                                                                                          |
|:-------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `x_`   | Loop iteration variables extracted from grid tables (e.g., `x_outcome`, `x_follow_up`, `x_file_analysis`). Used in generate and analysis scripts to distinguish loop variables from dataset columns.                                                                                                                |
| `rd_`  | Row-dependent variables (e.g., `rd_age_continuous`, `rd_intervention`). Variables that can change value across rows (time points) for the same person.                                                                                                                                                              |
| `ri_`  | Row-independent variables (e.g., `ri_birthcountry`, `ri_age_first_dx`, `ri_register_tag`). Variables that are fixed per person across all rows. See [`vignette("rowdep-rowind-concept")`](https://papadopoulos-lab.github.io/swereg/articles/rowdep-rowind-concept.md) for `rd_` -\> `ri_` transformation patterns. |

## Analysis types

The target trial emulation literature describes three analysis
strategies:

| Analysis                     | Description                                                                                                   | swereg support                                                                                                                                                                                                      |
|:-----------------------------|:--------------------------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Intention-to-treat (ITT)** | Compare initiators vs non-initiators regardless of subsequent adherence. No censoring at treatment switching. | **Yes** — `$s4_prepare_for_analysis(estimand = "itt")` keeps follow-up through switching (no IPCW); analyse with `$irr(weight_col = "ipw_trunc")`.                                                                  |
| **Per-protocol**             | Censor at protocol deviation (treatment switching), adjust for informative censoring with IPCW.               | **Yes** — `$s4_prepare_for_analysis(estimand = "pp")` (default) applies per-protocol censoring, then estimates IPCW-PP weights. The final combined weight `analysis_weight_pp = ipw × ipcw_pp` is used in `$irr()`. |
| **As-treated**               | Model time-varying treatment status with time-varying IPW.                                                    | **Not implemented.** Requires time-varying treatment weights P(A_t                                                                                                                                                  |

The production pipeline produces **both** estimands per ETT:
per-protocol via `$irr(weight_col = "analysis_weight_pp_trunc")` and
intention-to-treat via `$irr(weight_col = "ipw_trunc")`, reported side
by side (and as separate forest plots) in the exported tables.

## Enrollment band width

The `period_width` parameter in `TTEDesign` (default: 4 weeks) sets the
width of an enrollment band. The input is a person-week skeleton, so
eligibility and treatment status are assessed weekly. `period_width`
then groups consecutive weeks into bands, and each band opens exactly
one trial. With `period_width = 4`, one trial opens every four weeks,
and not one trial per week.

Time zero is the landmark, the week that closes the entry band. A person
must reach that week under observation and free of every enrollment
outcome to enter the trial. The entry band therefore carries no
follow-up and no within-band immortal time. `period_width` therefore
trades the number of trials against how long a newly eligible person
waits:

- **Narrower bands** (e.g., `period_width = 1`): a person waits at most
  one week for a trial, at the cost of more trials and a larger dataset.
- **Wider bands** (e.g., `period_width = 4`): fewer trials and lower
  computational cost, at the cost of a wait of up to four weeks.

Caniglia et al. (2023) report a residual immortal time under a different
time origin. They set time zero to the first day of the enrollment week
and define exposure over that whole week:

> “for each trial we defined time zero as the first day of the week and
> exposure as antibiotic initiation between the first and seventh day of
> the week. Accordingly, some amount of immortal time bias is still
> possible.”

> “Generally, defining trials on a shorter scale will reduce residual
> immortal time bias.”

That residual is a property of a time origin that opens before
classification ends. swereg opens follow-up at the landmark instead, so
a shorter band does not reduce it. See
[`vignette("tte-timing")`](https://papadopoulos-lab.github.io/swereg/articles/tte-timing.md)
for the timing rules and worked examples.

swereg implements no grace period. A grace period allows initiation
within a fixed window after assignment, and does not count that
initiation as a deviation. It requires cloning, censoring and weighting
(Hernan 2016, Section 4.4), which this pipeline does not do.
`period_width` gives within-band slack for the timing of initiation at
enrollment only. Deviation after the entry band censors per-protocol
follow-up at the right edge of the first discordant run that exceeds the
arm’s tolerance. See
[`vignette("tte-methodology")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methodology.md)
for the same statement, mapped to the reference papers.

Set `period_width = 1` when the protocol defines treatment at a single
time point. Set it wider when initiation is gradual.

Band boundaries are anchored to a fixed calendar origin, not to the
first observed week of a study. swereg numbers the rows of
[`cstime::dates_by_isoyearweek`](https://rdrr.io/pkg/cstime/man/dates_by_isoyearweek.html),
which starts at ISO week `1900-01`, then assigns
`trial_id = (week_index - 1) %/% period_width`. Two studies with
different start dates therefore share the same band boundaries.

## The comparator draw

swereg uses **incidence density sampling** within each sequential trial.
Within each enrollment band, it takes a seeded random sample of
comparator individuals. The sample size is the stated
comparator-to-intervention ratio, for example 5:1, applied to that
band’s count of intervention individuals. Where the band holds fewer
comparators than that, the draw takes all of them. The draw is therefore
stratified by the `period_width`-week entry band, and not by the week.
It reads no other variable. It attaches no comparator individual to an
intervention individual, so it forms no matched set, and no later step
conditions on one. A person can be an intervention individual in one
trial and a comparator individual in another. That property is what
names the draw incidence density sampling. It is a design choice for
computational efficiency on a large registry dataset.

Alternative approaches described in the literature:

- **No matching, IPW only** (Danaei 2013, Hernan 2008): Include all
  eligible non-initiators, adjust via propensity score. More
  statistically efficient but computationally expensive with large
  registries.
- **Propensity score matching** (Danaei 2013, sensitivity analysis):
  Match on estimated propensity score. More complex but can achieve
  better balance.

swereg combines the draw (for computational tractability) with IPW on
the covariates taken at the recruiting week. The IPW step re-weights the
drawn sample to balance those covariates.
