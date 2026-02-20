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
| **trial**            | After `$enroll()`, data is expanded to trial panels: one row per person per trial per time period. `data_level` becomes `"trial"`.                                                                                                                      |
| **counting-process** | The trial-level data uses counting-process format with `tstart`/`tstop` columns (Andersen-Gill style), suitable for time-varying Cox models and weighted Poisson regression.                                                                            |

## Classes

| Class               | Role                                                                                                                                                                                                                                                  |
|:--------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **`TTEDesign`**     | Column name mappings that define the trial schema: person ID, exposure, outcome, confounder, and time variables. Created once via [`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md) and reused across the workflow. |
| **`TTEEnrollment`** | Enrollment data container (data.table + design + workflow state). Methods modify in-place via R6 reference semantics and return `invisible(self)` for `$`-chaining.                                                                                   |
| **`TTEPlan`**       | Builder for trial generation. Holds the ETT grid, skeleton file paths, and per-ETT design parameters. Orchestrates Loop 1 via `$generate_enrollments_and_ipw()`.                                                                                      |

## ETT grid

| Term                            | Meaning                                                                                                                                                                                                                          |
|:--------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **ETT** (Emulated Target Trial) | One combination of outcome × follow-up duration × age group. Each ETT produces one analysis-ready dataset. Corresponds to one row in `plan$ett`.                                                                                 |
| **enrollment_id**               | Groups ETTs that share the same trial panels. ETTs within an `enrollment_id` have the same age group and matching design parameters (confounders, exposure, eligibility). They differ only in outcome and/or follow-up duration. |
| **ett_id**                      | Unique identifier for a single ETT (e.g., `"ETT01"`). Auto-assigned sequentially by `$add_one_ett()`.                                                                                                                            |
| **enrollment_spec**             | The metadata list returned by `plan$enrollment_spec(i)`. Contains `design` (`TTEDesign`), `enrollment_id`, `age_range`, and `n_threads`. Passed to the `process_fn` callback during Loop 1.                                      |

## Two-loop architecture

### Loop 1: enrollment + IPW

One iteration per `enrollment_id`. Run by
`plan$generate_enrollments_and_ipw()`:

    skeleton files ──(parallel callr workers)──► enroll
      ──► rbind ──► collapse ──► impute ──► IPW + truncate ──► save

Produces two files per enrollment_id:

- **file_raw** — post-enrollment, pre-imputation
- **file_imp** — post-imputation + IPW (input to Loop 2)

### Loop 2: per-ETT outcome weighting

One iteration per ETT. Runs sequentially in the main process:

    load file_imp ──► $prepare_outcome() ──► $ipcw_pp() ──► save file_analysis

`$ipcw_pp()` combines weights (`ipw × ipcw_pp` → `analysis_weight_pp`),
truncates, and drops intermediate IPCW columns in one step.

### process_fn callback

User-supplied function with signature
`function(enrollment_spec, file_path)`. Called once per skeleton file
per enrollment_id inside Loop 1. Responsible for:

1.  Eligibility checks (age, calendar time, exclusion criteria)
2.  Exposure definition
3.  Enrollment via `$enroll()`

Each invocation runs in a fresh R subprocess
([`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html)) to
avoid fork + OpenMP segfault issues with data.table.

## Weights

| Term                                                                   | Meaning                                                                                                                                             |
|:-----------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------|
| **IPW** (Inverse Probability of treatment Weighting)                   | Baseline confounding adjustment. Computed once per enrollment_id in Loop 1 via `$ipw()`.                                                            |
| **IPCW-PP** (Inverse Probability of Censoring Weighting, Per-Protocol) | Time-varying weight for per-protocol analysis. Accounts for treatment switching and loss to follow-up. Computed per ETT in Loop 2 via `$ipcw_pp()`. |
| **analysis_weight_pp**                                                 | Final combined weight (`ipw × ipcw_pp`), truncated. Created automatically by `$ipcw_pp()`.                                                          |
| **truncation**                                                         | Winsorization of extreme weights at the 0.5th and 99.5th percentiles (by default) to reduce variance. Applied via `$truncate()`.                    |

## File naming

All output files live in the project-specific data directory.

| Column in `plan$ett` | Pattern                            | When created          |
|:---------------------|:-----------------------------------|:----------------------|
| `file_raw`           | `{prefix}_raw_{enrollment_id}.qs2` | Loop 1 (intermediate) |
| `file_imp`           | `{prefix}_imp_{enrollment_id}.qs2` | Loop 1 (output)       |
| `file_analysis`      | `{prefix}_analysis_{ett_id}.qs2`   | Loop 2 (output)       |

## Variable prefixes

| Prefix | Convention                                                                                                                                                                                           |
|:-------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `x_`   | Loop iteration variables extracted from grid tables (e.g., `x_outcome`, `x_follow_up`, `x_file_analysis`). Used in generate and analysis scripts to distinguish loop variables from dataset columns. |
| `rd_`  | Registry-derived variables (e.g., `rd_age_continuous`, `rd_exposed`). Project-specific columns added during skeleton creation or eligibility checks.                                                 |
