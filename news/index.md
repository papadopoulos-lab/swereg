# Changelog

## swereg 26.2.12

### Breaking changes

- **RENAMED**: `TTETrial` class → `TTEEnrollment`, `tte_trial()` →
  [`tte_enrollment()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enrollment.md),
  `summary.TTETrial` → `summary.TTEEnrollment`. The class represents an
  enrollment (matching + panel expansion), not an individual emulated
  target trial (ETT). Aligns naming with the ETT grid concept in
  `TTEPlan`.

## swereg 26.2.11

### Breaking changes

- **REMOVED**: 19 standalone TTE functions moved to R6 methods on
  `TTETrial` (15 methods) and `TTEPlan` (4 methods). Pipe chaining
  (`trial |> tte_ipw()`) replaced with `$`-chaining (`trial$ipw()`).

  **TTETrial methods**: `$enroll()`, `$collapse()`, `$ipw()`,
  `$ipcw_pp()`, `$combine_weights()`, `$truncate()`,
  `$prepare_outcome()`, `$impute_confounders()`, `$weight_summary()`,
  `$extract()`, `$summary()`, `$table1()`, `$rates()`, `$irr()`,
  `$km()`.

  **TTEPlan methods**: `$add_one_ett()`, `$save()`,
  `$enrollment_spec()`, `$generate_enrollments_and_ipw()`.

- **RENAMED**: `TTEPlan$task()` → `TTEPlan$enrollment_spec()`. The
  method returns enrollment metadata (design, enrollment_id, age_range,
  n_threads), not a generic task. The `process_fn` callback parameter
  convention changes from `function(task, file_path)` to
  `function(enrollment_spec, file_path)`.

  Removed exports: `tte_enroll`, `tte_collapse`, `tte_ipw`,
  `tte_ipcw_pp`, `tte_weights`, `tte_truncate`, `tte_prepare_outcome`,
  `tte_extract`, `tte_summary`, `tte_weight_summary`, `tte_table1`,
  `tte_rates`, `tte_irr`, `tte_km`, `tte_plan_add_one_ett`,
  `tte_plan_save`, `tte_plan_task`, `tte_generate_enrollments_and_ipw`.

  Kept standalone:
  [`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md),
  [`tte_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates_combine.md),
  [`tte_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr_combine.md),
  [`tte_impute_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tte_impute_confounders.md)
  (thin wrapper for callback default).

- **CHANGED**: TTE classes (`TTEDesign`, `TTETrial`, `TTEPlan`) migrated
  from S7 to R6. Property access changes from `@` to `$` (e.g.,
  `trial@data` → `trial$data`, `design@id_var` → `design$id_var`). R6
  reference semantics eliminate copy-on-write overhead from
  `trial$data[, := ...]`, reducing peak RAM from ~3X to ~2X during the
  weight-calculation chain (Loop 2).

- **FIXED**: Three S7 `@` accessor bugs that silently produced no-ops:

  - `$ipcw_pp()`: dropping intermediate IPCW columns (`p_censor`, etc.)
  - `$collapse()`: creating `person_weeks` column
  - `$impute_confounders()`: deleting old confounder columns before
    merge All fixed automatically by R6 (in-place modification works).

- **CHANGED**: `$ipcw_pp()` now inlines weight combination and
  truncation (was calling
  [`tte_combine_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_combine_weights.md)
  and
  [`tte_truncate_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate_weights.md)
  via function parameters that created extra refcount). Keeps data.table
  refcount=1 throughout.

### File reorganization

- Split `tte_classes.R` and `tte_methods.R` into per-class files with
  methods inline: `tte_design.R`, `tte_trial.R`, `tte_plan.R`.
  `tte_generate.R` reduced to thin
  [`tte_impute_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tte_impute_confounders.md)
  wrapper +
  [`.tte_callr_pool()`](https://papadopoulos-lab.github.io/swereg/reference/dot-tte_callr_pool.md)
  helper.

- Added `S3method(summary, TTETrial)` → delegates to `$summary()`.

### Dependencies

- **ADDED**: R6 package to Imports (S7 retained for skeleton classes).

## swereg 26.2.10

### Bug fixes

- **FIXED**: `tte_ipw()`, `tte_ipcw_pp()`: in-place joins via S7 `@`
  accessor now use extract/modify/reassign pattern
  (`dt <- trial@data; dt[...]; trial@data <- dt`). The previous
  `trial@data[i, := ...]` silently modified a copy, leaving the S7
  object’s data unchanged.

### Performance

- **IMPROVED**: `tte_ipw()`, `tte_ipcw_pp()`,
  [`tte_calculate_ipcw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md):
  replace [`merge()`](https://rdrr.io/r/base/merge.html) with in-place
  keyed joins (`data[i, := ...]`), reducing peak RAM from ~3x to ~2x
  panel size during the weight-calculation chain.

### Breaking changes

- **CHANGED**: `tte_ipcw_pp()` now also combines weights
  (`ipw * ipcw_pp` → `analysis_weight_pp`), truncates
  `analysis_weight_pp`, and drops intermediate IPCW columns (`p_censor`,
  `p_uncensored`, `cum_p_uncensored`, `marginal_p`, `cum_marginal`).
  Callers no longer need `tte_weights()` + `tte_truncate()` after
  `tte_ipcw_pp()`.

- **RENAMED**: `tte_generate_enrollments()` →
  `tte_generate_enrollments_and_ipw()`. Now computes IPW + truncation
  once on the full combined enrollment (after imputation), so the
  per-ETT Loop 2 no longer needs to call `tte_ipw()`. New `stabilize`
  parameter (default TRUE) controls IPW stabilization.

### New features

- **NEW**:
  [`tte_plan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_load.md)
  reads a `.qs2` plan file and reconstructs the `TTEPlan` S7 object.
  Companion to `tte_plan_save()`.

- **CHANGED**: `tte_plan_save()` now persists `project_prefix` and
  `skeleton_files` alongside `ett` and `global_max_isoyearweek`, so
  [`tte_plan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_load.md)
  can fully reconstruct the object.

- **NEW**:
  [`skeleton_process()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_process.md)
  gains `n_workers` parameter for parallel batch processing. When \> 1,
  uses [`callr::r()`](https://callr.r-lib.org/reference/r.html) +
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) to
  process batches concurrently while avoiding `fork()` + data.table
  OpenMP segfaults.

## swereg 26.2.9

### Improvements

- **CHANGED**: Migrate serialization from `qs` (archived) to `qs2`.
  `.qs_save`/`.qs_read` wrappers now call
  [`qs2::qs_save`](https://rdrr.io/pkg/qs2/man/qs_save.html)/[`qs2::qs_read`](https://rdrr.io/pkg/qs2/man/qs_read.html)
  (standard format, preserves S7 objects). All file extensions changed
  from `.qs` to `.qs2`. The `preset` parameter is no longer used.

- **IMPROVED**: `tte_rates()` now sets `swereg_type` and `exposure_var`
  attributes on its output; `tte_irr()` sets `swereg_type`.

- **RENAMED**: `tte_rates_table()` →
  [`tte_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates_combine.md),
  `tte_irr_table()` →
  [`tte_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr_combine.md).
  New API accepts `(results, slot, descriptions)` — extracts the
  rates/irr slot internally, removing the need for
  `lapply(results, [[, "table2")` at call sites. Exposure column is now
  read from the `exposure_var` attribute instead of guessing via
  [`setdiff()`](https://rdrr.io/r/base/sets.html).

### Breaking changes

- **CHANGED**: `tte_plan_add_one_ett()` now requires explicit
  `enrollment_id` parameter. Auto-assignment based on follow_up +
  age_group removed. Validation that design params match within an
  enrollment_id is preserved.

- **IMPROVED**: `print(plan)` now shows both enrollment grid and full
  ETT grid.

- **CHANGED**: `tte_plan_add_one_ett()` bundles `age_group`, `age_min`,
  `age_max`, `person_id_var` into an `argset` named list parameter.
  `time_exposure_var` and `eligible_var` no longer have defaults (must
  be explicit). `exposure_var` removed from interface (hardcoded to
  `"baseline_exposed"`).

- **RENAMED**: `file_id` column in the `ett` data.table →
  `enrollment_id`. This makes explicit that ETTs sharing the same
  follow_up + age_group are processed together as one “enrollment”
  (shared eligibility, matching, collapse, imputation).

- **RENAMED**: `tte_generate_trials()` → `tte_generate_enrollments()`.
  The function generates enrollments (one per follow_up × age_group),
  not individual trials.

- **RENAMED**: `tte_plan_task()` return list key `file_id` →
  `enrollment_id`.

- **UPDATED**: `print(plan)` now shows “Enrollments: N x M skeleton
  files” instead of “Tasks: N file_id(s) x M skeleton files”.

## swereg 26.2.8

### Breaking changes

- **CHANGED**:
  [`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md)
  is now infrastructure-only — takes only `project_prefix`,
  `skeleton_files`, `global_max_isoyearweek`. Use
  `tte_plan_add_one_ett()` to add ETTs with per-ETT design parameters.

- **REMOVED**: TTEPlan plan-level properties `confounder_vars`,
  `person_id_var`, `exposure_var`, `time_exposure_var`, `eligible_var`.
  These are now per-ETT columns in the `ett` data.table.

- **REMOVED**: Internal `.tte_grid()` function. The ETT grid is now
  built incrementally via `tte_plan_add_one_ett()`.

- **ADDED**: `TTEPlan@project_prefix` property (needed for file naming
  in `tte_plan_add_one_ett()`).

### New features

- **NEW**: `tte_plan_add_one_ett()` — builder function that adds one ETT
  row to a plan. Stores design params (confounder_vars, person_id_var,
  exposure_var, time_exposure_var, eligible_var) per-ETT, allowing
  different ETTs to use different confounders. Validates that design
  params match within an enrollment_id (same follow_up + age_group).

- **RENAMED**: `TTEPlan@files` property → `TTEPlan@skeleton_files` for
  clarity.

## swereg 26.2.7

### Breaking changes

- **REFACTORED**: `tte_generate_enrollments()` (formerly
  `tte_generate_trials()`) now takes a `TTEPlan` object instead of
  separate parameters (`ett`, `files`, `confounder_vars`,
  `global_max_isoyearweek`). The `process_fn` callback signature changes
  from `function(file_path, design, file_id, age_range, n_threads)` to
  `function(task, file_path)` where `task` is a list with `design`,
  `enrollment_id`, `age_range`, and `n_threads`.

### New features

- **NEW**: `TTEPlan` S7 class bundles ETT grid, skeleton file paths,
  confounder definitions, and design column names into a single object
  for trial generation.
  - [`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md):
    Constructor function
  - `tte_plan_task(plan, i)`: Extract the i-th enrollment task as a list
    with `design`, `enrollment_id`, `age_range`, `n_threads`
  - `plan[[i]]`: Shorthand for `tte_plan_task(plan, i)`
  - `length(plan)`: Number of unique enrollment_id groups
  - Supports interactive testing:
    `task <- plan[[1]]; process_fn(task, plan@skeleton_files[1])`

## swereg 26.2.6

### Documentation

- **FIXED**: Add missing topics to pkgdown reference index (TTEDesign,
  TTETrial, x2026_mht_add_lmed)

## swereg 26.2.5

### Bug fixes

- **FIXED**: Set `eval = FALSE` in skeleton3-analyze vignette to prevent
  build errors from optional `qs` package dependency

## swereg 26.2.4

### Bug fixes

- **FIXED**: Remove `qs` from Suggests to fix GitHub Actions CI (package
  not available on CRAN)

## swereg 26.2.3

### Breaking changes

- **REPLACED**: `tte_match()` and `tte_expand()` merged into single
  `tte_enroll()` function:
  - Old workflow:
    `tte_trial(data, design) |> tte_match(ratio = 2, seed = 4) |> tte_expand(extra_cols = "isoyearweek")`
  - New workflow:
    `tte_trial(data, design) |> tte_enroll(ratio = 2, seed = 4, extra_cols = "isoyearweek")`
  - The two operations were tightly coupled and always used together
  - `tte_enroll()` combines sampling (matching) and panel expansion in
    one step
  - Records “enroll” in `steps_completed` (previously recorded “match”
    then “expand”)

### New features

- **NEW**: Trial eligibility helper functions for composable eligibility
  criteria:
  - [`tte_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_isoyears.md):
    Check eligibility based on calendar years
  - [`tte_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_age_range.md):
    Check eligibility based on age range
  - [`tte_eligible_no_events_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_in_window_excluding_wk0.md):
    Check for no events in prior window (correctly excludes baseline
    week)
  - [`tte_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_observation_in_window_excluding_wk0.md):
    Check for no specific value in prior window (for categorical
    variables)
  - [`tte_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_combine.md):
    Combine multiple eligibility columns using AND logic
  - All functions modify data.tables by reference and return invisibly
    for method chaining

### Documentation

- **IMPROVED**: Clarified that eligibility checks should EXCLUDE the
  baseline week. Using `cumsum(x) == 0` is incorrect because it includes
  the current week. The new eligibility functions use
  [`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md)
  which correctly excludes the current row.

## swereg 26.1.31

### New features

- **NEW**: S7 object-oriented API for target trial emulation workflows:
  - `TTEDesign` class: Define column name mappings once and reuse across
    all TTE functions
  - `TTETrial` class: Fluent method chaining with workflow state
    tracking
  - [`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md)
    / `tte_trial()`: Constructor functions for the S7 classes
  - `tte_match()`, `tte_expand()`, `tte_collapse()`, `tte_ipw()`: S7
    methods for data preparation
  - `tte_prepare_outcome()`, `tte_ipcw()`: Outcome-specific per-protocol
    analysis
  - `tte_weights()`, `tte_truncate()`: Weight combination and truncation
  - [`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md):
    Combine batched trial objects
  - `tte_extract()`, `tte_summary()`: Access data and diagnostics
  - `tte_table1()`, `tte_rates()`, `tte_irr()`, `tte_km()`: Analysis and
    visualization

### Breaking changes

- **REMOVED**: Deprecated S7 methods replaced by
  `tte_prepare_outcome()`:
  - `tte_tte()`: Use `tte_prepare_outcome()` which computes
    `weeks_to_event` internally
  - `tte_set_outcome()`: Use `tte_prepare_outcome(outcome = "...")`
    instead
  - `tte_censoring()`: Use `tte_prepare_outcome()` which handles
    censoring internally

### Dependencies

- **ADDED**: S7 package to Imports for object-oriented class system

## swereg 26.1.30

### New features

- **NEW**: Target trial emulation weight functions for causal inference
  in observational studies:
  - [`tte_calculate_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipw.md):
    Calculate stabilized inverse probability of treatment weights (IPW)
    for baseline confounding adjustment using propensity scores
  - [`tte_calculate_ipcw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md):
    Calculate time-varying inverse probability of censoring weights
    (IPCW) for per-protocol analysis using GAM or GLM
  - [`tte_identify_censoring()`](https://papadopoulos-lab.github.io/swereg/reference/tte_identify_censoring.md):
    Identify protocol deviation and loss to follow-up for per-protocol
    analysis
  - [`tte_combine_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_combine_weights.md):
    Combine IPW and IPCW weights for per-protocol effect estimation
  - [`tte_truncate_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate_weights.md):
    Truncate extreme weights at specified quantiles to reduce variance
- **NEW**: Target trial emulation data preparation functions:
  - [`tte_match_ratio()`](https://papadopoulos-lab.github.io/swereg/reference/tte_match_ratio.md):
    Sample comparison group at specified ratio (e.g., 2:1 unexposed to
    exposed)
  - [`tte_collapse_periods()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse_periods.md):
    Collapse fine-grained time intervals (e.g., weekly) to coarser
    periods (e.g., 4-week)
  - [`tte_time_to_event()`](https://papadopoulos-lab.github.io/swereg/reference/tte_time_to_event.md):
    Calculate time to first event for each trial/person

### Dependencies

- **ADDED**: mgcv package to Imports for flexible GAM-based censoring
  models in
  [`tte_calculate_ipcw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md)

## swereg 25.12.24

### API changes

- **SIMPLIFIED**: Removed `validate_source_column()` requirement from
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
  [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
  [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
  [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
  and
  [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md):
  - The `source` column is no longer required in diagnosis data
  - To track diagnoses by source (inpatient/outpatient/cancer), filter
    the dataset externally before calling
    [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  - See
    [`?add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
    for the recommended pattern

### New features

- **NEW**:
  [`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md)
  function for survival analysis:
  - Checks if any TRUE values exist in a preceding time window (excludes
    current row)
  - Useful for determining if an event occurred in a prior time period
  - Default window of 104 weeks (~2 years) with customizable size
  - Complements
    [`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)
    for comprehensive time-to-event analysis
- **ENHANCED**:
  [`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)
  function improvements:
  - Renamed parameter from `window` to `window_including_wk0` for
    clarity
  - Default window is now 104 (inclusive of current week)
  - Added `@family survival_analysis` tag and cross-reference to
    [`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md)

### Bug fixes

- **FIXED**: Added slider package to Imports in DESCRIPTION to fix R CMD
  check warning about undeclared import

### Data

- **BREAKING**: Replaced separate `fake_inpatient_diagnoses` and
  `fake_outpatient_diagnoses` with unified `fake_diagnoses` dataset:
  - New `SOURCE` column identifies data origin: “inpatient”,
    “outpatient”, or “cancer”
  - ~2000 inpatient records, ~2000 outpatient records, ~1000 cancer
    records
  - Cancer records always have populated `ICDO3` codes
  - Enables testing of source-based filtering and validation
- **ENHANCED**: Added ICD-O-3 and SNOMED-CT columns to fake diagnosis
  data:
  - `ICDO3`: ICD-O-3 morphology codes (always populated for cancer
    source)
  - `SNOMED3`: SNOMED-CT version 3 codes
  - `SNOMEDO10`: SNOMED-CT version 10 codes

### Validation

- **ENHANCED**: SOURCE column validation is now optional - filter
  externally if needed (see API changes above)

### Documentation

- **IMPROVED**: Examples for
  [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
  [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
  and
  [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)
  are now runnable using package fake data (previously wrapped in
  `\dontrun{}`)

## swereg 25.12.6

### New features

- **NEW**:
  [`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)
  function for survival analysis:
  - Calculates the number of steps (e.g., weeks) until the first TRUE
    value in a forward-looking window
  - Useful for time-to-event calculations in longitudinal registry data
  - Default window of 103 weeks (~2 years) with customizable size
  - Returns NA if no event occurs within the window

### Bug fixes

- **CRITICAL**: Fixed
  [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md)
  and
  [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)
  calling non-existent internal functions
  - Both functions now correctly call
    `add_diagnoses_or_operations_or_cods_or_icdo3_or_snomed()`
  - These functions would have caused runtime errors before this fix
- **FIXED**: Removed erroneous `icdo10` column references from
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md):
  - ICD-O only has editions 1, 2, and 3 (not 10)
  - ICD-O-3 codes should be handled via the dedicated
    [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md)
    function
- **FIXED**: Added `icd7*` and `icd9*` columns to diagnosis search in
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md):
  - Historical ICD-7 and ICD-9 columns are now properly searched when
    `diag_type = "both"`
  - Validation and helper function now consistent
- **FIXED**: Corrected error messages in
  [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
  [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
  and
  [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md):
  - Messages now correctly reference the appropriate data types instead
    of “operation data”

### Documentation

- **ENHANCED**:
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  documentation now clearly lists which diagnosis columns are searched:
  - When `diag_type = "both"`: `hdia`, `dia*`, `ekod*`, `icd7*`, `icd9*`
  - When `diag_type = "main"`: `hdia` only

## swereg 25.8.19

### CRAN Submission Preparation

- **CRAN READY**: Package prepared for CRAN submission with
  comprehensive compliance improvements:
  - Fixed DESCRIPTION file author field duplication issue
  - Updated .Rbuildignore to exclude all development files (docs/,
    .git/, .Rhistory, etc.)
  - Removed non-portable files ([@eaDir](https://github.com/eaDir)
    directories, .DS_Store files)
  - Added missing global variable declarations to prevent R CMD check
    warnings
  - Verified URL consistency between DESCRIPTION and package startup
    messages
- **OPTIMIZED**: Vignette structure significantly improved for CRAN
  submission:
  - Reduced total vignette content by 31% (626 lines removed)
  - Condensed cookbook-survival-analysis.Rmd (removed verbose
    descriptive statistics and redundant sections)
  - Simplified skeleton2-clean.Rmd (removed duplicated skeleton1_create
    workflow)
  - Streamlined skeleton3-analyze.Rmd (removed redundant data loading
    and best practices sections)
  - Fixed all vignette build errors by ensuring consistent data variable
    availability
  - All vignettes now compile successfully and use package synthetic
    data consistently
- **VALIDATED**: All examples are runnable using package fake data - no
  \dontrun sections without justification

### Code Quality Improvements

- **CONSISTENCY**: Fixed date_columns parameter usage throughout
  package:
  - Updated all vignettes to use lowercase date_columns parameters
    (e.g., “indatum” instead of “INDATUM”)
  - Added warning to make_lowercase_names() function when uppercase
    date_columns are provided
  - Enhanced documentation to clarify that date_columns should use
    lowercase names
  - Improved user experience with clear guidance and automatic handling
    of uppercase inputs
- **ELEGANCE**: Enhanced vignette code patterns for better readability:
  - Replaced verbose data() loading patterns with elegant pipe syntax
  - Updated all data loading to use swereg::fake\_\* \|\> copy() \|\>
    make_lowercase_names() pattern
  - Eliminated clumsy multi-step data preparation code throughout
    vignettes
  - Improved code flow and professional appearance of package examples
- **VERIFIED**: Package builds successfully with R CMD build and passes
  CRAN compliance checks
- **CONFIRMED**: inst/ directory contains only files referenced by
  package functions

## swereg 25.7.30

### New Features

- **NEW**:
  [`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
  helper function for rowdep → rowind transformations:
  - Simplifies the common pattern of creating row-independent variables
    from first occurrence of conditions
  - Automatically handles temp variable creation and cleanup
  - Uses
    [`first_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/first_non_na.md)
    for robust aggregation across all variable types
  - Includes comprehensive input validation and clear error messages
- **NEW**: “Understanding rowdep and rowind Variables” vignette:
  - Explains the fundamental distinction between row-dependent and
    row-independent variables
  - Demonstrates common transformation patterns with practical examples
  - Shows integration with the swereg workflow (skeleton1_create →
    skeleton2_clean → skeleton3_analyze)
  - Includes best practices for longitudinal registry data analysis

### Documentation

- **ENHANCED**: Helper functions now include `@family data_integration`
  tags for better organization
- **IMPROVED**: Function examples use existing fake datasets for
  consistency

## swereg 25.7.16

### New Swedish Date Parsing and Enhanced Data Cleaning

- **NEW**:
  [`parse_swedish_date()`](https://papadopoulos-lab.github.io/swereg/reference/parse_swedish_date.md)
  function for handling Swedish registry dates with varying precision:
  - Handles 4-character (YYYY), 6-character (YYYYMM), and 8-character
    (YYYYMMDD) formats
  - Automatically replaces “0000” with “0701” and “00” with “15” for
    missing date components
  - Supports custom defaults for missing date parts
  - Includes comprehensive error handling and vectorized processing
- **ENHANCED**:
  [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
  now supports automatic date cleaning:
  - New `date_column` parameter to specify which column contains dates
  - Automatically creates cleaned ‘date’ column using
    [`parse_swedish_date()`](https://papadopoulos-lab.github.io/swereg/reference/parse_swedish_date.md)
  - Works with both default and data.table methods
  - Maintains backward compatibility with existing code
- **ENHANCED**: All `add_*` functions now require cleaned date columns:
  - [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
    [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
    [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
    [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md)
    expect ‘date’ column
  - Clear error messages guide users to use
    `make_lowercase_names(data, date_column = "...")`
  - Improved validation ensures data preprocessing consistency
- **ENHANCED**:
  [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
  now includes `personyears` column:
  - Annual rows (is_isoyear==TRUE) have personyears = 1
  - Weekly rows (is_isoyear==FALSE) have personyears = 1/52.25
  - Facilitates person-time calculations for survival analysis
- **IMPROVED**: Survival analysis cookbook vignette updated:
  - Uses weekly data instead of yearly data for more precise analyses
  - Age calculation based on isoyearweeksun instead of isoyear
  - Includes person-time in descriptive statistics
  - Demonstrates proper use of new date cleaning workflow

### Enhanced error handling and validation

- **ENHANCED**: Comprehensive input validation for all `add_*`
  functions:
  - [`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md):
    Validates skeleton structure, ID column exists, checks for ID
    matches
  - [`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md):
    Validates isoyear parameter, checks skeleton year coverage
  - [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md):
    Validates diagnosis patterns, checks for diagnosis code columns
  - [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md):
    Validates operation patterns, checks for operation code columns  
  - [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md):
    Validates prescription data structure, checks source columns
  - [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md):
    Validates death data structure, checks cause of death columns
- **IMPROVED**: User-friendly error messages with specific guidance:
  - Clear indication when
    [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
    is forgotten
  - Helpful suggestions for column naming issues
  - Informative ID mismatch diagnostics with sample values
- **NEW**: Internal validation helper functions for consistent error
  handling
- **ADDED**: Input validation for pattern lists, data structures, and
  parameter ranges

### New cookbook documentation

- **NEW**: Comprehensive survival analysis cookbook
  (`cookbook-survival-analysis.Rmd`):
  - Complete workflow from raw data to Cox proportional hazards model
  - Time-varying covariates (annual income) with heart attack outcome
  - Handles common challenges: missing data, multiple events, competing
    risks
  - Performance tips for large datasets
  - Practical solutions for real-world registry analysis
- **ENHANCED**: Updated `_pkgdown.yml` with new “Cookbooks” section
- **ADDED**: `survival` package to Suggests dependencies

### Bug fixes

- **FIXED**: Improved ID matching warnings and error messages across all
  functions
- **CORRECTED**: Better handling of missing data in time-varying
  covariate analysis
- **ENHANCED**: More robust parameter validation prevents common user
  errors

## swereg 25.7.16

### Major documentation restructuring

- **RESTRUCTURED**: Complete vignette reorganization for clear learning
  progression:
  - NEW “Skeleton concept” vignette: Conceptual foundation explaining
    the skeleton approach without technical implementation
  - “Building the data skeleton (skeleton1_create)”: Pure data
    integration focus - raw data to time-structured skeleton
  - “Cleaning and deriving variables (skeleton2_clean)”: Pure data
    cleaning and variable derivation focus
  - “Production analysis workflows (skeleton3_analyze)”:
    Memory-efficient processing and final analysis datasets
- **IMPROVED**: Clear separation of concerns with focused,
  single-purpose tutorials
- **ENHANCED**: Systematic learning progression from concept to
  implementation to production
- **UPDATED**: \_pkgdown.yml structure with logical vignette grouping
- **PRESERVED**: All existing technical content while improving
  organization

### Content improvements

- **NEW**: Comprehensive conceptual introduction based on presentation
  content
- **IMPROVED**: Each vignette builds systematically on the previous one
- **ENHANCED**: Better explanation of three types of data integration
  (one-time, annual, event-based)
- **CLARIFIED**: Production workflow patterns with memory-efficient
  batching strategies
- **STANDARDIZED**: Consistent academic tone and sentence case
  throughout

## swereg 25.7.15

### Documentation and presentation improvements

- **STANDARDIZED**: Changed all titles and headings to normal sentence
  case throughout:
  - Vignette titles: “Basic Workflow” → “Basic workflow”, “Complete
    Workflow” → “Complete workflow”, etc.
  - README.md section headings: “Core Functions” → “Core functions”,
    “Data Integration” → “Data integration”, etc.
  - NEWS.md section headings: “Vignette Restructuring” → “Vignette
    restructuring”, etc.
  - CLAUDE.md section headings: “Project Overview” → “Project overview”,
    “Development Commands” → “Development commands”, etc.
- **IMPROVED**: Consistent normal sentence case for better readability
  and less formal appearance
- **SIMPLIFIED**: Removed subtitle text after colons in vignette titles
  for cleaner presentation
- **ENHANCED**: Improved Core Concept section in basic workflow vignette
  with clear explanation of three data types:
  - One-time data (demographics): Added to all rows for each person
  - Annual data (income, family status): Added to all rows for specific
    year
  - Event-based data (diagnoses, prescriptions, deaths): Added to rows
    where events occurred
- **CLARIFIED**: Step 1 documentation now properly explains all skeleton
  columns including `isoyearweeksun`
- **VERIFIED**: All vignettes compile successfully with improved content

### Major documentation and vignette reorganization

- **RESTRUCTURED**: Complete vignette reorganization with improved
  naming and content flow:
  - `swereg.Rmd` → `basic-workflow.Rmd`: Focused introduction to
    skeleton1_create
  - `advanced-workflow.Rmd` → `complete-workflow.Rmd`: Two-stage
    workflow (skeleton1_create + skeleton2_clean)
  - `memory-efficient-batching.Rmd`: Maintained as comprehensive
    three-stage workflow guide
- **IMPROVED**: Eliminated content redundancy between vignettes for
  clearer learning progression
- **ENHANCED**: Updated \_pkgdown.yml configuration to reflect new
  vignette structure

### Function documentation improvements

- **ENHANCED**: Comprehensive documentation improvements for all
  exported functions:
  - Added [@family](https://github.com/family) tags for logical grouping
    (data_integration, skeleton_creation, data_preprocessing)
  - Added [@seealso](https://github.com/seealso) sections with
    cross-references to related functions and vignettes
  - Replaced placeholder examples with runnable code using synthetic
    data
  - Improved parameter documentation with detailed descriptions and
    expected formats
  - Enhanced return value documentation with explicit side effects
    description
- **STANDARDIZED**: Consistent academic tone throughout all
  documentation

### Professional presentation updates

- **IMPROVED**: Removed informal elements and adopted academic tone
  across all documentation
- **UPDATED**: Changed terminology from “fake data” to “synthetic data”
  throughout
- **ENHANCED**: More professional language in README.md and vignettes
- **STANDARDIZED**: Consistent formal tone appropriate for scientific
  software

### Technical improvements

- **VERIFIED**: All vignettes compile successfully with updated content
- **TESTED**: Package passes R CMD check with all documentation
  improvements
- **UPDATED**: CLAUDE.md reflects new vignette structure and
  documentation standards

## swereg 25.7.1

### Vignette restructuring

- **RESTRUCTURED**: Reorganized vignettes for clearer learning
  progression:
  - `swereg.Rmd`: Clean skeleton1_create tutorial using full datasets
    (removed subset filtering)
  - `advanced-workflow.Rmd`: Focused skeleton1→skeleton2 workflow
    (removed batching and skeleton3 content)
  - `memory-efficient-batching.Rmd`: NEW comprehensive batching vignette
    with complete skeleton1→skeleton2→skeleton3 workflow for large-scale
    studies
- **IMPROVED**: GitHub Actions workflow optimization with dependency
  caching and binary packages for faster CI/CD

### Batching vignette fixes

- **FIXED**: Updated memory-efficient-batching vignette with
  production-ready improvements:
  - Replace [`split()`](https://rdrr.io/r/base/split.html) with
    [`csutil::easy_split`](https://rdrr.io/pkg/csutil/man/easy_split.html)
    for better batch handling
  - Replace `saveRDS/readRDS` with `qs::qsave/qread` for 2-10x faster
    file I/O
  - Fix skeleton3_analyze to properly aggregate weekly→yearly data using
    [`swereg::max_with_infinite_as_na`](https://papadopoulos-lab.github.io/swereg/reference/max_with_infinite_as_na.md)
  - Remove incorrect `is_isoyear == TRUE` filter in skeleton3_analyze
  - Fix analysis results to avoid NaN outputs in treatment rate
    calculations
  - Add explanations for weekly→yearly data aggregation and qs package
    performance benefits

### New features

- **NEW**: Added `isoyearweeksun` variable to
  [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
  function - provides Date representing the Sunday (last day) of each
  ISO week/year for easier date calculations
- **NEW**: Updated package logo
- **IMPROVED**: Updated all vignettes to not assume swereg is loaded -
  all functions use `swereg::` prefix and
  [`data()`](https://rdrr.io/r/utils/data.html) calls use
  `package="swereg"` argument
- **IMPROVED**: Updated function documentation to clarify that pattern
  matching functions (`add_diagnoses`, `add_cods`, `add_rx`)
  automatically add “^” prefix - users should NOT include “^” in their
  patterns
- **NEW**: Added comprehensive fake Swedish registry datasets for
  development and vignettes:
  - `fake_person_ids`: 1000 synthetic personal identifiers
  - `fake_demographics`: Demographics data matching SCB format
  - `fake_annual_family`: Annual family status data
  - `fake_inpatient_diagnoses` and `fake_outpatient_diagnoses`: NPR
    diagnosis data with ICD-10 codes
  - `fake_prescriptions`: LMED prescription data with ATC codes and
    hormone therapy focus
  - `fake_cod`: Cause of death data
- **NEW**: Added two comprehensive vignettes:
  - `swereg.Rmd`: Basic skeleton1_create workflow tutorial
  - `advanced-workflow.Rmd`: Complete 3-phase workflow (skeleton1 →
    skeleton2 → skeleton3)
- **NEW**: Replaced magrittr pipe (%\>%) with base pipe (\|\>)
  throughout codebase
- **NEW**: Added memory-efficient batched processing examples for large
  registry studies

### Bug fixes

- **CRITICAL**: Fixed incorrect variable names in `fake_cod` dataset -
  changed from non-Swedish
  `underlying_cod/contributory_cod1/contributory_cod2` to correct
  Swedish registry names `ulorsak/morsak1/morsak2`
- **VERIFIED**: Confirmed all fake datasets use correct Swedish registry
  variable name conventions
- **VERIFIED**: All ICD-10 and ATC codes in fake datasets are properly
  formatted and realistic

### Documentation improvements

- **BREAKING**: Fixed incorrect function descriptions that were copied
  from another package
- **NEW**: Added comprehensive roxygen2 documentation for all exported
  functions:
  - [`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md):
    Documents merging one-time/baseline data to skeleton
  - [`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md):
    Documents merging annual data for specific ISO years
  - [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md):
    Documents cause of death analysis with ICD-10 codes
  - [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md):
    Documents diagnosis analysis with main/secondary diagnoses
  - [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md):
    Documents surgical operation analysis including gender-affirming
    procedures
  - [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md):
    Documents prescription drug analysis with ATC/product codes
  - [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md):
    Documents longitudinal skeleton creation with detailed return
    structure
  - [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md):
    Documents generic function with S3 methods
  - [`x2023_mht_add_lmed()`](https://papadopoulos-lab.github.io/swereg/reference/x2023_mht_add_lmed.md):
    Documents specialized MHT study function
- **NEW**: Added documentation for all helper functions:
  - [`min_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/min_with_infinite_as_na.md),
    [`max_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/max_with_infinite_as_na.md)
  - [`as_logical_min_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/as_logical_min_with_infinite_as_na.md),
    [`as_logical_max_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/as_logical_max_with_infinite_as_na.md)
  - [`first_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/first_non_na.md),
    [`last_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/last_non_na.md)
- **NEW**: Added `@param` descriptions for all function parameters
- **NEW**: Added `@return` descriptions explaining function outputs
- **NEW**: Added `@examples` with practical usage demonstrations
- **NEW**: Added `@details` and `@note` sections for complex functions
- **IMPROVED**: Used proper roxygen2 practices including `@rdname` for
  S3 methods and `@seealso` cross-references

### Package structure

- All exported functions now have complete, accurate documentation
  suitable for CRAN submission
- Documentation focuses on Swedish registry data analysis workflows
- Examples use `\dontrun{}` appropriately for functions requiring
  external data
