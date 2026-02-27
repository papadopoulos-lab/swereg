# Package index

## Core functions

Main functions for creating and manipulating longitudinal skeletons

- [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
  : Create longitudinal data skeleton
- [`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md)
  : Add one-time data to skeleton
- [`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md)
  : Add annual data to skeleton

## Medical data integration

Functions for adding medical registry data

- [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  : Add diagnosis data to skeleton
- [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md)
  : Add surgical operation data to skeleton
- [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md)
  : Add ICD-O-3 oncology codes to skeleton
- [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md)
  : Add SNOMED-CT version 3 codes to skeleton
- [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)
  : Add SNOMED-CT version 10 codes to skeleton
- [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
  : Add prescription drug data to skeleton
- [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md)
  : Add cause of death data to skeleton

## Data transformation

Functions for transforming data structure and creating derived variables

- [`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
  : Transform rowdep variable to rowind variable using first occurrence

## Survival analysis

Helper functions for time-to-event and survival analysis

- [`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)
  : Calculate steps until first TRUE in a forward window
- [`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md)
  : Check for any TRUE values in a prior window

## Target trial emulation

Functions and classes for causal inference using target trial emulation
methodology

- [`TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md)
  : TTEDesign class for target trial emulation
- [`TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)
  : TTEPlan class for trial generation planning
- [`TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md)
  : TTEEnrollment class for target trial emulation
- [`tte_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_derived_confounders.md)
  : Compute derived confounder columns from a study spec
- [`tte_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_exclusions.md)
  : Apply exclusion criteria from a study spec to a skeleton
- [`tte_calculate_ipcw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md)
  : Calculate inverse probability of censoring weights (IPCW-PP)
- [`tte_calculate_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipw.md)
  : Calculate inverse probability of treatment weights (IPW)
- [`tte_callr_pool()`](https://papadopoulos-lab.github.io/swereg/reference/tte_callr_pool.md)
  : Run a function on each work item via a pool of callr::r_bg() workers
- [`tte_collapse_periods()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse_periods.md)
  : Collapse time intervals to coarser periods
- [`tte_combine_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_combine_weights.md)
  : Combine IPW and IPCW-PP weights for per-protocol analysis
- [`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md)
  : Create a TTE design specification
- [`tte_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_age_range.md)
  : Check eligibility based on age range
- [`tte_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_combine.md)
  : Combine multiple eligibility criteria
- [`tte_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_isoyears.md)
  : Check eligibility based on ISO years
- [`tte_eligible_no_events_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_in_window_excluding_wk0.md)
  : Check eligibility based on no events in prior window (excluding
  baseline week)
- [`tte_eligible_no_events_lifetime_before_and_after_baseline()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_lifetime_before_and_after_baseline.md)
  : Check eligibility based on no events ever (person-level, before and
  after baseline)
- [`tte_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_observation_in_window_excluding_wk0.md)
  : Check eligibility based on no observation of a specific value
  (excluding baseline week)
- [`tte_enrollment()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enrollment.md)
  : Create a TTE enrollment object
- [`tte_identify_censoring()`](https://papadopoulos-lab.github.io/swereg/reference/tte_identify_censoring.md)
  : Identify protocol deviation and loss to follow-up for per-protocol
  analysis
- [`tte_impute_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tte_impute_confounders.md)
  : Impute missing confounders by sampling from observed values
- [`tte_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr_combine.md)
  : Combine and format multiple irr outputs into a publication-ready
  table
- [`tte_match_ratio()`](https://papadopoulos-lab.github.io/swereg/reference/tte_match_ratio.md)
  : Match unexposed to exposed at a specified ratio
- [`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md)
  : Create an empty TTE plan
- [`tte_plan_from_spec_and_skeleton_meta()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_from_spec_and_skeleton_meta.md)
  : Create a TTEPlan from a study specification
- [`tte_plan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_load.md)
  : Load a TTE plan from disk
- [`tte_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates_combine.md)
  : Combine and format multiple rates outputs into a publication-ready
  table
- [`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md)
  : Combine multiple enrollment objects
- [`tte_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_read_spec.md)
  : Read and validate a YAML study specification
- [`tte_time_to_event()`](https://papadopoulos-lab.github.io/swereg/reference/tte_time_to_event.md)
  : Calculate time to first event for each trial
- [`tte_truncate_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate_weights.md)
  : Truncate extreme inverse probability weights
- [`tte_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_validate_spec.md)
  : Validate spec variables against skeleton data

## Skeleton pipeline

S7 classes and functions for batched skeleton processing

- [`SkeletonConfig()`](https://papadopoulos-lab.github.io/swereg/reference/SkeletonConfig.md)
  : SkeletonConfig class for skeleton pipeline
- [`SkeletonMeta()`](https://papadopoulos-lab.github.io/swereg/reference/SkeletonMeta.md)
  : SkeletonMeta class for skeleton pipeline
- [`skeleton_config()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_config.md)
  : Create a skeleton pipeline configuration
- [`skeleton_meta()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_meta.md)
  : Create a skeleton set from config and IDs
- [`skeleton_save_rawbatch()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_save_rawbatch.md)
  : Save rawbatch files for one group
- [`skeleton_load_rawbatch()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_load_rawbatch.md)
  : Load rawbatch files for a single batch
- [`skeleton_save()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_save.md)
  : Save skeleton output as sub-files split by ID count
- [`skeleton_checkpoint()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_checkpoint.md)
  : Create a profiling checkpoint closure
- [`skeleton_process()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_process.md)
  : Process batches through a user-defined function
- [`skeleton_delete_rawbatches()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_delete_rawbatches.md)
  : Delete all rawbatch files from disk
- [`skeleton_delete_skeletons()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_delete_skeletons.md)
  : Delete all skeleton output files from disk
- [`skeleton_reset()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_reset.md)
  : Reset the skeleton pipeline by deleting all generated files

## Utility functions

Helper functions for data processing

- [`fread_raw()`](https://papadopoulos-lab.github.io/swereg/reference/fread_raw.md)
  : Read a raw registry file with fread, then lowercase names
- [`qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)
  : Read a qs2 file (auto-detecting format)
- [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
  : Convert column names to lowercase and optionally clean date columns
- [`parse_swedish_date()`](https://papadopoulos-lab.github.io/swereg/reference/parse_swedish_date.md)
  : Parse Swedish registry dates
- [`first_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/first_non_na.md)
  : Get first non-NA value from vector
- [`last_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/last_non_na.md)
  : Get last non-NA value from vector
- [`min_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/min_with_infinite_as_na.md)
  : Calculate minimum while treating infinite values as NA
- [`max_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/max_with_infinite_as_na.md)
  : Calculate maximum while treating infinite values as NA
- [`as_logical_min_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/as_logical_min_with_infinite_as_na.md)
  : Convert minimum to logical while treating infinite values as NA
- [`as_logical_max_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/as_logical_max_with_infinite_as_na.md)
  : Convert maximum to logical while treating infinite values as NA

## Specialized functions

Functions for specific research applications

- [`x2023_mht_add_lmed()`](https://papadopoulos-lab.github.io/swereg/reference/x2023_mht_add_lmed.md)
  : Add 2023 MHT-specific prescription data to skeleton
- [`x2026_mht_add_lmed()`](https://papadopoulos-lab.github.io/swereg/reference/x2026_mht_add_lmed.md)
  : Add 2023 MHT-specific prescription data to skeleton

## Datasets

Synthetic registry data for development and examples

- [`fake_person_ids`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_demographics`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_annual_family`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_prescriptions`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_cod`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  : Fake Swedish Registry Datasets
