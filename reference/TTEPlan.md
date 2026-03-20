# TTEPlan class for trial generation planning

TTEPlan class for trial generation planning

TTEPlan class for trial generation planning

## Details

Bundles the ETT grid, skeleton file paths, and design column names into
a single object using a builder pattern. Create an empty plan with
\[TTEPlan\$new()\], then add ETTs one at a time with
\`\$add_one_ett()\`. Supports \`plan\[\[i\]\]\` to extract the i-th
enrollment spec for interactive testing.

Design parameters (confounder_vars, person_id_var, exposure_var, etc.)
are stored per-ETT in the \`ett\` data.table, allowing different ETTs to
use different confounders or design columns. Within an enrollment_id
(same follow_up + age_group), design params must match.

## Computed properties

- max_follow_up:

  (read-only) The maximum \`follow_up\` across all ETTs. Used by
  \`\$enrollment_spec()\` to set \`design\$follow_up_time\` so that
  enrollment covers the longest follow-up per enrollment group. Returns
  \`NA\` when no ETTs have been added.

## Methods

- \`\$add_one_ett(...)\`:

  Add one ETT row to the plan. Returns \`invisible(self)\`.

- \`\$save(dir)\`:

  Save the plan to disk as \`.qs2\`. Returns \`invisible(path)\`.

- \`\$enrollment_spec(i)\`:

  Extract the i-th enrollment spec as a list with design, age_range,
  etc.

- \`\$s1_generate_enrollments_and_ipw(...)\`:

  Run Loop 1: skeleton files to trial panels + IPW.

- \`\$s2_generate_analysis_files_and_ipcw_pp(...)\`:

  Run Loop 2: per-ETT IPCW-PP + analysis file generation.

## See also

\[qs2_read()\] to load from disk

Other tte_classes:
[`TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md)

## Public fields

- `project_prefix`:

  Character, string used for file naming.

- `ett`:

  NULL or a data.table with per-ETT columns.

- `skeleton_files`:

  Character vector of skeleton file paths.

- `global_max_isoyearweek`:

  Admin censoring boundary.

- `spec`:

  Parsed study spec (from \[tteplan_read_spec()\]), or NULL.

- `expected_skeleton_file_count`:

  Expected number of skeleton files, or NULL.

- `code_registry`:

  data.table from \[RegistryStudy\]\`\$summary_table()\`, or NULL.

- `expected_n_ids`:

  Total number of individuals across all batches, or NULL.

- `created_at`:

  POSIXct. When this plan was created.

- `registry_study_created_at`:

  POSIXct or NULL. When the source RegistryStudy was created.

- `skeleton_created_at`:

  POSIXct or NULL. When skeleton files were created (from first file's
  attribute).

- `period_width`:

  Integer, band width in weeks for enrollment (default: 4L).

- `enrollment_counts`:

  Named list of per-enrollment TARGET Item 8 data. Each element is a
  list with:

  attrition

  :   Long-format data.table (trial_id, criterion, n_persons,
      n_person_trials) showing cumulative attrition at each eligibility
      step.

  matching

  :   data.table (trial_id, n_exposed_total, n_unexposed_total,
      n_exposed_enrolled, n_unexposed_enrolled).

## Active bindings

- `max_follow_up`:

  (read-only) Maximum follow_up across all ETTs.

## Methods

### Public methods

- [`TTEPlan$new()`](#method-TTEPlan-new)

- [`TTEPlan$check_version()`](#method-TTEPlan-check_version)

- [`TTEPlan$print()`](#method-TTEPlan-print)

- [`TTEPlan$print_spec_summary()`](#method-TTEPlan-print_spec_summary)

- [`TTEPlan$print_target_checklist()`](#method-TTEPlan-print_target_checklist)

- [`TTEPlan$add_one_ett()`](#method-TTEPlan-add_one_ett)

- [`TTEPlan$save()`](#method-TTEPlan-save)

- [`TTEPlan$enrollment_spec()`](#method-TTEPlan-enrollment_spec)

- [`TTEPlan$s1_generate_enrollments_and_ipw()`](#method-TTEPlan-s1_generate_enrollments_and_ipw)

- [`TTEPlan$s2_generate_analysis_files_and_ipcw_pp()`](#method-TTEPlan-s2_generate_analysis_files_and_ipcw_pp)

- [`TTEPlan$clone()`](#method-TTEPlan-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TTEPlan object.

#### Usage

    TTEPlan$new(project_prefix, skeleton_files, global_max_isoyearweek, ett = NULL)

#### Arguments

- `project_prefix`:

  Character, string used for file naming.

- `skeleton_files`:

  Character vector of skeleton file paths.

- `global_max_isoyearweek`:

  Administrative censoring boundary (isoyearweek string).

- `ett`:

  NULL or a data.table with per-ETT columns including design params.

------------------------------------------------------------------------

### Method `check_version()`

Check if this object's schema version matches the current class version.
Warns if the object was saved with an older schema version.

#### Usage

    TTEPlan$check_version()

#### Returns

\`invisible(TRUE)\` if versions match, \`invisible(FALSE)\` otherwise.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the TTEPlan object.

#### Usage

    TTEPlan$print(...)

------------------------------------------------------------------------

### Method `print_spec_summary()`

Print a target trial specification summary. Console-friendly summary
derived from the study specification stored on this plan. When
\`\$code_registry\` is available, variable names are shown in red and
matched code details in blue (ANSI colors).

#### Usage

    TTEPlan$print_spec_summary()

#### Returns

\`invisible(NULL)\`

------------------------------------------------------------------------

### Method `print_target_checklist()`

Print a TARGET-aligned reporting checklist.

Generates a self-contained document following the TARGET Statement
(Cashin et al., JAMA 2025) 21-item checklist for transparent reporting
of target trial emulations. Each item includes the full TARGET
description, auto-filled content from the swereg spec where available,
and \`\[FILL IN\]\` placeholders for PI completion.

#### Usage

    TTEPlan$print_target_checklist()

#### Returns

\`invisible(NULL)\`

------------------------------------------------------------------------

### Method `add_one_ett()`

Add one ETT to the plan.

An ETT (Emulated Target Trial) is one outcome x follow_up x age_group
combination. ETTs sharing an enrollment_id use the same trial panels
(same matching, same age group, same confounders). They differ only in
outcome and/or follow-up duration. This avoids redundant re-enrollment
for each outcome/follow-up combo.

#### Usage

    TTEPlan$add_one_ett(
      enrollment_id,
      outcome_var,
      outcome_name,
      follow_up,
      confounder_vars,
      time_exposure_var,
      eligible_var,
      argset = list()
    )

#### Arguments

- `enrollment_id`:

  Character, enrollment group identifier (e.g., "01").

- `outcome_var`:

  Character, name of the outcome column.

- `outcome_name`:

  Character, human-readable outcome name.

- `follow_up`:

  Integer, follow-up duration in weeks.

- `confounder_vars`:

  Character vector of confounder column names.

- `time_exposure_var`:

  Character or NULL, time-varying exposure column.

- `eligible_var`:

  Character or NULL, eligibility column.

- `argset`:

  Named list with age_group, age_min, age_max (and optional
  person_id_var).

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save the plan to disk. File is named `{project_prefix}_plan.qs2` inside
\`dir\`. Saves the R6 object directly; reload with \[qs2_read()\].

#### Usage

    TTEPlan$save(dir)

#### Arguments

- `dir`:

  Directory to save into.

#### Returns

Invisibly returns the file path.

------------------------------------------------------------------------

### Method `enrollment_spec()`

Extract enrollment spec for the i-th enrollment_id group.

#### Usage

    TTEPlan$enrollment_spec(i = 1L)

#### Arguments

- `i`:

  Integer index (1-based).

#### Returns

A list with:

- design:

  A \[TTEDesign\] object with column mappings

- enrollment_id:

  Character, the enrollment group ID

- age_range:

  Numeric vector of length 2: c(min, max)

- n_threads:

  Integer, number of data.table threads to use

- exposure_impl:

  List with variable, exposed_value, comparator_value (present when plan
  was built from a spec)

- matching_ratio:

  Numeric, e.g. 2 for 1:2 matching (present when plan was built from a
  spec)

- seed:

  Integer for reproducible matching (present when plan was built from a
  spec)

------------------------------------------------------------------------

### Method `s1_generate_enrollments_and_ipw()`

Loop 1: Create trial panels from skeleton files and compute IPW.

Uses a two-pass pipeline to fix cross-batch matching ratio imbalance.
Requires \`self\$spec\` to be set (e.g., via
\[tteplan_from_spec_and_registrystudy()\]).

1.  \*\*Pass 1a (scout)\*\*: Lightweight parallel pass that reads each
    skeleton file, applies exclusions and exposure, and returns eligible
    \`(person_id, trial_id, exposed)\` tuples. No confounders or
    enrollment.

2.  \*\*Centralized matching\*\*: Combines all tuples from all batches,
    then per \`trial_id\` keeps all exposed and samples \`ratio \*
    n_exposed\` unexposed globally. Stores counts on
    \`self\$enrollment_counts\` for TARGET Item 8 reporting.

3.  \*\*Pass 1b (full enrollment)\*\*: Parallel pass that re-reads each
    skeleton file with full processing (exclusions + confounders +
    exposure), then enrolls using pre-matched IDs (skipping per-batch
    matching). Produces panel-expanded TTEEnrollment objects.

#### Usage

    TTEPlan$s1_generate_enrollments_and_ipw(
      output_dir,
      impute_fn = tteenrollment_impute_confounders,
      stabilize = TRUE,
      n_workers = 3L,
      swereg_dev_path = NULL
    )

#### Arguments

- `output_dir`:

  Directory for output files.

- `impute_fn`:

  Imputation callback or NULL (default:
  \[tteenrollment_impute_confounders\]).

- `stabilize`:

  Logical, stabilize IPW (default: TRUE).

- `n_workers`:

  Integer, concurrent subprocesses (default: 3L).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

------------------------------------------------------------------------

### Method `s2_generate_analysis_files_and_ipcw_pp()`

Loop 2: Per-ETT IPCW-PP calculation and analysis file generation. For
each ETT, loads the imputed enrollment file, calls
\`\$s4_prepare_for_analysis()\` (outcome + IPCW-PP + weight
combination + truncation), and saves the analysis-ready file.

#### Usage

    TTEPlan$s2_generate_analysis_files_and_ipcw_pp(
      output_dir,
      estimate_ipcw_pp_separately_by_exposure = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      n_workers = 1L,
      swereg_dev_path = NULL
    )

#### Arguments

- `output_dir`:

  Directory containing imp files and where analysis files are saved.

- `estimate_ipcw_pp_separately_by_exposure`:

  Logical, estimate IPCW-PP separately by exposure group (default:
  TRUE).

- `estimate_ipcw_pp_with_gam`:

  Logical, use GAM for IPCW-PP estimation (default: TRUE).

- `n_workers`:

  Integer, concurrent subprocesses (default: 1L).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TTEPlan$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- TTEPlan$new(
  project_prefix = "project002",
  skeleton_files = skeleton_files,
  global_max_isoyearweek = "2023-52"
)
plan$add_one_ett(
  outcome_var = "death",
  outcome_name = "Death",
  follow_up = 52,
  confounder_vars = c("age", "education"),
  time_exposure_var = "rd_exposed",
  eligible_var = "eligible",
  argset = list(age_group = "50_60", age_min = 50, age_max = 60)
)

# Extract first enrollment spec for interactive testing
enrollment_spec <- plan[[1]]
enrollment_spec$design
enrollment_spec$age_range
} # }
```
