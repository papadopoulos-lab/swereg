# TTEPlan class for trial generation planning

TTEPlan class for trial generation planning

TTEPlan class for trial generation planning

## Details

Bundles the ETT grid, skeleton file paths, and design column names into
a single object using a builder pattern. Create an empty plan with
\[tte_plan()\], then add ETTs one at a time with \`\$add_one_ett()\`.
Supports \`plan\[\[i\]\]\` to extract the i-th enrollment spec for
interactive testing.

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

- \`\$generate_enrollments_and_ipw(...)\`:

  Run Loop 1: skeleton files to trial panels + IPW.

- \`\$generate_analysis_files_and_ipcw_pp(...)\`:

  Run Loop 2: per-ETT IPCW-PP + analysis file generation.

## See also

\[tte_plan()\] for the constructor, \[tte_plan_load()\] to load from
disk

Other tte_classes:
[`TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md),
[`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md),
[`tte_enrollment()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enrollment.md),
[`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md),
[`tte_plan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_load.md)

## Public fields

- `project_prefix`:

  Character, string used for file naming.

- `ett`:

  NULL or a data.table with per-ETT columns.

- `skeleton_files`:

  Character vector of skeleton file paths.

- `global_max_isoyearweek`:

  Admin censoring boundary.

## Active bindings

- `max_follow_up`:

  (read-only) Maximum follow_up across all ETTs.

## Methods

### Public methods

- [`TTEPlan$new()`](#method-TTEPlan-new)

- [`TTEPlan$print()`](#method-TTEPlan-print)

- [`TTEPlan$add_one_ett()`](#method-TTEPlan-add_one_ett)

- [`TTEPlan$save()`](#method-TTEPlan-save)

- [`TTEPlan$enrollment_spec()`](#method-TTEPlan-enrollment_spec)

- [`TTEPlan$generate_enrollments_and_ipw()`](#method-TTEPlan-generate_enrollments_and_ipw)

- [`TTEPlan$generate_analysis_files_and_ipcw_pp()`](#method-TTEPlan-generate_analysis_files_and_ipcw_pp)

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

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the TTEPlan object.

#### Usage

    TTEPlan$print(...)

------------------------------------------------------------------------

### Method `add_one_ett()`

Add one ETT to the plan. Each ETT represents one outcome x follow_up x
age_group combination. ETTs with the same \`enrollment_id\` share trial
panels and must have matching design parameters.

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

Save the plan to disk. File is named \`project_prefix_plan.qs2\` inside
\`dir\`.

#### Usage

    TTEPlan$save(dir)

#### Arguments

- `dir`:

  Directory to save into.

#### Returns

Invisibly returns the file path.

------------------------------------------------------------------------

### Method `enrollment_spec()`

Extract enrollment spec for the i-th enrollment_id group. Returns a list
with design, enrollment_id, age_range, n_threads.

#### Usage

    TTEPlan$enrollment_spec(i = 1L)

#### Arguments

- `i`:

  Integer index (1-based).

------------------------------------------------------------------------

### Method `generate_enrollments_and_ipw()`

Loop 1: Create trial panels from skeleton files and compute IPW. For
each enrollment_id, processes skeleton files in parallel using
callr::r_bg() subprocesses. Combines, collapses, optionally imputes,
computes IPW + truncation, and saves raw + imp files.

#### Usage

    TTEPlan$generate_enrollments_and_ipw(
      process_fn,
      output_dir,
      period_width = 4L,
      impute_fn = tte_impute_confounders,
      stabilize = TRUE,
      n_workers = 3L,
      swereg_dev_path = NULL
    )

#### Arguments

- `process_fn`:

  Callback with signature \`function(enrollment_spec, file_path)\`.

- `output_dir`:

  Directory for output files.

- `period_width`:

  Integer, collapse period width (default: 4L).

- `impute_fn`:

  Imputation callback or NULL (default: \[tte_impute_confounders\]).

- `stabilize`:

  Logical, stabilize IPW (default: TRUE).

- `n_workers`:

  Integer, concurrent subprocesses (default: 3L).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

------------------------------------------------------------------------

### Method `generate_analysis_files_and_ipcw_pp()`

Loop 2: Per-ETT IPCW-PP calculation and analysis file generation. For
each ETT, loads the imputed enrollment file, calls
\`\$prepare_for_analysis()\` (outcome + IPCW-PP + weight combination +
truncation), and saves the analysis-ready file.

#### Usage

    TTEPlan$generate_analysis_files_and_ipcw_pp(
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
plan <- tte_plan(
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
