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
      n_person_trials, n_exposed, n_unexposed) showing cumulative
      attrition at each eligibility step. Includes a
      `"before_exclusions"` row with pre-filtering counts.

  matching

  :   data.table (trial_id, n_exposed_total, n_unexposed_total,
      n_exposed_enrolled, n_unexposed_enrolled).

- `output_dir`:

  Character. Directory where enrollment/analysis files are stored.

- `results_enrollment`:

  Named list of per-enrollment analysis results (keyed by
  enrollment_id).

- `results_ett`:

  Named list of per-ETT analysis results (keyed by ett_id).

- `spec_reloaded_at`:

  POSIXct or NULL. When \`\$reload_spec()\` was last called to refresh
  cosmetic labels.

- `spec_reload_skipped_diffs`:

  Character vector of structural spec differences that
  \`\$reload_spec()\` chose not to apply, or NULL.

- `spec_version`:

  Character. Spec version tag (e.g. \`"v003"\`) that selects the YAML
  filename and the results sub-directory.

- `dir_tteplan_cp`:

  \[CandidatePath\] for the directory where \`tteplan.qs2\` and its
  companion enrollment/analysis files live.

- `dir_spec_cp`:

  \[CandidatePath\] for the directory containing the spec YAML
  (\`spec_vXXX.yaml\`).

- `dir_results_cp`:

  \[CandidatePath\] for the results base directory. \`dir_results\`
  (active binding) appends \`spec_version\` to this.

- `registrystudy`:

  Embedded \[RegistryStudy\] R6 object. Owns the rawbatch and skeleton
  directory candidates; accessed via \`plan\$data_skeleton\` and
  \`plan\$data_rawbatch\`.

- `n_skeleton_files_limit`:

  Optional integer. When non-NULL, \`tteplan_load()\` caps
  \`self\$skeleton_files\` to this many entries after refreshing them
  from \`self\$registrystudy\`. Used for dev configs that only want a
  subset of skeletons.

## Active bindings

- `max_follow_up`:

  (read-only) Maximum follow_up across all ETTs.

- `dir_tteplan`:

  (read-only) Directory where \`tteplan.qs2\` is saved, resolved from
  \`self\$dir_tteplan_cp\` on the current host.

- `dir_spec`:

  (read-only) Directory containing the spec YAML, resolved from
  \`self\$dir_spec_cp\`.

- `dir_results_base`:

  (read-only) Results base directory, resolved from
  \`self\$dir_results_cp\`. \`dir_results\` appends \`spec_version\`.

- `dir_results`:

  (read-only) Results directory with version suffix:
  \`file.path(self\$dir_results_base, self\$spec_version)\`.

- `tteplan`:

  (read-only) Full path to \`tteplan.qs2\`.

- `spec_path`:

  (read-only) Full path to the spec YAML (\`spec_vXXX.yaml\`) selected
  by \`self\$spec_version\`.

- `spec_xlsx`:

  (read-only) Full path to \`spec.xlsx\` inside \`self\$dir_results\`.

- `tables_xlsx`:

  (read-only) Full path to \`tables.xlsx\` inside \`self\$dir_results\`.

- `data_skeleton`:

  (read-only) Delegates to \`self\$registrystudy\$data_skeleton_dir\`.

- `data_rawbatch`:

  (read-only) Delegates to \`self\$registrystudy\$data_rawbatch_dir\`.

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

- [`TTEPlan$s3_analyze()`](#method-TTEPlan-s3_analyze)

- [`TTEPlan$results_summary()`](#method-TTEPlan-results_summary)

- [`TTEPlan$excel_spec_summary()`](#method-TTEPlan-excel_spec_summary)

- [`TTEPlan$reload_spec()`](#method-TTEPlan-reload_spec)

- [`TTEPlan$recompute_baselines()`](#method-TTEPlan-recompute_baselines)

- [`TTEPlan$export_tables()`](#method-TTEPlan-export_tables)

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
Errors if the object was saved with an older schema.

#### Usage

    TTEPlan$check_version()

#### Returns

\`invisible(TRUE)\` if versions match. Errors otherwise with an
actionable migration message.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the TTEPlan object.

#### Usage

    TTEPlan$print(...)

#### Arguments

- `...`:

  Ignored.

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

  Character, short human-readable outcome label (used in forest plot
  rows and Table S10).

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
  person_id_var, outcome_description).

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save the plan to disk as \`tteplan.qs2\`.

Writes to \`self\$tteplan\` by default – that is, \`tteplan.qs2\` inside
the directory resolved from \`self\$dir_tteplan_cp\`. Supply \`dir\` to
override the destination (deprecated; used only by in-flight scripts
that don't yet have a \`dir_tteplan_cp\`).

Captures the destination path FIRST, then invalidates every
\[CandidatePath\] on the plan (and on its embedded \[RegistryStudy\]) so
the on-disk file never carries the saving host's resolved paths. Reload
with \[tteplan_load()\].

#### Usage

    TTEPlan$save(dir = NULL)

#### Arguments

- `dir`:

  Optional destination directory override. If \`NULL\` (default), writes
  to \`self\$tteplan\`.

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
      output_dir = NULL,
      impute_fn = tteenrollment_impute_confounders,
      stabilize = TRUE,
      n_workers = 3L,
      swereg_dev_path = NULL,
      resume = FALSE
    )

#### Arguments

- `output_dir`:

  Optional directory override for output files. If \`NULL\` (default),
  uses \`self\$dir_tteplan\`.

- `impute_fn`:

  Imputation callback or NULL (default:
  \[tteenrollment_impute_confounders\]).

- `stabilize`:

  Logical, stabilize IPW (default: TRUE).

- `n_workers`:

  Integer, concurrent subprocesses (default: 3L).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

- `resume`:

  Logical. If \`TRUE\`, skip enrollments whose \`\_imp\_\` file already
  exists in \`output_dir\` (default: FALSE).

------------------------------------------------------------------------

### Method `s2_generate_analysis_files_and_ipcw_pp()`

Loop 2: Per-ETT IPCW-PP calculation and analysis file generation. For
each ETT, loads the imputed enrollment file, calls
\`\$s4_prepare_for_analysis()\` (outcome + IPCW-PP + weight
combination + truncation), and saves the analysis-ready file.

#### Usage

    TTEPlan$s2_generate_analysis_files_and_ipcw_pp(
      output_dir = NULL,
      estimate_ipcw_pp_separately_by_exposure = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      n_workers = 1L,
      swereg_dev_path = NULL,
      resume = FALSE
    )

#### Arguments

- `output_dir`:

  Optional directory override containing imp files and where analysis
  files are saved. If \`NULL\` (default), uses \`self\$dir_tteplan\`.

- `estimate_ipcw_pp_separately_by_exposure`:

  Logical, estimate IPCW-PP separately by exposure group (default:
  TRUE).

- `estimate_ipcw_pp_with_gam`:

  Logical, use GAM for IPCW-PP estimation (default: TRUE).

- `n_workers`:

  Integer, concurrent subprocesses (default: 1L).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

- `resume`:

  Logical. If \`TRUE\`, skip ETTs whose analysis file already exists in
  \`output_dir\` (default: FALSE).

------------------------------------------------------------------------

### Method `s3_analyze()`

Loop 3: Compute all analysis results and store on the plan.

For each enrollment: loads one analysis file and the raw file, computes
baseline characteristics (raw, unweighted, IPW, IPW truncated). For each
ETT: loads the analysis file, computes rates, IRR, and heterogeneity
test with both truncated and untruncated weights.

Results are stored in \`self\$results_enrollment\` and
\`self\$results_ett\`. Existing results are skipped (resume-safe). Use
\`plan\$save()\` to persist.

#### Usage

    TTEPlan$s3_analyze(
      enrollment_ids = NULL,
      ett_ids = NULL,
      output_dir = NULL,
      swereg_dev_path = NULL
    )

#### Arguments

- `enrollment_ids`:

  Character vector of enrollment IDs to analyze, or \`NULL\` (default)
  for all.

- `ett_ids`:

  Character vector of ETT IDs to analyze, or \`NULL\` (default) for all.

- `output_dir`:

  Optional directory override. If \`NULL\` (default), uses
  \`self\$dir_tteplan\` (falls back to the legacy \`self\$output_dir\`
  for plans created before the CandidatePath migration).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

------------------------------------------------------------------------

### Method `results_summary()`

Print a diagnostic summary of stored results.

Shows one row per ETT with enrollment, event count, and whether
IRR/rates computed successfully.

#### Usage

    TTEPlan$results_summary()

------------------------------------------------------------------------

### Method `excel_spec_summary()`

Export the study specification to a standalone Excel file.

Writes a formatted summary of the spec (design, criteria, confounders,
outcomes, enrollments) with ICD-10/ATC code annotations from the code
registry. No analysis results required.

#### Usage

    TTEPlan$excel_spec_summary(path = NULL)

#### Arguments

- `path`:

  Optional output path override. If \`NULL\` (default), writes to
  \`self\$spec_xlsx\` (that is, \`spec.xlsx\` inside
  \`self\$dir_results\`).

#### Returns

\`invisible(self)\`

------------------------------------------------------------------------

### Method `reload_spec()`

Refresh cosmetic spec fields (enrollment names, exposure arm labels,
outcome names, ETT descriptions) on a cached plan without re-running the
upstream pipeline.

Structural fields (confounders, exclusion criteria, follow-up windows,
matching parameters, etc.) are \*not\* applied - they would invalidate
the cached results. The differences are surfaced via a loud warning and
recorded in \`self\$spec_reload_skipped_diffs\`.

#### Usage

    TTEPlan$reload_spec(spec_path = NULL, quiet = FALSE)

#### Arguments

- `spec_path`:

  Optional path to a \`.yaml\` study spec file. If \`NULL\` (default),
  uses \`self\$spec_path\` (resolved from \`dir_spec_cp\` +
  \`filename_spec(spec_version)\`).

- `quiet`:

  Logical, suppress the success message (default FALSE).

#### Returns

\`invisible(self)\`.

------------------------------------------------------------------------

### Method `recompute_baselines()`

Recompute baseline characteristic tables in-process.

Reads each enrollment's smallest analysis file (and the raw file when
present) from disk and re-runs the new \`swereg_table1\` engine. Used to
refresh stale results after upgrading swereg, without re-running the
full \`\$s3_analyze()\` pipeline.

#### Usage

    TTEPlan$recompute_baselines(output_dir = NULL, enrollment_ids = NULL)

#### Arguments

- `output_dir`:

  Optional directory holding the \`.qs2\` files. Defaults to
  \`self\$output_dir\`.

- `enrollment_ids`:

  Optional character vector. If NULL, refreshes every enrollment in
  \`self\$results_enrollment\`.

#### Returns

\`invisible(self)\`.

------------------------------------------------------------------------

### Method `export_tables()`

Export analysis results to an Excel workbook.

Requires \`self\$results_enrollment\` and \`self\$results_ett\` to be
populated (run \`\$s3_analyze()\` first).

If the cached baseline tables were produced by an older version of
\`swereg\` (when Table 1 was a \`tableone\` object), they are
automatically refreshed in-process via \`\$recompute_baselines()\` using
the analysis files in \`output_dir\`.

#### Usage

    TTEPlan$export_tables(
      path = NULL,
      table1_enrollment = NULL,
      featured_etts = NULL,
      output_dir = NULL,
      forest_label_format = NULL,
      forest_desc_header = NULL
    )

#### Arguments

- `path`:

  File path for the output \`.xlsx\` file.

- `table1_enrollment`:

  Enrollment ID for Table 1 (main baseline table). Default: the
  enrollment with the most baseline observations.

- `featured_etts`:

  Optional, either a flat character vector of ETT ids to feature in the
  Forest plot, or a \*\*named list\*\* of such vectors. When supplied as
  a named list, each name becomes a bold group header in the Forest plot
  (e.g. one group per exposure contrast). Order follows the list (and
  the vectors inside). When \`NULL\` (default), all ETTs are shown in
  the Forest plot with no grouping.

- `output_dir`:

  Optional directory holding the cached \`.qs2\` files. Used by the lazy
  \`recompute_baselines()\` refresh. Defaults to \`self\$output_dir\`.

- `forest_label_format`:

  Optional character(1) format string for the Forest plot row
  description. Supports \`placeholder\` tokens: \`outcome_name\`,
  \`outcome_description\`, \`enrollment_name\`, \`enrollment_id\`,
  \`exposed_name\`, \`comparator_name\`, \`follow_up\`, \`ett_id\`. When
  \`NULL\` (default), uses \`"outcome_name (follow_upw)"\` for grouped
  featured ETTs and \`"enrollment_name - outcome_name (follow_upw)"\`
  otherwise.

- `forest_desc_header`:

  Optional character(1) header label for the description column of the
  Forest plot left text panel. Defaults to \`"ETT"\`.

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
