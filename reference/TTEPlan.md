# TTEPlan class for trial generation planning

Bundles the ETT grid, skeleton file paths, and design column names into
a single object using a builder pattern. Create an empty plan with
\[TTEPlan\$new()\], then add ETTs one at a time with
\`\$add_one_ett()\`. Supports \`plan\[\[i\]\]\` to extract the i-th
enrollment spec for interactive testing.

Design parameters (confounder_vars, person_id_var, treatment_var, etc.)
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
      n_person_trials, n_intervention, n_comparator) showing cumulative
      attrition at each eligibility step. Includes a
      `"before_exclusions"` row with pre-filtering counts.

  matching

  :   data.table (trial_id, n_intervention_total, n_comparator_total,
      n_intervention_enrolled, n_comparator_enrolled).

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

  (read-only) Full path to \`spec\_\<version\>.xlsx\` inside
  \`self\$dir_results\`, where \`\<version\>\` is
  \`self\$spec_version\`.

- `tables_xlsx`:

  (read-only) Full path to \`tables.xlsx\` inside \`self\$dir_results\`.

- `data_skeleton`:

  (read-only) Delegates to \`self\$registrystudy\$data_skeleton_dir\`.

- `data_rawbatch`:

  (read-only) Delegates to \`self\$registrystudy\$data_rawbatch_dir\`.

## Methods

### Public methods

- [`TTEPlan$new()`](#method-TTEPlan-initialize)

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

- [`TTEPlan$get_estimates()`](#method-TTEPlan-get_estimates)

- [`TTEPlan$get_curves()`](#method-TTEPlan-get_curves)

- [`TTEPlan$get_baselines()`](#method-TTEPlan-get_baselines)

- [`TTEPlan$get_attrition()`](#method-TTEPlan-get_attrition)

- [`TTEPlan$get_matching()`](#method-TTEPlan-get_matching)

- [`TTEPlan$get_subgroups()`](#method-TTEPlan-get_subgroups)

- [`TTEPlan$excel_spec_summary()`](#method-TTEPlan-excel_spec_summary)

- [`TTEPlan$reload_spec()`](#method-TTEPlan-reload_spec)

- [`TTEPlan$recompute_baselines()`](#method-TTEPlan-recompute_baselines)

- [`TTEPlan$export_tables()`](#method-TTEPlan-export_tables)

- [`TTEPlan$export()`](#method-TTEPlan-export)

- [`TTEPlan$clone()`](#method-TTEPlan-clone)

------------------------------------------------------------------------

### `TTEPlan$new()`

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

### `TTEPlan$check_version()`

Check if this object's schema version matches the current class version.
Errors if the object was saved with an older schema.

#### Usage

    TTEPlan$check_version()

#### Returns

\`invisible(TRUE)\` if versions match. Errors otherwise with an
actionable migration message.

------------------------------------------------------------------------

### `TTEPlan$print()`

Print the TTEPlan object.

#### Usage

    TTEPlan$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `TTEPlan$print_spec_summary()`

Print a target trial specification summary. Console-friendly summary
derived from the study specification stored on this plan. When
\`\$code_registry\` is available, variable names are shown in red and
matched code details in blue (ANSI colors).

#### Usage

    TTEPlan$print_spec_summary()

#### Returns

\`invisible(NULL)\`

------------------------------------------------------------------------

### `TTEPlan$print_target_checklist()`

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

### `TTEPlan$add_one_ett()`

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
      subgroup_vars = NULL,
      time_treatment_var,
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

- `subgroup_vars`:

  Character vector or NULL, baseline subgroup columns for
  effect-modification analyses (default: NULL).

- `time_treatment_var`:

  Character or NULL, time-varying treatment column.

- `eligible_var`:

  Character or NULL, eligibility column.

- `argset`:

  Named list with age_group, age_min, age_max (and optional
  person_id_var, outcome_description).

------------------------------------------------------------------------

### `TTEPlan$save()`

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

### `TTEPlan$enrollment_spec()`

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

- treatment_impl:

  List with variable, intervention_value, comparator_value (present when
  plan was built from a spec)

- matching_ratio:

  Numeric, e.g. 2 for 1:2 matching (present when plan was built from a
  spec)

- seed:

  Integer for reproducible matching (present when plan was built from a
  spec)

------------------------------------------------------------------------

### `TTEPlan$s1_generate_enrollments_and_ipw()`

Loop 1: Create trial panels from skeleton files and compute IPW.

Uses a two-pass pipeline to fix cross-batch matching ratio imbalance.
Requires \`self\$spec\` to be set (e.g., via
\[tteplan_from_spec_and_registrystudy()\]).

1.  \*\*Pass 1a (scout)\*\*: Lightweight parallel pass that reads each
    skeleton file, applies exclusions and treatment, and returns
    eligible \`(person_id, trial_id, intervention)\` tuples. No
    confounders or enrollment.

2.  \*\*Centralized matching\*\*: Combines all tuples from all batches,
    then per \`trial_id\` keeps all intervention and samples \`ratio \*
    n_intervention\` comparator globally. Stores counts on
    \`self\$enrollment_counts\` for TARGET Item 8 reporting.

3.  \*\*Pass 1b (full enrollment)\*\*: Parallel pass that re-reads each
    skeleton file with full processing (exclusions + confounders +
    treatment), then enrolls using pre-matched IDs (skipping per-batch
    matching). Produces panel-expanded TTEEnrollment objects.

#### Usage

    TTEPlan$s1_generate_enrollments_and_ipw(
      output_dir = NULL,
      impute_fn = tteenrollment_impute_confounders,
      stabilize = TRUE,
      n_workers = default_n_workers("s1"),
      swereg_dev_path = NULL
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

  Integer, concurrent subprocesses. Default
  \[default_n_workers\]\`("s1")\` (1 unless \`SWEREG_N_WORKERS_S1\` is
  set).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

------------------------------------------------------------------------

### `TTEPlan$s2_generate_analysis_files_and_ipcw_pp()`

Loop 2: Per-ETT IPCW-PP calculation and analysis file generation. For
each ETT, loads the imputed enrollment file, calls
\`\$s4_prepare_for_analysis()\` (outcome + IPCW-PP + weight
combination + truncation), and saves the analysis-ready file.

#### Usage

    TTEPlan$s2_generate_analysis_files_and_ipcw_pp(
      output_dir = NULL,
      estimate_ipcw_pp_separately_by_treatment = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      n_workers = 1L,
      swereg_dev_path = NULL
    )

#### Arguments

- `output_dir`:

  Optional directory override containing imp files and where analysis
  files are saved. If \`NULL\` (default), uses \`self\$dir_tteplan\`.

- `estimate_ipcw_pp_separately_by_treatment`:

  Logical, estimate IPCW-PP separately by treatment group (default:
  TRUE).

- `estimate_ipcw_pp_with_gam`:

  Logical, use GAM for IPCW-PP estimation (default: TRUE).

- `n_workers`:

  Integer, concurrent subprocesses (default: 1L).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

------------------------------------------------------------------------

### `TTEPlan$s3_analyze()`

Loop 3: Compute all analysis results and store on the plan.

For each enrollment: loads one analysis file and the raw file, computes
baseline characteristics (raw, unweighted, IPW, IPW truncated). For each
ETT: loads the analysis file, computes rates, IRR, and heterogeneity
test with both truncated and untruncated weights.

Every ETT also gets the ABSOLUTE scale, and nothing switches it off. Two
estimand and weight combinations carry it: per-protocol on
\`analysis_weight_pp_trunc\`, stored under \`rd_pp_trunc\`, and
intention-to-treat on \`ipw_trunc\`, stored under \`rd_itt\`. Each
stores one summary row at the end of follow-up, with \`rd\`, \`rd_lo\`,
\`rd_hi\`, \`nnt\`, \`nnt_lo\`, \`nnt_hi\`, \`nnt_direction\` and
\`interval_status\`. Each also stores the full band-by-band curve under
\`rd_curve_pp_trunc\` or \`rd_curve_itt\`, with \`surv_comparator\` and
\`surv_intervention\` beside the risk difference.

The curve also carries \`n_persons_at_risk_comparator\` and
\`n_persons_at_risk_intervention\`. Each is a head count of distinct
people in that arm and band. It is the count a numbers-at-risk row
reports. The figure reads it rather than opening the analysis file
again.

The bootstrap runs at 500 replicates with seed 1. Both are fixed here.
The confidence level is a STUDY property, read from
\`spec\$study\$implementation\$conf_level\` and defaulting to 0.95. All
three are recorded on every stored row. The export path formats those
numbers and never recomputes them.

Cost. Each risk difference is its own work item, so it is its own worker
process with its own read of the analysis file. That is two more reads
per ETT, or 1,080 more reads on a 540-ETT grid.

Results are stored in \`self\$results_enrollment\` and
\`self\$results_ett\`. Every targeted result is recomputed on each call
(no skip cache). Use \`plan\$save()\` to persist.

#### Usage

    TTEPlan$s3_analyze(
      enrollment_ids = NULL,
      ett_ids = NULL,
      output_dir = NULL,
      swereg_dev_path = NULL,
      n_workers = default_n_workers("s3")
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

- `n_workers`:

  Integer \>= 1 (default \`1L\`). Number of concurrent worker
  subprocesses for both the enrollment loop and the per-ETT loop. Each
  worker reads its own analysis file fresh, so peak RAM scales linearly
  with \`n_workers\`; on machines with multi-GB analysis files, set this
  conservatively. CPU threads per worker are auto-partitioned as
  \`floor(detectCores() / n_workers)\`.

------------------------------------------------------------------------

### `TTEPlan$results_summary()`

Print a diagnostic summary of stored results.

Shows one row per ETT with enrollment, event count, and whether
IRR/rates computed successfully.

This method reads \`self\$results_ett\` directly, and it is the one
DIAGNOSTIC exception to the rule that every consumer reads an accessor.
A tool that reports ABSENCE cannot read through an interface that hides
absence. The accessors report a missing slot and a skipped slot the same
way, as absent rows or as \`NA\`. They expose no skip envelope and no
failure reason. This method prints exactly three states. \`"NULL"\`
names a slot the plan does not hold. \`"SKIP: \<reason\>"\` names a
worker that failed. \`"OK"\` names a stored result.

It reports on the CACHE and never on a number. A caller that wants the
numbers calls \`\$get_estimates()\`.

#### Usage

    TTEPlan$results_summary()

------------------------------------------------------------------------

### `TTEPlan$get_estimates()`

Every stored effect estimate, as one flat table.

One row per emulated trial, estimand and weighting.

\`estimand\` and \`weights\` are two columns, not one. \`estimand\`
reads \`"pp"\` or \`"itt"\`. \`weights\` reads \`"truncated"\` or
\`"untruncated"\` and names the weighting choice inside per-protocol.
Three combinations occur: per-protocol truncated, per-protocol
untruncated, and intention-to-treat.

Three rows per emulated trial is an UPPER BOUND, not a promise. A
combination gets a row when the plan holds at least one of its rates,
incidence rate ratio and risk-difference slots. A combination the plan
holds nothing for gets no row. So a complete 540-trial grid returns
1,620 rows, and a partial one returns fewer.

The method computes nothing. It reads \`plan\$results_ett\`, and it
joins the labels from \`plan\$ett\` and \`plan\$spec\`. A slot the plan
does not carry gives \`NA\` in that slot's columns. The method MUST NOT
fill the gap from a neighbouring slot.

\`irr_estimable\` is READ, not decided. \`\$s3_analyze()\` decides it
beside the ratio and stores it. A result stored before that column
existed gives \`NA\`, and the method MUST NOT apply the rule to fill the
gap.

Every number is a bare number. \`irr_pvalue\` is a probability, not
\`"\<0.001"\`. \`rd\` is a proportion, not a rate per 10,000. The
consumer formats it.

Five sibling methods return the other stored results in the same shape:
\`\$get_curves()\`, \`\$get_baselines()\`, \`\$get_attrition()\`,
\`\$get_matching()\` and \`\$get_subgroups()\`. Each takes no argument,
and each computes nothing.

The number needed to treat carries its interval. \`nnt\` is the point
estimate, and \`nnt_lo\` and \`nnt_hi\` are the bounds
\`\$s3_analyze()\` stored. Both bounds are \`NA\` where
\`interval_status\` reads \`"spans null"\`, because the reciprocal of an
interval that contains zero is not an interval. A consumer MUST NOT
invert \`rd_lo\` and \`rd_hi\` itself, and MUST NOT print \`nnt\` alone
where the bounds are missing.

#### Usage

    TTEPlan$get_estimates()

#### Returns

A data.table with 41 columns. The identifiers come first, then the
weighted counts, then the incidence rate ratio, then the risk difference
and the number needed to treat. \`n_boot\`, \`seed\` and \`conf_level\`
record what produced the risk-difference interval.

------------------------------------------------------------------------

### `TTEPlan$get_curves()`

Every stored survival curve, as one flat table.

One row per emulated trial, estimand, weighting, arm and band.
\`\$s3_analyze()\` stores one wide curve per estimand, with a survival
column for each arm. This method returns one row per arm instead.

The table carries the numbers at risk beside survival.
\`n_persons_at_risk\` is an unweighted count of distinct people, per arm
per band. \`\$s3_analyze()\` stores it and this method melts it. A risk
table reports people, so it cannot be derived from \`surv\`, which is a
weighted probability.

A curve stored before that column existed gives \`NA\`. A consumer that
draws a risk table MUST check for missing values first. It MUST refuse
to draw. A row of missing counts looks like a drawn risk table.

#### Usage

    TTEPlan$get_curves()

#### Returns

A data.table with columns \`ett_id\`, \`estimand\`, \`weights\`,
\`arm\`, \`band\`, \`surv\` and \`n_persons_at_risk\`.

------------------------------------------------------------------------

### `TTEPlan$get_baselines()`

Every stored baseline panel, as one flat table.

One row per enrollment, panel and table row. Three columns identify the
panel. \`imputation\` reads \`"raw"\` or \`"imputed"\`. \`weighting\`
reads \`"none"\`, \`"ipw"\` or \`"ipw_trunc"\`. \`variant\` reads
\`"main"\` or \`"supplementary"\`. Five combinations occur.

The \`"raw"\` panel needs a separate pre-imputation file. The table
holds no \`"raw"\` rows when the plan holds no such panel. The method
MUST NOT present another panel under that name.

\`overall\`, \`comparator\` and \`intervention\` are display strings,
such as \`"12.3 (4.5)"\` or \`"120 (8.1 \`smd_numeric\` is the unrounded
standardised mean difference.

\`variable\` repeats on every row of its block. The stored panel prints
the name once and indents its levels under it, so \`variable\` is blank
there. A renderer that wants that indent MUST blank the repeat itself.

#### Usage

    TTEPlan$get_baselines()

#### Returns

A data.table. \`n_baseline\`, \`n_baseline_intervention\` and
\`n_baseline_comparator\` repeat that enrollment's counts on every row.

------------------------------------------------------------------------

### `TTEPlan$get_attrition()`

The stored eligibility cascade, as one flat table.

One row per enrollment and stored row, in pipeline order. Counts are
remaining-after-step.

\`\$s1_generate_enrollments_and_ipw()\` stores one row per trial and
criterion, plus ONE GLOBAL ROW per criterion. The global row carries the
true overall count of distinct people. This method returns EVERY STORED
ROW. \`trial_id\` is \`NA\` on a global row and the trial index on a
per-trial row, so the caller filters on that column.

The method returns the stored rows and nothing else. It does not sum the
per-trial rows. It does not create a global row for a criterion that has
none. A criterion with per-trial rows and no global row therefore yields
per-trial rows and no global row.

Collapsing to one row per criterion is a RENDERER's decision, and
\`.attrition_overall()\` makes it. That renderer reads the global rows
and nothing else. It returns NULL when one criterion carries no global
row, and the enrollment then gets no attrition sheet and no CONSORT
diagram. This method makes no such decision. It returns every stored
row, and the renderer needs the per-trial rows to see a criterion that
has only those.

\`step_order\` is the position of the criterion in stored order, so
every row of one criterion carries the same value.

The table holds the ELIGIBILITY CASCADE only. It holds no matching step
and no analysis step, because \`\$s1_generate_enrollments_and_ipw()\`
stores neither as a step. \`.build_cohort_flow()\` builds those two rows
and derives the per-step change columns. Building a row is a renderer's
job, so this method calls that builder nowhere.

The table carries no step KIND, because nothing stores one. The first
stored criterion is the cohort start and every later one is an
exclusion. A consumer labels them from \`step_order\`, and this method
decides nothing.

#### Usage

    TTEPlan$get_attrition()

#### Returns

A data.table with columns \`enrollment_id\`, \`trial_id\`,
\`step_order\`, \`step_name\`, \`n_persons\`, \`n_person_trials\`,
\`n_arm_intervention\` and \`n_arm_comparator\`.

------------------------------------------------------------------------

### `TTEPlan$get_matching()`

The stored matching counts, as one flat table.

One row per enrollment and trial.
\`\$s1_generate_enrollments_and_ipw()\` stores it that way.
\`n_intervention_total\` and \`n_comparator_total\` count every
person-trial that was eligible for an arm. \`n_intervention_enrolled\`
and \`n_comparator_enrolled\` count the person-trials the matcher took.

This is a SIXTH method rather than four more columns on
\`\$get_attrition()\`. The matching table has one row per enrollment and
trial. The attrition table has one row per enrollment, trial and
criterion. Joining them would repeat one matching count on every
criterion row, and report a grain that neither producer stored.

The method computes nothing. It does not sum across trials, and it
derives no enrolment ratio. \`.build_cohort_flow()\` sums the enrolled
counts to build its matching step, and that sum is a renderer's.

An enrollment that stored no matching table gets NO ROW.

#### Usage

    TTEPlan$get_matching()

#### Returns

A data.table with columns \`enrollment_id\`, \`trial_id\`,
\`n_intervention_total\`, \`n_comparator_total\`,
\`n_intervention_enrolled\` and \`n_comparator_enrolled\`.

------------------------------------------------------------------------

### `TTEPlan$get_subgroups()`

Every stored stratified estimate, as one flat table.

One row per emulated trial, estimand, weighting, subgroup variable and
subgroup level. \`subgroup_level\` reads \`"all"\` on the whole-cohort
row, and the level label on every other row.

\`subgroup_var\` is part of the KEY, not a label. One emulated trial MAY
carry several subgroup variables, and each one has its own \`"all"\`
row.

TWO p-values, and they answer different questions.

- \`irr_pvalue\` is the stratum's own p-value. Is this stratum's rate
  ratio distinguishable from the null?

- \`em_pvalue\` is the interaction test. Do the strata differ from each
  other?

A consumer that renders one where the other belongs reports a different
finding. The two never share a name.

\`em_pvalue\`, \`ratio_of_irrs\`, \`ratio_lo\` and \`ratio_hi\` come
from the interaction test that \`\$s3_analyze()\` stores. Each is one
number for the whole stratified result, so each repeats on every row of
that result. A renderer that wants them once shows them on the \`"all"\`
row.

\`ratio_of_irrs\` is the ratio of the two stratum rate ratios. It is
\`NA\` unless the subgroup variable has exactly two levels.

The method reads the UNION of two stored families. \`\$s3_analyze()\`
dispatches the stratified rate ratios and the interaction test as
separate work items, in separate subprocesses, so either can fail alone.
Four states occur.

- Both stored. Full rows.

- Stratified only. One row per stored level, with all four interaction
  columns \`NA\`.

- Interaction only. ONE row, with \`subgroup_level\` reading \`"all"\`
  and the four stratum columns \`NA\`. No stored table names the levels,
  so the method MUST NOT invent a stratum row.

- Neither stored. No rows, even when the specification names the
  variable.

A skipped stratified result reads as absent.

Coverage. Study 002 runs no stratified analysis, so this method is
tested against a fixture. Other studies in the fleet do configure
subgroups, so treat the schema as production.

#### Usage

    TTEPlan$get_subgroups()

#### Returns

A data.table with 13 columns: \`ett_id\`, \`estimand\`, \`weights\`,
\`subgroup_var\`, \`subgroup_level\`, \`irr\`, \`irr_lo\`, \`irr_hi\`,
\`irr_pvalue\`, \`em_pvalue\`, \`ratio_of_irrs\`, \`ratio_lo\` and
\`ratio_hi\`.

------------------------------------------------------------------------

### `TTEPlan$excel_spec_summary()`

Export the study specification to a standalone Excel file.

Writes a formatted summary of the spec (design, criteria, confounders,
outcomes, enrollments) with ICD-10/ATC code annotations from the code
registry. No analysis results required.

#### Usage

    TTEPlan$excel_spec_summary(path = NULL)

#### Arguments

- `path`:

  Optional output path override. If \`NULL\` (default), writes to
  \`self\$spec_xlsx\` (that is, \`spec\_\<version\>.xlsx\` inside
  \`self\$dir_results\`, where \`\<version\>\` is
  \`self\$spec_version\`).

#### Returns

\`invisible(self)\`

------------------------------------------------------------------------

### `TTEPlan$reload_spec()`

Refresh cosmetic spec fields (enrollment names, treatment arm labels,
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

### `TTEPlan$recompute_baselines()`

Recompute baseline characteristic tables in-process.

Reads each enrollment's smallest analysis file (and the raw file when
present) from disk and re-runs the new \`swereg_table1\` engine. Used to
refresh stale results after upgrading swereg, without re-running the
full \`\$s3_analyze()\` pipeline.

This is a PRODUCER, and the read is s3's. It calls
\`.s3_enrollment_worker()\`, the same worker \`\$s3_analyze()\` calls,
and it stores what the worker returns. No renderer in the export path
opens an analysis file.

\`\$export_tables()\` calls this method on its own when a stored panel
is stale. Call it yourself when you want the refresh to be a visible
step. The lazy path costs minutes. Whether it runs at all depends on
what a cached plan happens to hold.

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

### `TTEPlan$export_tables()`

Export analysis results to an Excel workbook.

Requires \`self\$results_enrollment\` and \`self\$results_ett\` to be
populated (run \`\$s3_analyze()\` first).

If the cached baseline tables were produced by an older version of
\`swereg\` (when Table 1 was a \`tableone\` object), they are
automatically refreshed in-process via \`\$recompute_baselines()\` using
the analysis files in \`output_dir\`.

The workbook carries no forest plot. The \`PP results\` and \`ITT
results\` sheets already report every emulated trial with counts, rates,
ratios, risk differences, intervals and numbers needed to treat. A
forest image repeated a subset of those numbers. \`\$export()\` still
draws one for a manuscript.

#### Usage

    TTEPlan$export_tables(
      path = NULL,
      table1_enrollment = NULL,
      protocol_ett_id = NULL,
      output_dir = NULL
    )

#### Arguments

- `path`:

  File path for the output \`.xlsx\` file.

- `table1_enrollment`:

  Enrollment ID for Table 1 (main baseline table). Default: the
  enrollment with the most baseline observations.

- `protocol_ett_id`:

  Optional character(1) ETT id. The \`Target trial protocol\` sheet
  describes this one emulated trial. An id the plan does not hold raises
  a warning and falls back. When \`NULL\` (default), the sheet describes
  the first ETT of the Table 1 enrollment, and otherwise the first ETT
  in the grid.

- `output_dir`:

  Optional directory holding the cached \`.qs2\` files. Used by the lazy
  \`recompute_baselines()\` refresh. Defaults to \`self\$output_dir\`.

------------------------------------------------------------------------

### `TTEPlan$export()`

Produce an ORDERED set of exhibits (figures and/or tables) from a
manifest and write them to \`dir\` with two-digit order prefixes, so the
manifest order becomes the exhibit numbering. This is the single
programmatic entry point: a project declares its exhibit set once and
hands it over; other projects reuse the same driver with a different
manifest. Each spec's \`type\` routes it to a producer:

- figures:

  \`"survival"\` (weighted survival curve for one ETT cell, one image
  per estimand), \`"forest"\` (forest plot over a named \`exposures\`
  set, one image per estimand), and \`"consort"\` (CONSORT flow diagram
  for an enrollment).

- tables:

  \`"table1"\` (baseline characteristics for an enrollment, written as
  CSV).

Full per-type fields are documented on the private \`.export_figure()\`
/ \`.export_table()\` producers.

Two \`"forest"\` and \`"survival"\` fields carry a decision worth
stating here, because both are silent when they go wrong.

\`"survival"\` is drawn on the CUMULATIVE-FAILURE scale, which is one
minus survival. A y-axis window is therefore meaningless until it says
which scale it is measured on, so \`ylim\` requires a companion
\`ylim_scale\`, either \`"survival"\` or \`"cumulative_failure"\`. A
survival-scale window is translated onto the plotted scale: \`c(0.95,
1)\` becomes \`c(0, 0.05)\` and shows the same band of the figure it
always did. An undeclared window is an error, not a guess. Left
undeclared and applied as given, a survival-scale window clips the whole
cumulative-failure curve out of view and produces a blank panel with no
error and no warning.

\`"forest"\` takes \`risk_difference = TRUE\` to SHOW the signed
cause-specific risk difference per 10,000 people, with its interval. The
option computes nothing. \`\$s3_analyze()\` computes the risk difference
for every ETT and stores it, so this switch only decides whether the
figure carries the two extra columns.

The \`n_boot\`, \`seed\` and \`conf_level\` fields are inert and warn.
\`\$s3_analyze()\` fixes \`n_boot\` and \`seed\`. It reads the
confidence level from \`study\$implementation\$conf_level\`, so a study
sets its level once and every result and header carries it. A figure
that could restate the level would print a label the numbers do not
have.

#### Usage

    TTEPlan$export(manifest, dir = NULL)

#### Arguments

- `manifest`:

  A non-empty list of exhibit specs. Every spec needs a \`type\`; other
  fields depend on the type. Optional \`label\` (filename stem) and
  \`title\`.

- `dir`:

  Output directory. Defaults to \`self\$dir_results\`.

#### Returns

Character vector of all written paths (invisibly).

------------------------------------------------------------------------

### `TTEPlan$clone()`

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
  project_prefix = "myproject",
  skeleton_files = skeleton_files,
  global_max_isoyearweek = "2023-52"
)
plan$add_one_ett(
  outcome_var = "death",
  outcome_name = "Death",
  follow_up = 52,
  confounder_vars = c("age", "education"),
  time_treatment_var = "rd_intervention",
  eligible_var = "eligible",
  argset = list(age_group = "50_60", age_min = 50, age_max = 60)
)

# Extract first enrollment spec for interactive testing
enrollment_spec <- plan[[1]]
enrollment_spec$design
enrollment_spec$age_range
} # }
```
