# TTEPlan class for trial generation planning

Bundles the ETT grid, skeleton file paths, and design column names into
a single object using a builder pattern. Create an empty plan with
`TTEPlan$new()`, then add ETTs one at a time with `$add_one_ett()`.
Supports `plan[[i]]` to extract the i-th enrollment spec for interactive
testing.

Design parameters (confounder_vars, person_id_var, treatment_var, etc.)
are stored per-ETT in the `ett` data.table, allowing different ETTs to
use different confounders or design columns. Within an enrollment_id
(same follow_up + age_group), design params must match.

## Computed properties

- max_follow_up:

  (read-only) The maximum `follow_up` across all ETTs. Used by
  `$enrollment_spec()` to set `design$follow_up_time` so that enrollment
  covers the longest follow-up per enrollment group. Returns `NA` when
  no ETTs have been added.

## Methods

- `$add_one_ett(...)`:

  Add one ETT row to the plan. Returns `invisible(self)`.

- `$save(dir)`:

  Save the plan to disk as `.qs2`. Returns `invisible(path)`.

- `$enrollment_spec(i)`:

  Extract the i-th enrollment spec as a list with design, age_range,
  etc.

- `$s1_generate_enrollments_and_ipw(...)`:

  Run Loop 1: skeleton files to trial panels + IPW.

- `$s2_generate_analysis_files_and_ipcw_pp(...)`:

  Run Loop 2: per-ETT IPCW-PP + analysis file generation.

## The interval convention

Time zero is a landmark: the first week after the enrollment period
closes. A person enters the trial only if they reach that week under
observation and free of every enrollment outcome. Follow-up starts
there.

Every interval is `[tstart, tstop)`. The stop is exclusive. The person
leaves the risk set at `tstop`, and the row holds no part of that week.

Every duration is `tstop - tstart`. It never adds one. Three complete
four-week follow-up intervals span `[0, 12)`. That is 12 person-weeks,
and the intervals bill 4, 4 and 4. The inclusive convention bills 5, 5
and 5.

Every `weeks_to_*` column is a boundary on the same scale, counted from
time zero at week 0. `weeks_to_event`, `weeks_to_protocol_deviation`,
`weeks_to_loss`, `weeks_to_admin_end`, `weeks_to_record_end` and
`weeks_to_observation_gap` each name the first week the person no longer
contributes. A `weeks_to_record_end` of 9 means the person held
follow-up weeks 1 to 9 and bills 9 person-weeks.

The `+ 1` belongs to the inclusive convention, where weeks 1 through 4
is `4 - 1 + 1 = 4`. Both are correct arithmetic. The two differ in
whether the stop belongs to the interval. A mix of them makes a silently
wrong denominator, so swereg MUST read every stop as exclusive.

One place adds a week, and it converts a calendar reading into a stop.
`admin_censor_isoyearweek` names the last week under study, and
[`difftime()`](https://rdrr.io/r/base/difftime.html) returns the whole
weeks between that week and the time-zero week. The stop is one week
later, because the person holds the whole of the administrative week.

`tests/testthat/test-interval-convention.R` pins each of the five
boundaries.

## See also

[`qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)
to load from disk

Other tte_classes:
[`TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md)

## Public fields

- `project_prefix`:

  Character, string used for file naming.

- `ett`:

  NULL or a data.table with per-ETT columns. The column `s1_run_id`
  holds the id of the s1 run whose files s2 last wrote the analysis
  files from. It is absent before the first s2, and `NA` while s2 writes
  or after an s2 that failed. A later s1 run does not change it. s3
  refuses an ETT whose value differs from the current `s1_run_id` of the
  plan. The column `s2_run_id` holds the id of the s2 run that wrote the
  analysis files. It follows the same rules as `s1_run_id`, and it
  changes on every s2 run. s3 copies it into each result it stores. The
  logical column `feasibility_horizon` is `TRUE` on each ETT that the
  spec key `feasibility_follow_up_weeks` adds, and `FALSE` on every
  other ETT.
  [`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md)
  gives a plan saved without the column the value `FALSE` on every ETT.

- `skeleton_files`:

  Character vector of skeleton file paths.

- `global_max_isoyearweek`:

  Admin censoring boundary.

- `spec`:

  Parsed study spec (from
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)),
  or NULL.

- `expected_skeleton_file_count`:

  Expected number of skeleton files, or NULL.

- `code_registry`:

  data.table from
  [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$summary_table()`,
  or NULL.

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

  Integer, the width in weeks of the enrollment period (default: 4L).
  The same width sets each follow-up interval.

- `enrollment_counts`:

  Named list of per-enrollment TARGET Item 8 data. Each element is a
  list with:

  attrition

  :   Long-format data.table (enrollment_period_id, criterion,
      n_persons, n_person_trials, n_intervention, n_comparator) showing
      cumulative attrition at each eligibility step. Includes a
      `"before_exclusions"` row with pre-filtering counts.

  matching

  :   data.table (enrollment_period_id, n_intervention_total,
      n_comparator_total, n_intervention_enrolled,
      n_comparator_enrolled).

- `output_dir`:

  Character. Directory where enrollment/analysis files are stored.

- `results_enrollment`:

  Named list of per-enrollment analysis results (keyed by
  enrollment_id).

- `results_ett`:

  Named list of per-ETT analysis results (keyed by ett_id).

- `spec_reloaded_at`:

  POSIXct or NULL. When `$reload_spec()` was last called to refresh
  cosmetic labels.

- `spec_reload_skipped_diffs`:

  Character vector of structural spec differences that `$reload_spec()`
  chose not to apply, or NULL.

- `sparse_decision`:

  The frozen decision of the spec's `rare_condition_rule`, or NULL when
  the spec has none or s1 has not run. s1b writes it. Read it through
  `$get_sparse_decision()`. It is a data.table with one row per
  candidate, in the order of `rare_condition_rule$candidates`:

  variable, label

  :   The computed confounder and its name.

  `n_<enrollment>_intervention`, `n_<enrollment>_comparator`

  :   The count of persons who hold the condition at baseline, per
      driving enrollment and arm. A person holds it when one of her
      eligible person-trials in that arm holds it at the recruiting
      week. The same person can count in both arms.

  smallest, smallest_enrollment, smallest_arm

  :   The smallest of those counts, and the enrollment and arm that hold
      it.

  threshold, decided_by

  :   `min_n`, and the driving enrollments.

  decision

  :   `"excluded"` when `smallest` is below `threshold`, and
      `"adjusted"` otherwise.

  decided_at

  :   When s1b ran.

  The decision is made from baseline counts in the eligible population,
  before the comparator draw and without follow-up outcomes.

- `sparse_rule_hash`:

  The hash of the `rare_condition_rule` that made `sparse_decision`, or
  NULL. s1c, s2 and s3 stop when the hash of the rule in `spec` differs
  from it. `$export()`, `$export_tables()` and
  `$print_target_checklist()` stop on that difference too, when the plan
  holds a decision or a stored result. `$excel_spec_summary()` and
  `$print_spec_summary()` then show the rule as not decided yet.

- `s1_run_id`:

  The id of the last s1 run when that run completed, and NULL otherwise.
  The id is a digest of `sparse_decision` and `sparse_rule_hash`, then a
  token unique to the run. s1 sets it to NULL after its input checks,
  and drops `results_enrollment` and `results_ett` at the same point. s1
  sets the id after s1e writes every output. s2 and s3 stop unless the
  id matches the current decision. s2 copies the id into the `s1_run_id`
  column of `ett`, and s3 into each result it stores. `$export()`,
  `$export_tables()` and `$print_target_checklist()` stop when a stored
  result carries another id.

- `feasibility`:

  The result of `$s3_feasibility()`, or NULL when it has not run. It
  holds the decision of the minimum-events rule for each ETT and
  estimand, with the weighted person-years of each arm and window. It
  holds no event count, rate or expected events. `$s3_analyze()` does
  not read it.

- `spec_version`:

  Character. Spec version tag (e.g. `"v003"`) that selects the YAML
  filename and the results sub-directory.

- `dir_tteplan_cp`:

  [CandidatePath](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md)
  for the directory where `tteplan.qs2` and its companion
  enrollment/analysis files live.

- `dir_spec_cp`:

  [CandidatePath](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md)
  for the directory containing the spec YAML (`spec_vXXX.yaml`).

- `dir_results_cp`:

  [CandidatePath](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md)
  for the results base directory. `dir_results` (active binding) appends
  `spec_version` to this.

- `registrystudy`:

  Embedded
  [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)
  R6 object. Owns the rawbatch and skeleton directory candidates;
  accessed via `plan$data_skeleton` and `plan$data_rawbatch`.

- `n_skeleton_files_limit`:

  Optional integer. When non-NULL,
  [`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md)
  caps `self$skeleton_files` to this many entries after refreshing them
  from `self$registrystudy`. Used for dev configs that only want a
  subset of skeletons.

## Active bindings

- `max_follow_up`:

  (read-only) Maximum follow_up across all ETTs.

- `dir_tteplan`:

  (read-only) Directory where `tteplan.qs2` is saved, resolved from
  `self$dir_tteplan_cp` on the current host.

- `dir_spec`:

  (read-only) Directory containing the spec YAML, resolved from
  `self$dir_spec_cp`.

- `dir_results_base`:

  (read-only) Results base directory, resolved from
  `self$dir_results_cp`. `dir_results` appends `spec_version`.

- `dir_results`:

  (read-only) Results directory with version suffix:
  `file.path(self$dir_results_base, self$spec_version)`.

- `tteplan`:

  (read-only) Full path to `tteplan.qs2`.

- `spec_path`:

  (read-only) Full path to the spec YAML (`spec_vXXX.yaml`) selected by
  `self$spec_version`.

- `spec_xlsx`:

  (read-only) Full path to `spec_<version>.xlsx` inside
  `self$dir_results`, where `<version>` is `self$spec_version`.

- `tables_xlsx`:

  (read-only) Full path to `tables.xlsx` inside `self$dir_results`.

- `data_skeleton`:

  (read-only) Delegates to `self$registrystudy$data_skeleton_dir`.

- `data_rawbatch`:

  (read-only) Delegates to `self$registrystudy$data_rawbatch_dir`.

## Methods

### Public methods

- [`TTEPlan$print_target_checklist()`](#method-TTEPlan-print_target_checklist)

- [`TTEPlan$export_tables()`](#method-TTEPlan-export_tables)

- [`TTEPlan$export()`](#method-TTEPlan-export)

- [`TTEPlan$s3_feasibility()`](#method-TTEPlan-s3_feasibility)

- [`TTEPlan$s1_generate_enrollments_and_ipw()`](#method-TTEPlan-s1_generate_enrollments_and_ipw)

- [`TTEPlan$s2_generate_analysis_files_and_ipcw_pp()`](#method-TTEPlan-s2_generate_analysis_files_and_ipcw_pp)

- [`TTEPlan$s3_analyze()`](#method-TTEPlan-s3_analyze)

- [`TTEPlan$print()`](#method-TTEPlan-print)

- [`TTEPlan$print_spec_summary()`](#method-TTEPlan-print_spec_summary)

- [`TTEPlan$results_summary()`](#method-TTEPlan-results_summary)

- [`TTEPlan$get_estimates()`](#method-TTEPlan-get_estimates)

- [`TTEPlan$get_curves()`](#method-TTEPlan-get_curves)

- [`TTEPlan$get_baselines()`](#method-TTEPlan-get_baselines)

- [`TTEPlan$get_attrition()`](#method-TTEPlan-get_attrition)

- [`TTEPlan$get_matching()`](#method-TTEPlan-get_matching)

- [`TTEPlan$get_subgroups()`](#method-TTEPlan-get_subgroups)

- [`TTEPlan$get_horizon_windows()`](#method-TTEPlan-get_horizon_windows)

- [`TTEPlan$get_protocol_table()`](#method-TTEPlan-get_protocol_table)

- [`TTEPlan$recompute_baselines()`](#method-TTEPlan-recompute_baselines)

- [`TTEPlan$slurm_job()`](#method-TTEPlan-slurm_job)

- [`TTEPlan$add_one_ett()`](#method-TTEPlan-add_one_ett)

- [`TTEPlan$enrollment_spec()`](#method-TTEPlan-enrollment_spec)

- [`TTEPlan$excel_spec_summary()`](#method-TTEPlan-excel_spec_summary)

- [`TTEPlan$reload_spec()`](#method-TTEPlan-reload_spec)

- [`TTEPlan$new()`](#method-TTEPlan-initialize)

- [`TTEPlan$check_version()`](#method-TTEPlan-check_version)

- [`TTEPlan$get_sparse_decision()`](#method-TTEPlan-get_sparse_decision)

- [`TTEPlan$save()`](#method-TTEPlan-save)

- [`TTEPlan$clone()`](#method-TTEPlan-clone)

------------------------------------------------------------------------

### `TTEPlan$print_target_checklist()`

Print a TARGET-aligned reporting checklist.

Generates a self-contained document following the TARGET Statement
(Cashin et al., JAMA 2025) 21-item checklist for transparent reporting
of target trial emulations. Each item includes the full TARGET
description, auto-filled content from the swereg spec where available,
and `[FILL IN]` placeholders for PI completion.

Item 6h states the minimum-events rule from
`spec$min_expected_events_per_arm_per_year`. The method stops, and names
`$s3_analyze()`, when a stored result was judged with another value.

#### Usage

    TTEPlan$print_target_checklist()

#### Returns

`invisible(NULL)`

------------------------------------------------------------------------

### `TTEPlan$export_tables()`

Export analysis results to an Excel workbook.

Requires `self$results_enrollment` and `self$results_ett` to be
populated (run `$s3_analyze()` first).

If the cached baseline tables were produced by an older version of
`swereg` (when Table 1 was a `tableone` object), they are automatically
refreshed in-process via `$recompute_baselines()` using the analysis
files in `output_dir`.

The second sheet, `Summary`, holds one row per ETT. It shows these
values from the `ITT results` and `PP results` sheets:

- the persons with an event in each arm, for each estimand;

- the incidence rate ratio and its interval, for each estimand;

- the per-protocol person-years of each arm, as a percentage of the
  intention-to-treat person-years;

- the per-protocol risk difference per 10,000 and its interval.

Each per-protocol value uses the truncated weights. The header is row 1
and carries an autofilter. A `Family` column shows when the spec sets an
enrollment `family`.

A ratio, risk difference or number needed to treat that the
minimum-events rule withheld reads `NE` on every sheet. Its events and
person-years stay. The method stops, and names `$s3_analyze()`, when a
stored result was judged with a `min_expected_events_per_arm_per_year`
other than the one in the spec.

The `Minimum events (Y or -)` and `Minimum events (#)` sheets follow the
Rare conditions sheet. Both hold one row per outcome and comparison and
one column per whole follow-up year, for the per-protocol estimand. The
first shows Y when the estimate passes the minimum-events rule and -
when it fails the rule. A failed estimate is withheld unless its
comparison is exempt, and an exempt comparison ends with ` (exempt)`.
The second shows the largest cut, of 1, 2, 3, 4, 5, 10 and 20, at which
the estimate still passes the rule. The intention-to-treat estimand
follows the per-protocol decision. `$get_horizon_windows()` returns the
year-by-year detail.

The year columns run from 1 to the longest follow-up of all ETTs, in
whole years of 52 weeks. A follow-up that is not a whole number of years
gets no column. A year with an ETT of exactly that follow-up shows the
decision of that ETT. Another year shows the decision up to that year of
the shortest longer ETT. A year that no ETT of the row reaches is empty.

The two Minimum events sheets are the only sheets that show a
feasibility-horizon ETT, one that the spec key
`feasibility_follow_up_weeks` adds. Its years get columns. Every other
sheet, `results.qs2` and `$get_horizon_windows()` leave it out.

The method also writes `feasibility.xlsx` to the directory of the
workbook. It holds the sheets of `$s3_feasibility()`, in the same order.
Its Minimum events sheets read the decisions that s3 stored.

The workbook carries no forest plot. The `PP results` and `ITT results`
sheets already report every emulated trial with counts, rates, ratios,
risk differences and intervals. A forest image repeated a subset of
those numbers. `$export()` still draws one for a manuscript.

The method also writes `results.qs2` to the directory of the workbook. A
manuscript reads that file. It is a named list of aggregate results:

- `estimates`:

  `$get_estimates()`.

- `curves`:

  `$get_curves()`.

- `subgroups`:

  `$get_subgroups()`.

- `baselines`:

  `$get_baselines()`.

- `attrition`:

  `$get_attrition()`.

- `enrollments`:

  One row per enrollment: `enrollment_id`, `enrollment_name`,
  `intervention_name`, `comparator_name`, and the per-protocol
  person-trials of each arm, `n_baseline_intervention` and
  `n_baseline_comparator`.

- `spec_version`:

  The specification version of the plan.

- `swereg_version`:

  The swereg version that wrote the file.

- `swereg_version_computing`:

  A list with `s2` and `s3`, the swereg versions stamped on the stored
  results.

- `s1_run_id`, `s2_run_id`:

  The s1 run id of the plan, and the distinct s2 run ids of its ETTs.

The file holds no person-level data. No table has a person or
person-trial identifier, and no table has a row per person or per
person-trial.

#### Usage

    TTEPlan$export_tables(
      path = NULL,
      table1_enrollment = NULL,
      protocol_ett_id = NULL,
      output_dir = NULL,
      power = 0.8
    )

#### Arguments

- `path`:

  File path for the output `.xlsx` file.

- `table1_enrollment`:

  Enrollment ID for Table 1 (main baseline table). Default: the
  enrollment with the most baseline observations. An id that is not an
  analysed enrollment stops with an error that lists the analysed ids.

- `protocol_ett_id`:

  Optional character(1) ETT id. The `Target trial protocol` sheet
  describes this one emulated trial. An id the plan does not hold raises
  a warning and falls back. When `NULL` (default), the sheet describes
  the first ETT of the Table 1 enrollment, and otherwise the first ETT
  in the grid.

- `output_dir`:

  Optional directory holding the cached `.qs2` files. Used by the lazy
  `recompute_baselines()` refresh. Defaults to `self$output_dir`.

- `power`:

  Numeric(1), at least 0.5 and below 1, default 0.8. The power of the
  minimum detectable effect (MDE) on the `PP results` and `ITT results`
  sheets. Each sheet reports the expected intervention events under the
  null and the protective and harmful MDE incidence rate ratio. The MDE
  is a naive Poisson calculation at the study confidence level. Under
  the null, each arm expects the pooled weighted rate of both arms times
  its weighted person-years. The calculation ignores the repeated
  contributions of one person. So it can differ from the detectable
  effect of the weighted estimator with person-clustered standard
  errors. It is not observed power (Hoenig and Heisey 2001).

------------------------------------------------------------------------

### `TTEPlan$export()`

Produce an ORDERED set of exhibits (figures and/or tables) from a
manifest and write them to `dir` with two-digit order prefixes, so the
manifest order becomes the exhibit numbering. This is the single
programmatic entry point: a project declares its exhibit set once and
hands it over; other projects reuse the same driver with a different
manifest. Each spec's `type` routes it to a producer:

- figures:

  `"survival"` (weighted survival curve for one ETT cell, one image per
  estimand), `"forest"` (forest plot over a named `exposures` set, one
  image per estimand), and `"consort"` (CONSORT flow diagram for an
  enrollment).

- tables:

  `"table1"` (baseline characteristics for an enrollment, written as
  CSV).

Full per-type fields are documented on the `.plan_export_figure()` and
`.plan_export_table()` producers in `R/tteplan_export.R`.

Two `"forest"` and `"survival"` fields carry a decision worth stating
here, because both are silent when they go wrong.

`"survival"` is drawn on the CUMULATIVE-FAILURE scale, which is one
minus survival. A y-axis window is therefore meaningless until it says
which scale it is measured on, so `ylim` requires a companion
`ylim_scale`, either `"survival"` or `"cumulative_failure"`. A
survival-scale window is translated onto the plotted scale: `c(0.95, 1)`
becomes `c(0, 0.05)` and shows the same region of the figure it always
did. An undeclared window is an error, not a guess. Left undeclared and
applied as given, a survival-scale window clips the whole
cumulative-failure curve out of view and produces a blank panel with no
error and no warning.

`"forest"` takes `risk_difference = TRUE` to SHOW the signed
cause-specific risk difference per 10,000 people, with its interval. The
option computes nothing. `$s3_analyze()` computes the risk difference
for every ETT and stores it, so this switch only decides whether the
figure carries the two extra columns.

The minimum-events rule shows on both figures. A `"survival"` curve
stops at the reportable horizon, and a grey band labelled "Not
estimable" covers the rest of the follow-up. A `"forest"` cell that the
rule withheld reads `NE`. The method stops, and names `$s3_analyze()`,
when a stored result was judged with a
`min_expected_events_per_arm_per_year` other than the one in the spec.

The `n_boot`, `seed` and `conf_level` fields are inert and warn.
`$s3_analyze()` fixes `n_boot` and `seed`. It reads the confidence level
from `study$implementation$conf_level`, so a study sets its level once
and every result and header carries it. A figure that could restate the
level would print a label the numbers do not have.

#### Usage

    TTEPlan$export(manifest, dir = NULL)

#### Arguments

- `manifest`:

  A non-empty list of exhibit specs. Every spec needs a `type`; other
  fields depend on the type. Optional `label` (filename stem) and
  `title`.

- `dir`:

  Output directory. Defaults to `self$dir_results`.

#### Returns

Character vector of all written paths (invisibly).

------------------------------------------------------------------------

### `TTEPlan$s3_feasibility()`

Decide after s2 which ETTs and estimands pass the minimum-events rule,
and to which horizon. The method fits no outcome model, and
`self$feasibility` holds no event count, rate or expected events.

#### Usage

    TTEPlan$s3_feasibility(
      n_workers = default_n_workers("feasibility"),
      output_dir = NULL,
      swereg_dev_path = NULL
    )

#### Arguments

- `n_workers`:

  Integer \>= 1. The number of concurrent worker subprocesses. Each
  worker reads one analysis file. The default reads
  `default_n_workers("feasibility")`.

- `output_dir`:

  Optional directory of the analysis files. If `NULL` (default), the
  method uses `self$dir_tteplan`, then the legacy `self$output_dir`.

- `swereg_dev_path`:

  Path to a local swereg source tree for the workers, or `NULL`.

#### Details

The method reads the analysis files that s2 wrote. It resolves the file,
the weight column, the follow-up and the rule of each estimand with the
same code as `$s3_analyze()`. It then calls the same window computation
and decision. `reportable_weeks`, `withheld` and `status` are therefore
the values that `$s3_analyze()` stores in `horizon_pp` and
`horizon_itt`.

The worker subprocess computes the expected events, because the decision
needs them. It drops `events_weighted`, `rate_pooled` and
`events_expected` before it returns. The method stores the result in
`self$feasibility`. Each window keeps `window_lo`, `window_hi`, `arm`,
`py_weighted` and `pass`.

The decision depends on the observed events, through the pooled rate.
Neither `self$feasibility` nor `feasibility.xlsx` holds an event count,
a rate or expected events after time zero, or an effect estimate. The
record keeps `max_passing_cut`: the largest cut of a coarse grid at
which the estimand passes. The Attrition sheet keeps the eligibility
counts by arm, including people excluded for an outcome event before
time zero.

The intention-to-treat record of each ETT takes the decision of its
per-protocol record, as in `$s3_analyze()`.

The method writes `feasibility.xlsx` to `self$dir_results`, the
directory of `$export_tables()`. `$export_tables()` also writes it, from
the s3 results. The workbook holds these sheets, in this order:

- Study Specification:

  The sheet of `$export_tables()`.

- Rare conditions:

  The sheet of `$export_tables()`. It is absent when the plan holds no
  rare-condition decision.

- Minimum events (Y or -):

  One row per outcome and comparison, one column per whole follow-up
  year. Y: the per-protocol estimate passes the rule. -: it fails the
  rule, and the estimate is withheld unless the comparison is exempt.
  The comparison of an exempt enrollment ends with ` (exempt)`.

- Minimum events (#):

  The same layout. Each cell is the largest cut of 1, 2, 3, 4, 5, 10 and
  20 at which the estimate passes the rule.

- Attrition:

  The table of `$get_attrition()`.

- Table 1:

  The unweighted baseline table of each enrollment. One panel reads the
  s1 raw file, before imputation. The other reads the smallest s2
  analysis file of the enrollment.

The method stops when the spec has no
`min_expected_events_per_arm_per_year`. It also stops unless s1
completed and s2 wrote every analysis file after that s1 run.

#### Returns

The plan, invisibly. `self$feasibility` is a list:

- records:

  A list keyed by ETT id, then by estimand (`pp`, `itt`). Each record
  holds `rule`, `weight_col`, `follow_up`, `windows`,
  `reportable_weeks`, `withheld`, `fail_lo`, `fail_hi`, `fail_arm`,
  `status`, `max_passing_cut`, `exempt` and `by_year`. `by_year` holds
  one row per follow-up year: `year`, `reportable` and
  `max_passing_cut`, the decision up to the end of that year. It holds
  no expected events. `exempt` is `TRUE` when the spec sets
  `minimum_events_rule_exempt: true` on the enrollment of the ETT.
  `withheld` then still holds the verdict of the rule, and
  `$s3_analyze()` will withhold none of the estimates. The `itt` record
  also holds `decided_by = "pp"`.

- s1_run_id:

  The s1 run of the analysis files.

- swereg_version:

  The swereg version that ran the method.

- computed_at:

  When the method ran.

------------------------------------------------------------------------

### `TTEPlan$s1_generate_enrollments_and_ipw()`

Loop 1: Create trial panels from skeleton files and compute IPW.

Uses a two-pass pipeline to fix a cross-batch comparator-ratio
imbalance. Requires `self$spec` to be set (e.g., via
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)).

1.  **Pass 1a (scout)**: Lightweight parallel pass that reads each
    skeleton file, applies exclusions and treatment, and returns
    eligible `(person_id, enrollment_period_id, intervention)` tuples.
    No confounders or enrollment.

2.  **Centralized comparator draw**: Combines all tuples from all
    batches, then per `enrollment_period_id` keeps all intervention and
    samples `ratio * n_intervention` comparator globally. Stores counts
    on `self$enrollment_counts` for TARGET Item 8 reporting.

3.  **Pass 1b (full enrollment)**: Parallel pass that re-reads each
    skeleton file with full processing (exclusions + confounders +
    treatment), then enrolls using the pre-drawn IDs (skipping the
    per-batch draw). Produces panel-expanded TTEEnrollment objects.

#### Usage

    TTEPlan$s1_generate_enrollments_and_ipw(
      output_dir = NULL,
      impute_fn = tteenrollment_impute_confounders,
      stabilize = TRUE,
      n_workers = default_n_workers("s1"),
      swereg_dev_path = NULL,
      work_root = NULL,
      check_skeletons = TRUE
    )

#### Arguments

- `output_dir`:

  Optional directory override for output files. If `NULL` (default),
  uses `self$dir_tteplan`.

- `impute_fn`:

  Imputation callback or NULL (default:
  [tteenrollment_impute_confounders](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_impute_confounders.md)).
  swereg calls it with the panel and with the `.tte_entry__` snapshot
  names, not with the plain confounder names. It MUST impute only the
  columns it is given.

- `stabilize`:

  Logical, stabilize IPW (default: TRUE).

- `n_workers`:

  Integer, concurrent subprocesses. Default
  [default_n_workers](https://papadopoulos-lab.github.io/swereg/reference/default_n_workers.md)`("s1")`
  (1 unless `SWEREG_N_WORKERS_S1` is set).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

- `work_root`:

  Absolute path to a scratch root, or `NULL` (default). A leading `~`
  expands. The directory MUST already exist: s1 refuses a root it cannot
  find, and creates none. s1 works in `{root}/s1_work_{project_prefix}`,
  one flat directory per project. s1 first deletes every entry directly
  under `{root}` whose modification time is older than 14 days. Age is
  the only rule: the sweep deletes a dotfile like any other entry, and
  no keep-marker file protects one. `NULL` keeps
  `{data_meta_dir}/s1_work/{project_prefix}` and sweeps nothing.

- `check_skeletons`:

  Logical (default `TRUE`). Assert that the skeleton files this stage is
  about to read are one generation, and that generation is the one the
  embedded
  [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)
  describes. This is the definitive gate: the check in
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)
  ran when the plan was built, and the store can change before s1 runs.
  `FALSE` skips it.

------------------------------------------------------------------------

### `TTEPlan$s2_generate_analysis_files_and_ipcw_pp()`

Loop 2: Per-ETT IPCW-PP calculation and analysis file generation. For
each ETT, loads the imputed enrollment file, calls
`$s4_prepare_for_analysis()` (outcome + IPCW-PP + weight combination +
truncation), and saves the analysis-ready file.

s2 rewrites the analysis files of every ETT, so it drops the stored s3
results before it writes. It stamps each ETT with a new s2 run id in the
`s2_run_id` column of `$ett`. s3 copies that id into each result it
stores. `$export()`, `$export_tables()` and `$print_target_checklist()`
refuse a stored result whose s2 run id is not the current one of its
ETT.

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
  files are saved. If `NULL` (default), uses `self$dir_tteplan`.

- `estimate_ipcw_pp_separately_by_treatment`:

  Logical, estimate IPCW-PP separately by treatment group (default:
  TRUE). s2 records the value on each per-protocol analysis file, and
  item 6h of the TARGET checklist states it.

- `estimate_ipcw_pp_with_gam`:

  Logical, use GAM for IPCW-PP estimation (default: TRUE). s2 records it
  in the same way.

- `n_workers`:

  Integer, concurrent subprocesses (default: 1L).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

------------------------------------------------------------------------

### `TTEPlan$s3_analyze()`

Loop 3: Compute all analysis results and store on the plan.

For each enrollment: loads one analysis file and the raw file, computes
baseline characteristics (raw, unweighted, IPW, IPW truncated). For each
ETT: loads the analysis file, then computes the rates and the IRR with
both truncated and untruncated weights. For each subgroup the spec
names, it also computes the stratified IRRs and the effect-modification
test.

Every ETT also gets the ABSOLUTE scale, and nothing switches it off. Two
estimand and weight combinations carry it: per-protocol on
`analysis_weight_pp_trunc`, stored under `rd_pp_trunc`, and
intention-to-treat on `ipw_trunc`, stored under `rd_itt`. Each stores
one summary row at the end of follow-up, with `rd`, `rd_lo`, `rd_hi`,
`nnt`, `nnt_lo`, `nnt_hi`, `nnt_direction` and `interval_status`. Each
also stores the full curve, one row per stop time, under
`rd_curve_pp_trunc` or `rd_curve_itt`, with `surv_comparator` and
`surv_intervention` beside the risk difference.

The curve also carries `n_persons_at_risk_comparator` and
`n_persons_at_risk_intervention`. Each is a head count of distinct
people in that arm at that stop time. It is the count a numbers-at-risk
row reports. The figure reads it rather than opening the analysis file
again.

The bootstrap runs at 500 replicates with seed 1. Both are fixed here.
The confidence level is a STUDY property, read from
`spec$study$implementation$conf_level` and defaulting to 0.95. All three
are recorded on every stored risk-difference row. The export path
formats those numbers and never recomputes them. The IRR intervals and
the interval of the ratio of stratum IRRs use the same level.

The minimum-events rule. The RD item of each estimand also cuts
follow-up into 52-week windows. In each window it computes the expected
events of each arm under the null. They are the pooled weighted rate of
both arms times the weighted person-years of that arm. A window passes
when both arms reach `spec$min_expected_events_per_arm_per_year`. When a
window fails, the method withholds the estimates of that estimand. It
sets these to `NA`:

- the incidence rate ratios and the subgroup ratios;

- the risk difference and the number needed to treat;

- the interaction tests.

Their `interval_status` and `horizon_status` then read
`"below minimum-events rule: <arm> arm, weeks <lo>-<hi>"`, which names
the first failing window. Events, person-years, rates and the survival
curve stay. The record is stored as `horizon_pp` and `horizon_itt`, and
`$get_horizon_windows()` returns its window table. Which windows pass
depends on the observed events, through the pooled rate.

An enrollment that sets `minimum_events_rule_exempt: true` is exempt.
The method computes and stores the decision of its ETTs as for any ETT,
and withholds none of their estimates. Their `horizon_status` reads
`"exempt from minimum-events rule"`.

A feasibility-horizon ETT, one with `feasibility_horizon = TRUE` in
`$ett`, gets one work item per estimand. Each item computes the horizon
record of its estimand through the same `.tte_horizon_record()` call as
the RD item. The intention-to-treat record then takes the per-protocol
decision. The method stores no estimate for the ETT: no summary, rate,
ratio, risk difference, number needed to treat, survival curve or
subgroup result. The enrollment loop reads an analysis file of a
reported ETT.

The method stops when the spec has no
`min_expected_events_per_arm_per_year`. It also stops when an ETT of the
run has no horizon decision for an estimand.

Cost. Each risk difference is its own work item, so it is its own worker
process with its own read of the analysis file. That is two more reads
per ETT, or 1,080 more reads on a 540-ETT grid.

Results are stored in `self$results_enrollment` and `self$results_ett`.
Every targeted result is recomputed on each call (no skip cache). Use
`plan$save()` to persist.

The method ends with two lines on stdout. The first names every ETT
whose incidence rate ratio is `NA`, which means one arm held no event.
The second names every ETT whose fit raised a warning and still produced
an estimate. An ETT with no event in one arm appears on the first line
only, because its fit returns `NA` and raises a warning. Both counts
read the `irr_*` slots this call stored. The fit runs in a worker
subprocess. The pool keeps that subprocess's output only when the item
fails, so a warning from a successful fit reaches nobody. The driver is
the only place a reader of the job log meets either signal.

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

  Character vector of enrollment IDs to analyze, or `NULL` (default) for
  all.

- `ett_ids`:

  Character vector of ETT IDs to analyze, or `NULL` (default) for all.

- `output_dir`:

  Optional directory override. If `NULL` (default), uses
  `self$dir_tteplan` (falls back to the legacy `self$output_dir` for
  plans created before the CandidatePath migration).

- `swereg_dev_path`:

  Path to local swereg dev copy, or NULL.

- `n_workers`:

  Integer \>= 1 (default `1L`). Number of concurrent worker subprocesses
  for both the enrollment loop and the per-ETT loop. Each worker reads
  its own analysis file fresh, so peak RAM scales linearly with
  `n_workers`; on machines with multi-GB analysis files, set this
  conservatively. CPU threads per worker are auto-partitioned as
  `floor(detectCores() / n_workers)`.

------------------------------------------------------------------------

### `TTEPlan$print()`

Print the TTEPlan object.

The ETT grid it prints holds the reported ETTs. A feasibility-horizon
ETT, one that the spec key `feasibility_follow_up_weeks` adds, is not
printed.

#### Usage

    TTEPlan$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `TTEPlan$print_spec_summary()`

Print a target trial specification summary. Console-friendly summary
derived from the study specification stored on this plan. When
`$code_registry` is available, variable names are shown in red and
matched code details in blue (ANSI colors). A spec with a
`rare_condition_rule` also prints the rare-condition rule: `min_n`, the
driving enrollments, the candidates and the decision. Before s1 the
decision reads `not decided yet (s1 has not run)`. A stored decision
prints only when the rule in `$spec` still hashes to `$sparse_rule_hash`
and s1 completed for it. Otherwise the decision reads
`not decided yet (s1 has not completed for this rule)`. It also prints
the minimum-events rule, `min_expected_events_per_arm_per_year`: the
minimum number of expected events per arm in each follow-up year.

#### Usage

    TTEPlan$print_spec_summary()

#### Returns

`invisible(NULL)`

------------------------------------------------------------------------

### `TTEPlan$results_summary()`

Print a diagnostic summary of stored results.

Shows one row per ETT with enrollment, event count, and whether
IRR/rates computed successfully.

This method reads `self$results_ett` directly, and it is the one
DIAGNOSTIC exception to the rule that every consumer reads an accessor.
A tool that reports ABSENCE cannot read through an interface that hides
absence. The accessors report a missing slot and a skipped slot the same
way, as absent rows or as `NA`. They expose no skip envelope and no
failure reason. This method prints exactly three states. `"NULL"` names
a slot the plan does not hold. `"SKIP: <reason>"` names a worker that
failed. `"OK"` names a stored result.

It reports on the CACHE and never on a number. A caller that wants the
numbers calls `$get_estimates()`. A feasibility-horizon ETT gets no row,
as in `$get_estimates()`.

#### Usage

    TTEPlan$results_summary()

------------------------------------------------------------------------

### `TTEPlan$get_estimates()`

Every stored effect estimate, as one flat table.

One row per emulated trial, estimand and weighting.

`estimand` and `weights` are two columns, not one. `estimand` reads
`"pp"` or `"itt"`. `weights` reads `"truncated"` or `"untruncated"` and
names the weight of the estimate. Three combinations occur: per-protocol
truncated, per-protocol untruncated, and intention-to-treat truncated.
The intention-to-treat estimates use the truncated baseline
inverse-probability weight, `ipw_trunc`.

Three rows per emulated trial is an UPPER BOUND, not a promise. A
combination gets a row when the plan holds at least one of its rates,
incidence rate ratio and risk-difference slots. A combination the plan
holds nothing for gets no row. So a complete 540-trial grid returns
1,620 rows, and a partial one returns fewer.

The method computes nothing. It reads `plan$results_ett`, and it joins
the labels from `plan$ett` and `plan$spec`. A slot the plan does not
carry gives `NA` in that slot's columns. The method MUST NOT fill the
gap from a neighbouring slot.

`irr_estimable` is READ, not decided. `$s3_analyze()` decides it beside
the ratio and stores it. A result stored before that column existed
gives `NA`, and the method MUST NOT apply the rule to fill the gap.

Every number is a bare number. `irr_pvalue` is a probability, not
`"<0.001"`. `rd` is a proportion, not a rate per 10,000. The consumer
formats it.

Six sibling methods return the other stored results in the same shape:
`$get_curves()`, `$get_baselines()`, `$get_attrition()`,
`$get_matching()`, `$get_subgroups()` and `$get_horizon_windows()`. Each
takes no argument, and each computes nothing.

A feasibility-horizon ETT, one that the spec key
`feasibility_follow_up_weeks` adds, gets no row from this method,
`$get_curves()`, `$get_subgroups()` or `$get_horizon_windows()`. s3
stores its two horizon records and no estimate. The Minimum events
sheets of `$export_tables()` are the only output that shows its
decision.

`horizon_reportable_weeks` and `horizon_status` READ the decision of the
minimum-events rule that `$s3_analyze()` stored for the estimand. Both
weightings of the per-protocol estimand share one decision. A
`horizon_status` that starts with `"below minimum-events rule"` marks a
withheld estimand. Its ratio, risk difference and number needed to treat
are `NA`, and `interval_status` holds the same text. A result stored
before the rule gives `NA` in both columns.

An ETT of an enrollment that sets `minimum_events_rule_exempt: true` has
the `horizon_status` `"exempt from minimum-events rule"`. Its estimates
are reported, and `horizon_reportable_weeks` is its follow-up, whatever
the rule decided. `$get_horizon_windows()` keeps the computed decision.

The number needed to treat carries its interval. `nnt` is the point
estimate, and `nnt_lo` and `nnt_hi` are the bounds `$s3_analyze()`
stored. Both bounds are `NA` where `interval_status` reads
`"spans null"`, because the reciprocal of an interval that contains zero
is not an interval. A consumer MUST NOT invert `rd_lo` and `rd_hi`
itself, and MUST NOT print `nnt` alone where the bounds are missing.

`n_trials_int` and `n_trials_cmp` count the person-trials of each arm in
the analysis file of that estimand. On an intention-to-treat row they
are the intention-to-treat analysis set, which the CONSORT flow reads.

#### Usage

    TTEPlan$get_estimates()

#### Returns

A data.table with 49 columns. The identifiers come first, then the
weighted counts and the person-trials of each arm. The incidence rate
ratio follows, then the risk difference and the number needed to treat.
`n_boot`, `seed` and `conf_level` record what produced the
risk-difference interval.

------------------------------------------------------------------------

### `TTEPlan$get_curves()`

Every stored survival curve, as one flat table.

One row per emulated trial, estimand, weighting, arm and stop time.
`follow_up_interval` holds the stop time, in weeks from time zero, of
the rows the curve was evaluated at. `$s3_analyze()` stores one wide
curve per estimand, with a survival column for each arm. This method
returns one row per arm instead.

The table carries the numbers at risk beside survival.
`n_persons_at_risk` is an unweighted count of distinct people, per arm
per stop time. `$s3_analyze()` stores it and this method melts it. A
risk table reports people, so it cannot be derived from `surv`, which is
a weighted probability.

A curve stored before that column existed gives `NA`. A consumer that
draws a risk table MUST check for missing values first. It MUST refuse
to draw. A row of missing counts looks like a drawn risk table.

#### Usage

    TTEPlan$get_curves()

#### Returns

A data.table with columns `ett_id`, `estimand`, `weights`, `arm`,
`follow_up_interval`, `surv` and `n_persons_at_risk`.

------------------------------------------------------------------------

### `TTEPlan$get_baselines()`

Every stored baseline panel, as one flat table.

One row per enrollment, panel and table row. Three columns identify the
panel. `imputation` reads `"raw"` or `"imputed"`. `weighting` reads
`"none"`, `"ipw"` or `"ipw_trunc"`. `variant` reads `"main"` or
`"supplementary"`. Five combinations occur.

The `"raw"` panel needs a separate pre-imputation file. The table holds
no `"raw"` rows when the plan holds no such panel. The method MUST NOT
present another panel under that name.

`overall`, `comparator` and `intervention` are display strings, such as
`"12.3 (4.5)"` or `"120 (8.1%)"`. The producer stores them that way.
`smd_numeric` is the unrounded standardised mean difference.

`variable` repeats on every row of its block. The stored panel prints
the name once and indents its levels under it, so `variable` is blank
there. A renderer that wants that indent MUST blank the repeat itself.

`n_baseline` counts the person-trials of the per-protocol analysis set.
A person-trial that deviates from its assigned strategy at time zero has
no per-protocol follow-up, so `n_baseline` does not count it. The
intention-to-treat analysis keeps it, and the time-zero censoring factor
`ipcw_pp_time_zero` is fitted on it. The CONSORT diagram shows these
person-trials in a censoring box above the per-protocol analysis set. It
counts them as the intention-to-treat set of `$get_estimates()` minus
`n_baseline`.

#### Usage

    TTEPlan$get_baselines()

#### Returns

A data.table. `n_baseline`, `n_baseline_intervention` and
`n_baseline_comparator` repeat that enrollment's counts on every row.

------------------------------------------------------------------------

### `TTEPlan$get_attrition()`

The stored eligibility cascade, as one flat table.

One row per enrollment and stored row, in pipeline order. Counts are
remaining-after-step.

`$s1_generate_enrollments_and_ipw()` stores one row per trial and
criterion, plus ONE GLOBAL ROW per criterion. The global row carries the
true overall count of distinct people. This method returns EVERY STORED
ROW. `enrollment_period_id` is `NA` on a global row and the trial on a
per-trial row, so the caller filters on that column.

The method returns the stored rows and nothing else. It does not sum the
per-trial rows. It does not create a global row for a criterion that has
none. A criterion with per-trial rows and no global row therefore yields
per-trial rows and no global row.

Collapsing to one row per criterion is a RENDERER's decision, and
`.attrition_overall()` makes it. That renderer reads the global rows and
nothing else. It returns NULL when one criterion carries no global row,
and the enrollment then gets no attrition sheet and no CONSORT diagram.
This method makes no such decision. It returns every stored row, and the
renderer needs the per-trial rows to see a criterion that has only
those.

`step_order` is the position of the criterion in stored order, so every
row of one criterion carries the same value.

The table holds the ELIGIBILITY CASCADE only. It holds no
comparator-draw step and no analysis step, because
`$s1_generate_enrollments_and_ipw()` stores neither as a step.
`.build_cohort_flow()` builds those two rows and derives the per-step
change columns. Building a row is a renderer's job, so this method calls
that builder nowhere.

The table carries no step KIND, because nothing stores one. The first
stored criterion is the cohort start and every later one is an
exclusion. A consumer labels them from `step_order`, and this method
decides nothing.

#### Usage

    TTEPlan$get_attrition()

#### Returns

A data.table with columns `enrollment_id`, `enrollment_period_id`,
`step_order`, `step_name`, `n_persons`, `n_person_trials`,
`n_arm_intervention` and `n_arm_comparator`.

------------------------------------------------------------------------

### `TTEPlan$get_matching()`

The stored comparator-draw counts, as one flat table.

One row per enrollment and trial. `$s1_generate_enrollments_and_ipw()`
stores it that way. `n_intervention_total` and `n_comparator_total`
count every person-trial that was eligible for an arm.
`n_intervention_enrolled` and `n_comparator_enrolled` count the
person-trials the draw took. The per-protocol `n_baseline` of
`$get_baselines()` is smaller by two groups. The person-trials that
deviate from the assigned strategy at time zero stay in the
intention-to-treat analysis. A trial that enters at or after the
administrative end of follow-up is in neither analysis. The CONSORT
diagram shows each group as its own step.

This is a SIXTH method rather than four more columns on
`$get_attrition()`. The comparator-draw table has one row per enrollment
and trial. The attrition table has one row per enrollment, trial and
criterion. Joining them would repeat one comparator-draw count on every
criterion row, and report a grain that neither producer stored.

The method computes nothing. It does not sum across trials, and it
derives no enrolment ratio. `.build_cohort_flow()` sums the enrolled
counts to build its comparator-draw step, and that sum is a renderer's.

An enrollment that stored no comparator-draw table gets NO ROW.

#### Usage

    TTEPlan$get_matching()

#### Returns

A data.table with columns `enrollment_id`, `enrollment_period_id`,
`n_intervention_total`, `n_comparator_total`, `n_intervention_enrolled`
and `n_comparator_enrolled`.

------------------------------------------------------------------------

### `TTEPlan$get_subgroups()`

Every stored stratified estimate, as one flat table.

One row per emulated trial, estimand, weighting, subgroup variable and
subgroup level. `subgroup_level` reads `"all"` on the whole-cohort row,
and the level label on every other row.

`subgroup_var` is part of the KEY, not a label. One emulated trial MAY
carry several subgroup variables, and each one has its own `"all"` row.

TWO p-values, and they answer different questions.

- `irr_pvalue` is the stratum's own p-value. Is this stratum's rate
  ratio distinguishable from the null?

- `em_pvalue` is the interaction test. Do the strata differ from each
  other?

A consumer that renders one where the other belongs reports a different
finding. The two never share a name.

`em_pvalue`, `ratio_of_irrs`, `ratio_lo` and `ratio_hi` come from the
interaction test that `$s3_analyze()` stores. Each is one number for the
whole stratified result, so each repeats on every row of that result. A
renderer that wants them once shows them on the `"all"` row.

`ratio_of_irrs` is the ratio of the two stratum rate ratios. It is `NA`
unless the subgroup variable has exactly two levels.

The method reads the UNION of two stored families. `$s3_analyze()`
dispatches the stratified rate ratios and the interaction test as
separate work items, in separate subprocesses, so either can fail alone.
Four states occur.

- Both stored. Full rows.

- Stratified only. One row per stored level, with all four interaction
  columns `NA`.

- Interaction only. ONE row, with `subgroup_level` reading `"all"` and
  the four stratum columns `NA`. No stored table names the levels, so
  the method MUST NOT invent a stratum row.

- Neither stored. No rows, even when the specification names the
  variable.

A skipped stratified result reads as absent.

Coverage. Study 002 runs no stratified analysis, so this method is
tested against a fixture. Other studies in the fleet do configure
subgroups, so treat the schema as production.

#### Usage

    TTEPlan$get_subgroups()

#### Returns

A data.table with 13 columns: `ett_id`, `estimand`, `weights`,
`subgroup_var`, `subgroup_level`, `irr`, `irr_lo`, `irr_hi`,
`irr_pvalue`, `em_pvalue`, `ratio_of_irrs`, `ratio_lo` and `ratio_hi`.

------------------------------------------------------------------------

### `TTEPlan$get_horizon_windows()`

Every stored window of the minimum-events rule, as one flat table.

One row per emulated trial, estimand, follow-up window and arm. A window
is 52 weeks wide, and the last window ends at the follow-up of the ETT.
`py_weighted` and `events_weighted` are the weighted person-years and
events of the arm in the window. `rate_pooled` is the weighted events of
both arms over the weighted person-years of both arms. `events_expected`
is `rate_pooled` times `py_weighted`, the expected events under the
null. `pass` is the decision of the window: both arms reach
`min_expected_events_per_arm_per_year`.

Each row repeats the record it came from: `weight_col`,
`min_expected_events_per_arm_per_year`, `horizon_reportable_weeks` and
`horizon_status`. The method computes nothing. A result stored before
the rule gives no rows.

The table is the diagnostic of the rule. For an ETT of an enrollment
that sets `minimum_events_rule_exempt: true`, `pass` and
`horizon_reportable_weeks` still hold the computed decision, and
`horizon_status` reads `"exempt from minimum-events rule"`. The rule
withholds none of its estimates. `$get_estimates()` gives the reporting
horizon, which is then the follow-up of the ETT.

A feasibility-horizon ETT gets no row. Its decision shows only in the
Minimum events sheets.

#### Usage

    TTEPlan$get_horizon_windows()

#### Returns

A data.table with columns `ett_id`, `estimand`, `weight_col`,
`min_expected_events_per_arm_per_year`, `window_lo`, `window_hi`, `arm`,
`py_weighted`, `events_weighted`, `rate_pooled`, `events_expected`,
`pass`, `horizon_reportable_weeks` and `horizon_status`.

------------------------------------------------------------------------

### `TTEPlan$get_protocol_table()`

The target trial protocol table of one emulated trial.

The table has seven rows, one per protocol component in the order of
Dickerman et al., Table S1. Its three columns are `Protocol component`,
`Target trial specification` and `Target trial emulation`.

It is the table that the "Target trial protocol" sheet of
`$export_tables()` and `$excel_spec_summary()` holds, built by the same
two calls. The sheet adds a title row and a note row above it.

The specification column holds the clinical fields of the spec and the
prose under `target_trial`. The emulation column states the rules of the
`implementation` blocks. In it, each variable, criterion and exposure
level shows as its `name` or arm label in the spec. A variable that the
spec does not name shows as its column name.

The method reads `plan$spec` and `plan$ett`, and the rare-condition
decision when the plan holds one. It reads no s2 or s3 result, so it
works from s0 on.

#### Usage

    TTEPlan$get_protocol_table(ett_id = NULL)

#### Arguments

- `ett_id`:

  Character(1), the emulated trial the table documents. `NULL` takes the
  first ETT in the grid. A feasibility-horizon ETT is not in the grid
  this method reads, so its id stops as an unknown id.

#### Returns

A data.table with 7 rows and 3 character columns.

------------------------------------------------------------------------

### `TTEPlan$recompute_baselines()`

Recompute baseline characteristic tables in-process.

Reads each enrollment's smallest analysis file (and the raw file when
present) from disk and re-runs the new `swereg_table1` engine. The file
is one of a reported ETT, never one of a feasibility-horizon ETT. Used
to refresh stale results after upgrading swereg, without re-running the
full `$s3_analyze()` pipeline.

This is a PRODUCER, and the read is s3's. It calls
`.s3_enrollment_worker()`, the same worker `$s3_analyze()` calls, and it
stores what the worker returns. No renderer in the export path opens an
analysis file.

The method skips an enrollment whose cached panels are current. The rule
is `.baseline_panel_is_stale()`, the one `$export_tables()` uses. Each
present panel MUST be a `swereg_table1` and MUST carry `smd_numeric`. A
result holding no panel at all is not stale, so `force = FALSE` skips
it. `enrollment_ids` names which enrollments to consider, and the same
rule then decides each one.

`force = TRUE` skips the staleness test and recomputes every named
enrollment. It changes nothing else. The method still reads the stored
result, and it still carries forward every field the worker does not
return. Use it when the worker changed and the panel schema did not.

The method reports on one line how many enrollments it recomputed and
how many it skipped. The method warns when an enrollment it decides to
recompute has no analysis file on disk. That enrollment counts as
neither. A skipped enrollment never reaches that check, so a current
enrollment raises no warning about a missing file.

The method stops when an ETT of a named enrollment does not carry the s2
stamp of the current s1 run, as `$s3_analyze()` does. Its analysis files
then do not come from the current s1 run. A recomputed panel takes the
s1 and s2 run stamps of the analysis file it reads.

`$export_tables()` calls this method on its own when a stored panel is
stale. Call it yourself when you want the refresh to be a visible step.
The lazy path costs minutes. Whether it runs at all depends on what a
cached plan happens to hold.

#### Usage

    TTEPlan$recompute_baselines(
      output_dir = NULL,
      enrollment_ids = NULL,
      force = FALSE
    )

#### Arguments

- `output_dir`:

  Optional directory holding the `.qs2` files. Defaults to
  `self$output_dir`.

- `enrollment_ids`:

  Optional character vector. If NULL, refreshes every enrollment in
  `self$results_enrollment`.

- `force`:

  Logical. `TRUE` skips the staleness test and recomputes every named
  enrollment. It still carries forward every field the worker does not
  return. Defaults to `FALSE`.

#### Returns

`invisible(self)`.

------------------------------------------------------------------------

### `TTEPlan$slurm_job()`

Describe one Slurm job that runs one pipeline stage. The job body is a
single `Rscript` call to
[`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md),
and this method writes nothing.

The job name is derived, not passed: it is
`paste0(self$project_prefix, "_", stage)`. That is what keeps job names
unique across the projects that share one queue.

Every argument this method forwards reaches the script as a literal, so
`impute_fn` is absent from it. A function has no literal that a job
script can carry. Set `impute_fn` in the project's own stage script, or
call
[`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md)
directly.

#### Usage

    TTEPlan$slurm_job(
      stage,
      n_workers = .slurm_job_workers(stage),
      cpus = n_workers,
      mem = "95G",
      time,
      requeue = NULL,
      exclusive = TRUE,
      enrollment_ids = NULL,
      ett_ids = NULL,
      stabilize = NULL,
      estimate_ipcw_pp_with_gam = NULL,
      estimate_ipcw_pp_separately_by_treatment = NULL,
      work_root = NULL
    )

#### Arguments

- `stage`:

  One of `"s1"`, `"s2"`, `"s3"` or `"feasibility"`.

- `n_workers`:

  Worker count for the stage, forwarded to
  [`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md)
  as a named argument. The default is per stage:
  `default_n_workers("s1")` for `"s1"`, `1L` for `"s2"`,
  `default_n_workers("s3")` for `"s3"`, and
  `default_n_workers("feasibility")` for `"feasibility"`.

- `cpus`:

  Cores the job asks Slurm for. Defaults to `n_workers`.

- `mem`:

  Memory request in Slurm's own notation. Defaults to `"95G"`.

- `time`:

  Wall-clock limit, as `HH:MM:SS` or `D-HH:MM:SS`. There is no default,
  because a job that outlives its stage costs a queue slot.

- `requeue`:

  Logical(1), or `NULL` for the per-stage default. `TRUE` asks Slurm to
  requeue the job after a node failure. `NULL` gives `FALSE` for `"s1"`
  and `TRUE` for the other three stages. s1 deletes its own work
  directory at the start of every run, so a requeued s1 destroys the
  work the failed run had already done. The other stages delete nothing
  at start.

- `exclusive`:

  Logical(1). `TRUE` asks for the whole node.

- `enrollment_ids`:

  Character vector of enrollment ids for `"s3"`, or `NULL` to leave the
  stage default. Forwarded to
  [`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md)
  by name.

- `ett_ids`:

  Character vector of ETT ids for `"s3"`, or `NULL` to leave the stage
  default. Forwarded to
  [`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md)
  by name.

- `stabilize`:

  Logical(1) for `"s1"`, or `NULL` to leave the stage default. Forwarded
  to
  [`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md)
  by name.

- `estimate_ipcw_pp_with_gam`:

  Logical(1) for `"s2"`, or `NULL` to leave the stage default. Forwarded
  to
  [`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md)
  by name.

- `estimate_ipcw_pp_separately_by_treatment`:

  Logical(1) for `"s2"`, or `NULL` to leave the stage default. Forwarded
  to
  [`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md)
  by name.

- `work_root`:

  Scratch root for `"s1"`, or `NULL` to leave the stage default.
  Forwarded to
  [`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md)
  by name. The path reaches the script as the literal the caller gave.
  It names a directory on the compute node, so nothing normalises it
  here. That directory MUST exist on the compute node when the job runs.

#### Returns

An object of class `slurm_it`, from
[`batchit::slurm_it()`](https://papadopoulos-lab.github.io/batchit/reference/slurm_it.html).

------------------------------------------------------------------------

### `TTEPlan$add_one_ett()`

Add one ETT to the plan.

TTE (target trial emulation) names the method and the class prefix. An
ETT (emulated target trial) names one cell of the grid: one outcome x
one follow_up x one enrollment_id, and one row of `$ett`. ETT is always
countable. Write "one ETT" or "12 ETTs", never "the ETT approach".

ETTs sharing an enrollment_id use the same trial panels (same comparator
draw, same age group, same confounders). They differ only in outcome
and/or follow-up duration. This avoids redundant re-enrollment for each
outcome/follow-up combo.

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
      observed_var = NULL,
      intervention_tolerance_weeks = 0L,
      comparator_tolerance_weeks = 0L,
      argset = list(),
      feasibility_horizon = FALSE
    )

#### Arguments

- `enrollment_id`:

  Character, enrollment group identifier (e.g., "01").

- `outcome_var`:

  Character, name of the outcome column.

- `outcome_name`:

  Character, short human-readable outcome label (used in forest plot
  rows and Table S11).

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

- `observed_var`:

  The observation encoding, or NULL. Give a list with exactly one of
  `column` and `sentinel`. See the observation contract section of
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md).

- `intervention_tolerance_weeks`:

  Integer, the tolerance in weeks for the intervention arm (default:
  0L).

- `comparator_tolerance_weeks`:

  Integer, the tolerance in weeks for the comparator arm (default: 0L).

- `argset`:

  Named list with age_group, age_min, age_max (and optional
  person_id_var, outcome_description).

- `feasibility_horizon`:

  Logical(1), `FALSE` by default. `TRUE` marks a feasibility-horizon
  ETT. s2 builds it as any other ETT, s3 computes only the
  minimum-events rule for it, and only the Minimum events sheets show
  it. The spec key `feasibility_follow_up_weeks` adds these ETTs. See
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md).

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

  A
  [TTEDesign](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md)
  object with column mappings. It carries the observation encoding and
  both arm tolerances that the spec declared for this enrollment.

- enrollment_id:

  Character, the enrollment group ID

- age_range:

  Numeric vector of length 2: c(min, max)

- n_threads:

  Integer, number of data.table threads to use

- treatment_impl:

  List with variable, intervention_value, comparator_value (present when
  plan was built from a spec)

- comparator_to_intervention_ratio:

  Numeric. The draw takes this many times a trial's count of
  intervention individuals, capped at the comparators that trial holds.
  Present when the plan was built from a spec.

- seed:

  Integer. It makes the comparator draw reproducible. Present when the
  plan was built from a spec.

- reported_follow_up_time:

  Integer, the longest follow-up of the reported ETTs of the enrollment,
  in weeks. Present only when the enrollment holds a feasibility-horizon
  ETT. `design$follow_up_time` is then the feasibility horizon.

------------------------------------------------------------------------

### `TTEPlan$excel_spec_summary()`

Export the study specification to a standalone Excel file.

Writes the preregistration sheets of the plan. The Study Specification
sheet summarises the spec (design, criteria, confounders, outcomes,
enrollments) with ICD-10/ATC code annotations from the code registry.
The Target trial protocol sheet documents one ETT. The Rare conditions
sheet is written only when the plan holds a current sparse decision: the
rule in `$spec` still hashes to `$sparse_rule_hash`, and s1 completed
for it. Without one, the Study Specification sheet reads the rule as not
decided yet. Each sheet holds the same cells as the sheet of that name
that `$export_tables()` writes for the same plan state and the same
`protocol_ett_id`. No sheet reads an s2 or s3 result, so the method runs
straight after s0.

#### Usage

    TTEPlan$excel_spec_summary(path = NULL, protocol_ett_id = NULL)

#### Arguments

- `path`:

  Optional output path override. If `NULL` (default), writes to
  `self$spec_xlsx` (that is, `spec_<version>.xlsx` inside
  `self$dir_results`, where `<version>` is `self$spec_version`).

- `protocol_ett_id`:

  Optional ETT id that the Target trial protocol sheet documents. `NULL`
  (default) takes the first ETT of the grid. An id the plan does not
  hold warns and falls back to the first ETT.

#### Returns

`invisible(self)`

------------------------------------------------------------------------

### `TTEPlan$reload_spec()`

Refresh cosmetic spec fields (enrollment names, treatment arm labels,
outcome names, ETT descriptions) on a cached plan without re-running the
upstream pipeline.

Structural fields (confounders, exclusion criteria, follow-up windows,
comparator-draw parameters, etc.) are *not* applied - they would
invalidate the cached results. The differences are surfaced via a loud
warning and recorded in `self$spec_reload_skipped_diffs`.

#### Usage

    TTEPlan$reload_spec(spec_path = NULL, quiet = FALSE)

#### Arguments

- `spec_path`:

  Optional path to a `.yaml` study spec file. If `NULL` (default), uses
  `self$spec_path` (resolved from `dir_spec_cp` +
  `filename_spec(spec_version)`).

- `quiet`:

  Logical, suppress the success message (default FALSE).

#### Returns

`invisible(self)`.

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

`invisible(TRUE)` if versions match. Errors otherwise with an actionable
migration message.

------------------------------------------------------------------------

### `TTEPlan$get_sparse_decision()`

Read the frozen decision of the spec's `rare_condition_rule`.

#### Usage

    TTEPlan$get_sparse_decision()

#### Returns

A copy of the `sparse_decision` field, or NULL when the spec has no
`rare_condition_rule` or s1 has not run.

------------------------------------------------------------------------

### `TTEPlan$save()`

Save the plan to disk as `tteplan.qs2`.

Writes to `self$tteplan` by default – that is, `tteplan.qs2` inside the
directory resolved from `self$dir_tteplan_cp`. Supply `dir` to override
the destination (deprecated; used only by in-flight scripts that don't
yet have a `dir_tteplan_cp`).

Captures the destination path FIRST, then invalidates every
[CandidatePath](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md)
on the plan (and on its embedded
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md))
so the on-disk file never carries the saving host's resolved paths.
Reload with
[`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md).

#### Usage

    TTEPlan$save(dir = NULL)

#### Arguments

- `dir`:

  Optional destination directory override. If `NULL` (default), writes
  to `self$tteplan`.

#### Returns

Invisibly returns the file path.

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
