# TTEDesign class for target trial emulation

Holds column name mappings that define the schema for trial data. This
allows specifying variable names once and reusing them across all TTE
workflow functions.

swereg 26.9.0 moved time zero to the landmark. A `tstart == 0` row of a
schema-2 object is an entry band row, and a 26.9.0 reader takes it for a
landmark row. The check refuses the object, so that reinterpretation
cannot happen in silence.

## The interval convention

Every interval is `[tstart, tstop)`. The stop is exclusive. The person
leaves the risk set at `tstop`, and the row holds no part of that week.

Every duration is `tstop - tstart`. It never adds one. Three complete
four-week bands span `[0, 12)`. That is 12 person-weeks, and the bands
bill 4, 4 and 4. The inclusive convention bills 5, 5 and 5.

Every `weeks_to_*` column is a boundary on the same scale, counted from
the landmark at week 0. `weeks_to_event`, `weeks_to_protocol_deviation`,
`weeks_to_loss`, `weeks_to_admin_end` and `weeks_to_record_end` each
name the first week the person no longer contributes. A
`weeks_to_record_end` of 9 means the person held follow-up weeks 1 to 9
and bills 9 person-weeks.

The `+ 1` belongs to the inclusive convention, where weeks 1 through 4
is `4 - 1 + 1 = 4`. Both are correct arithmetic. The two differ in
whether the stop belongs to the interval. A mix of them makes a silently
wrong denominator, so swereg MUST read every stop as exclusive.

One place adds a week, and it converts a calendar reading into a stop.
`admin_censor_isoyearweek` names the last week under study, and
[`difftime()`](https://rdrr.io/r/base/difftime.html) returns the whole
weeks between that week and the landmark week. The stop is one week
later, because the person holds the whole of the administrative week.

`tests/testthat/test-interval-convention.R` pins each of the five
boundaries.

## See also

[TTEEnrollment](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md)
for the trial class.
[`vignette("tte-nomenclature")`](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.md)
for the enrollment band vocabulary.

Other tte_classes:
[`TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md),
[`TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)

## Public fields

- `person_id_var`:

  Character or NULL, person identifier column name.

- `id_var`:

  Character, person-trial identifier column name.

- `treatment_var`:

  Character, treatment column name. Enrollment reads every eligible week
  of the entry band, not only its first week. See the Baseline treatment
  section of TTEEnrollment for the full rule.

- `outcome_vars`:

  Character vector, outcome column names.

- `confounder_vars`:

  Character vector, confounder column names.

- `subgroup_vars`:

  Character vector or NULL, baseline subgroup (effect-modifier) column
  names; should be a subset of confounder_vars.

- `follow_up_time`:

  Integer, follow-up duration.

- `tstart_var`:

  Character, period start time column name.

- `tstop_var`:

  Character, period end time column name.

- `time_treatment_var`:

  Character or NULL, time-varying treatment column.

- `eligible_var`:

  Character or NULL, eligibility column name.

- `observed_var`:

  The observation encoding, or NULL. It is a `tte_observed_var` list
  with a `column` and a `sentinel`, exactly one of which is set.
  `column` names a real logical person-week column. `sentinel` of
  `"row_presence"` asserts a trimmed skeleton, where a row exists if and
  only if the person was observed that week.

- `intervention_tolerance_weeks`:

  Integer, the tolerance in weeks for the intervention arm.

- `comparator_tolerance_weeks`:

  Integer, the tolerance in weeks for the comparator arm.

- `admin_censor_var`:

  Character or NULL, admin censoring column.

- `admin_censor_isoyearweek`:

  Character or NULL, admin censoring date.

- `period_width`:

  Integer, band width in weeks for enrollment and aggregation.
  Eligibility and treatment status are assessed weekly. `period_width`
  collapses consecutive weeks into bands, and each band opens exactly
  one trial. Initiation in any week of a band is attributed to the start
  of that band.

## Methods

### Public methods

- [`TTEDesign$new()`](#method-TTEDesign-initialize)

- [`TTEDesign$check_version()`](#method-TTEDesign-check_version)

- [`TTEDesign$print()`](#method-TTEDesign-print)

- [`TTEDesign$clone()`](#method-TTEDesign-clone)

------------------------------------------------------------------------

### `TTEDesign$new()`

Create a new TTEDesign object.

#### Usage

    TTEDesign$new(
      person_id_var = "id",
      id_var = "enrollment_person_trial_id",
      treatment_var,
      outcome_vars,
      confounder_vars,
      follow_up_time,
      subgroup_vars = NULL,
      tstart_var = "tstart",
      tstop_var = "tstop",
      time_treatment_var = NULL,
      eligible_var = NULL,
      observed_var = NULL,
      intervention_tolerance_weeks = 0L,
      comparator_tolerance_weeks = 0L,
      admin_censor_var = NULL,
      admin_censor_isoyearweek = NULL,
      period_width = 4L
    )

#### Arguments

- `person_id_var`:

  Character or NULL, name of the person identifier column (default:
  `"id"`).
  [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
  names the person identifier `id`, and `TTEPlan` passes `"id"` whenever
  an argset does not override it, so the default matches what the
  pipeline already builds. A person contributes many sequential trials,
  so this column is what separates a head count of people from a count
  of person-trials.

- `id_var`:

  Character, name of the person-trial identifier column (default:
  "enrollment_person_trial_id").

- `treatment_var`:

  Character, name of the baseline treatment column. It holds `TRUE` for
  the intervention arm, `FALSE` for the comparator arm, and `NA` outside
  the two arms. Enrollment reads every eligible week of the entry band,
  not only its first week. See the Baseline treatment section of
  TTEEnrollment for the full rule.

- `outcome_vars`:

  Character vector, names of outcome event indicator columns.

- `confounder_vars`:

  Character vector, names of confounder columns for propensity/censoring
  models.

- `follow_up_time`:

  Integer, expected follow-up duration in time units.

- `subgroup_vars`:

  Character vector or NULL, baseline subgroup (effect-modifier) column
  names; should be a subset of confounder_vars.

- `tstart_var`:

  Character, name of period start time column (default: "tstart").

- `tstop_var`:

  Character, name of period end time column (default: "tstop").

- `time_treatment_var`:

  Character or NULL, name of time-varying treatment column for
  per-protocol analysis (default: NULL).

- `eligible_var`:

  Character or NULL, name of eligibility indicator column (default:
  NULL).

- `observed_var`:

  The observation encoding, or NULL (default: NULL). It states how the
  data records that a person was under observation in a week. Give a
  list with exactly one of two keys. `list(column = "rd_observed")`
  names a real logical person-week column.
  `list(sentinel = "row_presence")` asserts that the caller already
  deleted every unobserved person-week. A row then exists if and only if
  the person was observed that week. Person-week data MUST declare one
  of the two forms. Already-expanded trial data MAY leave this NULL. One
  row there is one trial, and not one week of observation.

- `intervention_tolerance_weeks`:

  Integer, the tolerance in weeks for the intervention arm (default:
  0L). It MUST be a whole number of at least 0.

- `comparator_tolerance_weeks`:

  Integer, the tolerance in weeks for the comparator arm (default: 0L).
  It MUST be a whole number of at least 0.

- `admin_censor_var`:

  Character or NULL, name of administrative censoring boundary column
  (default: NULL). Mutually exclusive with `admin_censor_isoyearweek`.
  Not implemented in outcome preparation: `s5_prepare_outcome()` stops
  if this is set – use `admin_censor_isoyearweek` instead.

- `admin_censor_isoyearweek`:

  Character or NULL, the study end date in ISO year-week format (e.g.,
  "2023-52"). When set, administrative censoring is computed internally
  as weeks from each trial's entry date to this global study end date.
  Requires an `isoyearweek` column in the data. Mutually exclusive with
  `admin_censor_var` (default: NULL).

- `period_width`:

  Integer, band width in weeks for enrollment and time aggregation
  (default: 4L). The input is a person-week skeleton, so eligibility and
  treatment status are assessed weekly. `period_width` then collapses
  consecutive weeks into bands, and each band opens exactly one trial.
  With `period_width = 4L`, one trial opens every four weeks, not one
  trial per week. Initiation in any week of a band is attributed to the
  start of that band. Must be a positive integer.

------------------------------------------------------------------------

### `TTEDesign$check_version()`

Check this object's schema version against the current class version. It
stops when the object carries an older schema.

#### Usage

    TTEDesign$check_version()

#### Returns

`invisible(TRUE)` when the versions match. It stops otherwise.

------------------------------------------------------------------------

### `TTEDesign$print()`

Print the TTEDesign object.

#### Usage

    TTEDesign$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `TTEDesign$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TTEDesign$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Design for post-panel (trial-level) data
design <- TTEDesign$new(
  id_var = "enrollment_person_trial_id",
  treatment_var = "baseline_intervention",
  outcome_vars = c("death", "hosp"),
  confounder_vars = c("age", "education"),
  follow_up_time = 156L
)

# Design for pre-panel (person-week) data with full workflow
design_prepanel <- TTEDesign$new(
  person_id_var = "id",
  treatment_var = "baseline_intervention",
  outcome_vars = c("death", "hosp"),
  confounder_vars = c("age", "education"),
  follow_up_time = 156L,
  eligible_var = "eligible",
  observed_var = list(column = "rd_observed")
)

# The same design on a trimmed skeleton. A row exists if and only if the
# person was observed that week, so there is no column to name.
design_trimmed <- TTEDesign$new(
  person_id_var = "id",
  treatment_var = "baseline_intervention",
  outcome_vars = c("death", "hosp"),
  confounder_vars = c("age", "education"),
  follow_up_time = 156L,
  eligible_var = "eligible",
  observed_var = list(sentinel = "row_presence")
)
```
