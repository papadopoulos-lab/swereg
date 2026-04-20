# TTEDesign class for target trial emulation

TTEDesign class for target trial emulation

TTEDesign class for target trial emulation

## Details

Holds column name mappings that define the schema for trial data. This
allows specifying variable names once and reusing them across all TTE
workflow functions.

## See also

\[TTEEnrollment\] for the trial class

Other tte_classes:
[`TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md),
[`TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)

## Public fields

- `person_id_var`:

  Character or NULL, person identifier column name.

- `id_var`:

  Character, person-trial identifier column name.

- `treatment_var`:

  Character, treatment column name.

- `outcome_vars`:

  Character vector, outcome column names.

- `confounder_vars`:

  Character vector, confounder column names.

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

- `admin_censor_var`:

  Character or NULL, admin censoring column.

- `admin_censor_isoyearweek`:

  Character or NULL, admin censoring date.

- `period_width`:

  Integer, band width in weeks for enrollment/aggregation.

## Methods

### Public methods

- [`TTEDesign$new()`](#method-TTEDesign-new)

- [`TTEDesign$check_version()`](#method-TTEDesign-check_version)

- [`TTEDesign$print()`](#method-TTEDesign-print)

- [`TTEDesign$clone()`](#method-TTEDesign-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TTEDesign object.

#### Usage

    TTEDesign$new(
      person_id_var = NULL,
      id_var = "enrollment_person_trial_id",
      treatment_var,
      outcome_vars,
      confounder_vars,
      follow_up_time,
      tstart_var = "tstart",
      tstop_var = "tstop",
      time_treatment_var = NULL,
      eligible_var = NULL,
      admin_censor_var = NULL,
      admin_censor_isoyearweek = NULL,
      period_width = 4L
    )

#### Arguments

- `person_id_var`:

  Character or NULL, name of the person identifier column for pre-panel
  (person-week) data (default: NULL).

- `id_var`:

  Character, name of the person-trial identifier column (default:
  "enrollment_person_trial_id").

- `treatment_var`:

  Character, name of the baseline treatment column.

- `outcome_vars`:

  Character vector, names of outcome event indicator columns.

- `confounder_vars`:

  Character vector, names of confounder columns for propensity/censoring
  models.

- `follow_up_time`:

  Integer, expected follow-up duration in time units.

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

- `admin_censor_var`:

  Character or NULL, name of administrative censoring boundary column
  (default: NULL). Mutually exclusive with \`admin_censor_isoyearweek\`.

- `admin_censor_isoyearweek`:

  Character or NULL, the study end date in ISO year-week format (e.g.,
  "2023-52"). When set, administrative censoring is computed internally
  as weeks from each trial's entry date to this global study end date.
  Requires an \`isoyearweek\` column in the data. Mutually exclusive
  with \`admin_censor_var\` (default: NULL).

- `period_width`:

  Integer, band width in weeks for enrollment and time aggregation
  (default: 4L). Calendar time is grouped into bands of this width. Must
  be a positive integer.

------------------------------------------------------------------------

### Method `check_version()`

Check if this object's schema version matches the current class version.
Warns if the object was saved with an older schema version.

#### Usage

    TTEDesign$check_version()

#### Returns

\`invisible(TRUE)\` if versions match, \`invisible(FALSE)\` otherwise.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the TTEDesign object.

#### Usage

    TTEDesign$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `clone()`

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
  eligible_var = "eligible"
)
```
