# Create a TTE enrollment object

Constructor function for \[TTEEnrollment\] objects. Wraps enrollment
data with a design specification to enable fluent \`\$\`-chaining.

## Usage

``` r
tte_enrollment(
  data,
  design,
  data_level = NULL,
  ratio = NULL,
  seed = NULL,
  extra_cols = NULL
)
```

## Arguments

- data:

  A data.table containing the trial data.

- design:

  A \[TTEDesign\] object specifying column mappings.

- data_level:

  Character or NULL. If NULL (default), auto-detects based on which
  identifier column exists in data. "person_week" for pre-panel data
  (requires person_id_var), "trial" for post-panel data (requires
  id_var).

- ratio:

  Numeric or NULL. If provided, automatically enrolls participants
  (sampling comparison group and creating trial panels). Only valid for
  person_week data.

- seed:

  Integer or NULL. Random seed for enrollment reproducibility.

- extra_cols:

  Character vector or NULL. Extra columns to include in trial panels
  during enrollment.

## Value

A \[TTEEnrollment\] object.

## See also

\[TTEEnrollment\] for class details, \[tte_design()\] for creating
designs

Other tte_classes:
[`TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md),
[`TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md),
[`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md),
[`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md),
[`tte_plan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_load.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Trial-level data (auto-detected)
design <- tte_design(
  exposure_var = "exposed",
  outcome_vars = "death",
  confounder_vars = c("age", "sex"),
  follow_up_time = 52L
)
trial <- tte_enrollment(my_trial_data, design)
trial$
  collapse(period_width = 4)$
  ipw()

# Person-week data with auto-enroll
enrollment <- tte_enrollment(skeleton, design,
  ratio = 2, seed = 4, extra_cols = "isoyearweek"
)
} # }
```
