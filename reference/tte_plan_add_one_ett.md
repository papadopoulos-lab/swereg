# Add one ETT to a TTE plan

Adds exactly one Emulated Target Trial (ETT) row to a \[TTEPlan\]
object. Each ETT represents one outcome x follow_up x age_group
combination with its own design parameters (confounders, exposure
columns, etc.).

## Usage

``` r
tte_plan_add_one_ett(
  plan,
  enrollment_id,
  outcome_var,
  outcome_name,
  follow_up,
  confounder_vars,
  time_exposure_var,
  eligible_var,
  argset = list()
)
```

## Arguments

- plan:

  A \[TTEPlan\] object.

- enrollment_id:

  Character, enrollment group identifier (e.g., "01"). ETTs with the
  same enrollment_id share trial panels and must have matching design
  parameters.

- outcome_var:

  Character, name of the outcome column.

- outcome_name:

  Character, human-readable outcome name.

- follow_up:

  Integer, follow-up duration in weeks.

- confounder_vars:

  Character vector of confounder column names.

- time_exposure_var:

  Character or NULL, name of time-varying exposure column. NULL if ITT
  only. No default — must be explicit.

- eligible_var:

  Character or NULL, name of eligibility column. NULL if no eligibility
  filter. No default — must be explicit.

- argset:

  Named list of additional parameters:

  age_group

  :   Character, age group label (e.g., "50_60"). Required.

  age_min

  :   Numeric, minimum age. Required.

  age_max

  :   Numeric, maximum age. Required.

  person_id_var

  :   Character, name of person ID column. Default: "id".

## Value

The modified \[TTEPlan\] object with one additional ETT row.

## Details

ETTs with the same \`enrollment_id\` share trial panels and must have
matching design parameters (person_id_var, exposure_var,
time_exposure_var, eligible_var, confounder_vars).

## See also

\[tte_plan()\] for creating an empty plan, \[TTEPlan\] for class details

Other tte_classes:
[`TTEDesign()`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEPlan()`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md),
[`TTETrial()`](https://papadopoulos-lab.github.io/swereg/reference/TTETrial.md),
[`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md),
[`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md),
[`tte_plan_task()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_task.md),
[`tte_trial()`](https://papadopoulos-lab.github.io/swereg/reference/tte_trial.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- tte_plan("project002", skeleton_files, "2023-52")
argset <- list(age_group = "50_60", age_min = 50, age_max = 60)
plan <- plan |>
  tte_plan_add_one_ett(
    enrollment_id = "01",
    outcome_var = "death",
    outcome_name = "Death",
    follow_up = 52,
    confounder_vars = c("age", "education"),
    time_exposure_var = "rd_exposed",
    eligible_var = "eligible",
    argset = argset
  ) |>
  tte_plan_add_one_ett(
    enrollment_id = "01",
    outcome_var = "hosp",
    outcome_name = "Hospitalization",
    follow_up = 52,
    confounder_vars = c("age", "education"),
    time_exposure_var = "rd_exposed",
    eligible_var = "eligible",
    argset = argset
  )
} # }
```
