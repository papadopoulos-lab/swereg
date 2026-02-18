# Create an empty TTE plan

Constructor for \[TTEPlan\] objects. Creates an empty plan with just
infrastructure parameters. Use \[tte_plan_add_one_ett()\] to add ETTs.

## Usage

``` r
tte_plan(project_prefix, skeleton_files, global_max_isoyearweek)
```

## Arguments

- project_prefix:

  string used for file naming (e.g., "project002_ozel_psychosis").

- skeleton_files:

  Character vector of skeleton file paths.

- global_max_isoyearweek:

  Administrative censoring boundary (isoyearweek string).

## Value

A \[TTEPlan\] object with no ETTs.

## See also

\[TTEPlan\] for class details, \[tte_plan_add_one_ett()\] to add ETTs,
\[tte_generate_enrollments()\] for the pipeline

Other tte_classes:
[`TTEDesign()`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEPlan()`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md),
[`TTETrial()`](https://papadopoulos-lab.github.io/swereg/reference/TTETrial.md),
[`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md),
[`tte_plan_add_one_ett()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_add_one_ett.md),
[`tte_plan_save()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_save.md),
[`tte_plan_task()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_task.md),
[`tte_trial()`](https://papadopoulos-lab.github.io/swereg/reference/tte_trial.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- tte_plan(
  project_prefix = "project002",
  skeleton_files = skeleton_files,
  global_max_isoyearweek = "2023-52"
)
plan <- plan |> tte_plan_add_one_ett(
  outcome_var = "death",
  outcome_name = "Death",
  follow_up = 52,
  age_group = "50_60",
  age_min = 50,
  age_max = 60,
  confounder_vars = c("age", "sex", "education")
)
} # }
```
