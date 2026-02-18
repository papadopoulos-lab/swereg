# TTEPlan class for trial generation planning

Bundles the ETT grid, skeleton file paths, and design column names into
a single object using a builder pattern. Create an empty plan with
\[tte_plan()\], then add ETTs one at a time with
\[tte_plan_add_one_ett()\]. Supports \`plan\[\[i\]\]\` to extract the
i-th task (enrollment) for interactive testing.

## Usage

``` r
TTEPlan(
  project_prefix = character(0),
  ett = NULL,
  skeleton_files = character(0),
  global_max_isoyearweek = NULL
)
```

## Arguments

- project_prefix:

  Character, string used for file naming.

- ett:

  NULL or a data.table with per-ETT columns including design params.

- skeleton_files:

  Character vector of skeleton file paths.

- global_max_isoyearweek:

  Administrative censoring boundary (isoyearweek string).

## Details

Design parameters (confounder_vars, person_id_var, exposure_var, etc.)
are stored per-ETT in the \`ett\` data.table, allowing different ETTs to
use different confounders or design columns. Within an enrollment_id
(same follow_up + age_group), design params must match.

## See also

\[tte_plan()\] for the constructor, \[tte_plan_add_one_ett()\] to add
ETTs, \[tte_plan_task()\] to extract tasks,
\[tte_generate_enrollments()\] for the pipeline

Other tte_classes:
[`TTEDesign()`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTETrial()`](https://papadopoulos-lab.github.io/swereg/reference/TTETrial.md),
[`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md),
[`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md),
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
  confounder_vars = c("age", "education"),
  time_exposure_var = "rd_exposed",
  eligible_var = "eligible"
)

# Extract first task for interactive testing
task <- plan[[1]]
task$design
task$age_range
} # }
```
