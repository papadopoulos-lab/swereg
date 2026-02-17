# Extract a task from a TTE plan

Returns a list with the TTEDesign object and metadata for the i-th
enrollment_id group. Design parameters (confounders, exposure columns)
are read from the per-ETT columns in the \`ett\` data.table. Useful for
interactive testing of the process_fn callback.

## Usage

``` r
tte_plan_task(plan, i = 1L)
```

## Arguments

- plan:

  A \[TTEPlan\] object.

- i:

  Integer index (1-based) into the unique enrollment_id groups.

## Value

A list with components:

- design:

  A \[TTEDesign\] object configured for this task

- enrollment_id:

  The enrollment_id string for this task

- age_range:

  Numeric vector of length 2 (min, max age)

- n_threads:

  Number of threads (from \[parallel::detectCores()\])

## See also

\[TTEPlan\], \[tte_plan()\]

Other tte_classes:
[`TTEDesign()`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEPlan()`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md),
[`TTETrial()`](https://papadopoulos-lab.github.io/swereg/reference/TTETrial.md),
[`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md),
[`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md),
[`tte_plan_add_one_ett()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_add_one_ett.md),
[`tte_trial()`](https://papadopoulos-lab.github.io/swereg/reference/tte_trial.md)

## Examples

``` r
if (FALSE) { # \dontrun{
task <- tte_plan_task(plan, 1)
# or equivalently: task <- plan[[1]]
result <- process_fn(task, plan@skeleton_files[1])
} # }
```
