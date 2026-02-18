# Save a TTE plan to disk

Saves the ETT grid and admin cutoff from a \[TTEPlan\] as a \`.qs2\`
file. File is named \`{project_prefix}\_plan.qs2\` inside \`dir\`.

## Usage

``` r
tte_plan_save(plan, dir)
```

## Arguments

- plan:

  A \[TTEPlan\] object.

- dir:

  Directory to save into.

## Value

Invisibly returns the file path.

## See also

\[tte_plan()\] for creating plans

Other tte_classes:
[`TTEDesign()`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEPlan()`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md),
[`TTETrial()`](https://papadopoulos-lab.github.io/swereg/reference/TTETrial.md),
[`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md),
[`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md),
[`tte_plan_add_one_ett()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_add_one_ett.md),
[`tte_plan_task()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_task.md),
[`tte_trial()`](https://papadopoulos-lab.github.io/swereg/reference/tte_trial.md)
