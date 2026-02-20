# Create a TTE trial object

Constructor function for \[TTETrial\] objects. Wraps trial data with a
design specification to enable fluent \`\$\`-chaining.

## Usage

``` r
tte_trial(data, design, data_level = NULL)
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

## Value

A \[TTETrial\] object.

## See also

\[TTETrial\] for class details, \[tte_design()\] for creating designs

Other tte_classes:
[`TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md),
[`TTETrial`](https://papadopoulos-lab.github.io/swereg/reference/TTETrial.md),
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
trial <- tte_trial(my_trial_data, design)
trial$
  collapse(period_width = 4)$
  ipw()
} # }
```
