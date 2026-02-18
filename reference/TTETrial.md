# TTETrial class for target trial emulation

Holds the trial data, design specification, and workflow state. Methods
modify and return self for chaining operations in a fluent API.

## Usage

``` r
TTETrial(
  data = NULL,
  design = TTEDesign(),
  data_level = "trial",
  steps_completed = character(0),
  active_outcome = character(0),
  weight_cols = character(0)
)
```

## Arguments

- data:

  A data.table containing the trial data.

- design:

  A \[TTEDesign\] object specifying column mappings.

- data_level:

  Character, either "person_week" for pre-panel data or "trial" for
  post-panel data. Determines which methods can be applied.

- steps_completed:

  Character vector of completed workflow steps.

- active_outcome:

  Character or NULL, the current outcome for IPCW-PP analysis.

- weight_cols:

  Character vector of weight column names created.

## Details

The \`data_level\` property controls which methods are available: -
\`"person_week"\`: Data has one row per person per time unit. Methods
\[tte_match()\] and \[tte_expand()\] require this level. - \`"trial"\`:
Data has been expanded to trial panels. Methods \[tte_collapse()\],
\[tte_ipw()\], \[tte_prepare_outcome()\], \[tte_ipcw_pp()\],
\[tte_weights()\], and \[tte_truncate()\] require this level.

The \[tte_expand()\] method transitions data from "person_week" to
"trial" level.

## See also

\[tte_trial()\] for creating trial objects, \[TTEDesign\] for design
class

Other tte_classes:
[`TTEDesign()`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEPlan()`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md),
[`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md),
[`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md),
[`tte_plan_add_one_ett()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_add_one_ett.md),
[`tte_plan_save()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_save.md),
[`tte_plan_task()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_task.md),
[`tte_trial()`](https://papadopoulos-lab.github.io/swereg/reference/tte_trial.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# For trial-level data
design <- tte_design(
  exposure_var = "exposed",
  outcome_vars = "death",
  confounder_vars = c("age", "sex"),
  follow_up_time = 52L
)
trial <- tte_trial(my_trial_data, design)

# For person-week data (full workflow)
design <- tte_design(
  person_id_var = "id",
  exposure_var = "exposed",
  outcome_vars = "death",
  confounder_vars = c("age", "sex"),
  follow_up_time = 52L,
  eligible_var = "eligible"
)
trial <- tte_trial(my_person_week_data, design) |>
  tte_match(ratio = 2) |>
  tte_expand()
} # }
```
