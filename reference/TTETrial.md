# TTETrial class for target trial emulation

TTETrial class for target trial emulation

TTETrial class for target trial emulation

## Details

Holds the trial data, design specification, and workflow state. Methods
modify in-place and return \`invisible(self)\` for \`\$\`-chaining. R6
reference semantics mean \`trial\$data\[, := ...\]\` modifies the
data.table in-place without copy-on-write overhead.

The \`data_level\` property controls which methods are available: -
\`"person_week"\`: Data has one row per person per time unit. Method
\`\$enroll()\` requires this level. - \`"trial"\`: Data has been
expanded to trial panels. Methods \`\$collapse()\`, \`\$ipw()\`,
\`\$prepare_outcome()\`, \`\$ipcw_pp()\`, \`\$combine_weights()\`, and
\`\$truncate()\` require this level.

The \`\$enroll()\` method transitions data from "person_week" to "trial"
level.

## Methods

\*\*Mutating (return \`invisible(self)\` for chaining):\*\*

- \`\$enroll(ratio, seed, extra_cols)\`:

  Sample comparison group and create trial panels

- \`\$collapse(period_width, ...)\`:

  Collapse time intervals to coarser periods

- \`\$ipw(stabilize)\`:

  Calculate inverse probability of treatment weights

- \`\$ipcw_pp(separate_by_exposure, use_gam, censoring_var)\`:

  Calculate IPCW for per-protocol

- \`\$combine_weights(ipw_col, ipcw_col, name)\`:

  Combine IPW and IPCW weights

- \`\$truncate(weight_cols, lower, upper, suffix)\`:

  Truncate extreme weights

- \`\$prepare_outcome(outcome, follow_up)\`:

  Prepare outcome-specific data

- \`\$impute_confounders(confounder_vars, seed)\`:

  Impute missing confounders

- \`\$weight_summary()\`:

  Print weight distribution diagnostics

\*\*Non-mutating (return data):\*\*

- \`\$extract()\`:

  Return the data.table

- \`\$summary(pretty)\`:

  Return summary statistics

- \`\$table1(ipw_col)\`:

  Generate baseline characteristics table

- \`\$rates(weight_col)\`:

  Calculate events, person-years, and rates

- \`\$irr(weight_col)\`:

  Fit Poisson models and extract IRR

- \`\$km(ipw_col, save_path, title)\`:

  Fit Kaplan-Meier curves

## See also

\[tte_trial()\] for creating trial objects, \[TTEDesign\] for design
class

Other tte_classes:
[`TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md),
[`tte_design()`](https://papadopoulos-lab.github.io/swereg/reference/tte_design.md),
[`tte_plan()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan.md),
[`tte_plan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_load.md),
[`tte_trial()`](https://papadopoulos-lab.github.io/swereg/reference/tte_trial.md)

## Public fields

- `data`:

  A data.table with trial data.

- `design`:

  A TTEDesign R6 object.

- `data_level`:

  Character, "person_week" or "trial".

- `steps_completed`:

  Character vector of completed workflow steps.

- `active_outcome`:

  Character or NULL, current outcome for IPCW-PP.

- `weight_cols`:

  Character vector of weight column names.

## Methods

### Public methods

- [`TTETrial$new()`](#method-TTETrial-new)

- [`TTETrial$print()`](#method-TTETrial-print)

- [`TTETrial$enroll()`](#method-TTETrial-enroll)

- [`TTETrial$collapse()`](#method-TTETrial-collapse)

- [`TTETrial$ipw()`](#method-TTETrial-ipw)

- [`TTETrial$ipcw_pp()`](#method-TTETrial-ipcw_pp)

- [`TTETrial$combine_weights()`](#method-TTETrial-combine_weights)

- [`TTETrial$truncate()`](#method-TTETrial-truncate)

- [`TTETrial$prepare_outcome()`](#method-TTETrial-prepare_outcome)

- [`TTETrial$impute_confounders()`](#method-TTETrial-impute_confounders)

- [`TTETrial$weight_summary()`](#method-TTETrial-weight_summary)

- [`TTETrial$extract()`](#method-TTETrial-extract)

- [`TTETrial$summary()`](#method-TTETrial-summary)

- [`TTETrial$table1()`](#method-TTETrial-table1)

- [`TTETrial$rates()`](#method-TTETrial-rates)

- [`TTETrial$irr()`](#method-TTETrial-irr)

- [`TTETrial$km()`](#method-TTETrial-km)

- [`TTETrial$clone()`](#method-TTETrial-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TTETrial object.

#### Usage

    TTETrial$new(
      data,
      design,
      data_level = "trial",
      steps_completed = character(),
      active_outcome = NULL,
      weight_cols = character()
    )

#### Arguments

- `data`:

  A data.table containing the trial data.

- `design`:

  A \[TTEDesign\] object specifying column mappings.

- `data_level`:

  Character, either "person_week" for pre-panel data or "trial" for
  post-panel data. Determines which methods can be applied.

- `steps_completed`:

  Character vector of completed workflow steps.

- `active_outcome`:

  Character or NULL, the current outcome for IPCW-PP analysis.

- `weight_cols`:

  Character vector of weight column names created.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the TTETrial object.

#### Usage

    TTETrial$print(...)

------------------------------------------------------------------------

### Method `enroll()`

Enroll participants into trials with matching and panel expansion.
Combines sampling (matching unexposed to exposed) and panel expansion
into a single step. Transitions \`data_level\` from "person_week" to
"trial".

#### Usage

    TTETrial$enroll(ratio = 2, seed = NULL, extra_cols = NULL)

#### Arguments

- `ratio`:

  Numeric, default 2. Sampling ratio for unexposed:exposed.

- `seed`:

  Integer or NULL. Random seed for reproducibility.

- `extra_cols`:

  Character vector of additional columns to include.

------------------------------------------------------------------------

### Method `collapse()`

Collapse time intervals to coarser periods. Wraps
\[tte_collapse_periods()\].

#### Usage

    TTETrial$collapse(
      period_width = 4L,
      time_var = NULL,
      first_cols = NULL,
      last_cols = NULL,
      max_cols = NULL,
      sum_cols = NULL
    )

#### Arguments

- `period_width`:

  Integer, default 4.

- `time_var`:

  Character or NULL; defaults to "trial_week" if present.

- `first_cols`:

  Character vector of additional first-aggregation columns.

- `last_cols`:

  Character vector of additional last-aggregation columns.

- `max_cols`:

  Character vector of additional max-aggregation columns.

- `sum_cols`:

  Character vector of additional sum-aggregation columns.

------------------------------------------------------------------------

### Method `ipw()`

Calculate inverse probability of treatment weights. Wraps
\[tte_calculate_ipw()\].

#### Usage

    TTETrial$ipw(stabilize = TRUE)

#### Arguments

- `stabilize`:

  Logical, default TRUE.

------------------------------------------------------------------------

### Method `ipcw_pp()`

Calculate IPCW for per-protocol analysis. Also combines weights (ipw \*
ipcw_pp), truncates, and drops intermediate IPCW columns. Wraps
\[tte_calculate_ipcw()\].

#### Usage

    TTETrial$ipcw_pp(
      separate_by_exposure = TRUE,
      use_gam = TRUE,
      censoring_var = NULL
    )

#### Arguments

- `separate_by_exposure`:

  Logical, default TRUE.

- `use_gam`:

  Logical, default TRUE.

- `censoring_var`:

  Character or NULL. If NULL, auto-detected.

------------------------------------------------------------------------

### Method `combine_weights()`

Combine IPW and IPCW weights. Wraps \[tte_combine_weights()\].

#### Usage

    TTETrial$combine_weights(
      ipw_col = "ipw",
      ipcw_col = "ipcw_pp",
      name = "analysis_weight_pp"
    )

#### Arguments

- `ipw_col`:

  Character, default "ipw".

- `ipcw_col`:

  Character, default "ipcw_pp".

- `name`:

  Character, default "analysis_weight_pp".

------------------------------------------------------------------------

### Method [`truncate()`](https://rdrr.io/r/base/seek.html)

Truncate extreme weights. Wraps \[tte_truncate_weights()\].

#### Usage

    TTETrial$truncate(
      weight_cols = NULL,
      lower = 0.01,
      upper = 0.99,
      suffix = "_trunc"
    )

#### Arguments

- `weight_cols`:

  Character vector or NULL.

- `lower`:

  Numeric, default 0.01.

- `upper`:

  Numeric, default 0.99.

- `suffix`:

  Character, default "\_trunc".

------------------------------------------------------------------------

### Method `prepare_outcome()`

Prepare outcome-specific data for per-protocol analysis. Computes event
times, censoring times, filters data, creates indicators. Can only be
run once per trial object (it deletes rows).

#### Usage

    TTETrial$prepare_outcome(outcome, follow_up = NULL)

#### Arguments

- `outcome`:

  Character scalar. Must be one of \`design\$outcome_vars\`.

- `follow_up`:

  Optional integer. Overrides \`design\$follow_up_time\`.

------------------------------------------------------------------------

### Method `impute_confounders()`

Impute missing confounders by sampling from observed values.

#### Usage

    TTETrial$impute_confounders(confounder_vars, seed = 4L)

#### Arguments

- `confounder_vars`:

  Character vector of confounder column names to impute.

- `seed`:

  Integer seed for reproducibility (default: 4L).

------------------------------------------------------------------------

### Method `weight_summary()`

Print weight distribution diagnostics.

#### Usage

    TTETrial$weight_summary()

------------------------------------------------------------------------

### Method `extract()`

Extract the data.table from the trial object.

#### Usage

    TTETrial$extract()

#### Returns

A data.table with the processed trial data.

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summarize trial data statistics.

#### Usage

    TTETrial$summary(pretty = FALSE)

#### Arguments

- `pretty`:

  Logical, default FALSE. If TRUE, prints formatted output.

#### Returns

If \`pretty = FALSE\`, a list with summary stats. If TRUE, prints
formatted output and invisibly returns the list.

------------------------------------------------------------------------

### Method `table1()`

Generate baseline characteristics table. Wraps
\[tableone::CreateTableOne()\] or \[tableone::svyCreateTableOne()\].

#### Usage

    TTETrial$table1(ipw_col = NULL)

#### Arguments

- `ipw_col`:

  Character or NULL. If specified, creates weighted table.

#### Returns

A tableone object.

------------------------------------------------------------------------

### Method `rates()`

Calculate events, person-years, and rates by exposure group.

#### Usage

    TTETrial$rates(weight_col)

#### Arguments

- `weight_col`:

  Character, required. Column name for weights.

#### Returns

A data.table with events, person-years, and rates.

------------------------------------------------------------------------

### Method `irr()`

Fit Poisson models and extract incidence rate ratios.

#### Usage

    TTETrial$irr(weight_col)

#### Arguments

- `weight_col`:

  Character, required. Column name for weights.

#### Returns

A data.table with IRR estimates and confidence intervals.

------------------------------------------------------------------------

### Method `km()`

Fit Kaplan-Meier curves and optionally plot. Uses IPW only (not IPCW)
because IPCW is time-varying.

#### Usage

    TTETrial$km(ipw_col, save_path = NULL, title = NULL)

#### Arguments

- `ipw_col`:

  Character, required. Column name for IPW weights.

- `save_path`:

  Character or NULL. If specified, saves the plot.

- `title`:

  Character or NULL. Plot title.

#### Returns

A svykm object (invisibly if save_path is specified).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TTETrial$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
design <- tte_design(
  exposure_var = "exposed",
  outcome_vars = "death",
  confounder_vars = c("age", "sex"),
  follow_up_time = 52L
)
trial <- tte_trial(my_trial_data, design)

# $-chaining
trial$
  collapse(period_width = 4)$
  ipw()$
  prepare_outcome(outcome = "death")$
  ipcw_pp(use_gam = TRUE)
} # }
```
