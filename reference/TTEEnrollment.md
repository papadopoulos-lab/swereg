# TTEEnrollment class for target trial emulation

TTEEnrollment class for target trial emulation

TTEEnrollment class for target trial emulation

## Details

Holds the enrollment data, design specification, and workflow state.
Methods modify in-place and return \`invisible(self)\` for
\`\$\`-chaining. R6 reference semantics mean \`trial\$data\[, := ...\]\`
modifies the data.table in-place without copy-on-write overhead.

The \`data_level\` property controls which methods are available: -
\`"person_week"\`: Data has one row per person per time unit. Pass
\`ratio\` to the constructor to enroll and transition to trial level. -
\`"trial"\`: Data has been expanded to trial panels (band-level).
Methods \`\$s2_ipw()\`, \`\$s4_prepare_for_analysis()\`, and
\`\$s3_truncate_weights()\` require this level.

Enrollment (matching + panel expansion) transitions data from
"person_week" to "trial" level and is triggered by passing \`ratio\` to
the constructor.

## Methods

\*\*Mutating (return \`invisible(self)\` for chaining, step-numbered for
execution order):\*\*

- \`\$s1_impute_confounders(confounder_vars, seed)\`:

  Step 1: Impute missing confounders

- \`\$s2_ipw(stabilize)\`:

  Step 2: Calculate inverse probability of treatment weights

- \`\$s3_truncate_weights(weight_cols, lower, upper, suffix)\`:

  Step 3: Truncate extreme weights

- \`\$s4_prepare_for_analysis(outcome, follow_up, ...)\`:

  Step 4: Prepare outcome data and calculate IPCW-PP in one step

\*\*Non-mutating (return data):\*\*

- \`\$extract()\`:

  Return the data.table

- \`\$summary(pretty)\`:

  Return summary statistics

- \`\$weight_summary()\`:

  Print weight distribution diagnostics

- \`\$table1(ipw_col)\`:

  Generate baseline characteristics table

- \`\$rates(weight_col)\`:

  Calculate events, person-years, and rates

- \`\$irr(weight_col)\`:

  Fit Poisson models and extract IRR

- \`\$km(ipw_col, save_path, title)\`:

  Fit Kaplan-Meier curves

\*\*Active bindings:\*\*

- \`\$enrollment_stage\`:

  Derived lifecycle stage: \`"pre_enrollment"\`, \`"enrolled"\`, or
  \`"analysis_ready"\`

## See also

\[TTEDesign\] for design class

Other tte_classes:
[`TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)

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

## Active bindings

- `enrollment_stage`:

  Derived lifecycle stage (read-only). Returns \`"pre_enrollment"\` when
  \`data_level == "person_week"\`, \`"analysis_ready"\` when
  \`s5_prepare_outcome\` has been run, or \`"enrolled"\` otherwise.

## Methods

### Public methods

- [`TTEEnrollment$new()`](#method-TTEEnrollment-new)

- [`TTEEnrollment$print()`](#method-TTEEnrollment-print)

- [`TTEEnrollment$check_version()`](#method-TTEEnrollment-check_version)

- [`TTEEnrollment$s1_impute_confounders()`](#method-TTEEnrollment-s1_impute_confounders)

- [`TTEEnrollment$s2_ipw()`](#method-TTEEnrollment-s2_ipw)

- [`TTEEnrollment$s3_truncate_weights()`](#method-TTEEnrollment-s3_truncate_weights)

- [`TTEEnrollment$s4_prepare_for_analysis()`](#method-TTEEnrollment-s4_prepare_for_analysis)

- [`TTEEnrollment$extract()`](#method-TTEEnrollment-extract)

- [`TTEEnrollment$summary()`](#method-TTEEnrollment-summary)

- [`TTEEnrollment$weight_summary()`](#method-TTEEnrollment-weight_summary)

- [`TTEEnrollment$table1()`](#method-TTEEnrollment-table1)

- [`TTEEnrollment$rates()`](#method-TTEEnrollment-rates)

- [`TTEEnrollment$irr()`](#method-TTEEnrollment-irr)

- [`TTEEnrollment$heterogeneity_test()`](#method-TTEEnrollment-heterogeneity_test)

- [`TTEEnrollment$km()`](#method-TTEEnrollment-km)

- [`TTEEnrollment$clone()`](#method-TTEEnrollment-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TTEEnrollment object.

#### Usage

    TTEEnrollment$new(
      data,
      design,
      data_level = NULL,
      steps_completed = character(),
      active_outcome = NULL,
      weight_cols = character(),
      ratio = NULL,
      seed = NULL,
      extra_cols = NULL,
      enrolled_ids = NULL,
      own_data = FALSE
    )

#### Arguments

- `data`:

  A data.table containing the trial data. A copy is made automatically
  to avoid modifying the caller's data.

- `design`:

  A \[TTEDesign\] object specifying column mappings.

- `data_level`:

  Character or NULL. If NULL (default), auto-detects based on which
  identifier column exists in data. "person_week" for pre-panel data
  (requires person_id_var), "trial" for post-panel data (requires
  id_var).

- `steps_completed`:

  Character vector of completed workflow steps.

- `active_outcome`:

  Character or NULL, the current outcome for IPCW-PP analysis.

- `weight_cols`:

  Character vector of weight column names created.

- `ratio`:

  Numeric or NULL. If provided, automatically enrolls participants
  (sampling comparison group and creating trial panels). Only valid for
  person_week data.

- `seed`:

  Integer or NULL. Random seed for enrollment reproducibility.

- `extra_cols`:

  Character vector or NULL. Extra columns to include in trial panels
  during enrollment.

- `enrolled_ids`:

  data.table or NULL. Pre-matched enrollment IDs from the two-pass
  pipeline. When provided, enrollment skips the matching phase and uses
  these IDs directly.

- `own_data`:

  Logical. If TRUE, takes ownership of the data.table without copying
  it. Use only when the caller will not reuse the data.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the TTEEnrollment object.

#### Usage

    TTEEnrollment$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `check_version()`

Check if this object's schema version matches the current class version.
Warns if the object was saved with an older schema version.

#### Usage

    TTEEnrollment$check_version()

#### Returns

\`invisible(TRUE)\` if versions match, \`invisible(FALSE)\` otherwise.

------------------------------------------------------------------------

### Method `s1_impute_confounders()`

Step 1: Impute missing confounders by sampling from observed values.

#### Usage

    TTEEnrollment$s1_impute_confounders(confounder_vars, seed = 4L)

#### Arguments

- `confounder_vars`:

  Character vector of confounder column names to impute.

- `seed`:

  Integer seed for reproducibility (default: 4L).

------------------------------------------------------------------------

### Method `s2_ipw()`

Step 2: Calculates inverse probability of treatment weights.

Estimates the propensity score P(A=1 \| L_baseline) via logistic
regression on baseline rows only, then computes stabilized (or
unstabilized) IPW. This addresses \*\*baseline\*\* confounding for the
per-protocol analysis pipeline.

Note: This does NOT estimate time-varying treatment weights for
as-treated analysis (Danaei 2013, Section 4.3). As-treated analysis is
not currently implemented.

Robust standard errors for within-person correlation are handled
downstream by \`survey::svydesign(ids = ~person_id_var)\` in \`\$irr()\`
and \`\$km()\` (Hernan 2008, Danaei 2013).

#### Usage

    TTEEnrollment$s2_ipw(stabilize = TRUE)

#### Arguments

- `stabilize`:

  Logical, default TRUE.

------------------------------------------------------------------------

### Method `s3_truncate_weights()`

Step 3: Truncates extreme weights at specified quantiles.

#### Usage

    TTEEnrollment$s3_truncate_weights(
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

### Method `s4_prepare_for_analysis()`

Step 4: Prepare outcome data and calculate IPCW-PP in one step. Calls
\`\$s5_prepare_outcome()\` followed by \`\$s6_ipcw_pp()\`. This is the
recommended way to prepare an enrollment for analysis.

#### Usage

    TTEEnrollment$s4_prepare_for_analysis(
      outcome,
      follow_up = NULL,
      estimate_ipcw_pp_separately_by_exposure = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      censoring_var = NULL
    )

#### Arguments

- `outcome`:

  Character scalar. Must be one of \`design\$outcome_vars\`.

- `follow_up`:

  Optional integer. Overrides \`design\$follow_up_time\`.

- `estimate_ipcw_pp_separately_by_exposure`:

  Logical, default TRUE.

- `estimate_ipcw_pp_with_gam`:

  Logical, default TRUE.

- `censoring_var`:

  Character or NULL. Defaults to \`"censor_this_period"\`.

------------------------------------------------------------------------

### Method `extract()`

Extract the data.table from the trial object.

#### Usage

    TTEEnrollment$extract()

#### Returns

A data.table with the processed trial data.

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summarize trial data statistics.

#### Usage

    TTEEnrollment$summary(pretty = FALSE)

#### Arguments

- `pretty`:

  Logical, default FALSE. If TRUE, prints formatted output.

#### Returns

If \`pretty = FALSE\`, a list with summary stats. If TRUE, prints
formatted output and invisibly returns the list.

------------------------------------------------------------------------

### Method `weight_summary()`

Print weight distribution diagnostics.

#### Usage

    TTEEnrollment$weight_summary()

------------------------------------------------------------------------

### Method `table1()`

Generate baseline characteristics table.

Returns a long-format \`data.table\` with one row per categorical level
plus one row per continuous variable. See \[.swereg_table1\] for the
layout. The result has S3 class \`c("swereg_table1", "data.table",
"data.frame")\`.

#### Usage

    TTEEnrollment$table1(
      ipw_col = NULL,
      arm_labels = NULL,
      include_smd = TRUE,
      show_missing = c("when_present", "always", "none")
    )

#### Arguments

- `ipw_col`:

  Character or NULL. If specified, the table is weighted by \`ipw_col\`.

- `arm_labels`:

  Optional named character vector \`c(comparator = "...", exposed =
  "...")\` used as column headers in place of the raw exposure values.

- `include_smd`:

  Logical, whether to emit an SMD column (default \`TRUE\`).

- `show_missing`:

  One of \`"when_present"\` (default — emit a Missing row only for
  variables with any missingness), \`"always"\` (emit a Missing row for
  every variable, even when zero), or \`"none"\` (suppress Missing rows
  entirely).

#### Returns

A \`data.table\` with class \`swereg_table1\`.

------------------------------------------------------------------------

### Method `rates()`

Calculate events, person-years, and rates by exposure group.

#### Usage

    TTEEnrollment$rates(weight_col)

#### Arguments

- `weight_col`:

  Character, required. Column name for weights.

#### Returns

A data.table with events, person-years, and rates.

------------------------------------------------------------------------

### Method `irr()`

Fit weighted Poisson regression and extract incidence rate ratios.

Uses \`survey::svyglm()\` with \`quasipoisson\` family and person-level
clustering (\`ids = ~person_id_var\`) for robust standard errors. This
accounts for within-person correlation across repeated trial entries
(Hernan 2008, Danaei 2013).

\*\*IRR vs HR\*\*: For rare events (typical in registry-based TTE
studies), the incidence rate ratio from Poisson regression approximates
the hazard ratio from Cox regression (Thompson 1977). The Poisson model
with \`splines::ns(tstop, df=3)\` flexibly models the baseline event
rate over follow-up time — analogous to Cox's nonparametric baseline
hazard and to Danaei et al.'s "month of follow-up and its squared terms"
in pooled logistic regression.

\*\*Computational choice\*\*: \`quasipoisson\` accounts for
overdispersion from survey weights, and \`svyglm\` scales to large
registry datasets (unlike \`survey::svycoxph()\`). This is
computationally equivalent to the pooled logistic approach used by
Danaei et al. (2013).

\*\*Calendar-time adjustment\*\*: When \`trial_id\` is present in the
data (from band-based enrollment), it is included in the model to adjust
for calendar-time variation in outcome rates across enrollment bands
(Caniglia 2023, Danaei 2013). Uses natural splines for \>=5 unique trial
IDs, linear term for 2-4, omitted for 1.

#### Usage

    TTEEnrollment$irr(weight_col)

#### Arguments

- `weight_col`:

  Character, required. Column name for weights.

#### Returns

A data.table with IRR estimates and confidence intervals.

------------------------------------------------------------------------

### Method `heterogeneity_test()`

Test for heterogeneity of treatment effects across trials.

Fits a model with a \`trial_id x exposure\` interaction term and returns
the Wald test p-value. This tests whether the treatment effect varies
across enrollment bands (Hernan 2008, Danaei 2013).

#### Usage

    TTEEnrollment$heterogeneity_test(weight_col)

#### Arguments

- `weight_col`:

  Character, required. Column name for weights.

#### Returns

A list with \`p_value\` (Wald test), \`n_trials\` (unique trial IDs),
and \`interaction_coefs\` (data.table of interaction coefficients).

------------------------------------------------------------------------

### Method `km()`

Fit Kaplan-Meier curves and optionally plot. Uses IPW only (not IPCW)
because IPCW is time-varying.

#### Usage

    TTEEnrollment$km(ipw_col, save_path = NULL, title = NULL)

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

    TTEEnrollment$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
design <- TTEDesign$new(
  person_id_var = "id",
  exposure_var = "exposed",
  outcome_vars = "death",
  confounder_vars = c("age", "sex"),
  follow_up_time = 52L,
  eligible_var = "eligible"
)

# Enroll via constructor (band-based), then $-chain
enrollment <- TTEEnrollment$new(my_skeleton, design,
  ratio = 2, seed = 4, extra_cols = "isoyearweek"
)
enrollment$
  s2_ipw()$
  s4_prepare_for_analysis(outcome = "death", estimate_ipcw_pp_with_gam = TRUE)
} # }
```
