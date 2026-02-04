# Enroll participants into trials with matching and panel expansion

S7 method that combines sampling (matching unexposed to exposed) and
panel expansion into a single step. This method transitions
\`data_level\` from "person_week" to "trial".

## Usage

``` r
tte_enroll(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object with person_week level data.

- ...:

  Method arguments:

  ratio

  :   Numeric, default 2. Sampling ratio for unexposed:exposed.

  seed

  :   Integer or NULL. Random seed for reproducibility.

  extra_cols

  :   Character vector of additional columns to include beyond design
      variables (e.g., "isoyearweek").

## Value

The modified \[TTETrial\] object with trial level data (for chaining).

## Details

The method performs two operations:

\*\*1. Matching (sampling)\*\* - Samples unexposed individuals at the
specified ratio relative to exposed - Uses \`eligible_var\` from design
to identify eligible entries - Marks sampled rows via \`to_include\`
column

\*\*2. Panel expansion\*\* - For each \`to_include == TRUE\` row,
extracts \`follow_up_time\` rows of follow-up - Creates \`trial_id\` as
\`"\<person_id\>.\<row_id\>"\` - Carries forward the baseline exposure
value - Creates \`trial_week\` (0-indexed time within each trial)

Columns included automatically from design: - \`person_id_var\` (kept in
panel for reference) - \`time_exposure_var\` (e.g., "rd_exposed") -
\`exposure_var\` (baseline, carried forward) - \`confounder_vars\` -
\`outcome_vars\`

If you need to prefix trial_id with file info for batching, do it after
enrollment: “\`r trial \<- tte_trial(data, design) \|\> tte_enroll(ratio
= 2, seed = 42) trial@data\[, trial_id := paste0(file_id, ".",
trial_id)\] “\`

## See also

\[tte_match_ratio()\] for the underlying sampling function,
\[tte_collapse()\] for the next step in the workflow

Other tte_methods:
[`tte_collapse()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse.md),
[`tte_extract()`](https://papadopoulos-lab.github.io/swereg/reference/tte_extract.md),
[`tte_ipcw_pp()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipcw_pp.md),
[`tte_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipw.md),
[`tte_irr()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr.md),
[`tte_km()`](https://papadopoulos-lab.github.io/swereg/reference/tte_km.md),
[`tte_prepare_outcome()`](https://papadopoulos-lab.github.io/swereg/reference/tte_prepare_outcome.md),
[`tte_rates()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates.md),
[`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md),
[`tte_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_summary.md),
[`tte_table1()`](https://papadopoulos-lab.github.io/swereg/reference/tte_table1.md),
[`tte_truncate()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate.md),
[`tte_weight_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weight_summary.md),
[`tte_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
design <- tte_design(
  person_id_var = "id",
  exposure_var = "baseline_exposed",
  time_exposure_var = "rd_exposed",
  outcome_vars = c("death", "hosp"),
  confounder_vars = c("age", "sex"),
  follow_up_time = 156L,
  eligible_var = "eligible"
)

trial <- tte_trial(person_week_data, design) |>
  tte_enroll(ratio = 2, seed = 42, extra_cols = "isoyearweek")
} # }
```
