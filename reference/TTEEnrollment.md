# TTEEnrollment class for target trial emulation

Holds the enrollment data, design specification, and workflow state.
Methods modify in-place and return `invisible(self)` for `$`-chaining.
R6 reference semantics mean `trial$data[, := ...]` modifies the
data.table in-place without copy-on-write overhead.

The `data_level` property controls which methods are available:

- `"person_week"`: Data has one row per person per time unit. Pass
  `ratio` to the constructor to enroll and transition to trial level.

- `"trial"`: Data has been expanded to trial panels (band-level).
  Methods `$s2_ipw()`, `$s4_prepare_for_analysis()`, and
  `$s3_truncate_weights()` require this level.

Enrollment (the comparator draw + panel expansion) transitions data from
"person_week" to "trial" level and is triggered by passing `ratio` to
the constructor.

swereg 26.9.0 moved time zero to the landmark. A `tstart == 0` row of a
schema-2 panel is an entry band row, and a 26.9.0 reader takes it for a
landmark row. The check refuses the object, so that reinterpretation
cannot happen in silence.

## Baseline treatment

The input is a person-week skeleton, so eligibility and treatment status
are assessed weekly. `period_width` collapses consecutive weeks into
bands, and each band opens one trial.

swereg reads only the weeks of a band that are eligible and hold `TRUE`
or `FALSE` in the treatment column. It drops every other week of the
band first, and then applies three rules.

- A person is an initiator when at least one week it reads holds `TRUE`.

- A person is a comparator when every week it reads holds `FALSE`.

- A person-band with no such week is ineligible, and enters neither arm.

The drop comes first, so an `NA` week does not stop a comparator
classification. A band of `FALSE`, `NA`, `FALSE`, `FALSE` is a
comparator band.

Time zero is the landmark, which is the first week of the band AFTER the
entry band. The panel therefore starts one band after the entry band,
and the entry band carries no follow-up. `entry_band_id` names the trial
and `trial_id` names the follow-up band.

Each confounder reaches the panel twice. The `.tte_entry__<v>` column
holds its value at the recruiting week, and `<v>` holds the time-updated
value of the follow-up band. `$s2_ipw()` and `$table1()` read the entry
column. See
[`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md)
for the full rule and
[`vignette("tte-nomenclature")`](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.md)
for the trade-off between bias and statistical power.

## Methods

**Mutating (return `invisible(self)` for chaining, step-numbered for
execution order):**

- `$s1_impute_confounders(confounder_vars, seed)`:

  Step 1: Impute missing confounders

- `$s2_ipw(stabilize)`:

  Step 2: Calculate inverse probability of treatment weights

- `$s3_truncate_weights(weight_cols, lower, upper, suffix)`:

  Step 3: Truncate extreme weights

- `$s4_prepare_for_analysis(outcome, follow_up, ...)`:

  Step 4: Prepare outcome data and calculate IPCW-PP in one step

**Non-mutating (return data):**

- `$extract()`:

  Return the data.table

- `$summary(pretty)`:

  Return summary statistics

- `$weight_summary()`:

  Print weight distribution diagnostics

- `$table1(ipw_col)`:

  Generate baseline characteristics table

- `$rates(weight_col)`:

  Calculate events, person-years, and rates

- `$irr(weight_col)`:

  Fit Poisson models and extract IRR

- `$survival_curve(weight_col, save_path, title)`:

  Weighted discrete-time survival curve from the person-week panel (ITT
  via baseline IPW, or PP via a time-varying `analysis_weight_pp_trunc`)

- `$risk_difference(weight_col, n_boot, seed, conf_level)`:

  Signed cause-specific risk difference per band, with a percentile
  bootstrap interval resampled at the person level

**Active bindings:**

- `$enrollment_stage`:

  Derived lifecycle stage: `"pre_enrollment"`, `"enrolled"`, or
  `"analysis_ready"`

## The interval convention

Every interval is `[tstart, tstop)`. The stop is exclusive. The person
leaves the risk set at `tstop`, and the row holds no part of that week.

Every duration is `tstop - tstart`. It never adds one. Three complete
four-week bands span `[0, 12)`. That is 12 person-weeks, and the bands
bill 4, 4 and 4. The inclusive convention bills 5, 5 and 5.

Every `weeks_to_*` column is a boundary on the same scale, counted from
the landmark at week 0. `weeks_to_event`, `weeks_to_protocol_deviation`,
`weeks_to_loss`, `weeks_to_admin_end` and `weeks_to_record_end` each
name the first week the person no longer contributes. A
`weeks_to_record_end` of 9 means the person held follow-up weeks 1 to 9
and bills 9 person-weeks.

The `+ 1` belongs to the inclusive convention, where weeks 1 through 4
is `4 - 1 + 1 = 4`. Both are correct arithmetic. The two differ in
whether the stop belongs to the interval. A mix of them makes a silently
wrong denominator, so swereg MUST read every stop as exclusive.

One place adds a week, and it converts a calendar reading into a stop.
`admin_censor_isoyearweek` names the last week under study, and
[`difftime()`](https://rdrr.io/r/base/difftime.html) returns the whole
weeks between that week and the landmark week. The stop is one week
later, because the person holds the whole of the administrative week.

`tests/testthat/test-interval-convention.R` pins each of the five
boundaries.

## See also

[TTEDesign](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md)
for design class.
[`vignette("tte-nomenclature")`](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.md)
for the enrollment band vocabulary.

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

- `estimand`:

  Character or NULL. Set to "pp" or "itt" once an analysis dataset is
  prepared; governs which weights are valid in `$irr()`. NULL (legacy /
  unprepared) is treated as per-protocol.

- `landmark_attrition`:

  A data.table or NULL. It reports why landmark qualification dropped
  each candidate person-band, by criterion and by arm. Its columns are
  `trial_id`, `criterion`, `n_persons`, `n_person_trials`,
  `n_intervention` and `n_comparator`. The row with `trial_id = NA`
  covers the whole cohort. The three criteria are `landmark_candidates`,
  `landmark_observed` and `landmark_event_free`, and each count is
  cumulative. It stays `NULL` when the design declares no
  `observed_var`, and when the caller supplies `enrolled_ids` from the
  two-pass pipeline.

## Active bindings

- `enrollment_stage`:

  Derived lifecycle stage (read-only). Returns `"pre_enrollment"` when
  `data_level == "person_week"`, `"analysis_ready"` when
  `s5_prepare_outcome` has been run, or `"enrolled"` otherwise.

## Methods

### Public methods

- [`TTEEnrollment$rates()`](#method-TTEEnrollment-rates)

- [`TTEEnrollment$irr()`](#method-TTEEnrollment-irr)

- [`TTEEnrollment$heterogeneity_test()`](#method-TTEEnrollment-heterogeneity_test)

- [`TTEEnrollment$effect_modification_test()`](#method-TTEEnrollment-effect_modification_test)

- [`TTEEnrollment$irr_by_subgroup()`](#method-TTEEnrollment-irr_by_subgroup)

- [`TTEEnrollment$survival_curve()`](#method-TTEEnrollment-survival_curve)

- [`TTEEnrollment$risk_difference()`](#method-TTEEnrollment-risk_difference)

- [`TTEEnrollment$s1_impute_confounders()`](#method-TTEEnrollment-s1_impute_confounders)

- [`TTEEnrollment$s2_ipw()`](#method-TTEEnrollment-s2_ipw)

- [`TTEEnrollment$s3_truncate_weights()`](#method-TTEEnrollment-s3_truncate_weights)

- [`TTEEnrollment$weight_summary()`](#method-TTEEnrollment-weight_summary)

- [`TTEEnrollment$new()`](#method-TTEEnrollment-initialize)

- [`TTEEnrollment$print()`](#method-TTEEnrollment-print)

- [`TTEEnrollment$check_version()`](#method-TTEEnrollment-check_version)

- [`TTEEnrollment$s4_prepare_for_analysis()`](#method-TTEEnrollment-s4_prepare_for_analysis)

- [`TTEEnrollment$extract()`](#method-TTEEnrollment-extract)

- [`TTEEnrollment$summary()`](#method-TTEEnrollment-summary)

- [`TTEEnrollment$table1()`](#method-TTEEnrollment-table1)

- [`TTEEnrollment$clone()`](#method-TTEEnrollment-clone)

------------------------------------------------------------------------

### `TTEEnrollment$rates()`

Calculate events, person-years, and rates by treatment group.

#### Usage

    TTEEnrollment$rates(weight_col)

#### Arguments

- `weight_col`:

  Character, required. Column name for weights.

#### Returns

A data.table with events, person-years, and rates.

------------------------------------------------------------------------

### `TTEEnrollment$irr()`

Fit weighted Poisson regression and extract incidence rate ratios.

Uses [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)
with `quasipoisson` family and person-level clustering
(`ids = ~person_id_var`) for robust standard errors. This accounts for
within-person correlation across repeated trial entries (Hernan 2008,
Danaei 2013).

**IRR vs HR**: For rare events (typical in registry-based TTE studies),
the incidence rate ratio from Poisson regression approximates the hazard
ratio from Cox regression (Thompson 1977). The Poisson model with
`splines::ns(tstop, df=3)` flexibly models the baseline event rate over
follow-up time — analogous to Cox's nonparametric baseline hazard and to
Danaei et al.'s "month of follow-up and its squared terms" in pooled
logistic regression.

**Computational choice**: `quasipoisson` accounts for overdispersion
from survey weights, and `svyglm` scales to large registry datasets
(unlike
[`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html)).
This is computationally equivalent to the pooled logistic approach used
by Danaei et al. (2013).

**Calendar-time adjustment**: When `trial_id` is present in the data
(from band-based enrollment), it is included in the model to adjust for
calendar-time variation in outcome rates across enrollment bands
(Caniglia 2023, Danaei 2013). Uses natural splines for \>=5 unique trial
IDs, linear term for 2-4, omitted for 1.

**Estimand (marginal)**: confounding is removed by the supplied
`weights`, not by adjusting for confounders in this model, so the
coefficient is a *marginal* (population-average) incidence rate ratio,
standardised over the covariate distribution. This contrasts with
covariate-adjusted outcome regressions (e.g. `TrialEmulation`'s pooled
logistic), which target a *conditional* effect. The two coincide for the
(collapsible) rate ratio but differ for the (non-collapsible) odds
ratio. See
[`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md),
"Marginal versus conditional estimands".

#### Usage

    TTEEnrollment$irr(weight_col)

#### Arguments

- `weight_col`:

  Character, required. Column name for weights.

#### Returns

A data.table with IRR estimates and confidence intervals.

------------------------------------------------------------------------

### `TTEEnrollment$heterogeneity_test()`

Test for heterogeneity of treatment effects across trials.

Fits a model with a `trial_id x treatment` interaction term and returns
the Wald test p-value. This tests whether the treatment effect varies
across enrollment bands (Hernan 2008, Danaei 2013).

#### Usage

    TTEEnrollment$heterogeneity_test(weight_col)

#### Arguments

- `weight_col`:

  Character, required. Column name for weights.

#### Returns

A list with `p_value` (Wald test), `n_trials` (unique trial IDs), and
`interaction_coefs` (data.table of interaction coefficients).

------------------------------------------------------------------------

### `TTEEnrollment$effect_modification_test()`

Test whether the treatment effect is modified by a categorical baseline
subgroup variable.

Fits one combined model with a `treatment x factor(subgroup_var)`
interaction and runs a Wald test on the interaction terms. This is the
correct test for "do the stratum-specific IRRs differ" – NOT comparing
the per-stratum confidence intervals. For a binary subgroup the single
interaction coefficient satisfies `exp(coef) = IRR(other) / IRR(ref)`,
where `ref` is the first factor level.

The subgroup variable should be a confounder (in the PS / IPCW models)
so the marginal weights remain valid within each stratum.

#### Usage

    TTEEnrollment$effect_modification_test(weight_col, subgroup_var)

#### Arguments

- `weight_col`:

  Character, required. Column name for weights.

- `subgroup_var`:

  Character, required. A categorical baseline column.

#### Returns

A list with `p_value` (Wald test), `subgroup_var`, `n_levels`,
`interaction_coefs` (data.table), and, for a binary subgroup,
`ratio_of_irrs = exp(beta)` with `ratio_lower` / `ratio_upper` (NA for
multi-level subgroups).

------------------------------------------------------------------------

### `TTEEnrollment$irr_by_subgroup()`

Stratified IRRs within each level of a baseline subgroup.

Returns one table with an `"all"` row (= `irr()`) plus one row per
subgroup level, each fit on that stratum's rows via the shared
estimation core. The effect-modification test p-value (and, for a binary
subgroup, the ratio of stratum IRRs) is attached as an attribute. Strata
with no events or only one treatment arm degrade to NA with a warning;
NA-subgroup rows are dropped (count attached as an attribute).

#### Usage

    TTEEnrollment$irr_by_subgroup(weight_col, subgroup_var)

#### Arguments

- `weight_col`:

  Character, required. Column name for weights.

- `subgroup_var`:

  Character, required. A categorical baseline column.

#### Returns

A data.table with columns
`level, IRR, IRR_lower, IRR_upper, IRR_pvalue, warn`, with attributes
`em_pvalue`, `ratio_of_irrs`, and `n_na_subgroup`.

------------------------------------------------------------------------

### `TTEEnrollment$survival_curve()`

Weighted discrete-time survival curve from the person-week panel. Per
treatment arm and reporting time, forms the weighted hazard
`h(t) = d(t) / Y(t)`, then `S(t) = prod(1 - h(t))`. The risk set `Y(t)`
is `sum(w)` over every row that SPANS `t`, which is
`tstart < t <= tstop`. The event count `d(t)` is `sum(w * event)` over
the rows that stop at `t`. The weight column `weight_col` may vary over
time. Because it works on the full panel (not one row per subject), it
accepts time-varying weights: pass a baseline IPW column for the ITT/IPW
curve, or a per-protocol weight (e.g. `"analysis_weight_pp_trunc"`) for
the PP curve. The weight is applied to each at-risk row exactly as in
`$rates()`/`$irr()`, so the curve shares their weighting convention.
Deaths are censored, not modelled as a competing risk, so `surv` is
cause-specific event-free survival under independent censoring;
`1 - surv` is therefore cause-specific failure, NOT a real-world
cumulative incidence (which would require a competing-risk estimator).
This is a descriptive weighted curve, not the MSM-standardised survival
estimator. Returned rows are post-interval survival at each observed
`tstop`, one row per arm and time. Where an arm holds nobody at risk,
the hazard is `NA` and the survival carries its latest exact value
forward.

#### Usage

    TTEEnrollment$survival_curve(
      weight_col,
      save_path = NULL,
      title = NULL,
      subtitle = NULL,
      ylim = NULL,
      arm_labels = NULL,
      scale = c("survival", "cumulative_failure")
    )

#### Arguments

- `weight_col`:

  Character, required. Weight column (time-varying allowed).

- `save_path`:

  Character or NULL. If specified, saves the plot.

- `title`:

  Character or NULL. Plot title (left-aligned to the whole plot).

- `subtitle`:

  Character or NULL. Plot subtitle under the title.

- `ylim`:

  Numeric length-2 or NULL. y-axis zoom (e.g. `c(0.95, 1)`) via
  `coord_cartesian`, so steps outside the range are clipped, not
  dropped. `NULL` (default) auto-scales – which for a rare outcome zooms
  near 100% and can visually exaggerate small absolute differences; set
  an explicit, pre-specified range for publication figures.

- `arm_labels`:

  Named character/list with `intervention` and `comparator` (e.g. from
  `.lookup_arm_labels()`), used for the legend labels. `NULL` (default)
  falls back to "Intervention"/"Comparator".

- `scale`:

  Character, y scale of the saved plot. `"survival"` (default) plots
  `surv`, starting at full survival. `"cumulative_failure"` plots
  `1 - surv`, starting at 0 – cause-specific failure, not a
  competing-risk cumulative incidence function (see above). Ignored when
  `save_path` is NULL, since no plot is built.

#### Returns

A data.table with columns `treatment_var`, `tstop`, `events` (weighted),
`at_risk` (weighted), `n_persons_at_risk`, `hazard`, `surv` (invisibly
if `save_path` is specified; a `group` column is also added when
plotting).

`at_risk` and `n_persons_at_risk` answer different questions and both
are returned. `at_risk` is the weighted risk set, `sum(w)`, and is the
denominator of the hazard. `n_persons_at_risk` is an unweighted count of
distinct people, taken over `design$person_id_var`, and is the number a
risk table under a survival panel reports. It is not a row count: the
panel holds one row per person-trial-band and a person contributes
several sequential trials, so rows exceed people. `$rates()` reports the
same idea at whole-arm grain under the name `n_persons`; the two names
differ because the grain differs.

------------------------------------------------------------------------

### `TTEEnrollment$risk_difference()`

Signed cause-specific risk difference at each band, with a percentile
bootstrap interval resampled at the person level.

The two arm-specific curves are the ones `$survival_curve()` builds,
from the same weighted discrete-time hazard, so the point estimate here
and the curve in the figure are the same numbers.

The sign convention is fixed:

`RD(t) = Risk_intervention(t) - Risk_comparator(t)`, which equals
`S_comparator(t) - S_intervention(t)`.

The returned `rd` is signed. A protective intervention gives a negative
risk difference; that minus sign is the result and is never removed.

The bootstrap resamples PERSONS, not person-trials and not rows. A woman
contributes several sequential trials that share her baseline covariates
and can carry the same outcome event, so her trials are not
exchangeable; the person is the cluster. One multiplicity vector is
drawn per replicate and applied to both arms, because a woman can be a
comparator in an early trial and an initiator in a later one, and a
separate draw per arm would discard the covariance between the two arms
and bias the interval while leaving the point estimate untouched.

A replicate that draws no person for an arm, or that empties a band,
yields `NA` for that band and onwards. The percentile step drops those.

A zero-event arm gets no interval. When either arm has no
positive-weight event through a horizon, `rd_lo` and `rd_hi` are `NA`
there and `interval_status` reads `"zero-event arm"`. An ordinary
empirical bootstrap cannot produce an event the sample does not hold, so
every replicate assigns that arm a failure risk of exactly zero. The
percentiles then describe the other arm alone, which is
anti-conservative, and more replicates do not repair it. The condition
is evaluated per horizon and per arm, on the events up to and including
that band.

Deaths are censored, not modelled as a competing risk, so this is a
cause-specific risk difference under independent censoring, not a
competing-risk one.

#### Usage

    TTEEnrollment$risk_difference(
      weight_col,
      n_boot = 500L,
      seed = NULL,
      conf_level = 0.95
    )

#### Arguments

- `weight_col`:

  Character, required. Weight column (time-varying allowed), as in
  `$survival_curve()`.

- `n_boot`:

  Integer, number of bootstrap replicates (default 500).

- `seed`:

  Integer or NULL. When given, the draw is reproducible; the caller's
  random stream is restored afterwards.

- `conf_level`:

  Numeric in (0, 1), percentile interval level (default 0.95).

#### Returns

A data.table with one row per band and columns `tstop` (named after
`design$tstop_var`), `surv_comparator`, `surv_intervention`, `rd`,
`rd_lo`, `rd_hi`, `interval_status`, `nnt`, `nnt_direction`,
`n_persons_with_event_comparator` and
`n_persons_with_event_intervention`.

`interval_status` takes one of three values. `"ok"` means the interval
is estimable and strictly excludes the null. `"spans null"` means the
interval is estimable and contains the null. `"zero-event arm"` means
there is no interval. A reader can therefore separate an interval that
spans the null from one that does not exist.

`nnt` is the signed number needed to treat, `-1/rd`. `nnt_direction`
reads `"benefit"`, `"harm"` or `NA_character_`, and it is the stored
decision every formatter reads. No formatter re-derives the direction
from a sign, so a figure and a results sheet cannot disagree about one
band.

The two event columns count distinct PEOPLE who had the outcome at or
before that band, in that arm. They are deliberately not row counts and
not person-trial counts: the panel holds one row per person-trial-band,
and one woman can carry the event in two of her sequential trials, which
is one person who had the outcome. `$rates()` and `$summary()` report
the event ROW count instead, and on real data the two numbers differ.

The replicate matrix the interval was read off is attached as the
`rd_boot` attribute (`n_boot` rows by one column per band), alongside
`conf_level` and `n_boot`.

------------------------------------------------------------------------

### `TTEEnrollment$s1_impute_confounders()`

Step 1: Impute missing confounders by sampling from observed values.

#### Usage

    TTEEnrollment$s1_impute_confounders(confounder_vars, seed = 4L)

#### Arguments

- `confounder_vars`:

  Character vector of confounder column names to impute.

- `seed`:

  Integer seed for reproducibility (default: 4L).

------------------------------------------------------------------------

### `TTEEnrollment$s2_ipw()`

Step 2: Calculates inverse probability of treatment weights.

Estimates the propensity score P(A=1 \| L_baseline) via logistic
regression on baseline rows only, then computes stabilized (or
unstabilized) IPW. This addresses **baseline** confounding for the
per-protocol analysis pipeline.

Note: This does NOT estimate time-varying treatment weights for
as-treated analysis (Danaei 2013, Section 4.3). As-treated analysis is
not currently implemented.

Robust standard errors for within-person correlation are handled
downstream by `survey::svydesign(ids = ~person_id_var)` in `$irr()`
(Hernan 2008, Danaei 2013).

#### Usage

    TTEEnrollment$s2_ipw(stabilize = TRUE)

#### Arguments

- `stabilize`:

  Logical, default TRUE.

------------------------------------------------------------------------

### `TTEEnrollment$s3_truncate_weights()`

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

### `TTEEnrollment$weight_summary()`

Print weight distribution diagnostics.

#### Usage

    TTEEnrollment$weight_summary()

------------------------------------------------------------------------

### `TTEEnrollment$new()`

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

  A
  [TTEDesign](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md)
  object specifying column mappings.

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
  person_week data. The Baseline treatment section of TTEEnrollment
  states the rule that decides the arm of each person-band.

- `seed`:

  Integer or NULL. Random seed for enrollment reproducibility.

- `extra_cols`:

  Character vector or NULL. Extra columns to include in trial panels
  during enrollment.

- `enrolled_ids`:

  data.table or NULL. Pre-drawn enrollment IDs from the two-pass
  pipeline. When provided, enrollment skips the comparator draw and uses
  these IDs directly.

- `own_data`:

  Logical. If TRUE, takes ownership of the data.table without copying
  it. Use only when the caller will not reuse the data.

------------------------------------------------------------------------

### `TTEEnrollment$print()`

Print the TTEEnrollment object.

#### Usage

    TTEEnrollment$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `TTEEnrollment$check_version()`

Check this object's schema version against the current class version. It
stops when the object carries an older schema.

#### Usage

    TTEEnrollment$check_version()

#### Returns

`invisible(TRUE)` when the versions match. It stops otherwise.

------------------------------------------------------------------------

### `TTEEnrollment$s4_prepare_for_analysis()`

Step 4: Prepare the outcome/analysis dataset for one estimand. For
`estimand = "pp"` (default) this calls `$s5_prepare_outcome()` then
`$s6_ipcw_pp()`. For `estimand = "itt"` it calls `$s5_prepare_outcome()`
in ITT mode, which never censors at treatment switching. ITT skips IPCW,
because baseline IPW alone is the valid ITT weight. This is the
recommended way to prepare an enrollment for analysis.

The censoring row stays in `self$data`, and it carries only the exposure
before its boundary. `s5_prepare_outcome()` clips that row at the exact
censoring week, and sets `person_weeks` to the clipped width. The
deviated regime therefore contributes no person-time and no outcome, so
the row cannot attribute a post-deviation outcome to the baseline
treatment. Releases before 26.9.0 deleted the row instead, which threw
away every valid week it held.

Event-priority convention: an outcome event that stops in the deviation
band wins. The row then counts as an event and not as a censoring. The
deviation does not clip it, `censor_this_period` is 0, and the censoring
model does not treat it as censored (since 26.7.3). The row still stops
at the exact event week, which can fall inside the band.

#### Usage

    TTEEnrollment$s4_prepare_for_analysis(
      outcome,
      follow_up = NULL,
      estimand = c("pp", "itt"),
      estimate_ipcw_pp_separately_by_treatment = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      censoring_var = NULL
    )

#### Arguments

- `outcome`:

  Character scalar. Must be one of `design$outcome_vars`.

- `follow_up`:

  Optional integer. Overrides `design$follow_up_time`.

- `estimand`:

  Character, `"pp"` (per-protocol, default) or `"itt"`
  (intention-to-treat). ITT keeps follow-up through treatment switching
  and uses baseline IPW only (no IPCW); analyse it with
  `$irr(weight_col = "ipw_trunc")`.

- `estimate_ipcw_pp_separately_by_treatment`:

  Logical, default TRUE.

- `estimate_ipcw_pp_with_gam`:

  Logical, default TRUE.

- `censoring_var`:

  Character or NULL. Defaults to `"censor_this_period"`.

------------------------------------------------------------------------

### `TTEEnrollment$extract()`

Extract the data.table from the trial object.

#### Usage

    TTEEnrollment$extract()

#### Returns

A data.table with the processed trial data.

------------------------------------------------------------------------

### `TTEEnrollment$summary()`

Summarize trial data statistics.

#### Usage

    TTEEnrollment$summary(pretty = FALSE)

#### Arguments

- `pretty`:

  Logical, default FALSE. If TRUE, prints formatted output.

#### Returns

If `pretty = FALSE`, a list with summary stats. If TRUE, prints
formatted output and invisibly returns the list.

------------------------------------------------------------------------

### `TTEEnrollment$table1()`

Generate baseline characteristics table.

Returns a long-format `data.table` with one row per categorical level
plus one row per continuous variable. See `.swereg_table1()` for the
layout. The result has S3 class
`c("swereg_table1", "data.table", "data.frame")`.

#### Usage

    TTEEnrollment$table1(
      ipw_col = NULL,
      arm_labels = NULL,
      include_smd = TRUE,
      show_missing = c("when_present", "always", "none")
    )

#### Arguments

- `ipw_col`:

  Character or NULL. If specified, the table is weighted by `ipw_col`.

- `arm_labels`:

  Optional named character vector
  `c(comparator = "...", intervention = "...")` used as column headers
  in place of the raw treatment values.

- `include_smd`:

  Logical, whether to emit an SMD column (default `TRUE`).

- `show_missing`:

  One of `"when_present"` (default — emit a Missing row only for
  variables with any missingness), `"always"` (emit a Missing row for
  every variable, even when zero), or `"none"` (suppress Missing rows
  entirely).

#### Returns

A `data.table` with class `swereg_table1`.

------------------------------------------------------------------------

### `TTEEnrollment$clone()`

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
  treatment_var = "intervention",
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
