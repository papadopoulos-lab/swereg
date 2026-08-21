# TTEEnrollment methods that produce an estimate from the weighted panel:
# rates, incidence rate ratios, survival curves and risk differences. Every
# method here is a one-call delegate. The bodies are plain functions in
# R/tte_estimation.R.

#' @include r6_tteenrollment.R
#' @description Calculate events, person-years, and rates by treatment group.
#' @param weight_col Character, required. Column name for weights.
#' @return A data.table with events, person-years, and rates.
TTEEnrollment$set("public", "rates", function(weight_col) {
  .tte_est_rates(self, weight_col)
})

#' @description Fit weighted Poisson regression and extract incidence rate ratios.
#'
#' Uses `survey::svyglm()` with `quasipoisson` family and person-level
#' clustering (`ids = ~person_id_var`) for robust standard errors. This
#' accounts for within-person correlation across repeated trial entries
#' (Hernan 2008, Danaei 2013).
#'
#' **IRR vs HR**: For rare events (typical in registry-based TTE studies),
#' the incidence rate ratio from Poisson regression approximates the hazard
#' ratio from Cox regression (Thompson 1977). The Poisson model with
#' `splines::ns(tstop, df=3)` flexibly models the baseline event rate over
#' follow-up time — analogous to Cox's nonparametric baseline hazard and
#' to Danaei et al.'s "month of follow-up and its squared terms" in pooled
#' logistic regression.
#'
#' **Computational choice**: `quasipoisson` accounts for overdispersion
#' from survey weights, and `svyglm` scales to large registry datasets
#' (unlike `survey::svycoxph()`). This is computationally equivalent to
#' the pooled logistic approach used by Danaei et al. (2013).
#'
#' **Calendar-time adjustment**: When `trial_id` is present in the data
#' (from band-based enrollment), it is included in the model to adjust for
#' calendar-time variation in outcome rates across enrollment bands
#' (Caniglia 2023, Danaei 2013). Uses natural splines for >=5 unique
#' trial IDs, linear term for 2-4, omitted for 1.
#'
#' **Estimand (marginal)**: confounding is removed by the supplied `weights`,
#' not by adjusting for confounders in this model, so the coefficient is a
#' *marginal* (population-average) incidence rate ratio, standardised over
#' the covariate distribution. This contrasts with covariate-adjusted
#' outcome regressions (e.g. `TrialEmulation`'s pooled logistic), which
#' target a *conditional* effect. The two coincide for the (collapsible)
#' rate ratio but differ for the (non-collapsible) odds ratio. See
#' `vignette("tte-methods")`, "Marginal versus conditional estimands".
#'
#' @param weight_col Character, required. Column name for weights.
#' @return A data.table with IRR estimates and confidence intervals.
TTEEnrollment$set("public", "irr", function(weight_col) {
  .tte_est_irr(self, weight_col)
})

#' @description Test for heterogeneity of treatment effects across trials.
#'
#' Fits a model with a `trial_id x treatment` interaction term and returns
#' the Wald test p-value. This tests whether the treatment effect varies
#' across enrollment bands (Hernan 2008, Danaei 2013).
#'
#' @param weight_col Character, required. Column name for weights.
#' @return A list with `p_value` (Wald test), `n_trials` (unique trial IDs),
#'   and `interaction_coefs` (data.table of interaction coefficients).
TTEEnrollment$set("public", "heterogeneity_test", function(weight_col) {
  .tte_est_heterogeneity_test(self, weight_col)
})

#' @description Test whether the treatment effect is modified by a
#' categorical baseline subgroup variable.
#'
#' Fits one combined model with a `treatment x factor(subgroup_var)`
#' interaction and runs a Wald test on the interaction terms. This is the
#' correct test for "do the stratum-specific IRRs differ" -- NOT comparing
#' the per-stratum confidence intervals. For a binary subgroup the single
#' interaction coefficient satisfies `exp(coef) = IRR(other) / IRR(ref)`,
#' where `ref` is the first factor level.
#'
#' The subgroup variable should be a confounder (in the PS / IPCW models)
#' so the marginal weights remain valid within each stratum.
#'
#' @param weight_col Character, required. Column name for weights.
#' @param subgroup_var Character, required. A categorical baseline column.
#' @return A list with `p_value` (Wald test), `subgroup_var`, `n_levels`,
#'   `interaction_coefs` (data.table), and, for a binary subgroup,
#'   `ratio_of_irrs = exp(beta)` with `ratio_lower` / `ratio_upper`
#'   (NA for multi-level subgroups).
TTEEnrollment$set(
  "public",
  "effect_modification_test",
  function(weight_col, subgroup_var) {
    .tte_est_effect_modification_test(self, weight_col, subgroup_var)
  }
)

#' @description Stratified IRRs within each level of a baseline subgroup.
#'
#' Returns one table with an `"all"` row (= `irr()`) plus one row per
#' subgroup level, each fit on that stratum's rows via the shared
#' estimation core. The effect-modification test p-value (and, for a binary
#' subgroup, the ratio of stratum IRRs) is attached as an attribute.
#' Strata with no events or only one treatment arm degrade to NA with a
#' warning; NA-subgroup rows are dropped (count attached as an attribute).
#'
#' @param weight_col Character, required. Column name for weights.
#' @param subgroup_var Character, required. A categorical baseline column.
#' @return A data.table with columns `level, IRR, IRR_lower, IRR_upper,
#'   IRR_pvalue, warn`, with attributes `em_pvalue`, `ratio_of_irrs`, and
#'   `n_na_subgroup`.
TTEEnrollment$set(
  "public",
  "irr_by_subgroup",
  function(weight_col, subgroup_var) {
    .tte_est_irr_by_subgroup(self, weight_col, subgroup_var)
  }
)

#' @description Weighted discrete-time survival curve from the person-week
#' panel. Per treatment arm and reporting time, forms the weighted hazard
#' `h(t) = d(t) / Y(t)`, then `S(t) = prod(1 - h(t))`. The risk set `Y(t)`
#' is `sum(w)` over every row that SPANS `t`, which is
#' `tstart < t <= tstop`. The event count `d(t)` is `sum(w * event)` over
#' the rows that stop at `t`. The weight column `weight_col` may vary over
#' time. Because it works on the
#' full panel (not one row per subject), it accepts time-varying weights:
#' pass a baseline IPW column for the ITT/IPW curve, or a per-protocol weight
#' (e.g. `"analysis_weight_pp_trunc"`) for the PP curve. The weight is applied
#' to each at-risk row exactly as in `$rates()`/`$irr()`, so the curve shares
#' their weighting convention. Deaths are censored, not modelled as a
#' competing risk, so `surv` is cause-specific event-free survival under
#' independent censoring; `1 - surv` is therefore cause-specific failure, NOT
#' a real-world cumulative incidence (which would require a competing-risk
#' estimator). This is a descriptive weighted curve, not the MSM-standardised
#' survival estimator. Returned rows are post-interval survival at each
#' observed `tstop`, one row per arm and time. Where an arm holds nobody at
#' risk, the hazard is `NA` and the survival carries its latest exact value
#' forward.
#' @param weight_col Character, required. Weight column (time-varying allowed).
#' @param save_path Character or NULL. If specified, saves the plot.
#' @param title Character or NULL. Plot title (left-aligned to the whole plot).
#' @param subtitle Character or NULL. Plot subtitle under the title.
#' @param ylim Numeric length-2 or NULL. y-axis zoom (e.g. `c(0.95, 1)`) via
#'   `coord_cartesian`, so steps outside the range are clipped, not dropped.
#'   `NULL` (default) auto-scales -- which for a rare outcome zooms near 100%
#'   and can visually exaggerate small absolute differences; set an explicit,
#'   pre-specified range for publication figures.
#' @param arm_labels Named character/list with `intervention` and
#'   `comparator` (e.g. from `.lookup_arm_labels()`), used for the legend
#'   labels. `NULL` (default) falls back to "Intervention"/"Comparator".
#' @param scale Character, y scale of the saved plot. `"survival"`
#'   (default) plots `surv`, starting at full survival.
#'   `"cumulative_failure"` plots `1 - surv`, starting at 0 --
#'   cause-specific failure, not a competing-risk cumulative incidence
#'   function (see above). Ignored when `save_path` is NULL, since no plot
#'   is built.
#' @return A data.table with columns `treatment_var`, `tstop`, `events`
#'   (weighted), `at_risk` (weighted), `n_persons_at_risk`, `hazard`, `surv`
#'   (invisibly if `save_path` is specified; a `group` column is also added
#'   when plotting).
#'
#'   `at_risk` and `n_persons_at_risk` answer different questions and both
#'   are returned. `at_risk` is the weighted risk set, `sum(w)`, and is the
#'   denominator of the hazard. `n_persons_at_risk` is an unweighted count
#'   of distinct people, taken over `design$person_id_var`, and is the
#'   number a risk table under a survival panel reports. It is not a row
#'   count: the panel holds one row per person-trial-band and a person
#'   contributes several sequential trials, so rows exceed people.
#'   `$rates()` reports the same idea at whole-arm grain under the name
#'   `n_persons`; the two names differ because the grain differs.
TTEEnrollment$set(
  "public",
  "survival_curve",
  function(
    weight_col,
    save_path = NULL,
    title = NULL,
    subtitle = NULL,
    ylim = NULL,
    arm_labels = NULL,
    scale = c("survival", "cumulative_failure")
  ) {
    .tte_est_survival_curve(
      self,
      weight_col,
      save_path,
      title,
      subtitle,
      ylim,
      arm_labels,
      scale
    )
  }
)

#' @description Signed cause-specific risk difference at each band, with a
#' percentile bootstrap interval resampled at the person level.
#'
#' The two arm-specific curves are the ones `$survival_curve()` builds, from
#' the same weighted discrete-time hazard, so the point estimate here and
#' the curve in the figure are the same numbers.
#'
#' The sign convention is fixed:
#'
#' `RD(t) = Risk_intervention(t) - Risk_comparator(t)`, which equals
#' `S_comparator(t) - S_intervention(t)`.
#'
#' The returned `rd` is signed. A protective intervention gives a negative
#' risk difference; that minus sign is the result and is never removed.
#'
#' The bootstrap resamples PERSONS, not person-trials and not rows. A woman
#' contributes several sequential trials that share her baseline covariates
#' and can carry the same outcome event, so her trials are not exchangeable;
#' the person is the cluster. One multiplicity vector is drawn per replicate
#' and applied to both arms, because a woman can be a comparator in an early
#' trial and an initiator in a later one, and a separate draw per arm would
#' discard the covariance between the two arms and bias the interval while
#' leaving the point estimate untouched.
#'
#' A replicate that draws no person for an arm, or that empties a band,
#' yields `NA` for that band and onwards. The percentile step drops those.
#'
#' A zero-event arm gets no interval. When either arm has no
#' positive-weight event through a horizon, `rd_lo` and `rd_hi` are `NA`
#' there and `interval_status` reads `"zero-event arm"`. An ordinary
#' empirical bootstrap cannot produce an event the sample does not hold, so
#' every replicate assigns that arm a failure risk of exactly zero. The
#' percentiles then describe the other arm alone, which is
#' anti-conservative, and more replicates do not repair it. The condition is
#' evaluated per horizon and per arm, on the events up to and including that
#' band.
#'
#' Deaths are censored, not modelled as a competing risk, so this is a
#' cause-specific risk difference under independent censoring, not a
#' competing-risk one.
#' @param weight_col Character, required. Weight column (time-varying
#'   allowed), as in `$survival_curve()`.
#' @param n_boot Integer, number of bootstrap replicates (default 500).
#' @param seed Integer or NULL. When given, the draw is reproducible; the
#'   caller's random stream is restored afterwards.
#' @param conf_level Numeric in (0, 1), percentile interval level
#'   (default 0.95).
#' @return A data.table with one row per band and columns `tstop` (named
#'   after `design$tstop_var`), `surv_comparator`, `surv_intervention`,
#'   `rd`, `rd_lo`, `rd_hi`, `interval_status`, `nnt`, `nnt_direction`,
#'   `n_persons_with_event_comparator` and
#'   `n_persons_with_event_intervention`.
#'
#'   `interval_status` takes one of three values. `"ok"` means the interval
#'   is estimable and strictly excludes the null. `"spans null"` means the
#'   interval is estimable and contains the null. `"zero-event arm"` means
#'   there is no interval. A reader can therefore separate an interval that
#'   spans the null from one that does not exist.
#'
#'   `nnt` is the signed number needed to treat, `-1/rd`. `nnt_direction`
#'   reads `"benefit"`, `"harm"` or `NA_character_`, and it is the stored
#'   decision every formatter reads. No formatter re-derives the direction
#'   from a sign, so a figure and a results sheet cannot disagree about one
#'   band.
#'
#'   The two event columns count distinct PEOPLE who had the outcome at or
#'   before that band, in that arm. They are deliberately not row counts and
#'   not person-trial counts: the panel holds one row per
#'   person-trial-band, and one woman can carry the event in two of her
#'   sequential trials, which is one person who had the outcome. `$rates()`
#'   and `$summary()` report the event ROW count instead, and on real data
#'   the two numbers differ.
#'
#'   The replicate matrix the interval was read off is attached as the
#'   `rd_boot` attribute (`n_boot` rows by one column per band), alongside
#'   `conf_level` and `n_boot`.
TTEEnrollment$set(
  "public",
  "risk_difference",
  function(
    weight_col,
    n_boot = 500L,
    seed = NULL,
    conf_level = 0.95
  ) {
    .tte_est_risk_difference(self, weight_col, n_boot, seed, conf_level)
  }
)
