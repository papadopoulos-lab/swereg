# TTEEnrollment methods that produce an estimate from the weighted panel:
# rates, incidence rate ratios, survival curves and risk differences.

#' @include r6_tteenrollment.R
#' @description Calculate events, person-years, and rates by treatment group.
#' @param weight_col Character, required. Column name for weights.
#' @return A data.table with events, person-years, and rates.
TTEEnrollment$set("public", "rates", function(weight_col) {
  if (self$data_level != "trial") {
    stop(
      "rates() requires trial level data.\n",
      "Current data_level: '",
      self$data_level,
      "'"
    )
  }

  design <- self$design
  data <- self$data

  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data")
  }
  if (!"event" %in% names(data)) {
    stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
  }
  if (!"person_weeks" %in% names(data)) {
    stop(
      "'person_weeks' column not found. Enrollment should create this automatically."
    )
  }

  # Sequential TTE inflates person-trial counts relative to unique
  # participants (one person contributes to many weekly trials). Surface
  # `n_persons` alongside `n_trials` so readers and downstream tables see
  # both the analytic denominator and the underlying sample size.
  result <- data[,
    .(
      n_persons = data.table::uniqueN(get(design$person_id_var)),
      n_trials = data.table::uniqueN(get(design$id_var)),
      events_weighted = sum(event * get(weight_col)),
      py_weighted = sum(person_weeks * get(weight_col)) / 52.25,
      rate_per_100000py = sum(event * get(weight_col)) /
        (sum(person_weeks * get(weight_col)) / 52.25) *
        100000
    ),
    by = c(design$treatment_var)
  ]
  data.table::setattr(result, "swereg_type", "rates")
  data.table::setattr(result, "treatment_var", design$treatment_var)
  result
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
  if (self$data_level != "trial") {
    stop(
      "irr() requires trial level data.\n",
      "Current data_level: '",
      self$data_level,
      "'"
    )
  }

  design <- self$design
  data <- self$data

  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data")
  }
  if (!"event" %in% names(data)) {
    stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
  }
  if (!"person_weeks" %in% names(data)) {
    stop(
      "'person_weeks' column not found. Enrollment should create this automatically."
    )
  }

  # Guard: a per-protocol dataset has been censored at protocol deviation,
  # so IPW-only weights (without IPCW) would produce biased ITT-like
  # estimates on it. This does NOT apply to an ITT dataset, which is never
  # censored at switching and for which baseline IPW is the valid weight.
  ipw_only_cols <- c("ipw", "ipw_trunc")
  if (
    weight_col %in%
      ipw_only_cols &&
      "prepare_outcome" %in% self$steps_completed &&
      !identical(self$estimand, "itt")
  ) {
    stop(
      "Cannot use '",
      weight_col,
      "' as weight_col after per-protocol censoring.\n",
      "The dataset has been censored at protocol deviation via $s4_prepare_for_analysis(),\n",
      "so only per-protocol weights (e.g., 'analysis_weight_pp_trunc') are valid.\n",
      "Using IPW-only weights on per-protocol censored data produces biased estimates.\n",
      "For an intention-to-treat analysis, prepare with estimand = \"itt\"."
    )
  }

  private$.fit_irr(data, weight_col)
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
  if (self$data_level != "trial") {
    stop("heterogeneity_test() requires trial level data.")
  }

  design <- self$design
  data <- self$data

  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data")
  }
  if (!"event" %in% names(data)) {
    stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
  }
  if (!"trial_id" %in% names(data)) {
    stop(
      "'trial_id' column not found. Heterogeneity test requires multiple trials."
    )
  }

  n_trials <- data[, data.table::uniqueN(trial_id)]
  if (n_trials < 2L) {
    stop("Need at least 2 unique trial_ids for heterogeneity test.")
  }

  keep_cols <- unique(c(
    design$person_id_var,
    design$treatment_var,
    design$tstop_var,
    weight_col,
    "event",
    "person_weeks",
    "trial_id"
  ))
  svy_data <- data[, ..keep_cols]

  svy_design <- survey::svydesign(
    ids = as.formula(paste0("~", design$person_id_var)),
    weights = as.formula(paste0("~", weight_col)),
    data = svy_data
  )
  rm(svy_data)

  # Spline interaction: does the treatment effect vary smoothly over
  # calendar time (trial period)? Uses ns(trial_id, df=3) interacted
  # with treatment — 3 interaction terms instead of one per trial period.
  spline_df <- min(3L, n_trials - 1L)
  formula_int <- stats::as.formula(paste0(
    "event ~ ",
    design$treatment_var,
    " * splines::ns(trial_id, df = ",
    spline_df,
    ")",
    " + splines::ns(",
    design$tstop_var,
    ", df = 3)",
    " + offset(log(person_weeks))"
  ))

  fit <- survey::svyglm(
    formula_int,
    design = svy_design,
    family = stats::quasipoisson()
  )
  rm(svy_design)

  # Extract interaction coefficients (treatment:ns(trial_id) terms)
  coef_names <- names(stats::coef(fit))
  interaction_idx <- grep(
    paste0("^", design$treatment_var, "TRUE:"),
    coef_names
  )

  if (length(interaction_idx) == 0) {
    return(list(
      p_value = NA_real_,
      n_trials = n_trials,
      interaction_coefs = data.table::data.table()
    ))
  }

  # Wald test for joint significance of interaction terms
  vcov_mat <- stats::vcov(fit)
  beta_int <- stats::coef(fit)[interaction_idx]
  vcov_int <- vcov_mat[interaction_idx, interaction_idx, drop = FALSE]
  # Guard against non-estimable interactions (NA coefficients or a
  # singular covariance from sparse / separated subgroup cells): drop
  # non-finite terms and return NA rather than crashing on solve().
  finite <- is.finite(beta_int) & is.finite(diag(vcov_int))
  p_value <- if (!any(finite)) {
    NA_real_
  } else {
    beta_f <- beta_int[finite]
    vcov_f <- vcov_int[finite, finite, drop = FALSE]
    wald_stat <- tryCatch(
      as.numeric(t(beta_f) %*% solve(vcov_f) %*% beta_f),
      error = function(e) NA_real_
    )
    if (is.na(wald_stat)) {
      NA_real_
    } else {
      stats::pchisq(wald_stat, df = length(beta_f), lower.tail = FALSE)
    }
  }

  fit_summary <- summary(fit)$coefficients
  interaction_coefs <- data.table::data.table(
    term = coef_names[interaction_idx],
    estimate = fit_summary[interaction_idx, "Estimate"],
    se = fit_summary[interaction_idx, "Std. Error"],
    p = fit_summary[interaction_idx, "Pr(>|t|)"]
  )
  rm(fit)

  list(
    p_value = p_value,
    n_trials = n_trials,
    interaction_coefs = interaction_coefs
  )
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
    if (self$data_level != "trial") {
      stop("effect_modification_test() requires trial level data.")
    }
    design <- self$design
    data <- self$data
    if (!weight_col %in% names(data)) {
      stop("weight_col '", weight_col, "' not found in data")
    }
    if (!subgroup_var %in% names(data)) {
      stop("subgroup_var '", subgroup_var, "' not found in data")
    }
    if (!"event" %in% names(data)) {
      stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
    }
    ipw_only_cols <- c("ipw", "ipw_trunc")
    if (
      weight_col %in%
        ipw_only_cols &&
        "prepare_outcome" %in% self$steps_completed &&
        !identical(self$estimand, "itt")
    ) {
      stop(
        "Cannot use '",
        weight_col,
        "' as weight_col after per-protocol censoring.\n",
        "Use a per-protocol weight (e.g. 'analysis_weight_pp_trunc'), or ",
        "prepare with estimand = \"itt\"."
      )
    }

    d <- data[!is.na(get(subgroup_var))]
    sg_levels <- sort(unique(d[[subgroup_var]]))
    n_levels <- length(sg_levels)
    if (n_levels < 2L) {
      stop(
        "subgroup_var '",
        subgroup_var,
        "' must have >= 2 non-NA levels for an effect-modification test."
      )
    }

    has_trial_id <- "trial_id" %in%
      names(d) &&
      d[, data.table::uniqueN(trial_id)] > 1L
    n_trial_ids <- if (has_trial_id) {
      d[, data.table::uniqueN(trial_id)]
    } else {
      0L
    }
    trial_term <- if (has_trial_id && n_trial_ids >= 5L) {
      " + splines::ns(trial_id, df = 3)"
    } else if (has_trial_id) {
      " + trial_id"
    } else {
      ""
    }

    keep_cols <- unique(c(
      design$person_id_var,
      design$treatment_var,
      design$tstop_var,
      weight_col,
      "event",
      "person_weeks",
      subgroup_var,
      if (has_trial_id) "trial_id"
    ))
    svy_data <- d[, ..keep_cols]
    svy_data[[subgroup_var]] <- factor(svy_data[[subgroup_var]])

    svy_design <- survey::svydesign(
      ids = as.formula(paste0("~", design$person_id_var)),
      weights = as.formula(paste0("~", weight_col)),
      data = svy_data
    )
    rm(svy_data)

    formula_int <- stats::as.formula(paste0(
      "event ~ ",
      design$treatment_var,
      " * factor(",
      subgroup_var,
      ")",
      " + splines::ns(",
      design$tstop_var,
      ", df = 3)",
      trial_term,
      " + offset(log(person_weeks))"
    ))

    fit <- survey::svyglm(
      formula_int,
      design = svy_design,
      family = stats::quasipoisson()
    )
    rm(svy_design)

    coef_names <- names(stats::coef(fit))
    interaction_idx <- grep(
      paste0("^", design$treatment_var, "TRUE:"),
      coef_names
    )

    if (length(interaction_idx) == 0) {
      return(list(
        p_value = NA_real_,
        subgroup_var = subgroup_var,
        n_levels = n_levels,
        interaction_coefs = data.table::data.table(),
        ratio_of_irrs = NA_real_,
        ratio_lower = NA_real_,
        ratio_upper = NA_real_
      ))
    }

    vcov_mat <- stats::vcov(fit)
    beta_int <- stats::coef(fit)[interaction_idx]
    vcov_int <- vcov_mat[interaction_idx, interaction_idx, drop = FALSE]
    # Guard against non-estimable interactions (NA coefficients or a
    # singular covariance from sparse / separated subgroup cells): drop
    # non-finite terms and return NA rather than crashing on solve().
    finite <- is.finite(beta_int) & is.finite(diag(vcov_int))
    p_value <- if (!any(finite)) {
      NA_real_
    } else {
      beta_f <- beta_int[finite]
      vcov_f <- vcov_int[finite, finite, drop = FALSE]
      wald_stat <- tryCatch(
        as.numeric(t(beta_f) %*% solve(vcov_f) %*% beta_f),
        error = function(e) NA_real_
      )
      if (is.na(wald_stat)) {
        NA_real_
      } else {
        stats::pchisq(wald_stat, df = length(beta_f), lower.tail = FALSE)
      }
    }

    fit_summary <- summary(fit)$coefficients
    interaction_coefs <- data.table::data.table(
      term = coef_names[interaction_idx],
      estimate = fit_summary[interaction_idx, "Estimate"],
      se = fit_summary[interaction_idx, "Std. Error"],
      p = fit_summary[interaction_idx, "Pr(>|t|)"]
    )

    # Binary subgroup: one interaction term -> ratio of stratum IRRs.
    if (n_levels == 2L && length(interaction_idx) == 1L) {
      b <- fit_summary[interaction_idx, "Estimate"]
      s <- fit_summary[interaction_idx, "Std. Error"]
      ratio <- exp(b)
      ratio_lower <- exp(b - 1.96 * s)
      ratio_upper <- exp(b + 1.96 * s)
    } else {
      ratio <- NA_real_
      ratio_lower <- NA_real_
      ratio_upper <- NA_real_
    }
    rm(fit)

    list(
      p_value = p_value,
      subgroup_var = subgroup_var,
      n_levels = n_levels,
      interaction_coefs = interaction_coefs,
      ratio_of_irrs = ratio,
      ratio_lower = ratio_lower,
      ratio_upper = ratio_upper
    )
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
    if (self$data_level != "trial") {
      stop("irr_by_subgroup() requires trial level data.")
    }
    design <- self$design
    data <- self$data
    if (!weight_col %in% names(data)) {
      stop("weight_col '", weight_col, "' not found in data")
    }
    if (!subgroup_var %in% names(data)) {
      stop("subgroup_var '", subgroup_var, "' not found in data")
    }
    if (!"event" %in% names(data)) {
      stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
    }
    ipw_only_cols <- c("ipw", "ipw_trunc")
    if (
      weight_col %in%
        ipw_only_cols &&
        "prepare_outcome" %in% self$steps_completed &&
        !identical(self$estimand, "itt")
    ) {
      stop(
        "Cannot use '",
        weight_col,
        "' as weight_col after per-protocol censoring.\n",
        "Use a per-protocol weight (e.g. 'analysis_weight_pp_trunc'), or ",
        "prepare with estimand = \"itt\"."
      )
    }

    treatment_var <- design$treatment_var
    n_na <- data[is.na(get(subgroup_var)), .N]
    d <- data[!is.na(get(subgroup_var))]
    sg_levels <- sort(unique(d[[subgroup_var]]))
    if (length(sg_levels) < 2L) {
      stop(
        "subgroup_var '",
        subgroup_var,
        "' must have >= 2 non-NA levels."
      )
    }

    na_row <- function(level_label) {
      data.table::data.table(
        level = level_label,
        IRR = NA_real_,
        IRR_lower = NA_real_,
        IRR_upper = NA_real_,
        IRR_pvalue = NA_real_,
        warn = TRUE
      )
    }
    fit_one <- function(subset, level_label) {
      # Need both treatment arms AND >= 1 event in EACH arm. Zero events in
      # one arm causes separation (infinite IRR), not a clean error, so
      # preflight it rather than relying on the fit to fail.
      ev_by_arm <- subset[,
        sum(event, na.rm = TRUE),
        by = c(treatment_var)
      ]
      if (nrow(ev_by_arm) < 2L || any(ev_by_arm$V1 == 0L)) {
        warning(
          "irr_by_subgroup: stratum '",
          level_label,
          "' has no events in one or both treatment arms; returning NA."
        )
        return(na_row(level_label))
      }
      r <- tryCatch(
        private$.fit_irr(subset, weight_col),
        error = function(e) {
          warning(
            "irr_by_subgroup: fit failed for stratum '",
            level_label,
            "': ",
            conditionMessage(e)
          )
          NULL
        }
      )
      if (is.null(r)) {
        return(na_row(level_label))
      }
      data.table::data.table(
        level = level_label,
        IRR = r$IRR,
        IRR_lower = r$IRR_lower,
        IRR_upper = r$IRR_upper,
        IRR_pvalue = r$IRR_pvalue,
        warn = r$warn
      )
    }

    rows <- list(fit_one(data, "all"))
    for (lv in sg_levels) {
      rows[[length(rows) + 1L]] <- fit_one(
        d[get(subgroup_var) == lv],
        as.character(lv)
      )
    }
    out <- data.table::rbindlist(rows)

    emt <- tryCatch(
      self$effect_modification_test(weight_col, subgroup_var),
      error = function(e) NULL
    )
    data.table::setattr(
      out,
      "em_pvalue",
      if (is.null(emt)) NA_real_ else emt$p_value
    )
    data.table::setattr(
      out,
      "ratio_of_irrs",
      if (is.null(emt)) NA_real_ else emt$ratio_of_irrs
    )
    data.table::setattr(out, "n_na_subgroup", n_na)
    data.table::setattr(out, "swereg_type", "irr_by_subgroup")
    out
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
    if (self$data_level != "trial") {
      stop(
        "survival_curve() requires trial level data.\n",
        "Current data_level: '",
        self$data_level,
        "'"
      )
    }
    scale <- match.arg(scale)

    design <- self$design
    data <- self$data

    if (!weight_col %in% names(data)) {
      stop("weight_col '", weight_col, "' not found in data")
    }
    if (!"event" %in% names(data)) {
      stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
    }

    tvar <- design$treatment_var
    time_var <- design$tstop_var

    # Validate analytic inputs loudly: a single NA weight/event otherwise
    # silently poisons every downstream survival value via cumprod().
    w <- data[[weight_col]]
    if (!is.numeric(w) || anyNA(w) || any(!is.finite(w)) || any(w < 0)) {
      stop(
        "weight_col '",
        weight_col,
        "' must be numeric, finite, non-missing and non-negative"
      )
    }
    if (anyNA(data$event) || !all(data$event %in% c(0L, 1L))) {
      stop("'event' must be a non-missing 0/1 indicator")
    }

    # Weighted discrete-time hazard per arm and reporting time. The weight is
    # applied to each at-risk row exactly as in $rates()/$irr(), so the curve
    # and the reported IRR share one weighting convention.
    #
    # `.tte_span_risk_sets()` owns the two rules: the risk set holds every
    # row that SPANS the time, and the event LANDS at the stop of its own
    # row. The risk set stays a weighted COUNT of the person-trials at risk.
    # It is not a sum of person-time; `$rates()` owns that quantity.
    #
    # `n_persons_at_risk` is a plain head count of distinct people, for the
    # risk table a reader expects under a survival panel. It is deliberately
    # NOT `.N`: the panel is one row per person-trial-band, and a person
    # contributes several sequential trials, so `.N` counts person-trials.
    # It is also not `at_risk`, which is the weighted risk set.
    times <- sort(unique(data[[time_var]]))
    curve <- .tte_span_risk_sets(
      arm = data[[tvar]],
      person = data[[design$person_id_var]],
      weight = data[[weight_col]],
      event = data[["event"]],
      tstart = .tte_interval_start(
        data,
        design$tstart_var,
        time_var,
        times
      ),
      tstop = data[[time_var]],
      times = times
    )
    data.table::setnames(curve, c("arm", "time"), c(tvar, time_var))
    data.table::setkeyv(curve, c(tvar, time_var))
    # An empty risk set is legitimate once an arm runs out of follow-up. A
    # positive head count with no weight behind it is not, and neither is an
    # event at a time no row covers.
    if (any(curve$at_risk <= 0 & curve$n_persons_at_risk > 0L)) {
      stop("weighted risk set (sum of weights) is <= 0 in an arm-period")
    }
    if (any(curve$at_risk <= 0 & curve$events > 0)) {
      stop("an event falls at a time whose risk set is empty")
    }
    curve[, hazard := events / at_risk]
    # Nobody at risk: the hazard is undefined and reads NA, and the survival
    # carries its latest exact value forward. `cumprod` is valid over these
    # exact event boundaries, and a band hazard over unequal intervals is
    # not, so a time between two boundaries multiplies by exactly 1.
    curve[at_risk <= 0, hazard := NA_real_]
    curve[,
      surv := cumprod(1 - data.table::fifelse(is.na(hazard), 0, hazard)),
      by = c(tvar)
    ]

    if (is.null(save_path)) {
      return(curve[])
    }

    tv <- curve[[tvar]]
    if (!is.logical(tv) && !all(tv %in% c(0L, 1L))) {
      stop(
        "plotting requires a logical (or 0/1) '",
        tvar,
        "'; got class '",
        class(tv)[1],
        "'"
      )
    }
    # The study's own arm labels when supplied, else generic ones;
    # intervention is red, comparator blue, intervention first.
    labs <- .tte_arm_labels_resolved(arm_labels)
    int_lab <- labs[["intervention"]]
    cmp_lab <- labs[["comparator"]]
    curve[, group := fifelse(as.logical(get(tvar)), int_lab, cmp_lab)]

    q <- .render_survival_curve(
      curve = curve,
      time_var = time_var,
      scale = scale,
      title = title,
      subtitle = subtitle,
      ylim = ylim,
      int_lab = int_lab,
      cmp_lab = cmp_lab
    )

    ggplot2::ggsave(save_path, q, width = 8, height = 6, dpi = 300)
    invisible(curve[])
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
    if (self$data_level != "trial") {
      stop(
        "risk_difference() requires trial level data.\n",
        "Current data_level: '",
        self$data_level,
        "'"
      )
    }
    design <- self$design

    if (!is.null(seed)) {
      has_old <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
      old_seed <- if (has_old) {
        get(".Random.seed", envir = globalenv(), inherits = FALSE)
      } else {
        NULL
      }
      on.exit(
        {
          if (is.null(old_seed)) {
            if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
              rm(".Random.seed", envir = globalenv())
            }
          } else {
            assign(".Random.seed", old_seed, envir = globalenv())
          }
        },
        add = TRUE
      )
      set.seed(seed)
    }

    .tte_rd_curve(
      data = self$data,
      person_id_var = design$person_id_var,
      id_var = design$id_var,
      treatment_var = design$treatment_var,
      time_var = design$tstop_var,
      weight_col = weight_col,
      n_boot = n_boot,
      conf_level = conf_level,
      tstart_var = design$tstart_var
    )
  }
)

# --- .fit_irr: weighted Poisson MSM fit for one data subset -------------
# The estimation core shared by irr() and irr_by_subgroup(). The caller is
# responsible for the guards (weight validity, required columns); this fits
# the model on whatever `data` it is handed and returns the one-row IRR
# data.table. Calendar trial_term matches irr() exactly.
TTEEnrollment$set("private", ".fit_irr", function(data, weight_col) {
  design <- self$design

  has_trial_id <- "trial_id" %in%
    names(data) &&
    data[, data.table::uniqueN(trial_id)] > 1L
  n_trial_ids <- if (has_trial_id) {
    data[, data.table::uniqueN(trial_id)]
  } else {
    0L
  }

  # Subset to only needed columns to reduce svydesign memory footprint
  keep_cols <- unique(c(
    design$person_id_var,
    design$treatment_var,
    design$tstop_var,
    weight_col,
    "event",
    "person_weeks",
    if (has_trial_id) "trial_id"
  ))
  svy_data <- data[, ..keep_cols]

  svy_design <- survey::svydesign(
    ids = as.formula(paste0("~", design$person_id_var)),
    weights = as.formula(paste0("~", weight_col)),
    data = svy_data
  )
  rm(svy_data)

  warn <- FALSE
  treatment_coef <- paste0(design$treatment_var, "TRUE")

  trial_term <- if (has_trial_id && n_trial_ids >= 5L) {
    paste0(" + splines::ns(trial_id, df = 3)")
  } else if (has_trial_id) {
    " + trial_id"
  } else {
    ""
  }

  formula <- stats::as.formula(paste0(
    "event ~ ",
    design$treatment_var,
    " + splines::ns(",
    design$tstop_var,
    ", df = 3)",
    trial_term,
    " + offset(log(person_weeks))"
  ))
  poisson_fit <- withCallingHandlers(
    survey::svyglm(
      formula,
      design = svy_design,
      family = stats::quasipoisson()
    ),
    warning = function(w) {
      warn <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  rm(svy_design)
  fit_summary <- summary(poisson_fit)$coefficients
  if (!treatment_coef %in% rownames(fit_summary)) {
    # logical/factor treatment yields '<var>TRUE'; numeric 0/1 yields
    # just '<var>'
    if (design$treatment_var %in% rownames(fit_summary)) {
      treatment_coef <- design$treatment_var
    } else {
      stop(
        "treatment coefficient '",
        treatment_coef,
        "' not found in the outcome model; available: ",
        paste(rownames(fit_summary), collapse = ", ")
      )
    }
  }
  coef <- fit_summary[treatment_coef, "Estimate"]
  se <- fit_summary[treatment_coef, "Std. Error"]
  pvalue <- fit_summary[treatment_coef, "Pr(>|t|)"]
  rm(poisson_fit)

  result <- data.table::data.table(
    IRR = exp(coef),
    IRR_lower = exp(coef - 1.96 * se),
    IRR_upper = exp(coef + 1.96 * se),
    IRR_pvalue = pvalue,
    warn = warn
  )
  data.table::setattr(result, "swereg_type", "irr")
  result
})
