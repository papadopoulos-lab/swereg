# Estimation for a TTEEnrollment: rates, incidence rate ratios, the two
# interaction tests, the weighted survival curve, the risk difference and
# Table 1.
#
# Each function here holds the whole body of the public method that carries
# its name, guards included. The method in `R/r6_tteenrollment_estimation.R`
# is a one-call delegate to it.
#
# Two functions have no method of their own. `.tte_fit_irr()` is the weighted
# Poisson fit that `$irr()` and `$irr_by_subgroup()` share.
# `.tte_table1_core()` is the computation that `$table1()` and the plan's
# `.s3_enrollment_table1()` share.

# `..keep_cols` is data.table's "read this name one frame out" prefix.
# `.tte_outcome_frame()` below assigns `keep_cols` in the same frame. A local
# `..keep_cols <- NULL` would put both names in that scope, and data.table
# then warns on every call. `R/imports.R` declares `..cache_cols` for the same
# reason.
utils::globalVariables("..keep_cols")


#' Events, person-years and rates by treatment group
#'
#' The body of `TTEEnrollment$rates()`.
#'
#' @param self A `TTEEnrollment`.
#' @param weight_col Character(1), the weight column.
#' @return A data.table with one row per treatment group. It carries the
#'   weighted events, person-years and rate, and the unweighted
#'   `events_unweighted` and `py_unweighted` of the same rows.
#' @noRd
.tte_est_rates <- function(self, weight_col) {
  # Local bindings (avoid R CMD check NSE notes)
  event <- person_weeks <- NULL # nolint

  if (self$data_level != "trial") {
    stop(
      "rates() requires trial level data.\n",
      "Current data_level: '",
      self$data_level,
      "'",
      call. = FALSE
    )
  }

  design <- self$design
  data <- self$data

  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data", call. = FALSE)
  }
  if (!"event" %in% names(data)) {
    stop(
      "'event' column not found. Run $s4_prepare_for_analysis() first.",
      call. = FALSE
    )
  }
  if (!"person_weeks" %in% names(data)) {
    stop(
      "'person_weeks' column not found. Enrollment should create this automatically.",
      call. = FALSE
    )
  }

  # Sequential TTE inflates person-trial counts relative to unique
  # participants (one person contributes to many weekly trials). Surface
  # `n_persons` alongside `n_trials` so readers and downstream tables see
  # both the analytic denominator and the underlying sample size.
  #
  # `events_unweighted` and `py_unweighted` are the plain sums over the same
  # rows. `.tte_mde()` reads them for the minimum detectable effect, which is a
  # naive Poisson calculation on counts and not on weighted totals.
  result <- data[,
    .(
      n_persons = data.table::uniqueN(get(design$person_id_var)),
      n_trials = data.table::uniqueN(get(design$id_var)),
      events_weighted = sum(event * get(weight_col)),
      py_weighted = sum(person_weeks * get(weight_col)) / 52.25,
      rate_per_100000py = sum(event * get(weight_col)) /
        (sum(person_weeks * get(weight_col)) / 52.25) *
        100000,
      events_unweighted = sum(event),
      py_unweighted = sum(person_weeks) / 52.25
    ),
    by = c(design$treatment_var)
  ]
  data.table::setattr(result, "swereg_type", "rates")
  data.table::setattr(result, "treatment_var", design$treatment_var)
  return(result)
}


#' Weighted Poisson incidence rate ratio for the whole panel
#'
#' The body of `TTEEnrollment$irr()`. It owns the guards and hands the fit to
#' `.tte_fit_irr()`.
#'
#' @param self A `TTEEnrollment`.
#' @param weight_col Character(1), the weight column.
#' @param conf_level Numeric(1) in (0, 1), the interval level.
#' @return A one-row data.table of IRR estimates. `events_intervention`
#'   and `events_comparator` are the event totals of the two arms, which
#'   is what `.tte_irr_estimable()` reads.
#' @noRd
.tte_est_irr <- function(self, weight_col, conf_level = 0.95) {
  if (self$data_level != "trial") {
    stop(
      "irr() requires trial level data.\n",
      "Current data_level: '",
      self$data_level,
      "'",
      call. = FALSE
    )
  }

  design <- self$design
  data <- self$data

  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data", call. = FALSE)
  }
  if (!"event" %in% names(data)) {
    stop(
      "'event' column not found. Run $s4_prepare_for_analysis() first.",
      call. = FALSE
    )
  }
  if (!"person_weeks" %in% names(data)) {
    stop(
      "'person_weeks' column not found. Enrollment should create this automatically.",
      call. = FALSE
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
      "For an intention-to-treat analysis, prepare with estimand = \"itt\".",
      call. = FALSE
    )
  }

  return(.tte_fit_irr(data, weight_col, design, conf_level))
}


#' Events in each arm, from the by-arm event table
#'
#' `.tte_fit_irr()` and `fit_one()` inside `.tte_est_irr_by_subgroup()` both
#' build `ev_by_arm`, one row per treatment arm with the event total in `V1`.
#' This reads the intervention and the comparator total off it.
#'
#' The treatment column is logical, a 0/1 numeric, or a factor with `TRUE` and
#' `FALSE` levels. Those are the forms the outcome model reads. Anything else
#' gives `NA` for both counts, and `.tte_irr_estimable()` then falls back to
#' its ratio rule. Measured on R 4.5.2: `as.logical()` returns `NA` for a
#' factor of `"0"` and `"1"`. Such a column already stops the fit upstream,
#' with an unknown treatment coefficient.
#'
#' @param ev_by_arm A data.table with the treatment column and `V1`.
#' @param treatment_var Character(1), the treatment column name.
#' @return A list with `intervention` and `comparator`, each numeric(1).
#' @noRd
.tte_arm_event_counts <- function(ev_by_arm, treatment_var) {
  arm <- suppressWarnings(as.logical(ev_by_arm[[treatment_var]]))
  n_ev <- suppressWarnings(as.numeric(ev_by_arm$V1))
  if (length(arm) == 0L || anyNA(arm) || anyNA(n_ev)) {
    return(list(intervention = NA_real_, comparator = NA_real_))
  }
  return(list(
    intervention = sum(n_ev[arm]),
    comparator = sum(n_ev[!arm])
  ))
}


#' The rows an outcome model reads, with the time since time zero
#'
#' Copies the columns the outcome model reads. It then writes the time since
#' time zero at the interval start under `design$tstart_var`.
#' `.tte_interval_start()` supplies that time, so a panel without a start
#' column still fits.
#'
#' @param data A data.table, the rows to fit on.
#' @param design A `TTEDesign`.
#' @param cols Character vector, the other columns the model reads.
#' @return A new data.table.
#' @noRd
.tte_outcome_frame <- function(data, design, cols) {
  keep_cols <- unique(c(
    design$person_id_var,
    design$treatment_var,
    "event",
    "person_weeks",
    cols
  ))
  out <- data[, ..keep_cols]
  times <- sort(unique(data[[design$tstop_var]]))
  data.table::set(
    out,
    j = design$tstart_var,
    value = .tte_interval_start(
      data,
      design$tstart_var,
      design$tstop_var,
      times
    )
  )
  return(out)
}

#' The time-term of one column of an outcome model
#'
#' Counts the distinct values of `var` in the rows the model is fitted to, and
#' names the term through `.tte_time_term()`. Every outcome model is a
#' `survey::svyglm()` fit, so the term never uses `s()`. A frame without the
#' column takes no term.
#'
#' @param frame A data.table, the rows the model is fitted to.
#' @param var Character(1), the column the term reads.
#' @return A character scalar. It is `""` when the term is dropped.
#' @noRd
.tte_outcome_term <- function(frame, var) {
  if (!var %in% names(frame)) {
    return("")
  }
  return(.tte_time_term(var, data.table::uniqueN(frame[[var]]), gam = FALSE))
}

#' The formula of an outcome model
#'
#' Joins the terms, drops the empty ones, and adds the person-time offset.
#' The formula takes the package namespace as its environment, so it holds no
#' reference to the panel when it is stored on a result.
#'
#' @param terms Character vector, the right-hand side terms.
#' @return A formula with `event` on the left.
#' @noRd
.tte_outcome_formula <- function(terms) {
  terms <- terms[nzchar(terms)]
  return(stats::as.formula(
    paste0(
      "event ~ ",
      paste(terms, collapse = " + "),
      " + offset(log(person_weeks))"
    ),
    env = topenv()
  ))
}

#' Two-sided normal critical value for a confidence level
#'
#' A Wald interval at level `conf_level` is `estimate +/- z * se` with this
#' `z`. A level of 0.95 gives 1.959964.
#'
#' @param conf_level Numeric(1) strictly between 0 and 1.
#' @return Numeric(1).
#' @noRd
.tte_z_crit <- function(conf_level) {
  return(stats::qnorm(1 - (1 - conf_level) / 2))
}

#' Is this the knot warning of `splines::ns()`?
#'
#' `splines::ns(x, df = 3)` puts its two interior knots at the tertiles of `x`.
#' A short panel holds a third or more of its rows at the first follow-up
#' interval, so the lower tertile ties with the boundary knot. `ns()` then
#' moves that knot just inside the boundary, and warns.
#'
#' The warning does not mark a fit problem. Measured in 27.2.0 on a panel with
#' 4 distinct values of `tstart`: the basis is full rank and the fit converges.
#' The treatment coefficient and its standard error equal those of the
#' `factor(tstart)` fit, because both terms span the same columns. On a panel
#' with 8 values, the ratio differs from the `factor(tstart)` fit by 0.04%.
#'
#' @param w A warning condition.
#' @return `TRUE` for the knot warning of `ns()`, `FALSE` otherwise.
#' @noRd
.tte_is_ns_knot_shove <- function(w) {
  return(grepl(
    "shoving 'interior' knots matching boundary knots to inside",
    conditionMessage(w),
    fixed = TRUE
  ))
}

#' Weighted Poisson MSM fit for one data subset
#'
#' The estimation core shared by `.tte_est_irr()` and
#' `.tte_est_irr_by_subgroup()`. The caller owns the guards, which are weight
#' validity and the required columns. This fits the model on whatever `data`
#' it is handed and returns the one-row IRR data.table.
#'
#' The model is `tx + flex(tstart) + flex(enrollment_period_id)` with the
#' person-time offset. Each `flex()` term comes from `.tte_time_term()`, and
#' counts the distinct values of the rows in `data`.
#'
#' @param data A data.table, the rows to fit on.
#' @param weight_col Character(1), the weight column.
#' @param design A `TTEDesign`.
#' @param conf_level Numeric(1) in (0, 1), the interval level.
#' @return A one-row data.table of IRR estimates. It carries
#'   `events_intervention` and `events_comparator` on both paths, so the
#'   `NA` row still says which arm held no event. A fitted row carries the
#'   formula the fit received in `attr(, "model_formula")`.
#' @noRd
.tte_fit_irr <- function(data, weight_col, design, conf_level = 0.95) {
  # Local bindings (avoid R CMD check NSE notes)
  event <- NULL # nolint

  # Both arms need at least one event. Zero events in one arm separates the
  # Poisson fit, which returns a very large or very small ratio with an
  # interval rather than an error. Zero events in both arms returns a ratio of
  # about 1. Preflight it here, in the same shape `irr_by_subgroup()` uses
  # before it calls this function.
  ev_by_arm <- data[, sum(event, na.rm = TRUE), by = c(design$treatment_var)]
  ev_n <- .tte_arm_event_counts(ev_by_arm, design$treatment_var)
  if (nrow(ev_by_arm) < 2L || any(ev_by_arm$V1 == 0L)) {
    warning(
      "irr: no events in one or both treatment arms; returning NA.",
      call. = FALSE
    )
    out_na <- data.table::data.table(
      IRR = NA_real_,
      IRR_lower = NA_real_,
      IRR_upper = NA_real_,
      IRR_pvalue = NA_real_,
      warn = TRUE,
      events_intervention = ev_n$intervention,
      events_comparator = ev_n$comparator
    )
    data.table::setattr(out_na, "swereg_type", "irr")
    return(out_na)
  }

  # Subset to only needed columns to reduce svydesign memory footprint
  svy_data <- .tte_outcome_frame(
    data,
    design,
    c(weight_col, intersect("enrollment_period_id", names(data)))
  )
  formula <- .tte_outcome_formula(c(
    design$treatment_var,
    .tte_outcome_term(svy_data, design$tstart_var),
    .tte_outcome_term(svy_data, "enrollment_period_id")
  ))

  svy_design <- survey::svydesign(
    ids = stats::as.formula(paste0("~", design$person_id_var)),
    weights = stats::as.formula(paste0("~", weight_col)),
    data = svy_data
  )
  rm(svy_data)

  warn <- FALSE
  treatment_coef <- paste0(design$treatment_var, "TRUE")

  poisson_fit <- withCallingHandlers(
    survey::svyglm(
      formula,
      design = svy_design,
      family = stats::quasipoisson()
    ),
    warning = function(w) {
      if (!.tte_is_ns_knot_shove(w)) {
        warn <<- TRUE
      }
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
        paste(rownames(fit_summary), collapse = ", "),
        call. = FALSE
      )
    }
  }
  coef <- fit_summary[treatment_coef, "Estimate"]
  se <- fit_summary[treatment_coef, "Std. Error"]
  pvalue <- fit_summary[treatment_coef, "Pr(>|t|)"]
  rm(poisson_fit)

  result <- data.table::data.table(
    IRR = exp(coef),
    IRR_lower = exp(coef - .tte_z_crit(conf_level) * se),
    IRR_upper = exp(coef + .tte_z_crit(conf_level) * se),
    IRR_pvalue = pvalue,
    warn = warn,
    events_intervention = ev_n$intervention,
    events_comparator = ev_n$comparator
  )
  data.table::setattr(result, "swereg_type", "irr")
  data.table::setattr(result, "model_formula", formula)
  return(result)
}


#' Wald test for heterogeneity of the treatment effect across trials
#'
#' The body of `TTEEnrollment$heterogeneity_test()`. The model is
#' `tx * splines::ns(enrollment_period_id, df = min(3, n - 1)) + flex(tstart)`
#' with the person-time offset, where `n` is the number of trials.
#'
#' @param self A `TTEEnrollment`.
#' @param weight_col Character(1), the weight column.
#' @return A list with `p_value`, `n_trials` and `interaction_coefs`. It
#'   carries the formula the fit received in `attr(, "model_formula")`.
#' @noRd
.tte_est_heterogeneity_test <- function(self, weight_col) {
  # Local bindings (avoid R CMD check NSE notes)
  enrollment_period_id <- NULL # nolint

  if (self$data_level != "trial") {
    stop("heterogeneity_test() requires trial level data.", call. = FALSE)
  }

  design <- self$design
  data <- self$data

  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data", call. = FALSE)
  }
  if (!"event" %in% names(data)) {
    stop(
      "'event' column not found. Run $s4_prepare_for_analysis() first.",
      call. = FALSE
    )
  }
  if (!"enrollment_period_id" %in% names(data)) {
    stop(
      "'enrollment_period_id' column not found. ",
      "Heterogeneity test requires multiple trials.",
      call. = FALSE
    )
  }

  n_trials <- data[, data.table::uniqueN(enrollment_period_id)]
  if (n_trials < 2L) {
    stop(
      "Need at least 2 unique enrollment_period_ids for heterogeneity test.",
      call. = FALSE
    )
  }

  svy_data <- .tte_outcome_frame(
    data,
    design,
    c(weight_col, "enrollment_period_id")
  )
  # Spline interaction: does the treatment effect vary smoothly over the
  # trials? `ns(enrollment_period_id)` interacted with treatment gives at most
  # 3 interaction terms instead of one per trial.
  spline_df <- min(3L, n_trials - 1L)
  formula_int <- .tte_outcome_formula(c(
    paste0(
      design$treatment_var,
      " * splines::ns(enrollment_period_id, df = ",
      spline_df,
      ")"
    ),
    .tte_outcome_term(svy_data, design$tstart_var)
  ))

  svy_design <- survey::svydesign(
    ids = stats::as.formula(paste0("~", design$person_id_var)),
    weights = stats::as.formula(paste0("~", weight_col)),
    data = svy_data
  )
  rm(svy_data)

  fit <- survey::svyglm(
    formula_int,
    design = svy_design,
    family = stats::quasipoisson()
  )
  rm(svy_design)

  # Extract interaction coefficients (treatment:ns(enrollment_period_id))
  coef_names <- names(stats::coef(fit))
  interaction_idx <- grep(
    paste0("^", design$treatment_var, "TRUE:"),
    coef_names
  )

  if (length(interaction_idx) == 0) {
    return(structure(
      list(
        p_value = NA_real_,
        n_trials = n_trials,
        interaction_coefs = data.table::data.table()
      ),
      model_formula = formula_int
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

  return(structure(
    list(
      p_value = p_value,
      n_trials = n_trials,
      interaction_coefs = interaction_coefs
    ),
    model_formula = formula_int
  ))
}


#' Wald test for effect modification by a baseline subgroup
#'
#' The body of `TTEEnrollment$effect_modification_test()`. The model is
#' `tx * factor(sg) + flex(tstart) + flex(enrollment_period_id)` with the
#' person-time offset, fitted on the rows with a known subgroup.
#'
#' @param self A `TTEEnrollment`.
#' @param weight_col Character(1), the weight column.
#' @param subgroup_var Character(1), a categorical baseline column.
#' @param conf_level Numeric(1) in (0, 1), the level of the interval of the
#'   ratio of stratum IRRs.
#' @return A list with `p_value`, `subgroup_var`, `n_levels`,
#'   `interaction_coefs` and the ratio of stratum IRRs. It carries the
#'   formula the fit received in `attr(, "model_formula")`.
#' @noRd
.tte_est_effect_modification_test <- function(
  self,
  weight_col,
  subgroup_var,
  conf_level = 0.95
) {
  if (self$data_level != "trial") {
    stop("effect_modification_test() requires trial level data.", call. = FALSE)
  }
  design <- self$design
  data <- self$data
  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data", call. = FALSE)
  }
  if (!subgroup_var %in% names(data)) {
    stop("subgroup_var '", subgroup_var, "' not found in data", call. = FALSE)
  }
  if (!"event" %in% names(data)) {
    stop(
      "'event' column not found. Run $s4_prepare_for_analysis() first.",
      call. = FALSE
    )
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
      "prepare with estimand = \"itt\".",
      call. = FALSE
    )
  }

  d <- data[!is.na(get(subgroup_var))]
  sg_levels <- sort(unique(d[[subgroup_var]]))
  n_levels <- length(sg_levels)
  if (n_levels < 2L) {
    stop(
      "subgroup_var '",
      subgroup_var,
      "' must have >= 2 non-NA levels for an effect-modification test.",
      call. = FALSE
    )
  }

  svy_data <- .tte_outcome_frame(
    d,
    design,
    c(weight_col, subgroup_var, intersect("enrollment_period_id", names(d)))
  )
  svy_data[[subgroup_var]] <- factor(svy_data[[subgroup_var]])
  formula_int <- .tte_outcome_formula(c(
    paste0(design$treatment_var, " * factor(", subgroup_var, ")"),
    .tte_outcome_term(svy_data, design$tstart_var),
    .tte_outcome_term(svy_data, "enrollment_period_id")
  ))

  svy_design <- survey::svydesign(
    ids = stats::as.formula(paste0("~", design$person_id_var)),
    weights = stats::as.formula(paste0("~", weight_col)),
    data = svy_data
  )
  rm(svy_data)

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
    return(structure(
      list(
        p_value = NA_real_,
        subgroup_var = subgroup_var,
        n_levels = n_levels,
        interaction_coefs = data.table::data.table(),
        ratio_of_irrs = NA_real_,
        ratio_lower = NA_real_,
        ratio_upper = NA_real_
      ),
      model_formula = formula_int
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
    ratio_lower <- exp(b - .tte_z_crit(conf_level) * s)
    ratio_upper <- exp(b + .tte_z_crit(conf_level) * s)
  } else {
    ratio <- NA_real_
    ratio_lower <- NA_real_
    ratio_upper <- NA_real_
  }
  rm(fit)

  return(structure(
    list(
      p_value = p_value,
      subgroup_var = subgroup_var,
      n_levels = n_levels,
      interaction_coefs = interaction_coefs,
      ratio_of_irrs = ratio,
      ratio_lower = ratio_lower,
      ratio_upper = ratio_upper
    ),
    model_formula = formula_int
  ))
}


#' Stratified incidence rate ratios within a baseline subgroup
#'
#' The body of `TTEEnrollment$irr_by_subgroup()`. The effect-modification
#' p-value still comes through `self$effect_modification_test()`.
#'
#' @param self A `TTEEnrollment`.
#' @param weight_col Character(1), the weight column.
#' @param subgroup_var Character(1), a categorical baseline column.
#' @param conf_level Numeric(1) in (0, 1), the interval level.
#' @return A data.table with one row per stratum, plus an `"all"` row.
#'   Each row carries `events_intervention` and `events_comparator` for
#'   that stratum. `model_formula` is the formula of the fit of that row, as
#'   one string, and `NA` when no fit produced the row.
#' @noRd
.tte_est_irr_by_subgroup <- function(
  self,
  weight_col,
  subgroup_var,
  conf_level = 0.95
) {
  # Local bindings (avoid R CMD check NSE notes)
  event <- NULL # nolint

  if (self$data_level != "trial") {
    stop("irr_by_subgroup() requires trial level data.", call. = FALSE)
  }
  design <- self$design
  data <- self$data
  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data", call. = FALSE)
  }
  if (!subgroup_var %in% names(data)) {
    stop("subgroup_var '", subgroup_var, "' not found in data", call. = FALSE)
  }
  if (!"event" %in% names(data)) {
    stop(
      "'event' column not found. Run $s4_prepare_for_analysis() first.",
      call. = FALSE
    )
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
      "prepare with estimand = \"itt\".",
      call. = FALSE
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
      "' must have >= 2 non-NA levels.",
      call. = FALSE
    )
  }

  na_row <- function(level_label, ev_n = NULL) {
    if (is.null(ev_n)) {
      ev_n <- list(intervention = NA_real_, comparator = NA_real_)
    }
    return(data.table::data.table(
      level = level_label,
      IRR = NA_real_,
      IRR_lower = NA_real_,
      IRR_upper = NA_real_,
      IRR_pvalue = NA_real_,
      warn = TRUE,
      events_intervention = ev_n$intervention,
      events_comparator = ev_n$comparator,
      model_formula = NA_character_
    ))
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
        "' has no events in one or both treatment arms; returning NA.",
        call. = FALSE
      )
      return(na_row(
        level_label,
        .tte_arm_event_counts(ev_by_arm, treatment_var)
      ))
    }
    r <- tryCatch(
      .tte_fit_irr(subset, weight_col, design, conf_level),
      error = function(e) {
        warning(
          "irr_by_subgroup: fit failed for stratum '",
          level_label,
          "': ",
          conditionMessage(e),
          call. = FALSE
        )
        return(NULL)
      }
    )
    if (is.null(r)) {
      return(na_row(level_label))
    }
    # Each row names its own fit. `.tte_fit_irr()` counts the distinct values
    # of the time terms in the rows it fits, so a stratum can take a different
    # time term from the whole cohort. The NA row of `.tte_fit_irr()` names
    # no formula.
    mf <- attr(r, "model_formula")
    return(data.table::data.table(
      level = level_label,
      IRR = r$IRR,
      IRR_lower = r$IRR_lower,
      IRR_upper = r$IRR_upper,
      IRR_pvalue = r$IRR_pvalue,
      warn = r$warn,
      events_intervention = r$events_intervention,
      events_comparator = r$events_comparator,
      model_formula = if (is.null(mf)) NA_character_ else deparse1(mf)
    ))
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
    self$effect_modification_test(weight_col, subgroup_var, conf_level),
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
  return(out)
}


#' Weighted discrete-time survival curve from the person-week panel
#'
#' The body of `TTEEnrollment$survival_curve()`.
#'
#' @param self A `TTEEnrollment`.
#' @param weight_col Character(1), the weight column. It may vary over time.
#' @param save_path Character(1) or NULL. A path saves the plot.
#' @param title Character(1) or NULL, the plot title.
#' @param subtitle Character(1) or NULL, the plot subtitle.
#' @param ylim Numeric(2) or NULL, the y-axis zoom.
#' @param arm_labels Named character or NULL, the legend labels.
#' @param scale Character(1), `"survival"` or `"cumulative_failure"`.
#' @return A data.table of the curve, invisibly when `save_path` is given.
#' @noRd
.tte_est_survival_curve <- function(
  self,
  weight_col,
  save_path = NULL,
  title = NULL,
  subtitle = NULL,
  ylim = NULL,
  arm_labels = NULL,
  scale = c("survival", "cumulative_failure")
) {
  # Local bindings (avoid R CMD check NSE notes)
  hazard <- events <- at_risk <- surv <- group <- NULL # nolint

  if (self$data_level != "trial") {
    stop(
      "survival_curve() requires trial level data.\n",
      "Current data_level: '",
      self$data_level,
      "'",
      call. = FALSE
    )
  }
  scale <- match.arg(scale)

  design <- self$design
  data <- self$data

  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data", call. = FALSE)
  }
  if (!"event" %in% names(data)) {
    stop(
      "'event' column not found. Run $s4_prepare_for_analysis() first.",
      call. = FALSE
    )
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
      "' must be numeric, finite, non-missing and non-negative",
      call. = FALSE
    )
  }
  if (anyNA(data$event) || !all(data$event %in% c(0L, 1L))) {
    stop("'event' must be a non-missing 0/1 indicator", call. = FALSE)
  }
  # An NA arm would form a third curve of its own, and one arm gives no
  # contrast to draw.
  tv <- data[[tvar]]
  if (anyNA(tv)) {
    stop("treatment '", tvar, "' must be non-missing", call. = FALSE)
  }
  if (length(unique(tv)) < 2L) {
    stop(
      "treatment '",
      tvar,
      "' must have both arms; the data has only ",
      paste(unique(tv), collapse = ", "),
      call. = FALSE
    )
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
  # NOT `.N`: the panel is one row per person-trial interval, and a person
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
    stop(
      "weighted risk set (sum of weights) is <= 0 in an arm-period",
      call. = FALSE
    )
  }
  if (any(curve$at_risk <= 0 & curve$events > 0)) {
    stop("an event falls at a time whose risk set is empty", call. = FALSE)
  }
  curve[, hazard := events / at_risk]
  # Nobody at risk: the hazard is undefined and reads NA, and the survival
  # carries its latest exact value forward. `cumprod` is valid over these exact
  # event boundaries, and a follow-up interval hazard over unequal intervals is
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
      "'",
      call. = FALSE
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
  return(invisible(curve[]))
}


#' Signed cause-specific risk difference at each follow-up interval
#'
#' The body of `TTEEnrollment$risk_difference()`.
#'
#' @param self A `TTEEnrollment`.
#' @param weight_col Character(1), the weight column. It may vary over time.
#' @param n_boot Integer(1), the number of bootstrap replicates.
#' @param seed Integer(1) or NULL. A seed makes the draw reproducible.
#' @param conf_level Numeric(1) in (0, 1), the percentile interval level.
#' @return A data.table with one row per follow-up interval.
#' @noRd
.tte_est_risk_difference <- function(
  self,
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
      "'",
      call. = FALSE
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

  return(.tte_rd_curve(
    data = self$data,
    person_id_var = design$person_id_var,
    id_var = design$id_var,
    treatment_var = design$treatment_var,
    time_var = design$tstop_var,
    weight_col = weight_col,
    n_boot = n_boot,
    conf_level = conf_level,
    tstart_var = design$tstart_var
  ))
}


#' Baseline characteristics for one enrollment
#'
#' The body of `TTEEnrollment$table1()`. It owns every check the method makes,
#' then calls the shared computation.
#'
#' @param self A `TTEEnrollment`.
#' @param ipw_col Character(1) or NULL, the weight column.
#' @param arm_labels Named character or NULL, the column headers.
#' @param include_smd Logical(1), whether to emit an SMD column.
#' @param show_missing One of `"when_present"`, `"always"` or `"none"`.
#' @return A data.table with class `swereg_table1`.
#' @noRd
.tte_est_table1 <- function(
  self,
  ipw_col = NULL,
  arm_labels = NULL,
  include_smd = TRUE,
  show_missing = c("when_present", "always", "none")
) {
  show_missing <- match.arg(show_missing)
  if (self$data_level != "trial") {
    stop(
      "table1() requires trial level data.\n",
      "Current data_level: '",
      self$data_level,
      "'\n",
      "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level.",
      call. = FALSE
    )
  }

  design <- self$design

  if (!is.null(ipw_col) && !ipw_col %in% names(self$data)) {
    stop("ipw_col '", ipw_col, "' not found in data", call. = FALSE)
  }

  return(.tte_table1_core(
    data = self$data,
    design = design,
    ipw_col = ipw_col,
    arm_labels = arm_labels,
    include_smd = include_smd,
    show_missing = show_missing
  ))
}


#' The Table 1 computation both routes share
#'
#' `.tte_est_table1()` reaches it from the method, and
#' `.s3_enrollment_table1()` reaches it from the plan's worker. The two routes
#' validate differently and MUST compute identically, which is what this
#' function guarantees.
#'
#' @param data A data.table, the whole person-trial panel.
#' @param design A `TTEDesign`, or a plain list carrying the same fields.
#' @param ipw_col Character(1) or NULL, the weight column.
#' @param arm_labels Named character or NULL, the column headers.
#' @param include_smd Logical(1), whether to emit an SMD column.
#' @param show_missing One of `"when_present"`, `"always"` or `"none"`.
#' @return A data.table with class `swereg_table1`.
#' @noRd
.tte_table1_core <- function(
  data,
  design,
  ipw_col = NULL,
  arm_labels = NULL,
  include_smd = TRUE,
  show_missing = "when_present"
) {
  baseline <- data[get(design$tstart_var) == 0]
  # Table 1 describes the cohort at time zero, so it reads the same
  # entry-window snapshot that `$s2_ipw()` fits on.
  return(.swereg_table1(
    data = .tte_entry_view(
      baseline,
      design$confounder_vars,
      keep_cols = c(design$treatment_var, ipw_col)
    ),
    vars = design$confounder_vars,
    strata = design$treatment_var,
    weights = ipw_col,
    include_smd = include_smd,
    show_missing = show_missing,
    arm_labels = arm_labels
  ))
}
