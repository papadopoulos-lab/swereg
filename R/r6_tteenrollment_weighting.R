# TTEEnrollment methods that build, adjust and describe the analysis
# weights. The treatment weights come from steps 1 to 3, the censoring
# weights from step 6, and their product is the analysis weight.

#' @include r6_tteenrollment.R
#' @description Step 1: Impute missing confounders by sampling from observed values.
#' @param confounder_vars Character vector of confounder column names to impute.
#' @param seed Integer seed for reproducibility (default: 4L).
TTEEnrollment$set(
  "public",
  "s1_impute_confounders",
  function(confounder_vars, seed = 4L) {
    id_var <- self$design$id_var

    # Build a trial-level table once. Prefer filtering to baseline rows
    # (tstart_var == 0), which is a single linear scan on the panel.
    # Fall back to a group-by first() collapse only when tstart_var is
    # missing. baseline_dt serves both the NA pre-scan and the
    # update-join below, so we never collapse twice.
    tstart_var <- self$design$tstart_var
    baseline_dt <- if (
      !is.null(tstart_var) && tstart_var %in% names(self$data)
    ) {
      self$data[
        get(tstart_var) == 0,
        .SD,
        .SDcols = c(id_var, confounder_vars)
      ]
    } else {
      self$data[,
        lapply(.SD, data.table::first),
        by = c(id_var),
        .SDcols = confounder_vars
      ]
    }
    needs_impute <- confounder_vars[
      vapply(confounder_vars, \(v) anyNA(baseline_dt[[v]]), logical(1))
    ]
    if (length(needs_impute) == 0L) {
      self$steps_completed <- c(self$steps_completed, "impute")
      return(invisible(self))
    }

    # Sample replacements for missing trial-level confounder values.
    set.seed(seed)
    for (var in needs_impute) {
      missing_trials <- baseline_dt[is.na(get(var)), get(id_var)]
      observed_vals <- baseline_dt[!is.na(get(var)), get(var)]
      sampled_vals <- sample(
        observed_vals,
        length(missing_trials),
        replace = TRUE
      )
      baseline_dt[get(id_var) %in% missing_trials, (var) := sampled_vals]
    }

    # Update-join: overwrite the needs_impute columns in `self$data` in
    # place with the imputed trial-level values. Avoids allocating a
    # new merged table.
    data.table::setkeyv(self$data, id_var)
    data.table::setkeyv(baseline_dt, id_var)
    self$data[
      baseline_dt,
      (needs_impute) := mget(paste0("i.", needs_impute)),
      on = id_var
    ]

    self$steps_completed <- c(self$steps_completed, "impute")
    return(invisible(self))
  }
)

#' @description Step 2: Calculates inverse probability of treatment weights.
#'
#' Estimates the propensity score P(A=1 | L_baseline) via logistic
#' regression on baseline rows only, then computes stabilized (or
#' unstabilized) IPW. This addresses **baseline** confounding for the
#' per-protocol analysis pipeline.
#'
#' Note: This does NOT estimate time-varying treatment weights
#' for as-treated analysis (Danaei 2013, Section 4.3). As-treated
#' analysis is not currently implemented.
#'
#' Robust standard errors for within-person correlation are handled
#' downstream by `survey::svydesign(ids = ~person_id_var)` in
#' `$irr()` (Hernan 2008, Danaei 2013).
#'
#' `$ps_fit` also records `n_dropped_na_snapshot`, the person-trials the fit
#' dropped for a missing confounder. `stats::glm()` drops such a row and says
#' nothing. The method prints a message when the count is above zero.
#'
#' @param stabilize Logical, default TRUE.
TTEEnrollment$set("public", "s2_ipw", function(stabilize = TRUE) {
  if (self$data_level != "trial") {
    stop(
      "s2_ipw() requires trial level data.\n",
      "Current data_level: '",
      self$data_level,
      "'\n",
      "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level.",
      call. = FALSE
    )
  }

  design <- self$design
  treatment_var <- design$treatment_var
  confounder_vars <- design$confounder_vars
  id_var <- design$id_var

  # --- Inline calculate_ipw logic ---
  baseline <- self$data[get(design$tstart_var) == 0]

  missing_confounders <- setdiff(confounder_vars, names(baseline))
  if (length(missing_confounders) > 0) {
    stop(
      "Confounders not found in data: ",
      paste(missing_confounders, collapse = ", "),
      call. = FALSE
    )
  }

  # Fit on the ENTRY-WINDOW snapshot. `tstart == 0` is the row of the first
  # follow-up interval, which starts at time zero. The confounder columns there
  # therefore hold follow-up values and not baseline ones. `fit_dt` is local,
  # and the rename inside it never reaches the panel.
  use_entry <- .tte_has_entry_snapshot(baseline, confounder_vars)
  entry_cols <- .tte_entry_col(confounder_vars)
  fit_cols <- unique(c(
    id_var,
    treatment_var,
    confounder_vars,
    if (use_entry) entry_cols
  ))
  fit_dt <- data.table::copy(
    baseline[, intersect(fit_cols, names(baseline)), with = FALSE]
  )
  if (use_entry) {
    for (i in seq_along(confounder_vars)) {
      data.table::set(
        fit_dt,
        j = confounder_vars[i],
        value = fit_dt[[entry_cols[i]]]
      )
    }
  }

  ps_formula <- stats::as.formula(
    paste(treatment_var, "~", paste(confounder_vars, collapse = " + "))
  )
  ps_model <- stats::glm(
    ps_formula,
    data = fit_dt,
    family = stats::binomial
  )
  fit_dt[, ps := stats::predict(ps_model, fit_dt, type = "response")]

  # Separation leaves the propensity model unusable and does not stop the
  # fit. A probability at the boundary gives an inverse probability weight
  # near 1e8, and a rank that reaches the row count means the model
  # reproduces the treatment column. Record the numbers on `$ps_fit`,
  # and warn on either sign. This is a warning and never a stop: at full
  # scale a handful of boundary probabilities is possible, and s1 runs for
  # hours.
  n_fit <- as.integer(stats::nobs(ps_model))
  ps_rank <- as.integer(ps_model$rank)
  converged <- isTRUE(ps_model$converged)
  n_boundary <- as.integer(sum(
    fit_dt$ps < 1e-8 | fit_dt$ps > 1 - 1e-8,
    na.rm = TRUE
  ))
  # `stats::glm()` drops a row with an NA in any model variable and says
  # nothing. With `impute_fn = NULL` the entry snapshot keeps its NAs, so the
  # propensity model can silently fit on fewer person-trials than the panel
  # holds. The difference is the count, and it is reported.
  n_dropped_na_snapshot <- as.integer(nrow(fit_dt) - n_fit)
  self$ps_fit <- data.table::data.table(
    n_fit = n_fit,
    rank = ps_rank,
    converged = converged,
    n_boundary = n_boundary,
    n_dropped_na_snapshot = n_dropped_na_snapshot
  )
  if (n_dropped_na_snapshot > 0L) {
    message(
      "s2_ipw(): the propensity fit dropped ",
      n_dropped_na_snapshot,
      " of ",
      nrow(fit_dt),
      " person-trials for a missing confounder. Run with an impute_fn to ",
      "fill the entry snapshot. $ps_fit$n_dropped_na_snapshot holds the count."
    )
  }
  if (ps_rank >= n_fit - 1L || n_boundary > 0L) {
    warning(
      "s2_ipw(): the propensity model has a boundary probability, a rank ",
      "within one of the row count, or both. Both are signs of separation. ",
      "A boundary probability gives an inverse probability weight near 1e8. ",
      "n_fit = ",
      n_fit,
      ", rank = ",
      ps_rank,
      ", converged = ",
      converged,
      ", n_boundary = ",
      n_boundary,
      ". $ps_fit holds these four numbers.",
      call. = FALSE
    )
  }

  if (stabilize) {
    p_intervention <- mean(fit_dt[[treatment_var]], na.rm = TRUE)
    fit_dt[,
      ipw := data.table::fifelse(
        get(treatment_var) == TRUE,
        p_intervention / ps,
        (1 - p_intervention) / (1 - ps)
      )
    ]
  } else {
    fit_dt[,
      ipw := data.table::fifelse(
        get(treatment_var) == TRUE,
        1 / ps,
        1 / (1 - ps)
      )
    ]
  }

  data.table::setkeyv(fit_dt, id_var)
  self$data[fit_dt, `:=`(ps = i.ps, ipw = i.ipw), on = id_var]

  self$weight_cols <- unique(c(self$weight_cols, "ipw"))
  self$steps_completed <- c(self$steps_completed, "ipw")
  return(invisible(self))
})

#' @description Step 3: Truncates extreme weights at specified quantiles.
#' @param weight_cols Character vector or NULL.
#' @param lower Numeric, default 0.01.
#' @param upper Numeric, default 0.99.
#' @param suffix Character, default "_trunc".
TTEEnrollment$set(
  "public",
  "s3_truncate_weights",
  function(
    weight_cols = NULL,
    lower = 0.01,
    upper = 0.99,
    suffix = "_trunc"
  ) {
    if (self$data_level != "trial") {
      stop(
        "s3_truncate_weights() requires trial level data.\n",
        "Current data_level: '",
        self$data_level,
        "'\n",
        "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level.",
        call. = FALSE
      )
    }

    if (is.null(weight_cols)) {
      weight_cols <- self$weight_cols
    }
    weight_cols <- intersect(weight_cols, names(self$data))

    if (length(weight_cols) == 0) {
      warning("No weight columns to truncate", call. = FALSE)
      return(invisible(self))
    }

    self$data <- private$.truncate_weights(
      data = self$data,
      weight_cols = weight_cols,
      lower = lower,
      upper = upper,
      suffix = suffix
    )

    new_cols <- paste0(weight_cols, suffix)
    self$weight_cols <- unique(c(self$weight_cols, new_cols))
    self$steps_completed <- c(self$steps_completed, "truncate")
    return(invisible(self))
  }
)

#' @description Print weight distribution diagnostics.
TTEEnrollment$set("public", "weight_summary", function() {
  cat("TTEEnrollment Weight Summary\n")
  cat("=======================\n\n")

  cat("Design:\n")
  if (!is.null(self$design$person_id_var)) {
    cat("  Person ID variable:", self$design$person_id_var, "\n")
  }
  cat("  Trial ID variable:", self$design$id_var, "\n")
  cat("  Treatment:", self$design$treatment_var, "\n")
  cat("  Outcomes:", paste(self$design$outcome_vars, collapse = ", "), "\n")
  cat("  Follow-up:", self$design$follow_up_time, "time units\n\n")

  cat("Data:\n")
  cat("  Level:", self$data_level, "\n")
  cat("  Rows:", format(nrow(self$data), big.mark = ","), "\n")
  cat("  Columns:", ncol(self$data), "\n\n")

  cat(
    "Steps completed:",
    paste(self$steps_completed, collapse = " -> "),
    "\n\n"
  )

  if (!is.null(self$active_outcome)) {
    cat("Active outcome:", self$active_outcome, "\n\n")
  }

  weight_cols <- intersect(self$weight_cols, names(self$data))
  if (length(weight_cols) > 0) {
    cat("Weight distributions:\n")
    for (col in weight_cols) {
      vals <- self$data[[col]]
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) {
        cat(sprintf(
          "  %s: mean=%.3f, sd=%.3f, min=%.3f, max=%.3f\n",
          col,
          mean(vals),
          stats::sd(vals),
          min(vals),
          max(vals)
        ))
      }
    }
  }

  return(invisible(self))
})

# =========================================================================
# Private weight/draw/collapse helpers
# =========================================================================
# --- s6_ipcw_pp: inverse probability of censoring weights (per-protocol) ----
#
# Three censoring models, fitted in each stratum in the order the data imply.
#
# 1. Loss. Loss is observed at the end of a row, after its outcome, so the
#    model fits on the rows without an event. An event row cannot be lost.
# 2. Deviation. A deviation is observed at the start of the next row, and a
#    person who is lost cannot be seen to deviate. The model fits on the rows
#    without an event that were not lost.
# 3. Time zero. A person-trial that is discordant in its first follow-up week
#    stops at time zero and keeps no row. A logistic model of that deviation
#    on the confounders at the recruiting week fits on every person-trial in
#    `self$time_zero_deviation`, the record `s5_prepare_outcome()` writes.
#
# The deviation model conditions on not being lost, so the product of the
# three uncensoring probabilities is the joint probability of remaining
# uncensored.
#
# The two row-level models are complementary log-log with a person-time
# offset:
#
#   cloglog{Pr(C_i = 1)} = eta_i + log(person_weeks_i)
#
# so the probability of staying uncensored over the row is
# `q_i = exp(-exp(eta_i) * person_weeks_i)`. One linear predictor then gives
# `q(4) = q(1)^4`, which is what makes a four-week follow-up interval and a
# one-week follow-up interval comparable.
#
# The row-level weights are LAGGED. Each is the probability of remaining
# uncensored through the START of the row, so the product stops at the row
# before. A censored follow-up interval stays in the risk set
# (`s5_prepare_outcome()` clips it and keeps it), and an inclusive product
# would count its own censoring probability inside its own weight.
#
# Each row-level model takes its time terms from `.tte_time_term()`, and each
# counts the distinct values of its own risk set after the zero-width rows
# leave:
#
#   denominator: flex(tstart) + flex(period_id) + confounders
#   numerator:   flex(tstart)
#
# A covariate of the stabilised-weight numerator MUST also be in the outcome
# model (Su et al. 2024, eq. 3). The outcome model carries
# `flex(tstart) + flex(enrollment_period_id)`, and a nonlinear `period_id`
# term is not in the span of those two. The numerator therefore reads
# `tstart` only. The time-zero numerator is the marginal proportion of the
# stratum.
#
# The weight of a row is
#
#   ipcw_pp = ipcw_pp_time_zero * ipcw_pp_loss * ipcw_pp_deviation
#
# and all four columns stay on the panel.
#
# `self$ipcw_formulas[[stratum]][[cause]]` records each model, with `cause`
# one of `loss`, `deviation` and `time_zero`. A fitted cause holds
# `list(fitted = TRUE, denominator = , numerator = )`. A cause that fits no
# model holds `list(fitted = FALSE, reason = )`, and its factor is 1.
#
# A row-level risk set in which every row is censored stops. swereg
# substitutes no marginal censoring rate for a model it could not fit.
TTEEnrollment$set(
  "private",
  "s6_ipcw_pp",
  function(
    estimate_ipcw_pp_separately_by_treatment = TRUE,
    estimate_ipcw_pp_with_gam = TRUE
  ) {
    if (self$data_level != "trial") {
      stop(
        "s6_ipcw_pp() requires trial level data.\n",
        "Current data_level: '",
        self$data_level,
        "'\n",
        "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level.",
        call. = FALSE
      )
    }

    if (!"ipw" %in% names(self$data)) {
      stop(
        "s6_ipcw_pp() requires 'ipw' column. Run $s2_ipw() first.",
        call. = FALSE
      )
    }

    design <- self$design
    treatment_var <- design$treatment_var
    confounder_vars <- design$confounder_vars
    id_var <- design$id_var
    tstart_var <- design$tstart_var
    tstop_var <- design$tstop_var
    use_gam <- estimate_ipcw_pp_with_gam

    needed <- c("event", "censor_loss", "censor_deviation")
    if (
      !all(needed %in% names(self$data)) || is.null(self$time_zero_deviation)
    ) {
      stop(
        "s6_ipcw_pp() needs the columns ",
        paste(needed, collapse = ", "),
        " and the time-zero record. Run $s4_prepare_for_analysis() first.",
        call. = FALSE
      )
    }

    working_data <- self$data[!is.na(get(treatment_var))]

    # The censoring models read the TIME-UPDATED confounder, and never the
    # entry-window snapshot. A missing value there makes `predict()` return
    # NA, and `cumprod()` below carries that NA through the rest of the
    # person-trial. Stop, and name what is missing.
    #
    # swereg MUST NOT overwrite an observed follow-up value with the
    # entry-window value here. That value describes the recruiting week.
    # `$s1b_fill_followup_confounders()` supplies a missing follow-up value
    # from the last observed value of the same person-trial, and s1d runs it
    # before this step.
    .tte_stop_on_missing_ipcw_confounders(
      working_data,
      confounder_vars,
      id_var
    )

    if (use_gam && !requireNamespace("mgcv", quietly = TRUE)) {
      stop(
        "Package 'mgcv' is required for use_gam = TRUE. ",
        "Install it with: install.packages('mgcv')",
        call. = FALSE
      )
    }

    # Person-time carries the offset. `s5_prepare_outcome()` writes
    # `person_weeks`, and a panel that arrives without it holds the same
    # quantity in its own interval.
    if (!"person_weeks" %in% names(working_data)) {
      working_data[,
        person_weeks := get(tstop_var) - get(tstart_var)
      ]
    }
    # `log(0)` is `-Inf`, so a zero-width row MUST NOT enter the offset. It
    # holds no person-time, so nothing can censor it. A row outside the risk
    # set of a cause cannot be censored by that cause either. Each such row
    # keeps an uncensoring probability of exactly 1.
    has_time <- !is.na(working_data$person_weeks) &
      working_data$person_weeks > 0
    no_event <- working_data$event == 0L
    not_lost <- working_data$censor_loss == 0L
    q_cols <- c("q_den_loss", "q_num_loss", "q_den_dev", "q_num_dev")
    working_data[, (q_cols) := 1]

    tz_all <- self$time_zero_deviation
    if (estimate_ipcw_pp_separately_by_treatment) {
      strata <- list(
        "the intervention arm" = TRUE,
        "the comparator arm" = FALSE
      )
    } else {
      strata <- list("the pooled cohort" = NA)
    }

    ipcw_formulas <- list()
    tz_factor <- list()
    for (label in names(strata)) {
      arm <- strata[[label]]
      in_stratum <- is.na(arm) | working_data[[treatment_var]] == arm
      tz <- tz_all[is.na(arm) | tz_all[[treatment_var]] == arm]

      # 1. Loss, on the rows without an event.
      rows <- which(in_stratum & has_time & no_event)
      loss <- .tte_ipcw_fit_cause(
        working_data[rows],
        "censor_loss",
        "loss",
        label,
        confounder_vars,
        tstart_var,
        use_gam
      )
      data.table::set(working_data, rows, "q_den_loss", loss$q_denominator)
      data.table::set(working_data, rows, "q_num_loss", loss$q_numerator)

      # 2. Deviation, on the rows without an event that were not lost.
      rows <- which(in_stratum & has_time & no_event & not_lost)
      dev <- .tte_ipcw_fit_cause(
        working_data[rows],
        "censor_deviation",
        "deviation",
        label,
        confounder_vars,
        tstart_var,
        use_gam
      )
      data.table::set(working_data, rows, "q_den_dev", dev$q_denominator)
      data.table::set(working_data, rows, "q_num_dev", dev$q_numerator)

      # 3. Time zero, on every person-trial of the stratum in the record.
      zero <- .tte_ipcw_fit_time_zero(tz, id_var, confounder_vars, label)
      tz_factor[[label]] <- zero$factor

      ipcw_formulas[[label]] <- list(
        loss = loss$record,
        deviation = dev$record,
        time_zero = zero$record
      )
      rm(loss, dev, zero)
      gc()
    }
    self$ipcw_formulas <- ipcw_formulas

    # The weight on the row of follow-up interval k is the probability of
    # remaining uncensored through the START of follow-up interval k, so each
    # product stops at follow-up interval k - 1. `shift()` supplies the empty
    # product of 1 on the first row of each person-trial.
    lagged_ratio <- function(q_num, q_den) {
      return(
        cumprod(data.table::shift(q_num, n = 1L, fill = 1)) /
          cumprod(data.table::shift(q_den, n = 1L, fill = 1))
      )
    }
    data.table::setorderv(working_data, c(id_var, tstart_var))
    working_data[,
      `:=`(
        ipcw_pp_loss = lagged_ratio(q_num_loss, q_den_loss),
        ipcw_pp_deviation = lagged_ratio(q_num_dev, q_den_dev)
      ),
      by = c(id_var)
    ]
    working_data[, ipcw_pp_time_zero := 1]
    tz_factor <- data.table::rbindlist(tz_factor)
    if (nrow(tz_factor) > 0L) {
      working_data[
        tz_factor,
        ipcw_pp_time_zero := i.ipcw_pp_time_zero,
        on = id_var
      ]
    }
    working_data[,
      ipcw_pp := ipcw_pp_time_zero * ipcw_pp_loss * ipcw_pp_deviation
    ]
    if (anyNA(working_data$ipcw_pp)) {
      stop(
        "s6_ipcw_pp() left ",
        sum(is.na(working_data$ipcw_pp)),
        " of ",
        nrow(working_data),
        " rows without a censoring weight.",
        call. = FALSE
      )
    }

    out_cols <- c(
      "ipcw_pp_time_zero",
      "ipcw_pp_loss",
      "ipcw_pp_deviation",
      "ipcw_pp"
    )
    old <- intersect(out_cols, names(self$data))
    if (length(old) > 0L) {
      self$data[, (old) := NULL]
    }
    # The follow-up interval, not the follow-up interval stop. A zero-width row
    # shares its stop with the row before it, so a stop alone does not name one
    # row.
    join_on <- c(id_var, tstart_var, tstop_var)
    self$data[
      working_data,
      (out_cols) := mget(paste0("i.", out_cols)),
      on = join_on
    ]

    rm(working_data)

    self$data[, analysis_weight_pp := ipw * ipcw_pp]

    self$data <- private$.truncate_weights(
      data = self$data,
      weight_cols = "analysis_weight_pp",
      lower = 0.01,
      upper = 0.99,
      suffix = "_trunc"
    )

    self$weight_cols <- unique(c(
      self$weight_cols,
      "ipcw_pp",
      "analysis_weight_pp",
      "analysis_weight_pp_trunc"
    ))
    self$steps_completed <- c(
      self$steps_completed,
      "ipcw",
      "weights",
      "truncate"
    )

    return(invisible(self))
  }
)

# --- combine_weights: multiply IPW x IPCW into a single column ----------
TTEEnrollment$set(
  "private",
  "combine_weights",
  function(
    ipw_col = "ipw",
    ipcw_col = "ipcw_pp",
    name = "analysis_weight_pp"
  ) {
    if (self$data_level != "trial") {
      stop(
        "combine_weights() requires trial level data.\n",
        "Current data_level: '",
        self$data_level,
        "'\n",
        "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level.",
        call. = FALSE
      )
    }

    if (!ipw_col %in% names(self$data)) {
      stop("ipw_col '", ipw_col, "' not found in data", call. = FALSE)
    }
    if (!ipcw_col %in% names(self$data)) {
      stop("ipcw_col '", ipcw_col, "' not found in data", call. = FALSE)
    }
    self$data[, (name) := get(ipw_col) * get(ipcw_col)]

    self$weight_cols <- unique(c(self$weight_cols, name))
    self$steps_completed <- c(self$steps_completed, "weights")
    return(invisible(self))
  }
)

# --- .truncate_weights: clip extreme weights at quantile bounds ----------
TTEEnrollment$set(
  "private",
  ".truncate_weights",
  function(
    data,
    weight_cols,
    lower = 0.01,
    upper = 0.99,
    suffix = "_trunc"
  ) {
    if (!data.table::is.data.table(data)) {
      stop("data must be a data.table", call. = FALSE)
    }
    if (!is.character(weight_cols) || length(weight_cols) == 0) {
      stop("weight_cols must be a non-empty character vector", call. = FALSE)
    }
    missing_cols <- setdiff(weight_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(
        "Columns not found in data: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
    if (
      !is.numeric(lower) ||
        !is.numeric(upper) ||
        lower < 0 ||
        upper > 1 ||
        lower >= upper
    ) {
      stop(
        "lower and upper must be numeric with 0 <= lower < upper <= 1",
        call. = FALSE
      )
    }

    for (col in weight_cols) {
      bounds <- stats::quantile(data[[col]], c(lower, upper), na.rm = TRUE)
      new_col <- paste0(col, suffix)
      data[, (new_col) := pmin(pmax(get(col), bounds[1]), bounds[2])]
    }

    return(data)
  }
)
