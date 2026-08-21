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
    invisible(self)
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
#' @param stabilize Logical, default TRUE.
TTEEnrollment$set("public", "s2_ipw", function(stabilize = TRUE) {
  if (self$data_level != "trial") {
    stop(
      "s2_ipw() requires trial level data.\n",
      "Current data_level: '",
      self$data_level,
      "'\n",
      "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level."
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
      paste(missing_confounders, collapse = ", ")
    )
  }

  # Fit on the ENTRY-WINDOW snapshot. `tstart == 0` is the LANDMARK band
  # row, so the confounder columns there hold follow-up values and not
  # baseline ones. `fit_dt` is local, and the rename inside it never
  # reaches the panel.
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
  invisible(self)
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
        "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level."
      )
    }

    if (is.null(weight_cols)) {
      weight_cols <- self$weight_cols
    }
    weight_cols <- intersect(weight_cols, names(self$data))

    if (length(weight_cols) == 0) {
      warning("No weight columns to truncate")
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
    invisible(self)
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

  invisible(self)
})

# =========================================================================
# Private weight/draw/collapse helpers
# =========================================================================
# --- s6_ipcw_pp: inverse probability of censoring weights (per-protocol) ----
#
# The censoring model is complementary log-log with a person-time offset:
#
#   cloglog{Pr(C_i = 1)} = eta_i + log(person_weeks_i)
#
# so the probability of staying uncensored over the row is
# `q_i = exp(-exp(eta_i) * person_weeks_i)`. One linear predictor then
# gives `q(4) = q(1)^4`, which is what makes a four-week band and a
# one-week band comparable. A logit link carries no such identity, so a
# clipped terminal band would take a whole band's censoring risk.
#
# The weight is LAGGED. It is the probability of remaining uncensored
# through the START of the row, so the product stops at the row before.
# The first row of every person-trial then weighs exactly 1. A censored
# band stays in the risk set (`s5_prepare_outcome()` clips it and keeps
# it), and an inclusive product would count that band's own censoring
# probability inside its own weight.
#
# The numerator is a second fitted model. It carries the same follow-up
# and calendar time terms as the denominator and drops the confounders.
# That is the stabilisation of Danaei (2013), read for a marginal outcome
# model: the numerator conditions on time and not on the confounders,
# because the outcome model carries no confounder to condition on.
#
# A stratum that cannot be estimated stops. swereg substitutes no marginal
# censoring rate for a model it could not fit.
TTEEnrollment$set(
  "private",
  "s6_ipcw_pp",
  function(
    estimate_ipcw_pp_separately_by_treatment = TRUE,
    estimate_ipcw_pp_with_gam = TRUE,
    censoring_var = NULL
  ) {
    if (self$data_level != "trial") {
      stop(
        "s6_ipcw_pp() requires trial level data.\n",
        "Current data_level: '",
        self$data_level,
        "'\n",
        "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level."
      )
    }

    if (!"ipw" %in% names(self$data)) {
      stop("s6_ipcw_pp() requires 'ipw' column. Run $s2_ipw() first.")
    }

    design <- self$design

    if (is.null(censoring_var)) {
      if ("prepare_outcome" %in% self$steps_completed) {
        censoring_var <- "censor_this_period"
      } else {
        censoring_var <- "censored"
      }
    }

    if (!censoring_var %in% names(self$data)) {
      stop(
        "censoring_var '",
        censoring_var,
        "' not found. Run $s4_prepare_for_analysis() first."
      )
    }

    working_data <- self$data[!is.na(get(design$treatment_var))]

    # --- Inline calculate_ipcw logic ---
    treatment_var <- design$treatment_var
    confounder_vars <- design$confounder_vars
    id_var <- design$id_var
    tstart_var <- design$tstart_var
    tstop_var <- design$tstop_var
    use_gam <- estimate_ipcw_pp_with_gam
    separate_by_treatment <- estimate_ipcw_pp_separately_by_treatment

    # The censoring model reads the TIME-UPDATED confounder, and never the
    # entry-window snapshot. A missing value there makes `predict()` return
    # NA, `p_uncensored` becomes NA, and `cumprod()` below carries that NA
    # through the rest of the person-trial. Stop, and name what is missing.
    #
    # swereg MUST NOT substitute the entry-window value here. That value
    # describes the recruiting week, and reading it during follow-up is the
    # confounding this design removes.
    .tte_stop_on_missing_ipcw_confounders(
      working_data,
      confounder_vars,
      id_var
    )

    if (use_gam && !requireNamespace("mgcv", quietly = TRUE)) {
      stop(
        "Package 'mgcv' is required for use_gam = TRUE. ",
        "Install it with: install.packages('mgcv')"
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
    # holds no person-time, so nothing can censor it, and its uncensoring
    # probability is exactly 1 in both models.
    working_data[,
      .ipcw_has_time := !is.na(person_weeks) & person_weeks > 0
    ]

    # The calendar-time term. `mgcv::s()` asks for 10 basis functions by
    # default and stops below 10 distinct values, so a shorter trial index
    # takes a linear term instead.
    n_trials <- if ("trial_id" %in% names(working_data)) {
      working_data[.ipcw_has_time == TRUE, data.table::uniqueN(trial_id)]
    } else {
      0L
    }
    calendar_term <- if (use_gam && n_trials >= 10L) {
      "s(trial_id)"
    } else if (n_trials > 1L) {
      "trial_id"
    } else {
      ""
    }

    # One stratum: fit the denominator and the numerator, and write the two
    # per-row uncensoring probabilities. `label` names the stratum in every
    # error message, because a stratum that stops must say which one it was.
    fit_stratum <- function(mask, label) {
      keep <- mask & working_data[[".ipcw_has_time"]]
      rows <- which(keep)
      n_rows <- length(rows)
      if (n_rows == 0L) {
        return(invisible(NULL))
      }
      fit_data <- working_data[rows]
      n_censor <- sum(fit_data[[censoring_var]], na.rm = TRUE)

      # No censoring anywhere in the stratum. Every row stays uncensored
      # with probability 1, in the numerator and in the denominator, so the
      # weight is 1. That is the exact answer and not a fallback.
      if (n_censor == 0L) {
        data.table::set(
          working_data,
          i = rows,
          j = "q_denominator",
          value = 1
        )
        data.table::set(working_data, i = rows, j = "q_numerator", value = 1)
        return(invisible(NULL))
      }
      if (n_censor == n_rows) {
        stop(
          "s6_ipcw_pp() cannot estimate the censoring model for ",
          label,
          ".\n",
          "Every one of its ",
          n_rows,
          " rows is censored, so the model has no uncensored row to ",
          "contrast them with.\n",
          "swereg substitutes no marginal censoring rate here. A weight ",
          "built from one is not the weight the analysis reports.\n",
          "Widen the stratum, or drop it from the analysis.",
          call. = FALSE
        )
      }

      n_starts <- data.table::uniqueN(fit_data[[tstart_var]])
      time_term <- .tte_ipcw_time_term(tstart_var, n_starts, use_gam)

      # `role` is "denominator" or "numerator". The two models differ only
      # in whether they carry the confounders.
      fit_one <- function(terms, role) {
        terms <- terms[nzchar(terms)]
        rhs <- if (length(terms) == 0L) {
          "1"
        } else {
          paste(terms, collapse = " + ")
        }
        model_formula <- stats::as.formula(paste0(
          censoring_var,
          " ~ ",
          rhs,
          " + offset(log(person_weeks))"
        ))
        fit <- tryCatch(
          if (use_gam) {
            mgcv::bam(
              model_formula,
              data = fit_data,
              family = stats::binomial(link = "cloglog"),
              discrete = TRUE
            )
          } else {
            stats::glm(
              model_formula,
              data = fit_data,
              family = stats::binomial(link = "cloglog")
            )
          },
          error = function(e) {
            stop(
              "s6_ipcw_pp() cannot fit the ",
              role,
              " censoring model for ",
              label,
              ".\n",
              "  formula: ",
              deparse1(model_formula),
              "\n",
              "  rows: ",
              n_rows,
              ", censored: ",
              n_censor,
              "\n",
              "  the model reported: ",
              conditionMessage(e),
              "\n",
              "swereg substitutes no marginal censoring rate here.",
              call. = FALSE
            )
          }
        )
        q <- 1 -
          as.numeric(stats::predict(
            fit,
            newdata = fit_data,
            type = "response"
          ))
        rm(fit)
        if (anyNA(q) || any(!is.finite(q)) || any(q <= 0)) {
          stop(
            "s6_ipcw_pp() fitted the ",
            role,
            " censoring model for ",
            label,
            ", and it predicts an uncensoring probability that is not ",
            "usable.\n",
            "  formula: ",
            deparse1(model_formula),
            "\n",
            "  rows: ",
            n_rows,
            ", not finite: ",
            sum(is.na(q) | !is.finite(q)),
            ", not positive: ",
            sum(!is.na(q) & is.finite(q) & q <= 0),
            "\n",
            "A weight divides by this probability, so swereg stops rather ",
            "than carry an infinite or missing weight into the analysis.",
            call. = FALSE
          )
        }
        q
      }

      data.table::set(
        working_data,
        i = rows,
        j = "q_denominator",
        value = fit_one(
          c(time_term, calendar_term, confounder_vars),
          "denominator"
        )
      )
      data.table::set(
        working_data,
        i = rows,
        j = "q_numerator",
        value = fit_one(c(time_term, calendar_term), "numerator")
      )
      rm(fit_data)
      gc()
    }

    working_data[, q_denominator := NA_real_]
    working_data[, q_numerator := NA_real_]
    if (separate_by_treatment) {
      tx_mask <- working_data[[treatment_var]] == TRUE
      fit_stratum(tx_mask, "the intervention arm")
      fit_stratum(!tx_mask, "the comparator arm")
    } else {
      fit_stratum(rep(TRUE, nrow(working_data)), "the pooled cohort")
    }
    # A zero-width row was held out of both fits. Nothing happens over an
    # empty interval, so it stays uncensored with probability 1.
    working_data[.ipcw_has_time == FALSE, q_denominator := 1]
    working_data[.ipcw_has_time == FALSE, q_numerator := 1]
    if (anyNA(working_data$q_denominator) || anyNA(working_data$q_numerator)) {
      stop(
        "s6_ipcw_pp() left ",
        sum(
          is.na(working_data$q_denominator) | is.na(working_data$q_numerator)
        ),
        " of ",
        nrow(working_data),
        " rows without an uncensoring probability.",
        call. = FALSE
      )
    }

    # The weight on the row of band k is the probability of remaining
    # uncensored through the START of band k, so the product stops at band
    # k - 1. `shift()` supplies the empty product of 1 on the first row of
    # each person-trial, which makes that row weigh exactly 1.
    data.table::setorderv(working_data, c(id_var, tstart_var))
    working_data[,
      cum_q_denominator := cumprod(
        data.table::shift(q_denominator, n = 1L, fill = 1)
      ),
      by = c(id_var)
    ]
    working_data[,
      cum_q_numerator := cumprod(
        data.table::shift(q_numerator, n = 1L, fill = 1)
      ),
      by = c(id_var)
    ]
    working_data[, ipcw_pp := cum_q_numerator / cum_q_denominator]

    if ("ipcw_pp" %in% names(self$data)) {
      self$data[, ipcw_pp := NULL]
    }
    # The band, not the band stop. A zero-width row shares its stop with the
    # row before it, so a stop alone does not name one row.
    join_on <- c(design$id_var, design$tstart_var, design$tstop_var)
    self$data[
      working_data,
      ipcw_pp := i.ipcw_pp,
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

    invisible(self)
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
        "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level."
      )
    }

    if (!ipw_col %in% names(self$data)) {
      stop("ipw_col '", ipw_col, "' not found in data")
    }
    if (!ipcw_col %in% names(self$data)) {
      stop("ipcw_col '", ipcw_col, "' not found in data")
    }
    self$data[, (name) := get(ipw_col) * get(ipcw_col)]

    self$weight_cols <- unique(c(self$weight_cols, name))
    self$steps_completed <- c(self$steps_completed, "weights")
    invisible(self)
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
      stop("data must be a data.table")
    }
    if (!is.character(weight_cols) || length(weight_cols) == 0) {
      stop("weight_cols must be a non-empty character vector")
    }
    missing_cols <- setdiff(weight_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(
        "Columns not found in data: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    if (
      !is.numeric(lower) ||
        !is.numeric(upper) ||
        lower < 0 ||
        upper > 1 ||
        lower >= upper
    ) {
      stop("lower and upper must be numeric with 0 <= lower < upper <= 1")
    }

    for (col in weight_cols) {
      bounds <- stats::quantile(data[[col]], c(lower, upper), na.rm = TRUE)
      new_col <- paste0(col, suffix)
      data[, (new_col) := pmin(pmax(get(col), bounds[1]), bounds[2])]
    }

    data
  }
)
