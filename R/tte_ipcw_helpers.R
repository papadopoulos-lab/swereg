# Helpers the censoring model uses: the missing-confounder guard, the
# time-term helper that every model shares, and the entry-window slice.

#' Stop when a time-updated confounder is missing on the IPCW fitting rows.
#'
#' `$s6_ipcw_pp()` fits censoring on the follow-up rows, so it reads the
#' time-updated confounder. An `NA` there makes `stats::predict()` return `NA`,
#' and `cumprod()` carries that `NA` through the rest of the person-trial. The
#' weight then reaches the survey fit as `NA`, far from the cause.
#'
#' swereg MUST NOT overwrite an observed follow-up value with the
#' `.tte_entry__` value. That value describes the recruiting week, and reading
#' it during follow-up is the confounding that a time-zero design removes.
#'
#' `$s1b_fill_followup_confounders()` supplies a missing follow-up value from
#' the last observed value of the same person-trial. s1d runs it before this
#' step.
#'
#' @param data The rows the censoring model fits.
#' @param confounder_vars Character vector of confounder names.
#' @param id_var Character, the person-trial identifier column.
#' @return `invisible(NULL)`, or an error.
#' @noRd
.tte_stop_on_missing_ipcw_confounders <- function(
  data,
  confounder_vars,
  id_var
) {
  cols <- intersect(confounder_vars, names(data))
  if (length(cols) == 0L || nrow(data) == 0L) {
    return(invisible(NULL))
  }
  n_missing <- vapply(cols, function(v) sum(is.na(data[[v]])), integer(1))
  if (all(n_missing == 0L)) {
    return(invisible(NULL))
  }

  ids <- data[[id_var]]
  n_trials <- data.table::uniqueN(ids)
  detail <- vapply(
    cols[n_missing > 0L],
    function(v) {
      na_rows <- is.na(data[[v]])
      return(sprintf(
        "  %s: %d of %d rows, %d of %d person-trials",
        v,
        sum(na_rows),
        nrow(data),
        data.table::uniqueN(ids[na_rows]),
        n_trials
      ))
    },
    character(1)
  )
  stop(
    "s6_ipcw_pp() cannot fit the censoring model.\n",
    "A time-updated confounder is missing on the rows it fits:\n",
    paste(detail, collapse = "\n"),
    "\nAn NA there gives an NA weight, and cumprod() carries it through the ",
    "rest of the person-trial.\n",
    "Fill those follow-up values before this step, or drop the affected ",
    "person-trials.\n",
    "$s1b_fill_followup_confounders() supplies a missing follow-up value from ",
    "the last observed value of the same person-trial. s1d runs it before ",
    "this step.\n",
    "swereg MUST NOT overwrite an observed follow-up value with the ",
    "entry-window value. That value describes the recruiting week.",
    call. = FALSE
  )
}

#' Name the time term of one model.
#'
#' Every time term of the outcome, heterogeneity and censoring models comes
#' from this one function. The ladder steps down as the fit sees fewer
#' distinct values. `mgcv::s()` asks for 10 basis functions by default, and it
#' stops when the covariate holds fewer than 10 distinct values. A natural
#' cubic spline of 3 degrees of freedom needs 4. A factor needs 2.
#'
#' `s()` fits only inside `mgcv::bam()`, so every `survey::svyglm()` and
#' `stats::glm()` fit passes `gam = FALSE`.
#'
#' @param var Character, the column the term reads.
#' @param n Integer, the number of distinct values of `var` in the rows that
#'   the model is fitted to.
#' @param gam Logical. `TRUE` asks for a penalised spline.
#' @return A character scalar. It is `""` when fewer than 2 distinct values
#'   leave nothing to fit.
#' @noRd
.tte_time_term <- function(var, n, gam) {
  if (gam && n >= 10L) {
    return(paste0("s(", var, ")"))
  }
  if (n >= 4L) {
    return(paste0("splines::ns(", var, ", df = 3)"))
  }
  if (n >= 2L) {
    return(paste0("factor(", var, ")"))
  }
  return("")
}

#' Read the confounders of a baseline slice at the entry window.
#'
#' The returned table names each confounder exactly as the design does, and
#' holds its entry-window value under that name. Every step that fits or
#' tabulates baseline confounders MUST read the panel through this function.
#'
#' The rename is local to the returned table. The panel keeps the follow-up
#' value under the confounder name, and the entry-window value under the
#' `.tte_entry__` name.
#'
#' @param data A data.table, one row per person-trial.
#' @param confounder_vars Character vector of confounder names.
#' @param keep_cols Character vector of other columns to carry, such as the
#'   identifier, the treatment column and a weight column.
#' @return A new data.table. It shares no column with `data`.
#' @noRd
.tte_entry_view <- function(data, confounder_vars, keep_cols = character(0)) {
  use_entry <- .tte_has_entry_snapshot(data, confounder_vars)
  conf <- intersect(confounder_vars, names(data))
  entry <- .tte_entry_col(conf)
  cols <- unique(c(keep_cols, conf, if (use_entry) entry))
  out <- data.table::copy(data[, intersect(cols, names(data)), with = FALSE])
  if (use_entry) {
    for (i in seq_along(conf)) {
      data.table::set(out, j = conf[i], value = out[[entry[i]]])
    }
  }
  return(out)
}

#' Record whether per-protocol follow-up stops at time zero.
#'
#' Under the left-edge rule, a person-trial that is discordant in its first
#' follow-up week stops at time zero. It keeps no row, so the row-level
#' censoring models cannot see it. This record keeps it for the time-zero model
#' of `s6_ipcw_pp()`.
#'
#' The record holds one row for each person-trial with a known arm that is
#' under follow-up at time zero apart from the deviation. Its outcome, its
#' record end and its planned end all fall after time zero. A person-trial
#' whose follow-up is empty for one of those reasons cannot be seen to deviate,
#' so the record leaves it out.
#'
#' @param data The panel inside `s5_prepare_outcome()`. The tie rule has set
#'   `.deviation_clip`, and the rows after the stop are still present.
#' @param design The [TTEDesign].
#' @param estimand `"pp"` or `"itt"`.
#' @return `NULL` for `"itt"`. Otherwise a data.table with the identifier, the
#'   arm, each confounder at the recruiting week under its design name, and
#'   `deviation_time_zero`. That column is 1 when the deviation stops follow-up
#'   at time zero, and 0 otherwise.
#' @noRd
.tte_time_zero_record <- function(data, design, estimand) {
  if (!identical(estimand, "pp")) {
    return(NULL)
  }
  id_var <- design$id_var
  tstart_var <- design$tstart_var
  treatment_var <- design$treatment_var
  # A grouped lookup. Base `order()` on a character identifier does not use
  # the radix sort, and it took 69 s against 5.9 s for 20 million rows.
  first <- data[, .I[which.min(get(tstart_var))], by = c(id_var)][["V1"]]
  base <- data[first]
  start <- base[[tstart_var]]
  other_stop <- pmin(
    base[["weeks_to_event"]],
    base[[".planned_end"]],
    base[[".record_end"]],
    na.rm = TRUE
  )
  at_risk <- !is.na(base[[treatment_var]]) &
    (is.na(other_stop) | other_stop > start)
  clip <- base[[".deviation_clip"]][at_risk]
  tz <- .tte_entry_view(
    base[at_risk],
    design$confounder_vars,
    keep_cols = c(id_var, treatment_var)
  )
  data.table::set(
    tz,
    j = "deviation_time_zero",
    value = as.integer(!is.na(clip) & clip <= start[at_risk])
  )
  keep <- c(
    id_var,
    treatment_var,
    intersect(design$confounder_vars, names(tz)),
    "deviation_time_zero"
  )
  return(tz[, keep, with = FALSE])
}

#' Fit one row-level censoring model and predict its uncensoring probability.
#'
#' The model is complementary log-log with a person-time offset. It predicts on
#' the rows it was fitted to.
#'
#' @param fit_data The rows of the risk set.
#' @param cause_col The censoring indicator of the cause.
#' @param terms Character vector of right-hand-side terms. Empty strings are
#'   dropped.
#' @param role `"denominator"` or `"numerator"`.
#' @param cause `"loss"` or `"deviation"`.
#' @param label The stratum label.
#' @param use_gam Logical. `TRUE` fits with `mgcv::bam()`.
#' @return `list(q = , formula = )`.
#' @noRd
.tte_ipcw_fit_one <- function(
  fit_data,
  cause_col,
  terms,
  role,
  cause,
  label,
  use_gam
) {
  terms <- terms[nzchar(terms)]
  rhs <- if (length(terms) == 0L) "1" else paste(terms, collapse = " + ")
  # The namespace is the environment, so the stored formula holds no
  # reference to the panel.
  model_formula <- stats::as.formula(
    paste0(cause_col, " ~ ", rhs, " + offset(log(person_weeks))"),
    env = topenv()
  )
  what <- paste0("the ", cause, " ", role, " model for ", label)
  counts <- paste0(
    "  rows: ",
    nrow(fit_data),
    ", censored by ",
    cause,
    ": ",
    sum(fit_data[[cause_col]]),
    "\n"
  )
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
        "s6_ipcw_pp() cannot fit ",
        what,
        ".\n",
        "  formula: ",
        deparse1(model_formula),
        "\n",
        counts,
        "  the model reported: ",
        conditionMessage(e),
        "\n",
        "swereg substitutes no marginal censoring rate here.",
        call. = FALSE
      )
    }
  )
  q <- 1 -
    as.numeric(stats::predict(fit, newdata = fit_data, type = "response"))
  if (anyNA(q) || any(!is.finite(q)) || any(q <= 0)) {
    stop(
      "s6_ipcw_pp() fitted ",
      what,
      ", and it predicts an uncensoring probability that is not usable.\n",
      "  formula: ",
      deparse1(model_formula),
      "\n",
      counts,
      "  not finite: ",
      sum(is.na(q) | !is.finite(q)),
      ", not positive: ",
      sum(!is.na(q) & is.finite(q) & q <= 0),
      "\n",
      "A weight divides by this probability, so swereg stops rather than ",
      "carry an infinite or missing weight into the analysis.",
      call. = FALSE
    )
  }
  return(list(q = q, formula = model_formula))
}

#' Fit the censoring model of one cause in one stratum.
#'
#' The caller passes the risk set of the cause. A risk set with no censoring by
#' the cause fits no model, and every row stays uncensored by it with
#' probability 1. A risk set in which every row is censored stops.
#'
#' Each time term counts the distinct values of the risk set. A panel without
#' `period_id` takes no calendar term.
#'
#' @param fit_data The rows of the risk set.
#' @param cause_col The censoring indicator of the cause.
#' @param cause `"loss"` or `"deviation"`.
#' @param label The stratum label.
#' @param confounder_vars Character vector of confounder names.
#' @param tstart_var The column of the interval start.
#' @param use_gam Logical. `TRUE` fits with `mgcv::bam()`.
#' @return `list(q_denominator = , q_numerator = , record = )`. `record` is
#'   `list(fitted = TRUE, denominator = , numerator = )` for a fitted cause,
#'   and `list(fitted = FALSE, reason = )` otherwise.
#' @noRd
.tte_ipcw_fit_cause <- function(
  fit_data,
  cause_col,
  cause,
  label,
  confounder_vars,
  tstart_var,
  use_gam
) {
  n_rows <- nrow(fit_data)
  n_censor <- sum(fit_data[[cause_col]])
  if (n_censor == 0L) {
    reason <- if (n_rows == 0L) {
      paste0(label, " has no follow-up row at risk of ", cause, ".")
    } else {
      paste0(
        "No row of ",
        label,
        " is censored by ",
        cause,
        ", so every row stays uncensored by it with probability 1."
      )
    }
    return(list(
      q_denominator = rep(1, n_rows),
      q_numerator = rep(1, n_rows),
      record = list(fitted = FALSE, reason = reason)
    ))
  }
  if (n_censor == n_rows) {
    stop(
      "s6_ipcw_pp() cannot estimate the ",
      cause,
      " censoring model for ",
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
  time_term <- .tte_time_term(
    tstart_var,
    data.table::uniqueN(fit_data[[tstart_var]]),
    use_gam
  )
  period_term <- ""
  if ("period_id" %in% names(fit_data)) {
    period_term <- .tte_time_term(
      "period_id",
      data.table::uniqueN(fit_data[["period_id"]]),
      use_gam
    )
  }
  den <- .tte_ipcw_fit_one(
    fit_data,
    cause_col,
    c(time_term, period_term, confounder_vars),
    "denominator",
    cause,
    label,
    use_gam
  )
  num <- .tte_ipcw_fit_one(
    fit_data,
    cause_col,
    time_term,
    "numerator",
    cause,
    label,
    use_gam
  )
  return(list(
    q_denominator = den$q,
    q_numerator = num$q,
    record = list(
      fitted = TRUE,
      denominator = den$formula,
      numerator = num$formula
    )
  ))
}

#' Fit the time-zero deviation model of one stratum.
#'
#' A logistic model of `deviation_time_zero` on the confounders at the
#' recruiting week, fitted on every person-trial of the stratum in the
#' time-zero record. The numerator is the marginal proportion of the stratum,
#' which is the fitted value of the intercept-only logistic model.
#'
#' A stratum with no deviation at time zero fits no model. A stratum in which
#' every person-trial deviates at time zero fits no model either, and it does
#' not stop. It has no follow-up row, so no row needs the factor.
#'
#' @param tz The rows of the time-zero record for the stratum.
#' @param id_var The person-trial identifier.
#' @param confounder_vars Character vector of confounder names.
#' @param label The stratum label.
#' @return `list(factor = , record = )`. `factor` is `NULL` or a data.table of
#'   the identifier and `ipcw_pp_time_zero`, which is
#'   `(1 - p_bar) / (1 - p0)`.
#' @noRd
.tte_ipcw_fit_time_zero <- function(tz, id_var, confounder_vars, label) {
  n <- nrow(tz)
  n_dev <- sum(tz[["deviation_time_zero"]])
  if (n_dev == 0L || n_dev == n) {
    reason <- if (n == 0L) {
      paste0(label, " has no person-trial under follow-up at time zero.")
    } else if (n_dev == 0L) {
      paste0(
        "No person-trial of ",
        label,
        " deviates at time zero, so the time-zero factor is 1."
      )
    } else {
      paste0(
        "Every one of the ",
        n,
        " person-trials of ",
        label,
        " deviates at time zero, so ",
        label,
        " has no follow-up row to weight."
      )
    }
    return(list(factor = NULL, record = list(fitted = FALSE, reason = reason)))
  }
  rhs <- if (length(confounder_vars) == 0L) {
    "1"
  } else {
    paste(confounder_vars, collapse = " + ")
  }
  den_formula <- stats::as.formula(
    paste0("deviation_time_zero ~ ", rhs),
    env = topenv()
  )
  num_formula <- stats::as.formula("deviation_time_zero ~ 1", env = topenv())
  fit <- tryCatch(
    stats::glm(den_formula, data = tz, family = stats::binomial()),
    error = function(e) {
      stop(
        "s6_ipcw_pp() cannot fit the time-zero deviation model for ",
        label,
        ".\n",
        "  formula: ",
        deparse1(den_formula),
        "\n",
        "  person-trials: ",
        n,
        ", deviating at time zero: ",
        n_dev,
        "\n",
        "  the model reported: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  p0 <- as.numeric(stats::predict(fit, newdata = tz, type = "response"))
  if (anyNA(p0) || any(p0 >= 1)) {
    stop(
      "s6_ipcw_pp() fitted the time-zero deviation model for ",
      label,
      ", and it predicts a probability that is not usable.\n",
      "  formula: ",
      deparse1(den_formula),
      "\n",
      "  person-trials: ",
      n,
      ", missing: ",
      sum(is.na(p0)),
      ", equal to 1: ",
      sum(!is.na(p0) & p0 >= 1),
      "\n",
      "A weight divides by one minus this probability, so swereg stops.",
      call. = FALSE
    )
  }
  p_bar <- n_dev / n
  out <- data.table::data.table(
    id = tz[[id_var]],
    ipcw_pp_time_zero = (1 - p_bar) / (1 - p0)
  )
  data.table::setnames(out, "id", id_var)
  return(list(
    factor = out,
    record = list(
      fitted = TRUE,
      denominator = den_formula,
      numerator = num_formula
    )
  ))
}

#' Stop on a censoring indicator the per-protocol weights cannot use.
#'
#' The per-protocol weights fit one model per cause. They read `censor_loss`
#' and `censor_deviation`, which `s5_prepare_outcome()` writes, so one custom
#' indicator for both causes cannot select them.
#'
#' @param censoring_var `NULL` or `"censor_this_period"`.
#' @return `invisible(NULL)`, or an error.
#' @noRd
.tte_check_censoring_var <- function(censoring_var) {
  if (is.null(censoring_var) || identical(censoring_var, "censor_this_period")) {
    return(invisible(NULL))
  }
  stop(
    "censoring_var = '",
    censoring_var,
    "' is not supported. The per-protocol censoring weights fit one model ",
    "per cause, and they read censor_loss and censor_deviation, which ",
    "s5_prepare_outcome() writes.",
    call. = FALSE
  )
}
