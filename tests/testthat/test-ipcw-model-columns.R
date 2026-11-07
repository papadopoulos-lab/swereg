# `$s6_ipcw_pp()` copies the panel into `working_data` and passes row subsets
# of it to each censoring fit. This file pins two things:
#
# 1. `working_data` holds only the columns that step reads. The data that
#    reaches `mgcv::bam()` holds the cause column, `person_weeks`, `tstart`,
#    `period_id`, the confounders, the treatment, `event`, `censor_loss` and
#    the identifier, and nothing else.
# 2. The weights are `identical()` to those of the 27.3.6 code, on a panel
#    that carries `period_id` and two columns that no step reads.
#
# The 27.3.6 code is a verbatim copy, below, of `s6_ipcw_pp()` from
# `R/r6_tteenrollment_weighting.R` and of `.tte_ipcw_fit_one()` and
# `.tte_ipcw_fit_cause()` from `R/tte_ipcw_helpers.R`, at commit 973950fc.
# Those two files are the same at 40f128dd, which is 27.3.7. The test installs
# the copy as the private method of one enrollment, so both enrollments run
# the same `$s4_prepare_for_analysis()`.

skip_if_not_installed("data.table")
skip_if_not_installed("mgcv")

# --- the panel ----------------------------------------------------------------
#
# 300 person-trials, half in each arm, with up to 16 weekly rows. The
# confounder `x` changes over follow-up. Deviation, loss and the outcome are
# random, and deviation and loss depend on the value of `x` at time zero. A
# person-trial that deviates on its first row deviates at time zero. Each
# person-trial carries one `period_id` from 0 to 11, so `s(period_id)` fits.
# `extra_num` and `extra_chr` are columns that no step of s6 reads.
.imc_panel <- function(period_id = TRUE, n = 300L, k = 16L, seed = 7L) {
  set.seed(seed)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    arm <- i <= n / 2
    x0 <- stats::rbinom(1, 1, 0.5)
    x <- x0 + cumsum(stats::rbinom(k, 1, 0.05))
    on_tx <- rep(arm, k)
    dev_at <- which(stats::runif(k) < 0.03 + 0.04 * x0)[1]
    if (!is.na(dev_at)) {
      on_tx[dev_at:k] <- !arm
    }
    d <- data.table::data.table(
      enrollment_person_trial_id = sprintf("p%04d", i),
      tstart = 0:(k - 1L),
      tstop = 1:k,
      exposed = arm,
      on_tx = on_tx,
      died = stats::runif(k) < 0.01,
      x = as.numeric(x),
      period_id = sample(0:11, 1),
      extra_num = stats::rnorm(k),
      extra_chr = sample(letters, k, replace = TRUE)
    )
    lost_at <- which(stats::runif(k) < 0.02 + 0.03 * x0)[1]
    if (!is.na(lost_at)) {
      d <- d[seq_len(lost_at)]
    }
    out[[i]] <- d
  }
  d <- data.table::rbindlist(out)
  if (!period_id) {
    d[, period_id := NULL]
  }
  return(d)
}

.imc_design <- function() {
  return(TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    outcome_vars = "died",
    confounder_vars = "x",
    follow_up_time = 16L
  ))
}

# The columns `working_data` holds, and the columns each fit receives.
.imc_working_cols <- c(
  "enrollment_person_trial_id",
  "tstart",
  "tstop",
  "exposed",
  "event",
  "censor_loss",
  "censor_deviation",
  "x",
  "person_weeks",
  "period_id"
)
.imc_fit_cols <- local({
  shared <- c(
    "person_weeks",
    "tstart",
    "period_id",
    "x",
    "exposed",
    "event",
    "censor_loss",
    "enrollment_person_trial_id"
  )
  list(
    censor_loss = shared,
    censor_deviation = c("censor_deviation", shared)
  )
})

# --- the 27.3.6 code ------------------------------------------------------------
#
# The copies live in an environment whose parent is the enclosing environment
# of the enrollment, so `self` and `private` resolve to that enrollment. Every
# other helper resolves to the package namespace. This change edits none of
# those helpers.
.imc_install_27_3_6 <- function(trial) {
  e <- new.env(parent = trial$.__enclos_env__)
  e$.tte_ipcw_fit_one <- function(
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
    fit_with <- function(discrete) {
      return(tryCatch(
        if (use_gam) {
          mgcv::bam(
            model_formula,
            data = fit_data,
            family = stats::binomial(link = "cloglog"),
            discrete = discrete
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
      ))
    }
    predict_q <- function(fit) {
      return(
        1 -
          as.numeric(stats::predict(fit, newdata = fit_data, type = "response"))
      )
    }
    fit_used <- if (use_gam) "bam, discrete = TRUE" else "glm"
    fit <- fit_with(discrete = TRUE)
    # mgcv 1.9.4 `predict.bam()` on a `discrete = TRUE` fit stops with
    # "object '<var>' not found" when `s()` sits beside a parametric term that
    # transforms a variable, such as `factor(g)` or `splines::ns(tt, df = 3)`.
    # The same formula fitted with `discrete = FALSE` predicts. So swereg fits
    # it again that way, and records which fit ran.
    q <- tryCatch(predict_q(fit), error = function(e) {
      if (!use_gam || !grepl("^object '.+' not found$", conditionMessage(e))) {
        stop(e)
      }
      return(NULL)
    })
    if (is.null(q)) {
      fit_used <- paste0(
        "bam, discrete = FALSE, after the discrete prediction failed ",
        "with an object-not-found error"
      )
      fit <- fit_with(discrete = FALSE)
      q <- predict_q(fit)
    }
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
    return(list(q = q, formula = model_formula, fit = fit_used))
  }
  e$.tte_ipcw_fit_cause <- function(
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
  e$s6_ipcw_pp <- function(
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
    # from the last observed value of the same person-trial, and s1e runs it
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
  for (nm in c(".tte_ipcw_fit_one", ".tte_ipcw_fit_cause", "s6_ipcw_pp")) {
    environment(e[[nm]]) <- e
  }
  priv <- trial$.__enclos_env__$private
  unlockBinding("s6_ipcw_pp", priv)
  assign("s6_ipcw_pp", e$s6_ipcw_pp, envir = priv)
  lockBinding("s6_ipcw_pp", priv)
  return(invisible(trial))
}

# --- running s4 -------------------------------------------------------------------

.imc_weight_cols <- c(
  "ipcw_pp_time_zero",
  "ipcw_pp_loss",
  "ipcw_pp_deviation",
  "ipcw_pp",
  "analysis_weight_pp",
  "analysis_weight_pp_trunc"
)

# Build one enrollment from the panel, optionally with the 27.3.6 method, and
# run `$s2_ipw()` and `$s4_prepare_for_analysis()`. The GAM fits on this small
# panel can warn, and the warning is about the fixture.
.imc_run <- function(d, old = FALSE, gam = TRUE, by_arm = TRUE) {
  trial <- TTEEnrollment$new(data.table::copy(d), .imc_design())
  if (old) {
    .imc_install_27_3_6(trial)
  }
  suppressWarnings({
    trial$s2_ipw()
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = 16L,
      estimand = "pp",
      estimate_ipcw_pp_separately_by_treatment = by_arm,
      estimate_ipcw_pp_with_gam = gam
    )
  })
  return(trial)
}

# The weights of an enrollment, one row per follow-up interval, in key order.
.imc_weights <- function(trial) {
  w <- trial$data[,
    c("enrollment_person_trial_id", "tstart", "tstop", .imc_weight_cols),
    with = FALSE
  ]
  data.table::setorderv(w, c("enrollment_person_trial_id", "tstart", "tstop"))
  return(as.data.frame(w))
}

# Run `fn` with `mgcv::bam()`, `stats::glm()` and
# `.tte_stop_on_missing_ipcw_confounders()` traced. The trace records the
# columns of each censoring fit by the left-hand side of its formula, and the
# columns of `working_data`. Each record is a copy, because `:=` later
# extends the names vector of `working_data` in place.
.imc_capture <- function(fn) {
  cap <- new.env(parent = emptyenv())
  cap$fits <- list()
  cap$working <- list()
  fit_tracer <- bquote({
    .imc_lhs <- deparse(formula[[2L]])
    if (.imc_lhs %in% c("censor_loss", "censor_deviation")) {
      assign(
        "fits",
        c(
          get("fits", envir = .(cap)),
          list(list(lhs = .imc_lhs, cols = data.table::copy(names(data))))
        ),
        envir = .(cap)
      )
    }
  })
  suppressMessages({
    trace(
      "bam",
      where = asNamespace("mgcv"),
      print = FALSE,
      tracer = fit_tracer
    )
    trace(
      "glm",
      where = asNamespace("stats"),
      print = FALSE,
      tracer = fit_tracer
    )
    trace(
      ".tte_stop_on_missing_ipcw_confounders",
      where = asNamespace("swereg"),
      print = FALSE,
      tracer = bquote(assign(
        "working",
        c(get("working", envir = .(cap)), list(data.table::copy(names(data)))),
        envir = .(cap)
      ))
    )
  })
  on.exit(suppressMessages({
    untrace("bam", where = asNamespace("mgcv"))
    untrace("glm", where = asNamespace("stats"))
    untrace(
      ".tte_stop_on_missing_ipcw_confounders",
      where = asNamespace("swereg")
    )
  }))
  fn()
  return(cap)
}

# --- the tests --------------------------------------------------------------------

test_that("s6 keeps only the model columns in working_data and in each bam fit", {
  d <- .imc_panel()
  cap <- .imc_capture(function() .imc_run(d, gam = TRUE, by_arm = TRUE))

  # One working_data, and four fits per arm: loss and deviation, each a
  # denominator and a numerator.
  expect_length(cap$working, 1L)
  expect_identical(sort(cap$working[[1L]]), sort(.imc_working_cols))
  expect_length(cap$fits, 8L)
  lhs <- vapply(cap$fits, `[[`, character(1), "lhs")
  expect_identical(
    as.vector(table(lhs)[c("censor_loss", "censor_deviation")]),
    c(4L, 4L)
  )
  for (f in cap$fits) {
    expect_identical(sort(f$cols), sort(.imc_fit_cols[[f$lhs]]), label = f$lhs)
  }
})

test_that("s6 weights are identical() to 27.3.6 on a panel with period_id and extra columns", {
  d <- .imc_panel()
  expect_true(all(c("period_id", "extra_num", "extra_chr") %in% names(d)))
  for (opt in list(
    c(gam = TRUE, by_arm = TRUE),
    c(gam = FALSE, by_arm = FALSE)
  )) {
    new <- .imc_run(d, gam = opt[["gam"]], by_arm = opt[["by_arm"]])
    old <- .imc_run(d, old = TRUE, gam = opt[["gam"]], by_arm = opt[["by_arm"]])
    # The reference fits a period term, so a fit without `period_id` cannot
    # give the same weights.
    den <- deparse1(old$ipcw_formulas[[1L]]$loss$denominator)
    expect_match(den, "period_id", fixed = TRUE)
    expect_identical(.imc_weights(new), .imc_weights(old))
    expect_identical(new$ipcw_formulas, old$ipcw_formulas)
  }
})

test_that("s6 passes no period_id to a fit when the panel has none", {
  d <- .imc_panel(period_id = FALSE)
  cap <- .imc_capture(function() .imc_run(d, gam = FALSE, by_arm = TRUE))
  expect_identical(
    sort(cap$working[[1L]]),
    sort(setdiff(.imc_working_cols, "period_id"))
  )
  expect_length(cap$fits, 8L)
  for (f in cap$fits) {
    expect_identical(
      sort(f$cols),
      sort(setdiff(.imc_fit_cols[[f$lhs]], "period_id")),
      label = f$lhs
    )
  }
  new <- .imc_run(d, gam = FALSE, by_arm = TRUE)
  old <- .imc_run(d, old = TRUE, gam = FALSE, by_arm = TRUE)
  expect_identical(.imc_weights(new), .imc_weights(old))
})
