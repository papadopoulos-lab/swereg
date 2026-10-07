# The time terms of every model, read off the formula each fit received.
#
# Three time axes exist on the analysis panel. `period_id` is the calendar
# period of the row, `enrollment_period_id` is the trial, and `tstart` is the
# time since time zero at the interval start. Each model reads two of them:
#
#   treatment weights   exposed ~ confounders                   (no time term)
#   IPCW denominator    flex(tstart) + flex(period_id) + confounders
#   IPCW numerator      flex(tstart)
#   IRR                 exposed + flex(tstart) + flex(enrollment_period_id)
#   effect modification exposed * factor(sg) + flex(tstart)
#                         + flex(enrollment_period_id)
#   heterogeneity       exposed * ns(enrollment_period_id, df = min(3, n - 1))
#                         + flex(tstart)
#
# `flex()` is `.tte_time_term()`. Every model keeps the person-time offset.
#
# The tests read `attr(, "model_formula")` and `$ipcw_formulas`, which hold the
# formula object the fitter received. They never rebuild the formula string.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")
skip_if_not_installed("survey")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")

.tt_pw <- 4L
.tt_n_fu <- 24L
.tt_offset <- "offset(log(person_weeks))"
.tt_ns_tstart <- "splines::ns(tstart, df = 3)"
.tt_ns_trial <- "splines::ns(enrollment_period_id, df = 3)"
.tt_ns_period <- "splines::ns(period_id, df = 3)"

# Consecutive ISO year-weeks, starting on an enrollment period boundary.
.tt_weeks <- function(n_weeks) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% .tt_pw == 0L
  ]$idx[1]
  return(wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)])
}

# A person-week skeleton. Each person is eligible in the four weeks of one
# enrollment period, so each enrollment period holds one trial with both arms.
# About a third of the people switch arm during follow-up, which censors them
# under the per-protocol estimand in both arms. An outcome falls in the second
# or third week of a follow-up interval, so `s5_prepare_outcome()` clips the
# event row short of the interval end.
.tt_skeleton <- function(
  n_periods,
  n_fu = .tt_n_fu,
  n_tx = 10L,
  n_cmp = 12L,
  seed = 11L
) {
  set.seed(seed)
  weeks <- .tt_weeks(.tt_pw * n_periods + n_fu)
  n <- length(weeks)
  out <- list()
  for (k in seq_len(n_periods) - 1L) {
    for (arm in c(TRUE, FALSE)) {
      for (i in seq_len(if (arm) n_tx else n_cmp)) {
        id <- sprintf("P%02d%s%02d", k, if (arm) "T" else "C", i)
        fu <- seq_len(n) - .tt_pw * (k + 1L)
        on_tx <- rep(arm, n)
        if (stats::runif(1) < 0.35) {
          on_tx[fu >= sample(2:(n_fu - 2L), 1)] <- !arm
        }
        died <- rep(FALSE, n)
        if (stats::runif(1) < 0.3) {
          died[fu == sample(c(2, 3, 6, 7, 10, 11, 14, 15, 18, 19), 1)] <- TRUE
        }
        out[[id]] <- data.table::data.table(
          id = id,
          isoyearweek = weeks,
          exposed = arm,
          eligible = fu <= 0L & fu > -.tt_pw,
          died = died,
          on_tx = on_tx,
          age = 40 + sample(0:30, 1) + seq_len(n) / 52,
          grp = i %% 2L
        )
      }
    }
  }
  return(data.table::rbindlist(out))
}

.tt_design <- function(n_fu = .tt_n_fu) {
  return(TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = n_fu,
    period_width = .tt_pw,
    intervention_tolerance_weeks = 0L,
    comparator_tolerance_weeks = 0L
  ))
}

# The production route: enroll, treatment weights, per-protocol preparation
# with censoring weights by arm. A ratio of 2 draws every comparator. The
# fixture is small, so the models warn about it, and the warnings are about
# the toy data.
.tt_enroll <- function(n_periods, use_gam = FALSE, n_fu = .tt_n_fu) {
  trial <- TTEEnrollment$new(
    data = .tt_skeleton(n_periods, n_fu),
    design = .tt_design(n_fu),
    ratio = 2,
    seed = 4,
    extra_cols = "grp"
  )
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = n_fu,
      estimand = "pp",
      estimate_ipcw_pp_with_gam = use_gam,
      estimate_ipcw_pp_separately_by_treatment = TRUE
    )
  })
  return(trial)
}

.tt_cache <- new.env(parent = emptyenv())
.tt_six <- function() {
  if (is.null(.tt_cache$six)) {
    .tt_cache$six <- .tt_enroll(6L)
  }
  return(.tt_cache$six)
}

.tt_labels <- function(f) {
  return(attr(stats::terms(f), "term.labels"))
}
.tt_offsets <- function(f) {
  tt <- stats::terms(f)
  return(as.character(attr(tt, "variables"))[-1L][attr(tt, "offset")])
}

.tt_weight <- "analysis_weight_pp_trunc"


test_that(".tte_time_term() steps down the ladder at 10, 4, 2 and 1", {
  expect_identical(.tte_time_term("x", 10L, gam = TRUE), "s(x)")
  expect_identical(.tte_time_term("x", 9L, gam = TRUE), "splines::ns(x, df = 3)")
  expect_identical(.tte_time_term("x", 4L, gam = TRUE), "splines::ns(x, df = 3)")
  expect_identical(.tte_time_term("x", 3L, gam = TRUE), "factor(x)")
  expect_identical(.tte_time_term("x", 2L, gam = TRUE), "factor(x)")
  expect_identical(.tte_time_term("x", 1L, gam = TRUE), "")

  expect_identical(.tte_time_term("x", 10L, gam = FALSE), "splines::ns(x, df = 3)")
  expect_identical(.tte_time_term("x", 4L, gam = FALSE), "splines::ns(x, df = 3)")
  expect_identical(.tte_time_term("x", 3L, gam = FALSE), "factor(x)")
  expect_identical(.tte_time_term("x", 2L, gam = FALSE), "factor(x)")
  expect_identical(.tte_time_term("x", 1L, gam = FALSE), "")
})


test_that("the fixture has six trials, both arms in each, and a clipped event row", {
  d <- .tt_six()$data
  per_trial <- d[, list(n_arms = data.table::uniqueN(exposed)), by = enrollment_period_id]
  expect_gte(nrow(per_trial), 6L)
  expect_true(all(per_trial$n_arms == 2L))
  # An event row that stops before the end of its follow-up interval.
  expect_true(any(d$event == 1L & d$person_weeks < .tt_pw))
  # `period_id`, `enrollment_period_id` and `tstart` are three distinct axes.
  expect_true(all(d$period_id == d$enrollment_period_id + d$tstart / .tt_pw + 1L))
})


test_that("the treatment-weight model carries no time term", {
  captured <- new.env(parent = emptyenv())
  captured$f <- list()
  suppressMessages(trace(
    "glm",
    where = asNamespace("stats"),
    tracer = bquote(
      assign("f", c(get("f", envir = .(captured)), list(formula)), envir = .(captured))
    ),
    print = FALSE
  ))
  withr::defer(suppressMessages(untrace("glm", where = asNamespace("stats"))))

  trial <- TTEEnrollment$new(
    data = .tt_skeleton(6L),
    design = .tt_design(),
    ratio = 2,
    seed = 4
  )
  suppressWarnings(trial$s2_ipw(stabilize = TRUE))
  f <- captured$f
  expect_length(f, 1L)
  expect_identical(deparse1(f[[1]]), "exposed ~ age")
})


test_that("each censoring stratum: tstart and period_id in the denominator, tstart only in the numerator", {
  forms <- .tt_six()$ipcw_formulas
  expect_setequal(names(forms), c("the intervention arm", "the comparator arm"))
  for (label in names(forms)) {
    den <- forms[[label]]$denominator
    num <- forms[[label]]$numerator
    expect_identical(.tt_labels(den), c(.tt_ns_tstart, .tt_ns_period, "age"), label = label)
    expect_identical(.tt_labels(num), .tt_ns_tstart, label = label)
    expect_identical(.tt_offsets(den), .tt_offset)
    expect_identical(.tt_offsets(num), .tt_offset)
    expect_false("period_id" %in% all.vars(num))
    expect_false("enrollment_period_id" %in% all.vars(den))
    expect_false("tstop" %in% all.vars(den))
  }
  expect_true(all(is.finite(.tt_six()$data$ipcw_pp)))
})


# The penalised model takes `s()` for a term with 10 or more distinct values.
# A follow-up of 40 weeks gives 10 interval starts, and six trials then give 15
# calendar periods.
#
# This fixture puts `s()` on both time terms. mgcv 1.9.4 `predict.bam()` with
# `discrete = TRUE` stops when a smooth sits beside a parametric term that
# transforms a variable, such as `splines::ns()` or `factor()`. That limit is
# not a property of the time terms, and this test does not cover it.
test_that("the penalised censoring model uses s() for 10 or more distinct values", {
  skip_if_not_installed("mgcv")
  trial <- .tt_enroll(6L, use_gam = TRUE, n_fu = 40L)
  expect_setequal(
    names(trial$ipcw_formulas),
    c("the intervention arm", "the comparator arm")
  )
  for (label in names(trial$ipcw_formulas)) {
    expect_identical(
      .tt_labels(trial$ipcw_formulas[[label]]$denominator),
      c("s(tstart)", "s(period_id)", "age"),
      label = label
    )
    expect_identical(
      .tt_labels(trial$ipcw_formulas[[label]]$numerator),
      "s(tstart)",
      label = label
    )
  }
  expect_true(all(is.finite(trial$data$ipcw_pp)))
})


test_that("the IRR model reads tstart and the trial, not period_id or tstop", {
  trial <- .tt_six()
  r <- trial$irr(.tt_weight)
  f <- attr(r, "model_formula")
  expect_s3_class(f, "formula")
  expect_identical(.tt_labels(f), c("exposed", .tt_ns_tstart, .tt_ns_trial))
  expect_identical(.tt_offsets(f), .tt_offset)
  expect_false("period_id" %in% all.vars(f))
  expect_false("tstop" %in% all.vars(f))
  expect_true(is.finite(log(r$IRR)))

  # The recorded formula reproduces the estimate, so it is the formula the
  # fit received.
  refit <- survey::svyglm(
    f,
    design = survey::svydesign(
      ids = ~id,
      weights = stats::as.formula(paste0("~", .tt_weight)),
      data = trial$data
    ),
    family = stats::quasipoisson()
  )
  expect_equal(unname(exp(stats::coef(refit)["exposedTRUE"])), r$IRR, tolerance = 1e-10)
})


test_that("the effect-modification model reads tstart and the trial", {
  emt <- .tt_six()$effect_modification_test(.tt_weight, "grp")
  f <- attr(emt, "model_formula")
  expect_identical(
    .tt_labels(f),
    c("exposed", "factor(grp)", .tt_ns_tstart, .tt_ns_trial, "exposed:factor(grp)")
  )
  expect_identical(.tt_offsets(f), .tt_offset)
  expect_false("period_id" %in% all.vars(f))
  expect_false("tstop" %in% all.vars(f))
  expect_true(is.finite(emt$ratio_of_irrs))
})


test_that("the heterogeneity model interacts treatment with the trial", {
  het <- .tt_six()$heterogeneity_test(.tt_weight)
  f <- attr(het, "model_formula")
  expect_identical(
    .tt_labels(f),
    c("exposed", .tt_ns_trial, .tt_ns_tstart, paste0("exposed:", .tt_ns_trial))
  )
  expect_identical(.tt_offsets(f), .tt_offset)
  expect_false("period_id" %in% all.vars(f))
  expect_false("tstop" %in% all.vars(f))
  expect_identical(het$n_trials, 6L)
  expect_true(all(is.finite(het$interaction_coefs$estimate)))
})


test_that("the IRR fits with 2, 3 and 4 trials, and the trial term follows the ladder", {
  expected <- c(
    "2" = "factor(enrollment_period_id)",
    "3" = "factor(enrollment_period_id)",
    "4" = .tt_ns_trial
  )
  for (k in names(expected)) {
    trial <- .tt_enroll(as.integer(k))
    expect_identical(data.table::uniqueN(trial$data$enrollment_period_id), as.integer(k))
    r <- trial$irr(.tt_weight)
    expect_identical(
      .tt_labels(attr(r, "model_formula")),
      c("exposed", .tt_ns_tstart, expected[[k]]),
      label = paste(k, "trials")
    )
    expect_true(is.finite(log(r$IRR)), label = paste(k, "trials"))
  }
})


# A trial-level panel. The intervention arm holds one trial and one follow-up
# interval, so its censoring stratum sees one calendar period and one start.
# The comparator arm holds two trials, so the pooled panel holds two calendar
# periods.
#
# A deviator carries `weeks_to_protocol_deviation = 2`, the weekly boundary
# `enroll()` would write for a switch in follow-up week 3. Her one row is
# clipped to `[0, 2)` and censored. Without the weekly value, the collapsed
# read stops her at the start of her only follow-up interval, so she would
# keep no row and the stratum would hold no censoring.
.tt_one_period_panel <- function() {
  rows <- list()
  for (i in 1:30) {
    rows[[length(rows) + 1L]] <- data.table::data.table(
      enrollment_person_trial_id = paste0("I", i),
      tstart = 0L,
      tstop = 4L,
      exposed = TRUE,
      on_tx = !(i %% 3L == 0L),
      weeks_to_protocol_deviation = if (i %% 3L == 0L) 2L else NA_integer_,
      died = FALSE,
      age = 40 + i,
      enrollment_period_id = 0L,
      period_id = 1L
    )
  }
  for (i in 1:30) {
    k <- i %% 2L
    rows[[length(rows) + 1L]] <- data.table::data.table(
      enrollment_person_trial_id = paste0("C", i),
      tstart = 0L,
      tstop = 4L,
      exposed = FALSE,
      on_tx = (i %% 4L == 0L),
      weeks_to_protocol_deviation = if (i %% 4L == 0L) 2L else NA_integer_,
      died = FALSE,
      age = 40 + i,
      enrollment_period_id = k,
      period_id = k + 1L
    )
  }
  return(data.table::rbindlist(rows))
}

test_that("a censoring stratum with one calendar period drops that term and fits", {
  trial <- TTEEnrollment$new(
    .tt_one_period_panel(),
    TTEDesign$new(
      treatment_var = "exposed",
      time_treatment_var = "on_tx",
      outcome_vars = "died",
      confounder_vars = "age",
      follow_up_time = 4L
    )
  )
  suppressWarnings({
    trial$s2_ipw()
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = 4L,
      estimand = "pp",
      estimate_ipcw_pp_with_gam = FALSE
    )
  })
  forms <- trial$ipcw_formulas
  tx <- forms[["the intervention arm"]]
  cmp <- forms[["the comparator arm"]]
  # One start and one calendar period in the intervention stratum: both terms
  # drop, and the numerator is the intercept and the offset.
  expect_identical(.tt_labels(tx$denominator), "age")
  expect_identical(.tt_labels(tx$numerator), character(0))
  expect_identical(.tt_offsets(tx$numerator), .tt_offset)
  # The comparator stratum holds two calendar periods.
  expect_identical(.tt_labels(cmp$denominator), c("factor(period_id)", "age"))
  expect_true(all(is.finite(trial$data$ipcw_pp)))
})


test_that("the TARGET methods name the trial for the outcome model and the calendar period for censoring", {
  dir <- withr::local_tempdir()
  for (d in c("spec", "tteplan", "results", "meta")) {
    dir.create(file.path(dir, d), recursive = TRUE, showWarnings = FALSE)
  }
  sk <- ttm_skeleton(
    "A",
    n_persons = 20L,
    date_max = "2016-12-31",
    n_init_periods = 4L
  )
  skel <- file.path(dir, "tteplan", "skel_a.qs2")
  qs2::qs_save(sk, skel)
  ttm_write_spec(file.path(dir, "spec", "spec_v001.yaml"), "tt", "ri_highrisk")
  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel, data_meta_dir = file.path(dir, "meta")),
    candidate_dir_spec = file.path(dir, "spec"),
    candidate_dir_tteplan = file.path(dir, "tteplan"),
    candidate_dir_results = file.path(dir, "results"),
    spec_version = "v001",
    global_max_isoyearweek = max(sk$isoyearweek, na.rm = TRUE),
    check_skeletons = FALSE
  )
  lines <- utils::capture.output(plan$print_target_checklist())
  i <- grep("Item 6h\\. ", lines)[1]
  j <- grep("Item 7a-h\\. ", lines)[1]
  it6h <- paste(lines[i:(j - 1L)], collapse = "\n")

  expect_match(
    it6h,
    "two time terms: the time since time zero, and the calendar period of follow-up",
    fixed = TRUE
  )
  expect_match(
    it6h,
    "second model that included only the time since time zero",
    fixed = TRUE
  )
  expect_match(
    it6h,
    "two time terms: the time since time zero, and the trial index",
    fixed = TRUE
  )
  expect_match(it6h, "4 or more distinct values", fixed = TRUE)
  # Retired claims: the censoring model used the trial index for calendar
  # time, and the outcome model read follow-up time and a linear trial term.
  expect_false(grepl("trial index to adjust for calendar time", it6h, fixed = TRUE))
  expect_false(grepl("a linear term with 2 to 4 trials", it6h, fixed = TRUE))
  expect_false(grepl("same time terms", it6h, fixed = TRUE))
})
