# The minimum-events rule at s3 (phase 3 of the plan that added the rule).
#
# s3 cuts the follow-up of each ETT into 52-week windows. In each window the
# expected events of an arm under the null are the pooled weighted rate times
# that arm's weighted person-years. A window passes when both arms reach
# `min_expected_events_per_arm_per_year`. Beyond the first failing window, s3
# withholds the incidence rate ratio, the risk difference and the number
# needed to treat of that estimand, and every exhibit prints `NE`.
#
# The blocks below follow the brief, (a) to (m):
#   (a) a hand-computed 2-window, 2-arm panel;
#   (b) an arm's own events reach its expected events only through the pooled
#       rate, and swapping the arm labels keeps every pass/fail decision;
#   (c) `$s3_analyze()` on a fixture with a thin intervention tail withholds
#       the 2-window ETT and reports the 1-window ETT;
#   (d) s3 stops on a spec without `min_expected_events_per_arm_per_year`;
#   (e) a failed horizon computation stops s3, never warn-and-skip;
#   (f) the read-back matrix over every place a withheld field reaches;
#   (g) window edge cases;
#   (h) the window person-years sum to the person-years of `$rates()`;
#   (i) the survival figure stops the curves at the reportable horizon;
#   (j) the writers stop when the stored rule differs from the spec;
#   (k) the writers stop on a stored result with no horizon record;
#   (l) the writers compare the stored rule with identical(), no tolerance;
#   (m) the writers accept a record of the current rule.

skip_if_not_installed("data.table")

# ---- (a), (b), (g): hand-built panels ---------------------------------------

# One row per interval. `arm` TRUE is the intervention arm.
.hz_toy <- function(arm, tstart, tstop, w, event) {
  data.table::data.table(
    tx = arm,
    tstart = tstart,
    tstop = tstop,
    w = w,
    event = event
  )
}

.hz_win <- function(d, follow_up, min_events) {
  swereg:::.tte_horizon_windows(
    d,
    "w",
    follow_up,
    list(min_expected_events = min_events),
    "tx"
  )
}

# Two windows, (0, 52] and (52, 104].
#   intervention: (0, 52] w 2 event; (50, 54] w 1; (60, 104] w 0.5 event
#   comparator:   (0, 104] w 1; (10, 30] w 3 event; (52, 80] w 2 event
# Window 1 person-weeks: int 2*52 + 1*2 = 106; cmp 1*52 + 3*20 = 112.
# Window 2 person-weeks: int 1*2 + 0.5*44 = 24; cmp 1*52 + 2*28 = 108.
# Window 1 events: int 2 (the event at week 52 counts here); cmp 3.
# Window 2 events: int 0.5; cmp 2.
.hz_toy_a <- function() {
  .hz_toy(
    arm = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    tstart = c(0, 50, 60, 0, 10, 52),
    tstop = c(52, 54, 104, 104, 30, 80),
    w = c(2, 1, 0.5, 1, 3, 2),
    event = c(1, 0, 1, 0, 1, 1)
  )
}

test_that("(a) the window table matches a hand computation", {
  win <- .hz_win(.hz_toy_a(), 104, 2)
  expect_identical(
    names(win),
    c(
      "window_lo",
      "window_hi",
      "arm",
      "py_weighted",
      "events_weighted",
      "rate_pooled",
      "events_expected",
      "pass"
    )
  )
  expect_equal(win$window_lo, c(0, 0, 52, 52))
  expect_equal(win$window_hi, c(52, 52, 104, 104))
  expect_identical(
    win$arm,
    c("intervention", "comparator", "intervention", "comparator")
  )
  expect_equal(win$py_weighted, c(106, 112, 24, 108) / 52.25, tolerance = 1e-12)
  expect_equal(win$events_weighted, c(2, 3, 0.5, 2), tolerance = 1e-12)
  rate1 <- 5 / (218 / 52.25)
  rate2 <- 2.5 / (132 / 52.25)
  expect_equal(
    win$rate_pooled,
    c(rate1, rate1, rate2, rate2),
    tolerance = 1e-12
  )
  expect_equal(
    win$events_expected,
    c(530 / 218, 560 / 218, 60 / 132, 270 / 132),
    tolerance = 1e-12
  )
  # Threshold 2: window 1 holds 2.43 and 2.57, window 2 holds 0.45 and 2.05.
  expect_identical(win$pass, c(TRUE, TRUE, FALSE, FALSE))

  dec <- swereg:::.tte_horizon_decision(win, list(min_expected_events = 2))
  expect_identical(dec$reportable_weeks, 52)
  expect_true(dec$withheld)
  expect_identical(dec$fail_arm, "intervention")
  expect_identical(
    dec$status,
    "below minimum-events rule: intervention arm, weeks 52-104"
  )
  expect_true(startsWith(dec$status, swereg:::.HORIZON_STATUS_PREFIX))
})

# One window. The intervention arm holds 10 weighted events and the
# comparator arm 1, over almost equal person-time, so the pooled expected
# events are about 5.5 in each arm. A comparator-rate rule would give about 1
# in each arm before a label swap and about 10 after it.
.hz_toy_b <- function(int_events = 1) {
  .hz_toy(
    arm = c(TRUE, FALSE, FALSE),
    tstart = c(0, 0, 0),
    tstop = c(52, 52, 1),
    w = c(10, 10, 1),
    event = c(int_events, 0, 1)
  )
}

test_that("(b) an arm's events reach its expected events only through the pooled rate", {
  base <- .hz_win(.hz_toy_a(), 104, 2)
  # Expected events are the pooled rate times the arm's person-years.
  expect_equal(
    base$events_expected,
    base$rate_pooled * base$py_weighted,
    tolerance = 1e-12
  )
  # Double the intervention events. Both arms' expected events move, and by
  # the ratio of the pooled rates, the comparator arm included.
  more <- .hz_toy_a()
  more[tx == TRUE, event := event * 2]
  moved <- .hz_win(more, 104, 2)
  expect_equal(moved$py_weighted, base$py_weighted)
  expect_equal(
    moved$events_expected / base$events_expected,
    moved$rate_pooled / base$rate_pooled,
    tolerance = 1e-12
  )
  cmp <- base$arm == "comparator"
  expect_true(all(moved$events_expected[cmp] > base$events_expected[cmp]))

  # Swap the arm labels. The pooled rate is symmetric, so every window keeps
  # its decision, and each arm's expected events move with its person-time.
  d <- .hz_toy_b()
  sw <- data.table::copy(d)[, tx := !tx]
  a <- .hz_win(d, 52, 5)
  b <- .hz_win(sw, 52, 5)
  expect_identical(a$pass, c(TRUE, TRUE))
  expect_identical(b$pass, a$pass)
  expect_equal(b$events_expected, rev(a$events_expected), tolerance = 1e-12)
  a2 <- .hz_win(.hz_toy_a(), 104, 2)
  b2 <- .hz_win(data.table::copy(.hz_toy_a())[, tx := !tx], 104, 2)
  expect_identical(b2$pass, a2$pass)
})

test_that("(g) window edges, partial windows, first failure, zero person-time and missing weights", {
  # An event at exactly week 52 counts in window 1, never in window 2.
  d <- .hz_toy(c(TRUE, FALSE), c(48, 0), c(52, 104), c(3, 1), c(1, 0))
  win <- .hz_win(d, 104, 0)
  expect_equal(win[arm == "intervention"]$events_weighted, c(3, 0))

  # A row (50, 54] splits 2 weeks into each window.
  d <- .hz_toy(c(TRUE, FALSE), c(50, 0), c(54, 104), c(1, 1), c(0, 0))
  win <- .hz_win(d, 104, 0)
  expect_equal(win[arm == "intervention"]$py_weighted, c(2, 2) / 52.25)

  # A 26-week ETT has one partial window, (0, 26].
  d <- .hz_toy(c(TRUE, FALSE), c(0, 0), c(26, 26), c(1, 1), c(1, 1))
  win <- .hz_win(d, 26, 0)
  expect_identical(nrow(win), 2L)
  expect_equal(unique(win$window_lo), 0)
  expect_equal(unique(win$window_hi), 26)

  # Fail, pass, fail: the intervention arm has person-time in window 2 only.
  # The reportable horizon ends before the FIRST failure, at 0.
  cmp <- .hz_toy(
    rep(FALSE, 3),
    c(0, 52, 104),
    c(52, 104, 156),
    c(1, 1, 1),
    c(1, 1, 1)
  )
  d <- rbind(.hz_toy(TRUE, 52, 104, 1, 1), cmp)
  rule <- list(min_expected_events = 0.5)
  win <- .hz_win(d, 156, 0.5)
  expect_identical(
    unique(win[, .(window_lo, pass)])$pass,
    c(FALSE, TRUE, FALSE)
  )
  dec <- swereg:::.tte_horizon_decision(win, rule)
  expect_identical(dec$reportable_weeks, 0)
  expect_identical(
    dec$status,
    "below minimum-events rule: intervention arm, weeks 0-52"
  )

  # Pass, fail, pass: the horizon ends at 52, not at the last window.
  d <- rbind(
    .hz_toy(c(TRUE, TRUE), c(0, 104), c(52, 156), c(1, 1), c(1, 1)),
    cmp
  )
  win <- .hz_win(d, 156, 0.5)
  expect_identical(unique(win[, .(window_lo, pass)])$pass, c(TRUE, FALSE, TRUE))
  expect_identical(
    swereg:::.tte_horizon_decision(win, rule)$reportable_weeks,
    52
  )

  # Zero person-time in window 2: a pooled rate of 0 and expected events of
  # 0. It fails at threshold 10 and passes at 0.
  d <- .hz_toy(c(TRUE, FALSE), c(0, 0), c(52, 52), c(1, 1), c(1, 1))
  at10 <- .hz_win(d, 104, 10)
  at0 <- .hz_win(d, 104, 0)
  expect_equal(at10[window_lo == 52]$rate_pooled, c(0, 0))
  expect_equal(at10[window_lo == 52]$events_expected, c(0, 0))
  expect_identical(at10[window_lo == 52]$pass, c(FALSE, FALSE))
  expect_identical(at0$pass, c(TRUE, TRUE, TRUE, TRUE))
  dec <- swereg:::.tte_horizon_decision(at10, list(min_expected_events = 10))
  expect_identical(dec$fail_arm, "both")
  expect_identical(dec$status, "below minimum-events rule: both arms, weeks 0-52")

  # A missing weight, interval start or interval stop is an error.
  bad <- .hz_toy_a()[1L, w := NA_real_]
  expect_error(.hz_win(bad, 104, 2), "weight column 'w'")
  bad <- .hz_toy_a()[2L, tstart := NA_real_]
  expect_error(.hz_win(bad, 104, 2), "interval start")
  bad <- .hz_toy_a()[3L, tstop := Inf]
  expect_error(.hz_win(bad, 104, 2), "interval stop")
})


# ---- the fixture plan --------------------------------------------------------

# One trial per person, 4-week intervals to `end_wk`. Every intervention
# person stops at week 52, except the first `tail_keep`, so the intervention
# arm holds a thin tail in window 2.
#
# `untrunc` sets the untruncated per-protocol weight. `"proportional"` is the
# truncated weight times 1.1. A constant factor cancels in the weighted
# survival of each arm, so that weight gives the risk difference of the
# truncated weight. `"person"` multiplies by 1, 1.75 or 2.5 by person, so the
# two weights give two different risk differences (swereg#66).
.hz_panel <- function(
  n = 160L,
  tail_keep = 4L,
  end_wk = 104L,
  seed = 7L,
  untrunc = c("proportional", "person")
) {
  untrunc <- match.arg(untrunc)
  set.seed(seed)
  rows <- vector("list", n)
  for (k in seq_len(n)) {
    arm <- k %% 2L == 1L
    last <- if (arm && (k + 1L) / 2L > tail_keep) 52L else end_wk
    n_int <- last %/% 4L
    ev <- stats::rbinom(n_int, 1L, if (arm) 0.01 else 0.06)
    keep <- if (any(ev == 1L)) seq_len(which(ev == 1L)[1L]) else seq_len(n_int)
    rows[[k]] <- data.table::data.table(
      id = sprintf("p%04d", k),
      enrollment_person_trial_id = sprintf("p%04d_t1", k),
      rd_intervention = arm,
      tstart = (keep - 1L) * 4L,
      tstop = keep * 4L,
      event = ev[keep],
      person_weeks = 4,
      rd_age_continuous = 50 + k %% 10L,
      rd_sg = if (k %% 4L < 2L) "a" else "b"
    )
  }
  d <- data.table::rbindlist(rows)
  w <- 1 + 0.2 * (d$tstop %/% 4L %% 3L)
  d[, analysis_weight_pp_trunc := w]
  if (untrunc == "proportional") {
    d[, analysis_weight_pp := w * 1.1]
  } else {
    d[,
      analysis_weight_pp := w *
        (1 + 0.75 * (as.integer(sub("^p", "", id)) %% 3L))
    ]
  }
  d[, ipw := 1]
  d[, ipw_trunc := w]
  return(d)
}

# Write one enrollment of two ETTs: ETT00001 at 52 weeks and ETT00002 at 104
# weeks, on the same panel. Returns the plan, with the stage stamps set and
# nothing computed. `untrunc` is passed to `.hz_panel()`.
.hz_build <- function(dir, min_events = 5, untrunc = "proportional") {
  d <- .hz_panel(untrunc = untrunc)
  design <- swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "rd_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "rd_age_continuous",
    follow_up_time = 104L
  )
  files <- list(
    `52` = d[tstop <= 52],
    `104` = d
  )
  for (fu in names(files)) {
    enr <- swereg::TTEEnrollment$new(files[[fu]], design, data_level = "trial")
    for (f in paste0(c("analysis_", "analysis_itt_"), fu, ".qs2")) {
      qs2::qs_save(enr, file.path(dir, f))
    }
  }
  qs2::qs_save(
    swereg::TTEEnrollment$new(d, design, data_level = "trial"),
    file.path(dir, "raw_01.qs2")
  )
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = c("ETT00001", "ETT00002"),
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    outcome_role = "primary",
    follow_up = c(52L, 104L),
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "id",
    treatment_var = "rd_intervention",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = c("analysis_52.qs2", "analysis_104.qs2"),
    file_analysis_itt = c("analysis_itt_52.qs2", "analysis_itt_104.qs2"),
    description = c("ETT00001", "ETT00002")
  )
  data.table::set(
    ett,
    j = "subgroup_vars",
    value = list(list("rd_sg", "rd_sg"))
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "hz",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$output_dir <- dir
  plan$spec <- list(
    study = list(
      title = "Fixture",
      implementation = list(project_prefix = "hz")
    ),
    enrollments = list(list(
      id = "01",
      name = "Enrollment one",
      treatment = list(
        arms = list(intervention = "Treated", comparator = "Untreated")
      )
    )),
    min_expected_events_per_arm_per_year = min_events,
    period_width = 4L,
    admin_censoring_isoyearweek = "2020-52"
  )
  tte_stamp_stages(plan)
  return(plan)
}

# The run every block from (c) on reads: s3 through the REAL batch runner
# (worker subprocesses), then the workbook. Built once per file.
.hz_cache <- new.env()
.hz_fixture <- function() {
  if (!is.null(.hz_cache$fx)) {
    return(.hz_cache$fx)
  }
  dir <- tempfile("hz_s3_")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = testthat::teardown_env())
  plan <- .hz_build(dir)
  out <- utils::capture.output(suppressWarnings(plan$s3_analyze(
    output_dir = dir,
    n_workers = 3L,
    swereg_dev_path = swereg:::.swereg_dev_path()
  )))
  xlsx <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
  .hz_cache$fx <- list(dir = dir, plan = plan, xlsx = xlsx, out = out)
  return(.hz_cache$fx)
}

.hz_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("survey")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
}

# One sheet as a character matrix, with the header row found by the cell
# `header`. Returns the rows under it, named by it.
.hz_sheet <- function(xlsx, sheet, header) {
  m <- openxlsx::read.xlsx(
    xlsx,
    sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  m[] <- lapply(m, as.character)
  h <- which(apply(m, 1L, function(r) header %in% r))[1L]
  out <- m[-seq_len(h), , drop = FALSE]
  names(out) <- make.unique(as.character(unlist(m[h, ])))
  return(out)
}


# ---- (c), (d), (e), (h), (j): the s3 boundary -------------------------------

test_that("(c) s3 withholds the 2-window ETT and reports the 1-window ETT", {
  .hz_skip()
  fx <- .hz_fixture()
  e <- fx$plan$get_estimates()

  # ETT00002: window 2 fails on the intervention arm, in both estimands.
  w2 <- e[ett_id == "ETT00002"]
  expect_identical(nrow(w2), 3L)
  status <- "below minimum-events rule: intervention arm, weeks 52-104"
  expect_identical(unique(w2$horizon_status), status)
  expect_equal(unique(w2$horizon_reportable_weeks), 52)
  expect_true(all(is.na(w2$irr)))
  rd2 <- w2[!is.na(interval_status)]
  expect_identical(nrow(rd2), 3L)
  expect_true(all(is.na(rd2$rd)))
  expect_true(all(is.na(rd2$nnt)))
  expect_identical(unique(rd2$interval_status), status)
  expect_match(fx$out, "minimum-events rule withheld 2 of 4", all = FALSE)

  # ETT00001: one window, which passes. Everything is reported.
  w1 <- e[ett_id == "ETT00001"]
  expect_identical(unique(w1$horizon_status), swereg:::.HORIZON_STATUS_PASS)
  expect_equal(unique(w1$horizon_reportable_weeks), 52)
  expect_true(all(is.finite(w1$irr)))
  expect_true(all(is.finite(w1[!is.na(interval_status)]$rd)))
  expect_true(all(is.finite(w1[!is.na(interval_status)]$nnt)))

  # The window table behind it.
  hw <- fx$plan$get_horizon_windows()
  x <- hw[ett_id == "ETT00002" & estimand == "pp" & window_lo == 52]
  expect_identical(x$pass, c(FALSE, FALSE))
  expect_lt(x[arm == "intervention"]$events_expected, 5)
  expect_gte(x[arm == "comparator"]$events_expected, 5)
  expect_identical(unique(hw$min_expected_events_per_arm_per_year), 5)
})

test_that("(h) the window person-years sum to the person-years of $rates()", {
  .hz_skip()
  fx <- .hz_fixture()
  for (f in c("analysis_52.qs2", "analysis_104.qs2")) {
    enr <- swereg::qs2_read(file.path(fx$dir, f))
    fu <- if (grepl("_52", f)) 52 else 104
    for (wc in c(
      "analysis_weight_pp_trunc",
      "analysis_weight_pp",
      "ipw_trunc"
    )) {
      win <- swereg:::.tte_horizon_windows(
        enr$data,
        wc,
        fu,
        list(min_expected_events = 5),
        "rd_intervention"
      )
      r <- enr$rates(weight_col = wc)
      for (a in c(TRUE, FALSE)) {
        lab <- if (a) "intervention" else "comparator"
        expect_equal(
          sum(win[arm == lab]$py_weighted),
          r[rd_intervention == a]$py_weighted,
          tolerance = 1e-8,
          info = paste(f, wc, lab)
        )
        expect_equal(
          sum(win[arm == lab]$events_weighted),
          r[rd_intervention == a]$events_weighted,
          tolerance = 1e-8,
          info = paste(f, wc, lab)
        )
      }
    }
  }
})

test_that("(d) s3 stops on a spec without the minimum-events key", {
  dir <- withr::local_tempdir()
  plan <- .hz_build(dir)
  plan$spec$min_expected_events_per_arm_per_year <- NULL
  testthat::local_mocked_bindings(
    .batch_run = function(...) stop("__HZ_DISPATCHED__"),
    .package = "swereg"
  )
  err <- tryCatch(
    plan$s3_analyze(output_dir = dir, n_workers = 1L),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "min_expected_events_per_arm_per_year", fixed = TRUE)
  expect_match(err, "$reload_spec()", fixed = TRUE)
  expect_false(grepl("__HZ_DISPATCHED__", err, fixed = TRUE))
})

# The dispatcher stand-in for (e). It runs each item IN PROCESS through the
# real worker, so a mocked `.tte_horizon_windows()` reaches it. `lenient`
# turns an item error into a NULL return, as a pool that drops a failed item
# would. The parent merge then has to notice the missing decision itself.
.hz_inproc <- function(lenient) {
  function(target, items, ...) {
    fn <- get(target$symbol, envir = asNamespace("swereg"))
    out <- lapply(items, function(it) {
      if (!lenient) {
        return(do.call(fn, it))
      }
      return(tryCatch(do.call(fn, it), error = function(e) NULL))
    })
    return(stats::setNames(out, names(items)))
  }
}

test_that("(e) a failed horizon computation stops s3, never warn-and-skip", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("survey")
  dir <- withr::local_tempdir()
  plan <- .hz_build(dir)
  testthat::local_mocked_bindings(
    .tte_horizon_windows = function(...) stop("__HZ_FORCED__"),
    .package = "swereg"
  )

  # The worker: the error leaves the item. A skip envelope would be a
  # warning and a list, and the item would look like a success.
  expect_error(
    swereg:::.s3_ett_worker(
      analysis_path = file.path(dir, "analysis_104.qs2"),
      method = "risk_difference",
      weight_col = "analysis_weight_pp_trunc",
      ett_id = "ETT00002",
      n_threads = 1L,
      subgroup_var = NULL,
      conf_level = 0.95,
      horizon_rule = list(min_expected_events = 5),
      follow_up = 104
    ),
    "__HZ_FORCED__"
  )

  # The parent, with a dispatcher that propagates the item error.
  testthat::local_mocked_bindings(
    .batch_run = .hz_inproc(lenient = FALSE),
    .package = "swereg"
  )
  expect_error(
    suppressWarnings(utils::capture.output(
      plan$s3_analyze(output_dir = dir, n_workers = 1L)
    )),
    "__HZ_FORCED__"
  )

  # The parent, with a dispatcher that DROPS the failed item. The merge must
  # stop on the missing decision, and name the ETT and the estimand.
  testthat::local_mocked_bindings(
    .batch_run = .hz_inproc(lenient = TRUE),
    .package = "swereg"
  )
  err <- tryCatch(
    suppressWarnings(utils::capture.output(
      plan$s3_analyze(output_dir = dir, n_workers = 1L)
    )),
    error = function(e) conditionMessage(e)
  )
  expect_match(
    err,
    "ETT00001 has no decision of the minimum-events rule",
    fixed = TRUE
  )
  expect_match(err, "per-protocol", fixed = TRUE)
  expect_false(isTRUE(plan$results_ett$ETT00001$horizon_pp$withheld == FALSE))
})

test_that("(j) the writers stop when the stored rule differs from the spec", {
  .hz_skip()
  fx <- .hz_fixture()
  plan <- fx$plan
  old <- plan$spec$min_expected_events_per_arm_per_year
  on.exit(plan$spec$min_expected_events_per_arm_per_year <- old, add = TRUE)
  plan$spec$min_expected_events_per_arm_per_year <- 6
  xlsx <- file.path(fx$dir, "stale.xlsx")
  expect_error(plan$export_tables(path = xlsx), "\\$s3_analyze\\(\\)")
  expect_error(
    plan$export_tables(path = xlsx),
    "min_expected_events_per_arm_per_year = 5"
  )
  expect_false(file.exists(xlsx))
  expect_error(
    plan$export(
      list(list(type = "table1", enrollment = "01")),
      dir = file.path(fx$dir, "stale")
    ),
    "\\$s3_analyze\\(\\)"
  )
  expect_error(
    utils::capture.output(plan$print_target_checklist()),
    "\\$s3_analyze\\(\\)"
  )
  # The rule the results were judged with passes.
  plan$spec$min_expected_events_per_arm_per_year <- old
  expect_no_error(swereg:::.tte_assert_horizon_rule_current(plan, "x"))
})

# ---- (f): the read-back matrix -----------------------------------------------

test_that("(f) read-back matrix: estimates, subgroups and results.qs2", {
  .hz_skip()
  fx <- .hz_fixture()
  ro <- qs2::qs_read(file.path(fx$dir, "results.qs2"))
  withheld_num <- c(
    "irr",
    "irr_lo",
    "irr_hi",
    "irr_pvalue",
    "rd",
    "rd_lo",
    "rd_hi",
    "nnt",
    "nnt_lo",
    "nnt_hi"
  )
  for (src in c("get_estimates", "results.qs2")) {
    e <- if (src == "get_estimates") fx$plan$get_estimates() else ro$estimates
    for (combo in list(
      c("pp", "truncated"),
      c("pp", "untruncated"),
      c("itt", "truncated")
    )) {
      w <- e[ett_id == "ETT00002" & estimand == combo[1] & weights == combo[2]]
      r <- e[ett_id == "ETT00001" & estimand == combo[1] & weights == combo[2]]
      lab <- paste(src, combo[1], combo[2])
      # Every combination carries a risk difference: per-protocol on the
      # untruncated weight too (swereg#66).
      has_rd <- TRUE
      for (nm in withheld_num) {
        expect_true(is.na(w[[nm]]), info = paste(lab, "withheld", nm))
        if (has_rd || startsWith(nm, "irr")) {
          expect_true(is.finite(r[[nm]]), info = paste(lab, "reported", nm))
        }
      }
      expect_false(w$irr_estimable, info = lab)
      expect_true(r$irr_estimable, info = lab)
      # Events, person-years and rates stay.
      for (nm in c(
        "events_int",
        "py_int",
        "rate_int",
        "events_cmp",
        "py_cmp"
      )) {
        expect_true(is.finite(w[[nm]]), info = paste(lab, "kept", nm))
      }
      if (has_rd) {
        expect_true(
          startsWith(w$interval_status, swereg:::.HORIZON_STATUS_PREFIX),
          info = lab
        )
        expect_false(
          startsWith(r$interval_status, swereg:::.HORIZON_STATUS_PREFIX),
          info = lab
        )
      }
    }
  }

  # The subgroup ratios and the interaction test, from the accessor and from
  # results.qs2.
  for (src in c("get_subgroups", "results.qs2")) {
    sg <- if (src == "get_subgroups") fx$plan$get_subgroups() else ro$subgroups
    expect_true(data.table::is.data.table(sg), info = src)
    for (est in c("pp", "itt")) {
      w <- sg[ett_id == "ETT00002" & estimand == est]
      r <- sg[ett_id == "ETT00001" & estimand == est]
      lab <- paste(src, est)
      expect_identical(sort(w$subgroup_level), c("a", "all", "b"), info = lab)
      expect_identical(sort(r$subgroup_level), c("a", "all", "b"), info = lab)
      for (nm in c(
        "irr",
        "irr_lo",
        "irr_hi",
        "irr_pvalue",
        "em_pvalue",
        "ratio_of_irrs"
      )) {
        expect_true(all(is.na(w[[nm]])), info = paste(lab, "withheld", nm))
        expect_true(all(is.finite(r[[nm]])), info = paste(lab, "reported", nm))
      }
    }
  }

  # The survival curve stays at full length for the withheld ETT.
  cv <- fx$plan$get_curves()[ett_id == "ETT00002" & estimand == "pp"]
  expect_equal(max(cv$follow_up_interval), 104)
  expect_true(all(is.finite(cv$surv)))
})

test_that("(f) read-back matrix: PP results sheet", {
  .hz_skip()
  fx <- .hz_fixture()
  s <- .hz_sheet(fx$xlsx, "PP results", "IRR")
  w <- s[s[["Follow-up (weeks)"]] == "104", ]
  r <- s[s[["Follow-up (weeks)"]] == "52", ]
  for (nm in c(
    "IRR",
    "95% CI",
    "p-value",
    "Risk difference per 10,000",
    "Risk difference 95% CI"
  )) {
    expect_identical(w[[nm]], "NE", info = paste("withheld", nm))
    expect_false(r[[nm]] %in% c("NE", NA, ""), info = paste("reported", nm))
  }
  expect_false(is.na(suppressWarnings(as.numeric(w[["PY (int)"]]))))
  expect_false(is.na(suppressWarnings(as.numeric(w[[
    "Persons with event (int)"
  ]]))))
})

test_that("(f) read-back matrix: ITT results sheet", {
  .hz_skip()
  fx <- .hz_fixture()
  s <- .hz_sheet(fx$xlsx, "ITT results", "IRR")
  w <- s[s[["Follow-up (weeks)"]] == "104", ]
  r <- s[s[["Follow-up (weeks)"]] == "52", ]
  for (nm in c(
    "IRR",
    "95% CI",
    "p-value",
    "Risk difference per 10,000",
    "Risk difference 95% CI"
  )) {
    expect_identical(w[[nm]], "NE", info = paste("withheld", nm))
    expect_false(r[[nm]] %in% c("NE", NA, ""), info = paste("reported", nm))
  }
  expect_false(is.na(suppressWarnings(as.numeric(w[["Events (int)"]]))))
})

test_that("(f) read-back matrix: Summary sheet", {
  .hz_skip()
  fx <- .hz_fixture()
  s <- .hz_sheet(fx$xlsx, "Summary", "ETT")
  w <- s[s$ETT == "ETT00002", ]
  r <- s[s$ETT == "ETT00001", ]
  for (nm in c("ITT IRR (CI)", "PP IRR (CI)", "PP RD per 10,000 (CI)")) {
    expect_identical(w[[nm]], "NE", info = paste("withheld", nm))
    expect_false(r[[nm]] %in% c("NE", NA, ""), info = paste("reported", nm))
  }
  expect_match(w[["Events int/cmp (PP)"]], "^[0-9,]+ / [0-9,]+$")
})

test_that("(f) read-back matrix: Sensitivity sheet", {
  .hz_skip()
  fx <- .hz_fixture()
  s <- .hz_sheet(fx$xlsx, "Weight truncation (PP)", "IRR")
  w <- s[s[["Follow-up (weeks)"]] == "104", ]
  r <- s[s[["Follow-up (weeks)"]] == "52", ]
  # Both blocks, truncated then untruncated: make.unique() names the second
  # block's columns `IRR.1`, `95% CI.1` and `p-value.1`. Each block also
  # carries the risk difference, its interval and the number needed to treat.
  rd_cols <- c(
    "Risk difference per 10,000",
    "Risk difference 95% CI",
    "NNT (95% CI)"
  )
  for (nm in c(
    "IRR",
    "95% CI",
    "p-value",
    rd_cols,
    "IRR.1",
    "95% CI.1",
    "p-value.1",
    paste0(rd_cols, ".1")
  )) {
    expect_identical(w[[nm]], "NE", info = paste("withheld", nm))
    expect_false(r[[nm]] %in% c("NE", NA, ""), info = paste("reported", nm))
  }
})

test_that("(f) read-back matrix: Effect modification sheet", {
  .hz_skip()
  fx <- .hz_fixture()
  s <- .hz_sheet(fx$xlsx, "Effect modification", "Subgroup")
  w <- s[s$Enrollment == "ETT00002", ]
  r <- s[s$Enrollment == "ETT00001", ]
  expect_identical(sort(w$Level), c("a", "all", "b"))
  expect_identical(sort(r$Level), c("a", "all", "b"))
  # Every stratum row: the ratio and its interval, both estimands.
  for (nm in c("PP IRR", "PP 95% CI", "ITT IRR", "ITT 95% CI")) {
    expect_identical(w[[nm]], rep("NE", 3L), info = paste("withheld", nm))
    expect_false(
      any(r[[nm]] %in% c("NE", NA, "")),
      info = paste("reported", nm)
    )
  }
  # The interaction test sits on the "all" row.
  for (nm in c("EM p (PP)", "EM ratio (PP)", "EM p (ITT)", "EM ratio (ITT)")) {
    expect_identical(
      w[w$Level == "all", nm],
      "NE",
      info = paste("withheld", nm)
    )
    expect_false(
      is.na(suppressWarnings(as.numeric(r[r$Level == "all", nm]))),
      info = paste("reported", nm)
    )
  }
})

test_that("(f) read-back matrix: no workbook sheet carries a number needed to treat", {
  # The number needed to treat reaches the forest figure and the Weight
  # truncation (PP) sheet only. The PP results, ITT results and Summary
  # sheets have no NNT column, so no withheld NNT can show there. This pins
  # that, so a future NNT column arrives through a failing test and gets its
  # own NE rule. The sensitivity read-back test above pins the NE rule of
  # the Weight truncation (PP) sheet.
  .hz_skip()
  fx <- .hz_fixture()
  for (sh in c("PP results", "ITT results", "Summary")) {
    hdr <- names(.hz_sheet(fx$xlsx, sh, if (sh == "Summary") "ETT" else "IRR"))
    expect_gt(length(hdr), 5L)
    expect_false(
      any(grepl("NNT|needed to treat", hdr, ignore.case = TRUE)),
      info = sh
    )
  }
})

test_that("(f) read-back matrix: forest plot data", {
  .hz_skip()
  fx <- .hz_fixture()
  seen <- list()
  real <- swereg:::.render_combined_forest_plot
  testthat::local_mocked_bindings(
    .render_combined_forest_plot = function(...) {
      res <- real(...)
      seen[[length(seen) + 1L]] <<- res$text
      return(res)
    },
    .package = "swereg"
  )
  paths <- suppressWarnings(fx$plan$export(
    list(list(
      type = "forest",
      label = "forest",
      exposures = list(Treated = c("ETT00001", "ETT00002")),
      estimands = c("pp", "itt"),
      risk_difference = TRUE
    )),
    dir = file.path(fx$dir, "forest")
  ))
  expect_length(paths, 2L)
  expect_length(seen, 2L)
  for (k in seq_along(seen)) {
    tx <- data.table::as.data.table(seen[[k]])
    w <- tx[ett_id == "ETT00002"]
    r <- tx[ett_id == "ETT00001"]
    for (nm in c("txt_irr", "txt_rd", "txt_nnt")) {
      expect_identical(w[[nm]], "NE", info = paste(k, "withheld", nm))
      expect_false(
        r[[nm]] %in% c("NE", NA, ""),
        info = paste(k, "reported", nm)
      )
    }
  }
})


# ---- (i): the survival figure ------------------------------------------------

test_that("(i) the survival curves stop at the reportable horizon under a grey band", {
  .hz_skip()
  fx <- .hz_fixture()
  seen <- list()
  real <- swereg:::.render_survival_curve
  testthat::local_mocked_bindings(
    .render_survival_curve = function(...) {
      p <- real(...)
      seen[[length(seen) + 1L]] <<- p
      return(p)
    },
    .package = "swereg"
  )
  fig_dir <- file.path(fx$dir, "survival")
  paths <- fx$plan$export(
    list(list(
      type = "survival",
      label = "surv",
      enrollment = "01",
      outcome = "osd_a",
      follow_up = 104L,
      age_group = "50_59",
      estimands = "pp"
    )),
    dir = fig_dir
  )
  expect_length(paths, 1L)
  expect_true(file.exists(paths[[1L]]))
  expect_gt(file.size(paths[[1L]]), 0)
  .hz_cache$survival_png <- paths[[1L]]

  p <- seen[[1L]]
  geoms <- vapply(p$layers, function(l) class(l$geom)[1L], character(1))
  expect_identical(sum(geoms == "GeomRect"), 1L)
  rect <- ggplot2::layer_data(p, which(geoms == "GeomRect"))
  expect_equal(c(rect$xmin, rect$xmax), c(52, 104))
  step <- ggplot2::layer_data(p, which(geoms == "GeomStep"))
  expect_lte(max(step$x), 52)
  expect_identical(
    p$layers[[which(geoms == "GeomText")]]$aes_params$label,
    "Not estimable"
  )
  expect_equal(p$layers[[which(geoms == "GeomText")]]$aes_params$angle, 90)

  # The numbers-at-risk table runs over the full axis: its last column sits
  # at week 104 and its x scale reaches 104.
  tbl <- p[[1L]]
  tdat <- ggplot2::layer_data(tbl, 1L)
  expect_equal(max(tdat$x), 104)
  expect_equal(max(ggplot2::layer_scales(tbl)$x$limits), 104)
  expect_equal(max(ggplot2::layer_scales(p)$x$limits), 104)

  # A horizon of 0 makes the whole panel the band.
  curve <- data.table::data.table(
    follow_up_interval = c(52, 104, 52, 104),
    surv = c(0.9, 0.8, 0.95, 0.9),
    n_persons_at_risk = c(10, 5, 10, 8),
    group = c("Treated", "Treated", "Untreated", "Untreated")
  )
  p0 <- swereg:::.render_survival_curve(
    curve,
    "follow_up_interval",
    scale = "cumulative_failure",
    int_lab = "Treated",
    cmp_lab = "Untreated",
    horizon_weeks = 0,
    follow_up = 104
  )
  g0 <- vapply(p0$layers, function(l) class(l$geom)[1L], character(1))
  r0 <- ggplot2::layer_data(p0, which(g0 == "GeomRect"))
  expect_equal(c(r0$xmin, r0$xmax), c(0, 104))
  expect_lte(max(ggplot2::layer_data(p0, which(g0 == "GeomStep"))$x), 0)

  # The reported ETT draws no band.
  seen <- list()
  fx$plan$export(
    list(list(
      type = "survival",
      label = "surv52",
      enrollment = "01",
      outcome = "osd_a",
      follow_up = 52L,
      age_group = "50_59",
      estimands = "pp"
    )),
    dir = fig_dir
  )
  g1 <- vapply(seen[[1L]]$layers, function(l) class(l$geom)[1L], character(1))
  expect_false("GeomRect" %in% g1)
})


# ---- (k), (l), (m): freshness of the stored decisions ----------------------

# These run last. (m) rewrites results.qs2 in the fixture directory, and (f)
# reads that file.
test_that("(k) the writers stop on a stored result with no horizon record", {
  .hz_skip()
  fx <- .hz_fixture()
  plan <- fx$plan
  old <- plan$results_ett$ETT00002$horizon_itt
  on.exit(plan$results_ett$ETT00002$horizon_itt <- old, add = TRUE)
  # The ITT estimand of ETT00002 holds a rate ratio and a risk difference.
  expect_true(data.table::is.data.table(plan$results_ett$ETT00002$irr_itt))
  expect_true(data.table::is.data.table(plan$results_ett$ETT00002$rd_itt))
  plan$results_ett$ETT00002["horizon_itt"] <- list(NULL)
  xlsx <- file.path(fx$dir, "no_record.xlsx")
  expect_error(plan$export_tables(path = xlsx), "s3_analyze")
  expect_error(plan$export_tables(path = xlsx), "ETT00002 (itt)", fixed = TRUE)
  expect_false(file.exists(xlsx))
  expect_error(
    plan$export(
      list(list(type = "table1", enrollment = "01")),
      dir = file.path(fx$dir, "no_record")
    ),
    "s3_analyze"
  )
  expect_error(
    utils::capture.output(plan$print_target_checklist()),
    "s3_analyze"
  )
})

test_that("(l) the writers compare the stored rule exactly, with no tolerance", {
  .hz_skip()
  fx <- .hz_fixture()
  plan <- fx$plan
  old <- plan$spec$min_expected_events_per_arm_per_year
  on.exit(plan$spec$min_expected_events_per_arm_per_year <- old, add = TRUE)
  expect_identical(
    plan$results_ett$ETT00001$horizon_pp$rule$min_expected_events,
    5
  )
  # all.equal() accepts this difference. identical() does not.
  plan$spec$min_expected_events_per_arm_per_year <- 5 + 1e-10
  expect_true(isTRUE(all.equal(5, 5 + 1e-10)))
  xlsx <- file.path(fx$dir, "near.xlsx")
  expect_error(plan$export_tables(path = xlsx), "s3_analyze")
  expect_error(
    plan$export_tables(path = xlsx),
    "spec holds 5.0000000001.",
    fixed = TRUE
  )
  expect_false(file.exists(xlsx))
})

test_that("(m) the writers accept a record of the current rule", {
  .hz_skip()
  fx <- .hz_fixture()
  plan <- fx$plan
  old <- plan$spec$min_expected_events_per_arm_per_year
  on.exit(plan$spec$min_expected_events_per_arm_per_year <- old, add = TRUE)
  # A YAML spec reads 5 as an integer. The writers compare it as a double.
  for (rule in list(5, 5L)) {
    plan$spec$min_expected_events_per_arm_per_year <- rule
    xlsx <- tempfile("current_", tmpdir = fx$dir, fileext = ".xlsx")
    expect_no_error(suppressMessages(suppressWarnings(
      plan$export_tables(path = xlsx)
    )))
    expect_true(file.exists(xlsx))
  }
})


# ---- swereg#66: the untruncated per-protocol risk difference -----------------

# s3 runs in this process, through the real item builder, worker and parent
# merge, and then `$export_tables()` writes the workbook and results.qs2. Only
# the subprocess transport is replaced, so these blocks need no
# `skip_on_cran()`. The panel multiplies the untruncated per-protocol weight
# by a factor that differs by person. `min_events = 0` withholds nothing, and
# `min_events = 5` withholds ETT00002 as in (c).
.hz66_cache <- new.env()
.hz66_fixture <- function(min_events) {
  key <- paste0("min_", min_events)
  if (!is.null(.hz66_cache[[key]])) {
    return(.hz66_cache[[key]])
  }
  dir <- tempfile("hz66_")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = testthat::teardown_env())
  plan <- .hz_build(dir, min_events = min_events, untrunc = "person")
  testthat::local_mocked_bindings(
    .batch_run = .hz_inproc(lenient = FALSE),
    .package = "swereg"
  )
  utils::capture.output(suppressWarnings(plan$s3_analyze(
    output_dir = dir,
    n_workers = 1L
  )))
  xlsx <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
  fx <- list(dir = dir, plan = plan, xlsx = xlsx)
  .hz66_cache[[key]] <- fx
  return(fx)
}

.hz66_skip <- function() {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("survey")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
}

# A direct `$risk_difference()` call on one analysis file, with the settings
# s3 fixes.
.hz66_direct_rd <- function(dir, file, weight_col) {
  enr <- swereg::qs2_read(file.path(dir, file))
  fresh <- swereg::TTEEnrollment$new(
    enr$data,
    enr$design,
    data_level = "trial"
  )
  return(fresh$risk_difference(
    weight_col,
    n_boot = 500L,
    seed = 1L,
    conf_level = 0.95
  ))
}

# One sheet as a character matrix, every row and column kept.
.hz66_cells <- function(xlsx, sheet) {
  m <- openxlsx::read.xlsx(
    xlsx,
    sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  m[] <- lapply(m, as.character)
  return(as.matrix(m))
}

.HZ66_RD_FIELDS <- c("rd", "rd_lo", "rd_hi", "nnt", "nnt_lo", "nnt_hi")

test_that("#66 the untruncated per-protocol RD equals a direct call in every reader", {
  .hz66_skip()
  fx <- .hz66_fixture(0)
  plan <- fx$plan
  ro <- qs2::qs_read(file.path(fx$dir, "results.qs2"))
  expect_identical(nrow(plan$ett), 2L)
  for (k in seq_len(nrow(plan$ett))) {
    eid <- plan$ett$ett_id[k]
    curve <- .hz66_direct_rd(
      fx$dir,
      plan$ett$file_analysis[k],
      "analysis_weight_pp"
    )
    last <- curve[which.max(curve$tstop)]
    for (src in c("get_estimates", "results.qs2")) {
      tab <- if (src == "get_estimates") plan$get_estimates() else ro$estimates
      u <- tab[ett_id == eid & estimand == "pp" & weights == "untruncated"]
      tr <- tab[ett_id == eid & estimand == "pp" & weights == "truncated"]
      lab <- paste(eid, src)
      expect_identical(nrow(u), 1L, label = lab)
      expect_true(u$rd_stored, info = lab)
      # Every compared field is a number, so the equality below is not NA
      # against NA. The NNT bounds of ETT00002 are NA on the untruncated
      # weight, because its interval does not exclude zero.
      for (nm in c("rd", "rd_lo", "rd_hi", "nnt")) {
        expect_true(is.finite(u[[nm]]), info = paste(lab, nm))
      }
      for (nm in .HZ66_RD_FIELDS) {
        expect_equal(
          u[[nm]],
          last[[nm]],
          tolerance = 1e-10,
          info = paste(lab, nm)
        )
      }
      expect_identical(u$nnt_direction, last$nnt_direction, info = lab)
      expect_identical(u$interval_status, last$interval_status, info = lab)
      expect_identical(
        u$interval_status,
        if (eid == "ETT00002") "spans null" else "ok",
        info = lab
      )
      expect_identical(u$n_boot, 500, info = lab)
      expect_identical(u$seed, 1, info = lab)
      expect_identical(u$conf_level, 0.95, info = lab)
      # Its own estimate: the truncated weight gives another one.
      for (nm in c("rd", "rd_lo", "rd_hi", "nnt")) {
        expect_gt(abs(u[[nm]] - tr[[nm]]), 1e-6, label = paste(lab, nm))
      }
    }
    # The stored curve, read through `$get_curves()` and from results.qs2.
    for (src in c("get_curves", "results.qs2")) {
      cv <- if (src == "get_curves") plan$get_curves() else ro$curves
      for (a in c("comparator", "intervention")) {
        got <- cv[
          ett_id == eid & estimand == "pp" & weights == "untruncated" & arm == a
        ]
        data.table::setorder(got, follow_up_interval)
        lab <- paste(eid, src, a)
        expect_identical(got$follow_up_interval, as.numeric(curve$tstop))
        expect_equal(
          got$surv,
          curve[[paste0("surv_", a)]],
          tolerance = 1e-10,
          info = lab
        )
      }
    }
  }
})

test_that("#66 a withheld truncated per-protocol estimate withholds the untruncated RD", {
  .hz66_skip()
  fx <- .hz66_fixture(5)
  plan <- fx$plan
  res <- plan$results_ett$ETT00002
  rec <- res$horizon_pp
  status <- "below minimum-events rule: intervention arm, weeks 52-104"
  # The decision is the record of the truncated weight.
  expect_identical(rec$weight_col, "analysis_weight_pp_trunc")
  expect_true(rec$withheld)
  expect_identical(rec$status, status)
  # On its own weight the untruncated estimate passes the rule in every
  # year. So the withholding below is the decision of the truncated weight.
  enr <- swereg::qs2_read(file.path(fx$dir, "analysis_104.qs2"))
  own <- swereg:::.tte_horizon_record(
    enr$data,
    "analysis_weight_pp",
    104,
    list(min_expected_events = 5, exempt = FALSE),
    "rd_intervention",
    "tstart",
    "tstop"
  )
  expect_false(own$withheld)
  expect_true(all(own$windows$pass))

  e <- plan$get_estimates()
  for (w in c("truncated", "untruncated")) {
    row <- e[ett_id == "ETT00002" & estimand == "pp" & weights == w]
    expect_true(row$rd_stored, info = w)
    for (nm in .HZ66_RD_FIELDS) {
      expect_true(is.na(row[[nm]]), info = paste(w, "withheld", nm))
    }
    expect_true(is.na(row$nnt_direction), info = w)
    expect_identical(row$interval_status, rec$status, info = w)
  }
  # The untruncated curve carries no risk difference after the reportable
  # horizon, and keeps it before.
  cu <- res$rd_curve_pp
  late <- cu$tstop > rec$reportable_weeks
  expect_true(any(late))
  expect_true(all(is.na(cu$rd[late])))
  expect_true(all(cu$interval_status[late] == status))
  expect_true(all(is.finite(cu$rd[!late])))

  # ETT00001 passes, so its untruncated risk difference is reported.
  r1 <- e[ett_id == "ETT00001" & estimand == "pp" & weights == "untruncated"]
  expect_true(is.finite(r1$rd))
  expect_false(startsWith(r1$interval_status, swereg:::.HORIZON_STATUS_PREFIX))
})

test_that("#66 the stored horizon_pp is the record of the truncated weight", {
  .hz66_skip()
  fx <- .hz66_fixture(5)
  plan <- fx$plan
  rule <- list(min_expected_events = 5, exempt = FALSE)
  for (k in seq_len(nrow(plan$ett))) {
    eid <- plan$ett$ett_id[k]
    fu <- as.numeric(plan$ett$follow_up[k])
    enr <- swereg::qs2_read(file.path(fx$dir, plan$ett$file_analysis[k]))
    record_on <- function(w) {
      return(swereg:::.tte_horizon_record(
        enr$data,
        w,
        fu,
        rule,
        "rd_intervention",
        "tstart",
        "tstop"
      ))
    }
    want <- record_on("analysis_weight_pp_trunc")
    other <- record_on("analysis_weight_pp")
    expect_equal(plan$results_ett[[eid]]$horizon_pp, want, info = eid)
    # The two weights give different windows, so a record of the untruncated
    # weight cannot pass the line above.
    expect_false(isTRUE(all.equal(other$windows, want$windows)), info = eid)
  }

  # The untruncated item computes no record and returns none.
  out <- swereg:::.s3_ett_worker(
    analysis_path = file.path(fx$dir, "analysis_104.qs2"),
    method = "risk_difference",
    weight_col = "analysis_weight_pp",
    ett_id = "ETT00002",
    n_threads = 1L,
    subgroup_var = NULL,
    conf_level = 0.95,
    horizon_rule = rule,
    follow_up = 104
  )
  expect_true(all(c("rd_pp", "rd_curve_pp") %in% names(out)))
  expect_false(any(startsWith(names(out), "horizon_")))
})

test_that("#66 the Weight truncation (PP) sheet shows the RD, its interval and the NNT for each weight", {
  .hz66_skip()
  fx <- .hz66_fixture(5)
  m <- .hz66_cells(fx$xlsx, "Weight truncation (PP)")
  h <- which(m[, 1L] == "Enrollment")[1L]
  meas <- c(
    "Events (int)",
    "PY (int)",
    "Rate/100k (int)",
    "Events (cmp)",
    "PY (cmp)",
    "Rate/100k (cmp)",
    "IRR",
    "95% CI",
    "p-value"
  )
  rd_h <- c(
    "Risk difference per 10,000",
    "Risk difference 95% CI",
    "NNT (95% CI)"
  )
  ids <- c(
    "Enrollment",
    "Intervention",
    "Comparator",
    "Outcome",
    "Follow-up (weeks)"
  )
  # The header names and their positions: columns 15 to 17 and 27 to 29.
  expect_identical(ncol(m), 29L)
  expect_identical(unname(m[h, ]), c(ids, meas, rd_h, meas, rd_h))
  expect_identical(
    unname(m[h - 1L, c(6L, 18L)]),
    c("Truncated weights", "Untruncated weights")
  )

  e <- fx$plan$get_estimates()
  body <- m[-seq_len(h), , drop = FALSE]
  for (side in list(
    list(w = "truncated", col = 15L),
    list(w = "untruncated", col = 27L)
  )) {
    for (eid in c("ETT00001", "ETT00002")) {
      fu <- fx$plan$ett[ett_id == eid]$follow_up
      cells <- body[body[, 5L] == as.character(fu), , drop = TRUE]
      x <- e[ett_id == eid & estimand == "pp" & weights == side$w]
      lab <- paste(side$w, eid)
      got <- unname(cells[side$col + 0:2])
      if (eid == "ETT00002") {
        expect_identical(got, rep("NE", 3L), info = lab)
        next
      }
      expect_equal(
        as.numeric(got[1L]),
        x$rd * 10000,
        tolerance = 1e-10,
        info = lab
      )
      expect_identical(
        got[2L],
        sprintf("%+.2f to %+.2f", x$rd_lo * 10000, x$rd_hi * 10000),
        info = lab
      )
      expect_identical(
        got[3L],
        swereg:::.tte_nntb_cell(
          x$nnt,
          x$nnt_lo,
          x$nnt_hi,
          nnt_direction = x$nnt_direction
        ),
        info = lab
      )
      expect_match(got[3L], "^NNT[BH] [0-9,]+ \\([0-9,]+ to [0-9,]+\\)$")
    }
  }
  # The two weights give two different cells.
  r1 <- body[body[, 5L] == "52", , drop = TRUE]
  expect_false(identical(r1[[15L]], r1[[27L]]))
})

test_that("#66 a result saved without rd_pp exports, and its untruncated RD reads not computed", {
  .hz66_skip()
  fx <- .hz66_fixture(5)
  plan <- fx$plan
  old <- plan$results_ett
  on.exit(plan$results_ett <- old, add = TRUE)
  # swereg 27.3.9 stored no `rd_pp` and no `rd_curve_pp`.
  plan$results_ett <- lapply(old, function(r) {
    r$rd_pp <- NULL
    r$rd_curve_pp <- NULL
    return(r)
  })
  out_dir <- file.path(fx$dir, "saved_2739")
  dir.create(out_dir)
  xlsx <- file.path(out_dir, "tables.xlsx")
  expect_no_error(suppressMessages(suppressWarnings(
    plan$export_tables(path = xlsx)
  )))

  e <- plan$get_estimates()
  u <- e[estimand == "pp" & weights == "untruncated"]
  expect_identical(nrow(u), 2L)
  expect_false(any(u$rd_stored))
  ro <- qs2::qs_read(file.path(out_dir, "results.qs2"))
  expect_false(any(
    ro$estimates[estimand == "pp" & weights == "untruncated"]$rd_stored
  ))
  expect_identical(
    nrow(ro$curves[estimand == "pp" & weights == "untruncated"]),
    0L
  )

  m <- .hz66_cells(xlsx, "Weight truncation (PP)")
  h <- which(m[, 1L] == "Enrollment")[1L]
  body <- m[-seq_len(h), , drop = FALSE]
  expect_identical(nrow(body), 2L)
  for (r in seq_len(nrow(body))) {
    expect_identical(
      unname(body[r, 27:29]),
      rep("not computed", 3L),
      info = paste("row", r)
    )
  }
  # The truncated block is the block of the export that held `rd_pp`.
  full <- .hz66_cells(fx$xlsx, "Weight truncation (PP)")
  expect_identical(body[, 1:26], full[-seq_len(h), 1:26, drop = FALSE])
})
