#!/usr/bin/env Rscript

# Equivalence capture for the TTEEnrollment estimation code.
#
# Run it from the package root:
#   Rscript dev/equivalence/p10-estimation.R capture /tmp/p10-before.rds
#   Rscript dev/equivalence/p10-estimation.R compare /tmp/p10-before.rds
#
# `capture` builds two fixtures under /tmp. It records ten estimation
# values and writes them to the output file. `compare` rebuilds the same two
# fixtures and reports identical() against the stored baseline, one value at a
# time. It exits with status 1 when any value differs.
#
# Every value here is an ESTIMATE a study reports. A refactor of the
# estimation code MUST leave all ten bit-identical. A moved decimal is a
# different scientific result, and nothing downstream reports it as an error.
#
# Fixture A is the `helper-tte_scenarios.R` build: scenario s1 at N = 2000,
# pushed through `$s2_ipw()`, `$s3_truncate_weights()` and
# `$s4_prepare_for_analysis(estimand = "itt")`. It carries no `trial_id`, so
# it drives the five single-trial methods.
#
# Fixture B is a small multi-trial enrollment, built as in
# `test-tte_effect_modification.R` with a `trial_id` column added. The three
# stratified methods need more than one trial. `$heterogeneity_test()` stops
# without a `trial_id` column, and the calendar-time term of the other two is
# dead without one.
#
# The fixtures write nothing to disk. `$survival_curve()` is called with no
# `save_path`, so it returns the curve data and builds no plot.

.libPaths(c("/tmp/plan-baseline-lib", .libPaths()))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !args[[1]] %in% c("capture", "compare")) {
  stop(
    "usage: p10-estimation.R capture <out.rds> | compare <baseline.rds>",
    call. = FALSE
  )
}
mode <- args[[1]]
path <- args[[2]]

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the swereg package root.", call. = FALSE)
}
pkg <- read.dcf("DESCRIPTION")[1, "Package"]
if (!identical(unname(pkg), "swereg")) {
  stop("DESCRIPTION names '", pkg, "', not 'swereg'.", call. = FALSE)
}

suppressMessages(pkgload::load_all(".", quiet = TRUE))

progressr::handlers("void")

# ---------------------------------------------------------------------------
# Fixture A: the single-trial analysis-ready enrollment
# ---------------------------------------------------------------------------

# A copy of `scen_simulate("s1", ...)` and `tte_build_long()` from
# tests/testthat/. The test helpers are not on the load path of a script. A
# baseline is only meaningful while its generator stays byte-identical, so the
# generator lives here.

.P10_T <- 20L
.P10_LOR <- -0.7
.P10_PERSIST <- 8

p10_simulate_s1 <- function(N = 2000L, seed = 42L) {
  set.seed(seed)
  L0 <- stats::rnorm(N)
  out <- vector("list", .P10_T)
  prev_A <- integer(N)
  for (t in 0:(.P10_T - 1L)) {
    logit_A <- if (t == 0) -0.3 else -3.0 + .P10_PERSIST * prev_A
    A <- stats::rbinom(N, 1, stats::plogis(logit_A))
    Y <- stats::rbinom(N, 1, stats::plogis(-3.5 + .P10_LOR * A))
    out[[t + 1L]] <- data.table::data.table(
      id = seq_len(N),
      period = t,
      L0 = L0,
      A_t = A,
      Y_t = Y
    )
    prev_A <- A
  }
  d <- data.table::rbindlist(out)
  data.table::setorder(d, id, period)
  d[]
}

p10_build_long <- function(dt) {
  sw <- data.table::copy(dt)
  sw[, baseline_treatment := A_t[period == 0L][1L], by = id]
  sw[, baseline_L0 := L0]
  sw[, tstart := period]
  sw[, tstop := period + 1L]
  sw[, time_treatment := as.logical(A_t)]
  sw[, treatment_baseline := as.logical(baseline_treatment)]
  sw[, person_weeks := 1L]
  data.table::setnames(sw, "id", "enrollment_person_trial_id")
  data.table::setnames(sw, "Y_t", "event")
  sw[, list(
    enrollment_person_trial_id,
    tstart,
    tstop,
    treatment_baseline,
    time_treatment,
    event,
    person_weeks,
    baseline_L0
  )]
}

build_fixture_a <- function() {
  long <- p10_build_long(p10_simulate_s1())
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = "baseline_L0",
    follow_up_time = max(long$tstop)
  )
  trial <- TTEEnrollment$new(long, design)
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  trial$s4_prepare_for_analysis(
    outcome = "event",
    follow_up = max(long$tstop),
    estimand = "itt"
  )
  trial
}

# ---------------------------------------------------------------------------
# Fixture B: the small multi-trial enrollment
# ---------------------------------------------------------------------------

# `.sim_em()` from test-tte_effect_modification.R, at a smaller size, plus a
# `trial_id` column. Z modifies the treatment effect, and trial_id carries a
# calendar-time drift in the baseline rate.

p10_simulate_em <- function(
  N = 1500L,
  T_periods = 8L,
  n_trials = 5L,
  seed = 42L
) {
  set.seed(seed)
  Z <- stats::rbinom(N, 1, 0.5)
  A <- stats::rbinom(N, 1, 0.5)
  trial_id <- rep_len(seq_len(n_trials), N)
  out <- vector("list", T_periods)
  for (t in seq_len(T_periods)) {
    haz <- stats::plogis(
      -3 + log(2) * A + 0.3 * Z + log(2) * A * Z + 0.15 * (trial_id - 3)
    )
    out[[t]] <- data.table::data.table(
      id = seq_len(N),
      tstart = t - 1L,
      tstop = t,
      treatment = as.logical(A),
      Z = Z,
      trial_id = trial_id,
      event = stats::rbinom(N, 1, haz),
      person_weeks = 1L,
      w = 1
    )
  }
  data.table::rbindlist(out)[]
}

build_fixture_b <- function() {
  em <- p10_simulate_em()
  design <- TTEDesign$new(
    id_var = "id",
    person_id_var = "id",
    treatment_var = "treatment",
    outcome_vars = "event",
    confounder_vars = "Z",
    follow_up_time = 8L
  )
  TTEEnrollment$new(em, design, data_level = "trial")
}

# ---------------------------------------------------------------------------
# The ten values
# ---------------------------------------------------------------------------

VALUE_NAMES <- c(
  "rates",
  "irr",
  "table1",
  "table1_worker",
  "risk_difference",
  "survival_curve",
  "irr_by_subgroup",
  "effect_modification_test",
  "heterogeneity_test",
  "irr_b"
)

# A stray progress bar or a `message()` would land in the compare report and
# make a real divergence hard to find.
quietly <- function(expr) {
  v <- NULL
  invisible(utils::capture.output(
    suppressMessages(suppressWarnings(v <- expr)),
    type = "output"
  ))
  v
}

capture_values <- function() {
  a <- quietly(build_fixture_a())
  b <- quietly(build_fixture_b())

  list(
    rates = quietly(a$rates("ipw_trunc")),
    irr = quietly(a$irr("ipw_trunc")),
    table1 = quietly(a$table1(ipw_col = "ipw_trunc", show_missing = "none")),
    # The plan's bypass route on the same object. `test-table1_smd_numeric.R`
    # pins that the two routes agree; the baseline pins what they agree ON.
    table1_worker = quietly(.s3_enrollment_table1(
      a,
      ipw_col = "ipw_trunc",
      show_missing = "none"
    )),
    risk_difference = quietly(a$risk_difference(
      "ipw_trunc",
      n_boot = 40L,
      seed = 1L
    )),
    survival_curve = quietly(a$survival_curve("ipw_trunc")),
    irr_by_subgroup = quietly(b$irr_by_subgroup("w", "Z")),
    effect_modification_test = quietly(b$effect_modification_test("w", "Z")),
    heterogeneity_test = quietly(b$heterogeneity_test("w")),
    # Fixture A carries no `trial_id`, so its IRR never reaches the
    # calendar-time spline branch. Fixture B has five trials and does.
    irr_b = quietly(b$irr("w"))
  )
}

# `.internal.selfref` is a live pointer into the process that built the table.
# A baseline read back from disk carries a nil pointer, and a fresh capture
# carries a live one. So `identical()` on the raw objects can never be TRUE,
# and the comparison could never go green. Removing it leaves every value,
# name, class and other attribute compared bit for bit.
strip_selfref <- function(x) {
  if (data.table::is.data.table(x)) {
    data.table::setattr(x, ".internal.selfref", NULL)
  }
  if (is.list(x)) {
    for (i in seq_along(x)) {
      strip_selfref(x[[i]])
    }
  }
  for (a in names(attributes(x))) {
    if (!identical(a, ".internal.selfref")) {
      strip_selfref(attr(x, a, exact = TRUE))
    }
  }
  invisible(x)
}

# A capture of NA values, or of the wrong shape, compares identical against
# another one just like it. The assertion below stops that from becoming a
# baseline.
assert_non_degenerate <- function(v) {
  stopifnot(
    identical(names(v), VALUE_NAMES),

    data.table::is.data.table(v$rates),
    nrow(v$rates) == 2L,
    !anyNA(v$rates$rate_per_100000py),
    all(v$rates$events_weighted > 0),

    data.table::is.data.table(v$irr),
    nrow(v$irr) == 1L,
    !is.na(v$irr$IRR),
    v$irr$IRR > 0,
    v$irr$IRR_lower < v$irr$IRR,
    v$irr$IRR < v$irr$IRR_upper,

    inherits(v$table1, "swereg_table1"),
    nrow(v$table1) >= 3L,
    "smd_numeric" %in% names(v$table1),
    !all(is.na(v$table1$smd_numeric)),
    # The two routes MUST agree, which is the property the three-layer split
    # exists to keep.
    identical(v$table1, v$table1_worker),

    data.table::is.data.table(v$risk_difference),
    nrow(v$risk_difference) == .P10_T,
    !anyNA(v$risk_difference$rd),
    is.matrix(attr(v$risk_difference, "rd_boot")),

    data.table::is.data.table(v$survival_curve),
    nrow(v$survival_curve) == 2L * .P10_T,
    !anyNA(v$survival_curve$surv),
    all(v$survival_curve$surv <= 1),

    data.table::is.data.table(v$irr_by_subgroup),
    nrow(v$irr_by_subgroup) == 3L,
    !anyNA(v$irr_by_subgroup$IRR),
    !is.na(attr(v$irr_by_subgroup, "em_pvalue")),

    is.list(v$effect_modification_test),
    !is.na(v$effect_modification_test$p_value),
    !is.na(v$effect_modification_test$ratio_of_irrs),
    nrow(v$effect_modification_test$interaction_coefs) == 1L,

    is.list(v$heterogeneity_test),
    !is.na(v$heterogeneity_test$p_value),
    v$heterogeneity_test$n_trials == 5L,
    nrow(v$heterogeneity_test$interaction_coefs) == 3L,

    data.table::is.data.table(v$irr_b),
    !is.na(v$irr_b$IRR),
    v$irr_b$IRR > 0
  )
  invisible(TRUE)
}

# One printable digest per value, so a compare report names WHICH number moved
# rather than only that something did.
digest_of <- function(nm, x) {
  switch(
    nm,
    rates = paste(
      sprintf(
        "ev=%.12g py=%.12g rate=%.12g",
        x$events_weighted,
        x$py_weighted,
        x$rate_per_100000py
      ),
      collapse = "  "
    ),
    irr = ,
    irr_b = sprintf(
      "IRR=%.12g lo=%.12g hi=%.12g p=%.12g",
      x$IRR,
      x$IRR_lower,
      x$IRR_upper,
      x$IRR_pvalue
    ),
    table1 = ,
    table1_worker = paste(
      sprintf("%.12g", x$smd_numeric[!is.na(x$smd_numeric)]),
      collapse = " "
    ),
    risk_difference = sprintf(
      "rd[last]=%.12g lo=%.12g hi=%.12g sum|rd|=%.12g",
      x$rd[nrow(x)],
      x$rd_lo[nrow(x)],
      x$rd_hi[nrow(x)],
      sum(abs(x$rd))
    ),
    survival_curve = sprintf(
      "min surv=%.12g sum hazard=%.12g",
      min(x$surv),
      sum(x$hazard, na.rm = TRUE)
    ),
    irr_by_subgroup = sprintf(
      "IRR=%s lo=%s em_p=%.12g",
      paste(sprintf("%.12g", x$IRR), collapse = ","),
      paste(sprintf("%.12g", x$IRR_lower), collapse = ","),
      attr(x, "em_pvalue")
    ),
    effect_modification_test = sprintf(
      "p=%.12g ratio=%.12g",
      x$p_value,
      x$ratio_of_irrs
    ),
    heterogeneity_test = sprintf(
      "p=%.12g n_trials=%d",
      x$p_value,
      x$n_trials
    ),
    "<no digest>"
  )
}

show_values <- function(v) {
  for (nm in VALUE_NAMES) {
    cat(sprintf("  %-26s %s\n", nm, digest_of(nm, v[[nm]])))
  }
}

# ---------------------------------------------------------------------------
# Modes
# ---------------------------------------------------------------------------

if (identical(mode, "capture")) {
  v <- capture_values()
  assert_non_degenerate(v)
  strip_selfref(v)
  saveRDS(v, path)
  cat(sprintf("CAPTURE: wrote %s\n", path))
  show_values(v)
  quit(status = 0L)
}

baseline <- readRDS(path)
assert_non_degenerate(baseline)
strip_selfref(baseline)
current <- capture_values()
assert_non_degenerate(current)
strip_selfref(current)

cat(sprintf("COMPARE: baseline %s\n", path))
cat(sprintf(
  "%-26s %-10s %-7s %s\n",
  "VALUE",
  "IDENTICAL",
  "NAMES",
  "ATTRIBUTES"
))
ok <- TRUE
for (nm in VALUE_NAMES) {
  b <- baseline[[nm]]
  c_ <- current[[nm]]
  same_value <- identical(b, c_)
  same_names <- identical(names(b), names(c_))
  same_attrs <- identical(attributes(b), attributes(c_))
  ok <- ok && same_value && same_names && same_attrs
  cat(sprintf("%-26s %-10s %-7s %s\n", nm, same_value, same_names, same_attrs))
}

if (!ok) {
  cat("\nDIVERGENT VALUES\n")
  for (nm in VALUE_NAMES) {
    b <- baseline[[nm]]
    c_ <- current[[nm]]
    if (identical(b, c_) && identical(attributes(b), attributes(c_))) {
      next
    }
    cat(sprintf("  %s\n", nm))
    cat(sprintf("    baseline: %s\n", digest_of(nm, b)))
    cat(sprintf("    current : %s\n", digest_of(nm, c_)))
  }
}

cat(sprintf("\nEQUIVALENCE: all ten values identical = %s\n", ok))
quit(status = if (ok) 0L else 1L)
