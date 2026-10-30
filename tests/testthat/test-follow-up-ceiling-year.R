# A follow-up that is not a whole number of years takes the verdict of the
# minimum-events rule at the next whole year (swereg#62).
#
# A part-year ETT has a follow-up that is not a multiple of 52 weeks. Its
# per-protocol decision is the decision of its source ETT: the ETT of the
# same enrollment and outcome at the next whole year. When the spec has no
# such follow-up, the grid adds it as a horizon-only ETT. The ITT decision
# follows the per-protocol decision, as for every ETT.
#
# Every panel below is `.ff_panel()` of helper-feasibility_follow_up.R, cut
# at the follow-up. Its smallest expected events per arm, per-protocol:
#   26 weeks, window 0-26:   14.3
#   52 weeks, window 0-52:   22.2
#   78 weeks, window 52-78:   5.9
#   104 weeks, window 52-104: 15.8
# So at a threshold of 20, 26 weeks fails on its own and 52 weeks passes. At
# 25, both fail. At 10, 78 weeks fails on its own and 104 weeks passes.
#
# The blocks follow the brief, (a) to (k):
#   (a), (b), (k): records built by hand, then `.s3_apply_horizon_rule()`;
#   (c), (d), (e): the grid, and the records of (c) and (d);
#   (j), (f), (g), (h): one real `$s3_analyze()` of a {26, 52} plan;
#   (c) also: one real `$s3_analyze()` of a {78} plan, and its export;
#   (i): the OSF text and TARGET item 6h.

skip_if_not_installed("data.table")
skip_if_not_installed("yaml")
skip_if_not_installed("qs2")

# ---- records built by hand ------------------------------------------------------

.CY_FIELDS <- c(
  "reportable_weeks",
  "withheld",
  "fail_lo",
  "fail_hi",
  "fail_arm",
  "status",
  "max_passing_cut",
  "exempt",
  "by_year"
)

.cy_rule <- function(k, exempt = FALSE) {
  rule <- list(min_expected_events = k)
  if (exempt) {
    rule$exempt <- TRUE
  }
  return(rule)
}

# One stored ETT result: an estimate in each ratio slot, and the horizon
# records of `.ff_panel()` cut at `fu`.
.cy_result <- function(fu, rule) {
  d <- .ff_panel()[tstop <= fu]
  irr <- function() {
    return(data.table::data.table(
      IRR = 1.5,
      IRR_lower = 1.1,
      IRR_upper = 2,
      IRR_pvalue = 0.01,
      irr_estimable = TRUE
    ))
  }
  res <- list(irr_pp_trunc = irr(), irr_pp = irr(), irr_itt = irr())
  weights <- c(pp = "analysis_weight_pp_trunc", itt = "ipw_trunc")
  for (est in names(weights)) {
    res[[paste0("horizon_", est)]] <- swereg:::.tte_horizon_record(
      d,
      weight_col = weights[[est]],
      follow_up = fu,
      rule = rule,
      treatment_var = "rd_intervention"
    )
  }
  return(res)
}

# One enrollment and one outcome, one ETT per follow-up, in the given order.
.cy_grid <- function(fu, horizon_only = rep(FALSE, length(fu))) {
  return(data.table::data.table(
    ett_id = sprintf("ETT%05d", seq_along(fu)),
    enrollment_id = "01",
    outcome_name = "Outcome A",
    follow_up = as.integer(fu),
    feasibility_horizon = horizon_only
  ))
}

# The records of `fu` at threshold `k`, before and after the parent merge.
.cy_apply <- function(fu, k, exempt = FALSE, horizon_only = NULL) {
  ett <- .cy_grid(fu, horizon_only %||% rep(FALSE, length(fu)))
  before <- lapply(fu, .cy_result, rule = .cy_rule(k, exempt))
  names(before) <- ett$ett_id
  after <- swereg:::.s3_apply_horizon_rule(before, ett$ett_id, ett)
  return(list(ett = ett, before = before, after = after))
}

.cy_irr <- function(res) {
  return(c(
    pp_trunc = res$irr_pp_trunc$IRR,
    pp = res$irr_pp$IRR,
    itt = res$irr_itt$IRR
  ))
}

test_that("(a) a 26-week estimate is reported when the 52-week ETT passes, although its own window fails", {
  x <- .cy_apply(c(26, 52), k = 20)
  # The discriminating panel: the 26-week window fails on its own.
  expect_true(x$before$ETT00001$horizon_pp$withheld)
  expect_false(x$before$ETT00002$horizon_pp$withheld)

  pp <- x$after$ETT00001$horizon_pp
  expect_false(pp$withheld)
  expect_identical(pp$decided_by_ett, "ETT00002")
  expect_identical(pp$decided_by_weeks, 52)
  expect_identical(pp$reportable_weeks, 26)
  expect_identical(
    .cy_irr(x$after$ETT00001),
    c(pp_trunc = 1.5, pp = 1.5, itt = 1.5)
  )
  # The ITT record follows its own per-protocol record.
  expect_false(x$after$ETT00001$horizon_itt$withheld)
  expect_identical(x$after$ETT00001$horizon_itt$decided_by, "pp")
})

test_that("(a) the grid order does not change the decision", {
  x <- .cy_apply(c(52, 26), k = 20)
  expect_identical(x$after$ETT00002$horizon_pp$decided_by_ett, "ETT00001")
  expect_false(x$after$ETT00002$horizon_pp$withheld)
  expect_false(anyNA(.cy_irr(x$after$ETT00002)))
})

# Rule 4: pass 1 judges the source before pass 2 copies it. With 26 weeks
# first in the grid and no decision on the 52-week ETT, the stop names the
# failed worker of the 52-week ETT, which is the cause.
test_that("(j) with 26 before 52 in the grid, a 52-week ETT without a decision stops on its own message", {
  ett <- .cy_grid(c(26, 52))
  results <- list(
    ETT00001 = .cy_result(26, .cy_rule(20)),
    ETT00002 = .cy_result(52, .cy_rule(20))
  )
  results$ETT00002$horizon_pp <- NULL
  expect_error(
    swereg:::.s3_apply_horizon_rule(results, ett$ett_id, ett),
    paste0(
      "s3_analyze(): ETT ETT00002 has no decision of the minimum-events ",
      "rule for the per-protocol estimand. Its risk-difference worker ",
      "failed or was skipped"
    ),
    fixed = TRUE
  )
})

test_that("(a) a missing source result stops, naming the part-year ETT, the source ETT and $s3_analyze()", {
  ett <- .cy_grid(c(26, 52))
  results <- list(ETT00001 = .cy_result(26, .cy_rule(20)))
  expect_error(
    swereg:::.s3_apply_horizon_rule(results, "ETT00001", ett),
    paste0(
      "s3_analyze(): ETT ETT00001 has a follow-up of 26 weeks, which is not ",
      "a whole number of years. Its estimate is reported when the ",
      "minimum-events rule passes at 52 weeks, in ETT ETT00002. The plan ",
      "holds no decision of the rule for ETT ETT00002. Run $s3_analyze() on ",
      "ETT ETT00002 first."
    ),
    fixed = TRUE
  )
})

test_that("(b) a 26-week estimate is withheld when the 52-week ETT fails, ITT too", {
  x <- .cy_apply(c(26, 52), k = 25)
  expect_true(x$after$ETT00002$horizon_pp$withheld)
  pp <- x$after$ETT00001$horizon_pp
  expect_true(pp$withheld)
  expect_identical(pp$decided_by_ett, "ETT00002")
  expect_identical(pp$status, x$after$ETT00002$horizon_pp$status)
  expect_true(all(is.na(.cy_irr(x$after$ETT00001))))
  expect_true(x$after$ETT00001$horizon_itt$withheld)
})

test_that("(k) an exempt enrollment's 26-week estimate is reported when the source fails", {
  x <- .cy_apply(c(26, 52), k = 25, exempt = TRUE)
  pp <- x$after$ETT00001$horizon_pp
  expect_true(pp$withheld)
  expect_true(pp$exempt)
  expect_identical(pp$status, "exempt from minimum-events rule")
  expect_identical(pp$decided_by_ett, "ETT00002")
  expect_identical(
    .cy_irr(x$after$ETT00001),
    c(pp_trunc = 1.5, pp = 1.5, itt = 1.5)
  )
})

# ---- (c), (d), (e): the grid ----------------------------------------------------

.cy_fu <- function(weeks, labels) {
  return(lapply(seq_along(weeks), function(i) {
    return(list(label = labels[i], weeks = as.integer(weeks[i])))
  }))
}

.cy_spec <- function(weeks, labels, w = NULL, min_events = 5) {
  spec <- .ff_spec(w, min_events = min_events)
  spec$follow_up <- .cy_fu(weeks, labels)
  return(spec)
}

.cy_grid_of <- function(spec, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  return(.ff_plan(spec, root))
}

test_that("(c) a spec with 78 weeks only gets one horizon-only 104-week ETT per enrollment and outcome, last", {
  spec <- .cy_spec(78, "18 months")
  plan <- .cy_grid_of(spec)
  ett <- plan$ett
  expect_identical(ett$ett_id, sprintf("ETT%05d", 1:4))
  expect_identical(ett$enrollment_id, c("01", "02", "01", "02"))
  expect_identical(ett$follow_up, c(78L, 78L, 104L, 104L))
  expect_identical(ett$feasibility_horizon, c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(
    swereg:::.plan_ett_reported(plan)$ett_id,
    c("ETT00001", "ETT00002")
  )
  ceil <- swereg:::.tte_ceiling_follow_ups(spec, "01")
  expect_length(ceil, 1L)
  expect_identical(ceil[[1]]$label, "2 years")
  expect_identical(ceil[[1]]$weeks, 104L)
  # s1 enrolls through the 104 weeks. Reported outputs stop at 78 weeks.
  es <- plan$enrollment_spec(1L)
  expect_identical(es$enrollment_id, "01")
  expect_identical(es$design$follow_up_time, 104L)
  expect_identical(es$reported_follow_up_time, 78L)
})

test_that("(c) the 78-week record carries every decision field of the 104-week record", {
  x <- .cy_apply(c(78, 104), k = 10, horizon_only = c(FALSE, TRUE))
  own <- x$before$ETT00001$horizon_pp
  src <- x$after$ETT00002$horizon_pp
  # The discriminating panel: the 78-week record decides otherwise on its own.
  expect_identical(own$fail_hi, 78)
  expect_identical(src$fail_hi, NA_real_)
  expect_false(identical(own$by_year, src$by_year))

  pp <- x$after$ETT00001$horizon_pp
  for (f in setdiff(.CY_FIELDS, "reportable_weeks")) {
    expect_identical(pp[[f]], src[[f]], info = f)
  }
  expect_identical(src$reportable_weeks, 104)
  expect_identical(pp$reportable_weeks, 78)
  expect_identical(pp$decided_by_ett, "ETT00002")
  expect_identical(pp$decided_by_weeks, 104)
  # Its own rule, weight, follow-up and windows stay.
  for (f in c("rule", "weight_col", "follow_up", "windows")) {
    expect_identical(pp[[f]], own[[f]], info = f)
  }
  expect_false(anyNA(.cy_irr(x$after$ETT00001)))
  # The ITT record follows the per-protocol record.
  for (f in .CY_FIELDS) {
    expect_identical(x$after$ETT00001$horizon_itt[[f]], pp[[f]], info = f)
  }
})

test_that("(d) a spec with 78 and 104 weeks gets no extra ETT, and 78 follows the reported 104", {
  plan <- .cy_grid_of(.cy_spec(c(78, 104), c("18 months", "2 years")))
  expect_identical(nrow(plan$ett), 4L)
  expect_false(any(plan$ett$feasibility_horizon))
  sources <- swereg:::.tte_horizon_sources(plan$ett)
  expect_identical(names(sources), c("ETT00001", "ETT00003"))
  expect_identical(sources$ETT00001$ett_id, "ETT00002")
  expect_identical(sources$ETT00003$ett_id, "ETT00004")

  x <- .cy_apply(c(78, 104), k = 10)
  expect_true(x$before$ETT00001$horizon_pp$withheld)
  expect_identical(x$after$ETT00001$horizon_pp$decided_by_ett, "ETT00002")
  expect_false(x$after$ETT00001$horizon_pp$withheld)
  expect_identical(
    .cy_irr(x$after$ETT00001),
    c(pp_trunc = 1.5, pp = 1.5, itt = 1.5)
  )
})

test_that("(e) with feasibility_follow_up_weeks 52, the feasibility ETT is the source of 26 weeks", {
  plan <- .cy_grid_of(.cy_spec(26, "6 months", w = 52L))
  ett <- plan$ett
  expect_identical(ett$follow_up, c(26L, 26L, 52L, 52L))
  expect_identical(ett$feasibility_horizon, c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(anyDuplicated(ett[, c("enrollment_id", "follow_up")]), 0L)
  sources <- swereg:::.tte_horizon_sources(ett)
  expect_identical(sources$ETT00001$ett_id, "ETT00003")
  expect_identical(sources$ETT00002$ett_id, "ETT00004")

  x <- .cy_apply(c(26, 52), k = 20, horizon_only = c(FALSE, TRUE))
  expect_identical(x$after$ETT00001$horizon_pp$decided_by_ett, "ETT00002")
  expect_false(anyNA(.cy_irr(x$after$ETT00001)))
})

# ---- one real s3 run -------------------------------------------------------------

# s3 through the REAL batch runner on the plan of `weeks` and `labels`, at
# threshold `k`, then the workbook. Built once per file and per key.
.cy_cache <- new.env()
.cy_fixture <- function(key, weeks, labels, k) {
  if (!is.null(.cy_cache[[key]]) && dir.exists(.cy_cache[[key]]$root)) {
    return(.cy_cache[[key]])
  }
  root <- tempfile(paste0("cy_", key, "_"))
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = testthat::teardown_env())
  plan <- .ff_plan(.cy_spec(weeks, labels, min_events = k), root)
  dir <- file.path(root, "tteplan")
  .ff_write_files(plan, dir, .ff_panel())
  plan$output_dir <- dir
  tte_stamp_stages(plan)
  out <- utils::capture.output(suppressWarnings(plan$s3_analyze(
    output_dir = dir,
    n_workers = 3L,
    swereg_dev_path = swereg:::.swereg_dev_path()
  )))
  xlsx <- file.path(root, "results", "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
  .cy_cache[[key]] <- list(root = root, dir = dir, plan = plan, xlsx = xlsx)
  return(.cy_cache[[key]])
}

# The {26, 52} plan at threshold 20: 26 weeks fails on its own, 52 passes.
.cy_fx_26 <- function() {
  return(.cy_fixture("y26", c(26, 52), c("6 months", "1 year"), 20))
}

# The {78} plan at threshold 10: 78 weeks fails on its own, 104 passes.
.cy_fx_78 <- function() {
  return(.cy_fixture("y78", 78, "18 months", 10))
}

.cy_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("survey")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
}

test_that("(j) after one full $s3_analyze() with 26 before 52 in the grid, the 26-week IRR is reported", {
  .cy_skip()
  plan <- .cy_fx_26()$plan
  expect_identical(plan$ett$follow_up, c(26L, 52L, 26L, 52L))
  expect_false(any(plan$ett$feasibility_horizon))
  for (pair in list(c("ETT00001", "ETT00002"), c("ETT00003", "ETT00004"))) {
    part <- plan$results_ett[[pair[1]]]
    src <- plan$results_ett[[pair[2]]]
    # Its own window fails, and the 52-week ETT passes.
    expect_false(any(part$horizon_pp$windows$pass), info = pair[1])
    expect_false(src$horizon_pp$withheld, info = pair[2])
    expect_identical(part$horizon_pp$decided_by_ett, pair[2])
    expect_false(part$horizon_pp$withheld, info = pair[1])
    expect_false(anyNA(part$irr_pp_trunc$IRR), info = pair[1])
    expect_false(anyNA(part$irr_itt$IRR), info = pair[1])
  }
  est <- plan$get_estimates()
  expect_false(anyNA(est$IRR[est$ett_id == "ETT00001"]))
})

test_that("(f) export passes on the fresh run, and stops with the source class on each stale record", {
  .cy_skip()
  fx <- .cy_fx_26()
  plan <- fx$plan
  saved <- plan$results_ett
  withr::defer(plan$results_ett <- saved)
  expect_true(swereg:::.tte_assert_horizon_rule_current(
    plan,
    "export_tables()"
  ))
  msg <- paste0(
    "export_tables(): the stored results of 1 ETT(s) with a follow-up that ",
    "is not a whole number of years do not hold the current decision of ",
    "their source ETT, the ETT at the next whole year: "
  )
  path <- file.path(fx$root, "results", "stale.xlsx")

  # (f1) decided_by_ett removed.
  plan$results_ett$ETT00001$horizon_pp$decided_by_ett <- NULL
  expect_error(
    plan$export_tables(path = path),
    paste0(msg, "ETT00001 (it names no source ETT)."),
    fixed = TRUE
  )

  # (f2) decided_by_ett names a different ETT.
  plan$results_ett <- saved
  plan$results_ett$ETT00001$horizon_pp$decided_by_ett <- "ETT00004"
  expect_error(
    plan$export_tables(path = path),
    paste0(
      msg,
      "ETT00001 (it names ETT ETT00004, and the source ETT is ETT00002)."
    ),
    fixed = TRUE
  )

  # (f3) the stored verdict of the source flipped after s3.
  plan$results_ett <- saved
  plan$results_ett$ETT00002$horizon_pp$withheld <- TRUE
  expect_error(
    plan$export_tables(path = path),
    paste0(msg, "ETT00001 (its withheld differ from ETT ETT00002)."),
    fixed = TRUE
  )
  expect_false(file.exists(path))
})

test_that("(g) $s3_analyze() on the part-year ETTs alone reads the source of the earlier full run", {
  .cy_skip()
  plan <- .cy_fx_26()$plan
  saved <- plan$results_ett
  withr::defer(plan$results_ett <- saved)
  src_before <- plan$results_ett$ETT00002
  utils::capture.output(suppressWarnings(plan$s3_analyze(
    ett_ids = c("ETT00001", "ETT00003"),
    n_workers = 2L,
    swereg_dev_path = swereg:::.swereg_dev_path()
  )))
  # The source was not recomputed.
  expect_identical(
    plan$results_ett$ETT00002$computed_at,
    src_before$computed_at
  )
  part <- plan$results_ett$ETT00001
  expect_false(identical(part$computed_at, saved$ETT00001$computed_at))
  expect_false(any(part$horizon_pp$windows$pass))
  expect_identical(part$horizon_pp$decided_by_ett, "ETT00002")
  expect_false(part$horizon_pp$withheld)
  expect_false(anyNA(part$irr_pp_trunc$IRR))
  expect_true(swereg:::.tte_assert_horizon_rule_current(
    plan,
    "export_tables()"
  ))
})

test_that("(h) $s3_feasibility() gives the 26-week record the decision of the 52-week record", {
  .cy_skip()
  plan <- .cy_fx_26()$plan
  utils::capture.output(plan$s3_feasibility(
    n_workers = 2L,
    swereg_dev_path = swereg:::.swereg_dev_path()
  ))
  rec <- plan$feasibility$records
  for (pair in list(c("ETT00001", "ETT00002"), c("ETT00003", "ETT00004"))) {
    pp <- rec[[pair[1]]]$pp
    src <- rec[[pair[2]]]$pp
    expect_false(any(pp$windows$pass), info = pair[1])
    expect_false(pp$withheld, info = pair[1])
    expect_identical(pp$decided_by_ett, pair[2])
    expect_identical(pp$decided_by_weeks, 52)
    for (f in setdiff(.CY_FIELDS, "reportable_weeks")) {
      expect_identical(pp[[f]], src[[f]], info = paste(pair[1], f))
    }
    expect_identical(pp$reportable_weeks, 26)
    expect_identical(rec[[pair[1]]]$itt$withheld, pp$withheld)
    # The feasibility decision is the decision s3 stored.
    expect_identical(
      pp$withheld,
      plan$results_ett[[pair[1]]]$horizon_pp$withheld
    )
  }
})

test_that("(c) the 104-week ETT has no estimate in a reported output, and the sheet note names it as the deciding whole year", {
  .cy_skip()
  fx <- .cy_fx_78()
  plan <- fx$plan
  expect_identical(
    swereg:::.plan_ett_reported(plan)$ett_id,
    c("ETT00001", "ETT00002")
  )
  est <- plan$get_estimates()
  expect_gt(nrow(est), 0L)
  expect_identical(sort(unique(est$ett_id)), c("ETT00001", "ETT00002"))
  expect_false(anyNA(est$IRR[est$ett_id == "ETT00001"]))
  expect_false("irr_pp_trunc" %in% names(plan$results_ett$ETT00003))
  pp <- plan$results_ett$ETT00001$horizon_pp
  expect_identical(pp$decided_by_ett, "ETT00003")
  expect_identical(pp$fail_hi, NA_real_)
  expect_false(all(pp$windows$pass))

  for (path in c(fx$xlsx, file.path(dirname(fx$xlsx), "feasibility.xlsx"))) {
    for (sheet in FF_MIN_SHEETS) {
      note <- .ff_cells(path, sheet)[3, 1]
      # Year 1 has no ETT of its own, so it reads the 104-week ETT.
      expect_identical(
        note,
        paste0(
          "Years without their own follow-up are read from the shortest ",
          "longer follow-up. The 104-week follow-up is the whole year that ",
          "decides the 78-week follow-up; no estimate is reported for the ",
          "104-week follow-up."
        ),
        info = paste(basename(path), sheet)
      )
      expect_false(grepl("feasibility", note, fixed = TRUE))
    }
  }
})

# ---- (i): the prose --------------------------------------------------------------

CY_OSF_RULE <- "If a horizon is not a whole number of years, its estimate is reported when the estimate at the next whole year passes the rule."
CY_TARGET_RULE <- "If a horizon was not a whole number of years, its estimate was reported when the estimate at the next whole year passed the rule."

test_that("(i) the OSF text and TARGET item 6h carry the part-year sentence and not the partial-year sentence", {
  .cy_skip()
  fx <- .cy_fx_26()
  osf <- suppressWarnings(swereg::tteplan_osf_text(
    file.path(fx$root, "spec", "spec_v001.yaml")
  ))
  x <- osf[["Inference criteria"]]
  expect_match(
    x,
    paste0(
      "Follow-up is cut into years of 52 weeks from time zero. ",
      CY_OSF_RULE,
      " The 6-month estimate is reported when the 1-year estimate passes ",
      "the rule. The expected events of an arm"
    ),
    fixed = TRUE
  )
  expect_false(grepl("can be shorter", x, fixed = TRUE))
  expect_false(grepl("The last year ends at the horizon", x, fixed = TRUE))

  txt <- spr_report_checklist(fx$plan)
  h6 <- paste(spr_report_item(txt, "6h", "7a-h"), collapse = " ")
  expect_match(
    gsub("\\s+", " ", h6),
    paste0(
      "Follow-up was cut into years of 52 weeks from time zero. ",
      CY_TARGET_RULE
    ),
    fixed = TRUE
  )
  expect_false(grepl("last year ended", gsub("\\s+", " ", h6), fixed = TRUE))
})

# At threshold 0 the rule withholds no estimate, so the text adds no
# part-year sentence (Richard, 2026-10-30).
.cy_osf_inference <- function(weeks, labels, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  plan <- .ff_plan(.cy_spec(weeks, labels, min_events = 0), root)
  osf <- suppressWarnings(swereg::tteplan_osf_text(
    file.path(root, "spec", "spec_v001.yaml")
  ))
  return(osf[["Inference criteria"]])
}

test_that("(i) at threshold 0, the OSF inference text of a {26, 52} spec equals that of a {52} spec", {
  part <- .cy_osf_inference(c(26, 52), c("6 months", "1 year"))
  whole <- .cy_osf_inference(52, "1 year")
  expect_identical(part, whole)
  expect_match(part, "so it withholds no estimate.", fixed = TRUE)
})
