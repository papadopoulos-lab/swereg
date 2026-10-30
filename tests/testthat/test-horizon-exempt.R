# The per-enrollment exemption from the minimum-events rule.
#
# An enrollment that sets `minimum_events_rule_exempt: true` is exempt. s3
# computes the rule for its ETTs and stores the verdict, and withholds none
# of their estimates. The fixture holds two enrollments of one 104-week ETT
# each, on the same panel. Both fail the rule at `min_events = 5`: the
# intervention arm holds a thin tail in the second follow-up year.
# Enrollment "02" is exempt, and enrollment "01" is not.
#
# The blocks follow the brief, (a) to (f):
#   (a) the spec key: a non-logical value stops, and an absent key reads FALSE;
#   (b) s3 reports the exempt ETT and withholds the other one;
#   (c) the workbook and the forest data;
#   (c2) get_estimates() and the Summary sheet;
#   (c3) subgroups, the sensitivity sheet and the survival renderer;
#   (d) a flipped key after s3 makes the writers stop;
#   (e) the generated prose;
#   (f) the console report of s3.

skip_if_not_installed("data.table")
skip_if_not_installed("yaml")

# ---- the fixture plan --------------------------------------------------------

# The panel of `.hz_panel()` in test-horizon-rule-s3.R. One trial per person,
# 4-week intervals to `end_wk`. Every intervention person stops at week 52,
# except the first `tail_keep`, so the intervention arm holds a thin tail in
# the second follow-up year.
.hx_panel <- function(n = 160L, tail_keep = 4L, end_wk = 104L, seed = 7L) {
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
  d[, analysis_weight_pp := w * 1.1]
  d[, ipw := 1]
  d[, ipw_trunc := w]
  return(d)
}

# The fixture spec: two enrollments and one subgroup. `exempt` is the value
# of `minimum_events_rule_exempt` on enrollment "02", or `NULL` for no key.
.hx_spec <- function(exempt = TRUE, min_events = 5) {
  enr <- function(id, name) {
    return(list(
      id = id,
      name = name,
      treatment = list(
        arms = list(intervention = "Treated", comparator = "Untreated")
      )
    ))
  }
  e2 <- enr("02", "Enrollment two")
  if (!is.null(exempt)) {
    e2$minimum_events_rule_exempt <- exempt
  }
  return(list(
    study = list(
      title = "Fixture",
      implementation = list(project_prefix = "hx")
    ),
    enrollments = list(enr("01", "Enrollment one"), e2),
    subgroups = list(list(
      name = "Subgroup",
      implementation = list(variable = "rd_sg")
    )),
    min_expected_events_per_arm_per_year = min_events
  ))
}

# Write the two enrollments, one 104-week ETT each, on the same panel:
# ETT00001 of enrollment "01" and ETT00002 of enrollment "02". Returns the
# plan, with the stage stamps set and nothing computed.
.hx_build <- function(dir, exempt = TRUE) {
  d <- .hx_panel()
  design <- swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "rd_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "rd_age_continuous",
    follow_up_time = 104L
  )
  for (eid in c("01", "02")) {
    enr <- swereg::TTEEnrollment$new(d, design, data_level = "trial")
    for (f in paste0(c("analysis_", "analysis_itt_", "raw_"), eid, ".qs2")) {
      qs2::qs_save(enr, file.path(dir, f))
    }
  }
  ett <- data.table::data.table(
    enrollment_id = c("01", "02"),
    ett_id = c("ETT00001", "ETT00002"),
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    outcome_role = "primary",
    follow_up = 104L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "id",
    treatment_var = "rd_intervention",
    file_imp = c("imp_01.qs2", "imp_02.qs2"),
    file_raw = c("raw_01.qs2", "raw_02.qs2"),
    file_analysis = c("analysis_01.qs2", "analysis_02.qs2"),
    file_analysis_itt = c("analysis_itt_01.qs2", "analysis_itt_02.qs2"),
    description = c("ETT00001", "ETT00002")
  )
  data.table::set(
    ett,
    j = "subgroup_vars",
    value = list(list("rd_sg", "rd_sg"))
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "hx",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$output_dir <- dir
  plan$spec <- .hx_spec(exempt)
  tte_stamp_stages(plan)
  return(plan)
}

# s3 through the REAL batch runner (worker subprocesses), then the workbook.
# Built once per file and per value of `exempt`.
.hx_cache <- new.env()
.hx_fixture <- function(exempt = TRUE) {
  key <- if (is.null(exempt)) "none" else as.character(exempt)
  if (!is.null(.hx_cache[[key]])) {
    return(.hx_cache[[key]])
  }
  dir <- tempfile("hx_s3_")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = testthat::teardown_env())
  plan <- .hx_build(dir, exempt)
  out <- utils::capture.output(suppressWarnings(plan$s3_analyze(
    output_dir = dir,
    n_workers = 3L,
    swereg_dev_path = swereg:::.swereg_dev_path()
  )))
  xlsx <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
  .hx_cache[[key]] <- list(dir = dir, plan = plan, xlsx = xlsx, out = out)
  return(.hx_cache[[key]])
}

.hx_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("survey")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
}

# One sheet as a character data.frame. The header row is the first row that
# holds the cell `header`. Returns the rows under it, named by it.
.hx_sheet <- function(xlsx, sheet, header) {
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

# The rows 1 and 2 of a sheet: the title and the note.
.hx_head <- function(xlsx, sheet) {
  m <- openxlsx::read.xlsx(
    xlsx,
    sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    rows = 1:2,
    cols = 1L
  )
  return(as.character(m[[1L]]))
}


# ---- (a): the spec key --------------------------------------------------------

# The fixture spec of helper-sparse_rule.R, with enrollments "01" and "02",
# and the key on enrollment "02" set to `value`. `NULL` writes no key, and
# `list(NULL)` writes the key with a null value.
.hx_spec_path <- function(value, env = parent.frame()) {
  spec <- spr_spec_list()
  if (!is.null(value)) {
    spec$enrollments[[2]]["minimum_events_rule_exempt"] <- list(value[[1L]])
  }
  dir <- withr::local_tempdir(.local_envir = env)
  return(spr_write_spec(spec, dir))
}

test_that("(a) an absent key reads as not exempt, and true reads as exempt", {
  ett <- data.table::data.table(
    enrollment_id = c("01", "02"),
    ett_id = c("ETT00001", "ETT00002"),
    file_analysis = c("a_01.qs2", "a_02.qs2"),
    follow_up = 52L
  )
  rule <- list(min_expected_events = 5)
  for (value in list(NULL, list(FALSE), list(TRUE))) {
    spec <- swereg::tteplan_read_spec(.hx_spec_path(value))
    want <- isTRUE(value[[1L]])
    lab <- if (is.null(value)) "absent" else as.character(value[[1L]])
    expect_false(
      swereg:::.tte_enrollment_rule_exempt(spec, "01"),
      info = lab
    )
    expect_identical(
      swereg:::.tte_enrollment_rule_exempt(spec, "02"),
      want,
      info = lab
    )
    a <- swereg:::.tte_s3_resolve_ett(ett, 1L, "D", rule, spec)
    b <- swereg:::.tte_s3_resolve_ett(ett, 2L, "D", rule, spec)
    expect_identical(a$rule, list(min_expected_events = 5, exempt = FALSE))
    expect_identical(b$rule, list(min_expected_events = 5, exempt = want))
    expect_identical(nrow(swereg::tteplan_check_spec(.hx_spec_path(value))), 0L)
  }
})

test_that("(a) a value that is not one logical stops, naming the enrollment id and the key", {
  bad <- list(
    number = list(1),
    string = list("yes"),
    na = list(NA),
    null = list(NULL),
    two = list(c(TRUE, FALSE))
  )
  for (nm in names(bad)) {
    path <- .hx_spec_path(bad[[nm]])
    expect_error(
      swereg::tteplan_read_spec(path),
      "enrollments[2] '02' has minimum_events_rule_exempt",
      fixed = TRUE,
      info = nm
    )
    expect_error(
      swereg::tteplan_read_spec(path),
      "minimum_events_rule_exempt MUST be one logical: true or false.",
      fixed = TRUE,
      info = nm
    )
    found <- swereg::tteplan_check_spec(path)
    expect_identical(found$kind, "enrollment_rule_exempt", info = nm)
    expect_identical(
      found$path,
      "$/enrollments[]/minimum_events_rule_exempt",
      info = nm
    )
    expect_match(found$message, "'02'", fixed = TRUE, info = nm)
  }
})


# ---- (b): s3 -------------------------------------------------------------------

test_that("(b) s3 reports the exempt ETT and withholds the other, and the record keeps the verdict", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
  e <- fx$plan$get_estimates()
  for (est in c("pp", "itt")) {
    x <- e[ett_id == "ETT00002" & estimand == est & weights == "truncated"]
    w <- e[ett_id == "ETT00001" & estimand == est & weights == "truncated"]
    expect_identical(nrow(x), 1L)
    expect_identical(nrow(w), 1L)
    for (nm in c("irr", "irr_lo", "irr_hi", "rd", "rd_lo", "rd_hi")) {
      expect_true(is.finite(x[[nm]]), info = paste(est, "exempt", nm))
      expect_true(is.na(w[[nm]]), info = paste(est, "withheld", nm))
    }
  }
  # The stored slots behind the accessor.
  r2 <- fx$plan$results_ett$ETT00002
  r1 <- fx$plan$results_ett$ETT00001
  for (s in c("irr_pp_trunc", "irr_pp", "irr_itt")) {
    expect_true(is.finite(r2[[s]]$IRR), info = s)
    expect_true(is.na(r1[[s]]$IRR), info = s)
  }

  # The exempt record keeps the computed verdict of the rule.
  for (est in c("pp", "itt")) {
    rec <- r2[[paste0("horizon_", est)]]
    expect_true(rec$withheld, info = est)
    expect_true(rec$exempt, info = est)
    expect_true(rec$rule$exempt, info = est)
    expect_identical(rec$status, "exempt from minimum-events rule")
    expect_false(startsWith(rec$status, "below"), info = est)
    expect_identical(rec$reportable_weeks, 52, info = est)
    expect_identical(rec$fail_lo, 52, info = est)
    expect_identical(rec$fail_arm, "intervention", info = est)
    expect_identical(rec$max_passing_cut, 2, info = est)
  }
  expect_identical(r2$horizon_itt$decided_by, "pp")

  # The record that is not exempt is the 27.3.0 record with `exempt = FALSE`
  # and, since 27.3.1, `by_year`.
  rec <- r1$horizon_pp
  expect_identical(
    names(rec),
    c(
      "rule",
      "weight_col",
      "follow_up",
      "windows",
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
  )
  expect_false(rec$exempt)
  expect_identical(rec$rule, list(min_expected_events = 5))
  expect_true(rec$withheld)
  expect_identical(
    rec$status,
    "below minimum-events rule: intervention arm, weeks 52-104"
  )
})

test_that("(b) without the key, s3 withholds both ETTs", {
  .hx_skip()
  fx <- .hx_fixture(NULL)
  e <- fx$plan$get_estimates()
  status <- "below minimum-events rule: intervention arm, weeks 52-104"
  for (id in c("ETT00001", "ETT00002")) {
    x <- e[ett_id == id]
    expect_identical(nrow(x), 3L, info = id)
    expect_true(all(is.na(x$irr)), info = id)
    expect_true(all(is.na(x$rd)), info = id)
    expect_identical(unique(x$horizon_status), status, info = id)
    expect_equal(unique(x$horizon_reportable_weeks), 52, info = id)
    for (est in c("pp", "itt")) {
      rec <- fx$plan$results_ett[[id]][[paste0("horizon_", est)]]
      expect_false(rec$exempt, info = paste(id, est))
      expect_identical(
        rec$rule,
        list(min_expected_events = 5),
        info = paste(id, est)
      )
    }
  }
})


# ---- (c), (c2), (c3): the exports ---------------------------------------------

# The columns of the results sheets that the rule withholds.
HX_RESULT_COLS <- c(
  "IRR",
  "95% CI",
  "p-value",
  "Risk difference per 10,000",
  "Risk difference 95% CI"
)

test_that("(c) the PP and ITT results sheets show numbers for the exempt ETT and NE for the other", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
  for (sh in c("PP results", "ITT results")) {
    s <- .hx_sheet(fx$xlsx, sh, "IRR")
    x <- s[s$Enrollment == "Enrollment two", ]
    w <- s[s$Enrollment == "Enrollment one", ]
    expect_identical(nrow(x), 1L, info = sh)
    expect_identical(nrow(w), 1L, info = sh)
    for (nm in HX_RESULT_COLS) {
      expect_false(x[[nm]] %in% c("NE", NA, ""), info = paste(sh, "exempt", nm))
      expect_identical(w[[nm]], "NE", info = paste(sh, "withheld", nm))
    }
    expect_false(is.na(suppressWarnings(as.numeric(x[["IRR"]]))), info = sh)
  }
})

test_that("(c) the forest data shows numbers for the exempt ETT and NE for the other", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
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
    x <- tx[ett_id == "ETT00002"]
    w <- tx[ett_id == "ETT00001"]
    for (nm in c("txt_irr", "txt_rd", "txt_nnt")) {
      expect_false(x[[nm]] %in% c("NE", NA, ""), info = paste(k, "exempt", nm))
      expect_identical(w[[nm]], "NE", info = paste(k, "withheld", nm))
    }
  }
})

test_that("(c) the Minimum events (Y or -) sheet shows the verdict of the rule and marks the exempt comparison", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
  sh <- "Minimum events (Y or -)"
  head <- .hx_head(fx$xlsx, sh)
  expect_identical(
    head[1L],
    "Minimum-events rule: per-protocol estimates pass (Y) or fail (-) the rule"
  )
  expect_match(
    head[2L],
    "-: fails the rule; the estimate is withheld unless the comparison is exempt.",
    fixed = TRUE
  )
  exempt_note <- paste0(
    "Exempt: the specification exempts this comparison; its estimates are ",
    "reported whatever the rule decides."
  )
  expect_true(endsWith(head[2L], paste0(" ", exempt_note)))
  s <- .hx_sheet(fx$xlsx, sh, "Comparison")
  expect_identical(s$Comparison, c("Enrollment one", "Enrollment two (exempt)"))
  # The 104-week ETTs fill the column of year 2.
  expect_identical(s[["2 years"]], c("-", "-"))
  # The (#) sheet carries the same comparison text and the same note
  # sentence. Its cells are the largest passing cut, 2, for both ETTs.
  cut <- .hx_sheet(fx$xlsx, "Minimum events (#)", "Comparison")
  expect_identical(cut$Comparison, s$Comparison)
  expect_identical(cut[["2 years"]], c("2", "2"))
  # The (#) sheet states the verdict of the rule too, never the reporting.
  cut_head <- .hx_head(fx$xlsx, "Minimum events (#)")
  expect_identical(
    cut_head[1L],
    "Minimum-events rule: the largest cut at which each per-protocol estimate passes the rule"
  )
  expect_identical(
    cut_head[2L],
    paste0(
      "Each cell is the largest cut, from 1, 2, 3, 4, 5, 10, 20 expected ",
      "events per arm per follow-up year, at which the estimate passes the ",
      "rule. -: fails the rule even at 1. The fill shows the decision at the ",
      "current cut, 5. ",
      exempt_note
    )
  )

  # Without an exempt enrollment, neither note carries the sentence, and no
  # comparison carries the marker.
  none <- .hx_fixture(NULL)
  for (s2 in c(sh, "Minimum events (#)")) {
    expect_false(grepl("Exempt:", .hx_head(none$xlsx, s2)[2L], fixed = TRUE))
    expect_identical(
      .hx_sheet(none$xlsx, s2, "Comparison")$Comparison,
      c("Enrollment one", "Enrollment two")
    )
  }
  expect_identical(
    .hx_head(none$xlsx, "Minimum events (#)"),
    c(
      cut_head[1L],
      sub(paste0(" ", exempt_note), "", cut_head[2L], fixed = TRUE)
    )
  )
})

test_that("(c2) get_estimates() reports the exempt ETT to its follow-up, and the Summary sheet shows its ratio", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
  e <- fx$plan$get_estimates()
  x <- e[ett_id == "ETT00002"]
  w <- e[ett_id == "ETT00001"]
  expect_identical(nrow(x), 3L)
  expect_equal(unique(x$follow_up), 104)
  expect_equal(x$horizon_reportable_weeks, rep(104, 3L))
  expect_identical(
    unique(x$horizon_status),
    "exempt from minimum-events rule"
  )
  expect_equal(unique(w$horizon_reportable_weeks), 52)
  expect_lt(unique(w$horizon_reportable_weeks), unique(w$follow_up))
  # The window table keeps the computed horizon of the rule.
  hw <- fx$plan$get_horizon_windows()
  expect_equal(unique(hw[ett_id == "ETT00002"]$horizon_reportable_weeks), 52)
  expect_identical(
    hw[ett_id == "ETT00002" & window_lo == 52 & estimand == "pp"]$pass,
    c(FALSE, FALSE)
  )

  s <- .hx_sheet(fx$xlsx, "Summary", "ETT")
  sx <- s[s$ETT == "ETT00002", ]
  sw <- s[s$ETT == "ETT00001", ]
  for (nm in c("ITT IRR (CI)", "PP IRR (CI)", "PP RD per 10,000 (CI)")) {
    expect_match(sx[[nm]], "^-?[0-9.]+ \\(", info = paste("exempt", nm))
    expect_identical(sw[[nm]], "NE", info = paste("withheld", nm))
  }
})

test_that("(c3) subgroups, the interaction test and the sensitivity sheet are reported for the exempt ETT only", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
  sg <- fx$plan$get_subgroups()
  for (est in c("pp", "itt")) {
    x <- sg[ett_id == "ETT00002" & estimand == est]
    w <- sg[ett_id == "ETT00001" & estimand == est]
    expect_identical(sort(x$subgroup_level), c("a", "all", "b"), info = est)
    expect_identical(sort(w$subgroup_level), c("a", "all", "b"), info = est)
    for (nm in c("irr", "irr_lo", "irr_hi", "em_pvalue", "ratio_of_irrs")) {
      expect_true(all(is.finite(x[[nm]])), info = paste(est, "exempt", nm))
      expect_true(all(is.na(w[[nm]])), info = paste(est, "withheld", nm))
    }
  }
  s <- .hx_sheet(fx$xlsx, "Weight truncation (PP)", "IRR")
  x <- s[s$Enrollment == "Enrollment two", ]
  w <- s[s$Enrollment == "Enrollment one", ]
  for (nm in c("IRR", "95% CI", "p-value", "IRR.1", "95% CI.1", "p-value.1")) {
    expect_false(x[[nm]] %in% c("NE", NA, ""), info = paste("exempt", nm))
    expect_identical(w[[nm]], "NE", info = paste("withheld", nm))
  }
})

test_that("(c3) the survival renderer gets the follow-up as the horizon of the exempt ETT", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
  seen <- list()
  real <- swereg:::.render_survival_curve
  testthat::local_mocked_bindings(
    .render_survival_curve = function(...) {
      args <- list(...)
      seen[[length(seen) + 1L]] <<- list(
        horizon_weeks = args$horizon_weeks,
        follow_up = args$follow_up
      )
      return(real(...))
    },
    .package = "swereg"
  )
  spec_for <- function(eid) {
    return(list(
      type = "survival",
      label = paste0("surv", eid),
      enrollment = eid,
      outcome = "osd_a",
      follow_up = 104L,
      age_group = "50_59",
      estimands = c("pp", "itt")
    ))
  }
  paths <- fx$plan$export(
    list(spec_for("01"), spec_for("02")),
    dir = file.path(fx$dir, "survival")
  )
  expect_length(paths, 4L)
  expect_length(seen, 4L)
  # The manifest order: enrollment 01 pp and itt, then enrollment 02.
  for (k in 1:2) {
    expect_equal(seen[[k]]$horizon_weeks, 52, info = k)
    expect_lt(seen[[k]]$horizon_weeks, seen[[k]]$follow_up)
  }
  for (k in 3:4) {
    expect_equal(seen[[k]]$horizon_weeks, 104, info = k)
    expect_equal(seen[[k]]$horizon_weeks, seen[[k]]$follow_up, info = k)
  }
})


# ---- (d): a flipped key after s3 ------------------------------------------------

test_that("(d) the writers stop when the spec flips the exemption after s3", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
  plan <- fx$plan
  old_spec <- plan$spec
  old_ett <- data.table::copy(plan$ett)
  on.exit(
    {
      plan$spec <- old_spec
      plan$ett <- old_ett
    },
    add = TRUE
  )
  expect_no_error(swereg:::.tte_assert_horizon_rule_current(plan, "x"))

  # The reload copies the flipped key into the plan, as a cosmetic change.
  new_spec <- old_spec
  new_spec$enrollments[[2]]$minimum_events_rule_exempt <- FALSE
  d <- swereg:::.diff_specs(old_spec, new_spec)
  expect_identical(d$structural, character(0))
  expect_identical(
    d$cosmetic,
    paste0(
      "enrollments[02]$minimum_events_rule_exempt: TRUE -> FALSE ",
      "(rerun $s3_analyze(); s1 and s2 output stays valid)"
    )
  )
  suppressWarnings(swereg:::.apply_cosmetic_spec_updates(plan, new_spec))
  expect_false(plan$spec$enrollments[[2]]$minimum_events_rule_exempt)

  xlsx <- file.path(fx$dir, "flipped.xlsx")
  expect_error(
    plan$export_tables(path = xlsx),
    "ETT00002 (enrollment 02: stored true, spec false)",
    fixed = TRUE
  )
  expect_error(plan$export_tables(path = xlsx), "\\$s3_analyze\\(\\)")
  expect_false(file.exists(xlsx))
  expect_error(
    plan$export(
      list(list(type = "table1", enrollment = "01")),
      dir = file.path(fx$dir, "flipped")
    ),
    "minimum_events_rule_exempt",
    fixed = TRUE
  )
  expect_error(
    utils::capture.output(plan$print_target_checklist()),
    "minimum_events_rule_exempt",
    fixed = TRUE
  )

  # A spec without the key reads as not exempt, so it stops too.
  plan$spec$enrollments[[2]]$minimum_events_rule_exempt <- NULL
  expect_error(
    swereg:::.tte_assert_horizon_rule_current(plan, "x"),
    "ETT00002 (enrollment 02: stored true, spec false)",
    fixed = TRUE
  )

  # The other direction: the spec exempts enrollment 01 after s3.
  plan$spec <- old_spec
  plan$spec$enrollments[[1]]$minimum_events_rule_exempt <- TRUE
  expect_error(
    swereg:::.tte_assert_horizon_rule_current(plan, "x"),
    "ETT00001 (enrollment 01: stored false, spec true)",
    fixed = TRUE
  )

  plan$spec <- old_spec
  expect_no_error(swereg:::.tte_assert_horizon_rule_current(plan, "x"))
})

test_that("(d) a stored record without rule$exempt counts as not exempt", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
  plan <- fx$plan
  old <- plan$results_ett
  on.exit(plan$results_ett <- old, add = TRUE)
  # A record of swereg 27.3.0 holds no rule$exempt.
  for (id in c("ETT00001", "ETT00002")) {
    for (est in c("pp", "itt")) {
      slot <- paste0("horizon_", est)
      plan$results_ett[[id]][[slot]]$rule <- list(min_expected_events = 5)
    }
  }
  # Enrollment 01 is not exempt, so its records stay current. Enrollment 02
  # is exempt, so its records are stale.
  expect_error(
    swereg:::.tte_assert_horizon_rule_current(plan, "x"),
    "1 ETT(s) were judged with another minimum_events_rule_exempt",
    fixed = TRUE
  )
  plan$results_ett$ETT00002 <- old$ETT00002
  expect_no_error(swereg:::.tte_assert_horizon_rule_current(plan, "x"))
})


# ---- (e): the generated prose ----------------------------------------------------

# The 27.3.0 strings, captured from the unpatched tree (commit dcfc8f37) for
# the fixture below at threshold 5. The transcript of the phase records the
# capture.
HX_TARGET_27_3_0 <- "A prespecified reporting rule, the minimum-events rule, required at least 5 expected events per arm in each year of follow-up. Follow-up was cut into years of 52 weeks from time zero, and the last year ended at the horizon. The expected events of an arm in a year were computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms. When any year failed the rule, the incidence rate ratio, risk difference and number needed to treat of that estimand were not reported. Its subgroup rate ratios and interaction tests were withheld too. Events, person-time and rates were still reported. Which years passed depended on the observed events, through the pooled rate. The rule was a reporting rule and did not guarantee precision or coverage."
HX_TARGET_27_3_0_NO_SG <- "A prespecified reporting rule, the minimum-events rule, required at least 5 expected events per arm in each year of follow-up. Follow-up was cut into years of 52 weeks from time zero, and the last year ended at the horizon. The expected events of an arm in a year were computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms. When any year failed the rule, the incidence rate ratio, risk difference and number needed to treat of that estimand were not reported. Events, person-time and rates were still reported. Which years passed depended on the observed events, through the pooled rate. The rule was a reporting rule and did not guarantee precision or coverage."
HX_PROTOCOL_27_3_0 <- "Minimum-events rule: at least 5 expected events per arm in each year of follow-up, computed under the null as the pooled weighted event rate of both arms times the weighted person-years of the arm. Follow-up years are 52 weeks from time zero. When any year up to the horizon fails, the rate ratio, risk difference and number needed to treat of that estimand read NE"
HX_OSF_27_3_0 <- "A prespecified reporting rule, the minimum-events rule, requires at least 5 expected events per arm in each year of follow-up. Follow-up is cut into years of 52 weeks from time zero. The last year ends at the horizon, so it can be shorter. The expected events of an arm in a year are computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms. The reportable horizon is the end of the last year before the first year that fails.\n\nWhen any year up to the horizon fails the rule, the estimates of that estimand are withheld. Its incidence rate ratio, risk difference and number needed to treat are reported as NE, not estimable. Its subgroup rate ratios and interaction tests are withheld too. Events, person-time, rates and the survival curve are still reported.\n\nThe rule is applied to the per-protocol estimate. The intention-to-treat estimate is reported exactly when the per-protocol estimate is, to the same horizon.\n\nWhich years pass depends on the observed events, through the pooled rate. The rule is a reporting rule and does not guarantee precision or coverage."
HX_OSF_27_3_0_NO_SG <- "A prespecified reporting rule, the minimum-events rule, requires at least 5 expected events per arm in each year of follow-up. Follow-up is cut into years of 52 weeks from time zero. The last year ends at the horizon, so it can be shorter. The expected events of an arm in a year are computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms. The reportable horizon is the end of the last year before the first year that fails.\n\nWhen any year up to the horizon fails the rule, the estimates of that estimand are withheld. Its incidence rate ratio, risk difference and number needed to treat are reported as NE, not estimable. Events, person-time, rates and the survival curve are still reported.\n\nThe rule is applied to the per-protocol estimate. The intention-to-treat estimate is reported exactly when the per-protocol estimate is, to the same horizon.\n\nWhich years pass depends on the observed events, through the pooled rate. The rule is a reporting rule and does not guarantee precision or coverage."

HX_SUBGROUPS <- list(list(
  name = "Age group",
  implementation = list(variable = "rd_agegroup")
))

# The sentence that names the exempt enrollments, for one and for two.
HX_ONE <- "The specification exempts Treated vs control 02 from the rule, so the rule withholds none of its estimates."
HX_TWO <- "The specification exempts Treated vs control 01 and Treated vs control 02 from the rule, so the rule withholds none of their estimates."

# The fast plan of helper-sparse_report.R, with enrollments "01" and "02" at
# threshold 5. `exempt` holds the value of the key per enrollment, NULL for
# no key.
.hx_prose_plan <- function(
  exempt = list(NULL, NULL),
  subgroups = TRUE,
  env = parent.frame(),
  k = 5
) {
  plan <- spr_report_fast_plan(env = env)
  plan$spec$min_expected_events_per_arm_per_year <- k
  if (subgroups) {
    plan$spec$subgroups <- HX_SUBGROUPS
  }
  for (i in seq_along(exempt)) {
    plan$spec$enrollments[[i]]$minimum_events_rule_exempt <- exempt[[i]]
  }
  return(plan)
}

# Item 6h of the TARGET checklist, as one string.
.hx_target_6h <- function(plan) {
  txt <- spr_report_checklist(plan)
  return(paste(spr_report_item(txt, "6h", "7a-h"), collapse = " "))
}

# The minimum-events line of the Analysis plan cell of the protocol table.
.hx_protocol_line <- function(plan) {
  ctx <- swereg:::.protocol_context(plan, plan$ett$ett_id[1])
  pt <- swereg:::.build_protocol_table(plan$spec, ctx)
  expect_identical(pt$`Protocol component`[7], "Analysis plan")
  cell <- strsplit(pt$`Target trial emulation`[7], "\n", fixed = TRUE)[[1]]
  return(cell[startsWith(cell, "Minimum-events rule: ")])
}

# The OSF Inference criteria field of the fixture spec.
.hx_osf <- function(
  exempt = list(NULL, NULL),
  subgroups = TRUE,
  env = parent.frame(),
  k = 5
) {
  spec <- spr_report_spec()
  spec$min_expected_events_per_arm_per_year <- k
  if (subgroups) {
    spec$subgroups <- HX_SUBGROUPS
  }
  for (i in seq_along(exempt)) {
    spec$enrollments[[i]]$minimum_events_rule_exempt <- exempt[[i]]
  }
  dir <- withr::local_tempdir(.local_envir = env)
  path <- spr_write_spec(spec, dir)
  return(suppressWarnings(swereg::tteplan_osf_text(path))[[
    "Inference criteria"
  ]])
}

test_that("(e) TARGET 6h names the exempt enrollment and qualifies its withholding sentence", {
  plan <- .hx_prose_plan(list(NULL, TRUE))
  want <- paste0(
    "A prespecified reporting rule, the minimum-events rule, required at least 5 expected events per arm in each year of follow-up. Follow-up was cut into years of 52 weeks from time zero, and the last year ended at the horizon. The expected events of an arm in a year were computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms. ",
    "When any year failed the rule, except in an exempt comparison, the incidence rate ratio, risk difference and number needed to treat of that estimand were not reported. Its subgroup rate ratios and interaction tests were withheld too. ",
    HX_ONE,
    " Events, person-time and rates were still reported. Which years passed depended on the observed events, through the pooled rate. The rule was a reporting rule and did not guarantee precision or coverage."
  )
  expect_identical(swereg:::.plan_horizon_rule_text(plan$spec), want)
  x <- .hx_target_6h(plan)
  expect_match(x, want, fixed = TRUE)
  # The naming sentence follows the withholding sentence.
  expect_gt(
    regexpr(HX_ONE, x, fixed = TRUE),
    regexpr("except in an exempt comparison,", x, fixed = TRUE)
  )

  two <- swereg:::.plan_horizon_rule_text(
    .hx_prose_plan(list(TRUE, TRUE))$spec
  )
  expect_match(two, HX_TWO, fixed = TRUE)
  expect_false(grepl(HX_ONE, two, fixed = TRUE))
})

test_that("(e) the OSF text names the exempt enrollment and qualifies its withholding sentence", {
  want <- sub(
    "When any year up to the horizon fails the rule, the estimates",
    "When any year up to the horizon fails the rule, except in an exempt comparison, the estimates",
    HX_OSF_27_3_0,
    fixed = TRUE
  )
  want <- sub(
    "withheld too. Events, person-time, rates",
    paste0("withheld too. ", HX_ONE, " Events, person-time, rates"),
    want,
    fixed = TRUE
  )
  expect_false(identical(want, HX_OSF_27_3_0))
  expect_identical(.hx_osf(list(NULL, TRUE)), want)
  expect_match(.hx_osf(list(TRUE, TRUE)), HX_TWO, fixed = TRUE)
})

test_that("(e) the protocol table names the exempt enrollment and qualifies its withholding sentence", {
  expect_identical(
    .hx_protocol_line(.hx_prose_plan(list(NULL, TRUE))),
    paste0(
      "Minimum-events rule: at least 5 expected events per arm in each year of follow-up, computed under the null as the pooled weighted event rate of both arms times the weighted person-years of the arm. Follow-up years are 52 weeks from time zero. ",
      "When any year up to the horizon fails, except in an exempt comparison, the rate ratio, risk difference and number needed to treat of that estimand read NE. ",
      HX_ONE
    )
  )
  expect_match(
    .hx_protocol_line(.hx_prose_plan(list(TRUE, TRUE))),
    HX_TWO,
    fixed = TRUE
  )
})

test_that("(e) with no exempt enrollment each text equals its 27.3.0 string", {
  for (exempt in list(list(NULL, NULL), list(FALSE, FALSE))) {
    lab <- if (is.null(exempt[[1L]])) "absent" else "false"
    plan <- .hx_prose_plan(exempt)
    expect_identical(
      swereg:::.plan_horizon_rule_text(plan$spec),
      HX_TARGET_27_3_0,
      info = lab
    )
    expect_identical(
      lengths(regmatches(
        .hx_target_6h(plan),
        gregexpr(HX_TARGET_27_3_0, .hx_target_6h(plan), fixed = TRUE)
      )),
      1L,
      info = lab
    )
    expect_identical(
      swereg:::.plan_horizon_rule_text(
        .hx_prose_plan(exempt, subgroups = FALSE)$spec
      ),
      HX_TARGET_27_3_0_NO_SG,
      info = lab
    )
    expect_identical(.hx_protocol_line(plan), HX_PROTOCOL_27_3_0, info = lab)
    expect_identical(.hx_osf(exempt), HX_OSF_27_3_0, info = lab)
    expect_identical(
      .hx_osf(exempt, subgroups = FALSE),
      HX_OSF_27_3_0_NO_SG,
      info = lab
    )
  }
})


# The 27.3.0 strings at threshold 0, captured from the unpatched tree
# (commit dcfc8f37) for the same fixture. The subgroup block does not change
# them.
HX_TARGET_0_27_3_0 <- "A prespecified reporting rule, the minimum-events rule, required at least 0 expected events per arm in each year of follow-up, so it withheld no estimate. The expected events of an arm in a year were computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms."
HX_PROTOCOL_0_27_3_0 <- "Minimum-events rule: at least 0 expected events per arm in each year of follow-up, computed under the null as the pooled weighted event rate of both arms times the weighted person-years of the arm, so no estimate is withheld"
HX_OSF_0_27_3_0 <- "A prespecified reporting rule, the minimum-events rule, requires at least 0 expected events per arm in each year of follow-up, so it withholds no estimate. The expected events of an arm in a year are computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms."

test_that("(e0) at threshold 0 each text names the exempt enrollment, and without one equals its 27.3.0 string", {
  plan <- .hx_prose_plan(list(NULL, TRUE), k = 0)
  expect_identical(
    swereg:::.plan_horizon_rule_text(plan$spec),
    paste0(HX_TARGET_0_27_3_0, " ", HX_ONE)
  )
  expect_identical(
    .hx_protocol_line(plan),
    paste0(HX_PROTOCOL_0_27_3_0, ". ", HX_ONE)
  )
  expect_identical(
    .hx_osf(list(NULL, TRUE), k = 0),
    paste0(HX_OSF_0_27_3_0, " ", HX_ONE)
  )
  expect_match(
    swereg:::.plan_horizon_rule_text(
      .hx_prose_plan(list(TRUE, TRUE), k = 0)$spec
    ),
    HX_TWO,
    fixed = TRUE
  )
  for (exempt in list(list(NULL, NULL), list(FALSE, FALSE))) {
    lab <- if (is.null(exempt[[1L]])) "absent" else "false"
    none <- .hx_prose_plan(exempt, k = 0)
    expect_identical(
      swereg:::.plan_horizon_rule_text(none$spec),
      HX_TARGET_0_27_3_0,
      info = lab
    )
    expect_identical(.hx_protocol_line(none), HX_PROTOCOL_0_27_3_0, info = lab)
    expect_identical(.hx_osf(exempt, k = 0), HX_OSF_0_27_3_0, info = lab)
  }
})

# ---- (f): the console reports -----------------------------------------------------

test_that("(f) the s3 console report names the exempt ETT as exempt", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
  expect_true(any(
    fx$out ==
      "s3: the minimum-events rule withheld 2 of 4 ETT estimands (reportable weeks): ETT00001 (pp, 52 w), ETT00001 (itt, 52 w)"
  ))
  expect_true(any(
    fx$out ==
      "s3: the specification exempts 2 of 4 ETT estimands from the minimum-events rule, so the rule withheld none of their estimates: ETT00002 (pp), ETT00002 (itt)"
  ))
  # Without the key, the report is the one line of 27.3.0.
  none <- .hx_fixture(NULL)
  expect_true(any(
    none$out ==
      "s3: the minimum-events rule withheld 4 of 4 ETT estimands (reportable weeks): ETT00001 (pp, 52 w), ETT00001 (itt, 52 w), ETT00002 (pp, 52 w), ETT00002 (itt, 52 w)"
  ))
  expect_false(any(grepl("exempt", none$out, fixed = TRUE)))
})

test_that("(f) the feasibility report and record name the exempt ETT as exempt", {
  .hx_skip()
  fx <- .hx_fixture(TRUE)
  records <- lapply(fx$plan$results_ett, function(r) {
    return(lapply(
      list(pp = r$horizon_pp, itt = r$horizon_itt),
      swereg:::.tte_feasibility_strip
    ))
  })
  expect_true(records$ETT00002$pp$exempt)
  expect_false(records$ETT00001$pp$exempt)
  out <- utils::capture.output(swereg:::.feasibility_report(records))
  expect_identical(
    out,
    c(
      "feasibility: the minimum-events rule would withhold 2 of 4 ETT estimands (reportable weeks): ETT00001 (pp, 52 w), ETT00001 (itt, 52 w)",
      "feasibility: the specification exempts 2 of 4 ETT estimands from the minimum-events rule, so the rule would withhold none of their estimates: ETT00002 (pp), ETT00002 (itt)"
    )
  )
})
