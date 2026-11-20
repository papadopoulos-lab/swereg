# The minimum-events rule per subgroup level, at s3.
#
# s3 judges each level of each subgroup variable by the rule on its own, with
# its own pooled rate. A level that fails loses its rate ratio, and the
# interaction test of its variable goes with it. A level that passes is
# reported. The "all" row follows the rule of the whole ETT.
#
# One fixture runs the real `$s3_analyze()` through the batch runner. It holds
# three enrollments.
#   - 01: ETT00001 (52 weeks), ETT00002 (40 weeks, part-year, source
#     ETT00001), ETT00003 (90 weeks, part-year, source ETT00004), ETT00004
#     (104 weeks, feasibility horizon).
#   - 02: ETT00005 (52 weeks). The enrollment is exempt from the rule.
#   - 03: ETT00006 (52 weeks). Both levels pass.
# The subgroup variable `rd_sg` has a large level "big" and a level "thin"
# with 8 person-trials per arm. `rd_one` has one non-NA level.

skip_if_not_installed("data.table")

# One row per 4-week interval. `rd_intervention` TRUE is the intervention
# arm. In each arm, the first `thin_events` persons of level "thin" have an
# event, at weeks 8, 12, 16, and so on. Every other thin person is followed
# to `end_wk` with no event. The "big" level and the NA level have random
# events.
.hl_panel <- function(thin_events, end_wk = 104L, seed = 11L) {
  set.seed(seed)
  n_int <- end_wk %/% 4L
  one <- function(id, arm, sg, ev) {
    keep <- if (any(ev == 1L)) seq_len(which(ev == 1L)[1L]) else seq_len(n_int)
    return(data.table::data.table(
      id = id,
      enrollment_person_trial_id = paste0(id, "_t1"),
      rd_intervention = arm,
      tstart = (keep - 1L) * 4L,
      tstop = keep * 4L,
      event = ev[keep],
      person_weeks = 4,
      rd_age_continuous = 50 + as.integer(sub("^p", "", id)) %% 10L,
      rd_sg = sg,
      rd_one = if (is.na(sg)) NA_character_ else "x"
    ))
  }
  rows <- list()
  k <- 0L
  for (arm in c(TRUE, FALSE)) {
    for (j in seq_len(100L)) {
      k <- k + 1L
      ev <- stats::rbinom(n_int, 1L, 0.02)
      rows[[k]] <- one(sprintf("p%04d", k), arm, "big", ev)
    }
    for (j in seq_len(8L)) {
      k <- k + 1L
      ev <- integer(n_int)
      if (j <= thin_events) {
        ev[j + 1L] <- 1L
      }
      rows[[k]] <- one(sprintf("p%04d", k), arm, "thin", ev)
    }
    for (j in seq_len(4L)) {
      k <- k + 1L
      ev <- stats::rbinom(n_int, 1L, 0.02)
      rows[[k]] <- one(sprintf("p%04d", k), arm, NA_character_, ev)
    }
  }
  d <- data.table::rbindlist(rows)
  for (w in c("analysis_weight_pp_trunc", "analysis_weight_pp", "ipw")) {
    data.table::set(d, j = w, value = 1)
  }
  data.table::set(d, j = "ipw_trunc", value = 1)
  return(d)
}

# Write the analysis files and return the plan, with the stage stamps set and
# nothing computed.
.hl_build <- function(dir) {
  design <- swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "rd_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "rd_age_continuous",
    follow_up_time = 104L
  )
  # Two events per arm in level "thin" fail the rule at 5. Seven pass it.
  few <- .hl_panel(thin_events = 2L)
  many <- .hl_panel(thin_events = 7L)
  save <- function(d, file) {
    enr <- swereg::TTEEnrollment$new(d, design, data_level = "trial")
    qs2::qs_save(enr, file.path(dir, file))
  }
  files <- list(
    # The per-protocol level "thin" of ETT00001 fails. Its own
    # intention-to-treat level passes.
    e1_pp = few[tstop <= 52],
    e1_itt = many[tstop <= 52],
    # The part-year ETT00002: its own level "thin" passes in both estimands.
    e2 = many[tstop <= 40],
    e3 = few[tstop <= 90],
    e4 = few,
    e5 = few[tstop <= 52],
    e6 = many[tstop <= 52]
  )
  for (nm in names(files)) {
    save(files[[nm]], paste0(nm, ".qs2"))
  }
  for (enr in c("01", "02", "03")) {
    save(if (enr == "03") many else few, paste0("raw_", enr, ".qs2"))
  }
  ett <- data.table::data.table(
    enrollment_id = c("01", "01", "01", "01", "02", "03"),
    ett_id = sprintf("ETT%05d", 1:6),
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    outcome_role = "primary",
    follow_up = c(52L, 40L, 90L, 104L, 52L, 52L),
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "id",
    treatment_var = "rd_intervention",
    file_imp = "imp.qs2",
    file_raw = paste0("raw_", c("01", "01", "01", "01", "02", "03"), ".qs2"),
    file_analysis = c(
      "e1_pp.qs2",
      "e2.qs2",
      "e3.qs2",
      "e4.qs2",
      "e5.qs2",
      "e6.qs2"
    ),
    file_analysis_itt = c(
      "e1_itt.qs2",
      "e2.qs2",
      "e3.qs2",
      "e4.qs2",
      "e5.qs2",
      "e6.qs2"
    ),
    description = sprintf("ETT%05d", 1:6),
    feasibility_horizon = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  data.table::set(
    ett,
    j = "subgroup_vars",
    value = list(rep(list(c("rd_sg", "rd_one")), 6L))
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "hl",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$output_dir <- dir
  arms <- list(arms = list(intervention = "Treated", comparator = "Untreated"))
  plan$spec <- list(
    study = list(
      title = "Fixture",
      implementation = list(project_prefix = "hl")
    ),
    enrollments = list(
      list(id = "01", name = "Enrollment one", treatment = arms),
      list(
        id = "02",
        name = "Enrollment two",
        treatment = arms,
        minimum_events_rule_exempt = TRUE
      ),
      list(id = "03", name = "Enrollment three", treatment = arms)
    ),
    min_expected_events_per_arm_per_year = 5,
    period_width = 4L,
    admin_censoring_isoyearweek = "2020-52"
  )
  tte_stamp_stages(plan)
  return(plan)
}

# s3 through the REAL batch runner (worker subprocesses), then the workbook.
# Built once per file.
.hl_cache <- new.env()
.hl_fixture <- function() {
  if (!is.null(.hl_cache$fx)) {
    return(.hl_cache$fx)
  }
  dir <- tempfile("hl_s3_")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = testthat::teardown_env())
  plan <- .hl_build(dir)
  out <- utils::capture.output(suppressWarnings(plan$s3_analyze(
    output_dir = dir,
    n_workers = 3L,
    swereg_dev_path = swereg:::.swereg_dev_path()
  )))
  # The export error is kept, not thrown. A test that reads s3 results then
  # fails at its own assertion, also when the export stops.
  xlsx <- file.path(dir, "tables.xlsx")
  export_error <- tryCatch(
    {
      suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  .hl_cache$fx <- list(
    dir = dir,
    plan = plan,
    xlsx = xlsx,
    out = out,
    export_error = export_error
  )
  return(.hl_cache$fx)
}

.hl_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("survey")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
}

# The rows of `$get_subgroups()` for one ETT, estimand and variable.
.hl_rows <- function(plan, ett, est, var = "rd_sg") {
  sg <- plan$get_subgroups()
  return(sg[ett_id == ett & estimand == est & subgroup_var == var])
}

.hl_level <- function(rows, level) {
  return(rows[subgroup_level == level])
}

HL_PREFIX <- "below minimum-events rule"


test_that("(1) a thin level is withheld, the other level is reported, and the interaction test is withheld", {
  .hl_skip()
  fx <- .hl_fixture()
  res <- fx$plan$results_ett$ETT00001
  # The whole ETT passes, so only the per-level rule can withhold.
  expect_false(res$horizon_pp$withheld)
  expect_identical(res$horizon_pp$status, swereg:::.HORIZON_STATUS_PASS)

  r <- .hl_rows(fx$plan, "ETT00001", "pp")
  expect_identical(sort(r$subgroup_level), c("all", "big", "thin"))
  thin <- .hl_level(r, "thin")
  expect_true(is.na(thin$irr))
  expect_true(is.na(thin$irr_lo))
  expect_true(is.na(thin$irr_hi))
  expect_true(is.na(thin$irr_pvalue))
  expect_true(all(is.na(r$em_pvalue)))
  expect_true(all(is.na(r$ratio_of_irrs)))
  expect_true(startsWith(thin$horizon_status, HL_PREFIX))

  big <- .hl_level(r, "big")
  expect_true(is.finite(big$irr))
  expect_true(is.finite(big$irr_pvalue))
  expect_identical(big$horizon_status, swereg:::.HORIZON_STATUS_PASS)

  # The "all" row follows the rule of the whole ETT, and carries the status
  # of the interaction test.
  all <- .hl_level(r, "all")
  expect_true(is.finite(all$irr))
  expect_identical(
    all$horizon_status,
    "below minimum-events rule: subgroup level thin"
  )

  # The stored slots behind the accessor.
  expect_true(res$hsub_rd_sg_pp$thin$withheld)
  expect_false(res$hsub_rd_sg_pp$big$withheld)
  expect_true(is.na(res$emtest_rd_sg_pp$p_value))
  expect_true(all(is.na(res$emtest_rd_sg_pp$interaction_coefs$estimate)))
  expect_true(is.na(attr(res$subgroup_rd_sg_pp, "em_pvalue")))
})

test_that("(2) when both levels pass, both rate ratios and the interaction test are reported", {
  .hl_skip()
  fx <- .hl_fixture()
  for (est in c("pp", "itt")) {
    r <- .hl_rows(fx$plan, "ETT00006", est)
    expect_identical(sort(r$subgroup_level), c("all", "big", "thin"))
    for (lv in c("big", "thin")) {
      x <- .hl_level(r, lv)
      expect_true(is.finite(x$irr), info = paste(est, lv))
      expect_true(is.finite(x$irr_pvalue), info = paste(est, lv))
      expect_identical(
        x$horizon_status,
        swereg:::.HORIZON_STATUS_PASS,
        info = paste(est, lv)
      )
    }
    expect_true(all(is.finite(r$em_pvalue)), info = est)
    expect_identical(
      .hl_level(r, "all")$horizon_status,
      swereg:::.HORIZON_STATUS_PASS,
      info = est
    )
  }
})

test_that("(3) an intention-to-treat level is withheld when its per-protocol level fails", {
  .hl_skip()
  fx <- .hl_fixture()
  res <- fx$plan$results_ett$ETT00001
  itt <- res$hsub_rd_sg_itt$thin
  # The intention-to-treat level passes on its own windows ...
  expect_true(all(itt$windows$pass))
  # ... and takes the failing decision of its per-protocol level.
  expect_identical(itt$decided_by, "pp")
  expect_true(itt$withheld)
  expect_identical(itt$status, res$hsub_rd_sg_pp$thin$status)

  r <- .hl_rows(fx$plan, "ETT00001", "itt")
  expect_true(is.na(.hl_level(r, "thin")$irr))
  expect_true(is.finite(.hl_level(r, "big")$irr))
  expect_true(all(is.na(r$em_pvalue)))
})

test_that("(4) a part-year ETT withholds the level that the record of its source ETT fails", {
  .hl_skip()
  fx <- .hl_fixture()
  res <- fx$plan$results_ett$ETT00002
  own <- res$hsub_rd_sg_pp$thin
  # The level passes on the part-year ETT's own windows ...
  expect_true(all(own$windows$pass))
  # ... and takes the failing decision of ETT00001.
  expect_identical(own$decided_by_ett, "ETT00001")
  expect_true(own$withheld)
  for (est in c("pp", "itt")) {
    r <- .hl_rows(fx$plan, "ETT00002", est)
    expect_true(is.na(.hl_level(r, "thin")$irr), info = est)
    expect_true(is.finite(.hl_level(r, "big")$irr), info = est)
    expect_true(all(is.na(r$em_pvalue)), info = est)
  }

  # A source that is a feasibility-horizon ETT holds per-level records too.
  src <- fx$plan$results_ett$ETT00004
  expect_identical(names(src$hsub_rd_sg_pp), c("big", "thin"))
  expect_true(src$hsub_rd_sg_pp$thin$withheld)
  part <- fx$plan$results_ett$ETT00003$hsub_rd_sg_pp$thin
  expect_identical(part$decided_by_ett, "ETT00004")
  expect_true(part$withheld)
  expect_true(is.na(.hl_level(.hl_rows(fx$plan, "ETT00003", "pp"), "thin")$irr))
})

test_that("(5) an exempt enrollment withholds nothing at any level", {
  .hl_skip()
  fx <- .hl_fixture()
  res <- fx$plan$results_ett$ETT00005
  # The rule fails the level, and the exemption keeps it reported.
  expect_true(res$hsub_rd_sg_pp$thin$withheld)
  expect_true(res$hsub_rd_sg_pp$thin$exempt)
  for (est in c("pp", "itt")) {
    r <- .hl_rows(fx$plan, "ETT00005", est)
    for (lv in c("big", "thin")) {
      expect_true(is.finite(.hl_level(r, lv)$irr), info = paste(est, lv))
    }
    expect_true(all(is.finite(r$em_pvalue)), info = est)
    expect_identical(
      unique(r$horizon_status),
      swereg:::.HORIZON_STATUS_EXEMPT,
      info = est
    )
  }
})

test_that("(6) a subgroup variable with one level gives one record and no error", {
  .hl_skip()
  fx <- .hl_fixture()
  for (eid in sprintf("ETT%05d", c(1:3, 5:6))) {
    res <- fx$plan$results_ett[[eid]]
    for (est in c("pp", "itt")) {
      recs <- res[[paste0("hsub_rd_one_", est)]]
      expect_identical(names(recs), "x", info = paste(eid, est))
      # The estimator stops below 2 levels, and the worker stores a skip.
      expect_true(
        isTRUE(res[[paste0("subgroup_rd_one_", est)]]$skipped),
        info = paste(eid, est)
      )
    }
  }
  # The NA rows are dropped: "big" and "thin" are the only levels of rd_sg.
  expect_identical(
    names(fx$plan$results_ett$ETT00001$hsub_rd_sg_pp),
    c("big", "thin")
  )
  sg <- fx$plan$get_subgroups()
  expect_identical(nrow(sg[subgroup_var == "rd_one"]), 0L)
})

test_that("(7) results without per-level records make get_subgroups() and export_tables() stop, naming s3", {
  .hl_skip()
  fx <- .hl_fixture()
  plan <- fx$plan
  # The passing direction: the fixture's own export wrote the workbook.
  expect_null(fx$export_error)
  expect_true(file.exists(fx$xlsx))
  expect_no_error(plan$get_subgroups())

  kept <- plan$results_ett
  withr::defer(plan$results_ett <- kept)
  msg <- "no decision of the minimum-events rule per subgroup level.*\\$s3_analyze\\(\\)"
  # The error message, or "no error". Another error is then a failure of the
  # match below, and the next case still runs.
  error_of <- function(expr) {
    return(tryCatch(
      {
        force(expr)
        "no error"
      },
      error = conditionMessage
    ))
  }
  # ETT00001 is the source of the part-year ETT00002. No part-year ETT reads
  # ETT00006, so only the refusal can stop its export.
  for (eid in c("ETT00001", "ETT00006")) {
    old <- kept
    for (s in grep("^hsub_", names(old[[eid]]), value = TRUE)) {
      old[[eid]][[s]] <- NULL
    }
    plan$results_ett <- old
    path <- file.path(fx$dir, paste0("old_", eid, ".xlsx"))
    expect_match(error_of(plan$get_subgroups()), msg, info = eid)
    expect_match(
      error_of(suppressMessages(suppressWarnings(
        plan$export_tables(path = path)
      ))),
      msg,
      info = eid
    )
    expect_false(file.exists(path), info = eid)
  }
})

test_that("(8) the \"all\" row carries the status of the whole ETT when the ETT is withheld", {
  .hl_skip()
  fx <- .hl_fixture()
  plan <- fx$plan
  kept <- plan$results_ett
  withr::defer(plan$results_ett <- kept)

  # Every branch of the rule for the "all" row, on the stored records.
  recs <- kept$ETT00001$hsub_rd_sg_pp
  ett_pass <- kept$ETT00001$horizon_pp
  ett_fail <- utils::modifyList(
    ett_pass,
    list(withheld = TRUE, status = "below minimum-events rule: ETT")
  )
  ett_exempt <- kept$ETT00005$horizon_pp
  status_of <- swereg:::.tte_horizon_subgroup_status
  expect_identical(status_of(ett_fail, recs), "below minimum-events rule: ETT")
  expect_identical(
    status_of(ett_pass, recs),
    "below minimum-events rule: subgroup level thin"
  )
  expect_identical(
    status_of(ett_pass, kept$ETT00006$hsub_rd_sg_pp),
    swereg:::.HORIZON_STATUS_PASS
  )
  expect_identical(
    status_of(ett_exempt, kept$ETT00005$hsub_rd_sg_pp),
    swereg:::.HORIZON_STATUS_EXEMPT
  )

  # The same through the accessor: a withheld ETT gives its own status on the
  # "all" row, and each level row keeps the status of its own record.
  res <- kept
  res$ETT00006$horizon_pp <- utils::modifyList(
    res$ETT00006$horizon_pp,
    list(withheld = TRUE, status = "below minimum-events rule: ETT")
  )
  plan$results_ett <- res
  r <- .hl_rows(plan, "ETT00006", "pp")
  expect_identical(
    .hl_level(r, "all")$horizon_status,
    "below minimum-events rule: ETT"
  )
  expect_identical(
    .hl_level(r, "thin")$horizon_status,
    swereg:::.HORIZON_STATUS_PASS
  )
})
