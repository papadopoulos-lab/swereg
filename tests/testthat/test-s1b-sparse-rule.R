# The rare_condition_rule and the s1b step (#52), through the real s1 dispatch.
#
# The rule decides once per study, from the s1a tuples of the driving
# enrollments, before the comparator draw. The plan freezes the decision, and
# every enrollment applies it: the person-trials that hold an excluded
# condition at their recruiting week leave in the order of the spec, and the
# condition leaves every confounder set. Without the block, s1 writes what
# 27.2.0 wrote.
#
# The fixture is in helper-sparse_rule.R. Two real s1 runs drive every test
# below, and each run is made once per file. test-s1b-sparse-rule-spec.R holds
# the tests that need no s1 run.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")

spr_runs <- new.env(parent = emptyenv())

# `rule`: the three candidates, two of them rare. `none`: no rare_condition_rule.
spr_get <- function(name) {
  if (is.null(spr_runs[[name]])) {
    rule <- if (identical(name, "rule")) spr_rule() else NULL
    spr_runs[[name]] <- spr_run_s1(rule)
  }
  return(spr_runs[[name]])
}

# The global rows of one enrollment's attrition, in pipeline order.
spr_global <- function(plan, eid) {
  att <- plan$enrollment_counts[[eid]]$attrition
  return(att[is.na(att$enrollment_period_id)])
}

# The step-2 rows: every global row after the last s1a cascade row.
spr_step2 <- function(plan, eid) {
  g <- spr_global(plan, eid)
  last_s1a <- which(g$criterion == "landmark_event_free")
  return(g[seq_len(nrow(g)) > last_s1a])
}

SPR_SOURCES <- c("osd_rare_b", "osd_common", "osd_rare_a")
SPR_KEPT <- c("rd_age_continuous", "rd_no_osd_common_everbefore")

# ---------------------------------------------------------------------------
# s1a: the baseline columns on the tuples
# ---------------------------------------------------------------------------

test_that("s1a writes each candidate's baseline value on every tuple", {
  r <- spr_get("rule")
  # The fixture MUST hold a tuple whose recruiting week directly follows the
  # mid-2018 event, or a read of the wrong week could not show.
  mid_idx0 <- match(spr_mid_week(), cstime::dates_by_isoyearweek$isoyearweek) -
    1L
  tp01 <- r$pre[["01"]]$tuples
  expect_true(any(
    tp01$recruit_week_index == mid_idx0 + 1L &
      spr_holds(r$sk, tp01, "osd_common")
  ))
  for (eid in c("01", "02")) {
    tp <- r$pre[[eid]]$tuples
    for (src in SPR_SOURCES) {
      expect_identical(
        tp[[spr_var(src)]],
        !spr_holds(r$sk, tp, src),
        info = paste(eid, src)
      )
    }
  }
})

# ---------------------------------------------------------------------------
# The decision
# ---------------------------------------------------------------------------

test_that("s1b excludes the rare conditions and adjusts for the common one", {
  r <- spr_get("rule")
  d <- r$plan$get_sparse_decision()
  tp <- r$pre[["01"]]$tuples

  expect_identical(d$variable, spr_var(SPR_SOURCES))
  expect_identical(
    d$label,
    c("Prior rare condition B", "Prior common condition", "Prior rare condition A")
  )
  # The hand count: unique persons per arm with a holding tuple in 01.
  hand <- function(src, arm) {
    held <- spr_holds(r$sk, tp, src) & tp$intervention == arm
    return(length(unique(tp$id[held])))
  }
  expect_identical(
    d$n_01_intervention,
    vapply(SPR_SOURCES, hand, integer(1), arm = TRUE, USE.NAMES = FALSE)
  )
  expect_identical(
    d$n_01_comparator,
    vapply(SPR_SOURCES, hand, integer(1), arm = FALSE, USE.NAMES = FALSE)
  )
  expect_identical(d$smallest, pmin(d$n_01_intervention, d$n_01_comparator))
  expect_identical(d$decision, c("excluded", "adjusted", "excluded"))
  expect_true(all(d$threshold == 10L))
  expect_true(all(d$decided_by == "01"))
  expect_s3_class(d$decided_at, "POSIXct")
  expect_identical(
    r$plan$sparse_rule_hash,
    swereg:::.tte_sparse_rule_hash(r$plan$spec$rare_condition_rule)
  )
})

test_that("excluded conditions leave every enrollment's confounder set", {
  r <- spr_get("rule")
  for (cv in r$plan$ett$confounder_vars) {
    expect_identical(cv, SPR_KEPT)
  }
  for (i in seq_len(2L)) {
    expect_identical(r$plan$enrollment_spec(i)$design$confounder_vars, SPR_KEPT)
  }
})

test_that("the non-driving enrollment inherits the exclusions and the confounder set", {
  r <- spr_get("rule")
  d <- r$plan$get_sparse_decision()
  # Enrollment 02 did not drive: the decision holds no count of it.
  expect_false(any(grepl("^n_02_", names(d))))

  # 02 alone would have excluded the common condition: fewer than 10 persons
  # per arm of 02 hold it.
  tp02 <- r$pre[["02"]]$tuples
  common <- spr_holds(r$sk, tp02, "osd_common")
  for (arm in c(TRUE, FALSE)) {
    expect_lt(length(unique(tp02$id[common & tp02$intervention == arm])), 10L)
  }

  s01 <- spr_step2(r$plan, "01")$criterion
  s02 <- spr_step2(r$plan, "02")$criterion
  expect_length(s02, 2L)
  expect_identical(s02, s01)
  expect_true(all(endsWith(
    s02,
    c("Prior rare condition B", "Prior rare condition A")
  )))
  cv <- r$plan$ett$confounder_vars[r$plan$ett$enrollment_id == "02"]
  expect_identical(cv[[1]], SPR_KEPT)
})

test_that("the CONSORT step-2 counts match a hand count, in spec order", {
  r <- spr_get("rule")
  for (eid in c("01", "02")) {
    tp <- r$pre[[eid]]$tuples
    g <- spr_global(r$plan, eid)
    s2 <- spr_step2(r$plan, eid)
    # rare B first, then rare A, as `candidates` orders them.
    expect_true(all(endsWith(
      s2$criterion,
      c("Prior rare condition B", "Prior rare condition A")
    )))
    after_b <- tp[!spr_holds(r$sk, tp, "osd_rare_b")]
    after_a <- after_b[!spr_holds(r$sk, after_b, "osd_rare_a")]
    expect_identical(
      s2$n_persons,
      c(length(unique(after_b$id)), length(unique(after_a$id))),
      info = eid
    )
    expect_identical(
      s2$n_person_trials,
      c(nrow(after_b), nrow(after_a)),
      info = eid
    )
    expect_identical(
      s2$n_comparator,
      c(sum(!after_b$intervention), sum(!after_a$intervention)),
      info = eid
    )
    # Step 2 starts from the last step-1 row, which counts every tuple.
    expect_identical(
      g$n_person_trials[g$criterion == "landmark_event_free"],
      nrow(tp),
      info = eid
    )

    # The cohort flow renders both as exclusions, with their own deltas.
    flow <- swereg:::.build_cohort_flow(r$plan$enrollment_counts[[eid]])
    rows <- flow[flow$step %in% s2$criterion]
    expect_identical(rows$kind, c("exclusion", "exclusion"), info = eid)
    expect_identical(
      as.numeric(rows$change_persons),
      as.numeric(c(
        length(unique(tp$id)) - length(unique(after_b$id)),
        length(unique(after_b$id)) - length(unique(after_a$id))
      )),
      info = eid
    )
  }
})

test_that("each step-2 criterion is the exact label string", {
  r <- spr_get("rule")
  expect_identical(
    spr_step2(r$plan, "01")$criterion,
    c(
      "Excluded (fewer than 10 at baseline): Prior rare condition B",
      "Excluded (fewer than 10 at baseline): Prior rare condition A"
    )
  )
})

# ---------------------------------------------------------------------------
# Parity without the rule, against what 27.2.0 wrote
# ---------------------------------------------------------------------------

spr_ref <- function() {
  return(readRDS(test_path(
    "fixtures",
    "sparse-rule-27.2.0",
    "reference.rds"
  )))
}

test_that("without a rare_condition_rule the enrollment counts equal 27.2.0's", {
  r <- spr_get("none")
  plain <- lapply(r$plan$enrollment_counts, function(ec) {
    return(lapply(ec, as.data.frame))
  })
  expect_identical(plain, spr_ref()$enrollment_counts)
})

test_that("without a rare_condition_rule the confounder sets equal 27.2.0's", {
  r <- spr_get("none")
  expect_identical(r$plan$ett$confounder_vars, spr_ref()$confounder_vars)
  expect_null(r$plan$get_sparse_decision())
  expect_null(r$plan$sparse_rule_hash)
})

test_that("without a rare_condition_rule the s1a pre files carry 27.2.0's columns", {
  r <- spr_get("none")
  ref <- spr_ref()
  expect_identical(lapply(r$pre, function(p) names(p$tuples)), ref$tuple_cols)
  expect_identical(
    lapply(r$pre, function(p) names(p$attrition)),
    ref$attrition_cols
  )
})

# ---------------------------------------------------------------------------
# A failed s1b, then a retry
# ---------------------------------------------------------------------------

test_that("a failed s1b keeps the previous decision for the retry", {
  r <- spr_run_s1(spr_rule())
  plan <- r$plan
  x <- spr_var("osd_rare_a")
  before <- plan$get_sparse_decision()
  hash <- plan$sparse_rule_hash
  cv_before <- plan$ett$confounder_vars
  # The first run excluded x, so x is in no confounder set.
  expect_identical(before$decision[before$variable == x], "excluded")
  expect_false(any(vapply(cv_before, function(cv) x %in% cv, logical(1))))

  run_s1 <- function() {
    return(suppressWarnings(invisible(utils::capture.output(
      plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = swereg:::.swereg_dev_path(),
        check_skeletons = FALSE
      ),
      type = "output"
    ))))
  }

  # s1b fails: the plan keeps the decision, the hash and the confounder sets.
  local({
    testthat::local_mocked_bindings(
      .s1b_run = function(...) stop("s1b failed in the test"),
      .package = "swereg"
    )
    expect_error(run_s1(), "s1b failed in the test")
  })
  expect_identical(plan$get_sparse_decision(), before)
  expect_identical(plan$sparse_rule_hash, hash)
  expect_identical(plan$ett$confounder_vars, cv_before)

  # The retry adjusts for x: x goes back into every confounder set.
  retry <- data.table::copy(before)
  retry$decision[retry$variable == x] <- "adjusted"
  local({
    testthat::local_mocked_bindings(
      .s1b_run = function(...) retry,
      .package = "swereg"
    )
    run_s1()
  })
  expect_identical(plan$get_sparse_decision(), retry)
  expect_identical(plan$sparse_rule_hash, hash)
  expect_identical(
    plan$ett$confounder_vars,
    lapply(spr_ref()$confounder_vars, function(cv) {
      return(cv[cv != spr_var("osd_rare_b")])
    })
  )
})

# ---------------------------------------------------------------------------
# An s1 that fails after s1b, then s2 and s3
# ---------------------------------------------------------------------------

# Call the real s2 up to the line after its guards. The mock stops s2 at the
# next call, so a guard that passes gives this sentinel error.
spr_s2_past_guards <- function(plan) {
  testthat::local_mocked_bindings(
    .threads_per_worker = function(...) stop("s2 passed its guards"),
    .package = "swereg"
  )
  return(plan$s2_generate_analysis_files_and_ipcw_pp(n_workers = 1L))
}

# The three stages on a fixture plan, with the output muffled. `ett_ids`
# narrows s3.
spr_stage <- function(plan, stage, ett_ids = NULL) {
  dev <- swereg:::.swereg_dev_path()
  run <- switch(
    stage,
    s1 = function() {
      plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = dev,
        check_skeletons = FALSE
      )
    },
    s2 = function() {
      plan$s2_generate_analysis_files_and_ipcw_pp(
        n_workers = 1L,
        swereg_dev_path = dev
      )
    },
    s3 = function() {
      plan$s3_analyze(ett_ids = ett_ids, n_workers = 1L, swereg_dev_path = dev)
    }
  )
  suppressWarnings(invisible(utils::capture.output(run(), type = "output")))
  return(invisible(plan))
}

# The first decision with x adjusted, where the first run excluded x.
spr_changed_decision <- function(plan) {
  x <- spr_var("osd_rare_a")
  d <- data.table::copy(plan$get_sparse_decision())
  d$decision[d$variable == x] <- "adjusted"
  return(d)
}

# One plan per scenario, built once per file. Each starts with a real s1, s2
# and s3, and then runs s1 again with the decision of
# `spr_changed_decision()`. s3 analyses the first ETT only, which stores one
# enrollment result and one ETT result in half the time. `failed`: s1e of the second run fails. `rerun`:
# the second run completes. `results` holds the stored s3 results of the
# first run, and `first_id` its s1 run id.
spr_two_runs <- new.env(parent = emptyenv())
spr_two_run_plan <- function(name) {
  if (!is.null(spr_two_runs[[name]])) {
    return(spr_two_runs[[name]])
  }
  plan <- spr_run_s1(spr_rule())$plan
  spr_stage(plan, "s2")
  spr_stage(plan, "s3", ett_ids = plan$ett$ett_id[1L])
  out <- list(
    first_id = plan$s1_run_id,
    results = list(enr = plan$results_enrollment, ett = plan$results_ett),
    new_decision = spr_changed_decision(plan)
  )
  dispatch <- swereg:::.batch_run_and_write
  out$error <- local({
    testthat::local_mocked_bindings(
      .s1b_run = function(...) out$new_decision,
      .batch_run_and_write = function(..., label) {
        if (identical(name, "failed") && identical(label, "s1e")) {
          stop("s1e failed in the test")
        }
        return(dispatch(..., label = label))
      },
      .package = "swereg"
    )
    tryCatch(
      {
        spr_stage(plan, "s1")
        NULL
      },
      error = conditionMessage
    )
  })
  out$plan <- plan
  spr_two_runs[[name]] <- out
  return(out)
}

# Put the stored s3 results of the first run back on the plan, until the
# calling test ends.
spr_restore_first_results <- function(f, env = parent.frame()) {
  f$plan$results_enrollment <- f$results$enr
  f$plan$results_ett <- f$results$ett
  withr::defer(
    {
      f$plan$results_enrollment <- NULL
      f$plan$results_ett <- NULL
    },
    envir = env
  )
  return(invisible(f$plan))
}

# The three writers that read the decision and the stored s3 results.
spr_export_tables <- function(plan) {
  return(plan$export_tables(path = tempfile(fileext = ".xlsx")))
}
spr_export <- function(plan) {
  return(plan$export(
    list(list(type = "table1", enrollment = "01")),
    dir = tempfile("spr_export_")
  ))
}
spr_checklist <- function(plan) {
  return(utils::capture.output(plan$print_target_checklist()))
}

test_that("s2 refuses a plan whose s1 did not complete", {
  # A completed s1, with and without a rule, lets s2 pass its guards.
  for (name in c("rule", "none")) {
    plan <- spr_get(name)$plan
    expect_true(swereg:::.tte_s1_run_current(plan), info = name)
    expect_error(spr_s2_past_guards(plan), "s2 passed its guards", info = name)
  }
  # The second s1 run set the new decision, and its s1e failed.
  f <- spr_two_run_plan("failed")
  expect_identical(f$error, "s1e failed in the test")
  expect_identical(f$plan$get_sparse_decision(), f$new_decision)
  expect_null(f$plan$s1_run_id)
  expect_error(
    spr_s2_past_guards(f$plan),
    "s2_generate_analysis_files_and_ipcw_pp\\(\\): s1 did not complete"
  )
})

test_that("s3 refuses a plan whose s1 did not complete", {
  f <- spr_two_run_plan("failed")
  expect_error(
    f$plan$s3_analyze(
      n_workers = 1L,
      swereg_dev_path = swereg:::.swereg_dev_path()
    ),
    "s3_analyze\\(\\): s1 did not complete"
  )
})

test_that("s3 refuses analysis files from an earlier s1 run", {
  f <- spr_two_run_plan("rerun")
  plan <- f$plan
  # The second s1 run completed with another decision, under a new id.
  expect_null(f$error)
  expect_identical(plan$get_sparse_decision(), f$new_decision)
  expect_true(swereg:::.tte_s1_run_current(plan))
  expect_false(identical(plan$s1_run_id, f$first_id))
  # s2 last ran after the first s1 run, so every ETT carries its id.
  expect_identical(plan$ett$s1_run_id, rep(f$first_id, nrow(plan$ett)))
  expect_error(
    plan$s3_analyze(
      n_workers = 1L,
      swereg_dev_path = swereg:::.swereg_dev_path()
    ),
    paste0(
      "s3_analyze\\(\\): s2 did not write the analysis files of ",
      nrow(plan$ett),
      " ETT\\(s\\) after the current s1 run"
    )
  )
})

# One test per writer. s1 drops the stored s3 results when it starts, so a
# writer without its s1 guard stops at "No enrollment results" for another
# reason. Each test puts the results of the first, completed run back on the
# plan. The writer then has every input it reads, and only its s1 guard
# stops it. A writer without the guard writes its output.
SPR_S1_FAILED_MSG <- paste0(
  ": the plan holds a sparse decision, but s1 did not complete for it"
)

test_that("export_tables refuses a plan whose s1 failed", {
  f <- spr_two_run_plan("failed")
  plan <- f$plan
  expect_null(plan$results_enrollment)
  expect_null(plan$results_ett)
  spr_restore_first_results(f)
  path <- tempfile(fileext = ".xlsx")
  expect_error(
    plan$export_tables(path = path),
    paste0("export_tables\\(\\)", SPR_S1_FAILED_MSG)
  )
  expect_false(file.exists(path))
})

test_that("export refuses a plan whose s1 failed", {
  f <- spr_two_run_plan("failed")
  plan <- f$plan
  spr_restore_first_results(f)
  dir <- tempfile("spr_export_")
  expect_error(
    plan$export(list(list(type = "table1", enrollment = "01")), dir = dir),
    paste0("export\\(\\)", SPR_S1_FAILED_MSG)
  )
  expect_length(list.files(dir), 0L)
})

test_that("checklist refuses a plan whose s1 failed", {
  f <- spr_two_run_plan("failed")
  plan <- f$plan
  # The plan as the failed s1 left it, without stored s3 results.
  expect_error(
    out <- spr_checklist(plan),
    paste0("print_target_checklist\\(\\)", SPR_S1_FAILED_MSG)
  )
  # The plan with the stored s3 results of the first run.
  spr_restore_first_results(f)
  expect_error(
    out <- spr_checklist(plan),
    paste0("print_target_checklist\\(\\)", SPR_S1_FAILED_MSG)
  )
  expect_false(exists("out", inherits = FALSE))
})

test_that("export refuses results stamped by an earlier s1 run", {
  f <- spr_two_run_plan("rerun")
  plan <- f$plan
  # The completed second s1 run dropped the stored results of the first, so
  # the checklist that tte_stage() prints after s1 runs.
  expect_null(plan$results_enrollment)
  expect_null(plan$results_ett)
  expect_no_error(spr_checklist(plan))
  # No public method puts them back. A test does, as a plan that kept them.
  spr_restore_first_results(f)
  stamps <- vapply(
    c(plan$results_enrollment, plan$results_ett),
    function(r) r[["s1_run_id"]],
    character(1)
  )
  expect_identical(unique(stamps), f$first_id)
  n <- length(stamps)
  msg <- paste0(
    "\\(\\): ",
    n,
    " stored s3 result\\(s\\) do not come from the current s1 run"
  )
  expect_error(spr_export_tables(plan), paste0("export_tables", msg))
  expect_error(spr_export(plan), paste0("export", msg))
  expect_error(spr_checklist(plan), paste0("print_target_checklist", msg))
})

test_that("a save and a load keep the s1 run id and every stamp", {
  f <- spr_two_run_plan("rerun")
  plan <- f$plan
  spr_restore_first_results(f)
  loaded <- swereg::tteplan_locate_and_load(dirname(plan$save()))
  stamps <- function(p) {
    return(lapply(c(p$results_enrollment, p$results_ett), `[[`, "s1_run_id"))
  }
  expect_identical(loaded$s1_run_id, plan$s1_run_id)
  expect_identical(loaded$ett$s1_run_id, plan$ett$s1_run_id)
  expect_identical(stamps(loaded), stamps(plan))
  expect_length(stamps(loaded), 2L)
  # The loaded decision gives the digest of the saved one.
  expect_true(swereg:::.tte_s1_run_current(loaded))
})


test_that("the preregistration workbook and print show no decision of an s1 that failed after s1b", {
  skip_if_not_installed("openxlsx")
  f <- spr_two_run_plan("failed")
  expect_null(f$plan$s1_run_id)
  # The failed run's decision is on the plan, and its rule hash matches the
  # spec, so only the s1 completion test can keep it out.
  expect_identical(f$plan$get_sparse_decision(), f$new_decision)
  expect_identical(
    f$plan$sparse_rule_hash,
    swereg:::.tte_sparse_rule_hash(f$plan$spec$rare_condition_rule)
  )
  pending <- "not decided yet (s1 has not completed for this rule)"

  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  suppressMessages(f$plan$excel_spec_summary(path = path))
  expect_identical(
    openxlsx::getSheetNames(path),
    c("Study Specification", "Target trial protocol")
  )
  ss <- openxlsx::read.xlsx(
    path,
    sheet = "Study Specification",
    colNames = FALSE,
    skipEmptyRows = FALSE
  )
  expect_identical(ss[[2]][which(ss[[1]] == "Decision:")], pending)
  expect_false(any(c("Excluded conditions:", "Final adjustment set:") %in% ss[[1]]))

  txt <- trimws(
    gsub("\033\\[[0-9;]*m", "", utils::capture.output(f$plan$print_spec_summary())),
    which = "right"
  )
  expect_true(paste0("  Decision:             ", pending) %in% txt)
  expect_false(any(grepl("Excluded conditions:", txt, fixed = TRUE)))
})
