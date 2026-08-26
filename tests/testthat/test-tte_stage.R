# tte_stage() forwards every argument BY NAME.
#
# The three stage methods take their arguments in different orders and none of
# them takes `...`. s3_analyze() reverses swereg_dev_path and n_workers
# relative to s1 and s2, so a positional forward binds n_workers to
# enrollment_ids and R reports nothing. Every test below drives the real
# tte_stage() against a fake plan whose formal NAMES are asserted to match the
# real TTEPlan generator, so the fixture cannot drift away from the signature
# it stands in for.

# Fake plan + call recorder. The defaults are sentinels: a positional forward
# leaves a sentinel where a supplied value belongs, which is what makes the
# by-name mutation visible.
.ts_fixture <- function() {
  rec <- new.env(parent = emptyenv())
  rec$calls <- character(0)
  rec$args <- list()
  rec$loaded <- FALSE
  rec$progress <- FALSE
  rec$dir <- NULL

  note <- function(what) rec$calls <- c(rec$calls, what)

  plan <- list(
    s1_generate_enrollments_and_ipw = function(
      output_dir = "D_output_dir",
      impute_fn = "D_impute_fn",
      stabilize = "D_stabilize",
      n_workers = "D_n_workers",
      swereg_dev_path = "D_swereg_dev_path"
    ) {
      rec$args$s1 <- list(
        output_dir = output_dir,
        impute_fn = impute_fn,
        stabilize = stabilize,
        n_workers = n_workers,
        swereg_dev_path = swereg_dev_path
      )
      note("s1_generate_enrollments_and_ipw")
      invisible(NULL)
    },
    s2_generate_analysis_files_and_ipcw_pp = function(
      output_dir = "D_output_dir",
      estimate_ipcw_pp_separately_by_treatment = "D_separately",
      estimate_ipcw_pp_with_gam = "D_gam",
      n_workers = "D_n_workers",
      swereg_dev_path = "D_swereg_dev_path"
    ) {
      rec$args$s2 <- list(
        output_dir = output_dir,
        estimate_ipcw_pp_separately_by_treatment = estimate_ipcw_pp_separately_by_treatment,
        estimate_ipcw_pp_with_gam = estimate_ipcw_pp_with_gam,
        n_workers = n_workers,
        swereg_dev_path = swereg_dev_path
      )
      note("s2_generate_analysis_files_and_ipcw_pp")
      invisible(NULL)
    },
    s3_analyze = function(
      enrollment_ids = "D_enrollment_ids",
      ett_ids = "D_ett_ids",
      output_dir = "D_output_dir",
      swereg_dev_path = "D_swereg_dev_path",
      n_workers = "D_n_workers"
    ) {
      rec$args$s3 <- list(
        enrollment_ids = enrollment_ids,
        ett_ids = ett_ids,
        output_dir = output_dir,
        swereg_dev_path = swereg_dev_path,
        n_workers = n_workers
      )
      note("s3_analyze")
      invisible(NULL)
    },
    save = function(dir = NULL) {
      note("save")
      invisible(NULL)
    },
    print_target_checklist = function() {
      note("print_target_checklist")
      invisible(NULL)
    },
    results_summary = function() {
      note("results_summary")
      invisible(NULL)
    }
  )
  list(rec = rec, plan = plan)
}

# Install the fake plan in place of the real load, and record whether the load
# ran at all. `rec$loaded` is the observable that proves WHEN the unknown-name
# rejection fires.
.ts_mock <- function(fx, env = parent.frame()) {
  testthat::local_mocked_bindings(
    setup_progress_handlers = function() {
      fx$rec$progress <- TRUE
      invisible(NULL)
    },
    tteplan_locate_and_load = function(candidate_dir_tteplan) {
      fx$rec$loaded <- TRUE
      fx$rec$dir <- candidate_dir_tteplan
      fx$plan
    },
    .package = "swereg",
    .env = env
  )
}


test_that("the fake plan carries the real stage signatures", {
  # A fixture that drifts from the real formals would let a broken forward
  # look correct. Compare NAMES and ORDER; the defaults are deliberately
  # sentinels and are not compared.
  fx <- .ts_fixture()
  pm <- swereg::TTEPlan$public_methods
  for (m in c(
    "s1_generate_enrollments_and_ipw",
    "s2_generate_analysis_files_and_ipcw_pp",
    "s3_analyze"
  )) {
    expect_identical(
      names(formals(fx$plan[[m]])),
      names(formals(pm[[m]])),
      info = m
    )
  }
  # The property the by-name forward exists for: s3 orders its last two
  # formals the other way round.
  expect_identical(
    names(formals(pm$s3_analyze))[4:5],
    c("swereg_dev_path", "n_workers")
  )
  expect_identical(
    names(formals(pm$s1_generate_enrollments_and_ipw))[4:5],
    c("n_workers", "swereg_dev_path")
  )
})


test_that("tte_stage('s1') forwards by name, then saves and prints the checklist", {
  fx <- .ts_fixture()
  .ts_mock(fx)

  out <- swereg::tte_stage(
    "s1",
    "/no/such/plan/dir",
    n_workers = 4L,
    stabilize = FALSE,
    swereg_dev_path = "DEV"
  )

  expect_identical(
    fx$rec$args$s1,
    list(
      output_dir = "D_output_dir",
      impute_fn = "D_impute_fn",
      stabilize = FALSE,
      n_workers = 4L,
      swereg_dev_path = "DEV"
    )
  )
  expect_identical(
    fx$rec$calls,
    c("s1_generate_enrollments_and_ipw", "save", "print_target_checklist")
  )
  expect_true(fx$rec$loaded)
  expect_true(fx$rec$progress)
  expect_identical(fx$rec$dir, "/no/such/plan/dir")
  expect_identical(out, fx$plan)
})


test_that("tte_stage('s2') forwards by name and runs no step after the method", {
  fx <- .ts_fixture()
  .ts_mock(fx)

  swereg::tte_stage(
    "s2",
    "/no/such/plan/dir",
    n_workers = 3L,
    estimate_ipcw_pp_with_gam = FALSE,
    swereg_dev_path = "DEV"
  )

  expect_identical(
    fx$rec$args$s2,
    list(
      output_dir = "D_output_dir",
      estimate_ipcw_pp_separately_by_treatment = "D_separately",
      estimate_ipcw_pp_with_gam = FALSE,
      n_workers = 3L,
      swereg_dev_path = "DEV"
    )
  )
  expect_identical(fx$rec$calls, "s2_generate_analysis_files_and_ipcw_pp")
})


test_that("tte_stage('s3') forwards by name, then summarises results and saves", {
  fx <- .ts_fixture()
  .ts_mock(fx)

  # s3 declares swereg_dev_path BEFORE n_workers. A positional forward of
  # `n_workers = 2L` binds 2L to enrollment_ids, s3's first formal.
  swereg::tte_stage(
    "s3",
    "/no/such/plan/dir",
    n_workers = 2L,
    output_dir = "OUT"
  )

  expect_identical(
    fx$rec$args$s3,
    list(
      enrollment_ids = "D_enrollment_ids",
      ett_ids = "D_ett_ids",
      output_dir = "OUT",
      swereg_dev_path = "D_swereg_dev_path",
      n_workers = 2L
    )
  )
  expect_identical(
    fx$rec$calls,
    c("s3_analyze", "results_summary", "save")
  )
})


test_that("an unnamed element of ... is an error", {
  fx <- .ts_fixture()
  .ts_mock(fx)

  expect_error(
    swereg::tte_stage("s1", "/no/such/plan/dir", 4L),
    "must be named"
  )
  expect_identical(fx$rec$calls, character(0))

  # A partly named call is still an error.
  expect_error(
    swereg::tte_stage("s1", "/no/such/plan/dir", n_workers = 4L, TRUE),
    "must be named"
  )
})


test_that("an unknown argument name is rejected BEFORE the plan is loaded", {
  fx <- .ts_fixture()
  .ts_mock(fx)

  # Do NOT assert on the error message. do.call() also errors on an unknown
  # name, with a message that contains the name, so a message assertion passes
  # whether or not tte_stage() checks anything. WHEN the failure happens is the
  # whole property: rejecting early saves a plan read off a network share.
  expect_error(swereg::tte_stage("s1", "/no/such/plan/dir", no_such_arg = 1))
  expect_false(fx$rec$loaded)
  expect_identical(fx$rec$calls, character(0))
})


test_that("an argument valid for another stage is rejected for this one", {
  fx <- .ts_fixture()
  .ts_mock(fx)

  # enrollment_ids is an s3 formal and is not an s1 formal.
  expect_error(
    swereg::tte_stage("s1", "/no/such/plan/dir", enrollment_ids = 1L),
    "enrollment_ids"
  )
  expect_false(fx$rec$loaded)

  expect_error(
    swereg::tte_stage("s3", "/no/such/plan/dir", stabilize = TRUE),
    "stabilize"
  )
  expect_false(fx$rec$loaded)
})


test_that("an unknown stage is rejected before the plan is loaded", {
  fx <- .ts_fixture()
  .ts_mock(fx)

  expect_error(swereg::tte_stage("s4", "/no/such/plan/dir"), "s4")
  expect_error(swereg::tte_stage("S1", "/no/such/plan/dir"), "S1")
  expect_error(swereg::tte_stage(c("s1", "s2"), "/no/such/plan/dir"), "single")
  expect_error(swereg::tte_stage(1L, "/no/such/plan/dir"), "single")
  expect_false(fx$rec$loaded)
})
