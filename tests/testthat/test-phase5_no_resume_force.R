# Phase 5' (PROJECT.md): the TTE stages (s1/s2/s3) lost their resume/force
# heuristics -- s1 and s2 no longer take a `resume` argument, s3 no longer
# takes `force`, and s3_analyze() now recomputes every targeted result on
# every call instead of skipping anything already stored. This file pins
# that contract three ways: (1) the formals are actually gone, (2) the
# internal cache/sentinel machinery is gone from the namespace, and (3) the
# REAL boundary -- a live s3_analyze() call through .batch_run() -- proves
# the recompute, not just a helper in isolation.

skip_if_not_installed("data.table")

test_that("resume is not a formal of s1 or s2, and force is not a formal of s3", {
  gen <- swereg:::TTEPlan
  expect_false(
    "resume" %in%
      names(formals(gen$public_methods$s1_generate_enrollments_and_ipw))
  )
  expect_false(
    "resume" %in%
      names(formals(gen$public_methods$s2_generate_analysis_files_and_ipcw_pp))
  )
  expect_false(
    "force" %in% names(formals(gen$public_methods$s3_analyze))
  )
})

test_that("the deleted resume/cache internals are gone from the namespace", {
  deleted <- c(
    ".resume_fresh",
    ".s1_cache_key",
    ".skeleton_manifest_on_disk",
    ".assert_skeleton_selection",
    ".skeleton_batch_ids",
    ".touch_sentinel",
    ".s1a_done_path",
    ".s1b_done_path",
    ".s1c_done_path",
    ".s1d_done_path",
    "tteplan_s1_cache_delete"
  )
  for (fn in deleted) {
    expect_false(
      exists(fn, envir = asNamespace("swereg"), inherits = FALSE),
      info = fn
    )
  }
})

# --- Behavioral: drive the REAL boundary --------------------------------------
#
# Mirrors the fixture in test-batch_s3_production.R: a full synthetic
# s1 -> s2 -> s3 run through the actual subprocess dispatch path, not a
# fixture or a mocked .batch_run. A poisoned results_ett slot is the
# staleness a skip-cache would have preserved; recomputation must clobber it.

.phase5_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("yaml")
  dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(dev_tree, "R", "batch_adapter.R")),
    "package source tree not available"
  )
}

.phase5_build_plan <- function() {
  sk <- ttm_skeleton("A", n_persons = 2500L, seed = 2026L)
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dirs <- list(
    spec = file.path(root, "spec"),
    tteplan = file.path(root, "tteplan"),
    results = file.path(root, "results"),
    meta = file.path(root, "meta")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  skel_path <- file.path(dirs$tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  global_max <- sk[, max(isoyearweek, na.rm = TRUE)]
  ttm_write_spec(
    file.path(dirs$spec, "spec_v001.yaml"),
    "ttmphase5",
    "rd_age_continuous"
  )

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs$meta),
    candidate_dir_spec = dirs$spec,
    candidate_dir_tteplan = dirs$tteplan,
    candidate_dir_results = dirs$results,
    spec_version = "v001",
    global_max_isoyearweek = global_max
  )
  dev_path <- ttm_dev_path()
  invisible(utils::capture.output(
    {
      plan$s1_generate_enrollments_and_ipw(n_workers = 1L, swereg_dev_path = dev_path)
      plan$s2_generate_analysis_files_and_ipcw_pp(n_workers = 1L, swereg_dev_path = dev_path)
      plan$s3_analyze(n_workers = 1L, swereg_dev_path = dev_path)
    },
    type = "output"
  ))
  list(plan = plan, dev_path = dev_path)
}

test_that("s3_analyze recomputes a targeted result even when a stale value is already stored", {
  .phase5_skip()
  built <- .phase5_build_plan()
  plan <- built$plan

  expect_gt(length(plan$results_ett), 0L)
  eid <- names(plan$results_ett)[[1L]]

  # Poison the slot the way a stale skip-cache would have left it stale: a
  # recognisable sentinel value that a real recompute cannot produce.
  plan$results_ett[[eid]] <- list(POISONED = TRUE)

  invisible(utils::capture.output(
    plan$s3_analyze(n_workers = 1L, swereg_dev_path = built$dev_path),
    type = "output"
  ))

  r <- plan$results_ett[[eid]]
  expect_false(isTRUE(r$POISONED))
  expect_false(is.null(r$summary))
})

test_that("s3_analyze clears only ETTs it will recompute (combined scope keeps siblings)", {
  # Regression: the scope-clear used the raw `ett_ids`, but the recompute set
  # intersects `ett_ids` with the ETTs under the targeted enrollments. An
  # ett_id whose enrollment is OUTSIDE `enrollment_ids` was therefore dropped
  # but never recomputed -- silent result loss. Clearing must match the
  # recompute set exactly.
  ett <- data.table::data.table(
    enrollment_id   = c("E1", "E2"),
    ett_id          = c("T1", "T2"),
    outcome_var     = "osd_a",
    outcome_name    = "Outcome A",
    follow_up       = 52L,
    age_min         = 50L,
    age_max         = 59L,
    age_group       = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var   = "lopnr",
    treatment_var   = "rd_tx",
    file_imp        = c("imp_E1.qs2", "imp_E2.qs2"),
    file_raw        = c("raw_E1.qs2", "raw_E2.qs2"),
    file_analysis   = c("analysis_T1.qs2", "analysis_T2.qs2"),
    description     = c("T1", "T2")
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$results_enrollment <- list(E1 = list(x = 1), E2 = list(SURVIVE = TRUE))
  plan$results_ett <- list(T1 = list(x = 1), T2 = list(SURVIVE = TRUE))

  output_dir <- withr::local_tempdir()
  # The enrollment-item builder stats the targeted enrollment's analysis file.
  file.create(file.path(output_dir, "analysis_T1.qs2"))

  # Stop at the first dispatch: the scope-clear has already run by then, so we
  # inspect what it dropped without needing real workers.
  testthat::local_mocked_bindings(
    .batch_run = function(target, items, n_workers, ...) stop("__CLEAR_CHECK__"),
    .package = "swereg"
  )
  expect_error(
    plan$s3_analyze(
      output_dir = output_dir,
      enrollment_ids = "E1",
      ett_ids = c("T1", "T2"),
      n_workers = 1L
    ),
    "__CLEAR_CHECK__"
  )

  # T1 is in the recompute scope -> cleared. T2 (enrollment E2, outside the
  # enrollment scope) is NOT recomputed, so it must be PRESERVED.
  expect_null(plan$results_ett[["T1"]])
  expect_true(isTRUE(plan$results_ett[["T2"]]$SURVIVE))
  expect_null(plan$results_enrollment[["E1"]])
  expect_true(isTRUE(plan$results_enrollment[["E2"]]$SURVIVE))
})
