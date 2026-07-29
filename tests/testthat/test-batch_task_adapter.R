# Phase 1 of the atomic-commit migration. Two invariants, both preconditions
# for moving the TTE stage workers onto batchit's
# `run_and_write_files_atomically()` in the later phases:
#
#   1. swereg can dispatch a declared-output commit through ONE adapter
#      wrapper (`.batch_run_and_write`, R/batch_adapter.R -- the only file in
#      swereg permitted to name `batchit::`, enforced by
#      test-batch_lockdown.R).
#   2. Every path that will become a declared output is ABSOLUTE. batchit
#      REJECTS a relative declared-output path, and the failure mode of
#      getting this wrong is silent: `normalizePath(mustWork = FALSE)` returns
#      an absolute path for a path that EXISTS, but returns a non-existent
#      relative path UNCHANGED and still relative.
#
# Nothing in production calls `.batch_run_and_write` yet; the migrations come
# in the later phases. These tests pin the wrapper's forwarding contract and
# the absoluteness of the paths, so the later phases build on something
# already proven.

skip_if_not_installed("data.table")
skip_if_not_installed("withr")

# A path is absolute on POSIX if it starts with "/" or "~"; on Windows if it
# carries a drive letter or a UNC prefix. Same predicate the production
# assertions in R/r6_tteplan.R use.
.is_abs <- function(p) grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", p)

# ---------------------------------------------------------------------------
# 1. The adapter wrapper
# ---------------------------------------------------------------------------

test_that(".batch_run_and_write() exists in swereg's namespace", {
  expect_true(exists(".batch_run_and_write", envir = asNamespace("swereg"), inherits = FALSE))
  expect_true(is.function(swereg:::.batch_run_and_write))
})

test_that(".batch_run_and_write() forwards `target` as `fn`, never as `target`", {
  skip_if_not_installed("batchit")
  seen <- NULL
  testthat::local_mocked_bindings(
    run_and_write_files_atomically = function(...) {
      seen <<- list(...)
      "SENTINEL_RETURN"
    },
    .package = "batchit"
  )

  res <- swereg:::.batch_run_and_write(
    "TARGET_SENTINEL",
    items = list(a = 1),
    outputs = list(a = "/tmp/nonexistent_declared_output.qs2"),
    n_workers = 1L,
    label = "phase1"
  )

  expect_identical(res, "SENTINEL_RETURN")
  expect_identical(seen$fn, "TARGET_SENTINEL")
  # `target` is a live DEPRECATED alias for `fn` in batchit and passing BOTH
  # errors. The wrapper must forward exactly one of them.
  expect_false("target" %in% names(seen))
  expect_identical(seen$items, list(a = 1))
  expect_identical(seen$outputs, list(a = "/tmp/nonexistent_declared_output.qs2"))
  expect_identical(seen$label, "phase1")
})

test_that(".batch_run_and_write() defaults style to 'return' and honours an override", {
  skip_if_not_installed("batchit")
  seen <- NULL
  testthat::local_mocked_bindings(
    run_and_write_files_atomically = function(...) {
      seen <<- list(...)
      NULL
    },
    .package = "batchit"
  )

  swereg:::.batch_run_and_write("T", items = list(a = 1))
  expect_identical(seen$style, "return")

  swereg:::.batch_run_and_write("T", items = list(a = 1), style = "staged_writer")
  expect_identical(seen$style, "staged_writer")
})

test_that("batchit::run_and_write_files_atomically() has no `...`, so a bad name errors", {
  skip_if_not_installed("batchit")
  # Pinning the reason the wrapper needs no name-munging: the callee takes a
  # fixed formal list, so an unknown name passed through the wrapper's `...`
  # is a hard error rather than a silently dropped argument.
  fmls <- names(formals(batchit::run_and_write_files_atomically))
  expect_false("..." %in% fmls)
  expect_true(all(
    c("fn", "items", "outputs", "style", "n_workers", "dev_path", "p", "label", "timeout") %in%
      fmls
  ))

  expect_error(
    swereg:::.batch_run_and_write("T", items = list(a = 1), no_such_argument_name = 1),
    "unused argument"
  )
})

# ---------------------------------------------------------------------------
# 2. Absolute paths
# ---------------------------------------------------------------------------

test_that(".s1_work_dir() returns an ABSOLUTE path from a relative data_meta_dir", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  dir.create("meta_rel")
  expect_true(dir.exists("meta_rel"))

  # `.s1_work_dir()` reaches for plan$registrystudy$data_meta_dir and
  # plan$project_prefix only, so a plain list is a faithful stand-in.
  plan <- list(
    registrystudy = list(data_meta_dir = "meta_rel"),
    project_prefix = "phase1prefix"
  )
  work_dir <- swereg:::.s1_work_dir(plan)

  expect_true(.is_abs(work_dir))
  expect_true(dir.exists(work_dir))
  expect_identical(basename(work_dir), "phase1prefix")
})

test_that("s2 builds declared-output paths ABSOLUTE from a relative output_dir", {
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = "ETT00001",
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    follow_up = 52L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = "analysis_001.qs2",
    description = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "phase1_abs",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )

  captured <- NULL
  testthat::local_mocked_bindings(
    .batch_run = function(...) {
      captured <<- list(...)$items
      stop("__SENTINEL_S2_ABS__")
    },
    .package = "swereg"
  )

  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  # Relative AND not yet existing -- the exact case where normalizing too
  # early would leave the path relative.
  expect_error(
    plan$s2_generate_analysis_files_and_ipcw_pp(
      output_dir = "outs_rel",
      n_workers = 1L
    ),
    "__SENTINEL_S2_ABS__"
  )

  expect_false(is.null(captured))
  expect_gt(length(captured), 0L)
  apaths <- vapply(captured, function(it) it$file_analysis_path, character(1))
  expect_true(all(.is_abs(apaths)))
  # And the field the plan reports is deliberately NOT normalized: s3_analyze
  # falls back to it, so absolutizing it would change what a saved plan says.
  expect_true(all(basename(apaths) %in% c("analysis_001.qs2", "analysis_itt_001.qs2")))
})
