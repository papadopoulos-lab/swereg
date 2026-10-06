# Objects that swereg 26.14.0 saved load into this release under the renamed
# columns, and give the same numbers.
#
# swereg 26.15.0 renamed two stored columns:
#
#   * `entry_band_id` becomes `enrollment_period_id` in an enrollment panel.
#     TTEEnrollment schema 4 becomes 5.
#   * `band` becomes `follow_up_interval` in the stored risk-difference rows of
#     a plan. TTEPlan schema 3 becomes 4.
#
# `qs2_read()` renames both before it calls `check_version()`. The fixtures
# under `fixtures/schema-26.14.0/` were written by the INSTALLED swereg 26.14.0
# (see `make.R` there), so every object carries the method bodies 26.14.0
# serialised with it. That is the point. The saved `check_version()` body
# refuses an object below the package constant, so a migration placed inside
# the new `check_version()` never runs. A downgraded object built by this
# release carries the new bodies and cannot show that.
#
# The test drives the production readers, `qs2_read()` and `tteplan_load()`,
# and the s2 and s3 worker functions the pipeline dispatches.
#
# A 26.14.0 enrollment has no `weeks_to_observation_gap` column, so
# `qs2_read()` warns once per file per R process. Each test that reads first
# forgets the files that already warned, so its counts do not depend on test
# order. The test pins the warning text on every read, and no other warning is
# allowed.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")

.smg_dir <- testthat::test_path("fixtures", "schema-26.14.0")
.smg_expected <- function() {
  return(qs2::qs_read(file.path(.smg_dir, "expected.qs2")))
}
# The renames this release documents, applied to a 26.14.0 output.
.smg_rename <- function(x, old, new) {
  x <- data.table::copy(x)
  data.table::setnames(x, old, new)
  return(x)
}

.smg_legacy_text <- paste(
  "This enrollment was enrolled before swereg 26.15.0, so its panel has no",
  "`weeks_to_observation_gap` column. Gaps in observation cannot be detected",
  "in it, and an outcome after such a gap may be counted. Re-run s1",
  "(`$s1_generate_enrollments_and_ipw()`) to remove this limitation."
)
# Evaluates `expr` and expects exactly `n` warnings, each one the legacy
# warning. Returns the value of `expr`.
.smg_expect_legacy <- function(expr, n = 1L) {
  msgs <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_identical(msgs, rep(.smg_legacy_text, n))
  return(value)
}

test_that("the fixtures are 26.14.0 objects at the previous schemas", {
  e <- .smg_expected()
  expect_identical(e$swereg_version, "26.14.0")

  # Read past the migration, with qs2 directly, to see what is on disk.
  imp <- qs2::qs_read(file.path(.smg_dir, e$ett$file_imp))
  expect_identical(swereg:::.tte_stored_schema(imp), 4L)
  expect_true("entry_band_id" %in% names(imp$data))
  expect_false("enrollment_period_id" %in% names(imp$data))

  plan <- qs2::qs_read(file.path(.smg_dir, "tteplan.qs2"))
  expect_identical(swereg:::.tte_stored_schema(plan), 3L)
  expect_true("band" %in% names(plan$results_ett[[1L]]$rd_itt))

  # The saved private `enroll()` is the 26.14.0 body. It names the old
  # column, which this release's body does not.
  old_body <- paste(deparse(imp$.__enclos_env__$private$enroll), collapse = "")
  new_body <- paste(
    deparse(TTEEnrollment$private_methods$enroll),
    collapse = ""
  )
  expect_true(grepl("entry_band_id", old_body, fixed = TRUE))
  expect_false(grepl("entry_band_id", new_body, fixed = TRUE))
})

test_that("qs2_read() migrates a 26.14.0 enrollment and keeps every value", {
  swereg:::.tte_reset_legacy_gap_seen()
  e <- .smg_expected()
  for (f in c(e$ett$file_raw, e$ett$file_imp)) {
    on_disk <- qs2::qs_read(file.path(.smg_dir, f))
    en <- .smg_expect_legacy(qs2_read(file.path(.smg_dir, f)))
    expect_identical(swereg:::.tte_stored_schema(en), 5L)
    expect_true("enrollment_period_id" %in% names(en$data))
    expect_false("entry_band_id" %in% names(en$data))
    expect_equal(
      as.data.frame(en$data),
      as.data.frame(.smg_rename(
        on_disk$data,
        "entry_band_id",
        "enrollment_period_id"
      ))
    )
  }
})

test_that("tteplan_load() migrates a 26.14.0 plan and keeps every number", {
  e <- .smg_expected()
  plan <- tteplan_load(file.path(.smg_dir, "tteplan.qs2"))
  expect_identical(swereg:::.tte_stored_schema(plan), 4L)

  res <- plan$results_ett[[e$ett$ett_id]]
  for (slot in c("rd_itt", "rd_pp_trunc")) {
    expect_true("follow_up_interval" %in% names(res[[slot]]))
    expect_false("band" %in% names(res[[slot]]))
    expect_equal(
      res[[slot]],
      .smg_rename(e$results_ett[[slot]], "band", "follow_up_interval")
    )
  }

  curves <- plan$get_curves()
  expect_true("follow_up_interval" %in% names(curves))
  expect_false("band" %in% names(curves))
  expect_identical(nrow(curves), nrow(e$curves))
  expect_equal(
    curves,
    .smg_rename(e$curves, "band", "follow_up_interval")
  )

  # The attrition steps keep their `landmark_*` names, and every count.
  attrition <- plan$get_attrition()
  expect_equal(attrition, e$attrition)
  expect_true(all(
    c("landmark_candidates", "landmark_observed", "landmark_event_free") %in%
      attrition$step_name
  ))

  expect_equal(plan$get_estimates(), e$estimates)
})

test_that("the s2 and s3 workers give the 26.14.0 numbers on 26.14.0 s1 files", {
  swereg:::.tte_reset_legacy_gap_seen()
  e <- .smg_expected()
  ett <- e$ett
  out <- withr::local_tempdir()
  imp_path <- file.path(.smg_dir, ett$file_imp)

  # s2: the worker reads the s1 file through `qs2_read()`, then calls the
  # 26.14.0 `$s4_prepare_for_analysis()` body the object carries.
  paths <- c(
    pp = file.path(out, ett$file_analysis),
    itt = file.path(out, ett$file_analysis_itt)
  )
  # Both calls read the one s1 file, so only the first warns. The saved
  # 26.14.0 `$s4_prepare_for_analysis()` body never warns.
  n_warn <- c(pp = 1L, itt = 0L)
  for (est in names(paths)) {
    analysis <- .smg_expect_legacy(n = n_warn[[est]], swereg:::.s2_worker(
      outcome = ett$outcome_var,
      follow_up = ett$follow_up,
      file_imp_path = imp_path,
      n_threads = 1L,
      sep_by_tx = TRUE,
      with_gam = TRUE,
      estimand = est
    ))$analysis
    expect_true("enrollment_period_id" %in% names(analysis$data))
    expect_false("entry_band_id" %in% names(analysis$data))
    qs2::qs_save(analysis, paths[[est]])
  }

  # s3, one call per slot, exactly as `$s3_analyze()` builds them.
  spec <- tteplan_load(file.path(.smg_dir, "tteplan.qs2"))$spec
  # Each analysis file warns at its first read only.
  s3 <- function(path, method, weight_col, n) {
    return(.smg_expect_legacy(n = n, swereg:::.s3_ett_worker(
      analysis_path = path,
      method = method,
      weight_col = weight_col,
      ett_id = ett$ett_id,
      n_threads = 1L,
      conf_level = swereg:::.s3_conf_level(spec)
    )))
  }
  got <- c(
    s3(paths[["pp"]], "summary_and_rates", "", 1L),
    s3(paths[["pp"]], "irr", "analysis_weight_pp_trunc", 0L),
    s3(paths[["pp"]], "irr", "analysis_weight_pp", 0L),
    s3(paths[["itt"]], "irr", "ipw_trunc", 1L),
    s3(paths[["itt"]], "rates", "ipw_trunc", 0L),
    s3(paths[["pp"]], "risk_difference", "analysis_weight_pp_trunc", 0L),
    s3(paths[["itt"]], "risk_difference", "ipw_trunc", 0L)
  )
  want <- e$results_ett

  # `size_mb` is the in-memory size of the panel. The longer column name
  # changes it, and it is not an estimate, so it is compared apart.
  expect_equal(
    got$summary[setdiff(names(got$summary), "size_mb")],
    want$summary[setdiff(names(want$summary), "size_mb")]
  )
  for (slot in c(
    "rates_pp_trunc",
    "rates_pp",
    "irr_pp_trunc",
    "irr_pp",
    "irr_itt",
    "rates_itt",
    "rd_curve_pp_trunc",
    "rd_curve_itt"
  )) {
    expect_equal(got[[slot]], want[[slot]], label = slot)
  }
  for (slot in c("rd_pp_trunc", "rd_itt")) {
    expect_true("follow_up_interval" %in% names(got[[slot]]))
    expect_false("band" %in% names(got[[slot]]))
    expect_equal(
      got[[slot]],
      .smg_rename(want[[slot]], "band", "follow_up_interval"),
      label = slot
    )
  }

  # s3, the enrollment-level worker on the s1 raw file.
  # This worker reads two enrollments. The pp analysis file already warned,
  # so only the raw file warns.
  enr <- .smg_expect_legacy(
    swereg:::.s3_enrollment_worker(
      analysis_path = paths[["pp"]],
      raw_path = file.path(.smg_dir, ett$file_raw),
      enrollment_id = ett$enrollment_id,
      n_threads = 1L,
      arm_labels = e$results_enrollment[[ett$enrollment_id]]$arm_labels
    ),
    n = 1L
  )
  want_enr <- e$results_enrollment[[ett$enrollment_id]]
  for (nm in setdiff(names(enr), "computed_at")) {
    expect_equal(enr[[nm]], want_enr[[nm]], label = nm)
  }
})

test_that("one file read twice in one process warns once", {
  swereg:::.tte_reset_legacy_gap_seen()
  e <- .smg_expected()
  path <- file.path(.smg_dir, e$ett$file_imp)
  .smg_expect_legacy(qs2_read(path), n = 1L)
  .smg_expect_legacy(qs2_read(path), n = 0L)
  # The same file named by another path string is the same file.
  .smg_expect_legacy(
    qs2_read(file.path(.smg_dir, ".", e$ett$file_imp)),
    n = 0L
  )
})

test_that("an object below the previous schema is still refused", {
  e <- .smg_expected()
  en <- qs2::qs_read(file.path(.smg_dir, e$ett$file_imp))
  assign(".schema_version", 3L, envir = en$.__enclos_env__$private)
  path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(en, path)
  expect_error(qs2_read(path), "schema version 3")
})
