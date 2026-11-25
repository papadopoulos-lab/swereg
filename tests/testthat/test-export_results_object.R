# =============================================================================
# `$export_tables()` writes `results.qs2` beside the workbook
# =============================================================================
#
# A manuscript reads this file, so it MUST hold the aggregate results and MUST
# NOT hold person-level data. The plan fixture is `.xp_plan()` from
# `helper-export_parity.R`.

skip_if_not_installed("openxlsx")
skip_if_not_installed("withr")

.ro_dir <- withr::local_tempdir(.local_envir = teardown_env())
.ro_plan <- .xp_plan("new", subgroups = TRUE, fill_summary = TRUE)
# `.xp_plan()` stores horizon records with no window table, so
# `$get_horizon_windows()` gives no rows. One hand-chosen window per arm
# gives the `horizons` field rows to compare.
.ro_plan$results_ett$ETT00001$horizon_pp$windows <- data.table::data.table(
  window_lo = 0,
  window_hi = 52,
  arm = c("intervention", "comparator"),
  py_weighted = c(120.5, 240.25),
  events_weighted = c(6, 9),
  rate_pooled = 15 / 360.75,
  events_expected = c(120.5, 240.25) * 15 / 360.75,
  pass = TRUE
)
suppressMessages(suppressWarnings(
  .ro_plan$export_tables(path = file.path(.ro_dir, "tables.xlsx"))
))
.ro <- qs2_read(file.path(.ro_dir, "results.qs2"))
# The 13 fields of swereg 27.3.25, in that order.
.ro_old_names <- c(
  "estimates",
  "curves",
  "subgroups",
  "baselines",
  "attrition",
  "enrollments",
  "adherence",
  "horizons",
  "spec_version",
  "swereg_version",
  "swereg_version_computing",
  "s1_run_id",
  "s2_run_id"
)
# The fields that swereg 27.3.26 adds after them.
.ro_new_names <- c(
  "matching",
  "sparse_decision",
  "spec",
  "ett",
  "protocol_table",
  "design"
)
.ro_file_cols <- c("file_raw", "file_imp", "file_analysis", "file_analysis_itt")

# The body of `.plan_results_object()` in swereg 27.3.25, frozen verbatim.
# It is the reference for the 13 older fields.
.ro_old_object <- function(plan) {
  baselines <- plan$get_baselines()
  reported <- .plan_ett_reported(plan)
  eids <- unique(as.character(reported$enrollment_id))
  enrollments <- data.table::rbindlist(lapply(eids, function(eid) {
    arms <- .lookup_arm_labels(plan$spec, eid)
    arm <- function(which) {
      if (is.null(arms)) {
        return(NA_character_)
      }
      return(as.character(arms[[which]]))
    }
    n_arm <- function(col) {
      return(.baseline_count(baselines, eid, col))
    }
    return(data.table::data.table(
      enrollment_id = eid,
      enrollment_name = .enrollment_label(plan, eid),
      intervention_name = arm("intervention"),
      comparator_name = arm("comparator"),
      n_baseline_intervention = n_arm("n_baseline_intervention"),
      n_baseline_comparator = n_arm("n_baseline_comparator")
    ))
  }))
  s2 <- .tte_s2_run_of(reported)
  return(list(
    estimates = plan$get_estimates(),
    curves = plan$get_curves(),
    subgroups = plan$get_subgroups(),
    baselines = baselines,
    attrition = plan$get_attrition(),
    enrollments = enrollments,
    adherence = plan$get_adherence(),
    horizons = plan$get_horizon_windows(),
    spec_version = plan$spec_version %||% NA_character_,
    swereg_version = as.character(utils::packageVersion("swereg")),
    swereg_version_computing = .plan_computing_versions(plan),
    s1_run_id = plan$s1_run_id %||% NA_character_,
    s2_run_id = sort(unique(s2[!is.na(s2)]))
  ))
}

# The person and person-trial identifier columns that no table of the object
# may carry. `TTEDesign` defaults to `id` and `enrollment_person_trial_id`.
.ro_id_cols <- c("id", "lopnr", "person_id", "enrollment_person_trial_id")
# The only identifier columns a table MAY carry. Each names an emulated trial,
# an enrollment, an enrollment period or a pipeline run, never a person. The
# `ett` field carries the two run ids of each ETT.
.ro_id_allowed <- c(
  "ett_id",
  "enrollment_id",
  "enrollment_period_id",
  "s1_run_id",
  "s2_run_id"
)

# Every column name of every data.frame in `x`, at any depth of a plain list
# or of a list column.
.ro_column_names <- function(x) {
  if (is.data.frame(x)) {
    nested <- Filter(is.list, as.list(x))
    return(c(
      names(x),
      unlist(lapply(nested, .ro_column_names), use.names = FALSE)
    ))
  }
  if (is.list(x)) {
    return(unlist(lapply(x, .ro_column_names), use.names = FALSE))
  }
  return(character(0))
}

test_that("results.qs2 holds the named fields, next to tables.xlsx", {
  expect_true(file.exists(file.path(.ro_dir, "tables.xlsx")))
  expect_identical(names(.ro), c(.ro_old_names, .ro_new_names))
  expect_equal(.ro$estimates, .ro_plan$get_estimates())
  expect_equal(.ro$curves, .ro_plan$get_curves())
  expect_equal(.ro$subgroups, .ro_plan$get_subgroups())
  expect_equal(.ro$baselines, .ro_plan$get_baselines())
  expect_equal(.ro$attrition, .ro_plan$get_attrition())
  expect_gt(nrow(.ro_plan$get_adherence()), 0L)
  expect_equal(.ro$adherence, .ro_plan$get_adherence())
  expect_gt(nrow(.ro_plan$get_horizon_windows()), 0L)
  expect_equal(.ro$horizons, .ro_plan$get_horizon_windows())
  expect_identical(
    .ro$swereg_version,
    as.character(utils::packageVersion("swereg"))
  )
  expect_identical(.ro$s1_run_id, .ro_plan$s1_run_id %||% NA_character_)
})

test_that("the 13 fields of swereg 27.3.25 are computed as before", {
  old <- .ro_old_object(.ro_plan)
  expect_identical(names(old), .ro_old_names)
  expect_identical(.ro[.ro_old_names], old)
})

test_that("results.qs2 holds what a manuscript reads from the plan", {
  expect_gt(nrow(.ro_plan$get_matching()), 0L)
  expect_identical(.ro$matching, .ro_plan$get_matching())

  # The fixture holds no sparse decision, so the field is a NULL that keeps
  # its name.
  expect_true("sparse_decision" %in% names(.ro))
  expect_null(.ro_plan$get_sparse_decision())
  expect_identical(.ro$sparse_decision, .ro_plan$get_sparse_decision())

  expect_false(is.null(.ro_plan$spec))
  expect_identical(.ro$spec, .ro_plan$spec)

  expect_identical(nrow(.ro_plan$get_protocol_table()), 7L)
  expect_identical(.ro$protocol_table, .ro_plan$get_protocol_table())

  expect_identical(
    .ro$design,
    list(
      period_width = .ro_plan$period_width,
      global_max_isoyearweek = .ro_plan$global_max_isoyearweek
    )
  )
  expect_identical(
    .ro$design,
    list(period_width = 4L, global_max_isoyearweek = "2020-52")
  )
})

test_that("the ett field is the ETT grid without its file columns", {
  # The fixture grid holds three of the four file columns.
  expect_identical(
    intersect(.ro_file_cols, names(.ro_plan$ett)),
    c("file_raw", "file_imp", "file_analysis")
  )
  keep <- setdiff(names(.ro_plan$ett), .ro_file_cols)
  expect_identical(intersect(.ro_file_cols, names(.ro$ett)), character(0))
  expect_true(data.table::is.data.table(.ro$ett))
  expect_identical(.ro$ett, .ro_plan$ett[, keep, with = FALSE])
  # The field is a copy. The object leaves the plan grid as it was, and a
  # change to the field does not reach the plan.
  obj <- suppressMessages(suppressWarnings(.plan_results_object(.ro_plan)))
  expect_identical(
    intersect(.ro_file_cols, names(.ro_plan$ett)),
    c("file_raw", "file_imp", "file_analysis")
  )
  obj$ett[, outcome_name := "changed"]
  expect_false(any(.ro_plan$ett$outcome_name == "changed"))
})

test_that("a sparse decision and an intention-to-treat file column reach the object", {
  plan <- .xp_plan("new", subgroups = TRUE, fill_summary = TRUE)
  plan$sparse_decision <- data.table::data.table(
    variable = c("rd_b", "rd_a"),
    decision = c("excluded", "adjusted")
  )
  plan$ett[, file_analysis_itt := sprintf("analysis_itt_00%d.qs2", 1:8)]
  obj <- suppressMessages(suppressWarnings(.plan_results_object(plan)))
  expect_false(is.null(obj$sparse_decision))
  expect_identical(obj$sparse_decision, plan$get_sparse_decision())
  expect_identical(intersect(.ro_file_cols, names(obj$ett)), character(0))
  expect_identical(
    obj$ett,
    plan$ett[, setdiff(names(plan$ett), .ro_file_cols), with = FALSE]
  )
})

test_that("a plan without a spec gets a NULL protocol table and the 13 older fields", {
  plan <- .xp_plan("new", subgroups = TRUE, fill_summary = TRUE)
  plan$spec <- NULL
  expect_error(plan$get_protocol_table(), "plan has no spec")
  obj <- suppressMessages(suppressWarnings(.plan_results_object(plan)))
  expect_identical(names(obj), c(.ro_old_names, .ro_new_names))
  expect_null(obj$protocol_table)
  expect_null(obj$spec)
  expect_identical(
    obj[.ro_old_names],
    suppressMessages(suppressWarnings(.ro_old_object(plan)))
  )
})

test_that("results.qs2 has one enrollment row per enrollment, with arm counts", {
  e <- .ro$enrollments
  expect_identical(
    e$enrollment_id,
    unique(as.character(.ro_plan$ett$enrollment_id))
  )
  expect_identical(
    names(e),
    c(
      "enrollment_id",
      "enrollment_name",
      "intervention_name",
      "comparator_name",
      "n_baseline_intervention",
      "n_baseline_comparator"
    )
  )
  b <- .ro_plan$get_baselines()
  for (eid in e$enrollment_id) {
    expect_equal(
      e[enrollment_id == eid, n_baseline_intervention],
      b[enrollment_id == eid, n_baseline_intervention][1]
    )
    expect_equal(
      e[enrollment_id == eid, n_baseline_comparator],
      b[enrollment_id == eid, n_baseline_comparator][1]
    )
  }
})

test_that("results.qs2 holds no person or person-trial identifier column", {
  expect_true(length(Filter(is.data.frame, .ro)) >= 5L)
  expect_true(all(
    c("adherence", "horizons") %in% names(Filter(is.data.frame, .ro))
  ))
  cols <- tolower(.ro_column_names(.ro))
  expect_identical(intersect(cols, .ro_id_cols), character(0))
  id_like <- unique(cols[grepl("(^|_)id$", cols)])
  expect_identical(setdiff(id_like, .ro_id_allowed), character(0))
})

test_that("a result saved without the untruncated intention-to-treat slots exports no row for them", {
  # `.xp_plan()` stores the slots a plan held before s3 took the
  # intention-to-treat estimates on `ipw`. No slot, no row.
  slots <- unlist(lapply(.ro_plan$results_ett, names), use.names = FALSE)
  expect_false(any(grepl("_itt_untrunc$", slots)))
  e <- .ro$estimates
  expect_gt(nrow(e[estimand == "itt" & weights == "truncated"]), 0L)
  expect_identical(nrow(e[estimand == "itt" & weights == "untruncated"]), 0L)
  expect_identical(
    nrow(.ro$curves[estimand == "itt" & weights == "untruncated"]),
    0L
  )
  expect_no_warning(.ro_plan$get_estimates())
  expect_no_warning(.ro_plan$get_curves())
})
