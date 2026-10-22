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
suppressMessages(suppressWarnings(
  .ro_plan$export_tables(path = file.path(.ro_dir, "tables.xlsx"))
))
.ro <- qs2_read(file.path(.ro_dir, "results.qs2"))

# The person and person-trial identifier columns that no table of the object
# may carry. `TTEDesign` defaults to `id` and `enrollment_person_trial_id`.
.ro_id_cols <- c("id", "lopnr", "person_id", "enrollment_person_trial_id")
# The only identifier columns a table MAY carry. Each names an emulated trial,
# an enrollment or an enrollment period, never a person.
.ro_id_allowed <- c("ett_id", "enrollment_id", "enrollment_period_id")

# Every column name of every data.frame in `x`, at any depth of a plain list
# or of a list column.
.ro_column_names <- function(x) {
  if (is.data.frame(x)) {
    nested <- Filter(is.list, as.list(x))
    return(c(names(x), unlist(lapply(nested, .ro_column_names), use.names = FALSE)))
  }
  if (is.list(x)) {
    return(unlist(lapply(x, .ro_column_names), use.names = FALSE))
  }
  return(character(0))
}

test_that("results.qs2 holds the named fields, next to tables.xlsx", {
  expect_true(file.exists(file.path(.ro_dir, "tables.xlsx")))
  expect_identical(
    names(.ro),
    c(
      "estimates", "curves", "subgroups", "baselines", "attrition",
      "enrollments",
      "spec_version", "swereg_version", "swereg_version_computing",
      "s1_run_id", "s2_run_id"
    )
  )
  expect_equal(.ro$estimates, .ro_plan$get_estimates())
  expect_equal(.ro$curves, .ro_plan$get_curves())
  expect_equal(.ro$subgroups, .ro_plan$get_subgroups())
  expect_equal(.ro$baselines, .ro_plan$get_baselines())
  expect_equal(.ro$attrition, .ro_plan$get_attrition())
  expect_identical(.ro$swereg_version, as.character(utils::packageVersion("swereg")))
  expect_identical(.ro$s1_run_id, .ro_plan$s1_run_id %||% NA_character_)
})

test_that("results.qs2 has one enrollment row per enrollment, with arm counts", {
  e <- .ro$enrollments
  expect_identical(e$enrollment_id, unique(as.character(.ro_plan$ett$enrollment_id)))
  expect_identical(
    names(e),
    c(
      "enrollment_id", "enrollment_name", "intervention_name",
      "comparator_name", "n_baseline_intervention", "n_baseline_comparator"
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
  cols <- tolower(.ro_column_names(.ro))
  expect_identical(intersect(cols, .ro_id_cols), character(0))
  id_like <- unique(cols[grepl("(^|_)id$", cols)])
  expect_identical(setdiff(id_like, .ro_id_allowed), character(0))
})
