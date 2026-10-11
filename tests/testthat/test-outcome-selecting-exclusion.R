# An exclusion criterion with the window `lifetime_before_and_after_baseline`
# reads the weeks after time zero. On an outcome variable it removes each
# person who has the outcome during follow-up, so the study selects on its own
# outcome. `tteplan_check_spec()` reports it as a row, and
# `tteplan_read_spec()` refuses it, so s0 refuses it too.
#
# Each spec here is the `valid` check_spec fixture with one edit.

skip_if_not_installed("yaml")

ose_spec <- function() {
  return(yaml::read_yaml(
    testthat::test_path("fixtures", "check_spec", "valid", "spec_v001.yaml")
  ))
}

ose_write <- function(spec) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(spec, path)
  return(path)
}

OSE_GLOBAL_MESSAGE <- paste0(
  "exclusion_criteria[1] 'Lifetime exclusion' has the window ",
  "lifetime_before_and_after_baseline on 'osd_a', which is the variable of ",
  "outcome 'Outcome A'. That window reads the weeks after time zero, so the ",
  "criterion excludes each person who has the outcome during follow-up. Use ",
  "lifetime_before_baseline, or a window that ends before time zero."
)

# The global exclusion on outcome A's variable.
ose_global_spec <- function() {
  spec <- ose_spec()
  spec$exclusion_criteria[[1]]$implementation$source_variable <- "osd_a"
  spec$exclusion_criteria[[1]]$implementation$window <-
    "lifetime_before_and_after_baseline"
  return(spec)
}


test_that("check_spec reports a global exclusion that selects on an outcome", {
  r <- tteplan_check_spec(ose_write(ose_global_spec()))
  # One row: the read_error of the same finding is not repeated.
  expect_identical(r$path, "$/exclusion_criteria[]/implementation/window")
  expect_identical(r$kind, "outcome_selecting_exclusion")
  expect_identical(r$message, OSE_GLOBAL_MESSAGE)
})


test_that("read_spec refuses a global exclusion that selects on an outcome", {
  expect_error(
    tteplan_read_spec(ose_write(ose_global_spec())),
    OSE_GLOBAL_MESSAGE,
    fixed = TRUE
  )
})


test_that("s0 refuses a global exclusion that selects on an outcome", {
  path <- ose_write(ose_global_spec())
  root <- withr::local_tempdir()
  expect_error(
    tteplan_from_spec_and_registrystudy(
      study = list(skeleton_files = file.path(root, "skeleton_001.qs2")),
      candidate_dir_spec = dirname(path),
      candidate_dir_tteplan = root,
      candidate_dir_results = root,
      spec_version = "v001",
      # A supplied cutoff loads no skeleton, so without the refusal s0
      # builds a plan and returns.
      global_max_isoyearweek = "2020-52",
      check_skeletons = FALSE
    ),
    OSE_GLOBAL_MESSAGE,
    fixed = TRUE
  )
})


test_that("an additional_exclusion on one source of a multi-source outcome is refused", {
  spec <- ose_spec()
  # Outcome B reads two variables, and the criterion reads one of them.
  spec$outcomes[[2]]$implementation$variable <- list("osd_b", "osd_b2")
  spec$enrollments[[2]]$additional_exclusion <- list(list(
    name = "Ever B2",
    implementation = list(
      source_variable = "osd_b2",
      window = "lifetime_before_and_after_baseline"
    )
  ))
  msg <- paste0(
    "enrollments[2] '02' additional_exclusion[1] 'Ever B2' has the window ",
    "lifetime_before_and_after_baseline on 'osd_b2', which is the variable ",
    "of outcome 'Outcome B'. That window reads the weeks after time zero, so ",
    "the criterion excludes each person who has the outcome during ",
    "follow-up. Use lifetime_before_baseline, or a window that ends before ",
    "time zero."
  )
  path <- ose_write(spec)

  r <- tteplan_check_spec(path)
  expect_identical(
    r$path,
    "$/enrollments[]/additional_exclusion[]/implementation/window"
  )
  expect_identical(r$kind, "outcome_selecting_exclusion")
  expect_identical(r$message, msg)
  expect_error(tteplan_read_spec(path), msg, fixed = TRUE)
})


test_that("the same window on a variable that is no outcome stays accepted", {
  spec <- ose_spec()
  spec$exclusion_criteria[[1]]$implementation$window <-
    "lifetime_before_and_after_baseline"
  spec$enrollments[[1]]$additional_exclusion <- list(list(
    name = "Ever X2",
    implementation = list(
      source_variable = "osd_x2",
      window = "lifetime_before_and_after_baseline"
    )
  ))
  path <- ose_write(spec)

  expect_identical(nrow(tteplan_check_spec(path)), 0L)
  parsed <- tteplan_read_spec(path)
  expect_identical(
    parsed$exclusion_criteria[[1]]$implementation$window,
    "lifetime_before_and_after_baseline"
  )
  expect_identical(
    parsed$enrollments[[1]]$additional_exclusion[[1]]$implementation$window,
    "lifetime_before_and_after_baseline"
  )
})


test_that("an outcome variable under another window stays accepted", {
  spec <- ose_spec()
  spec$exclusion_criteria[[1]]$implementation$source_variable <- "osd_a"
  path <- ose_write(spec)

  expect_identical(nrow(tteplan_check_spec(path)), 0L)
  expect_identical(
    tteplan_read_spec(path)$exclusion_criteria[[1]]$implementation$window,
    "lifetime_before_baseline"
  )
})
