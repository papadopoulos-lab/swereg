# qs2_read() refuses a stored enrollment or plan from an older schema.
#
# The `period_id` rename raised the TTEEnrollment schema to 6 and the TTEPlan
# schema to 5. A deserialised R6 object runs the method bodies it was saved
# with, so an old object under a migrated column would fit the wrong model and
# give no error. `qs2_read()` therefore refuses it, BEFORE any method of the
# object runs, with an error that names the file and says to rebuild the plan
# with s0 and re-run s1.
#
# Each object below carries a `check_version()` that stops with its own text.
# That stands in for the body an older release saved. The refusal MUST win:
# when the error text is the stand-in's, a method of the object ran first.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")

.srf_rebuild <- "Rebuild the plan with s0 and re-run s1, then s2 and s3."
.srf_old_body <- "the saved check_version() ran"

.srf_design <- function() {
  TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = 4L,
    period_width = 4L
  )
}

# Nine people over eight weeks: enough for `TTEEnrollment$new()` to build a
# panel with one enrollment period of follow-up.
.srf_data <- function() {
  weeks <- cstime::dates_by_isoyearweek$isoyearweek
  idx <- which(weeks >= "2020-01")[1]
  wk <- weeks[idx:(idx + 7L)]
  ids <- c(paste0("I", 1:3), paste0("C", 1:6))
  data.table::rbindlist(lapply(ids, function(nm) {
    data.table::data.table(
      id = nm,
      isoyearweek = wk,
      exposed = startsWith(nm, "I"),
      eligible = TRUE,
      died = FALSE,
      age = 50
    )
  }))
}

.srf_enrollment <- function() {
  TTEEnrollment$new(
    data = .srf_data(),
    design = .srf_design(),
    ratio = 2,
    seed = 4,
    extra_cols = "isoyearweek"
  )
}

.srf_plan <- function(dir) {
  TTEPlan$new(
    project_prefix = "srf",
    skeleton_files = file.path(dir, "skel_a.qs2"),
    global_max_isoyearweek = "2020-08",
    ett = data.table::data.table(ett_id = character(0))
  )
}

# Set the stored schema, and give the object a `check_version()` that stops
# with `.srf_old_body`.
.srf_age <- function(obj, version) {
  assign(
    ".schema_version",
    as.integer(version),
    envir = obj$.__enclos_env__$private
  )
  unlockBinding("check_version", obj)
  assign(
    "check_version",
    function() stop(.srf_old_body, call. = FALSE),
    envir = obj
  )
  lockBinding("check_version", obj)
  obj
}

.srf_error <- function(path) {
  cond <- tryCatch(qs2_read(path), error = function(e) e)
  expect_s3_class(cond, "error")
  conditionMessage(cond)
}

test_that("qs2_read() refuses a schema-5 enrollment and a schema-4 plan", {
  dir <- withr::local_tempdir()
  enrollment_path <- file.path(dir, "enrollment_schema5.qs2")
  plan_path <- file.path(dir, "plan_schema4.qs2")
  qs2_write_atomic(.srf_age(.srf_enrollment(), 5L), enrollment_path)
  qs2_write_atomic(.srf_age(.srf_plan(dir), 4L), plan_path)

  # The refusal stops each read and states the remedy. Each read is caught
  # first, so the two expectations below are judged independently.
  enrollment_msg <- .srf_error(enrollment_path)
  plan_msg <- .srf_error(plan_path)
  expect_match(enrollment_msg, "re-run s1", fixed = TRUE)
  expect_match(plan_msg, "re-run s1", fixed = TRUE)

  msg <- enrollment_msg
  expect_true(grepl(enrollment_path, msg, fixed = TRUE))
  expect_true(grepl(.srf_rebuild, msg, fixed = TRUE))
  expect_true(grepl("TTEEnrollment at schema version 5", msg, fixed = TRUE))
  expect_true(grepl("requires version 6", msg, fixed = TRUE))
  expect_false(grepl(.srf_old_body, msg, fixed = TRUE))

  msg <- plan_msg
  expect_true(grepl(plan_path, msg, fixed = TRUE))
  expect_true(grepl(.srf_rebuild, msg, fixed = TRUE))
  expect_true(grepl("TTEPlan at schema version 4", msg, fixed = TRUE))
  expect_true(grepl("requires version 5", msg, fixed = TRUE))
  expect_false(grepl(.srf_old_body, msg, fixed = TRUE))
})

test_that("qs2_read() reads an enrollment and a plan at the current schema", {
  # The pass direction. A refusal that also stopped a current object would
  # pass the test above and break every reader.
  dir <- withr::local_tempdir()
  enrollment_path <- file.path(dir, "enrollment_current.qs2")
  plan_path <- file.path(dir, "plan_current.qs2")
  qs2_write_atomic(.srf_enrollment(), enrollment_path)
  qs2_write_atomic(.srf_plan(dir), plan_path)

  en <- qs2_read(enrollment_path)
  expect_s3_class(en, "TTEEnrollment")
  expect_identical(swereg:::.tte_stored_schema(en), 6L)
  expect_true(all(c("period_id", "enrollment_period_id") %in% names(en$data)))

  plan <- qs2_read(plan_path)
  expect_s3_class(plan, "TTEPlan")
  expect_identical(swereg:::.tte_stored_schema(plan), 5L)
})

test_that("objects that swereg 26.14.0 saved are refused through every reader", {
  # These files carry the method bodies the installed 26.14.0 serialised. See
  # `fixtures/schema-26.14.0/make.R`.
  dir <- testthat::test_path("fixtures", "schema-26.14.0")
  e <- qs2::qs_read(file.path(dir, "expected.qs2"))
  expect_identical(e$swereg_version, "26.14.0")

  for (f in c(e$ett$file_raw, e$ett$file_imp)) {
    path <- file.path(dir, f)
    msg <- .srf_error(path)
    expect_true(grepl(path, msg, fixed = TRUE))
    expect_true(grepl("TTEEnrollment at schema version 4", msg, fixed = TRUE))
    expect_true(grepl(.srf_rebuild, msg, fixed = TRUE))
  }

  plan_path <- file.path(dir, "tteplan.qs2")
  cond <- tryCatch(tteplan_load(plan_path), error = function(e) e)
  expect_s3_class(cond, "error")
  msg <- conditionMessage(cond)
  expect_true(grepl(plan_path, msg, fixed = TRUE))
  expect_true(grepl("TTEPlan at schema version 3", msg, fixed = TRUE))
  expect_true(grepl(.srf_rebuild, msg, fixed = TRUE))
})

test_that("enrolled_ids that carry the retired trial key are refused", {
  # An s1 file from an earlier release names the trial with the column that
  # this release renamed. The name is built from two parts, so that a search of
  # the source for it finds no live use.
  retired <- paste0("trial", "_id")
  d <- .srf_data()
  design <- .srf_design()
  probe <- data.table::data.table(isoyearweek = unique(d$isoyearweek))
  swereg:::.assign_period_ids(probe, period_width = 4L)
  period0 <- min(probe$period_id)
  ids <- c("I1", "C1", "C2")
  old <- data.table::data.table(
    id = ids,
    period = period0,
    intervention = c(TRUE, FALSE, FALSE),
    enrollment_person_trial_id = paste0(ids, ".", period0)
  )
  data.table::setnames(old, "period", retired)

  enroll <- function(enrolled_ids) {
    TTEEnrollment$new(
      data = data.table::copy(d),
      design = design,
      enrolled_ids = enrolled_ids,
      extra_cols = "isoyearweek"
    )
  }
  # The error text, or NA when the call does not stop. Each call is caught
  # first, so the two expectations below are judged independently.
  error_of <- function(enrolled_ids) {
    tryCatch(
      {
        enroll(enrolled_ids)
        NA_character_
      },
      error = conditionMessage
    )
  }
  refusal <- paste(
    "This release reads the trial of each person-trial from",
    "`enrollment_period_id`"
  )

  # The retired column alone, and the retired column beside the new one. The
  # second file comes from a mixed run, and no error would follow without the
  # check.
  mixed <- data.table::copy(old)
  mixed[, enrollment_period_id := period0]
  expect_match(error_of(old), refusal, fixed = TRUE)
  expect_match(error_of(mixed), refusal, fixed = TRUE)

  # The same ids under the new name enroll.
  new <- data.table::copy(old)
  data.table::setnames(new, retired, "enrollment_period_id")
  en <- enroll(new)
  expect_setequal(unique(en$data$id), ids)
  expect_false(retired %in% names(en$data))
})
