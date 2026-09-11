# Every TTEEnrollment serialised by swereg 26.10.18 or earlier is REFUSED at
# load, and the refusal names the remedy.
#
# 26.10.18 still wrote schema 3, so the break is not the 26.10.17 field
# addition. `.TTE_ENROLLMENT_SCHEMA_VERSION` is 4L from 26.10.19, and
# `$check_version()` stops on any lower number. The message must therefore
# say that an earlier swereg wrote the object, and must send the reader to
# s1. It must not attribute the refusal to the 26.9.0 landmark move, which is
# a different break.
#
# The reachability witness is `qs2_read()`: it is the function every swereg
# reader goes through, and it calls `$check_version()` on each R6 object it
# deserialises.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")

# The panel `TTEEnrollment$new()` needs, at the smallest size that builds one.
.esr_enrollment <- function() {
  weeks <- cstime::dates_by_isoyearweek$isoyearweek
  idx <- which(weeks >= "2020-01")[1]
  wk <- weeks[idx:(idx + 7L)]
  ids <- c(paste0("I", 1:3), paste0("C", 1:6))
  d <- data.table::rbindlist(lapply(ids, function(nm) {
    data.table::data.table(
      id = nm,
      isoyearweek = wk,
      exposed = startsWith(nm, "I"),
      eligible = TRUE,
      died = FALSE,
      age = 50
    )
  }))
  design <- TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = 4L,
    period_width = 4L
  )
  TTEEnrollment$new(
    data = d,
    design = design,
    ratio = 2,
    seed = 4,
    extra_cols = "isoyearweek"
  )
}

# Force one object down to the schema an older release left it on.
.esr_downgrade <- function(obj, version) {
  assign(
    ".schema_version",
    as.integer(version),
    envir = obj$.__enclos_env__$private
  )
  obj
}


test_that("the enrollment schema version is above what 26.10.18 wrote", {
  # 3L is what 26.10.18 and every earlier release wrote. The constant MUST be
  # higher than that, so every one of those objects is refused.
  expect_gt(swereg:::.TTE_ENROLLMENT_SCHEMA_VERSION, 3L)
})


test_that("a schema-3 TTEEnrollment is refused at load", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "enrollment_v3.qs2")
  qs2::qs_save(.esr_downgrade(.esr_enrollment(), 3L), path)

  expect_error(qs2_read(path), "schema version 3")
  expect_error(qs2_read(path), "requires version 4")

  # A warning is not enough. The caller must get no object at all, because an
  # object that loads is an object whose `fill_summary` reads NULL.
  cond <- tryCatch(qs2_read(path), condition = function(e) e)
  expect_s3_class(cond, "error")

  # The remedy MUST be in the message. An operator who reads only this line
  # has to know which stage rebuilds the object.
  msg <- conditionMessage(cond)
  expect_match(msg, "[Rr]e-run s1")

  # The refusal MUST NOT be attributed to the 26.9.0 landmark move. That
  # release is not the boundary this version gate enforces.
  expect_false(grepl("26.9.0", msg, fixed = TRUE))
})


test_that("a current-schema TTEEnrollment still loads", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "enrollment_v4.qs2")
  qs2::qs_save(.esr_enrollment(), path)

  got <- qs2_read(path)
  expect_s3_class(got, "TTEEnrollment")
  expect_true(got$check_version())
})
