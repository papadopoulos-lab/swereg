# The migration gate: an object saved under an older schema is REFUSED.
#
# swereg 26.9.0 moved time zero to the landmark. A `tstart == 0` row of a
# schema-2 object is an entry band row. A 26.9.0 reader takes the same row for
# a landmark row, so a silent load changes what every row of that object means.
#
# `TTEDesign`, `TTEEnrollment` and `TTEPlan` therefore each stop in
# `check_version()`. `TTEPlan` already did. The other two warned and continued,
# and a warning is exactly the failure mode this file exists to close: the
# caller gets an object, the numbers move, and nothing states why.
#
# The reachability witness is `qs2_read()`. It is the function every swereg
# reader goes through, and it calls `check_version()` on any R6 object it
# deserialises.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")

# Force one object down to schema 2, exactly as an older release left it.
.lmg_downgrade <- function(obj, version = 2L) {
  assign(
    ".schema_version",
    as.integer(version),
    envir = obj$.__enclos_env__$private
  )
  obj
}

.lmg_save <- function(obj, dir, name) {
  path <- file.path(dir, name)
  qs2::qs_save(obj, path)
  path
}

.lmg_design <- function() {
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

# A person-week table large enough for `TTEEnrollment$new()` to build a panel.
.lmg_data <- function() {
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

.lmg_enrollment <- function() {
  TTEEnrollment$new(
    data = .lmg_data(),
    design = .lmg_design(),
    ratio = 2,
    seed = 4,
    extra_cols = "isoyearweek"
  )
}


# ---------------------------------------------------------------------------
# PROOF -- a version-2 object is refused, not reinterpreted
# ---------------------------------------------------------------------------

test_that("a version-2 object is refused, not reinterpreted", {
  dir <- withr::local_tempdir()

  # The three schema constants moved to 3 together, so 2 is the last release
  # that could have written any of them.
  expect_identical(swereg:::.TTE_DESIGN_SCHEMA_VERSION, 3L)
  expect_identical(swereg:::.TTE_ENROLLMENT_SCHEMA_VERSION, 3L)
  expect_identical(swereg:::.TTE_PLAN_SCHEMA_VERSION, 3L)

  # --- TTEDesign ---
  design_path <- .lmg_save(
    .lmg_downgrade(.lmg_design()),
    dir,
    "design_v2.qs2"
  )
  expect_error(qs2_read(design_path), "schema version 2")
  expect_error(qs2_read(design_path), "landmark")

  # --- TTEEnrollment ---
  enrollment_path <- .lmg_save(
    .lmg_downgrade(.lmg_enrollment()),
    dir,
    "enrollment_v2.qs2"
  )
  expect_error(qs2_read(enrollment_path), "schema version 2")
  expect_error(qs2_read(enrollment_path), "landmark")

  # --- TTEPlan ---
  plan <- TTEPlan$new(
    project_prefix = "lmg",
    skeleton_files = file.path(dir, "skel_a.qs2"),
    global_max_isoyearweek = "2020-08",
    ett = data.table::data.table(ett_id = character(0))
  )
  plan_path <- .lmg_save(.lmg_downgrade(plan), dir, "plan_v2.qs2")
  expect_error(qs2_read(plan_path), "schema version 2")

  # A warning is NOT enough. The old path warned and returned the object, so
  # the caller kept running on rows whose meaning had changed underneath.
  # Each read MUST raise a condition of class "error".
  for (p in c(design_path, enrollment_path, plan_path)) {
    cond <- tryCatch(qs2_read(p), condition = function(e) e)
    expect_s3_class(cond, "error")
  }
})

test_that("a current-schema object still loads", {
  dir <- withr::local_tempdir()
  design_path <- .lmg_save(.lmg_design(), dir, "design_v3.qs2")
  got <- qs2_read(design_path)
  expect_s3_class(got, "TTEDesign")
  expect_true(got$check_version())
})

test_that("the release version and the newest NEWS section agree", {
  # `read.dcf()` reads the file, so this cannot pass on a string literal that
  # someone updated in the test instead of in DESCRIPTION.
  #
  # `R CMD check` runs the tests from a built tarball. That tarball has no
  # DESCRIPTION two levels up, so `skip_if_not()` fires and CI reports green.
  # This block is a source-tree check, never a CI gate. A stale assertion here
  # stays red on a developer machine and invisible on CI.
  desc <- testthat::test_path("..", "..", "DESCRIPTION")
  news <- testthat::test_path("..", "..", "NEWS.md")
  skip_if_not(file.exists(desc) && file.exists(news), "source tree only")

  version <- as.character(read.dcf(desc)[1, "Version"])
  lines <- readLines(news, warn = FALSE)
  heads <- grep("^# swereg ", lines)
  newest <- sub("^# swereg ", "", lines[heads[1]])

  # Assert that the two agree. Name neither. A version literal here is green
  # for one release and red at every bump after it.
  expect_identical(version, newest)
  expect_true(package_version(version) > package_version("26.8.21"))

  # The landmark notes MUST sit under 26.9.0, the release that moved time zero.
  # They MUST NOT sit under 26.8.21, which does not contain them. Both releases
  # are published, so both section bodies are frozen history. Neither literal
  # goes stale at a bump, unlike a current-version literal.
  span <- function(v) {
    i <- match(paste0("# swereg ", v), lines)
    j <- heads[heads > i][1]
    lines[i:(if (is.na(j)) length(lines) else j - 1L)]
  }
  landmark <- "Time zero is the landmark"
  expect_true(any(grepl(landmark, span("26.9.0"), fixed = TRUE)))
  expect_false(any(grepl(landmark, span("26.8.21"), fixed = TRUE)))
})
