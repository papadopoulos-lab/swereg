# Each enrollment MUST carry its own id (issue #58).
#
# `$reload_spec()` compares enrollments by id, and the Summary sheet looks a
# family up by id. Both take the first match, so a repeated id hides every
# later enrollment. `tteplan_check_spec()` reported a repeated id, and
# `tteplan_read_spec()` accepted it.
#
# Three properties are pinned here:
#
#   1. `tteplan_read_spec()` stops on a repeated id. The message names each
#      repeated id and the positions of the enrollments that carry it.
#   2. `tteplan_read_spec()` accepts a spec where every id is unique. A gate
#      that stops on every spec passes the failing tests alone.
#   3. Every entry point that reads a spec file refuses a repeated id:
#      `tteplan_from_spec_and_registrystudy()`, `tteplan_excel_spec_summary()`
#      and `$reload_spec()`.
#
# Each fixture is `fixtures/spec_3x2x2.yaml` with its three enrollment ids
# replaced, so an error can only come from the ids.

eu_ids <- c("01", "02", "03")

eu_spec_path <- function(ids, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  lines <- readLines(testthat::test_path("fixtures", "spec_3x2x2.yaml"))
  at <- vapply(
    eu_ids,
    function(id) {
      hit <- which(lines == paste0('  - id: "', id, '"'))
      stopifnot(length(hit) == 1L)
      return(hit)
    },
    integer(1)
  )
  lines[at] <- paste0('  - id: "', ids, '"')
  path <- file.path(dir, "spec_v001.yaml")
  writeLines(lines, path)
  return(path)
}


test_that("read_spec stops on an enrollment id that two enrollments carry", {
  expect_error(
    tteplan_read_spec(eu_spec_path(c("01", "01", "03"))),
    paste0(
      "Enrollment id '01' is used by 2 enrollments: enrollments[1], ",
      "enrollments[2]. Each enrollment MUST carry its own id."
    ),
    fixed = TRUE
  )
})


test_that("read_spec names the positions of a repeated id that are not adjacent", {
  expect_error(
    tteplan_read_spec(eu_spec_path(c("02", "01", "02"))),
    paste0(
      "Enrollment id '02' is used by 2 enrollments: enrollments[1], ",
      "enrollments[3]. Each enrollment MUST carry its own id."
    ),
    fixed = TRUE
  )
})


test_that("read_spec accepts a spec where every enrollment id is unique", {
  spec <- tteplan_read_spec(eu_spec_path(c("01", "02", "03")))
  expect_identical(
    vapply(spec$enrollments, `[[`, character(1), "id"),
    c("01", "02", "03")
  )
})


test_that("check_spec reports a repeated id once, with no read_error row", {
  r <- tteplan_check_spec(eu_spec_path(c("01", "01", "03")))
  expect_identical(r$kind, "duplicate_enrollment_id")
})


test_that("from_spec_and_registrystudy refuses a repeated enrollment id", {
  dir <- withr::local_tempdir()
  eu_spec_path(c("03", "02", "03"), dir = dir)
  expect_error(
    tteplan_from_spec_and_registrystudy(
      study = list(skeleton_files = "skeleton.qs2"),
      candidate_dir_spec = dir,
      candidate_dir_tteplan = dir,
      candidate_dir_results = dir,
      spec_version = "v001"
    ),
    "Enrollment id '03' is used by 2 enrollments: enrollments[1], enrollments[3].",
    fixed = TRUE
  )
})


test_that("excel_spec_summary refuses a repeated enrollment id", {
  out <- file.path(withr::local_tempdir(), "spec_summary.xlsx")
  expect_error(
    tteplan_excel_spec_summary(eu_spec_path(c("01", "02", "02")), out),
    "Enrollment id '02' is used by 2 enrollments: enrollments[2], enrollments[3].",
    fixed = TRUE
  )
  expect_false(file.exists(out))
})


test_that("reload_spec refuses a repeated enrollment id and keeps the old spec", {
  plan <- swereg::TTEPlan$new(
    project_prefix = "eu",
    skeleton_files = "eu_skeleton.qs2",
    global_max_isoyearweek = "2016-52"
  )
  plan$spec <- tteplan_read_spec(eu_spec_path(c("01", "02", "03")))
  before <- plan$spec

  expect_error(
    plan$reload_spec(
      spec_path = eu_spec_path(c("01", "02", "01")),
      quiet = TRUE
    ),
    "Enrollment id '01' is used by 2 enrollments: enrollments[1], enrollments[3].",
    fixed = TRUE
  )
  expect_identical(plan$spec, before)
})
