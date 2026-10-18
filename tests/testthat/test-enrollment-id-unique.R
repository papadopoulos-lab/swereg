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
#   4. Each id MUST be one non-empty character string or one number. A
#      sequence, a mapping, an empty or whitespace string and a missing id
#      each give one `invalid_enrollment_id` row and a `tteplan_read_spec()`
#      stop. Before, a sequence id escaped the repeat check, so two equal
#      sequences passed both functions.
#   5. YAML reads an unquoted `01` as the integer 1. Two unquoted `01` ids
#      repeat, and `"01"` with `01` are two different ids. That is a
#      decision, pinned so that a change to it is visible.
#
# yaml::yaml.load() reads the sequence `[foo]` as the string "foo", so a
# one-element sequence of one string passes as that string. Nothing after
# the parser can tell the two apart.
#
# Each fixture is `fixtures/spec_3x2x2.yaml` with its three enrollment ids
# replaced, so an error can only come from the ids.

eu_ids <- c("01", "02", "03")

eu_spec_path <- function(ids, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  return(eu_spec_path_raw(paste0('"', ids, '"'), dir = dir))
}

# `values` is the YAML text after `id: `, written unchanged, so a test can set
# an unquoted number, a sequence or a mapping.
eu_spec_path_raw <- function(values, dir = withr::local_tempdir(.local_envir = parent.frame())) {
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
  lines[at] <- paste0("  - id: ", values)
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


eu_invalid <- function(i, shown) {
  return(paste0(
    "enrollments[", i, "] has id ", shown, ". Each enrollment MUST carry ",
    "one id: one non-empty character string or one number."
  ))
}


test_that("read_spec stops on a sequence id, and check_spec reports one row", {
  path <- eu_spec_path_raw(c('"01"', "[foo, bar]", '"03"'))
  expect_error(
    tteplan_read_spec(path),
    eu_invalid(2, "a character of length 2"),
    fixed = TRUE
  )
  r <- tteplan_check_spec(path)
  expect_identical(r$kind, "invalid_enrollment_id")
  expect_identical(r$path, "$/enrollments[]/id")
  expect_identical(r$message, eu_invalid(2, "a character of length 2"))
})


test_that("two equal sequence ids are refused by the one-value rule", {
  path <- eu_spec_path_raw(c("[foo, bar]", "[foo, bar]", '"03"'))
  expect_error(
    tteplan_read_spec(path),
    paste0(
      eu_invalid(1, "a character of length 2"), "\n",
      eu_invalid(2, "a character of length 2")
    ),
    fixed = TRUE
  )
  r <- tteplan_check_spec(path)
  expect_identical(r$kind, rep("invalid_enrollment_id", 2L))
})


test_that("read_spec stops on an empty mapping id, and check_spec reports one row", {
  path <- eu_spec_path_raw(c('"01"', '"02"', "{}"))
  expect_error(
    tteplan_read_spec(path),
    eu_invalid(3, "a list of length 0"),
    fixed = TRUE
  )
  r <- tteplan_check_spec(path)
  expect_identical(r$kind, "invalid_enrollment_id")
  expect_identical(r$message, eu_invalid(3, "a list of length 0"))
})


test_that("a mapping id with a key gives a key row and an id row", {
  # The key gate refuses `$/enrollments[]/id/a` and stops `tteplan_read_spec()`
  # first. The id rule refuses the mapping value itself.
  path <- eu_spec_path_raw(c('"01"', "{a: 1}", '"03"'))
  expect_error(
    tteplan_read_spec(path),
    "$/enrollments[]/id/a",
    fixed = TRUE
  )
  r <- tteplan_check_spec(path)
  expect_identical(r$kind, c("undeclared_key", "invalid_enrollment_id"))
  expect_identical(r$message[2], eu_invalid(2, "a list of length 1"))
})


test_that("read_spec stops on an empty string id, and check_spec reports one row", {
  path <- eu_spec_path_raw(c('"01"', '""', '"03"'))
  expect_error(tteplan_read_spec(path), eu_invalid(2, "''"), fixed = TRUE)
  r <- tteplan_check_spec(path)
  expect_identical(r$kind, "invalid_enrollment_id")
  expect_identical(r$message, eu_invalid(2, "''"))
})


test_that("read_spec stops on a whitespace id, and check_spec reports one row", {
  path <- eu_spec_path_raw(c('"01"', '"02"', '"  "'))
  expect_error(tteplan_read_spec(path), eu_invalid(3, "'  '"), fixed = TRUE)
  r <- tteplan_check_spec(path)
  expect_identical(r$kind, "invalid_enrollment_id")
  expect_identical(r$message, eu_invalid(3, "'  '"))
})


test_that("read_spec stops on an id that is only a non-breaking space", {
  # Default trimws() strips ASCII whitespace only, so U+00A0 needs the
  # Unicode classes.
  path <- eu_spec_path_raw(c('"01"', '"02"', '"\\u00A0"'))
  expect_error(tteplan_read_spec(path), "enrollments[3] has id", fixed = TRUE)
  r <- tteplan_check_spec(path)
  expect_identical(r$kind, "invalid_enrollment_id")
})


test_that("a missing id gives one id row and no read_error row", {
  path <- eu_spec_path_raw(c('"01"', "~", '"03"'))
  msg <- paste0(
    "enrollments[2] is missing 'id'. Each enrollment MUST carry one id: ",
    "one non-empty character string or one number."
  )
  expect_error(tteplan_read_spec(path), msg, fixed = TRUE)
  r <- tteplan_check_spec(path)
  expect_identical(r$kind, "invalid_enrollment_id")
  expect_identical(r$message, msg)
})


test_that("read_spec and check_spec accept a number id beside quoted ids", {
  path <- eu_spec_path_raw(c('"01"', "2", '"03"'))
  spec <- tteplan_read_spec(path)
  expect_identical(spec$enrollments[[2]]$id, 2L)
  expect_identical(nrow(tteplan_check_spec(path)), 0L)
})


test_that("check_spec accepts a spec where every id is one quoted string", {
  expect_identical(
    nrow(tteplan_check_spec(eu_spec_path(c("01", "02", "03")))),
    0L
  )
})


test_that("two unquoted 01 ids repeat, because YAML reads both as 1", {
  path <- eu_spec_path_raw(c("01", "01", '"03"'))
  msg <- paste0(
    "Enrollment id '1' is used by 2 enrollments: enrollments[1], ",
    "enrollments[2]. Each enrollment MUST carry its own id."
  )
  expect_error(tteplan_read_spec(path), msg, fixed = TRUE)
  r <- tteplan_check_spec(path)
  expect_identical(r$kind, "duplicate_enrollment_id")
  expect_identical(r$message, msg)
})


test_that("a quoted \"01\" and an unquoted 01 are two different ids", {
  path <- eu_spec_path_raw(c('"01"', "01", '"03"'))
  spec <- tteplan_read_spec(path)
  expect_identical(spec$enrollments[[1]]$id, "01")
  expect_identical(spec$enrollments[[2]]$id, 1L)
  expect_identical(nrow(tteplan_check_spec(path)), 0L)
})
