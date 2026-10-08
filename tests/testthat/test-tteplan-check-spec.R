# `tteplan_check_spec()` reads one specification and returns every problem it
# finds as a row. It never stops on a bad specification.
#
# Each fixture under `fixtures/check_spec/<kind>/spec_v001.yaml` is
# `fixtures/spec_3x2x2.yaml` with one edit, so a row can only come from that
# edit. The `valid` fixture has no edit. It pins the passing direction: a
# checker that reports a problem on every file passes every other test here.

check_spec_fixture <- function(kind) {
  return(testthat::test_path("fixtures", "check_spec", kind, "spec_v001.yaml"))
}


test_that("a valid specification gives zero rows and the three columns", {
  path <- check_spec_fixture("valid")
  expect_true(file.exists(path))

  r <- tteplan_check_spec(path)
  expect_s3_class(r, "data.table")
  expect_identical(names(r), c("path", "kind", "message"))
  expect_identical(
    vapply(r, class, character(1), USE.NAMES = FALSE),
    rep("character", 3L)
  )
  expect_identical(nrow(r), 0L)
})


test_that("a file name with no spec_vNNN skips the version check", {
  # `spec_3x2x2.yaml` holds version v001 and its name gives no version.
  r <- tteplan_check_spec(testthat::test_path("fixtures", "spec_3x2x2.yaml"))
  expect_identical(nrow(r), 0L)
})


test_that("every undeclared key path is a row, not only the first", {
  r <- tteplan_check_spec(check_spec_fixture("undeclared_key"))

  # The fixture adds two undeclared keys at two depths. A checker that stops
  # at the first one gives one row.
  expect_identical(
    r$path,
    c(
      "$/study/implementation/bogus_implementation_key",
      "$/study/bogus_study_key"
    )
  )
  expect_identical(r$kind, rep("undeclared_key", 2L))
  expect_identical(
    r$message,
    c(
      paste0(
        "Unknown key 'bogus_implementation_key'. $/study/implementation ",
        "accepts: conf_level, date, project_prefix, status, version."
      ),
      paste0(
        "Unknown key 'bogus_study_key'. $/study accepts: description, ",
        "design, implementation, principal_investigator, title."
      )
    )
  )
})


test_that("a retired key is a row that names its replacement", {
  r <- tteplan_check_spec(check_spec_fixture("retired_key"))

  # The walk does not descend into a retired key, so its children add no row.
  expect_identical(r$path, "$/inclusion_criteria/additional_inclusion")
  expect_identical(r$kind, "retired_key")
  expect_match(r$message, "Move each entry to inclusion_criteria$criteria.", fixed = TRUE)
})


test_that("an enrollment id that two enrollments carry is a row", {
  r <- tteplan_check_spec(check_spec_fixture("duplicate_enrollment_id"))

  expect_identical(r$path, "$/enrollments[]/id")
  expect_identical(r$kind, "duplicate_enrollment_id")
  expect_identical(
    r$message,
    paste0(
      "Enrollment id '01' is used by 2 enrollments: enrollments[1], ",
      "enrollments[2]. Each enrollment MUST carry its own id."
    )
  )
})


test_that("a version that differs from the file name is a row", {
  r <- tteplan_check_spec(check_spec_fixture("version_mismatch"))

  expect_identical(r$path, "$/study/implementation/version")
  expect_identical(r$kind, "version_mismatch")
  expect_identical(
    r$message,
    paste0(
      "study$implementation$version is 'v002', and the file name ",
      "spec_v001.yaml gives 'v001'."
    )
  )
})


test_that("a missing version is a row when the file name gives one", {
  lines <- readLines(check_spec_fixture("valid"))
  version_line <- grep('^    version: "v001"$', lines)
  expect_length(version_line, 1L)
  dir <- withr::local_tempdir()
  path <- file.path(dir, "spec_v001.yaml")
  writeLines(lines[-version_line], path)

  r <- tteplan_check_spec(path)
  expect_identical(r$kind, "version_mismatch")
  expect_match(r$message, "version is missing", fixed = TRUE)
})


test_that("an error from tteplan_read_spec() after the key gate is a row", {
  path <- check_spec_fixture("read_error")
  r <- tteplan_check_spec(path)

  expected <- tryCatch(
    {
      tteplan_read_spec(path)
      NA_character_
    },
    error = conditionMessage
  )
  expect_identical(
    expected,
    "enrollments[1] 'Arm A vs B, age 50-54' is missing treatment$implementation$seed"
  )
  expect_identical(r$path, "$")
  expect_identical(r$kind, "read_error")
  expect_identical(r$message, expected)
})


test_that("a missing, non-UTF-8 or unparseable file is one unreadable row", {
  cases <- list(
    missing = file.path(withr::local_tempdir(), "spec_v001.yaml"),
    not_utf8 = check_spec_fixture("unreadable_not_utf8"),
    not_yaml = check_spec_fixture("unreadable_yaml")
  )
  expect_false(file.exists(cases$missing))
  expect_true(file.exists(cases$not_utf8))
  expect_true(file.exists(cases$not_yaml))

  messages <- c(
    missing = "Spec file not found: ",
    not_utf8 = "Spec file is not valid UTF-8",
    not_yaml = "Spec file is not valid YAML: "
  )
  for (case in names(cases)) {
    expect_no_error(r <- tteplan_check_spec(cases[[case]]))
    expect_identical(r$path, "$", label = case)
    expect_identical(r$kind, "unreadable", label = case)
    expect_true(startsWith(r$message, messages[[case]]), label = case)
  }
})


test_that("problems of three kinds in one file are three rows", {
  # Three edits to the valid fixture. The checker continues past each one.
  lines <- readLines(check_spec_fixture("valid"))
  edits <- c(
    '^  - id: "02"$' = '  - id: "01"',
    '^    version: "v001"$' = '    version: "v003"',
    '^  isoyears: \\[2010, 2020\\]$' = "  isoyears: [2010, 2020]\n  bogus: 1"
  )
  for (pattern in names(edits)) {
    hit <- grep(pattern, lines)
    expect_length(hit, 1L)
    lines[hit] <- edits[[pattern]]
  }
  dir <- withr::local_tempdir()
  path <- file.path(dir, "spec_v001.yaml")
  writeLines(lines, path)

  r <- tteplan_check_spec(path)
  expect_identical(
    r$kind,
    c("undeclared_key", "duplicate_enrollment_id", "version_mismatch")
  )
  expect_identical(
    r$path,
    c(
      "$/inclusion_criteria/bogus",
      "$/enrollments[]/id",
      "$/study/implementation/version"
    )
  )
})


test_that("a scalar where a mapping belongs gives a row and no error", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "spec_v001.yaml")
  writeLines(c("study: 3", "enrollments: 4"), path)

  expect_no_error(r <- tteplan_check_spec(path))
  expect_identical(r$kind, c("version_mismatch", "read_error"))
})
