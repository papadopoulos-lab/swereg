# Every spec MUST declare the flat key `min_expected_events_per_arm_per_year`,
# a number of at least 0. `tteplan_read_spec()` refuses a spec without the key,
# with no value, with a bad value, or with a mapping under the key.
# `$reload_spec()` copies the value onto a cached plan, and writes it into a
# plan saved before the rule existed.

skip_if_not_installed("yaml")

# The fixture spec, read as a plain list. It declares
# `min_expected_events_per_arm_per_year: 0`.
hrs_spec <- function() {
  return(yaml::read_yaml(testthat::test_path("fixtures", "spec_3x2x2.yaml")))
}

hrs_write <- function(spec) {
  dir <- tempfile("hrs_spec_")
  dir.create(dir)
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(spec, path)
  return(path)
}

# The spec text with one line replaced. yaml::write_yaml() cannot write a key
# with no value, so the empty-value case edits the text.
hrs_write_lines <- function(from, to) {
  lines <- readLines(testthat::test_path("fixtures", "spec_3x2x2.yaml"))
  i <- which(lines == from)
  stopifnot(length(i) == 1L)
  lines[i] <- to
  dir <- tempfile("hrs_spec_")
  dir.create(dir)
  path <- file.path(dir, "spec_v001.yaml")
  writeLines(lines, path)
  return(path)
}

hrs_with_value <- function(value) {
  spec <- hrs_spec()
  spec$min_expected_events_per_arm_per_year <- value
  return(hrs_write(spec))
}

# ---------------------------------------------------------------------------
# The reader
# ---------------------------------------------------------------------------

test_that("a spec without the minimum-events key is refused, naming it", {
  spec <- hrs_spec()
  spec$min_expected_events_per_arm_per_year <- NULL
  expect_error(
    tteplan_read_spec(hrs_write(spec)),
    "Missing required sections in spec: min_expected_events_per_arm_per_year",
    fixed = TRUE
  )
})

test_that("a valid minimum-events value is accepted and stored as a double", {
  for (value in list(0L, 10L, 2.5)) {
    path <- hrs_with_value(value)
    expect_no_error(tteplan_read_spec(path))
    spec <- tteplan_read_spec(path)
    expect_identical(
      spec$min_expected_events_per_arm_per_year,
      as.numeric(value)
    )
  }
})

test_that("a negative minimum-events value is refused", {
  expect_error(
    tteplan_read_spec(hrs_with_value(-1L)),
    "min_expected_events_per_arm_per_year MUST be at least 0. It holds: -1.",
    fixed = TRUE
  )
})

test_that("a minimum-events value that is not one finite number is refused", {
  expect_error(
    tteplan_read_spec(hrs_with_value("10")),
    "min_expected_events_per_arm_per_year MUST be one number",
    fixed = TRUE
  )
  expect_error(
    tteplan_read_spec(hrs_with_value(TRUE)),
    "min_expected_events_per_arm_per_year MUST be one number",
    fixed = TRUE
  )
  expect_error(
    tteplan_read_spec(hrs_with_value(c(1L, 2L))),
    "min_expected_events_per_arm_per_year MUST be one number",
    fixed = TRUE
  )
  expect_error(
    tteplan_read_spec(hrs_write_lines(
      "min_expected_events_per_arm_per_year: 0",
      "min_expected_events_per_arm_per_year: .inf"
    )),
    "min_expected_events_per_arm_per_year MUST be a finite number",
    fixed = TRUE
  )
})

test_that("a minimum-events key with no value is refused", {
  for (to in c(
    "min_expected_events_per_arm_per_year:",
    "min_expected_events_per_arm_per_year: {}"
  )) {
    path <- hrs_write_lines("min_expected_events_per_arm_per_year: 0", to)
    expect_error(
      tteplan_read_spec(path),
      "min_expected_events_per_arm_per_year is present but has no value",
      fixed = TRUE
    )
  }
})

test_that("a mapping under the minimum-events key is refused as undeclared", {
  spec <- hrs_spec()
  spec$min_expected_events_per_arm_per_year <- list(window_weeks = 52L)
  expect_error(
    tteplan_read_spec(hrs_write(spec)),
    paste0(
      "$/min_expected_events_per_arm_per_year/window_weeks\n    Unknown key ",
      "'window_weeks'. The schema declares no key under ",
      "$/min_expected_events_per_arm_per_year, so that value MUST NOT be a ",
      "mapping."
    ),
    fixed = TRUE
  )
})

# ---------------------------------------------------------------------------
# The reload
# ---------------------------------------------------------------------------

# A plan that holds `spec`. `$reload_spec()` reads `plan$spec` and writes it
# back, which is the path under test.
hrs_plan <- function(spec) {
  plan <- swereg::TTEPlan$new(
    project_prefix = "fixture-3x2x2",
    skeleton_files = "hrs_skeleton.qs2",
    global_max_isoyearweek = "2020-52"
  )
  plan$spec <- spec
  return(plan)
}

test_that("reload_spec writes the minimum-events value into a plan saved without it", {
  old <- tteplan_read_spec(hrs_with_value(0L))
  # A plan saved before the rule existed holds a spec without it.
  old$min_expected_events_per_arm_per_year <- NULL
  plan <- hrs_plan(old)
  expect_null(plan$spec$min_expected_events_per_arm_per_year)

  expect_no_warning(
    plan$reload_spec(spec_path = hrs_with_value(10L), quiet = TRUE)
  )
  expect_identical(plan$spec$min_expected_events_per_arm_per_year, 10)
  expect_null(plan$spec_reload_skipped_diffs)
})

test_that("reload_spec copies a changed minimum-events value", {
  plan <- hrs_plan(tteplan_read_spec(hrs_with_value(0L)))
  expect_no_warning(
    plan$reload_spec(spec_path = hrs_with_value(5L), quiet = TRUE)
  )
  expect_identical(plan$spec$min_expected_events_per_arm_per_year, 5)
  expect_null(plan$spec_reload_skipped_diffs)
})

test_that("a minimum-events change is cosmetic, never structural", {
  old <- tteplan_read_spec(hrs_with_value(0L))
  new <- tteplan_read_spec(hrs_with_value(10L))
  d <- swereg:::.diff_specs(old, new)
  expect_identical(d$cosmetic, "min_expected_events_per_arm_per_year: 0 -> 10")
  expect_identical(d$structural, character(0))

  old$min_expected_events_per_arm_per_year <- NULL
  d <- swereg:::.diff_specs(old, new)
  expect_identical(
    d$cosmetic,
    "min_expected_events_per_arm_per_year: NULL -> 10"
  )
  expect_identical(d$structural, character(0))
})

# ---------------------------------------------------------------------------
# The spec summary
# ---------------------------------------------------------------------------

test_that("print_spec_summary prints the minimum-events rule", {
  plan <- hrs_plan(tteplan_read_spec(hrs_with_value(10L)))
  out <- paste(capture.output(plan$print_spec_summary()), collapse = "\n")
  expect_match(out, "Minimum-events rule:", fixed = TRUE)
  expect_match(
    out,
    "Min expected events   10 per arm, per follow-up year",
    fixed = TRUE
  )

  plan$spec$min_expected_events_per_arm_per_year <- NULL
  out <- paste(capture.output(plan$print_spec_summary()), collapse = "\n")
  expect_match(
    out,
    "Min expected events   not declared (call $reload_spec() with a spec",
    fixed = TRUE
  )
})
