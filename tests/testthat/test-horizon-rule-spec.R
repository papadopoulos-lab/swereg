# Every spec MUST declare a `horizon_rule` block with one key,
# `min_expected_events`, a number of at least 0. `tteplan_read_spec()` refuses
# a spec without the block, with an empty block, with a bad value, or with any
# other key in the block. `$reload_spec()` copies the value onto a cached plan,
# and writes the block into a plan saved before the block existed.

skip_if_not_installed("yaml")

# The fixture spec, read as a plain list. It declares
# `horizon_rule: {min_expected_events: 0}`.
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
# with no value, so the empty-block case edits the text.
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
  spec$horizon_rule$min_expected_events <- value
  return(hrs_write(spec))
}

# ---------------------------------------------------------------------------
# The reader
# ---------------------------------------------------------------------------

test_that("a spec without horizon_rule is refused, naming horizon_rule", {
  spec <- hrs_spec()
  spec$horizon_rule <- NULL
  expect_error(
    tteplan_read_spec(hrs_write(spec)),
    "Missing required sections in spec: horizon_rule",
    fixed = TRUE
  )
})

test_that("a valid horizon_rule block is accepted and stored as a double", {
  for (value in list(0L, 10L, 2.5)) {
    path <- hrs_with_value(value)
    expect_no_error(tteplan_read_spec(path))
    spec <- tteplan_read_spec(path)
    expect_identical(
      spec$horizon_rule,
      list(min_expected_events = as.numeric(value))
    )
  }
})

test_that("a negative min_expected_events is refused", {
  expect_error(
    tteplan_read_spec(hrs_with_value(-1L)),
    "horizon_rule$min_expected_events MUST be at least 0. It holds: -1.",
    fixed = TRUE
  )
})

test_that("a min_expected_events that is not one finite number is refused", {
  expect_error(
    tteplan_read_spec(hrs_with_value("10")),
    "horizon_rule$min_expected_events MUST be one number",
    fixed = TRUE
  )
  expect_error(
    tteplan_read_spec(hrs_with_value(TRUE)),
    "horizon_rule$min_expected_events MUST be one number",
    fixed = TRUE
  )
  expect_error(
    tteplan_read_spec(hrs_with_value(c(1L, 2L))),
    "horizon_rule$min_expected_events MUST be one number",
    fixed = TRUE
  )
  expect_error(
    tteplan_read_spec(hrs_write_lines(
      "  min_expected_events: 0",
      "  min_expected_events: .inf"
    )),
    "horizon_rule$min_expected_events MUST be a finite number",
    fixed = TRUE
  )
})

test_that("an empty horizon_rule block is refused", {
  path <- hrs_write_lines("  min_expected_events: 0", "")
  expect_error(
    tteplan_read_spec(path),
    "horizon_rule is present but empty",
    fixed = TRUE
  )
})

test_that("a horizon_rule that is not a mapping is refused", {
  path <- hrs_write_lines("horizon_rule:", "horizon_rule: 10")
  # The block line now holds the value, and the indented key below it is a
  # YAML error. Remove that line too.
  lines <- readLines(path)
  writeLines(lines[lines != "  min_expected_events: 0"], path)
  expect_error(
    tteplan_read_spec(path),
    "horizon_rule MUST be a mapping with the one key min_expected_events.",
    fixed = TRUE
  )
})

test_that("a horizon_rule without min_expected_events is refused", {
  path <- hrs_write_lines("  min_expected_events: 0", "  min_expected_events:")
  expect_error(
    tteplan_read_spec(path),
    "horizon_rule has no min_expected_events",
    fixed = TRUE
  )
})

test_that("a window_weeks key in horizon_rule is refused as undeclared", {
  spec <- hrs_spec()
  spec$horizon_rule$window_weeks <- 52L
  expect_error(
    tteplan_read_spec(hrs_write(spec)),
    "$/horizon_rule/window_weeks\n    Unknown key 'window_weeks'. $/horizon_rule accepts: min_expected_events.",
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

test_that("reload_spec writes horizon_rule into a plan saved without it", {
  old <- tteplan_read_spec(hrs_with_value(0L))
  # A plan saved before the block existed holds a spec without it.
  old$horizon_rule <- NULL
  plan <- hrs_plan(old)
  expect_null(plan$spec$horizon_rule)

  expect_no_warning(
    plan$reload_spec(spec_path = hrs_with_value(10L), quiet = TRUE)
  )
  expect_identical(plan$spec$horizon_rule$min_expected_events, 10)
  expect_null(plan$spec_reload_skipped_diffs)
})

test_that("reload_spec copies a changed min_expected_events", {
  plan <- hrs_plan(tteplan_read_spec(hrs_with_value(0L)))
  expect_no_warning(
    plan$reload_spec(spec_path = hrs_with_value(5L), quiet = TRUE)
  )
  expect_identical(plan$spec$horizon_rule$min_expected_events, 5)
  expect_null(plan$spec_reload_skipped_diffs)
})

test_that("a min_expected_events change is cosmetic, never structural", {
  old <- tteplan_read_spec(hrs_with_value(0L))
  new <- tteplan_read_spec(hrs_with_value(10L))
  d <- swereg:::.diff_specs(old, new)
  expect_identical(d$cosmetic, "horizon_rule$min_expected_events: 0 -> 10")
  expect_identical(d$structural, character(0))

  old$horizon_rule <- NULL
  d <- swereg:::.diff_specs(old, new)
  expect_identical(d$cosmetic, "horizon_rule$min_expected_events: NULL -> 10")
  expect_identical(d$structural, character(0))
})

# ---------------------------------------------------------------------------
# The spec summary
# ---------------------------------------------------------------------------

test_that("print_spec_summary prints min_expected_events", {
  plan <- hrs_plan(tteplan_read_spec(hrs_with_value(10L)))
  out <- paste(capture.output(plan$print_spec_summary()), collapse = "\n")
  expect_match(
    out,
    "Min expected events   10 per arm, per follow-up year",
    fixed = TRUE
  )

  plan$spec$horizon_rule <- NULL
  out <- paste(capture.output(plan$print_spec_summary()), collapse = "\n")
  expect_match(
    out,
    "Min expected events   not declared (call $reload_spec() with a spec",
    fixed = TRUE
  )
})
