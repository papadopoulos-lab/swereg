# Pin spec-shape invariants on a fixture (3 enrollments x 2 outcomes
# x 2 follow_ups). Drift in `tteplan_read_spec()` -- e.g. silently
# dropping an enrollment, or losing the arm labels -- is currently
# undetectable until s0_init runs against real data.

test_that("tteplan_read_spec: parses fixture into expected counts and arm labels", {
  spec_path <- testthat::test_path("fixtures", "spec_3x2x2.yaml")
  skip_if_not(file.exists(spec_path), "fixture YAML missing")

  spec <- swereg::tteplan_read_spec(spec_path)

  # Top-level sections present
  expect_true(all(c("study", "enrollments", "outcomes", "follow_up",
                    "inclusion_criteria", "exclusion_criteria",
                    "confounders") %in% names(spec)))

  # Counts (used downstream to materialise the ETT grid: 3 x 2 x 2 = 12)
  expect_equal(length(spec$enrollments), 3L)
  expect_equal(length(spec$outcomes),    2L)
  expect_equal(length(spec$follow_up),   2L)

  # Inclusion isoyears range survives parse
  expect_equal(spec$inclusion_criteria$isoyears, c(2010L, 2020L))

  # Each enrollment carries arm labels (intervention + comparator)
  for (i in seq_along(spec$enrollments)) {
    arms <- spec$enrollments[[i]]$treatment$arms
    expect_false(is.null(arms),
      info = paste0("enrollment ", i, " missing treatment$arms"))
    expect_true(nzchar(arms$intervention),
      info = paste0("enrollment ", i, " has empty intervention label"))
    expect_true(nzchar(arms$comparator),
      info = paste0("enrollment ", i, " has empty comparator label"))
  }

  # Project prefix flows from study$implementation
  expect_equal(spec$study$implementation$project_prefix, "fixture-3x2x2")
})

test_that("tteplan_read_spec: window_weeks normalisation works for the fixture", {
  spec_path <- testthat::test_path("fixtures", "spec_3x2x2.yaml")
  skip_if_not(file.exists(spec_path))
  spec <- swereg::tteplan_read_spec(spec_path)
  expect_identical(
    spec$exclusion_criteria[[1]]$implementation$window_weeks,
    Inf
  )
})

test_that("tteplan_read_spec: enrollment ids are unique", {
  spec_path <- testthat::test_path("fixtures", "spec_3x2x2.yaml")
  skip_if_not(file.exists(spec_path))
  spec <- swereg::tteplan_read_spec(spec_path)
  ids <- vapply(spec$enrollments, `[[`, character(1), "id")
  expect_equal(length(unique(ids)), length(ids),
               info = "enrollment ids must be unique")
})
