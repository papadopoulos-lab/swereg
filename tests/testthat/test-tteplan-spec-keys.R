# The schema is the gate.
#
# `tteplan_read_spec()` refuses a key the schema does not name, at any depth.
# It accepts every specification that reads today.
#
# A collaborator wrote a cohort restriction under
# `inclusion_criteria$additional_inclusion`. swereg reads no key of that name,
# so the restriction never reached the study population, and nothing reported
# it. A key that swereg ignores looks exactly like a key that swereg reads.
#
# This file pins both directions. The refusals are the four tests after the
# first. The passing direction is the first test and the last two. A red proof
# cannot find a defect there. A gate that refuses everything passes every
# refusal test and blocks every correct specification.

# Write a specification file from the packaged fixture, with one edit applied.
# Each test below edits one key. Everything else stays valid, so a refusal can
# only come from that edit. The first test reads an unedited round trip and
# pins that.
spec_keys_write <- function(edit = identity) {
  spec <- yaml::yaml.load_file(
    testthat::test_path("fixtures", "spec_3x2x2.yaml")
  )
  path <- tempfile(fileext = ".yaml")
  writeLines(yaml::as.yaml(edit(spec)), path)
  return(path)
}


test_that("the unedited round trip reads, so a refusal below names its edit", {
  withr::local_options(swereg.warn_prevalent_user = FALSE)
  f <- spec_keys_write()
  on.exit(unlink(f), add = TRUE)

  expect_no_error(swereg::tteplan_read_spec(f))
})


test_that("an unknown key at the root is refused by name", {
  withr::local_options(swereg.warn_prevalent_user = FALSE)
  f <- spec_keys_write(function(spec) {
    spec$totally_unknown_key <- "x"
    return(spec)
  })
  on.exit(unlink(f), add = TRUE)

  expect_error(
    swereg::tteplan_read_spec(f),
    "$/totally_unknown_key",
    fixed = TRUE
  )
  # The message states the keys the root accepts, so the reader can find the
  # name they meant.
  expect_error(
    swereg::tteplan_read_spec(f),
    "$ accepts: confounders, enrollments",
    fixed = TRUE
  )
})


test_that("an unknown key inside a treatment implementation is refused", {
  withr::local_options(swereg.warn_prevalent_user = FALSE)
  f <- spec_keys_write(function(spec) {
    spec$enrollments[[1]]$treatment$implementation$bogus_depth4_key <- 1
    return(spec)
  })
  on.exit(unlink(f), add = TRUE)

  # Depth 4. A gate that checks the root alone accepts this.
  expect_error(
    swereg::tteplan_read_spec(f),
    "$/enrollments[]/treatment/implementation/bogus_depth4_key",
    fixed = TRUE
  )
  expect_error(
    swereg::tteplan_read_spec(f),
    "accepts: comparator_to_intervention_ratio",
    fixed = TRUE
  )
})


test_that("the retired matching_ratio key carries its migration message", {
  withr::local_options(swereg.warn_prevalent_user = FALSE)
  f <- spec_keys_write(function(spec) {
    spec$enrollments[[2]]$treatment$implementation$matching_ratio <- 2
    return(spec)
  })
  on.exit(unlink(f), add = TRUE)

  expect_error(
    swereg::tteplan_read_spec(f),
    "$/enrollments[]/treatment/implementation/matching_ratio",
    fixed = TRUE
  )
  # The schema carries this rule now. `tteplan_read_spec()` held a second
  # copy of it until this gate landed.
  expect_error(
    swereg::tteplan_read_spec(f),
    "Rename it to comparator_to_intervention_ratio.",
    fixed = TRUE
  )
})


test_that("inclusion_criteria$additional_inclusion is refused and names criteria", {
  withr::local_options(swereg.warn_prevalent_user = FALSE)
  f <- spec_keys_write(function(spec) {
    spec$inclusion_criteria$additional_inclusion <- list(list(
      name = "Prior event",
      type = "has_event",
      implementation = list(source_variable = "osd_x")
    ))
    return(spec)
  })
  on.exit(unlink(f), add = TRUE)

  expect_error(
    swereg::tteplan_read_spec(f),
    "$/inclusion_criteria/additional_inclusion",
    fixed = TRUE
  )
  # The repair is the container swereg does read.
  expect_error(
    swereg::tteplan_read_spec(f),
    "Move each entry to inclusion_criteria$criteria.",
    fixed = TRUE
  )
})


test_that("the packaged fixture still reads", {
  withr::local_options(swereg.warn_prevalent_user = FALSE)
  path <- testthat::test_path("fixtures", "spec_3x2x2.yaml")
  expect_true(file.exists(path))

  expect_no_error(swereg::tteplan_read_spec(path))
})


test_that("the gate reads the specification as written, not the normalised one", {
  withr::local_options(swereg.warn_prevalent_user = FALSE)
  path <- testthat::test_path("fixtures", "spec_3x2x2.yaml")
  # A refused fixture MUST report as a failure here, not as an error. An error
  # hides which assertion detected the defect.
  spec <- tryCatch(
    swereg::tteplan_read_spec(path),
    error = function(e) NULL
  )
  expect_false(is.null(spec))

  # swereg writes derived keys back into the specification. The schema names
  # none of them, so running the gate on swereg's own output refuses it. That
  # is what fixes the gate's position to before the first normaliser.
  derived <- vapply(
    as.list(spec[["exclusion_criteria"]]),
    function(ec) !is.null(ec[["implementation"]][["window_weeks"]]),
    logical(1)
  )
  expect_true(any(derived))
  expect_error(
    swereg:::.tte_spec_check_keys(spec, path),
    "$/exclusion_criteria[]/implementation/window_weeks",
    fixed = TRUE
  )
})
