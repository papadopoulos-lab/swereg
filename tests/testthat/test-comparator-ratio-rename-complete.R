# The ETT fixture uses `comparator_to_intervention_ratio`, and not the name
# swereg 26.10.0 retired. `tteplan_read_spec()` refuses the retired name as an
# unknown key (test-comparator-ratio-key.R).
#
# This test reads the SOURCE tree, which `R CMD check` does not copy. It skips
# when the tree is absent.

.crr_root <- function() {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  ok <- file.exists(file.path(root, "DESCRIPTION")) &&
    dir.exists(file.path(root, "R")) &&
    dir.exists(file.path(root, "vignettes"))
  if (!ok) {
    return(NULL)
  }
  pkg <- tryCatch(
    unname(read.dcf(file.path(root, "DESCRIPTION"), "Package")[1, 1]),
    error = function(e) NA_character_
  )
  if (!identical(pkg, "swereg")) {
    return(NULL)
  }
  root
}


test_that("the new comparator_to_intervention_ratio key reaches the ETT grid", {
  root <- .crr_root()
  skip_if(is.null(root), "source tree not available")

  fixture <- file.path(root, "tests", "testthat", "fixtures", "spec_3x2x2.yaml")
  expect_true(file.exists(fixture))
  lines <- readLines(fixture, warn = FALSE)
  expect_length(grep("matching_ratio", lines, fixed = TRUE), 0L)
  expect_length(
    grep("comparator_to_intervention_ratio", lines, fixed = TRUE),
    3L
  )
})
