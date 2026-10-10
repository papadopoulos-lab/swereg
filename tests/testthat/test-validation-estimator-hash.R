# val_estimator_hash() hashes the parsed code of the estimator source files.
# These tests edit temporary copies of the files in .VAL_ESTIMATOR_FILES. An
# edit to comments, roxygen, whitespace or blank lines MUST leave the hash
# unchanged. An edit to a function body, a default argument or a top-level
# constant MUST change it.

# Copy the estimator source files into a new temporary package root.
val_hash_copy_root <- function() {
  src <- val_package_root()
  dst <- tempfile("estimator-hash-")
  dir.create(file.path(dst, "R"), recursive = TRUE)
  ok <- file.copy(
    file.path(src, .VAL_ESTIMATOR_FILES),
    file.path(dst, .VAL_ESTIMATOR_FILES)
  )
  stopifnot(all(ok))
  dst
}

# Replace the one exact occurrence of `old` with `new` in `file` under `root`.
# With `first_only = TRUE`, `old` MAY occur more than once and only the first
# occurrence changes. It stops when `old` does not occur, when it occurs more
# than once without `first_only`, or when the file bytes do not change. So an
# edit that fails to apply cannot pass as a result.
val_hash_edit <- function(root, file, old, new, first_only = FALSE) {
  path <- file.path(root, file)
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  n <- lengths(regmatches(txt, gregexpr(old, txt, fixed = TRUE)))
  if (n < 1L || (n > 1L && !first_only)) {
    stop("expected one match of '", old, "' in ", file, ", found ", n)
  }
  before <- unname(tools::md5sum(path))
  writeLines(sub(old, new, txt, fixed = TRUE), path)
  if (identical(before, unname(tools::md5sum(path)))) {
    stop("the edit did not change ", file)
  }
  invisible(path)
}

test_that("comment, roxygen, whitespace and blank-line edits leave the hash unchanged", {
  root <- val_hash_copy_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  h0 <- val_estimator_hash(root)

  # Add a comment line, a roxygen line and blank lines.
  val_hash_edit(
    root,
    "R/tte_boundaries.R",
    "# Deviation boundary\n",
    "# Deviation boundary\n# An added comment line.\n\n\n"
  )
  val_hash_edit(
    root,
    "R/tte_risk_difference.R",
    "#' @noRd\n.boot_person_index <- function(person) {",
    "#' @noRd\n#' @keywords internal\n.boot_person_index <- function(person) {"
  )
  # Change a comment, a comment inside the R6 class body, and add an inline
  # comment and extra indentation to a code line.
  val_hash_edit(
    root,
    "R/tte_risk_difference.R",
    "# The cause-specific risk difference and the numbers needed to treat",
    "# A changed comment about the risk difference"
  )
  val_hash_edit(
    root,
    "R/r6_tteenrollment.R",
    "    # Mutating methods — ordered by workflow execution sequence",
    "    # A changed comment in the R6 class body"
  )
  val_hash_edit(
    root,
    "R/tte_risk_difference.R",
    "  np <- nlevels(f)\n",
    "      np <- nlevels(f) # an inline comment\n"
  )
  # Remove a roxygen line and a blank line.
  val_hash_edit(
    root,
    "R/tte_estimation.R",
    "#' @noRd\n",
    "",
    first_only = TRUE
  )
  val_hash_edit(root, "R/tte_ipcw_helpers.R", "\n\n", "\n", first_only = TRUE)

  expect_identical(val_estimator_hash(root), h0)
})

test_that("a one-token change in a function body changes the hash", {
  root <- val_hash_copy_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  h0 <- val_estimator_hash(root)
  val_hash_edit(
    root,
    "R/tte_risk_difference.R",
    "draw <- sample.int(np, np, replace = TRUE)",
    "draw <- sample.int(np, np, replace = FALSE)"
  )
  expect_false(identical(val_estimator_hash(root), h0))
})

test_that("a changed default argument changes the hash", {
  root <- val_hash_copy_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  h0 <- val_estimator_hash(root)
  val_hash_edit(
    root,
    "R/tte_risk_difference.R",
    "  conf_level = 0.95,\n  keep_mult = FALSE,",
    "  conf_level = 0.9,\n  keep_mult = FALSE,"
  )
  expect_false(identical(val_estimator_hash(root), h0))
})

test_that("a changed top-level constant changes the hash", {
  root <- val_hash_copy_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  h0 <- val_estimator_hash(root)
  val_hash_edit(
    root,
    "R/r6_tteenrollment.R",
    ".TTE_ENROLLMENT_SCHEMA_VERSION <- 7L",
    ".TTE_ENROLLMENT_SCHEMA_VERSION <- 8L"
  )
  expect_false(identical(val_estimator_hash(root), h0))
})
