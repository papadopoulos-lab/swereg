# The rename of `matching_ratio` to `comparator_to_intervention_ratio` is
# complete across the swereg source tree.
#
# A partial rename is the failure this pins. One surviving read of the old key
# means one code path resolves a ratio under a name that asserts covariate
# matching. Nothing else reports it.
#
# Four places keep the old string, and each MUST:
#
#   1. `R/tteplan_read_spec.R`, inside `tteplan_read_spec()`. The gate detects
#      the old key, so it has to name it.
#   2. `tests/testthat/test-comparator-ratio-key.R`. That file drives the gate.
#   3. `NEWS.md`. The 26.10.0 entry names both keys so a reader can migrate.
#   4. This file, which searches for the string and therefore holds it.
#
# The assertions below pin that exact set. Anywhere else is a failure.
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

# Every tracked-source line holding the retired key, as `<relpath>:<line>`.
.crr_hits <- function(root) {
  files <- list.files(
    root,
    pattern = "\\.(R|Rmd|Rd|yaml|yml|md)$",
    recursive = TRUE,
    full.names = TRUE
  )
  files <- c(files, file.path(root, "DESCRIPTION"))
  files <- files[file.exists(files)]
  rel <- substring(files, nchar(root) + 2L)
  # `papers/` holds manuscript sources, not package sources.
  keep <- !startsWith(rel, "papers/") & !grepl(".Rcheck/", rel, fixed = TRUE)
  files <- files[keep]
  rel <- rel[keep]

  out <- character()
  for (i in seq_along(files)) {
    lines <- readLines(files[i], warn = FALSE)
    hit <- grep("matching_ratio", lines, fixed = TRUE)
    if (length(hit) > 0L) {
      out <- c(out, sprintf("%s:%d", rel[i], hit))
    }
  }
  out
}


test_that("the retired matching_ratio key survives only in the gate, its test and NEWS", {
  root <- .crr_root()
  skip_if(is.null(root), "source tree not available")

  hits <- .crr_hits(root)
  files <- sub(":[0-9]+$", "", hits)

  # The four files that MUST carry it, and nothing else.
  expect_setequal(
    unique(files),
    c(
      "R/tteplan_read_spec.R",
      "NEWS.md",
      "tests/testthat/test-comparator-ratio-key.R",
      "tests/testthat/test-comparator-ratio-rename-complete.R"
    )
  )

  # Inside `R/`, every occurrence sits in the body of `tteplan_read_spec()`,
  # which owns the file of the same name.
  src <- readLines(file.path(root, "R", "tteplan_read_spec.R"), warn = FALSE)
  fn_start <- grep("^tteplan_read_spec <- function", src)
  expect_length(fn_start, 1L)
  fn_end <- fn_start + which(src[(fn_start + 1L):length(src)] == "}")[1]
  in_r <- as.integer(sub("^.*:", "", hits[files == "R/tteplan_read_spec.R"]))
  expect_gt(length(in_r), 0L)
  expect_true(all(in_r > fn_start & in_r < fn_end))

  # In NEWS.md, every occurrence sits in the 26.10.0 entry. Find that entry by
  # name. Its position moves down the file at every later release, so a test
  # that finds it by position breaks on the next version bump.
  news <- readLines(file.path(root, "NEWS.md"), warn = FALSE)
  heads <- grep("^# swereg ", news)
  sec_start <- grep("^# swereg 26\\.10\\.0$", news)
  expect_length(sec_start, 1L)
  # The entry ends at the next release heading. When 26.10.0 is the last entry
  # in the file, it ends after the last line.
  later <- heads[heads > sec_start]
  sec_end <- if (length(later) > 0L) later[1] else length(news) + 1L
  in_news <- as.integer(sub("^.*:", "", hits[files == "NEWS.md"]))
  expect_gt(length(in_news), 0L)
  expect_true(all(in_news > sec_start & in_news < sec_end))
})


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
