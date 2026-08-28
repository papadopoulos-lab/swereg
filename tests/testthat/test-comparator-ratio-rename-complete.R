# The rename of `matching_ratio` to `comparator_to_intervention_ratio` is
# complete across the swereg source tree.
#
# A partial rename is the failure this pins. One surviving read of the old key
# means one code path resolves a ratio under a name that asserts covariate
# matching. Nothing else reports it.
#
# Seven places keep the old string, and each MUST:
#
#   1. `R/tteplan_spec_schema.R`. The schema is the canonical home of the
#      refusal. It declares the key `legacy` at
#      `$/enrollments[]/treatment/implementation/matching_ratio`, and it
#      carries the migration message.
#   2. `R/tteplan_read_spec.R`, inside `tteplan_read_spec()`, in a comment
#      only. That function held a second copy of the rule until the key gate
#      landed. The comment points at the schema.
#   3. `tests/testthat/test-tteplan-spec-keys.R`. That file drives the gate
#      end to end, through `tteplan_read_spec()`.
#   4. `tests/testthat/test-tteplan-spec-schema.R`. That file pins how the
#      schema classifies the key.
#   5. `tests/testthat/test-comparator-ratio-key.R`. That file drives the
#      refusal from a hand-written specification.
#   6. `NEWS.md`. The 26.10.0 entry names both keys so a reader can migrate.
#   7. This file, which searches for the string and therefore holds it.
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
#
# The pattern refuses a following underscore or alphanumeric, so
# `matching_ratio_default` does not match. That is a different key at a
# different path, and `R/tteplan_spec_schema.R` accepts it. A `fixed = TRUE`
# search cannot tell the two apart. It then pulls every file that names the
# accepted key into the set below, which is a set of files that name the
# RETIRED key.
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
    hit <- grep("matching_ratio(?![_[:alnum:]])", lines, perl = TRUE)
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

  # The seven files that MUST carry it, and nothing else.
  expect_setequal(
    unique(files),
    c(
      "R/tteplan_spec_schema.R",
      "R/tteplan_read_spec.R",
      "NEWS.md",
      "tests/testthat/test-tteplan-spec-keys.R",
      "tests/testthat/test-tteplan-spec-schema.R",
      "tests/testthat/test-comparator-ratio-key.R",
      "tests/testthat/test-comparator-ratio-rename-complete.R"
    )
  )

  # In `R/tteplan_read_spec.R` every occurrence is a comment, and every one
  # sits in the body of `tteplan_read_spec()`. A line of code there would be a
  # second spelling of a rule the schema already carries.
  src <- readLines(file.path(root, "R", "tteplan_read_spec.R"), warn = FALSE)
  fn_start <- grep("^tteplan_read_spec <- function", src)
  expect_length(fn_start, 1L)
  fn_end <- fn_start + which(src[(fn_start + 1L):length(src)] == "}")[1]
  in_r <- as.integer(sub("^.*:", "", hits[files == "R/tteplan_read_spec.R"]))
  expect_gt(length(in_r), 0L)
  expect_true(all(in_r > fn_start & in_r < fn_end))
  expect_true(all(startsWith(trimws(src[in_r]), "#")))

  # In `R/tteplan_spec_schema.R` exactly one line outside a comment names the
  # retired key. That line declares it `legacy`, which is the rule itself.
  # `matching_ratio_default` is a different key at a different path, and the
  # schema accepts it, so it is excluded from the count.
  sch <- readLines(file.path(root, "R", "tteplan_spec_schema.R"), warn = FALSE)
  in_s <- as.integer(sub("^.*:", "", hits[files == "R/tteplan_spec_schema.R"]))
  expect_gt(length(in_s), 0L)
  rule <- trimws(sch[in_s])
  rule <- rule[!startsWith(rule, "#")]
  rule <- rule[!grepl("matching_ratio_default", rule, fixed = TRUE)]
  expect_length(rule, 1L)

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
