# An enrollment MAY carry `family`, a display label that groups enrollments.
#
# Three properties are pinned here:
#
#   1. `tteplan_check_spec()` reports a family that is not one non-empty
#      string, and a spec where some enrollments set a family and others do
#      not. A spec where every enrollment sets one, or none does, gives zero
#      rows. Both directions are pinned, because a checker that reports every
#      spec passes the failing tests alone.
#   2. `tteplan_read_spec()` stops on the same findings, and keeps a valid
#      family on the parsed spec.
#   3. `$reload_spec()` treats an added, changed or removed family as a label
#      change. It applies the new value and records no structural change.
#
# Each fixture is `fixtures/spec_3x2x2.yaml`, with one `family:` line added
# under each enrollment `id:` line, so a row can only come from that line.

ef_ids <- c("01", "02", "03")

# `values` holds one raw YAML value per enrollment, or `NA` for an enrollment
# that sets no family. The text is inserted as written, so a case can hold an
# unquoted number, an empty string, a sequence or a null.
ef_spec_path <- function(values, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  lines <- readLines(testthat::test_path("fixtures", "spec_3x2x2.yaml"))
  for (k in rev(seq_along(ef_ids))) {
    at <- which(lines == paste0('  - id: "', ef_ids[k], '"'))
    stopifnot(length(at) == 1L)
    if (!is.na(values[k])) {
      lines <- append(lines, paste0("    family: ", values[k]), after = at)
    }
  }
  path <- file.path(dir, "spec_v001.yaml")
  writeLines(lines, path)
  return(path)
}


test_that("check_spec gives zero rows when no enrollment sets a family", {
  r <- tteplan_check_spec(ef_spec_path(c(NA, NA, NA)))
  expect_identical(nrow(r), 0L)
})


test_that("check_spec gives zero rows when every enrollment sets a family", {
  r <- tteplan_check_spec(ef_spec_path(
    c('"Main"', '"Sens: age 54-60"', '"Period: 2008-2016"')
  ))
  expect_identical(r$kind, character(0))
  expect_identical(nrow(r), 0L)
})


test_that("check_spec reports a family that is not a string", {
  r <- tteplan_check_spec(ef_spec_path(c("3", '"Main"', '"Main"')))
  expect_identical(r$kind, "enrollment_family")
  expect_identical(r$path, "$/enrollments[]/family")
  expect_identical(
    r$message,
    paste0(
      "enrollments[1] '01' has family 3 (integer). family MUST be one ",
      "non-empty character string. Quote a value that YAML reads as a ",
      "number or a logical."
    )
  )
})


test_that("check_spec reports an empty, a blank, a sequence and a null family", {
  r <- tteplan_check_spec(ef_spec_path(c('""', '"  "', "[a, b]")))
  expect_identical(r$kind, rep("enrollment_family", 3L))
  expect_match(r$message[1], "^enrollments\\[1\\] '01' has family ''\\. ")
  expect_match(r$message[2], "^enrollments\\[2\\] '02' has family '  '\\. ")
  expect_match(
    r$message[3],
    "^enrollments\\[3\\] '03' has family a character of length 2\\. "
  )

  r <- tteplan_check_spec(ef_spec_path(c("~", '"Main"', '"Main"')))
  expect_identical(r$kind, "enrollment_family")
  expect_match(r$message, "^enrollments\\[1\\] '01' has family null\\. ")
})


test_that("check_spec reports a family that some enrollments set and others do not", {
  r <- tteplan_check_spec(ef_spec_path(c('"Main"', NA, NA)))
  expect_identical(r$kind, "enrollment_family")
  expect_identical(r$path, "$/enrollments[]/family")
  expect_identical(
    r$message,
    paste0(
      "enrollments[2] '02', enrollments[3] '03' set no family, and 1 of 3 ",
      "enrollments set one. When one enrollment sets family, every ",
      "enrollment MUST set it."
    )
  )
})


test_that("read_spec stops on a partial family and keeps a valid one", {
  path <- ef_spec_path(c('"Main"', NA, '"Route"'))
  expect_error(
    tteplan_read_spec(path),
    "enrollments[2] '02' set no family, and 2 of 3 enrollments set one.",
    fixed = TRUE
  )

  spec <- tteplan_read_spec(ef_spec_path(c('"Main"', '"Route"', '"Route"')))
  expect_identical(
    vapply(spec$enrollments, `[[`, character(1), "family"),
    c("Main", "Route", "Route")
  )
})


# --- reload ------------------------------------------------------------------

ef_reload <- function(old_values, new_values) {
  plan <- swereg::TTEPlan$new(
    project_prefix = "ef",
    skeleton_files = "ef_skeleton.qs2",
    global_max_isoyearweek = "2016-52"
  )
  plan$spec <- tteplan_read_spec(ef_spec_path(old_values))
  new_path <- ef_spec_path(new_values)
  expect_no_warning(plan$reload_spec(spec_path = new_path, quiet = TRUE))
  return(plan)
}

ef_families <- function(plan) {
  return(vapply(
    plan$spec$enrollments,
    function(e) {
      return(e[["family"]] %||% NA_character_)
    },
    character(1)
  ))
}


test_that("reload_spec applies an added family as a label change", {
  plan <- ef_reload(c(NA, NA, NA), c('"Main"', '"Route"', '"Route"'))
  expect_null(plan$spec_reload_skipped_diffs)
  expect_identical(ef_families(plan), c("Main", "Route", "Route"))
})


test_that("reload_spec applies a changed family as a label change", {
  plan <- ef_reload(
    c('"Main"', '"Route"', '"Route"'),
    c('"Main"', '"Formulation"', '"Negative control"')
  )
  expect_null(plan$spec_reload_skipped_diffs)
  expect_identical(
    ef_families(plan),
    c("Main", "Formulation", "Negative control")
  )
})


test_that("reload_spec applies a removed family as a label change", {
  plan <- ef_reload(c('"Main"', '"Route"', '"Route"'), c(NA, NA, NA))
  expect_null(plan$spec_reload_skipped_diffs)
  expect_identical(ef_families(plan), rep(NA_character_, 3L))
  expect_false(any(vapply(
    plan$spec$enrollments,
    function(e) {
      return("family" %in% names(e))
    },
    logical(1)
  )))
})


test_that("the spec diff classifies a family change as cosmetic", {
  old <- tteplan_read_spec(ef_spec_path(c(NA, NA, NA)))
  new <- tteplan_read_spec(ef_spec_path(c('"Main"', '"Route"', '"Route"')))
  d <- swereg:::.diff_specs(old, new)
  expect_identical(d$structural, character(0))
  expect_identical(
    d$cosmetic,
    c(
      "enrollments[01]$family: NULL -> Main",
      "enrollments[02]$family: NULL -> Route",
      "enrollments[03]$family: NULL -> Route"
    )
  )
})
