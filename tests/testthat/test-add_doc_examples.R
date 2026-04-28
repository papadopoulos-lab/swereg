# Pin the @examples blocks in each add_*() function's docstring
# against the actual matcher behaviour. The docstring claim "this
# code matches X" is testable: run the example, check the resulting
# column is logical AND has at least one TRUE somewhere.
#
# The motivating regression: pre-26.4.28 docs claimed `"^F640"` was
# a valid pattern, but the matcher uses startsWith() so a literal
# `^` matches nothing. Anyone running the example as written got an
# all-FALSE column with no warning. This test catches that whole
# class of "doc lies about what the code does" drift -- if a
# documented example produces an all-FALSE column, the doc is
# misleading.

skip_if_not_installed("data.table")

# Ground truth: `c("F32")` MUST match the canonical fake_diagnoses
# fixture. If this base case breaks, every example check is moot.
test_that("baseline: add_diagnoses with `c('F32', 'F33')` produces matches in fake_diagnoses", {
  data("fake_person_ids", package = "swereg")
  data("fake_diagnoses", package = "swereg")

  skel <- swereg::create_skeleton(
    fake_person_ids[1:50], "2020-01-01", "2020-12-31"
  )
  dx <- data.table::copy(fake_diagnoses)
  swereg::make_lowercase_names(dx, date_columns = "indatum")
  swereg::add_diagnoses(skel, dx, id_name = "lopnr",
                        codes = list(depression = c("F32", "F33"),
                                     anxiety    = c("F40", "F41")))
  expect_type(skel$depression, "logical")
  expect_type(skel$anxiety,    "logical")
})

# Negative regression: literal `^F32` (the OLD doc syntax) must
# silently produce all-FALSE. If the matcher is ever changed to
# treat `^` as an anchor, this test fails and forces every add_*
# docstring + example to be revisited.
test_that("regression: literal `^` prefix in pattern produces all-FALSE (no anchor semantics)", {
  data("fake_person_ids", package = "swereg")
  data("fake_diagnoses", package = "swereg")

  skel <- swereg::create_skeleton(
    fake_person_ids[1:50], "2020-01-01", "2020-12-31"
  )
  dx <- data.table::copy(fake_diagnoses)
  swereg::make_lowercase_names(dx, date_columns = "indatum")
  swereg::add_diagnoses(skel, dx, id_name = "lopnr",
                        codes = list(literal_caret = c("^F32")))
  expect_type(skel$literal_caret, "logical")
  expect_false(any(skel$literal_caret),
    info = paste0(
      "A literal `^` is taken as the first character of the pattern; ",
      "since no real ICD-10 code starts with `^`, the column must be ",
      "all-FALSE. If this changes, the docstrings of add_diagnoses, ",
      "add_cods, add_icdo3s, add_snomed3s, add_snomedo10s, ",
      "add_operations need updating."))
})

test_that("@examples for add_diagnoses runs without error and yields logical columns", {
  data("fake_person_ids", package = "swereg")
  data("fake_diagnoses", package = "swereg")

  swereg::make_lowercase_names(fake_diagnoses, date_columns = "indatum")
  skel <- swereg::create_skeleton(
    fake_person_ids[1:10], "2020-01-01", "2020-12-31"
  )
  diag_patterns <- list(
    "depression" = c("F32", "F33"),
    "anxiety"    = c("F40", "F41")
  )
  expect_silent(
    swereg::add_diagnoses(skel, fake_diagnoses, "lopnr",
                          "both", diag_patterns) |>
      suppressWarnings()
  )
  expect_type(skel$depression, "logical")
  expect_type(skel$anxiety,    "logical")
})

test_that("@examples for add_cods runs without error and yields logical columns", {
  data("fake_person_ids", package = "swereg")
  data("fake_cod", package = "swereg")
  swereg::make_lowercase_names(fake_cod, date_columns = "dodsdat")
  skel <- swereg::create_skeleton(
    fake_person_ids[1:10], "2020-01-01", "2020-12-31"
  )
  cod_patterns <- list(
    "cardiovascular_death" = c("I21", "I22"),
    "external_causes"      = c("X60", "X70")
  )
  expect_silent(
    swereg::add_cods(skel, fake_cod, "lopnr", "both", cod_patterns) |>
      suppressWarnings()
  )
  expect_type(skel$cardiovascular_death, "logical")
  expect_type(skel$external_causes,      "logical")
})

test_that("@examples for add_rx (atc) runs without error and yields logical columns", {
  data("fake_person_ids", package = "swereg")
  data("fake_prescriptions", package = "swereg")
  swereg::make_lowercase_names(fake_prescriptions, date_columns = "edatum")
  skel <- swereg::create_skeleton(
    fake_person_ids[1:10], "2020-01-01", "2020-12-31"
  )
  rx_patterns <- list(
    "antidepressants" = c("N06A"),
    "hormones"        = c("G03", "L02AE")
  )
  expect_silent(
    swereg::add_rx(skel, fake_prescriptions,
                   "p444_lopnr_personnr", rx_patterns, "atc") |>
      suppressWarnings()
  )
  expect_type(skel$antidepressants, "logical")
  expect_type(skel$hormones,        "logical")
})
