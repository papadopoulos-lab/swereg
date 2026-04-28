# Pin the matching semantics of add_diagnoses(): every exclusion
# variable, outcome flag, and confounder window in a TTE study spec
# resolves to a column produced here. A pattern-matching bug silently
# misclassifies thousands.
#
# Documented syntax (from R/add_diagnoses_and_operations.R):
#   - Patterns match by `startsWith(col_vals, pattern)` (no auto-anchor,
#     no regex; bare codes are prefix-matched as-is).
#   - Negation patterns prefixed with "!" exclude (set XXX_EXCLUDE).
#     Final value is `(name) == TRUE & XXX_EXCLUDE == FALSE`.
#   - All HDIA / DIA1.. / EKOD1.. / ICD7.. / ICD9.. columns are scanned
#     when diag_type = "both"; only HDIA when diag_type = "main".

skip_if_not_installed("data.table")

.tiny_skeleton <- function(ids = c(1L, 2L, 3L)) {
  swereg::create_skeleton(
    ids = ids,
    date_min = as.Date("2020-01-06"),
    date_max = as.Date("2020-12-27")
  )
}

.tiny_dx <- function() {
  data.table::data.table(
    lopnr   = c(1L,        2L,        3L,         1L,        2L),
    indatum = as.Date(c("2020-03-02", "2020-04-06", "2020-05-04",
                        "2020-06-01", "2020-07-06")),
    hdia    = c("F320",    "F33",     "XF32",     "F640",    "I60"),
    dia1    = c(NA_character_, "F410", NA_character_, NA_character_, "F32"),
    ekod1   = c(NA_character_, NA_character_, NA_character_,
                NA_character_, NA_character_)
  )
}

test_that("add_diagnoses: bare 'F32' matches F320 (startsWith)", {
  skel <- .tiny_skeleton()
  dx <- .tiny_dx()
  swereg::add_diagnoses(skel, dx, id_name = "lopnr",
                        codes = list("dx_f32" = "F32"))
  # Person 1 had F320 in HDIA -> some week is TRUE.
  expect_true(any(skel[id == 1L, dx_f32]))
})

test_that("add_diagnoses: bare 'F32' does NOT match XF32 (anchored at start)", {
  skel <- .tiny_skeleton()
  dx <- .tiny_dx()
  swereg::add_diagnoses(skel, dx, id_name = "lopnr",
                        codes = list("dx_f32" = "F32"))
  # Person 3 had XF32 -- prefix-match on 'F32' must NOT fire.
  expect_false(any(skel[id == 3L, dx_f32]))
})

test_that("add_diagnoses: scans DIA1 (not just HDIA) under diag_type='both'", {
  skel <- .tiny_skeleton()
  dx <- .tiny_dx()
  swereg::add_diagnoses(skel, dx, id_name = "lopnr",
                        codes = list("dx_f41" = "F41"))
  # Person 2 had F410 only in DIA1 -- if HDIA-only, would miss this.
  expect_true(any(skel[id == 2L, dx_f41]))
})

test_that("add_diagnoses: diag_type='main' restricts to HDIA only", {
  skel <- .tiny_skeleton()
  dx <- .tiny_dx()
  swereg::add_diagnoses(skel, dx, id_name = "lopnr",
                        codes = list("dx_f41" = "F41"),
                        diag_type = "main")
  # F41 only appears in DIA1 (person 2) -- with diag_type='main',
  # nothing should match.
  expect_false(any(skel$dx_f41))
})

test_that("add_diagnoses: '!' prefix excludes matches", {
  skel <- .tiny_skeleton()
  dx <- .tiny_dx()
  # F64 matches person 1's F640. Adding "!F640" should exclude it.
  swereg::add_diagnoses(skel, dx, id_name = "lopnr",
                        codes = list("dx_f64_no_640" = c("F64", "!F640")))
  expect_false(any(skel$dx_f64_no_640),
               info = "!F640 should mask F640 even though F64 prefix-matches")

  # Sanity: without the exclusion, F64 alone would match.
  skel2 <- .tiny_skeleton()
  swereg::add_diagnoses(skel2, .tiny_dx(), id_name = "lopnr",
                        codes = list("dx_f64" = "F64"))
  expect_true(any(skel2$dx_f64))
})

test_that("add_diagnoses: column dtype is logical, FALSE outside hits", {
  skel <- .tiny_skeleton()
  dx <- .tiny_dx()
  swereg::add_diagnoses(skel, dx, id_name = "lopnr",
                        codes = list("dx_f32" = "F32"))
  expect_type(skel$dx_f32, "logical")
  # Person 3 never has F32 anywhere -> all FALSE.
  expect_false(any(skel[id == 3L, dx_f32]))
})

test_that("add_diagnoses: never-matching code produces all-FALSE column", {
  skel <- .tiny_skeleton()
  dx <- .tiny_dx()
  swereg::add_diagnoses(skel, dx, id_name = "lopnr",
                        codes = list("dx_zzz" = "Z9999"))
  expect_type(skel$dx_zzz, "logical")
  expect_false(any(skel$dx_zzz))
})

test_that("add_diagnoses: multiple codes produce independent columns", {
  skel <- .tiny_skeleton()
  dx <- .tiny_dx()
  swereg::add_diagnoses(
    skel, dx, id_name = "lopnr",
    codes = list("dx_f32" = "F32", "dx_f64" = "F64", "dx_i6" = "I6")
  )
  expect_true(all(c("dx_f32", "dx_f64", "dx_i6") %in% names(skel)))
  expect_true(any(skel[id == 1L, dx_f64]))  # person 1 has F640 in HDIA
  expect_true(any(skel[id == 2L, dx_i6]))   # person 2 has I60 in HDIA (row 5)
})
