test_that("swereg:::expand_codes() handles bracket ranges", {
  expect_equal(
    swereg:::expand_codes("I2[0-5]"),
    c("I20", "I21", "I22", "I23", "I24", "I25")
  )
})

test_that("swereg:::expand_codes() handles enumerations", {
  expect_equal(
    swereg:::expand_codes("H03[ABC]"),
    c("H03A", "H03B", "H03C")
  )
  expect_equal(
    swereg:::expand_codes("E14[01689]"),
    c("E140", "E141", "E146", "E148", "E149")
  )
})

test_that("swereg:::expand_codes() handles mixed range + literal", {
  expect_equal(
    swereg:::expand_codes("E14[2-57]"),
    c("E142", "E143", "E144", "E145", "E147")
  )
})

test_that("swereg:::expand_codes() handles multiple bracket groups (Cartesian)", {
  out <- swereg:::expand_codes("FN[AB][0-9]")
  expect_length(out, 20L)
  expect_equal(head(out, 3L), c("FNA0", "FNA1", "FNA2"))
  expect_equal(tail(out, 1L), "FNB9")
})

test_that("swereg:::expand_codes() preserves leading '!' on every literal", {
  out <- swereg:::expand_codes("!302[A-Z]")
  expect_length(out, 26L)
  expect_true(all(startsWith(out, "!302")))
  expect_equal(out[1], "!302A")
  expect_equal(out[26], "!302Z")
})

test_that("swereg:::expand_codes() passes through patterns with no brackets", {
  expect_equal(swereg:::expand_codes("302,31"), "302,31")
  expect_equal(swereg:::expand_codes(c("F640", "!F640")), c("F640", "!F640"))
})

test_that("swereg:::expand_codes() handles empty input", {
  expect_equal(swereg:::expand_codes(character()), character())
})

test_that("swereg:::expand_code_list() preserves names and applies expansion per entry", {
  out <- swereg:::expand_code_list(list(
    a = c("I2[0-5]"),
    b = c("F640", "F648")
  ))
  expect_named(out, c("a", "b"))
  expect_length(out$a, 6L)
  expect_equal(out$b, c("F640", "F648"))
})

test_that("swereg:::warn_unmatched_codes() warns on literals absent from source", {
  src <- data.table::data.table(
    code = c("I20", "I21", "F640")
  )
  cl <- list(
    diag_mi   = c("I20", "I21"),     # both match
    diag_typo = c("I20", "QQ99")     # QQ99 absent
  )
  expect_warning(
    swereg:::warn_unmatched_codes(src, cl, "test"),
    "QQ99"
  )
})

test_that("swereg:::warn_unmatched_codes() is silent when every literal matches", {
  src <- data.table::data.table(code = c("I20", "I21"))
  cl  <- list(diag_mi = c("I20", "I21"))
  expect_silent(swereg:::warn_unmatched_codes(src, cl, "test"))
})

test_that("swereg:::warn_unmatched_codes() also checks '!'-prefixed exclusions", {
  src <- data.table::data.table(code = c("F32", "F33"))
  # exclusion typo: !F999 doesn't exist, should warn
  cl  <- list(diag_depression = c("F32", "F33", "!F999"))
  expect_warning(
    swereg:::warn_unmatched_codes(src, cl, "test"),
    "!F999"
  )
})

test_that("swereg:::warn_unmatched_codes() handles empty / no-char-col inputs", {
  empty_src <- data.table::data.table(x = integer(0))
  expect_silent(swereg:::warn_unmatched_codes(empty_src, list(a = "I20"), "t"))

  no_chars <- data.table::data.table(x = 1:3)
  expect_silent(swereg:::warn_unmatched_codes(no_chars, list(a = "I20"), "t"))
})

test_that("swereg:::warn_empty_logical_cols() warns on missing columns", {
  skel <- data.table::data.table(diag_a = c(TRUE, FALSE))
  expect_warning(
    swereg:::warn_empty_logical_cols(skel, list(diag_a = "x", diag_missing = "y"), "test"),
    "diag_missing"
  )
})

test_that("swereg:::warn_empty_logical_cols() warns on all-FALSE columns", {
  skel <- data.table::data.table(
    diag_ok    = c(TRUE, FALSE),
    diag_empty = c(FALSE, FALSE)
  )
  expect_warning(
    swereg:::warn_empty_logical_cols(skel, list(diag_ok = "x", diag_empty = "y"), "test"),
    "diag_empty"
  )
})

test_that("swereg:::warn_empty_logical_cols() is silent on healthy skeleton", {
  skel <- data.table::data.table(diag_a = c(TRUE, FALSE))
  expect_silent(swereg:::warn_empty_logical_cols(skel, list(diag_a = "x"), "test"))
})
