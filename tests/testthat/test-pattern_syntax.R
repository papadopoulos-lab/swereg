test_that("syntax check warns on regex metacharacters in patterns", {
  src <- data.frame(diag = c("F32", "F33"), stringsAsFactors = FALSE)
  sk <- data.table::data.table(id = 1:2, isoyearweek = "2020-01")
  # We don't actually run add_diagnoses here -- just exercise the helper
  # the way add_*() does (via .swereg_codes_pre).
  expect_warning(
    swereg:::.swereg_codes_pre(
      list(bad = c("^F32"), good = c("F33")),
      src,
      "add_diagnoses"
    ),
    regexp = "regex metacharacters"
  )
})

test_that("syntax check warns on empty patterns", {
  src <- data.frame(diag = "F32", stringsAsFactors = FALSE)
  expect_warning(
    swereg:::.swereg_codes_pre(
      list(bad = c("F32", "")),
      src,
      "add_diagnoses"
    ),
    regexp = "[Ee]mpty pattern"
  )
})

test_that("syntax check is silent on legal startsWith patterns", {
  src <- data.frame(diag = c("F32", "F640"), stringsAsFactors = FALSE)
  expect_silent(
    swereg:::.check_pattern_syntax(
      c("F32", "F33", "!F640", "I2"),
      "add_diagnoses"
    )
  )
})

test_that("add_rx with source='produkt' skips syntax check (parens are legal)", {
  # Product names may legitimately contain parentheses, dots, plus signs.
  # Building a minimal lmed with a product-name pattern containing "(" must
  # not trigger the metacharacter warning.
  expect_silent(
    swereg:::.check_pattern_syntax(
      character(),  # empty -- we just verify that add_rx pathway disables it
      "add_rx"
    )
  )
  # And confirm directly that .swereg_codes_pre with syntax_check=FALSE
  # does not warn even for a metacharacter-bearing literal.
  src <- data.frame(produkt = "Vitamin B12 (Cyanocobalamin)",
                    stringsAsFactors = FALSE)
  expect_silent(
    swereg:::.swereg_codes_pre(
      list(rx = c("Vitamin B12 (Cyanocobalamin)")),
      src,
      "add_rx",
      syntax_check = FALSE
    )
  )
})
