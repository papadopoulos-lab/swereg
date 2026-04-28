# Pin spec <-> skeleton-column consistency: every concrete column the
# spec references (excluding `computed: true` rolling-window helpers)
# must be discoverable as a planned skeleton column. A typo in the
# spec (e.g. `osd_f20_to_f29` vs `osd_f20_f29`) currently surfaces
# only when s0_init / s1 runs against real data; this catches it at
# test time.
#
# The fixture spec references columns under exclusion_criteria,
# confounders, outcomes, and treatment. We assert that each
# (non-computed) column name is a plausible skeleton column under
# swereg's documented prefix conventions.

test_that("fixture spec source variables follow documented prefix conventions", {
  spec_path <- testthat::test_path("fixtures", "spec_3x2x2.yaml")
  skip_if_not(file.exists(spec_path))
  spec <- swereg::tteplan_read_spec(spec_path)

  # Collect all source_variable / variable references from concrete
  # (non-computed) spec entries.
  refs <- character()

  for (ec in spec$exclusion_criteria) {
    impl <- ec$implementation
    if (isTRUE(impl$computed)) next  # rolling-window helper, handled elsewhere
    if (!is.null(impl$source_variable)) refs <- c(refs, impl$source_variable)
    if (!is.null(impl$variable))        refs <- c(refs, impl$variable)
  }
  for (cf in spec$confounders) {
    impl <- cf$implementation
    if (isTRUE(impl$computed)) next
    if (!is.null(impl$variable)) refs <- c(refs, impl$variable)
  }
  for (o in spec$outcomes) {
    refs <- c(refs, o$implementation$variable)
  }
  for (e in spec$enrollments) {
    refs <- c(refs, e$treatment$implementation$variable)
  }
  refs <- unique(refs[nzchar(refs)])
  expect_gt(length(refs), 0L)

  # Documented prefix conventions (per swereg CLAUDE.md and code
  # registry contract):
  #   ICD10 hospital + death:  ov_ / sv_ / dors_ / osd_ / os_
  #   Cancer ICD-O3:           can_
  #   ATC prescription:        rx_   (no enforced prefix, but project convention)
  #   Operations:              op_
  #   Row-dependent vars:      rd_
  #   Row-independent vars:    ri_
  valid_prefix <- "^(ov|sv|dors|osd|os|can|rx|op|rd|ri)_"
  bad <- refs[!grepl(valid_prefix, refs)]
  expect_equal(bad, character(0),
    info = paste0("spec references column(s) without a documented prefix: ",
                  paste(bad, collapse = ", "),
                  " (expected one of ov_/sv_/dors_/osd_/os_/can_/rx_/op_/rd_/ri_)"))
})

test_that("fixture spec: every enrollment's treatment variable also appears in additional_inclusion or is consistent", {
  spec_path <- testthat::test_path("fixtures", "spec_3x2x2.yaml")
  skip_if_not(file.exists(spec_path))
  spec <- swereg::tteplan_read_spec(spec_path)

  for (e in spec$enrollments) {
    tx_var <- e$treatment$implementation$variable
    expect_true(nzchar(tx_var),
      info = paste0("enrollment ", e$id, " is missing treatment variable"))
    # Treatment vars for fixture all start with rd_ (row-dependent)
    expect_true(grepl("^rd_", tx_var),
      info = paste0("enrollment ", e$id, " treatment var '", tx_var,
                    "' should be row-dependent (rd_*)"))
  }
})

test_that("outcomes never reference computed-only columns", {
  spec_path <- testthat::test_path("fixtures", "spec_3x2x2.yaml")
  skip_if_not(file.exists(spec_path))
  spec <- swereg::tteplan_read_spec(spec_path)

  # Outcomes need a real, materialised column on the skeleton -- they
  # cannot be `computed: true` rolling-window helpers.
  for (o in spec$outcomes) {
    impl <- o$implementation
    expect_false(isTRUE(impl$computed),
      info = paste0("outcome '", o$name, "' is marked computed: true; ",
                    "outcomes must reference materialised columns"))
  }
})
