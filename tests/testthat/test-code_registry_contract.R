# Pin the code-registry column-shape contract documented in
# CLAUDE.md:
#
#   | Code type           | Applied to            | Column naming                       |
#   |---------------------|-----------------------|-------------------------------------|
#   | icd10_codes         | ov, sv, dors          | <prefix>_<name> per group            |
#   | icd10_codes + combine_as = "osd" | + osd  | osd_<name> additionally              |
#   | icdo3_codes         | cancer (group "can")  | can_<name>                           |
#   | rx_atc_codes        | lmed                  | <name> (no enforced prefix)          |
#   | operation_codes     | sv + ov combined      | <name> (no enforced prefix)          |
#
# `.entry_columns(reg)` is the single source of truth for "which
# columns will this registry entry write?" -- it's used by
# Skeleton$drop_code_entry() to know what to drop. If
# .entry_columns() drifts from what apply_codes_to_skeleton()
# actually writes, drops leak orphan columns. test-entry_columns_parity.R
# already covers that drift; this test pins the *naming* contract.

skip_if_not_installed("data.table")

test_that(".entry_columns: ICD-10 in ov+sv+dors generates 3 columns per code", {
  reg <- list(
    codes      = list(stroke = c("I60", "I61"), mi = c("I21")),
    fn         = swereg::add_diagnoses,
    fn_args    = list(),
    groups     = list(ov = "outpatient", sv = "inpatient", dors = "dors"),
    combine_as = NULL,
    label      = "add_diagnoses"
  )
  cols <- swereg:::.entry_columns(reg)
  expect_setequal(cols, c("ov_stroke", "sv_stroke", "dors_stroke",
                          "ov_mi",     "sv_mi",     "dors_mi"))
})

test_that(".entry_columns: combine_as adds an extra <combined>_<name> column", {
  reg <- list(
    codes      = list(stroke = c("I60", "I61")),
    fn         = swereg::add_diagnoses,
    fn_args    = list(),
    groups     = list(ov = "outpatient", sv = "inpatient", dors = "dors"),
    combine_as = "osd",
    label      = "add_diagnoses"
  )
  cols <- swereg:::.entry_columns(reg)
  expect_true("osd_stroke" %in% cols)
  # All four group columns should be present.
  expect_setequal(cols, c("ov_stroke", "sv_stroke", "dors_stroke",
                          "osd_stroke"))
})

test_that(".entry_columns: ICD-O3 in cancer group with prefix `can` produces a single can_<name>", {
  reg <- list(
    codes      = list(breast = c("C50"), colorectal = c("C18", "C19")),
    fn         = swereg::add_icdo3s,
    fn_args    = list(),
    groups     = list(can = "cancer"),
    combine_as = NULL,
    label      = "add_icdo3s"
  )
  cols <- swereg:::.entry_columns(reg)
  expect_setequal(cols, c("can_breast", "can_colorectal"))
})

test_that(".entry_columns: ATC in single lmed group produces <name> per code (no prefix)", {
  reg <- list(
    codes      = list(rx_n05a = "N05A", rx_n06a = "N06A"),
    fn         = swereg::add_rx,
    fn_args    = list(source = "atc"),
    groups     = list("lmed"),  # unnamed group -> no prefix
    combine_as = NULL,
    label      = "add_rx"
  )
  cols <- swereg:::.entry_columns(reg)
  expect_setequal(cols, c("rx_n05a", "rx_n06a"))
})

test_that(".entry_columns: operations from combined inpatient+outpatient sources produce <name>", {
  # An unnamed group containing a vector of source registries combines
  # them into one logical scan -> single column per code, no prefix.
  reg <- list(
    codes      = list(hysterectomy = c("LCD00", "LCD01")),
    fn         = swereg::add_operations,
    fn_args    = list(),
    groups     = list(c("inpatient", "outpatient")),
    combine_as = NULL,
    label      = "add_operations"
  )
  cols <- swereg:::.entry_columns(reg)
  expect_setequal(cols, "hysterectomy")
})

test_that("derived registry entries: <as>_<code_name> column per code", {
  reg <- list(
    kind  = "derived",
    codes = list(f20 = c("F20"), vte = c("I26", "I80")),
    from  = c("os", "dorsu", "dorsm"),
    as    = "osd",
    label = "derived"
  )
  cols <- swereg:::.entry_columns(reg)
  expect_setequal(cols, c("osd_f20", "osd_vte"))
})

test_that("RegistryStudy$register_codes records entries in code_registry", {
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = tempfile("rawbatch_"),
    group_names = c("inpatient", "outpatient", "dors", "lmed", "cancer", "other")
  )
  study$register_codes(
    codes      = list(stroke = c("I60", "I61")),
    fn         = swereg::add_diagnoses,
    groups     = list(ov = "outpatient", sv = "inpatient", dors = "dors"),
    combine_as = "osd"
  )
  expect_length(study$code_registry, 1L)
  expect_equal(study$code_registry[[1]]$combine_as, "osd")
  expect_setequal(
    swereg:::.entry_columns(study$code_registry[[1]]),
    c("ov_stroke", "sv_stroke", "dors_stroke", "osd_stroke")
  )
})
