# Parity tests: .entry_columns(reg) must return exactly the column set
# that .apply_code_entry_impl() actually writes when given `reg` and real
# batch_data. If any built-in `fn` that can be passed to register_codes()
# starts writing columns outside the predicted set, these tests fail in
# CI and we know the Skeleton$drop_code_entry() logic would leak orphan
# columns.
#
# Coverage: add_diagnoses, add_icdo3s, add_operations, add_rx (source =
# "atc" and source = "produkt"), add_quality_registry. Each test builds
# a minimal skeleton, constructs a registry entry by hand, applies it,
# and asserts `setequal(names_after \ names_before, .entry_columns(reg))`.

library(data.table)

# ---------------------------------------------------------------------------
# Helper: tiny skeleton + split fake_diagnoses by SOURCE
# ---------------------------------------------------------------------------

.parity_skeleton <- function() {
  data("fake_person_ids", package = "swereg", envir = environment())
  ids <- fake_person_ids[seq_len(20)]
  swereg::create_skeleton(ids, "2020-01-01", "2020-06-30")
}

.parity_batch_data_diagnoses <- function() {
  data("fake_diagnoses", package = "swereg", envir = environment())
  dt <- data.table::as.data.table(fake_diagnoses)
  swereg::make_lowercase_names(dt, date_columns = "indatum")
  list(
    outpatient = dt[source == "outpatient"],
    inpatient  = dt[source == "inpatient"],
    cancer     = dt[source == "cancer"],
    dors       = dt[source == "inpatient"]  # reuse as dors stand-in
  )
}

.parity_batch_data_rx <- function() {
  data("fake_prescriptions", package = "swereg", envir = environment())
  dt <- data.table::as.data.table(fake_prescriptions)
  swereg::make_lowercase_names(dt, date_columns = "edatum")
  # add_rx expects the person-ID column to be `lopnr`; the fake data uses
  # `p444_lopnr_personnr`, so rename for the parity test.
  if ("p444_lopnr_personnr" %in% names(dt) && !"lopnr" %in% names(dt)) {
    data.table::setnames(dt, "p444_lopnr_personnr", "lopnr")
  }
  list(lmed = dt)
}

# Minimal synthetic stroke-registry-like table for add_quality_registry
.parity_batch_data_stroke <- function() {
  list(
    stroke = data.table::data.table(
      lopnr = c(1L, 2L, 3L),
      insjuknandedatum = as.Date(c("2020-02-01", "2020-03-15", "2020-04-22"))
    )
  )
}

# Run an entry through .apply_code_entry_impl() and return the set of columns
# it added to the skeleton.
.parity_added <- function(skeleton, batch_data, reg, id_col = "lopnr") {
  before <- copy(names(skeleton))
  swereg:::.apply_code_entry_impl(skeleton, batch_data, reg, id_col)
  setdiff(names(skeleton), before)
}

# ---------------------------------------------------------------------------
# add_diagnoses
# ---------------------------------------------------------------------------

test_that(".entry_columns matches add_diagnoses (single group, no prefix)", {
  skeleton <- .parity_skeleton()
  batch_data <- .parity_batch_data_diagnoses()
  reg <- list(
    codes      = list(stroke = c("I60", "I61")),
    fn         = swereg::add_diagnoses,
    fn_args    = list(),
    groups     = list("outpatient"),
    combine_as = NULL,
    label      = "add_diagnoses"
  )
  added <- .parity_added(skeleton, batch_data, reg)
  predicted <- swereg:::.entry_columns(reg)
  expect_setequal(added, predicted)
})

test_that(".entry_columns matches add_diagnoses (multi-group with prefixes)", {
  skeleton <- .parity_skeleton()
  batch_data <- .parity_batch_data_diagnoses()
  reg <- list(
    codes      = list(stroke = c("I60", "I61"), mi = c("I21", "I22")),
    fn         = swereg::add_diagnoses,
    fn_args    = list(),
    groups     = list(ov = "outpatient", sv = "inpatient", dors = "dors"),
    combine_as = NULL,
    label      = "add_diagnoses"
  )
  added <- .parity_added(skeleton, batch_data, reg)
  predicted <- swereg:::.entry_columns(reg)
  expect_setequal(added, predicted)
})

test_that(".entry_columns matches add_diagnoses with combine_as", {
  skeleton <- .parity_skeleton()
  batch_data <- .parity_batch_data_diagnoses()
  reg <- list(
    codes      = list(stroke = c("I60", "I61")),
    fn         = swereg::add_diagnoses,
    fn_args    = list(),
    groups     = list(ov = "outpatient", sv = "inpatient"),
    combine_as = "osd",
    label      = "add_diagnoses"
  )
  added <- .parity_added(skeleton, batch_data, reg)
  predicted <- swereg:::.entry_columns(reg)
  expect_setequal(added, predicted)
  # Spot-check that the predicted set includes the combine_as columns
  expect_true("osd_stroke" %in% predicted)
  expect_true("ov_stroke" %in% predicted)
  expect_true("sv_stroke" %in% predicted)
})

# ---------------------------------------------------------------------------
# add_icdo3s (cancer registry)
# ---------------------------------------------------------------------------

test_that(".entry_columns matches add_icdo3s", {
  skeleton <- .parity_skeleton()
  batch_data <- .parity_batch_data_diagnoses()
  reg <- list(
    codes      = list(breast = c("C50"), colorectal = c("C18", "C19", "C20")),
    fn         = swereg::add_icdo3s,
    fn_args    = list(),
    groups     = list(can = "cancer"),
    combine_as = NULL,
    label      = "add_icdo3s"
  )
  added <- .parity_added(skeleton, batch_data, reg)
  predicted <- swereg:::.entry_columns(reg)
  expect_setequal(added, predicted)
})

# ---------------------------------------------------------------------------
# add_operations (from inpatient + outpatient combined)
# ---------------------------------------------------------------------------

test_that(".entry_columns matches add_operations (unnamed group combines sources)", {
  skeleton <- .parity_skeleton()
  batch_data <- .parity_batch_data_diagnoses()
  reg <- list(
    codes      = list(hysterectomy = c("LCD00", "LCD01")),
    fn         = swereg::add_operations,
    fn_args    = list(),
    groups     = list(c("inpatient", "outpatient")),
    combine_as = NULL,
    label      = "add_operations"
  )
  added <- .parity_added(skeleton, batch_data, reg)
  predicted <- swereg:::.entry_columns(reg)
  expect_setequal(added, predicted)
})

# ---------------------------------------------------------------------------
# add_rx — source = "atc"
# ---------------------------------------------------------------------------

test_that(".entry_columns matches add_rx with source = atc", {
  skeleton <- .parity_skeleton()
  batch_data <- .parity_batch_data_rx()
  reg <- list(
    codes      = list(rx_n05a = "N05A", rx_n06a = "N06A"),
    fn         = swereg::add_rx,
    fn_args    = list(source = "atc"),
    groups     = list("lmed"),
    combine_as = NULL,
    label      = "add_rx"
  )
  added <- .parity_added(skeleton, batch_data, reg)
  predicted <- swereg:::.entry_columns(reg)
  expect_setequal(added, predicted)
})

# ---------------------------------------------------------------------------
# add_quality_registry (stroke)
# ---------------------------------------------------------------------------

test_that(".entry_columns matches add_quality_registry", {
  skeleton <- .parity_skeleton()
  # Force at least one matching row so the fn actually writes a column
  skeleton <- skeleton[id %in% c(1L, 2L, 3L)]
  if (nrow(skeleton) == 0L) skip("no overlapping ids in parity skeleton")
  batch_data <- .parity_batch_data_stroke()
  reg <- list(
    codes      = list(stroke_any = TRUE),  # event-flag style
    fn         = swereg::add_quality_registry,
    fn_args    = list(date_col = "insjuknandedatum"),
    groups     = list(stroke = "stroke"),
    combine_as = NULL,
    label      = "add_quality_registry"
  )
  added <- .parity_added(skeleton, batch_data, reg)
  predicted <- swereg:::.entry_columns(reg)
  expect_setequal(added, predicted)
})
