# Every swereg `add_*` function writes its new columns into the caller's
# skeleton with `:=`, and its help page calls that change by reference. The
# claim holds only while the skeleton keeps one spare column slot per new
# column. Past that point data.table allocates a longer column list. A longer
# list is a new R object, so the caller keeps the old one and never sees the
# new columns. data.table 1.18.4 reports nothing.
#
# Each test below gives one function a skeleton with 3 free column slots and
# asks it for 10 columns. The measured live case was a skeleton of 1025
# columns with 11 free slots and a LISA join of 59 columns.

skip_if_not_installed("data.table")

# A data.table with exactly `spare` free column slots. Serialization drops
# data.table's over-allocation, which is what a qs2 file on disk does to a
# table, and `setalloccol()` then sets the exact number back. `setalloccol()`
# never shrinks, so it cannot produce this on its own.
.headroom <- function(dt, spare) {
  dt <- unserialize(serialize(dt, NULL))
  dt <- data.table::setalloccol(dt, spare)
  return(dt)
}

.hr_skeleton <- function(spare, ids = 1:3) {
  return(.headroom(
    swereg::create_skeleton(ids, "2021-01-01", "2021-12-31"),
    spare
  ))
}

.hr_cols <- function(n) {
  return(sprintf("v%02d", seq_len(n)))
}

# lopnr plus `n_cols` numeric columns, v01 upward. Column `vNN` holds
# NN * seq_along(ids), so every column carries a value the join can check.
.hr_wide <- function(n_cols, ids = 1:3) {
  d <- data.table::data.table(lopnr = ids)
  for (i in seq_len(n_cols)) {
    data.table::set(d, j = sprintf("v%02d", i), value = i * seq_along(ids))
  }
  return(d)
}

test_that("add_annual reaches the caller when the skeleton is out of slots", {
  sk <- .hr_skeleton(3L)
  expect_identical(data.table::truelength(sk) - ncol(sk), 3L)

  swereg::add_annual(sk, .hr_wide(10L), id_name = "lopnr", isoyear = 2021)

  expect_true(all(.hr_cols(10L) %in% names(sk)))
  got <- sk[isoyear == 2021, .(v = unique(v10)), keyby = id]
  expect_identical(got$v, c(10L, 20L, 30L))
  expect_true(all(is.na(sk[isoyear != 2021]$v10)))
})

test_that("add_annual mutates in place when the skeleton has the slots", {
  sk <- .hr_skeleton(200L)
  before <- data.table::address(sk)

  swereg::add_annual(sk, .hr_wide(10L), id_name = "lopnr", isoyear = 2021)

  expect_identical(data.table::address(sk), before)
  expect_true(all(.hr_cols(10L) %in% names(sk)))
})

test_that("add_annual shares the column vectors when it grows the skeleton", {
  # R cannot grow a list in place, so the caller's binding must hold a new
  # object after the growth. The column vectors move with it by reference,
  # which is what makes the growth cheap.
  sk <- .hr_skeleton(3L)
  before_table <- data.table::address(sk)
  before_id <- data.table::address(sk$id)

  swereg::add_annual(sk, .hr_wide(10L), id_name = "lopnr", isoyear = 2021)

  expect_true("v10" %in% names(sk))
  expect_identical(data.table::address(sk$id), before_id)
  expect_false(identical(data.table::address(sk), before_table))
})

test_that("add_annual leaves swereg's own slack after it grows", {
  # swereg reserves `.DT_ALLOC_SPARE_SLOTS` free slots on every growth, and
  # `qs2_read()` restores the same number. One growth therefore serves the
  # calls that follow it. The number is swereg's, not data.table's: data.table
  # sets `datatable.alloccol` to 1024 when it loads, and a code registry
  # writes more columns than that.
  expect_identical(swereg:::.DT_ALLOC_SPARE_SLOTS, 4096L)

  sk <- .hr_skeleton(3L)
  swereg::add_annual(sk, .hr_wide(10L), id_name = "lopnr", isoyear = 2021)

  expect_identical(data.table::truelength(sk) - ncol(sk), 4096L)
})

test_that("add_annual reaches a skeleton held in an R6 field", {
  sk <- swereg:::Skeleton$new(data = .hr_skeleton(3L), batch_number = 1L)

  swereg::add_annual(sk$data, .hr_wide(10L), id_name = "lopnr", isoyear = 2021)

  expect_true(all(.hr_cols(10L) %in% names(sk$data)))
})

test_that("add_annual warns when it cannot reach the caller's binding", {
  # `identity(sk)` is a call, so there is no binding to write the grown table
  # back to. The columns reach the return value and nothing else.
  sk <- .hr_skeleton(3L)

  expect_warning(
    got <- swereg::add_annual(
      identity(sk),
      .hr_wide(10L),
      id_name = "lopnr",
      isoyear = 2021
    ),
    "Free column slots"
  )
  expect_true("v10" %in% names(got))
  expect_false("v10" %in% names(sk))
})

test_that("the warning names an action every add_* caller can take", {
  # The advice in the warning MUST work for every `add_*` function. All eight
  # return the skeleton, so the return value is an action every caller of
  # every one of them can take. `add_diagnoses()` returned NULL until
  # 26.10.14, which left a caller of it nothing to use.
  sk <- .hr_skeleton(3L)
  dx <- data.table::data.table(
    lopnr = 1:3,
    indatum = as.Date(rep("2021-03-01", 3)),
    hdia = rep("F320", 3)
  )
  codes <- stats::setNames(rep(list("F32"), 10L), .hr_cols(10L))

  expect_warning(
    got <- swereg::add_diagnoses(
      identity(sk),
      dx,
      id_name = "lopnr",
      codes = codes
    ),
    "Use the table this call returns"
  )
  expect_true(data.table::is.data.table(got))
  expect_true(all(.hr_cols(10L) %in% names(got)))
  expect_false("v10" %in% names(sk))
})

test_that("every add_* returns a data.table carrying its new columns", {
  # Eight functions, one contract. Each is called through `identity()`, which
  # is an expression rather than a variable, so the rebind cannot reach it and
  # the return value is the only route the new columns have.
  cols <- .hr_cols(10L)
  codes_true <- stats::setNames(rep(list(TRUE), 10L), cols)
  dx <- data.table::data.table(
    lopnr = 1:3,
    indatum = as.Date(rep("2021-03-01", 3)),
    hdia = rep("F320", 3),
    op1 = rep("HAC10", 3),
    icdo10 = rep("C509", 3)
  )
  cods <- data.table::data.table(
    lopnr = 1:3,
    dodsdat = as.Date(rep("2021-03-01", 3)),
    ulorsak = rep("I219", 3)
  )
  rx <- data.table::data.table(
    lopnr = 1:3,
    edatum = as.Date(rep("2021-03-01", 3)),
    atc = rep("C10AA01", 3),
    fddd = rep(30, 3)
  )
  qual <- data.table::data.table(
    lopnr = 1:3,
    eventdate = as.Date(rep("2021-03-01", 3))
  )

  calls <- list(
    add_annual = function(x) {
      swereg::add_annual(x, .hr_wide(10L), id_name = "lopnr", isoyear = 2021)
    },
    add_onetime = function(x) swereg::add_onetime(x, .hr_wide(10L), "lopnr"),
    add_diagnoses = function(x) {
      swereg::add_diagnoses(
        x,
        dx,
        "lopnr",
        codes = stats::setNames(rep(list("F32"), 10L), cols)
      )
    },
    add_operations = function(x) {
      swereg::add_operations(
        x,
        dx,
        "lopnr",
        codes = stats::setNames(rep(list("HAC10"), 10L), cols)
      )
    },
    add_cods = function(x) {
      swereg::add_cods(
        x,
        cods,
        "lopnr",
        codes = stats::setNames(rep(list("I21"), 10L), cols)
      )
    },
    add_cancer_without_morphology = function(x) {
      swereg::add_cancer_without_morphology(
        x,
        dx,
        "lopnr",
        codes = stats::setNames(rep(list("C50"), 10L), cols)
      )
    },
    add_rx = function(x) {
      swereg::add_rx(
        x,
        rx,
        "lopnr",
        codes = stats::setNames(rep(list("C10A"), 10L), cols),
        source = "atc"
      )
    },
    add_quality_registry = function(x) {
      swereg::add_quality_registry(
        x,
        qual,
        "lopnr",
        date_col = "eventdate",
        codes = codes_true
      )
    }
  )

  for (nm in names(calls)) {
    sk <- .hr_skeleton(3L)
    got <- suppressWarnings(calls[[nm]](identity(sk)))
    expect_true(data.table::is.data.table(got), info = nm)
    expect_true(all(cols %in% names(got)), info = nm)
  }
})

test_that("add_onetime reaches the caller when the skeleton is out of slots", {
  sk <- .hr_skeleton(3L)

  swereg::add_onetime(sk, .hr_wide(10L), id_name = "lopnr")

  expect_true(all(.hr_cols(10L) %in% names(sk)))
  got <- sk[, .(v = unique(v10)), keyby = id]
  expect_identical(got$v, c(10L, 20L, 30L))
})

test_that("add_diagnoses reaches the caller when the skeleton is out of slots", {
  sk <- .hr_skeleton(3L)
  dx <- data.table::data.table(
    lopnr = 1:3,
    indatum = as.Date(rep("2021-03-01", 3)),
    hdia = c("F320", "F410", "I60")
  )
  codes <- stats::setNames(rep(list("F32"), 10L), .hr_cols(10L))

  swereg::add_diagnoses(sk, dx, id_name = "lopnr", codes = codes)

  expect_true(all(.hr_cols(10L) %in% names(sk)))
  expect_true(any(sk[id == 1L]$v10))
  expect_false(any(sk[id == 3L]$v10))
})

test_that("add_operations reaches the caller when the skeleton is out of slots", {
  sk <- .hr_skeleton(3L)
  ops <- data.table::data.table(
    lopnr = 1:3,
    indatum = as.Date(rep("2021-03-01", 3)),
    op1 = c("HAC10", "HAC20", "ZZZ99")
  )
  codes <- stats::setNames(rep(list("HAC10"), 10L), .hr_cols(10L))

  swereg::add_operations(sk, ops, id_name = "lopnr", codes = codes)

  expect_true(all(.hr_cols(10L) %in% names(sk)))
  expect_true(any(sk[id == 1L]$v10))
  expect_false(any(sk[id == 2L]$v10))
})

test_that("add_cods reaches the caller when the skeleton is out of slots", {
  sk <- .hr_skeleton(3L)
  cods <- data.table::data.table(
    lopnr = 1:3,
    dodsdat = as.Date(rep("2021-03-01", 3)),
    ulorsak = c("I219", "C509", "J189")
  )
  codes <- stats::setNames(rep(list("I21"), 10L), .hr_cols(10L))

  swereg::add_cods(sk, cods, id_name = "lopnr", codes = codes)

  expect_true(all(.hr_cols(10L) %in% names(sk)))
  expect_true(any(sk[id == 1L]$v10))
  expect_false(any(sk[id == 2L]$v10))
})

test_that("add_cancer_without_morphology reaches the caller out of slots", {
  sk <- .hr_skeleton(3L)
  ca <- data.table::data.table(
    lopnr = 1:3,
    indatum = as.Date(rep("2021-03-01", 3)),
    icdo10 = c("C509", "C541", "C569")
  )
  codes <- stats::setNames(rep(list("C50"), 10L), .hr_cols(10L))

  swereg::add_cancer_without_morphology(sk, ca, "lopnr", codes = codes)

  expect_true(all(.hr_cols(10L) %in% names(sk)))
  expect_true(any(sk[id == 1L]$v10))
  expect_false(any(sk[id == 2L]$v10))
})

test_that("add_rx reaches the caller when the skeleton is out of slots", {
  sk <- .hr_skeleton(3L)
  rx <- data.table::data.table(
    lopnr = c(1L, 2L, 3L),
    edatum = as.Date(c("2021-03-01", "2021-04-01", "2021-05-01")),
    atc = c("C10AA01", "N06AB03", "N06AB04"),
    fddd = c(30, 30, 30)
  )
  codes <- stats::setNames(rep(list("C10A"), 10L), .hr_cols(10L))

  swereg::add_rx(sk, rx, id_name = "lopnr", codes = codes, source = "atc")

  expect_true(all(.hr_cols(10L) %in% names(sk)))
  expect_true(any(sk[id == 1L]$v10))
  expect_false(any(sk[id == 2L]$v10))
})

test_that("add_quality_registry reaches the caller out of slots", {
  sk <- .hr_skeleton(3L)
  reg <- data.table::data.table(
    lopnr = 1:3,
    eventdate = as.Date(c("2021-02-01", "2021-03-15", "2021-04-22"))
  )
  codes <- stats::setNames(rep(list(TRUE), 10L), .hr_cols(10L))

  swereg::add_quality_registry(
    sk,
    reg,
    id_name = "lopnr",
    date_col = "eventdate",
    codes = codes
  )

  expect_true(all(.hr_cols(10L) %in% names(sk)))
  expect_true(any(sk[id == 1L]$v10))
})
