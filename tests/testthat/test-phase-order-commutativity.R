# Do the two skeleton phase orders agree?
#
# `RegistryStudy$process_skeletons()` runs the randvars phase before the code
# registry phase. `.process_one_batch()` in R/r6_registrystudy.R calls
# `sk$sync_randvars()` first and `sk$sync_with_registry()` second. This file
# measures what a swap of the two does to the skeleton content.
#
# The answer depends on the randvar. `add_rx()` reads the skeleton's earliest
# weekly row to decide which prescriptions belong on the annual spine. See
# `min_isoyearweek` in R/add_rx.R. A randvar that deletes the earliest weekly
# rows therefore changes what `add_rx()` writes, and it reads no randvar column
# to do it.
#
# Case 1 pins the agreeing case: a step that only adds a column.
# Case 2 pins a step that deletes the earliest weekly rows. The two orders
# disagreed while create_skeleton() built an annual row and weekly rows of the
# same ISO year: order A remapped a prescription onto that annual row. Since
# 26.12.0 no ISO year holds both, so the two orders agree.
# Case 3 pins this plan's central claim: move the same deletion into the trim,
# ahead of both phases, and the two orders agree.
#
# Every case drives the real `add_rx()` through `Skeleton$sync_with_registry()`.
# None uses a stub code function, because the whole effect lives inside
# `add_rx()`.
#
# Case 2 calls its deleting function DIRECTLY on the data.table, below the sync
# API. `Skeleton$sync_randvars()` now stops any step that changes the row
# count, so the historical mechanism can no longer be shown through it. The
# deletion itself is what case 2 measures, and a direct call reproduces it
# exactly.

skip_if_not_installed("data.table")

library(data.table)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# One base skeleton, built once and copied per run. The weekly spine starts at
# "2015-01" and the annual spine ends at "2014-**". No ISO year holds both, so
# a 2015 prescription has no annual row to be remapped onto.
utils::data("fake_person_ids", package = "swereg", envir = environment())
utils::data("fake_prescriptions", package = "swereg", envir = environment())

.poc_base <- swereg::create_skeleton(
  ids = fake_person_ids,
  isoyear_min = 2015,
  isoyearweek_max = "2016-52"
)

# `add_rx()` needs lowercase `atc`, `edatum` and `fddd`. `make_lowercase_names()`
# renames by reference, so it runs on a copy and leaves the shipped data alone.
.poc_lmed <- data.table::copy(fake_prescriptions)
swereg::make_lowercase_names(.poc_lmed, date_columns = "edatum")

.poc_id_col <- "p444_lopnr_personnr"

# The registry entry shape that `$register_codes()` builds. The `rx` group
# prefix makes `add_rx()` write one column named `rx_antidep`.
.poc_entry <- list(
  codes = list(antidep = "N06A"),
  fn = swereg::add_rx,
  fn_args = list(source = "atc"),
  groups = list(rx = "prescriptions"),
  combine_as = NULL,
  label = "add_rx"
)

.poc_batch_data <- function() list(prescriptions = .poc_lmed)

.poc_sync_codes <- function(sk) {
  sk$sync_with_registry(
    current_fps = "fp_add_rx",
    registry = list(.poc_entry),
    batch_data_loader = .poc_batch_data,
    id_col = .poc_id_col
  )
}

.poc_sync_randvars <- function(sk, fn) {
  sk$sync_randvars(
    randvars_fns = list(step = fn),
    randvars_hashes = c(step = "hash_step"),
    batch_data_loader = .poc_batch_data,
    config = NULL
  )
}

# Order A is today's order: randvars, then the code registry. `base` is the
# table the two phases start from. Case 3 passes a base the trim already cut.
.poc_order_a <- function(fn, base = .poc_base) {
  sk <- Skeleton$new(data = data.table::copy(base), batch_number = 1L)
  .poc_sync_randvars(sk, fn)
  .poc_sync_codes(sk)
  sk$data
}

# Order B is the swap: the code registry, then randvars.
.poc_order_b <- function(fn, base = .poc_base) {
  sk <- Skeleton$new(data = data.table::copy(base), batch_number = 1L)
  .poc_sync_codes(sk)
  .poc_sync_randvars(sk, fn)
  sk$data
}

# The same two orders with the step applied directly to the data.table, below
# `Skeleton$sync_randvars()`. Used by case 2, whose step deletes rows.
.poc_apply_direct <- function(sk, fn) {
  sk$data <- fn(sk$data, .poc_batch_data(), NULL)
  invisible(sk)
}

.poc_order_a_direct <- function(fn) {
  sk <- Skeleton$new(data = data.table::copy(.poc_base), batch_number = 1L)
  .poc_apply_direct(sk, fn)
  .poc_sync_codes(sk)
  sk$data
}

.poc_order_b_direct <- function(fn) {
  sk <- Skeleton$new(data = data.table::copy(.poc_base), batch_number = 1L)
  .poc_sync_codes(sk)
  .poc_apply_direct(sk, fn)
  sk$data
}

# The deletion, as a standalone function. Case 2 calls it directly. Case 3
# registers the same predicate as a trim.
#
# ISO year 2015 holds every weekly row before "2016-01". The predicate
# therefore deletes the earliest weekly rows. The minimum weekly row moves
# from "2015-01" to "2016-01". The annual rows survive.
.poc_delete_2015_weekly <- function(skeleton, batch_data, config) {
  skeleton[!(is_isoyear == FALSE & isoyear == 2015L)]
}

# ---------------------------------------------------------------------------
# Case 1 -- an additive randvar
# ---------------------------------------------------------------------------

test_that("phase order is content-preserving for an additive randvar", {
  additive_fn <- function(skeleton, batch_data, config) {
    skeleton[, rv_flag := TRUE]
    invisible(skeleton)
  }

  a <- .poc_order_a(additive_fn)
  b <- .poc_order_b(additive_fn)

  # Only the column order differs, because each phase appends its own columns.
  expect_identical(
    names(a),
    c("id", "isoyear", "isoyearweek", "is_isoyear", "isoyearweeksun",
      "personyears", "rv_flag", "rx_antidep")
  )
  expect_identical(
    names(b),
    c("id", "isoyear", "isoyearweek", "is_isoyear", "isoyearweeksun",
      "personyears", "rx_antidep", "rv_flag")
  )

  data.table::setcolorder(b, names(a))
  expect_identical(a, b)
})

# ---------------------------------------------------------------------------
# Case 2 -- a step that deletes the earliest weekly rows
# ---------------------------------------------------------------------------

test_that("phase order leaves add_rx() output unchanged when the step deletes a whole ISO year of weekly rows", {
  a <- .poc_order_a_direct(.poc_delete_2015_weekly)
  b <- .poc_order_b_direct(.poc_delete_2015_weekly)
  data.table::setcolorder(b, names(a))

  # Both orders delete the same rows, in the same order. Every comparison below
  # is therefore row by row on a shared row sequence.
  expect_identical(nrow(a), 167000L)
  expect_identical(nrow(b), 167000L)
  expect_identical(a$id, b$id)
  expect_identical(a$isoyearweek, b$isoyearweek)
  expect_identical(min(a[is_isoyear == FALSE]$isoyearweek), "2016-01")
  expect_identical(min(.poc_base[is_isoyear == FALSE]$isoyearweek), "2015-01")

  for (col in c("id", "isoyear", "isoyearweek", "is_isoyear",
                "isoyearweeksun", "personyears")) {
    expect_identical(a[[col]], b[[col]])
  }

  # THE TWO ORDERS AGREE. Order A finds no 2015 annual row to remap a 2015
  # prescription onto, and order B deletes the weekly rows it marked.
  expect_false("2015-**" %in% a$isoyearweek)
  expect_identical(a$rx_antidep, b$rx_antidep)

  # Totals, so a change in either direction is caught.
  expect_identical(sum(a$rx_antidep), 1083L)
  expect_identical(sum(b$rx_antidep), 1083L)
  expect_identical(sum(a$rx_antidep[a$isoyearweek == "2014-**"]), 71L)
})

# ---------------------------------------------------------------------------
# Case 3 -- the same deletion, moved into the trim
# ---------------------------------------------------------------------------

# `.process_one_batch()` runs the registered trim on the fresh base, after the
# framework and before both later phases. See R/r6_registrystudy.R. So the
# table both orders start from is the trimmed one, and neither phase deletes
# anything after it.
.poc_trimmed_base <- .poc_delete_2015_weekly(
  data.table::copy(.poc_base),
  NULL,
  NULL
)

test_that("phase order agrees again when the trim deletes the earliest weekly rows", {
  additive_fn <- function(skeleton, batch_data, config) {
    skeleton[, rv_flag := TRUE]
    invisible(skeleton)
  }

  a <- .poc_order_a(additive_fn, base = .poc_trimmed_base)
  b <- .poc_order_b(additive_fn, base = .poc_trimmed_base)
  data.table::setcolorder(b, names(a))

  # The trim deleted the same rows case 2 deletes, so the agreement below is
  # not vacuous.
  expect_identical(nrow(a), 167000L)
  expect_identical(min(a[is_isoyear == FALSE]$isoyearweek), "2016-01")

  # THE TWO ORDERS AGREE, column for column. That is this plan's claim.
  expect_identical(a, b)

  # Both match order A of case 2. The code registry reads the trimmed rows
  # whichever phase runs first, because the deletion already happened.
  expect_identical(sum(a$rx_antidep), 1083L)
  expect_identical(sum(b$rx_antidep), 1083L)
  expect_identical(sum(a$rx_antidep[a$isoyearweek == "2014-**"]), 71L)
  expect_identical(sum(b$rx_antidep[b$isoyearweek == "2014-**"]), 71L)
})
