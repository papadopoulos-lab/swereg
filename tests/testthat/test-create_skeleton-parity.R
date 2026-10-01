# Tests added to validate PR #3 (sort-once-replicate refactor of
# create_skeleton). The strategy is to embed the original implementation
# below as a reference oracle and assert identical() for a battery of
# inputs, plus structural / edge-case checks.

# Reference implementation copied from main @ 48b0e25 (pre-PR).
.create_skeleton_reference <- function(ids, date_min, date_max) {
  personyears <- isoyear <- isoyearweek <- is_isoyear <- isoyearweeksun <- id <- NULL

  skeleton_isoyear <- expand.grid(
    id = ids,
    isoyear = 1900:cstime::date_to_isoyear_n(as.Date(date_min) - 1),
    stringsAsFactors = FALSE
  ) |> data.table::setDT()
  skeleton_isoyear[, isoyearweek := paste0(isoyear, "-**")]
  skeleton_isoyear[, is_isoyear := TRUE]

  isoyearweeks <- seq.Date(as.Date(date_min), as.Date(date_max), 1) |>
    cstime::date_to_isoyearweek_c() |>
    unique()

  skeleton_isoyearweek <- expand.grid(
    id = ids,
    isoyearweek = isoyearweeks,
    stringsAsFactors = FALSE
  ) |> data.table::setDT()
  skeleton_isoyearweek[, isoyear := cstime::isoyearweek_to_isoyear_n(isoyearweek)]
  skeleton_isoyearweek[, is_isoyear := FALSE]

  skeleton <- data.table::rbindlist(
    list(skeleton_isoyear, skeleton_isoyearweek), use.names = TRUE
  )

  skeleton[is_isoyear == FALSE, isoyearweeksun := cstime::isoyearweek_to_last_date(isoyearweek)]
  skeleton[is_isoyear == TRUE,  isoyearweeksun := cstime::isoyearweek_to_last_date(paste0(isoyear, "-26"))]
  skeleton[is.na(isoyearweeksun), isoyearweeksun := as.Date(paste0(isoyear, "-06-28"))]

  skeleton[is_isoyear == TRUE,  personyears := 1]
  skeleton[is_isoyear == FALSE, personyears := 1 / 52.25]

  data.table::setcolorder(skeleton, c("id", "isoyear", "isoyearweek", "is_isoyear", "isoyearweeksun", "personyears"))
  data.table::setorder(skeleton, id, isoyearweek)
  skeleton
}

# The reference takes dates and the function takes ISO weeks. Week 01 of
# `isoyear_min` starts on the Monday after the last day of ISO year
# `isoyear_min - 1`, so the reference builds no partial year from these dates
# and the two MUST agree.
expect_parity <- function(ids, isoyear_min, isoyearweek_max) {
  date_min <- cstime::isoyearweek_to_last_date(paste0(isoyear_min, "-01")) - 6L
  date_max <- cstime::isoyearweek_to_last_date(isoyearweek_max)
  ref <- .create_skeleton_reference(ids, date_min, date_max)
  new <- create_skeleton(ids, isoyear_min, isoyearweek_max)
  # Drop data.table secondary indices: they are a hidden cache that
  # depends on which [i, j] expressions were evaluated and have no
  # bearing on the row/column data we care about.
  data.table::setindex(ref, NULL)
  data.table::setindex(new, NULL)
  expect_identical(new, ref)
}

# --- Parity tests across input shapes -----------------------------------------

test_that("parity: integer ids, single-month range", {
  expect_parity(ids = c(1L, 2L, 3L), 2020, "2020-05")
})

test_that("parity: character ids", {
  expect_parity(ids = c("a", "b", "c"), 2020, "2020-11")
})

test_that("parity: single id", {
  expect_parity(ids = 42L, 2020, "2020-53")
})

test_that("parity: single-week range", {
  expect_parity(ids = c(1L, 2L), 2020, "2020-01")
})

test_that("parity: multi-year range crossing calendar boundaries", {
  expect_parity(ids = c(1L, 2L), 2018, "2021-07")
})

test_that("parity: range that includes an ISO W53 year (2020)", {
  # 2020 has 53 ISO weeks; ends Sun 2021-01-03 (W53 of 2020).
  expect_parity(ids = c(1L, 2L), 2020, "2021-02")
})

test_that("parity: annual rows end at isoyear_min - 1", {
  expect_parity(ids = c(1L, 2L), 2015, "2015-05")
})

test_that("parity: week 01 that starts in the previous calendar year", {
  # 2014-12-29 belongs to ISO year 2015 (week 1).
  expect_parity(ids = c(1L, 2L), 2015, "2015-01")
})

test_that("parity: numeric (double) ids", {
  expect_parity(ids = c(1.0, 2.0, 3.0), 2020, "2020-05")
})

# --- Structural invariants ----------------------------------------------------

test_that("output is a data.table with the expected column order", {
  s <- create_skeleton(c(1L, 2L), 2020, "2020-05")
  expect_s3_class(s, "data.table")
  expect_identical(
    names(s),
    c("id", "isoyear", "isoyearweek", "is_isoyear", "isoyearweeksun", "personyears")
  )
})

test_that("isoyearweeksun has no NA values", {
  s <- create_skeleton(1L, 2020, "2020-53")
  expect_false(anyNA(s$isoyearweeksun))
  expect_s3_class(s$isoyearweeksun, "Date")
})

test_that("personyears: 1 for isoyear rows, 1/52.25 for week rows", {
  s <- create_skeleton(1L, 2020, "2020-53")
  expect_true(all(s[is_isoyear == TRUE,  personyears] == 1))
  expect_true(all(s[is_isoyear == FALSE, personyears] == 1 / 52.25))
})

test_that("each id produces the same number of rows", {
  ids <- c("a", "b", "c", "d")
  s <- create_skeleton(ids, 2020, "2020-27")
  per_id <- s[, .N, by = id]$N
  expect_true(length(unique(per_id)) == 1L)
  expect_equal(length(per_id), length(ids))
})

test_that("rows are sorted by (id, isoyearweek)", {
  s <- create_skeleton(c(2L, 1L), 2020, "2020-14")
  reordered <- data.table::copy(s)
  data.table::setorder(reordered, id, isoyearweek)
  expect_identical(s, reordered)
})

test_that("id column type is preserved (integer in -> integer out)", {
  s <- create_skeleton(c(10L, 20L), 2020, "2020-05")
  expect_type(s$id, "integer")
})

test_that("id column type is preserved (character in -> character out)", {
  s <- create_skeleton(c("x", "y"), 2020, "2020-05")
  expect_type(s$id, "character")
})

test_that("isoyear-row range is 1900..isoyear_min - 1", {
  s <- create_skeleton(1L, 2020, "2020-53")
  isoyear_rows <- s[is_isoyear == TRUE, sort(unique(isoyear))]
  expect_equal(min(isoyear_rows), 1900L)
  expect_equal(max(isoyear_rows), 2019L)
})

test_that("week rows cover every ISO week from isoyear_min-01 to isoyearweek_max", {
  s <- create_skeleton(1L, 2020, "2020-05")
  week_rows <- s[is_isoyear == FALSE, sort(unique(isoyearweek))]
  expected_weeks <- sort(unique(cstime::date_to_isoyearweek_c(
    seq.Date(as.Date("2020-01-01"), as.Date("2020-01-31"), 1)
  )))
  expect_identical(week_rows, expected_weeks)
})

test_that("year-row isoyearweek strings match 'YYYY-**' format", {
  s <- create_skeleton(1L, 2020, "2020-05")
  yrows <- s[is_isoyear == TRUE]
  expect_true(all(yrows$isoyearweek == paste0(yrows$isoyear, "-**")))
})

# --- Edge cases probing the rep()-based replication path of PR #3 -------------

test_that("empty ids returns a 0-row skeleton with correct columns", {
  # rep(integer(0), each = n) is well-defined (returns integer(0)),
  # but worth pinning so a future change to the replication path
  # doesn't silently produce a non-zero-row result.
  s <- create_skeleton(integer(0), 2020, "2020-05")
  expect_equal(nrow(s), 0L)
  expect_identical(
    names(s),
    c("id", "isoyear", "isoyearweek", "is_isoyear", "isoyearweeksun", "personyears")
  )
})

test_that("parity: empty ids", {
  # Reference: expand.grid() with id=integer(0) also yields 0 rows.
  expect_parity(integer(0), 2020, "2020-05")
})

test_that("duplicate ids are replicated once per occurrence", {
  s <- create_skeleton(c(1L, 1L, 2L), 2020, "2020-05")
  per_id_count <- s[, .N, by = id]
  # Two unique ids, but id == 1 should appear twice as many times as id == 2.
  expect_equal(nrow(per_id_count), 2L)
  expect_equal(per_id_count[id == 1L, N], 2L * per_id_count[id == 2L, N])
})

test_that("parity: duplicate ids", {
  expect_parity(c(1L, 1L, 2L), 2020, "2020-05")
})

test_that("parity: NA in ids", {
  # Old expand.grid produces NA-id rows; new rep() does too.
  expect_parity(c(1L, NA_integer_, 2L), 2020, "2020-05")
})

# Note on a known divergence: the post-PR implementation may attach a
# data.table secondary index to the returned skeleton (depending on
# which (ids, dates) inputs trigger auto-indexing inside the new code
# path), whereas the original implementation never did. This is a hidden
# cache attribute and does not affect the data; expect_parity() above
# strips indices on both sides to make the comparison meaningful. Any
# downstream code that compares skeletons with base::identical() will
# need to do the same.
