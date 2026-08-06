# Number needed to treat for benefit.
#
# `.tte_nntb()` inverts the SIGNED cause-specific risk difference that
# `$risk_difference()` returns:
#
#   RD(t) = Risk_intervention(t) - Risk_comparator(t)
#         = S_comparator(t) - S_intervention(t)
#
# so a protective intervention gives a NEGATIVE risk difference, and
#
#   nntb = -1/rd,  nntb_lo = -1/rd_lo,  nntb_hi = -1/rd_hi
#
# The minus sign is what makes a benefit read as a positive number of women.
# The value stays SIGNED: harm returns a negative number and keeps it. `abs()`
# has no place in this arithmetic, and the harm row below is the only assertion
# that can see a stray one, because a benefit row has a correct-looking
# magnitude either way.
#
# The fixture is pure arithmetic on injected risk differences and intervals.
# No bootstrap, no panel, no survival curve: the guard and the inversion are
# the whole subject, and a resampled interval would only make them harder to
# read.
#
#     rd      rd_lo   rd_hi   nntb   nntb_lo  nntb_hi
#   -0.25    -0.40   -0.10       4       2.5     10.0   benefit
#    0.25     0.10    0.40      -4     -10.0     -2.5   harm, signed
#   -0.05    -0.12    0.02      NA        NA       NA   interval spans zero
#   -0.25    -0.40    0.00      NA        NA       NA   upper bound exactly 0
#    0.05     0.00    0.12      NA        NA       NA   lower bound exactly 0
#
# Rows 4 and 5 are the point of the file. A bound of exactly zero touches the
# null, so the interval does NOT exclude it, and all three values are
# undefined. `>` loosened to `>=` (or `<` to `<=`) is a one-character change
# that reports a finite number needed to treat for an interval compatible with
# no effect, and it reports it on ONE side only, which is why both sides are
# pinned separately here.
#
# The bounds keep their roles while inverting in value: `x -> -1/x` is monotone
# increasing on each side of zero, so `rd_lo` maps to `nntb_lo` and
# `nntb_lo < nntb_hi` still holds. That monotonicity is exactly what fails
# across zero, and is the mathematical reason the guard must be strict.
#
# `.tte_nntb_cell()` then reads the sign and picks the label: a positive value
# is `NNTB`, the number needed to treat for benefit, and a negative value is
# `NNTH`, the number needed to harm. Those are opposite clinical statements,
# and the label is the only thing on the figure that separates them.
#
# The cell carries the INTERVAL as well, whenever the caller hands over the two
# bounds. Three things about that interval are pinned below and none of them is
# visible in the point estimate:
#
#   * The bounds print in ascending order on BOTH signs. `-1/x` is monotone
#     increasing on each side of zero. So the benefit branch prints the bounds
#     as it holds them, and the harm branch negates each one, which reverses
#     the order. A branch that negates without reordering prints a descending
#     interval and still draws.
#   * A row whose bounds are missing prints NOTHING, not the point estimate on
#     its own. `.tte_rd_curve()` blanks both bounds on a zero-event arm, where
#     a bare point estimate would read as precise.
#   * Both bounds take the point estimate's thousands separator and its 0
#     decimal places.
#
# The values below are illustrative and synthetic. They are round on purpose,
# so nobody can read a measurement out of a test fixture.

skip_if_not_installed("data.table")

test_that("nntb is the negated reciprocal with reciprocal-inverted bounds", {
  x <- swereg:::.tte_nntb(rd = -0.25, rd_lo = -0.40, rd_hi = -0.10)

  expect_equal(x$nntb, 4, tolerance = 1e-12)
  expect_equal(x$nntb_lo, 2.5, tolerance = 1e-12)
  expect_equal(x$nntb_hi, 10, tolerance = 1e-12)
})

test_that("the nntb bounds keep the ordering of the risk difference bounds", {
  benefit <- swereg:::.tte_nntb(rd = -0.25, rd_lo = -0.40, rd_hi = -0.10)
  harm <- swereg:::.tte_nntb(rd = 0.25, rd_lo = 0.10, rd_hi = 0.40)

  expect_lt(benefit$nntb_lo, benefit$nntb_hi)
  expect_lt(harm$nntb_lo, harm$nntb_hi)
})

test_that("a harmful risk difference returns a negative nntb", {
  x <- swereg:::.tte_nntb(rd = 0.25, rd_lo = 0.10, rd_hi = 0.40)

  expect_equal(x$nntb, -4, tolerance = 1e-12)
  expect_lt(x$nntb, 0)
  expect_equal(x$nntb_lo, -10, tolerance = 1e-12)
  expect_equal(x$nntb_hi, -2.5, tolerance = 1e-12)
})

test_that("an interval spanning the null gives NA", {
  x <- swereg:::.tte_nntb(rd = -0.05, rd_lo = -0.12, rd_hi = 0.02)

  expect_true(is.na(x$nntb))
  expect_true(is.na(x$nntb_lo))
  expect_true(is.na(x$nntb_hi))
})

test_that("an upper bound of exactly zero is not exclusion of the null", {
  x <- swereg:::.tte_nntb(rd = -0.25, rd_lo = -0.40, rd_hi = 0)

  expect_true(is.na(x$nntb))
  expect_true(is.na(x$nntb_lo))
  expect_true(is.na(x$nntb_hi))
})

test_that("a lower bound of exactly zero is not exclusion of the null", {
  x <- swereg:::.tte_nntb(rd = 0.05, rd_lo = 0, rd_hi = 0.12)

  expect_true(is.na(x$nntb))
  expect_true(is.na(x$nntb_lo))
  expect_true(is.na(x$nntb_hi))
})

test_that("the whole fixture table survives one vectorised call", {
  fx <- data.table::data.table(
    rd = c(-0.25, 0.25, -0.05, -0.25, 0.05),
    rd_lo = c(-0.40, 0.10, -0.12, -0.40, 0.00),
    rd_hi = c(-0.10, 0.40, 0.02, 0.00, 0.12)
  )

  x <- swereg:::.tte_nntb(fx$rd, fx$rd_lo, fx$rd_hi)

  expect_equal(x$nntb, c(4, -4, NA, NA, NA), tolerance = 1e-12)
  expect_equal(x$nntb_lo, c(2.5, -10, NA, NA, NA), tolerance = 1e-12)
  expect_equal(x$nntb_hi, c(10, -2.5, NA, NA, NA), tolerance = 1e-12)
})

# --- the cell builder: the SIGN chooses the label -------------------------
#
# `.tte_nntb_cell()` takes the value and its two bounds. The `role` guard is
# gone: every row gets a number needed to treat, so a guard that could never
# change the result was misdirection rather than a filter.

test_that("the cell builder takes the value and its bounds, with no role guard", {
  expect_identical(
    names(formals(swereg:::.tte_nntb_cell)),
    c("nntb", "nntb_lo", "nntb_hi")
  )
  expect_false("role" %in% names(formals(swereg:::.tte_nntb_cell)))
})

test_that("a protective risk difference renders NNTB, whole people, comma-grouped", {
  # Synthetic and round, on the scale a national-registry panel reports: a
  # protective risk difference of -5.00 per 10,000, so -1/rd is 2,000 people.
  # The four-digit value is the point of the row. No other assertion in this
  # file can see the thousands separator, because every other value is small.
  x <- swereg:::.tte_nntb(rd = -5.0e-4, rd_lo = -8.0e-4, rd_hi = -2.0e-4)

  expect_identical(swereg:::.tte_nntb_cell(x$nntb), "NNTB 2,000")
  # The separator survives on both bounds too, not just on the point estimate.
  expect_identical(
    swereg:::.tte_nntb_cell(x$nntb, x$nntb_lo, x$nntb_hi),
    "NNTB 2,000 (1,250 to 5,000)"
  )
})

test_that("a harmful risk difference renders NNTH, never NNTB and never a minus", {
  # THE assertion this file exists for. A synthetic harmful risk difference of
  # +20.00 per 10,000, so nntb is -500. Two defects render here and nowhere
  # else: a fixed `NNTB` label calls harm a benefit, and a leaked minus sign
  # prints a negative number of people.
  x <- swereg:::.tte_nntb(rd = 2.0e-3, rd_lo = 1.0e-3, rd_hi = 4.0e-3)
  cell <- swereg:::.tte_nntb_cell(x$nntb)

  expect_identical(cell, "NNTH 500")
  expect_false(grepl("NNTB", cell, fixed = TRUE))
  expect_false(grepl("-", cell, fixed = TRUE))

  # The bounds are -1,000 and -250, so the harm branch must negate AND reorder
  # to print them ascending. Negating alone gives "(1,000 to 250)".
  with_ci <- swereg:::.tte_nntb_cell(x$nntb, x$nntb_lo, x$nntb_hi)
  expect_identical(with_ci, "NNTH 500 (250 to 1,000)")
  expect_false(grepl("-", with_ci, fixed = TRUE))
})

# --- the interval reaches the cell -----------------------------------------

test_that("the cell carries the interval whenever both bounds are supplied", {
  benefit <- swereg:::.tte_nntb(rd = -5.0e-4, rd_lo = -8.0e-4, rd_hi = -2.0e-4)
  harm <- swereg:::.tte_nntb(rd = 2.0e-3, rd_lo = 1.0e-3, rd_hi = 4.0e-3)

  b <- swereg:::.tte_nntb_cell(benefit$nntb, benefit$nntb_lo, benefit$nntb_hi)
  h <- swereg:::.tte_nntb_cell(harm$nntb, harm$nntb_lo, harm$nntb_hi)

  # The whole interval, not a fragment of it.
  expect_identical(b, "NNTB 2,000 (1,250 to 5,000)")
  expect_identical(h, "NNTH 500 (250 to 1,000)")

  # Stated again as a property, so a renderer that drops the bounds fails here
  # even if somebody relaxes the two strings above.
  expect_match(b, "^NNTB [0-9,]+ \\([0-9,]+ to [0-9,]+\\)$")
  expect_match(h, "^NNTH [0-9,]+ \\([0-9,]+ to [0-9,]+\\)$")

  # One separator across the figure: ` to `, the same one the risk-difference
  # column uses.
  expect_true(grepl(" to ", b, fixed = TRUE))
  expect_false(grepl("–", b, fixed = TRUE))
})

test_that("the printed bounds ascend on both signs", {
  read_bounds <- function(cell) {
    inner <- sub("^[A-Z]+ [0-9,]+ \\((.*)\\)$", "\\1", cell)
    as.numeric(gsub(",", "", strsplit(inner, " to ", fixed = TRUE)[[1]]))
  }
  benefit <- swereg:::.tte_nntb(rd = -5.0e-4, rd_lo = -8.0e-4, rd_hi = -2.0e-4)
  harm <- swereg:::.tte_nntb(rd = 2.0e-3, rd_lo = 1.0e-3, rd_hi = 4.0e-3)

  b <- read_bounds(swereg:::.tte_nntb_cell(
    benefit$nntb,
    benefit$nntb_lo,
    benefit$nntb_hi
  ))
  h <- read_bounds(swereg:::.tte_nntb_cell(
    harm$nntb,
    harm$nntb_lo,
    harm$nntb_hi
  ))

  expect_length(b, 2L)
  expect_length(h, 2L)
  expect_lt(b[1], b[2])
  # The harm branch is the one a reciprocal transform gets wrong: its stored
  # bounds are both negative, so negating them without reordering descends.
  expect_lt(h[1], h[2])

  # The printed bounds bracket the printed point estimate on both signs.
  expect_lt(b[1], 2000)
  expect_gt(b[2], 2000)
  expect_lt(h[1], 500)
  expect_gt(h[2], 500)
})

test_that("a finite point estimate with missing bounds renders an empty cell", {
  # A point estimate shown without its interval reads as precise.
  # `.tte_rd_curve()` blanks both bounds on a zero-event arm, so this is the
  # cell that arm reaches.
  expect_identical(swereg:::.tte_nntb_cell(2000, NA_real_, NA_real_), "")
  expect_identical(swereg:::.tte_nntb_cell(-500, NA_real_, NA_real_), "")

  # Not the point estimate, and not the point estimate plus an "NA" interval.
  cell <- swereg:::.tte_nntb_cell(2000, NA_real_, NA_real_)
  expect_false(grepl("2,000", cell, fixed = TRUE))
  expect_false(grepl("NA", cell, fixed = TRUE))
})

test_that("omitting both bounds renders the point estimate alone", {
  # `.forest_rd_map()` in R/forest_plot.R still calls the one-argument form.
  expect_identical(swereg:::.tte_nntb_cell(2000), "NNTB 2,000")
  expect_identical(swereg:::.tte_nntb_cell(-500), "NNTH 500")
})

test_that("benefit and harm of equal magnitude do not render the same cell", {
  # The assertion that sees a stray `abs()`. Under `abs()` both sides carry the
  # same magnitude AND the same label, and the figure still draws.
  benefit <- swereg:::.tte_nntb(rd = -0.25, rd_lo = -0.40, rd_hi = -0.10)
  harm <- swereg:::.tte_nntb(rd = 0.25, rd_lo = 0.10, rd_hi = 0.40)

  b <- swereg:::.tte_nntb_cell(benefit$nntb)
  h <- swereg:::.tte_nntb_cell(harm$nntb)
  expect_identical(b, "NNTB 4")
  expect_identical(h, "NNTH 4")
  expect_false(identical(b, h))
})

test_that("an undefined nntb renders an empty cell", {
  x <- swereg:::.tte_nntb(rd = -0.05, rd_lo = -0.12, rd_hi = 0.02)

  expect_identical(swereg:::.tte_nntb_cell(x$nntb), "")
  expect_identical(
    swereg:::.tte_nntb_cell(x$nntb, x$nntb_lo, x$nntb_hi),
    ""
  )
})

test_that("the cell builder vectorises over the whole fixture in one call", {
  x <- swereg:::.tte_nntb(
    rd = c(-0.25, 0.25, -0.05, -0.25, 0.05),
    rd_lo = c(-0.40, 0.10, -0.12, -0.40, 0.00),
    rd_hi = c(-0.10, 0.40, 0.02, 0.00, 0.12)
  )

  expect_identical(
    swereg:::.tte_nntb_cell(x$nntb),
    c("NNTB 4", "NNTH 4", "", "", "")
  )

  # The same five rows with their intervals. `.ff_num()` rounds half to even,
  # so a bound of 2.5 prints as 2.
  expect_identical(
    swereg:::.tte_nntb_cell(x$nntb, x$nntb_lo, x$nntb_hi),
    c("NNTB 4 (2 to 10)", "NNTH 4 (2 to 10)", "", "", "")
  )
})
