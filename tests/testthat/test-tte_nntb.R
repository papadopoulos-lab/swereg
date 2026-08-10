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
# `.tte_nntb_cell()` then reads the STORED DECISION and picks the label:
# `"benefit"` gives `NNTB`, the number needed to treat for benefit, and
# `"harm"` gives `NNTH`, the number needed to harm. Those are opposite clinical
# statements, and the label is the only thing on the figure that separates
# them.
#
# The cell used to test the sign of `nntb` instead. Every formatter was then its
# own decision-maker, and nothing forced two of them to agree. `.tte_rd_curve()`
# now stores an `nnt_direction` column on every band, and the cell reads it.
# `.tte_nntb()` reports no direction at all, so this path holds one producer and
# one consumer. The block "the cell reads the stored direction, never the sign"
# below is the only assertion that can see a formatter that went back to the
# sign.
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

# --- the direction is DATA, decided once -----------------------------------
#
# `.tte_nnt_from_rd()` is the one place a signed risk difference becomes a
# benefit-or-harm decision. `.tte_rd_curve()` stores its two columns on every
# band, and every reader downstream reads the stored column.
#
# `.tte_nntb()` reports NO direction. It returns the magnitude and the interval
# only. A second producer there would be a second decision site, and a caller
# could then pick whichever of the two it happened to reach.

test_that("the direction reads benefit for a protective risk difference", {
  x <- swereg:::.tte_nnt_from_rd(-0.25)

  expect_identical(x$nnt_direction, "benefit")
  expect_equal(x$nnt, 4, tolerance = 1e-12)
})

test_that("the direction reads harm for a harmful risk difference", {
  x <- swereg:::.tte_nnt_from_rd(0.25)

  expect_identical(x$nnt_direction, "harm")
  expect_equal(x$nnt, -4, tolerance = 1e-12)
  # Signed. A stray `abs()` upstream would return +4 here and the direction
  # would still read "harm", so the two assertions are not redundant.
  expect_lt(x$nnt, 0)
})

test_that("a zero or missing risk difference has no direction and no nnt", {
  # `-1/0` is not a number of women, so there is nothing to label.
  x <- swereg:::.tte_nnt_from_rd(c(0, NA_real_, Inf))

  expect_identical(x$nnt_direction, rep(NA_character_, 3L))
  expect_identical(x$nnt, rep(NA_real_, 3L))
})

test_that("the direction survives an interval that spans the null", {
  # The direction belongs to the POINT estimate, so the interval guard does not
  # erase it. The rendered cell is still empty, because `nntb` is NA.
  rd <- c(-0.25, 0.25, -0.05)
  x <- swereg:::.tte_nntb(rd = rd, rd_lo = c(-0.40, 0.10, -0.12), rd_hi = c(-0.10, 0.40, 0.02))

  expect_identical(
    swereg:::.tte_nnt_from_rd(rd)$nnt_direction,
    c("benefit", "harm", "benefit")
  )
  expect_true(is.na(x$nntb[3]))
})

test_that("nntb reports three numbers and no decision", {
  # THE assertion that keeps the decision to one producer. A direction column
  # here would let a caller rebuild the decision from `rd` instead of reading
  # the one `.tte_rd_curve()` stored.
  x <- swereg:::.tte_nntb(rd = -0.25, rd_lo = -0.40, rd_hi = -0.10)

  expect_identical(names(x), c("nntb", "nntb_lo", "nntb_hi"))
  expect_false("nnt_direction" %in% names(x))

  # The zero-length return keeps the same three columns.
  empty <- swereg:::.tte_nntb(numeric(0), numeric(0), numeric(0))
  expect_identical(names(empty), c("nntb", "nntb_lo", "nntb_hi"))
})

# --- the cell builder: the STORED DECISION chooses the label ---------------
#
# `.tte_nntb_cell()` takes the value, its two bounds and the stored direction.
# The `role` guard is gone: every row gets a number needed to treat, so a guard
# that could never change the result was misdirection rather than a filter.
#
# `nntb_dir()` below is how a caller assembles the two halves: the magnitude and
# the interval from `.tte_nntb()`, and the direction from `.tte_nnt_from_rd()`,
# which is the same function `.tte_rd_curve()` calls. Production reads the
# direction off a stored column instead; the decision function is the same one.

nntb_dir <- function(rd, rd_lo, rd_hi) {
  x <- swereg:::.tte_nntb(rd, rd_lo, rd_hi)
  x$nnt_direction <- swereg:::.tte_nnt_from_rd(rd)$nnt_direction
  x
}

test_that("the cell builder takes the value, its bounds and the stored direction", {
  expect_identical(
    names(formals(swereg:::.tte_nntb_cell)),
    c("nntb", "nntb_lo", "nntb_hi", "nnt_direction")
  )
  expect_false("role" %in% names(formals(swereg:::.tte_nntb_cell)))
})

test_that("a cell builder called with no direction errors instead of guessing", {
  expect_error(
    swereg:::.tte_nntb_cell(2000),
    "nnt_direction is required"
  )
  expect_error(
    swereg:::.tte_nntb_cell(2000, 1250, 5000),
    "nnt_direction is required"
  )
})

test_that("an unknown direction is an error, not an empty cell", {
  expect_error(
    swereg:::.tte_nntb_cell(2000, nnt_direction = "protective"),
    "must be 'benefit', 'harm' or NA"
  )
})

test_that("a protective risk difference renders NNTB, whole people, comma-grouped", {
  # Synthetic and round, on the scale a national-registry panel reports: a
  # protective risk difference of -5.00 per 10,000, so -1/rd is 2,000 people.
  # The four-digit value is the point of the row. No other assertion in this
  # file can see the thousands separator, because every other value is small.
  x <- nntb_dir(rd = -5.0e-4, rd_lo = -8.0e-4, rd_hi = -2.0e-4)

  expect_identical(
    swereg:::.tte_nntb_cell(x$nntb, nnt_direction = x$nnt_direction),
    "NNTB 2,000"
  )
  # The separator survives on both bounds too, not just on the point estimate.
  expect_identical(
    swereg:::.tte_nntb_cell(x$nntb, x$nntb_lo, x$nntb_hi, x$nnt_direction),
    "NNTB 2,000 (1,250 to 5,000)"
  )
})

test_that("a harmful risk difference renders NNTH, never NNTB and never a minus", {
  # THE assertion this file exists for. A synthetic harmful risk difference of
  # +20.00 per 10,000, so nntb is -500. Two defects render here and nowhere
  # else: a fixed `NNTB` label calls harm a benefit, and a leaked minus sign
  # prints a negative number of people.
  x <- nntb_dir(rd = 2.0e-3, rd_lo = 1.0e-3, rd_hi = 4.0e-3)
  cell <- swereg:::.tte_nntb_cell(x$nntb, nnt_direction = x$nnt_direction)

  expect_identical(cell, "NNTH 500")
  expect_false(grepl("NNTB", cell, fixed = TRUE))
  expect_false(grepl("-", cell, fixed = TRUE))

  # The bounds are -1,000 and -250, so the harm branch must negate AND reorder
  # to print them ascending. Negating alone gives "(1,000 to 250)".
  with_ci <- swereg:::.tte_nntb_cell(
    x$nntb,
    x$nntb_lo,
    x$nntb_hi,
    x$nnt_direction
  )
  expect_identical(with_ci, "NNTH 500 (250 to 1,000)")
  expect_false(grepl("-", with_ci, fixed = TRUE))
})

# --- the interval reaches the cell -----------------------------------------

test_that("the cell carries the interval whenever both bounds are supplied", {
  benefit <- nntb_dir(rd = -5.0e-4, rd_lo = -8.0e-4, rd_hi = -2.0e-4)
  harm <- nntb_dir(rd = 2.0e-3, rd_lo = 1.0e-3, rd_hi = 4.0e-3)

  b <- swereg:::.tte_nntb_cell(
    benefit$nntb,
    benefit$nntb_lo,
    benefit$nntb_hi,
    benefit$nnt_direction
  )
  h <- swereg:::.tte_nntb_cell(
    harm$nntb,
    harm$nntb_lo,
    harm$nntb_hi,
    harm$nnt_direction
  )

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
  benefit <- nntb_dir(rd = -5.0e-4, rd_lo = -8.0e-4, rd_hi = -2.0e-4)
  harm <- nntb_dir(rd = 2.0e-3, rd_lo = 1.0e-3, rd_hi = 4.0e-3)

  b <- read_bounds(swereg:::.tte_nntb_cell(
    benefit$nntb,
    benefit$nntb_lo,
    benefit$nntb_hi,
    benefit$nnt_direction
  ))
  h <- read_bounds(swereg:::.tte_nntb_cell(
    harm$nntb,
    harm$nntb_lo,
    harm$nntb_hi,
    harm$nnt_direction
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
  expect_identical(
    swereg:::.tte_nntb_cell(2000, NA_real_, NA_real_, "benefit"),
    ""
  )
  expect_identical(
    swereg:::.tte_nntb_cell(-500, NA_real_, NA_real_, "harm"),
    ""
  )

  # Not the point estimate, and not the point estimate plus an "NA" interval.
  cell <- swereg:::.tte_nntb_cell(2000, NA_real_, NA_real_, "benefit")
  expect_false(grepl("2,000", cell, fixed = TRUE))
  expect_false(grepl("NA", cell, fixed = TRUE))
})

test_that("omitting both bounds renders the point estimate alone", {
  # The bounds stay optional. `.forest_rd_map()` in R/forest_plot.R supplies
  # them, and the direction is required either way.
  expect_identical(
    swereg:::.tte_nntb_cell(2000, nnt_direction = "benefit"),
    "NNTB 2,000"
  )
  expect_identical(
    swereg:::.tte_nntb_cell(-500, nnt_direction = "harm"),
    "NNTH 500"
  )
})

# --- the cell reads the stored direction, never the sign -------------------
#
# THE block this phase exists for. Every other assertion in this file feeds the
# cell a value whose sign AGREES with its direction, so every one of them passes
# on a formatter that went back to `nntb > 0`.
#
# These two rows disagree on purpose. The sign says one thing and the stored
# direction says the other, and the cell MUST follow the stored direction.
# Nothing in production builds a row like this: `.tte_nnt_from_rd()` decides
# both from one number. That is the point. The rows exist to make the read
# observable, in the one place a re-derivation is invisible everywhere else.
#
# The rendered magnitude then carries a minus sign, because the harm branch
# negates a value that was already positive. That is honest: the formatter obeys
# the data it was handed and invents nothing.

test_that("the cell follows the stored direction when the sign disagrees", {
  # Sign says harm (-500). Stored direction says benefit.
  says_benefit <- swereg:::.tte_nntb_cell(-500, nnt_direction = "benefit")
  expect_match(says_benefit, "^NNTB ")
  expect_false(grepl("NNTH", says_benefit, fixed = TRUE))

  # Sign says benefit (+2000). Stored direction says harm.
  says_harm <- swereg:::.tte_nntb_cell(2000, nnt_direction = "harm")
  expect_match(says_harm, "^NNTH ")
  expect_false(grepl("NNTB", says_harm, fixed = TRUE))
})

test_that("the cell follows the stored direction with an interval too", {
  # The same disagreement on the interval branch, which formats separately.
  says_benefit <- swereg:::.tte_nntb_cell(-500, -1000, -250, "benefit")
  expect_match(says_benefit, "^NNTB ")

  says_harm <- swereg:::.tte_nntb_cell(2000, 1250, 5000, "harm")
  expect_match(says_harm, "^NNTH ")
})

test_that("a missing direction renders an empty cell, whatever the sign", {
  # The value is finite and the sign is unambiguous. No stored decision means
  # no label, so the cell stays empty rather than guessing one.
  expect_identical(swereg:::.tte_nntb_cell(2000, nnt_direction = NA), "")
  expect_identical(swereg:::.tte_nntb_cell(-500, nnt_direction = NA), "")
})

test_that("benefit and harm of equal magnitude do not render the same cell", {
  # The assertion that sees a stray `abs()`. Under `abs()` both sides carry the
  # same magnitude AND the same label, and the figure still draws.
  benefit <- nntb_dir(rd = -0.25, rd_lo = -0.40, rd_hi = -0.10)
  harm <- nntb_dir(rd = 0.25, rd_lo = 0.10, rd_hi = 0.40)

  b <- swereg:::.tte_nntb_cell(
    benefit$nntb,
    nnt_direction = benefit$nnt_direction
  )
  h <- swereg:::.tte_nntb_cell(harm$nntb, nnt_direction = harm$nnt_direction)
  expect_identical(b, "NNTB 4")
  expect_identical(h, "NNTH 4")
  expect_false(identical(b, h))
})

test_that("an undefined nntb renders an empty cell", {
  x <- nntb_dir(rd = -0.05, rd_lo = -0.12, rd_hi = 0.02)

  expect_identical(
    swereg:::.tte_nntb_cell(x$nntb, nnt_direction = x$nnt_direction),
    ""
  )
  expect_identical(
    swereg:::.tte_nntb_cell(x$nntb, x$nntb_lo, x$nntb_hi, x$nnt_direction),
    ""
  )
})

test_that("the cell builder vectorises over the whole fixture in one call", {
  x <- nntb_dir(
    rd = c(-0.25, 0.25, -0.05, -0.25, 0.05),
    rd_lo = c(-0.40, 0.10, -0.12, -0.40, 0.00),
    rd_hi = c(-0.10, 0.40, 0.02, 0.00, 0.12)
  )

  expect_identical(
    swereg:::.tte_nntb_cell(x$nntb, nnt_direction = x$nnt_direction),
    c("NNTB 4", "NNTH 4", "", "", "")
  )

  # The same five rows with their intervals. `.ff_num()` rounds half to even,
  # so a bound of 2.5 prints as 2.
  expect_identical(
    swereg:::.tte_nntb_cell(x$nntb, x$nntb_lo, x$nntb_hi, x$nnt_direction),
    c("NNTB 4 (2 to 10)", "NNTH 4 (2 to 10)", "", "", "")
  )
})
