# Cause-specific risk difference with a person-level percentile bootstrap.
#
# `$risk_difference()` returns, at each band,
#
#   RD(t) = Risk_intervention(t) - Risk_comparator(t)
#         = [1 - S_intervention(t)] - [1 - S_comparator(t)]
#         = S_comparator(t) - S_intervention(t)
#
# SIGNED. A protective intervention gives a negative risk difference, and that
# minus sign is the answer. `abs()` has no place anywhere in this arithmetic.
# An absolute risk difference has a correct-looking magnitude at every band, so
# no assertion on the point estimate alone can see a stray one. TWO assertions
# in this file can, and both are kept deliberately: the mirror test below, and
# "person-trial aggregation is exactly person-level aggregation", whose
# bootstrap replicates straddle zero even where the point estimate does not, so
# `abs()` breaks 83 of its 600 bit-identical replicate comparisons. The sign
# convention is therefore pinned in two unrelated places, not one.
#
# Fixture (measured against the real `$survival_curve()`, not hand-waved):
#
#    exposed tstop events at_risk n_persons_at_risk    hazard      surv
#      FALSE     4    0.0       6                 2 0.0000000 1.0000000
#      FALSE     8    2.0       6                 2 0.3333333 0.6666667
#       TRUE     4    0.5       2                 3 0.2500000 0.7500000
#       TRUE     8    0.5       1                 2 0.5000000 0.3750000
#
# so RD(4) = 1 - 3/4 = 1/4 and RD(8) = 2/3 - 3/8 = 7/24. Both strictly
# positive, and unequal, so an off-by-one in time cannot hide behind them.
#
# Two structural properties of the fixture carry the two subtlest assertions:
#
#   * p3 is in BOTH arms (TRUE = {p1,p2,p3}, FALSE = {p3,p4}). On a real
#     national-registry panel a few percent of women straddle the arms the
#     same way. One multiplicity vector must therefore reach both arms; a
#     separate draw per arm leaves the point estimate untouched and biases
#     the interval, and nothing but assertion 5 can see it.
#   * p1 carries an event in TWO of her trials (p1_tB at band 4, p1_tA at
#     band 8). She is ONE person who had the outcome. Assertion 7 pins that the
#     reported count is 1 and not 2.
#
# A third structural property carries the zero-event block at the end of the
# file. The comparator arm has NO event through band 4. It has one event,
# weight 2, by band 8. So one arm is inestimable at the early horizon and
# estimable at the late one, in a single fixture. That is what makes "per
# horizon" testable against "per panel".
#
# Person blocks are unequal on purpose: p1 = 3 rows, p2 = 2, p3 = 5, p4 = 2.

skip_if_not_installed("data.table")

# --- fixture ---------------------------------------------------------------

rd_dt <- function() {
  data.table::data.table(
    enrollment_person_trial_id = c(
      "p1_tA",
      "p1_tB",
      "p2_tA",
      "p3_tC",
      "p3_tD",
      "p3_tE",
      "p4_tF",
      "p1_tA",
      "p2_tA",
      "p3_tC",
      "p3_tD",
      "p4_tF"
    ),
    id = c("p1", "p1", "p2", "p3", "p3", "p3", "p4", "p1", "p2", "p3", "p3", "p4"),
    exposed = c(
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE
    ),
    tstop = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 8L, 8L, 8L, 8L, 8L),
    event = c(0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L),
    w = c(0.5, 0.5, 0.5, 2, 2, 0.5, 2, 0.5, 0.5, 2, 2, 2),
    age = 50,
    death = 0L
  )
}

rd_trial <- function(dt = rd_dt()) {
  design <- swereg::TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )
  swereg::TTEEnrollment$new(dt, design)
}

# Drive the public method, which is what a caller reaches.
rd_out <- function(dt = rd_dt(), n_boot = 200L, seed = 4L, conf_level = 0.95) {
  rd_trial(dt)$risk_difference(
    weight_col = "w",
    n_boot = n_boot,
    seed = seed,
    conf_level = conf_level
  )
}

# --- assertion 1: arm order is read at source, never inferred --------------

test_that("survival is reported per named arm, comparator 2/3 and intervention 3/8", {
  out <- rd_out()
  at8 <- out[tstop == 8L]
  # Named fields. A positional read (row 1 vs row 2, column 2 vs column 3)
  # would pass with the arms swapped, which is the whole failure mode here.
  expect_equal(at8$surv_comparator, 2 / 3)
  expect_equal(at8$surv_intervention, 3 / 8)
  at4 <- out[tstop == 4L]
  expect_equal(at4$surv_comparator, 1)
  expect_equal(at4$surv_intervention, 3 / 4)
})

# --- assertion 2: the sign, twice ------------------------------------------

test_that("risk difference is S_comparator - S_intervention and is positive here", {
  out <- rd_out()
  rd_at_8 <- out[tstop == 8L]$rd
  rd_at_4 <- out[tstop == 4L]$rd
  expect_equal(rd_at_8, 7 / 24)
  expect_gt(rd_at_8, 0)
  expect_equal(rd_at_4, 1 / 4)
  expect_gt(rd_at_4, 0)
})

# --- assertion 3: the mirror ------------------------------------------------

test_that("mirroring the arms flips the sign of the risk difference", {
  # Relabelling every row swaps which arm is which and nothing else, so every
  # risk difference must come back with the opposite sign and the same size.
  # This is the ONLY assertion in this file that a stray abs() cannot survive.
  dt <- rd_dt()
  dt[, exposed := !exposed]
  out <- rd_out(dt)
  rd_at_8 <- out[tstop == 8L]$rd
  rd_at_4 <- out[tstop == 4L]$rd
  expect_equal(rd_at_8, -7 / 24)
  expect_lt(rd_at_8, 0)
  expect_equal(rd_at_4, -1 / 4)
  expect_lt(rd_at_4, 0)
})

# --- assertion 4: the resampling unit is the person ------------------------

test_that("the bootstrap index draws whole persons, not rows", {
  dt <- rd_dt()
  block <- table(dt$id) # p1 = 3, p2 = 2, p3 = 5, p4 = 2 -- deliberately unequal

  set.seed(11L)
  for (i in 1:50) {
    idx <- swereg:::.boot_person_index(dt$id)
    tab <- table(dt$id[idx])
    blk <- as.integer(block[names(tab)])
    # Whole blocks only: a row-level resample would almost never land on an
    # exact multiple of every drawn person's block size.
    expect_true(all(as.integer(tab) %% blk == 0L))
    # Exactly as many persons as exist, drawn with replacement.
    expect_equal(sum(as.integer(tab) / blk), length(block))
  }

  # The same contract on the vector production actually hands it: one element
  # per person-trial. p3 owns three person-trials, so her multiplicity has to
  # be carried identically by all three.
  pt <- unique(dt[, .(pt = enrollment_person_trial_id, person = id)])
  data.table::setkeyv(pt, "pt")
  set.seed(12L)
  for (i in 1:50) {
    mult <- tabulate(swereg:::.boot_person_index(pt$person), nbins = nrow(pt))
    per_person <- split(mult, pt$person)
    expect_true(all(vapply(
      per_person,
      function(m) length(unique(m)) == 1L,
      logical(1)
    )))
    expect_equal(sum(vapply(per_person, function(m) m[1], numeric(1))), 4)
  }

  # Seed reproducibility.
  set.seed(99L)
  a <- swereg:::.boot_person_index(dt$id)
  set.seed(99L)
  b <- swereg:::.boot_person_index(dt$id)
  expect_identical(a, b)
})

# --- assertion 5: ONE draw, applied to BOTH arms ---------------------------

test_that("one multiplicity vector is applied to both arms", {
  dt <- rd_dt()

  real_idx <- swereg:::.boot_person_index
  n_calls <- 0L
  testthat::local_mocked_bindings(
    .boot_person_index = function(...) {
      n_calls <<- n_calls + 1L
      real_idx(...)
    },
    .package = "swereg"
  )

  set.seed(7L)
  n_boot <- 60L
  out <- swereg:::.tte_rd_curve(
    data = dt,
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    time_var = "tstop",
    weight_col = "w",
    n_boot = n_boot,
    keep_mult = TRUE
  )

  # One draw per replicate. A separate resample per arm doubles this.
  expect_identical(n_calls, n_boot)

  mult_int <- attr(out, "mult_intervention")
  mult_cmp <- attr(out, "mult_comparator")
  expect_identical(nrow(mult_int), n_boot)

  # Recorded where each vector was applied, so this compares what the two arms
  # were actually multiplied by, replicate for replicate.
  expect_identical(mult_int, mult_cmp)

  # --- the property, stated so it outlives the representation ---------------
  #
  # The multiplicity is indexed by whatever the estimator chose to index it by.
  # The shipped one indexes person-trials; a person-indexed one would carry one
  # column per person and is proven exactly equal to it by
  # "person-trial aggregation is exactly person-level aggregation" below. So
  # resolve the column owners from the fixture instead of asserting a width:
  # what is pinned here is the property, not the layout.
  pt_levels <- levels(factor(dt$enrollment_person_trial_id))
  persons <- levels(factor(dt$id))
  owner <- if (ncol(mult_int) == length(pt_levels)) {
    dt$id[match(pt_levels, dt$enrollment_person_trial_id)]
  } else if (ncol(mult_int) == length(persons)) {
    persons
  } else {
    stop("unrecognised multiplicity layout: ", ncol(mult_int), " columns")
  }

  # p3 straddles the arms: her trials p3_tC and p3_tD sit in the comparator arm
  # and p3_tE in the intervention arm.
  p3_cols <- which(owner == "p3")
  expect_gt(length(p3_cols), 0L)

  # "p3's multiplicity in this arm" is well defined only if every column she
  # owns carries the same number, so establish that before reading one off.
  p3_agrees <- function(m) {
    apply(m[, p3_cols, drop = FALSE], 1L, function(r) length(unique(r)) == 1L)
  }
  expect_true(all(p3_agrees(mult_int)))
  expect_true(all(p3_agrees(mult_cmp)))

  # THE PROPERTY: for every replicate, the multiplicity p3 carries into the
  # intervention arm equals the multiplicity she carries into the comparator
  # arm. An independent draw per arm is what breaks this and nothing else can.
  m_p3_int <- mult_int[, p3_cols[1]]
  m_p3_cmp <- mult_cmp[, p3_cols[1]]
  expect_identical(m_p3_int, m_p3_cmp)

  # The draw is genuinely varying, so the identity above is not vacuous.
  expect_gt(length(unique(m_p3_int)), 1L)
})

# --- the representation is exact: person-trial rows == person rows ----------

test_that("person-trial aggregation is exactly person-level aggregation", {
  # The shipped estimator lays its matrices out with ONE ROW PER PERSON-TRIAL,
  # not one row per person. The resampling unit is still the person: the index
  # expands whole person blocks, so every person-trial carries its owner's
  # multiplicity and
  #
  #   sum over person-trials t of  m_owner(t) * N_t
  #     = sum over persons p of  m_p * (sum of N_t over the trials p owns)
  #
  # collapses onto the person-indexed form. This test PROVES that equality on
  # the shipped code instead of arguing it. The person-indexed estimator is not
  # implemented because it does not need to be, and this assertion is what earns
  # that: one person multiplicity vector, applied to both representations, must
  # give the same risk difference.
  dt <- rd_dt()
  persons <- levels(factor(dt$id))
  pt_levels <- levels(factor(dt$enrollment_person_trial_id))
  owner <- dt$id[match(pt_levels, dt$enrollment_person_trial_id)]
  bands <- sort(unique(dt$tstop))

  # Person-indexed matrices, built here from the fixture and from nothing the
  # estimator computed.
  person_mats <- function(arm_value) {
    a <- dt[
      exposed == arm_value,
      .(num = sum(w * event), den = sum(w)),
      keyby = .(p = match(id, persons), b = match(tstop, bands))
    ]
    num <- matrix(0, length(persons), length(bands))
    den <- matrix(0, length(persons), length(bands))
    num[cbind(a$p, a$b)] <- a$num
    den[cbind(a$p, a$b)] <- a$den
    list(num = num, den = den)
  }
  p_int <- person_mats(TRUE)
  p_cmp <- person_mats(FALSE)
  person_surv <- function(m, mats) {
    num <- as.numeric(m %*% mats$num)
    den <- as.numeric(m %*% mats$den)
    den[!is.finite(den) | den <= 0] <- NA_real_
    cumprod(1 - num / den)
  }

  set.seed(21L)
  n_boot <- 300L
  out <- swereg:::.tte_rd_curve(
    data = dt,
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    time_var = "tstop",
    weight_col = "w",
    n_boot = n_boot,
    keep_mult = TRUE
  )
  boot <- attr(out, "rd_boot")
  mult_int <- attr(out, "mult_intervention")
  mult_cmp <- attr(out, "mult_comparator")

  # Each arm's reference is built from the vector THAT arm was given. Whether
  # the two arms share one vector is a different property, pinned by
  # "one multiplicity vector is applied to both arms"; reading both here keeps
  # this test about the layout alone, so exactly one assertion answers each
  # question.
  to_person_grain <- function(m_row) {
    vapply(split(m_row, owner), function(v) v[1], numeric(1))[persons]
  }

  n_degenerate <- 0L
  for (b in seq_len(n_boot)) {
    # Every trial a person owns carries her number, which the assertion above
    # pins, so v[1] is that number.
    ref <- person_surv(to_person_grain(mult_cmp[b, ]), p_cmp) -
      person_surv(to_person_grain(mult_int[b, ]), p_int)
    # Bit-identical, not merely close. A layout that were only approximately
    # equal would be a second estimator, not a representation of the first.
    expect_identical(boot[b, ], ref)
    if (anyNA(ref)) {
      n_degenerate <- n_degenerate + 1L
    }
  }

  # Degenerate draws (an arm with no person drawn, or an emptied band) are in
  # the comparison too, so the NA pattern is covered and not stepped around.
  expect_gt(n_degenerate, 0L)
})

# --- assertion 6: percentile interval, not a normal approximation ----------

test_that("the interval is the percentile of the stored replicates", {
  conf_level <- 0.9
  out <- rd_out(conf_level = conf_level)
  boot <- attr(out, "rd_boot")
  expect_identical(dim(boot), c(200L, 2L))
  expect_identical(attr(out, "conf_level"), conf_level)

  # Band 4 has no comparator event, so its interval is suppressed and there is
  # no percentile to compare against; see the zero-event block below. Band 8 is
  # estimable and is what this assertion is about.
  ok <- which(out$interval_status == "ok")
  expect_equal(out$tstop[ok], 8L)

  alpha <- (1 - conf_level) / 2
  for (k in ok) {
    expect_equal(
      out$rd_lo[k],
      stats::quantile(boot[, k], alpha, na.rm = TRUE, names = FALSE)
    )
    expect_equal(
      out$rd_hi[k],
      stats::quantile(boot[, k], 1 - alpha, na.rm = TRUE, names = FALSE)
    )
  }

  # A degenerate replicate (no person drawn for an arm, or an emptied band)
  # must arrive as NA rather than as an error or a substituted number.
  expect_true(anyNA(boot))
  # The suppression is the ONLY source of a missing bound on an estimable
  # band: the percentile step drops degenerate replicates rather than
  # propagating them.
  expect_false(anyNA(out$rd_lo[ok]))
  expect_false(anyNA(out$rd_hi[ok]))
})

# --- a zero-event arm has no estimable interval ----------------------------
#
# With no positive-weight event in an arm, an ordinary empirical bootstrap can
# never produce one. Every replicate draws from the same event-free set, so
# every replicate gives that arm a failure risk of exactly zero. The interval
# then carries only the OTHER arm's sampling variation and treats this arm's
# risk as known with certainty. It is anti-conservative, and more replicates do
# not repair it, because the degeneracy is in the resampling scheme.
#
# The main fixture already carries the case, and carries it BOTH ways. The
# comparator arm has no event by band 4 and one event (weight 2) by band 8.
# Band 4 therefore has no interval and band 8 has one. That is what makes the
# condition per horizon rather than per panel.

test_that("a zero-event arm loses its interval and keeps its point estimate", {
  out <- rd_out()
  at4 <- out[tstop == 4L]

  # The witness, read off the fixture and not off the estimator: no comparator
  # event carries a positive weight through band 4.
  dt <- rd_dt()
  expect_equal(sum(dt[exposed == FALSE & tstop <= 4L, w * event]), 0)

  expect_true(is.na(at4$rd_lo))
  expect_true(is.na(at4$rd_hi))
  # The point estimate is a valid descriptive quantity and stays.
  expect_true(is.finite(at4$rd))
  expect_equal(at4$rd, 1 / 4)

  # The status names the reason, so a reader can tell "spans the null" from
  # "not estimable".
  expect_identical(at4$interval_status, "zero-event arm")

  # And the number needed to treat suppresses itself, because `.tte_nntb()`
  # returns NA whenever the interval is not strictly one-sided.
  nn <- swereg:::.tte_nntb(at4$rd, at4$rd_lo, at4$rd_hi)
  expect_true(is.na(nn$nntb))
  expect_true(is.na(nn$nntb_lo))
  expect_true(is.na(nn$nntb_hi))
  expect_identical(swereg:::.tte_nntb_cell(nn$nntb), "")
  expect_identical(
    swereg:::.tte_nntb_cell(nn$nntb, nn$nntb_lo, nn$nntb_hi),
    ""
  )
})

test_that("the zero-event condition is per horizon, not per panel", {
  out <- rd_out()
  at4 <- out[tstop == 4L]
  at8 <- out[tstop == 8L]

  dt <- rd_dt()
  # No comparator event through band 4; one comparator event, weight 2, by
  # band 8. The same arm, two horizons, two answers.
  expect_equal(sum(dt[exposed == FALSE & tstop <= 4L, w * event]), 0)
  expect_equal(sum(dt[exposed == FALSE & tstop <= 8L, w * event]), 2)

  expect_true(is.na(at4$rd_lo))
  expect_true(is.na(at4$rd_hi))
  expect_identical(at4$interval_status, "zero-event arm")

  # THE assertion a whole-panel condition cannot survive. Band 8 is estimable
  # and must keep both bounds.
  expect_false(is.na(at8$rd_lo))
  expect_false(is.na(at8$rd_hi))
  expect_true(is.finite(at8$rd_lo))
  expect_true(is.finite(at8$rd_hi))
  expect_identical(at8$interval_status, "ok")
})

test_that("the suppression reads the weights, not the raw event flag", {
  # A weight of zero removes the person-trial from both sums, so an event
  # carrying no weight is not an event this estimator can resample. Band 8 has
  # exactly one comparator event; zeroing its weight must suppress band 8 too.
  dt <- rd_dt()
  dt[exposed == FALSE & event == 1L, w := 0]
  out <- rd_out(dt, n_boot = 20L)

  expect_equal(sum(dt[exposed == FALSE, w * event]), 0)
  expect_identical(out$interval_status, c("zero-event arm", "zero-event arm"))
  expect_true(all(is.na(out$rd_lo)))
  expect_true(all(is.na(out$rd_hi)))

  # The unweighted person count still reports the person, so the two columns
  # answer different questions and neither stands in for the other.
  expect_equal(out[tstop == 8L]$n_persons_with_event_comparator, 1L)
})

test_that("both arms with events through the horizon keep the interval", {
  # The complement, so the guard cannot pass by blanking everything. Give the
  # comparator an early event and band 4 becomes estimable.
  dt <- rd_dt()
  dt[exposed == FALSE & tstop == 4L, event := c(1L, 0L, 0L)]
  out <- rd_out(dt, n_boot = 100L)

  expect_identical(out$interval_status, c("ok", "ok"))
  expect_false(anyNA(out$rd_lo))
  expect_false(anyNA(out$rd_hi))
})

# --- assertion 7: event counts are distinct persons ------------------------

test_that("event counts are distinct persons, not person-trials", {
  dt <- rd_dt()

  # The witness: p1 carries the event in two different trials, so the
  # person-trial count is 2 where the person count is 1.
  ev <- dt[event == 1L & exposed == TRUE]
  expect_equal(data.table::uniqueN(ev$enrollment_person_trial_id), 2L)
  expect_equal(data.table::uniqueN(ev$id), 1L)

  out <- rd_out(dt)
  # Cumulative distinct people with the outcome at or before the band.
  expect_equal(out[tstop == 4L]$n_persons_with_event_intervention, 1L)
  expect_equal(out[tstop == 8L]$n_persons_with_event_intervention, 1L)
  expect_equal(out[tstop == 4L]$n_persons_with_event_comparator, 0L)
  expect_equal(out[tstop == 8L]$n_persons_with_event_comparator, 1L)
})

# --- an ETT with no event at all is legitimate, not an error ---------------

test_that("no events anywhere gives zeros and no warning", {
  # A rare outcome in a small stratum can produce a panel with no event in the
  # window. data.table types an empty grouping by evaluating min() once, which
  # warns "no non-missing arguments to min"; the caller must not see that.
  dt <- rd_dt()
  dt[, event := 0L]
  out <- expect_no_warning(rd_out(dt, n_boot = 20L))
  expect_equal(out$rd, c(0, 0))
  expect_equal(out$n_persons_with_event_intervention, c(0L, 0L))
  expect_equal(out$n_persons_with_event_comparator, c(0L, 0L))

  # Both arms are event-free at every horizon, so no band has an interval.
  expect_identical(out$interval_status, c("zero-event arm", "zero-event arm"))
  expect_true(all(is.na(out$rd_lo)))
  expect_true(all(is.na(out$rd_hi)))
})

# --- equivalence: pre-aggregation is an optimisation, so it must be exact ---

test_that("the pre-aggregated point estimate equals survival_curve() exactly", {
  trial <- rd_trial()
  curve <- trial$survival_curve(weight_col = "w")
  out <- trial$risk_difference(weight_col = "w", n_boot = 20L, seed = 4L)

  ref_int <- curve[exposed == TRUE][order(tstop)]$surv
  ref_cmp <- curve[exposed == FALSE][order(tstop)]$surv
  expect_equal(out$surv_intervention, ref_int, tolerance = 1e-12)
  expect_equal(out$surv_comparator, ref_cmp, tolerance = 1e-12)
  expect_equal(out$rd, ref_cmp - ref_int, tolerance = 1e-12)
})
