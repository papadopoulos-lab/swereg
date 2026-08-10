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
  #
  # `"zero-event arm"` is the state that has NO interval. `"ok"` and
  # `"spans null"` both have one, and they differ only in where it sits
  # relative to the null, so both belong in this comparison.
  estimable <- which(out$interval_status != "zero-event arm")
  expect_equal(out$tstop[estimable], 8L)

  alpha <- (1 - conf_level) / 2
  for (k in estimable) {
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
  expect_false(anyNA(out$rd_lo[estimable]))
  expect_false(anyNA(out$rd_hi[estimable]))
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
  # The direction comes off the curve row, which is where it is stored.
  expect_identical(
    swereg:::.tte_nntb_cell(nn$nntb, nnt_direction = at4$nnt_direction),
    ""
  )
  expect_identical(
    swereg:::.tte_nntb_cell(
      nn$nntb,
      nn$nntb_lo,
      nn$nntb_hi,
      at4$nnt_direction
    ),
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
  # Band 8 HAS an interval, and that interval contains the null, so its status
  # is `"spans null"` rather than `"zero-event arm"`. The two are different
  # facts: no interval, against an interval that includes no effect.
  expect_identical(at8$interval_status, "spans null")
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

  # Four persons give a wide percentile interval, so both bands land on
  # `"spans null"`. The assertion is that neither is `"zero-event arm"`: both
  # bands HAVE an interval, which is what the guard could wrongly blank.
  expect_identical(out$interval_status, c("spans null", "spans null"))
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

# --- the statistical decisions are DATA on the curve -----------------------
#
# Three columns carry a decision that used to live only inside a formatted
# string. `interval_status` says where the interval sits. `nnt` is the signed
# number needed to treat. `nnt_direction` is the benefit-or-harm decision.
#
# The main fixture cannot reach `"ok"`. Four persons give a percentile interval
# that always contains the null, so `"ok"` would be unreachable from a real
# computation and only constructable by hand. `rd_ok_dt()` below is therefore a
# second fixture, sized so the interval strictly excludes the null.
#
# It is synthetic and deliberately blunt: 30 persons per arm, one trial each,
# unit weights, and every event at band 4. The intervention arm loses 12 of 30
# and the comparator arm 2 of 30, so RD(4) = 28/30 - 18/30 = 1/3 and
# `-1/rd` is exactly -3. Both arms carry events, so neither band is a
# zero-event arm.

rd_ok_dt <- function(ev_int = 12L, ev_cmp = 2L) {
  one_arm <- function(prefix, n, n_event, exposed) {
    ids <- sprintf("%s%02d", prefix, seq_len(n))
    band4 <- data.table::data.table(
      id = ids,
      exposed = exposed,
      tstop = 4L,
      event = c(rep(1L, n_event), rep(0L, n - n_event))
    )
    band8 <- data.table::data.table(
      id = ids[(n_event + 1L):n],
      exposed = exposed,
      tstop = 8L,
      event = 0L
    )
    rbind(band4, band8)
  }
  dt <- rbind(
    one_arm("i", 30L, ev_int, TRUE),
    one_arm("c", 30L, ev_cmp, FALSE)
  )
  dt[, enrollment_person_trial_id := id]
  dt[, w := 1]
  dt[, age := 50]
  dt[, death := 0L]
  dt[]
}

test_that("an interval that strictly excludes the null reads ok", {
  out <- rd_out(rd_ok_dt(), n_boot = 200L)

  # The witness, read off the returned bounds and not off the status column:
  # both bounds are strictly positive, so the interval excludes the null.
  expect_true(all(out$rd_lo > 0))
  expect_true(all(out$rd_hi > 0))
  expect_identical(out$interval_status, c("ok", "ok"))

  # And the point estimate is what the fixture was built to give.
  expect_equal(out$rd, c(1 / 3, 1 / 3), tolerance = 1e-12)
})

test_that("an estimable interval that contains the null reads spans null", {
  # The SAME estimator on the main fixture. Band 8 has both bounds, and they
  # straddle zero, so the third state is reachable from a real computation.
  out <- rd_out()
  at8 <- out[tstop == 8L]

  expect_true(is.finite(at8$rd_lo))
  expect_true(is.finite(at8$rd_hi))
  expect_lt(at8$rd_lo, 0)
  expect_gt(at8$rd_hi, 0)
  expect_identical(at8$interval_status, "spans null")

  # Not collapsed into "ok", and not confused with the no-interval state.
  expect_false(identical(at8$interval_status, "ok"))
  expect_false(identical(at8$interval_status, "zero-event arm"))
})

test_that("the three interval states are distinct and none is a synonym", {
  spans <- rd_out()
  none <- rd_out(rd_ok_dt(ev_int = 12L, ev_cmp = 0L), n_boot = 100L)
  strict <- rd_out(rd_ok_dt(), n_boot = 200L)

  seen <- unique(c(
    spans$interval_status,
    none$interval_status,
    strict$interval_status
  ))
  expect_setequal(seen, c("ok", "spans null", "zero-event arm"))

  # A zero-event arm has NO bounds; a spanning interval has both. That is the
  # distinction the old two-value column could not make.
  expect_true(all(is.na(none[tstop == 4L]$rd_lo)))
  expect_false(anyNA(spans[tstop == 8L]$rd_lo))
})

test_that("the curve carries the number needed to treat and its direction", {
  out <- rd_out(rd_ok_dt(), n_boot = 200L)

  expect_true(all(c("nnt", "nnt_direction") %in% names(out)))
  # RD is +1/3, so the intervention raises the risk: -1/rd is -3 and the
  # direction is harm. The value is SIGNED and stays signed.
  expect_equal(out$nnt, c(-3, -3), tolerance = 1e-12)
  expect_identical(out$nnt_direction, c("harm", "harm"))
})

test_that("mirroring the arms flips the stored direction, not just the sign", {
  # The same fixture with the arms relabelled. A stray `abs()` in the number
  # needed to treat would leave `nnt` positive on both, and only the direction
  # column would show it.
  dt <- rd_ok_dt()
  dt[, exposed := !exposed]
  out <- rd_out(dt, n_boot = 200L)

  expect_equal(out$nnt, c(3, 3), tolerance = 1e-12)
  expect_identical(out$nnt_direction, c("benefit", "benefit"))
})

test_that("every band carries a direction unless the risk difference is zero", {
  # A risk difference of exactly zero has no reciprocal and no direction, so
  # both decision columns are missing there rather than guessed.
  dt <- rd_dt()
  dt[, event := 0L]
  out <- rd_out(dt, n_boot = 20L)

  expect_equal(out$rd, c(0, 0))
  expect_identical(out$nnt, c(NA_real_, NA_real_))
  expect_identical(out$nnt_direction, c(NA_character_, NA_character_))
})

test_that("the stored direction agrees with the number needed to treat cell", {
  # The whole chain, end to end: the curve decides, the cell reads. Band 4 of
  # the strict fixture is `"ok"`, so the cell renders a label.
  out <- rd_out(rd_ok_dt(), n_boot = 200L)
  at4 <- out[tstop == 4L]

  # `.tte_nntb()` supplies the magnitude and the interval. It supplies no
  # direction, so the only direction available is the one the curve stored.
  nn <- swereg:::.tte_nntb(at4$rd, at4$rd_lo, at4$rd_hi)
  expect_false("nnt_direction" %in% names(nn))

  cell <- swereg:::.tte_nntb_cell(
    nn$nntb,
    nn$nntb_lo,
    nn$nntb_hi,
    at4$nnt_direction
  )
  expect_match(cell, "^NNTH ")
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

# --- the replicates are multiplied in batches, and the numbers do not move --
#
# The estimator multiplies `.RD_BOOT_BATCH` replicates at once, so each product
# is one level-3 BLAS call instead of that many level-2 calls. Nothing about
# the answer may move. The reference values below were MEASURED on the
# one-replicate-at-a-time estimator at commit 41544b8, and they are pinned, not
# recomputed. To regenerate them, check out that commit and run:
#
#   set.seed(4L); o <- swereg:::.tte_rd_curve(
#     data = rd_dt(), person_id_var = "id",
#     id_var = "enrollment_person_trial_id", treatment_var = "exposed",
#     time_var = "tstop", weight_col = "w", n_boot = 60L, keep_mult = TRUE)
#   dput(as.vector(attr(o, "rd_boot")),
#        control = c("keepNA", "keepInteger", "niceNames", "digits17"))
#
# The multiplicity matrices are pinned as a digit string. Four persons drawn
# four times cannot give a multiplicity above 4, so one character per cell is
# unambiguous here. `expect_lt(max(...), 10L)` checks that, rather than
# assuming it.

rd_keep_mult <- function(n_boot, seed = 4L, dt = rd_dt()) {
  set.seed(seed)
  swereg:::.tte_rd_curve(
    data = dt,
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    time_var = "tstop",
    weight_col = "w",
    n_boot = n_boot,
    keep_mult = TRUE
  )
}

mult_digits <- function(m) paste(as.vector(m), collapse = "")

test_that("batched replicates reproduce the unbatched numbers exactly", {
  out <- rd_keep_mult(60L)

  expect_identical(out$tstop, c(4L, 8L))
  expect_identical(out$surv_comparator, c(1, 0.66666666666666674))
  expect_identical(out$surv_intervention, c(0.75, 0.375))
  expect_identical(out$rd, c(0.25, 0.29166666666666674))
  expect_identical(out$rd_lo, c(NA, -0.5))
  expect_identical(out$rd_hi, c(NA, 1))
  expect_identical(out$n_persons_with_event_comparator, c(0L, 1L))
  expect_identical(out$n_persons_with_event_intervention, c(1L, 1L))

  boot <- attr(out, "rd_boot")
  expect_identical(dim(boot), c(60L, 2L))
  # The whole replicate matrix, cell for cell, including the missing cells a
  # replicate that drew no person for an arm produces.
  expect_identical(
    as.vector(boot),
    c(0, 0, 0.25, NA, 0.33333333333333326, 0.19999999999999996, 0.5, 0, 0.33333333333333326,
      0.5, 0, 0.40000000000000002, 0, 0.33333333333333326, 0, NA, 0, 0, 0,
      0, 0.33333333333333326, 0, 0.19999999999999996, 0.5, 0.33333333333333326,
      0.33333333333333326, 0.33333333333333326, 0.5, NA, 0, 0.40000000000000002,
      0.19999999999999996, 0.33333333333333326, 0.33333333333333326, NA, 0.33333333333333326,
      0, NA, 0.33333333333333326, 0.40000000000000002, 0.40000000000000002,
      NA, 0, 0.25, 0.25, 0, 0.19999999999999996, 0.33333333333333326, 0.33333333333333326,
      0, 0.25, 0.33333333333333326, 0, 0, NA, 0.19999999999999996, 0.25, 0.25,
      0, 0.33333333333333326, NA, -0.40000000000000002, 0.29166666666666674,
      NA, 0.75, 0.5, 1, -0.25, 0.27777777777777773, 1, -0.40000000000000002,
      0.80000000000000004, 0, 0.75, 0, NA, 0, 0, -0.33333333333333326, -0.5,
      0.27777777777777773, -0.25, 0.099999999999999978, 1, 0.66666666666666663,
      0.75, 0.66666666666666663, 1, NA, -0.33333333333333326, 0.66666666666666674,
      0.099999999999999978, 0.27777777777777773, 0.5, NA, 0.75, NA, NA, 0.5,
      0.66666666666666674, 0.66666666666666674, NA, -0.5, 0.5, 0.59999999999999998,
      -0.5, 0.099999999999999978, 0.75, 0.75, -0.40000000000000002, 0.59999999999999998,
      0.66666666666666663, -0.33333333333333326, 0, NA, -0.033333333333333437,
      0.5, 0.5, -0.33333333333333326, 0.27777777777777773)
  )

  mult_int <- attr(out, "mult_intervention")
  mult_cmp <- attr(out, "mult_comparator")
  expect_identical(storage.mode(mult_int), "integer")
  expect_identical(dim(mult_int), c(60L, 7L))
  expect_lt(max(mult_int), 10L)
  expect_identical(
    mult_digits(mult_int),
    paste0(
      "00101120220201040000201211122021220102222201101110",
      "11002111020010112022020104000020121112202122010222",
      "22011011101100211102011400011011102032221110101022",
      "01100002000212031001012122222132101301102001000012",
      "11200100011212014021103021211220100100113210130110",
      "20010000121120010001121201402110302121122010010011",
      "32101301102001000012112001000112120140211030212112",
      "20100100111110202202113220121002022222011000420001",
      "10011002211213001110"
    )
  )
  # One draw reaches both arms, so the two stores hold the same rows.
  expect_identical(mult_int, mult_cmp)
})

test_that("replicates are multiplied in batches of 50 rows", {
  seen <- integer(0)
  real_batch <- swereg:::.rd_surv_batch
  testthat::local_mocked_bindings(
    .rd_surv_batch = function(mult, mats) {
      seen <<- c(seen, nrow(mult))
      real_batch(mult, mats)
    },
    .package = "swereg"
  )

  out <- rd_keep_mult(137L)

  # The first two rows are the point estimate, one arm each, one replicate row
  # each. The other six are three replicate batches of 50, 50 and 37, each
  # multiplied once for the comparator arm and once for the intervention arm.
  expect_identical(seen, c(1L, 1L, 50L, 50L, 50L, 50L, 37L, 37L))
  expect_identical(dim(attr(out, "rd_boot")), c(137L, 2L))
})

test_that("a replicate count that is not a multiple of 50 gives the same numbers", {
  # 7 replicates is one partial batch and never fills a whole one.
  out7 <- rd_keep_mult(7L)
  expect_identical(out7$rd, c(0.25, 0.29166666666666674))
  expect_identical(out7$rd_lo, c(NA, -0.33083333333333331))
  expect_identical(out7$rd_hi, c(NA, 0.97500000000000009))
  boot7 <- attr(out7, "rd_boot")
  expect_identical(dim(boot7), c(7L, 2L))
  expect_identical(
    as.vector(boot7),
    c(0, 0, 0.25, NA, 0.33333333333333326, 0.19999999999999996, 0.5, NA,
      -0.40000000000000002, 0.29166666666666674, NA, 0.75, 0.5, 1)
  )
  expect_identical(
    mult_digits(attr(out7, "mult_intervention")),
    "0010112001011201140003210130321013032101301110202"
  )

  # 137 replicates is two full batches and a partial third one.
  out137 <- rd_keep_mult(137L)
  expect_identical(out137$rd, c(0.25, 0.29166666666666674))
  expect_identical(out137$rd_lo, c(NA, -0.5))
  expect_identical(out137$rd_hi, c(NA, 1))
  boot137 <- attr(out137, "rd_boot")
  expect_identical(dim(boot137), c(137L, 2L))
  # Checksums over every cell of the 137 by 2 replicate matrix, measured on the
  # one-replicate-at-a-time estimator. `sum()` reads the cells in one fixed
  # order, so the value does not depend on the batch layout.
  expect_identical(sum(boot137, na.rm = TRUE), 55.975793650793648)
  expect_identical(sum(is.na(boot137)), 35L)
  expect_identical(sum(attr(out137, "mult_intervention")), 959L)

  # A replicate does not depend on the batch it landed in. Replicates 51 to 60
  # sit in a 10-row final batch at 60 replicates, and inside a 50-row middle
  # batch at 137. Replicates 1 to 7 are a whole 7-row batch at 7 replicates,
  # and the start of a 50-row batch at 137.
  boot60 <- attr(rd_keep_mult(60L), "rd_boot")
  expect_identical(boot137[1:60, ], boot60)
  expect_identical(boot137[1:7, ], boot7)
})

# --- the whole replicate matrix, against an unbatched reference --------------
#
# The literal blocks above pin absolute values, and they pin only the replicate
# counts they name. A sum, an NA count and a percentile are all insensitive to
# row order, so a permutation of the replicate rows, or two changes that cancel,
# would pass every one of them. This block closes that gap.
#
# `rd_unbatched()` is the estimator as it multiplied ONE replicate at a time,
# before the batching change. It is self-contained: it reads no file, it parses
# no commit, and it lives beside the tests it serves. It calls
# `swereg:::.boot_person_index()` once per replicate in replicate order, which
# is the draw the batching change did not touch.
#
# The comparison is `expect_identical()` on the FULL `rd_boot` matrix and on
# BOTH full multiplicity matrices. That is order-sensitive, so no permutation
# and no cancelling pair of changes can satisfy it.
#
# The two kinds of assertion fail in different ways, so both are kept. A literal
# block catches a change that moves every replicate the same way. This block
# catches a change that moves one replicate.

rd_unbatched <- function(n_boot, seed = 4L, dt = rd_dt(), conf_level = 0.95) {
  . <- arm <- pt <- band <- num <- den <- NULL # nolint

  pt_f <- factor(dt$enrollment_person_trial_id)
  pt_code <- as.integer(pt_f)
  n_pt <- nlevels(pt_f)
  person_raw <- as.character(dt$id)
  pt_person <- factor(person_raw[match(seq_len(n_pt), pt_code)])

  band_vals <- sort(unique(dt$tstop))
  n_band <- length(band_vals)
  band_code <- match(dt$tstop, band_vals)

  tv <- dt$exposed
  w <- as.numeric(dt$w)
  ev <- dt$event

  agg <- data.table::data.table(
    arm = tv,
    pt = pt_code,
    band = band_code,
    num = w * as.numeric(ev),
    den = w
  )
  agg <- agg[, .(num = sum(num), den = sum(den)), keyby = .(arm, pt, band)]
  arm_mats <- function(sub) {
    mn <- matrix(0, nrow = n_pt, ncol = n_band)
    md <- matrix(0, nrow = n_pt, ncol = n_band)
    ij <- cbind(sub$pt, sub$band)
    mn[ij] <- sub$num
    md[ij] <- sub$den
    list(num = mn, den = md)
  }
  m_int <- arm_mats(agg[arm == TRUE])
  m_cmp <- arm_mats(agg[arm == FALSE])

  # One replicate, one matrix-vector product per arm. This is the shape the
  # batching change replaced.
  arm_surv <- function(mult, mats) {
    numerator <- as.numeric(mult %*% mats$num)
    denominator <- as.numeric(mult %*% mats$den)
    denominator[!is.finite(denominator) | denominator <= 0] <- NA_real_
    cumprod(1 - numerator / denominator)
  }

  one <- rep(1, n_pt)
  surv_int <- arm_surv(one, m_int)
  surv_cmp <- arm_surv(one, m_cmp)

  boot <- matrix(NA_real_, nrow = n_boot, ncol = n_band)
  mult_store <- matrix(0L, nrow = n_boot, ncol = n_pt)
  set.seed(seed)
  for (b in seq_len(n_boot)) {
    mult <- tabulate(swereg:::.boot_person_index(pt_person), nbins = n_pt)
    mult_store[b, ] <- as.integer(mult)
    boot[b, ] <- arm_surv(mult, m_cmp) - arm_surv(mult, m_int)
  }

  alpha <- (1 - conf_level) / 2
  pctl <- function(p) {
    apply(boot, 2L, stats::quantile, probs = p, na.rm = TRUE, names = FALSE)
  }
  rd_lo <- pctl(alpha)
  rd_hi <- pctl(1 - alpha)
  zero_event <- cumsum(colSums(m_int$num)) <= 0 | cumsum(colSums(m_cmp$num)) <= 0
  rd_lo[zero_event] <- NA_real_
  rd_hi[zero_event] <- NA_real_

  cum_persons <- function(which_arm) {
    keep <- ev == 1L & tv == which_arm
    n <- integer(n_band)
    if (any(keep)) {
      first <- tapply(band_code[keep], person_raw[keep], min)
      n <- tabulate(as.integer(first), nbins = n_band)
    }
    cumsum(n)
  }

  list(
    tstop = band_vals,
    surv_comparator = surv_cmp,
    surv_intervention = surv_int,
    rd = surv_cmp - surv_int,
    rd_lo = rd_lo,
    rd_hi = rd_hi,
    n_persons_with_event_comparator = cum_persons(FALSE),
    n_persons_with_event_intervention = cum_persons(TRUE),
    rd_boot = boot,
    mult = mult_store
  )
}

expect_matches_unbatched <- function(n_boot) {
  out <- rd_keep_mult(n_boot)
  ref <- rd_unbatched(n_boot)

  expect_identical(out$tstop, ref$tstop)
  expect_identical(out$surv_comparator, ref$surv_comparator)
  expect_identical(out$surv_intervention, ref$surv_intervention)
  expect_identical(out$rd, ref$rd)
  expect_identical(out$rd_lo, ref$rd_lo)
  expect_identical(out$rd_hi, ref$rd_hi)
  expect_identical(
    out$n_persons_with_event_comparator,
    ref$n_persons_with_event_comparator
  )
  expect_identical(
    out$n_persons_with_event_intervention,
    ref$n_persons_with_event_intervention
  )

  # The full matrices, cell for cell and row for row.
  expect_identical(attr(out, "rd_boot"), ref$rd_boot)
  expect_identical(attr(out, "mult_intervention"), ref$mult)
  expect_identical(attr(out, "mult_comparator"), ref$mult)
}

test_that("the batched estimator equals an unbatched reference at 7 replicates", {
  # Under one batch, and never fills one.
  expect_matches_unbatched(7L)
})

test_that("the batched estimator equals an unbatched reference at 50 replicates", {
  # Exactly one full batch, so the loop runs once and leaves no remainder.
  expect_matches_unbatched(50L)
})

test_that("the batched estimator equals an unbatched reference at 100 replicates", {
  # Two full batches and no remainder.
  expect_matches_unbatched(100L)
})

test_that("the batched estimator equals an unbatched reference at 137 replicates", {
  # Two full batches and a partial third one.
  expect_matches_unbatched(137L)
})
