# Numbers at risk count PERSONS, not rows and not weights.
#
# Sequential target trial emulation enrols the same person into many trials, so
# the analysis panel holds one row per person-trial-band. Three different
# numbers therefore live in the same arm-band cell and are easy to confuse:
#
#   .N                     rows      = person-trials in the band
#   sum(w)                 at_risk   = the WEIGHTED risk set, the hazard denominator
#   uniqueN(person_id)     persons   = the head count a risk table reports
#
# On a large national-registry panel the first two exceed the third:
# person-trials outnumber persons. `$survival_curve()` therefore returns
# `n_persons_at_risk` alongside `at_risk`, and this file pins that it is the
# third quantity and neither of the other two.
#
# Fixture: the canonical 9-row panel from test-tte_classes.R, with the weights
# changed and a person column added. Three people hold five trials:
#
#   p1 -> p1_trialA, p1_trialB      (exposed)
#   p2 -> p2_trialA                 (exposed)
#   p3 -> p3_trialC, p3_trialD      (unexposed)
#
# so in three of the four arm-bands the person count is strictly below the row
# count, and the person counts c(1, 1, 2, 2) are NOT a constant offset from the
# row counts c(2, 2, 3, 2). A bug that returned "rows minus one" would survive a
# fixture where every band differed by the same amount; it does not survive this
# one.
#
# Hand-computed, and unchanged from the canonical fixture despite the reweight,
# because Kaplan-Meier is invariant to rescaling the weights within an arm:
#   S_TRUE(4) = 2/3, S_TRUE(8) = 1/3, S_FALSE(4) = 1, S_FALSE(8) = 1/2.

skip_if_not_installed("data.table")

# --- fixture ---------------------------------------------------------------

# Row order is (band 4, then band 8). `id` carries the person; each person holds
# one or two trials, so `id` and `enrollment_person_trial_id` are genuinely
# different columns and cannot collapse into one another.
at_risk_trial <- function() {
  dt <- data.table::data.table(
    enrollment_person_trial_id = c(
      "p1_trialA",
      "p1_trialB",
      "p2_trialA",
      "p3_trialC",
      "p3_trialD",
      "p1_trialA",
      "p2_trialA",
      "p3_trialC",
      "p3_trialD"
    ),
    id = c("p1", "p1", "p2", "p3", "p3", "p1", "p2", "p3", "p3"),
    exposed = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    tstop = c(4L, 4L, 4L, 4L, 4L, 8L, 8L, 8L, 8L),
    event = c(0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L),
    w = c(0.5, 0.5, 0.5, 2, 2, 0.5, 0.5, 2, 2),
    age = 50,
    death = 0L
  )
  design <- swereg::TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )
  # data_level is auto-detected as "trial" from the presence of both id columns.
  swereg::TTEEnrollment$new(dt, design)
}

# --- the default that makes the person id always present --------------------

test_that("TTEDesign defaults person_id_var to id", {
  design <- swereg::TTEDesign$new(
    treatment_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )
  expect_equal(design$person_id_var, "id")
})

# --- n_persons_at_risk is a head count, not a weight ------------------------

test_that("survival_curve returns an integer person count distinct from the weighted risk set", {
  curve <- at_risk_trial()$survival_curve(weight_col = "w")

  expect_type(curve$n_persons_at_risk, "integer")
  expect_false(isTRUE(all.equal(
    as.numeric(curve$n_persons_at_risk),
    curve$at_risk
  )))
})

# --- n_persons_at_risk is a head count, not a row count ---------------------

test_that("n_persons_at_risk counts persons, not person-trial rows", {
  trial <- at_risk_trial()
  curve <- trial$survival_curve(weight_col = "w")

  # keyby (exposed, tstop) gives FALSE/4, FALSE/8, TRUE/4, TRUE/8.
  expect_equal(curve$exposed, c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(curve$tstop, c(4L, 8L, 4L, 8L))

  expect_equal(curve$n_persons_at_risk, c(1L, 1L, 2L, 2L))

  # The row counts in the same four cells, for contrast. Three of the four
  # differ from the person counts, and the offsets are not constant.
  rows_per_cell <- trial$extract()[, .N, keyby = .(exposed, tstop)]$N
  expect_equal(rows_per_cell, c(2L, 2L, 3L, 2L))
})

# --- adding a column removes nothing ----------------------------------------

test_that("survival_curve keeps every pre-existing column and its Kaplan-Meier values", {
  curve <- at_risk_trial()$survival_curve(weight_col = "w")

  expect_true(all(
    c("exposed", "tstop", "events", "at_risk", "hazard", "surv") %in%
      names(curve)
  ))

  expect_equal(curve[exposed == TRUE & tstop == 4L, surv], 2 / 3)
  expect_equal(curve[exposed == TRUE & tstop == 8L, surv], 1 / 3)
  expect_equal(curve[exposed == FALSE & tstop == 4L, surv], 1)
  expect_equal(curve[exposed == FALSE & tstop == 8L, surv], 1 / 2)
})
