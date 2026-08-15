# The deviation boundary is exact, and it comes from the weekly assessments.
#
# `enroll()` collapses each band to one row, and the collapse keeps `last()` of
# the band for the treatment column. Deviation used to be decided from that one
# value, so a woman's verdict followed where her weeks fell against the
# calendar grid. Two women who behaved the same way got opposite verdicts.
#
# `.tte_deviation_boundary()` now reads the weekly sequence at enrollment,
# where it still exists, and writes one integer per person-trial. The stored
# panel stays one row per person-trial-band.
#
# This file pins four properties.
#
# 1. A concordant assessment resets the tolerance run.
# 2. Censoring is at the right edge of the `(k + 1)`th consecutive discordant
#    week, and not at the start of the run.
# 3. An internal observation gap censors, whatever the tolerance is.
# 4. Each arm reads its own tolerance.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")

.ld_pw <- 4L
.ld_n_fu <- 12L

# Consecutive ISO year-weeks starting on a band boundary. Sixteen of them make
# one entry band and three follow-up bands under `period_width = 4`.
.ld_weeks <- function(n_weeks = 16L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% .ld_pw == 0L
  ]$idx[1]
  wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)]
}

# The landmark week index of the band-0 trial. Follow-up week `f` of that trial
# is week index `landmark + f - 1`, so a run that starts at follow-up week `f`
# starts at `u0 = L + f - 1`.
.ld_landmark <- function(weeks) {
  swereg:::.tte_week_index0(weeks[.ld_pw + 1L])
}

# One person, one row per week.
#
# `arm` is her assigned arm. `TRUE` is the intervention arm and `FALSE` is the
# comparator arm. `exposed` holds that arm in every week. `eligible` holds
# `TRUE` only inside the entry band, so band 0 is the only band that recruits
# her.
#
# `on_tx` is the weekly assessment. It holds her assigned arm by default, which
# is concordant. Three arguments move it, and each one names 1-indexed FOLLOW-UP
# weeks rather than rows of `weeks`.
#
# * `discordant_fu` sets the opposite arm, which is discordant.
# * `na_fu` sets `NA`, which is discordant in both arms.
# * `absent_fu` deletes the row, which is loss of observation under the
#   `row_presence` sentinel.
.ld_person <- function(
  id,
  weeks,
  arm,
  discordant_fu = integer(0),
  na_fu = integer(0),
  absent_fu = integer(0)
) {
  n <- length(weeks)
  fu <- seq_len(n) - .ld_pw
  on_tx <- rep(arm, n)
  on_tx[fu %in% discordant_fu] <- !arm
  on_tx[fu %in% na_fu] <- NA
  d <- data.table::data.table(
    id = id,
    isoyearweek = weeks,
    exposed = rep(arm, n),
    eligible = seq_len(n) <= .ld_pw,
    died = FALSE,
    on_tx = on_tx,
    age = 50 + seq_len(n)
  )
  d[!(fu %in% absent_fu)]
}

# Enough concordant fillers for the propensity model to have something to fit.
#
# Every fixture below holds at least nine intervention women and at most
# thirteen comparators. A ratio of 2 therefore requests more comparators than
# the fixture holds, so `sample()` draws EVERY one of them. Which comparator a
# test names is not a seed question.
.ld_fillers <- function(weeks, n_intervention = 8L, n_comparator = 12L) {
  data.table::rbindlist(list(
    data.table::rbindlist(lapply(
      seq_len(n_intervention),
      function(i) .ld_person(paste0("FI", i), weeks, arm = TRUE)
    )),
    data.table::rbindlist(lapply(
      seq_len(n_comparator),
      function(i) .ld_person(paste0("FC", i), weeks, arm = FALSE)
    ))
  ))
}

# Both tolerances are arguments with no default, so no fixture in this file can
# leave one at zero by accident.
.ld_design <- function(intervention_k, comparator_k) {
  TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = .ld_n_fu,
    period_width = .ld_pw,
    intervention_tolerance_weeks = intervention_k,
    comparator_tolerance_weeks = comparator_k
  )
}

.ld_enroll <- function(d, design, ratio = 2, seed = 4) {
  TTEEnrollment$new(
    data = data.table::copy(d),
    design = design,
    ratio = ratio,
    seed = seed,
    extra_cols = "isoyearweek"
  )
}

# The public route. `$s5_prepare_outcome()` and `$s6_ipcw_pp()` are private, so
# every test drives `$s4_prepare_for_analysis()` instead.
#
# The fixtures are small and deterministic, so the censoring model separates
# and warns. That warning is about the toy data and not about the boundary.
.ld_prepare <- function(trial) {
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = .ld_n_fu,
      estimand = "pp",
      estimate_ipcw_pp_with_gam = FALSE
    )
  })
  trial$data
}

# The boundary of one woman, read off the rows she keeps. It is constant across
# them, so a second value would be a defect in itself.
.ld_boundary <- function(d, who) {
  unique(d[id == who]$weeks_to_protocol_deviation)
}


# ---------------------------------------------------------------------------
# PROOF 1
# ---------------------------------------------------------------------------

test_that("a concordant week resets the tolerance run", {
  weeks <- .ld_weeks()
  # Tolerance 1 allows one discordant week. It needs two CONSECUTIVE ones to
  # censor.
  #
  # RESET is discordant in follow-up weeks 5, 7 and 9. A concordant week sits
  # between each pair, so no run ever reaches two.
  # PAIRED is discordant in follow-up weeks 5 and 6, which is a run of two.
  d <- data.table::rbindlist(list(
    .ld_person("RESET", weeks, arm = TRUE, discordant_fu = c(5L, 7L, 9L)),
    .ld_person("PAIRED", weeks, arm = TRUE, discordant_fu = c(5L, 6L)),
    .ld_fillers(weeks)
  ))

  trial <- .ld_enroll(d, .ld_design(intervention_k = 1L, comparator_k = 3L))
  out <- .ld_prepare(trial)

  # RESET never censors. She keeps all three follow-up bands.
  expect_identical(.ld_boundary(out, "RESET"), NA_integer_)
  expect_identical(nrow(out[id == "RESET"]), 3L)

  # PAIRED censors. Her run starts at follow-up week 5, which is week index
  # `L + 4`, so the boundary is `(u0 + k + 1) - L` = 4 + 1 + 1 = 6.
  #
  # The second band carries the censoring, and it is clipped to week 6. It
  # bills the 2 weeks before the boundary and nothing after.
  expect_identical(.ld_boundary(out, "PAIRED"), 6L)
  expect_identical(nrow(out[id == "PAIRED"]), 2L)
  expect_identical(out[id == "PAIRED"]$tstop, c(4L, 6L))
  expect_identical(out[id == "PAIRED"]$person_weeks, c(4L, 2L))

  # RESET holds MORE discordant weeks than PAIRED, and still does not censor.
  # A rule that counted them cumulatively would censor RESET first.
  expect_identical(sum(d[id == "RESET"]$on_tx == FALSE), 3L)
  expect_identical(sum(d[id == "PAIRED"]$on_tx == FALSE), 2L)
})


# ---------------------------------------------------------------------------
# PROOF 2
# ---------------------------------------------------------------------------

test_that("censoring is at the (k+1)th discordant week, not at the run start", {
  weeks <- .ld_weeks()
  landmark <- .ld_landmark(weeks)
  # One discordant run of three weeks, starting at follow-up week 5. With
  # tolerance 1 the boundary is the right edge of the SECOND discordant week.
  d <- data.table::rbindlist(list(
    .ld_person("RUN", weeks, arm = TRUE, discordant_fu = c(5L, 6L, 7L)),
    .ld_fillers(weeks)
  ))

  trial <- .ld_enroll(d, .ld_design(intervention_k = 1L, comparator_k = 3L))
  out <- .ld_prepare(trial)

  k <- 1L
  u0 <- landmark + 5L - 1L
  # The contract, written out rather than reduced to a number.
  expect_identical(.ld_boundary(out, "RUN"), (u0 + k + 1L) - landmark)
  # The boundary MUST sit later than the start of the run.
  expect_gt(.ld_boundary(out, "RUN"), u0 - landmark)
  # 6 falls inside the second band, so the first band is complete follow-up
  # and the second one carries the censoring. The second band is clipped to
  # week 6, and bills the 2 weeks it holds.
  expect_identical(.ld_boundary(out, "RUN"), 6L)
  expect_identical(out[id == "RUN"]$tstop, c(4L, 6L))
  expect_identical(out[id == "RUN"]$person_weeks, c(4L, 2L))
})


# ---------------------------------------------------------------------------
# PROOF 3
# ---------------------------------------------------------------------------

test_that("an internal observation gap censors and is never tolerated", {
  weeks <- .ld_weeks()
  landmark <- .ld_landmark(weeks)
  # GAP is concordant in every week she has. Her row for follow-up week 6 is
  # deleted, and her rows for follow-up weeks 7 to 12 are present. Under the
  # `row_presence` sentinel that absent row is loss of observation.
  #
  # WHOLE is the same woman with no deleted row. She shows that the fixture
  # censors for the gap and for nothing else.
  d <- data.table::rbindlist(list(
    .ld_person("GAP", weeks, arm = TRUE, absent_fu = 6L),
    .ld_person("WHOLE", weeks, arm = TRUE),
    .ld_fillers(weeks)
  ))
  # The gap is one week wide, and the band around it survives with three of
  # its four weeks. A band-level read therefore still sees that band.
  expect_identical(nrow(d[id == "GAP"]), nrow(d[id == "WHOLE"]) - 1L)

  # Tolerance 2 for her arm. It applies to discordance, and never to loss of
  # observation.
  trial <- .ld_enroll(d, .ld_design(intervention_k = 2L, comparator_k = 4L))
  out <- .ld_prepare(trial)

  # Follow-up stops at the start of the absent week, which is `u - L` for the
  # first absent week `u`.
  expect_identical(.ld_boundary(out, "GAP"), (landmark + 6L - 1L) - landmark)
  expect_identical(.ld_boundary(out, "GAP"), 5L)
  # 5 falls one week into the second band, which is clipped there and bills
  # that one week.
  expect_identical(nrow(out[id == "GAP"]), 2L)
  expect_identical(out[id == "GAP"]$tstop, c(4L, 5L))
  expect_identical(out[id == "GAP"]$person_weeks, c(4L, 1L))

  # WHOLE keeps every band, so the fixture censors GAP for the gap alone.
  expect_identical(.ld_boundary(out, "WHOLE"), NA_integer_)
  expect_identical(nrow(out[id == "WHOLE"]), 3L)
  expect_identical(out[id == "WHOLE"]$person_weeks, c(4L, 4L, 4L))
})


# ---------------------------------------------------------------------------
# PROOF 4
# ---------------------------------------------------------------------------

test_that("each arm uses its own tolerance", {
  weeks <- .ld_weeks()
  landmark <- .ld_landmark(weeks)
  # ONARM and OFFARM hold the identical discordant pattern, each read against
  # her own arm: three consecutive discordant weeks from follow-up week 7.
  # ONARM is an initiator and OFFARM is a comparator.
  d <- data.table::rbindlist(list(
    .ld_person("ONARM", weeks, arm = TRUE, discordant_fu = c(7L, 8L, 9L)),
    .ld_person("OFFARM", weeks, arm = FALSE, discordant_fu = c(7L, 8L, 9L)),
    .ld_fillers(weeks)
  ))

  trial <- .ld_enroll(d, .ld_design(intervention_k = 0L, comparator_k = 2L))
  out <- .ld_prepare(trial)

  # Every comparator is drawn, so OFFARM is in the panel by construction.
  expect_true("OFFARM" %in% out$id)

  u0 <- landmark + 7L - 1L
  # Intervention tolerance 0 censors at the right edge of the first discordant
  # week. Comparator tolerance 2 censors at the right edge of the third.
  expect_identical(.ld_boundary(out, "ONARM"), (u0 + 0L + 1L) - landmark)
  expect_identical(.ld_boundary(out, "OFFARM"), (u0 + 2L + 1L) - landmark)
  expect_identical(.ld_boundary(out, "ONARM"), 7L)
  expect_identical(.ld_boundary(out, "OFFARM"), 9L)

  # The two boundaries fall in different bands, so the retained follow-up
  # differs as well as the reported week. Each terminal band is clipped at its
  # own boundary, so the two also bill different person-time.
  expect_identical(out[id == "ONARM"]$tstop, c(4L, 7L))
  expect_identical(out[id == "ONARM"]$person_weeks, c(4L, 3L))
  expect_identical(out[id == "OFFARM"]$tstop, c(4L, 8L, 9L))
  expect_identical(out[id == "OFFARM"]$person_weeks, c(4L, 4L, 1L))
})


# ---------------------------------------------------------------------------
# Supporting behaviour, tested and not mutation-proven
# ---------------------------------------------------------------------------

test_that("the five weekly patterns each get their own exact boundary", {
  # The five patterns that the band-collapsed read decided from `last()` of the
  # band. Each one occupies follow-up weeks 5 to 8, which is the second band.
  # The premise of the table is zero tolerance, so the intervention arm reads
  # zero here. The comparator arm reads 3, and no comparator in the fixture is
  # ever discordant.
  weeks <- .ld_weeks()
  d <- data.table::rbindlist(list(
    # {T,T,F,F}: first discordant week is follow-up week 7.
    .ld_person("TTFF", weeks, arm = TRUE, discordant_fu = c(7L, 8L)),
    # {T,F,T,T}: the mid-band switch the collapsed read could not see.
    .ld_person("TFTT", weeks, arm = TRUE, discordant_fu = 6L),
    # {T,F,F,T}: two discordant weeks the collapsed read could not see.
    .ld_person("TFFT", weeks, arm = TRUE, discordant_fu = c(6L, 7L)),
    # {T,NA,T,T}: `NA` is discordant.
    .ld_person("TNATT", weeks, arm = TRUE, na_fu = 6L),
    # {T,T,T,NA}: the collapsed read saw this one, and it still censors.
    .ld_person("TTTNA", weeks, arm = TRUE, na_fu = 8L),
    .ld_fillers(weeks)
  ))

  trial <- .ld_enroll(d, .ld_design(intervention_k = 0L, comparator_k = 3L))
  out <- .ld_prepare(trial)

  # Every one of the five censors, and the week is the right edge of her own
  # first discordant week.
  expect_identical(.ld_boundary(out, "TTFF"), 7L)
  expect_identical(.ld_boundary(out, "TFTT"), 6L)
  expect_identical(.ld_boundary(out, "TFFT"), 6L)
  expect_identical(.ld_boundary(out, "TNATT"), 6L)
  expect_identical(.ld_boundary(out, "TTTNA"), 8L)

  # The second band reaches every one of the five boundaries, so all five keep
  # the first band and a second band clipped at their own week. The clipped
  # width is what separates them: four boundaries fall inside the band, and
  # TTTNA sits on its edge.
  billed <- list(
    TTFF = c(4L, 3L),
    TFTT = c(4L, 2L),
    TFFT = c(4L, 2L),
    TNATT = c(4L, 2L),
    TTTNA = c(4L, 4L)
  )
  for (who in names(billed)) {
    expect_identical(out[id == who]$tstop, c(4L, .ld_boundary(out, who)))
    expect_identical(out[id == who]$person_weeks, billed[[who]])
  }
})

test_that("ITT keeps follow-up through a discordant run", {
  weeks <- .ld_weeks()
  d <- data.table::rbindlist(list(
    .ld_person("SWITCH", weeks, arm = TRUE, discordant_fu = c(5L, 6L, 7L)),
    .ld_fillers(weeks)
  ))

  trial <- .ld_enroll(d, .ld_design(intervention_k = 1L, comparator_k = 3L))
  # ITT never censors at a switch, so the carried boundary MUST drop out.
  expect_identical(unique(trial$data[id == "SWITCH"]$weeks_to_protocol_deviation), 6L)
  trial$s2_ipw(stabilize = TRUE)
  trial$s4_prepare_for_analysis(
    outcome = "died",
    follow_up = .ld_n_fu,
    estimand = "itt"
  )
  out <- trial$data
  expect_identical(.ld_boundary(out, "SWITCH"), NA_integer_)
  expect_identical(nrow(out[id == "SWITCH"]), 3L)
})

test_that("a trial panel built outside enroll() keeps the band-collapsed read", {
  # A caller who hands in trial-level data has no weekly sequence to read, so
  # `s5_prepare_outcome()` falls back to the collapsed treatment value. That is
  # what every release before this one did, on every panel.
  d <- data.table::CJ(
    enrollment_person_trial_id = 1:40,
    tstop = seq_len(4L)
  )
  d[, tstart := tstop - 1L]
  d[, exposed := enrollment_person_trial_id <= 20L]
  d[, on_tx := exposed]
  d[, died := 0L]
  d[, person_weeks := 1L]
  d[, age := (enrollment_person_trial_id %% 5L) - 2]
  d[enrollment_person_trial_id == 1L & tstop == 3L, on_tx := FALSE]

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = 4L
  )
  trial <- TTEEnrollment$new(d, design)
  expect_false("weeks_to_protocol_deviation" %in% names(trial$data))

  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = 4L,
      estimand = "pp",
      estimate_ipcw_pp_with_gam = FALSE
    )
  })
  out <- trial$data
  # The band is one week wide here, so the collapsed value and the weekly
  # value agree. Person-trial 1 deviates at band 3, and band 3 carries her
  # censoring. A one-week band cannot be clipped, so it bills its whole week.
  keep <- out[enrollment_person_trial_id == 1L]
  expect_identical(unique(keep$weeks_to_protocol_deviation), 3L)
  expect_identical(keep$tstop, c(1L, 2L, 3L))
  expect_identical(keep$person_weeks, c(1L, 1L, 1L))
  expect_identical(keep$censor_this_period, c(0L, 0L, 1L))
  expect_identical(sum(out$censor_this_period), 1L)
})
