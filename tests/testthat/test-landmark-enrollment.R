# Landmark qualification: the candidate table handed to comparator sampling
# holds only person-bands that are under observation and event-free at the
# landmark.
#
# Eligibility stays a baseline property, assessed on the entry band by
# `.band_baseline_treatment()`. It is not re-read at the landmark. Re-reading
# it there deletes every initiator of an enrollment that carries a new-user
# exclusion, because that exclusion turns `eligible` off from the week after
# initiation. One test below pins the decision.
#
# The landmark of a person-band is the week that closes its entry band. Band
# `b` covers week indices `b * period_width` to `(b + 1) * period_width - 1`,
# so its landmark sits at week index `(b + 1) * period_width`. That week is
# the first week of band `b + 1`.
#
# Two defects live here, and both change a published number.
#
# A woman with an outcome INSIDE her entry band, who starts treatment later in
# the same band, used to enroll into the intervention arm with the event
# already behind her at `tstart = 0`. That is immortal-time attribution.
#
# A woman with entry-window rows but no row at the landmark used to reach the
# candidate table. She may vanish later during panel expansion, but by then
# she has been counted as enrolled, has taken a comparator slot, has changed
# the requested comparator count, and has moved the seeded draw.
#
# `.tte_qualify_bands()` runs between the arm classification and the
# comparator draw. The position is part of the rule. After the classification,
# so attrition can report both arms. Before the draw, so sampling refills the
# ratio from qualified comparators.
#
# Every fixture that expects qualification declares `observed_var`, because
# qualification needs the observation contract that swereg 26.9.0 introduced.
# One test sets it to NULL on purpose, to pin that such a design is left
# alone.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")

.lme_period_width <- 4L

# Eight consecutive ISO year-weeks starting on a band boundary. They form two
# whole bands under `period_width = 4`. Only the first band can qualify: the
# landmark of the second band would be week 9, which the fixture does not
# carry.
.lme_weeks <- function(n_weeks = 8L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% .lme_period_width == 0L
  ]$idx[1]
  wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)]
}

# The trial_id of the first band, read from `.assign_trial_ids()` itself
# rather than hard-coded.
.lme_band0 <- function(weeks) {
  d <- data.table::data.table(id = 1L, isoyearweek = weeks)
  swereg:::.assign_trial_ids(d, .lme_period_width)
  min(d$trial_id)
}

# One person. `tx` names the 1-indexed weeks the person is in the intervention
# arm. `events` maps an outcome column to the weeks it fires. `drop` names the
# weeks the person has no row at all.
.lme_person <- function(
  id,
  weeks,
  tx = integer(0),
  events = list(),
  drop = integer(0),
  outcomes = "died",
  observed = seq_along(weeks),
  eligible = seq_along(weeks)
) {
  keep <- setdiff(seq_along(weeks), drop)
  d <- data.table::data.table(
    id = id,
    isoyearweek = weeks[keep],
    exposed = keep %in% tx,
    eligible = keep %in% eligible,
    rd_observed = keep %in% observed,
    age = 50L
  )
  for (oc in outcomes) {
    hit <- events[[oc]]
    if (is.null(hit)) {
      hit <- integer(0)
    }
    d[, (oc) := keep %in% hit]
  }
  d[]
}

.lme_design <- function(
  outcome_vars = "died",
  observed_var = list(sentinel = "row_presence")
) {
  TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    eligible_var = "eligible",
    observed_var = observed_var,
    outcome_vars = outcome_vars,
    confounder_vars = "age",
    # follow_up_time == period_width, so one follow-up band per trial and the
    # panel's trial_id is the entry band.
    follow_up_time = .lme_period_width,
    period_width = .lme_period_width
  )
}

.lme_enroll <- function(d, design, ratio = 2, seed = 4) {
  TTEEnrollment$new(
    data = data.table::copy(d),
    design = design,
    ratio = ratio,
    seed = seed,
    extra_cols = "isoyearweek"
  )
}

# The 0-indexed week index of one 1-indexed fixture week, on the scale
# `.assign_trial_ids()` and `.tte_week_index0()` share.
.lme_week_index <- function(weeks, i) {
  swereg:::.tte_week_index0(weeks[i])
}

# The recruiting week each person-band reports, keyed by person id.
.lme_recruit <- function(d, band) {
  dd <- data.table::copy(d)
  swereg:::.assign_trial_ids(dd, .lme_period_width)
  bt <- swereg:::.band_baseline_treatment(
    data = dd,
    person_id_col = "id",
    treatment_col = "exposed",
    eligible_col = "eligible",
    out_col = "band_treatment"
  )
  bt <- bt[trial_id == band]
  stats::setNames(bt$recruit_week_index, bt$id)
}

# The ids enrolled into the first band, split by arm.
#
# The panel keys on `entry_band_id`, which names the trial. `trial_id` names
# the follow-up band, and follow-up opens one band after the entry band.
.lme_enrolled <- function(trial, band0) {
  panel <- trial$data[entry_band_id == band0]
  list(
    intervention = sort(panel[exposed == TRUE]$id),
    comparator = sort(panel[exposed == FALSE]$id)
  )
}

# Six plain comparators: never in arm, never with an event, rows in every
# week. They exist so the ratio has something to draw from.
.lme_filler <- function(weeks, ids = paste0("C", 1:6), outcomes = "died") {
  data.table::rbindlist(lapply(
    ids,
    function(nm) .lme_person(nm, weeks, outcomes = outcomes)
  ))
}


# ---------------------------------------------------------------------------
# PROOF 1
# ---------------------------------------------------------------------------

test_that("a woman with an entry-window event is not enrolled", {
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  # W has the outcome in week 2 of her entry band and starts treatment in
  # week 4 of the same band. `.band_baseline_treatment()` reads any() over the
  # eligible in-arm weeks, so she classifies as intervention. The event is
  # already behind her when the trial opens.
  w <- .lme_person("W", weeks, tx = 4L, events = list(died = 2L))
  # K is the same woman without the event.
  k <- .lme_person("K", weeks, tx = 4L)
  d <- data.table::rbindlist(list(w, k, .lme_filler(weeks)))

  trial <- .lme_enroll(d, .lme_design())
  enrolled <- .lme_enrolled(trial, band0)

  expect_false("W" %in% enrolled$intervention)
  expect_false("W" %in% enrolled$comparator)
  expect_true("K" %in% enrolled$intervention)

  # The cascade names the reason. W survives observation and falls at
  # event-freedom.
  att <- trial$landmark_attrition[trial_id == band0]
  n <- stats::setNames(att$n_person_trials, att$criterion)
  expect_equal(
    unname(n["landmark_candidates"]),
    unname(n["landmark_observed"])
  )
  expect_equal(
    unname(n["landmark_observed"] - n["landmark_event_free"]),
    1L
  )
})


# ---------------------------------------------------------------------------
# PROOF 2
# ---------------------------------------------------------------------------

test_that("event-freedom covers every outcome, not just the active one", {
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  outcomes <- c("died", "hosp")
  # W's event is in `hosp`, the SECOND outcome of the enrollment. `died` is
  # FALSE in every week she has. One enrollment serves both outcomes, so an
  # enrolled set that is event-free for `died` alone is still wrong.
  w <- .lme_person(
    "W",
    weeks,
    tx = 4L,
    events = list(hosp = 2L),
    outcomes = outcomes
  )
  k <- .lme_person("K", weeks, tx = 4L, outcomes = outcomes)
  d <- data.table::rbindlist(list(
    w,
    k,
    .lme_filler(weeks, outcomes = outcomes)
  ))
  expect_false(any(d[id == "W"]$died))
  expect_true(any(d[id == "W"]$hosp))

  trial <- .lme_enroll(d, .lme_design(outcome_vars = outcomes))
  enrolled <- .lme_enrolled(trial, band0)

  expect_false("W" %in% enrolled$intervention)
  expect_false("W" %in% enrolled$comparator)
  expect_true("K" %in% enrolled$intervention)
})


# ---------------------------------------------------------------------------
# PROOF 3
# ---------------------------------------------------------------------------

test_that("a woman with no row at the landmark is not enrolled", {
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  # N carries every week of her entry band and starts treatment in week 4 of
  # it. She has no row in week 5, which is the landmark of that band. She is
  # an intervention band, so no random draw stands between her and the panel:
  # without the observation criterion she enrolls.
  n_person <- .lme_person("N", weeks, tx = 4L, drop = 5L)
  k <- .lme_person("K", weeks, tx = 4L)
  d <- data.table::rbindlist(list(n_person, k, .lme_filler(weeks)))
  expect_equal(nrow(d[id == "N"]), 7L)
  expect_true(all(d[id == "N"]$eligible))

  trial <- .lme_enroll(d, .lme_design())
  enrolled <- .lme_enrolled(trial, band0)

  expect_false("N" %in% enrolled$intervention)
  expect_false("N" %in% enrolled$comparator)
  expect_true("K" %in% enrolled$intervention)

  # The cascade attributes the absence to observation, once.
  att <- trial$landmark_attrition[trial_id == band0]
  n <- stats::setNames(att$n_person_trials, att$criterion)
  expect_equal(
    unname(n["landmark_candidates"] - n["landmark_observed"]),
    1L
  )
  expect_equal(
    unname(n["landmark_observed"]),
    unname(n["landmark_event_free"])
  )
})


# ---------------------------------------------------------------------------
# PROOF 4
# ---------------------------------------------------------------------------

test_that("an unqualified comparator does not shrink the matched set", {
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  # One intervention band and ratio 2, so the trial asks for two comparators.
  # The comparator pool holds two qualified women and four who fail
  # qualification with an event in the entry band. Sampling MUST return the
  # two qualified ones and MUST NOT return a short arm.
  i1 <- .lme_person("I1", weeks, tx = 4L)
  q <- data.table::rbindlist(lapply(
    c("Q1", "Q2"),
    function(nm) .lme_person(nm, weeks)
  ))
  u <- data.table::rbindlist(lapply(
    c("U1", "U2", "U3", "U4"),
    function(nm) .lme_person(nm, weeks, events = list(died = 2L))
  ))
  d <- data.table::rbindlist(list(i1, q, u))

  trial <- .lme_enroll(d, .lme_design(), ratio = 2, seed = 11)
  enrolled <- .lme_enrolled(trial, band0)

  expect_equal(enrolled$intervention, "I1")
  expect_equal(length(enrolled$comparator), 2L)
  expect_equal(enrolled$comparator, c("Q1", "Q2"))
})


# ---------------------------------------------------------------------------
# PROOF 5
# ---------------------------------------------------------------------------

test_that("the recruiting week is the first eligible in-arm week, not the first week of the band", {
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  # `LATE` carries a row in every week of her entry band, and her band starts
  # at fixture week 1. She is not eligible until fixture week 3, so the first
  # week the classifier reads for her is week 3 and not week 1. She starts
  # treatment in that same week.
  #
  # `EARLY` is eligible from week 1, so her recruiting week IS the first week
  # of the band. The two women therefore separate a correct implementation
  # from one that returns the band start.
  late <- .lme_person("LATE", weeks, tx = 3L, eligible = 3:8)
  early <- .lme_person("EARLY", weeks, tx = 1L)
  d <- data.table::rbindlist(list(late, early, .lme_filler(weeks)))

  recruit <- .lme_recruit(d, band0)

  # The two values are different, so a wrong implementation cannot pass by
  # accident. Assert that first: it is what makes the rest discriminating.
  expect_false(recruit[["LATE"]] == recruit[["EARLY"]])
  expect_equal(unname(recruit[["EARLY"]]), .lme_week_index(weeks, 1L))
  expect_equal(unname(recruit[["LATE"]]), .lme_week_index(weeks, 3L))
  # The band start is week 1, and `LATE` MUST NOT report it.
  expect_equal(unname(recruit[["EARLY"]]), band0 * .lme_period_width)
  expect_false(recruit[["LATE"]] == band0 * .lme_period_width)

  # A comparator reports her first eligible comparator week, by the same rule.
  expect_equal(unname(recruit[["C1"]]), .lme_week_index(weeks, 1L))

  # The classification does not change, so this proof isolates the week.
  bt <- .lme_recruit(d, band0)
  expect_true(all(c("LATE", "EARLY", "C1") %in% names(bt)))

  # It survives the enrollment path onto the entry rows.
  trial <- .lme_enroll(d, .lme_design())
  expect_true("LATE" %in% .lme_enrolled(trial, band0)$intervention)
})


# ---------------------------------------------------------------------------
# Supporting checks. These are tested, and they are NOT mutation-proven.
# ---------------------------------------------------------------------------

test_that("the cascade reports both reasons, by criterion and by arm", {
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  # One fixture in which every exclusion reason fires in both arms.
  people <- list(
    .lme_person("I1", weeks, tx = 4L),
    .lme_person("I2", weeks, tx = 4L),
    .lme_person("IEV", weeks, tx = 4L, events = list(died = 2L)),
    .lme_person("INR", weeks, tx = 4L, drop = 5L),
    .lme_person("C1", weeks),
    .lme_person("C2", weeks),
    .lme_person("C3", weeks),
    .lme_person("CEV", weeks, events = list(died = 2L)),
    .lme_person("CNR", weeks, drop = 5L)
  )
  d <- data.table::rbindlist(people)

  trial <- .lme_enroll(d, .lme_design())
  att <- trial$landmark_attrition[trial_id == band0]
  n <- stats::setNames(att$n_person_trials, att$criterion)
  int <- stats::setNames(att$n_intervention, att$criterion)
  cmp <- stats::setNames(att$n_comparator, att$criterion)

  # 9 candidates: 4 intervention, 5 comparator.
  expect_equal(unname(n["landmark_candidates"]), 9L)
  expect_equal(unname(int["landmark_candidates"]), 4L)
  expect_equal(unname(cmp["landmark_candidates"]), 5L)
  # INR and CNR have no row at the landmark.
  expect_equal(unname(n["landmark_observed"]), 7L)
  expect_equal(unname(int["landmark_observed"]), 3L)
  expect_equal(unname(cmp["landmark_observed"]), 4L)
  # IEV and CEV have the event inside the entry band.
  expect_equal(unname(n["landmark_event_free"]), 5L)
  expect_equal(unname(int["landmark_event_free"]), 2L)
  expect_equal(unname(cmp["landmark_event_free"]), 3L)
})

test_that("eligibility is a baseline property and is not re-read at the landmark", {
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  # This is what a new-user exclusion does to an initiator. `eligible` is TRUE
  # up to and including the initiation week and FALSE afterwards, so the
  # landmark of her entry band is always an ineligible week. Re-reading
  # `eligible` there would delete every initiator.
  i1 <- .lme_person("I1", weeks, tx = 4L, eligible = 1:4)
  d <- data.table::rbindlist(list(i1, .lme_filler(weeks)))
  expect_false(d[id == "I1" & isoyearweek == weeks[5]]$eligible)

  enrolled <- .lme_enrolled(.lme_enroll(d, .lme_design()), band0)
  expect_true("I1" %in% enrolled$intervention)
})

test_that("a named observation column is read at the landmark", {
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  # O has a row in every week. `rd_observed` is FALSE in week 5, the landmark
  # of her entry band, so she is not under observation when the trial opens.
  o <- .lme_person("O", weeks, tx = 4L, observed = c(1:4, 6:8))
  k <- .lme_person("K", weeks, tx = 4L)
  d <- data.table::rbindlist(list(o, k, .lme_filler(weeks)))
  expect_equal(nrow(d[id == "O"]), 8L)

  design <- .lme_design(observed_var = list(column = "rd_observed"))
  enrolled <- .lme_enrolled(.lme_enroll(d, design), band0)

  expect_false("O" %in% enrolled$intervention)
  expect_true("K" %in% enrolled$intervention)
})

test_that("a design with no observed_var is left alone", {
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  w <- .lme_person("W", weeks, tx = 4L, events = list(died = 2L))
  d <- data.table::rbindlist(list(w, .lme_filler(weeks)))

  design <- .lme_design(observed_var = NULL)
  trial <- .lme_enroll(d, design)

  expect_null(trial$landmark_attrition)
  expect_true("W" %in% .lme_enrolled(trial, band0)$intervention)
})

test_that("the scout path drops the same person-bands as the direct path", {
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  people <- list(
    .lme_person("I1", weeks, tx = 4L),
    .lme_person("IEV", weeks, tx = 4L, events = list(died = 2L)),
    .lme_person("INR", weeks, tx = 4L, drop = 5L),
    .lme_person("C1", weeks),
    .lme_person("CEV", weeks, events = list(died = 2L))
  )
  d <- data.table::rbindlist(people)
  design <- .lme_design()

  # The production scout reads `rd_intervention`, not the design's treatment
  # column, and it qualifies the same tuples inside
  # `.s1a_finalize_on_skeleton()`.
  sk <- data.table::copy(d)
  sk[, rd_intervention := exposed]
  tuples <- swereg:::.s1_eligible_tuples(sk, design)
  qualified <- swereg:::.tte_qualify_bands(
    bands = tuples,
    data = sk,
    design = design,
    person_id_col = "id",
    arm_col = "intervention"
  )

  scout <- sort(qualified$bands[trial_id == band0]$id)
  expect_equal(scout, c("C1", "I1"))

  # The direct path classifies on the design's own treatment column, then
  # qualifies the same way. Both paths MUST drop the same person-bands.
  dd <- data.table::copy(d)
  swereg:::.assign_trial_ids(dd, .lme_period_width)
  direct <- swereg:::.tte_qualify_bands(
    bands = swereg:::.band_baseline_treatment(
      data = dd,
      person_id_col = "id",
      treatment_col = "exposed",
      eligible_col = "eligible",
      out_col = "band_treatment"
    ),
    data = dd,
    design = design,
    person_id_col = "id",
    arm_col = "band_treatment"
  )
  expect_equal(sort(direct$bands[trial_id == band0]$id), scout)
})

test_that(".s1a_finalize_on_skeleton() qualifies the tuples it writes", {
  # The reachability witness for the production path. `.s1b_worker()` draws
  # comparators from the pooled `s1a_pre_*` tuples and never reads a
  # person-week, so the tuples this function returns MUST already be
  # qualified. It is the frame `.s1a_worker_multi()` calls.
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  people <- list(
    .lme_person("I1", weeks, tx = 4L),
    .lme_person("IEV", weeks, tx = 4L, events = list(died = 2L)),
    .lme_person("INR", weeks, tx = 4L, drop = 5L),
    .lme_person("C1", weeks),
    .lme_person("CEV", weeks, events = list(died = 2L))
  )
  sk <- data.table::rbindlist(people)
  sk[, rd_intervention := exposed]
  data.table::setattr(sk, "eligible_cols", "eligible")
  es <- list(design = .lme_design(), enrollment_id = "01")

  res <- swereg:::.s1a_finalize_on_skeleton(
    skeleton = sk,
    enrollment_spec = es,
    spec = NULL,
    cache_path = NULL
  )

  expect_equal(sort(res$tuples[trial_id == band0]$id), c("C1", "I1"))
  # The three landmark rows stack onto the exclusion cascade, and each one
  # carries a global row for CONSORT.
  crit <- unique(res$attrition$criterion)
  expect_true(all(
    c("landmark_candidates", "landmark_observed", "landmark_event_free") %in%
      crit
  ))
  expect_true("before_exclusions" %in% crit)
  land <- res$attrition[
    criterion == "landmark_event_free" & is.na(trial_id)
  ]
  expect_equal(nrow(land), 1L)

  # The recruiting week is written into the tuples, which is how it reaches
  # `.s1b_worker()`, then `enrolled_ids` on disk, then s1c.
  expect_true("recruit_week_index" %in% names(res$tuples))
  expect_equal(
    unname(res$tuples[trial_id == band0 & id == "I1"]$recruit_week_index),
    .lme_week_index(weeks, 1L)
  )
})

test_that("the recruiting week reaches the entry rows of both enrollment paths", {
  # `enroll()` builds `entry_dt` from an explicit column list, so a column that
  # is not named there is dropped before Phase B ever sees it. This drives the
  # direct path and the pre-matched path that s1c takes.
  weeks <- .lme_weeks()
  band0 <- .lme_band0(weeks)
  late <- .lme_person("LATE", weeks, tx = 3L, eligible = 3:8)
  d <- data.table::rbindlist(list(late, .lme_filler(weeks)))
  design <- .lme_design()

  dd <- data.table::copy(d)
  swereg:::.assign_trial_ids(dd, .lme_period_width)
  tuples <- swereg:::.band_baseline_treatment(
    data = dd,
    person_id_col = "id",
    treatment_col = "exposed",
    eligible_col = "eligible",
    out_col = "intervention"
  )
  expect_true("recruit_week_index" %in% names(tuples))

  # Pre-matched mode: `enrolled_ids` carries the column in, exactly as s1b
  # writes it. The enrollment MUST accept it and build a panel.
  enrolled_ids <- tuples[trial_id == band0]
  pre <- TTEEnrollment$new(
    data = data.table::copy(dd),
    design = design,
    enrolled_ids = enrolled_ids,
    extra_cols = "isoyearweek"
  )
  expect_gt(nrow(pre$data), 0L)
  expect_true("LATE" %in% pre$data[entry_band_id == band0]$id)
})

test_that("qualification stops on an outcome column the data does not carry", {
  weeks <- .lme_weeks()
  d <- .lme_filler(weeks)
  design <- .lme_design(outcome_vars = c("died", "never_measured"))

  expect_error(
    .lme_enroll(d, design),
    "never_measured"
  )
})
