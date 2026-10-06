# Pin the (person, enrollment period) -> baseline treatment rule.
#
# `.enrollment_period_baseline_treatment()` is the single source of truth for
# that mapping, and both enrollment paths call it: `.s1_eligible_tuples()` (the
# s1a scout path) and `enroll()` Phase C (the direct `TTEEnrollment$new(...,
# ratio =)` path). vignettes/tte-methods.Rmd states the same rule.
#
# The helper reads only the weeks of the enrollment period that are eligible and
# hold TRUE or FALSE. It drops every other week of the enrollment period first.
# It then reports intervention when at least one week it reads holds TRUE. It
# reports comparator when every week it reads holds FALSE. It returns no row at
# all when it reads no week.
#
# The drop comes first, so an enrollment period of FALSE, NA, FALSE, FALSE is a
# comparator enrollment period. "Comparator in every eligible week" is a
# different rule, and it is not the one the code implements.
#
# The fixture below is the discriminating case, and no other fixture in the
# suite carries it: a four-week enrollment period with FOUR eligible weeks, in
# which the person is untreated in weeks 1 and 2 and treated in weeks 3 and 4.
# `.make_person_week_data()` marks only the first week of each person eligible,
# so first() and any() agree on it and it cannot separate the two rules.

# Return `n_weeks` consecutive ISO year-weeks starting on an enrollment period
# boundary, so that weeks 1 to 4 form one whole enrollment period under
# period_width = 4.
.period_fixture_weeks <- function(n_weeks, period_width = 4L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% period_width == 0L
  ]$idx[1]
  wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)]
}

# One person-week row per (id, week), every week eligible.
#
# `n_weeks` above 8 repeats the last value of each arm vector into the extra
# weeks. Follow-up opens at time zero, the first week after the enrollment
# period, so a test that reads the panel for the SECOND enrollment period needs
# a third enrollment period of data behind it.
.period_fixture <- function(tx_by_person, n_weeks = 8L) {
  weeks <- .period_fixture_weeks(n_weeks)
  d <- data.table::rbindlist(lapply(
    names(tx_by_person),
    function(nm) {
      tx <- tx_by_person[[nm]]
      if (length(tx) < n_weeks) {
        tx <- c(tx, rep(tx[length(tx)], n_weeks - length(tx)))
      }
      data.table::data.table(
        id = as.integer(nm),
        isoyearweek = weeks,
        exposed = tx
      )
    }
  ))
  d[, eligible := TRUE]
  d[, age := 50L]
  d[, death := 0L]
  d[]
}

.period_design <- function() {
  TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 4L,
    period_width = 4L
  )
}

# follow_up_time == period_width, so one follow-up interval per trial. The
# panel's `trial_id` names that follow-up interval, and `enrollment_period_id`
# names the trial, so the summary keys on `enrollment_period_id`.
.period_direct_path <- function(d, design, ratio, seed = 4) {
  trial <- TTEEnrollment$new(
    data.table::copy(d),
    design,
    ratio = ratio,
    seed = seed,
    extra_cols = "isoyearweek"
  )
  trial$data[,
    list(candidate_treatment = exposed[1]),
    by = list(id, trial_id = enrollment_period_id)
  ]
}

.period_scout_path <- function(d, design) {
  sk <- data.table::copy(d)
  sk[, rd_intervention := exposed]
  swereg:::.s1_eligible_tuples(sk, design)
}

# The two enrollment period ids the fixture spans, read from the mapping itself
# rather than hard-coded.
.period_ids <- function(d) {
  probe <- data.table::data.table(isoyearweek = unique(d$isoyearweek))
  swereg:::.assign_trial_ids(probe, period_width = 4L)
  probe$trial_id
}


test_that("fixture precondition: weeks 1-4 are one enrollment period and weeks 5-8 another", {
  d <- .period_fixture(list("1" = rep(FALSE, 8L)))
  period <- .period_ids(d)

  expect_equal(length(period), 8L)
  expect_equal(data.table::uniqueN(period[1:4]), 1L)
  expect_equal(data.table::uniqueN(period[5:8]), 1L)
  expect_false(period[1] == period[5])
})


test_that("both paths read every eligible week of the enrollment period, not only the first", {
  d <- .period_fixture(c(
    # Person 1 initiates in week 3 of the enrollment period. first() reads FALSE
    # off week 1; any() reads TRUE. This is the whole discriminator.
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 6L),
      as.character(2:7)
    )
  ))
  design <- .period_design()
  entry_period <- .period_ids(d)[1]

  scout <- .period_scout_path(d, design)
  scout_val <- scout[id == 1L & trial_id == entry_period]$intervention

  direct <- .period_direct_path(d, design, ratio = 1)
  direct_val <- direct[id == 1L & trial_id == entry_period]$candidate_treatment

  # Ground truth: treated in weeks 3 and 4 of the enrollment period, so the
  # enrollment period is intervention. Asserted as a value, not only as an
  # agreement: both paths now share one helper, so a wrong rule moves both of
  # them together and an agreement-only test would stay green.
  expect_length(scout_val, 1L)
  expect_length(direct_val, 1L)
  expect_true(scout_val)
  expect_true(direct_val)
  expect_identical(scout_val, direct_val)
})


test_that("both paths agree on every enrollment period the direct path enrolls", {
  d <- .period_fixture(c(
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    list("2" = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 6L),
      as.character(3:8)
    )
  ))
  design <- .period_design()

  scout <- .period_scout_path(d, design)
  # ratio = 20 exhausts the comparator pool, so every classified enrollment
  # period enrolls and the comparison covers all of them rather than a random
  # sample.
  direct <- .period_direct_path(d, design, ratio = 20)

  both <- merge(
    direct,
    scout,
    by = c("id", "trial_id"),
    all.x = TRUE
  )
  expect_equal(nrow(both), nrow(direct))
  expect_false(anyNA(both$intervention))
  expect_identical(both$candidate_treatment, both$intervention)
})


test_that("an enrollment period whose eligible weeks are all out of arm enters neither arm", {
  # Twelve weeks, because the assertions below read the panel for the SECOND
  # enrollment period and its follow-up interval is the third one.
  d <- .period_fixture(
    c(
      list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
      # Person 7 has no protocol arm in the enrollment period, and is a
      # comparator in the second enrollment period.
      list("7" = c(NA, NA, NA, NA, FALSE, FALSE, FALSE, FALSE)),
      stats::setNames(
        rep(list(rep(FALSE, 8L)), 5L),
        as.character(2:6)
      )
    ),
    n_weeks = 12L
  )
  design <- .period_design()
  period <- .period_ids(d)
  entry_period <- period[1]
  second_period <- period[5]

  scout <- .period_scout_path(d, design)
  # ratio = 20 takes every comparator candidate, so an absent enrollment is the
  # rule and never an unlucky draw.
  direct <- .period_direct_path(d, design, ratio = 20)

  # State 3: not returned by either path.
  expect_equal(nrow(scout[id == 7L & trial_id == entry_period]), 0L)
  expect_equal(nrow(direct[id == 7L & trial_id == entry_period]), 0L)

  # The same person IS classified in the enrollment period where a protocol arm
  # is present, so the drop is per enrollment period and not per person.
  expect_identical(scout[id == 7L & trial_id == second_period]$intervention, FALSE)
  expect_identical(
    direct[id == 7L & trial_id == second_period]$candidate_treatment,
    FALSE
  )
})


test_that("out-of-arm weeks are dropped, and the enrollment period keeps its in-arm weeks", {
  d <- .period_fixture(c(
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    # Person 8 has no protocol arm in weeks 1 and 2, then initiates in week 3.
    # Under first() the whole enrollment period read NA and vanished from both
    # arms.
    list("8" = c(NA, NA, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 5L),
      as.character(2:6)
    )
  ))
  design <- .period_design()
  entry_period <- .period_ids(d)[1]

  scout <- .period_scout_path(d, design)
  direct <- .period_direct_path(d, design, ratio = 20)

  scout_val <- scout[id == 8L & trial_id == entry_period]$intervention
  direct_val <- direct[id == 8L & trial_id == entry_period]$candidate_treatment

  expect_length(scout_val, 1L)
  expect_length(direct_val, 1L)
  expect_true(scout_val)
  expect_true(direct_val)
})


test_that("an enrollment period whose first week is out of arm is classified from the weeks that remain", {
  d <- .period_fixture(c(
    # Person 1 anchors the enrollment period with an initiator, so enroll()
    # always has an intervention candidate to match comparators against.
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    # Person 2 reads NA, FALSE, FALSE, FALSE in the enrollment period. first()
    # returns NA and the enrollment period vanishes from both arms. The rule
    # drops the NA week and reads FALSE, so the enrollment period is a
    # comparator candidate.
    list("2" = c(NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
    # Person 3 reads NA, TRUE, FALSE, FALSE in the enrollment period. first()
    # again returns NA. The rule reads TRUE, so the enrollment period is an
    # intervention candidate.
    list("3" = c(NA, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 4L),
      as.character(4:7)
    )
  ))
  design <- .period_design()
  entry_period <- .period_ids(d)[1]

  # Fixture precondition: week 1 of the enrollment period is out of arm for both
  # persons, so first() would return NA for both enrollment periods.
  first_week <- d[isoyearweek == unique(d$isoyearweek)[1]]
  expect_true(is.na(first_week[id == 2L]$exposed))
  expect_true(is.na(first_week[id == 3L]$exposed))

  scout <- .period_scout_path(d, design)
  # ratio = 20 exhausts the comparator pool, so an absent enrollment is the
  # rule and never an unlucky draw.
  direct <- .period_direct_path(d, design, ratio = 20)

  expect_identical(scout[id == 2L & trial_id == entry_period]$intervention, FALSE)
  expect_identical(
    direct[id == 2L & trial_id == entry_period]$candidate_treatment,
    FALSE
  )
  expect_identical(scout[id == 3L & trial_id == entry_period]$intervention, TRUE)
  expect_identical(
    direct[id == 3L & trial_id == entry_period]$candidate_treatment,
    TRUE
  )
})


test_that("an out-of-arm week inside the enrollment period does not stop a comparator classification", {
  d <- .period_fixture(c(
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    # Person 4 reads FALSE, NA, FALSE, FALSE in the enrollment period. The
    # person is NOT on the comparator treatment in every eligible week of that
    # enrollment period, because week 2 is out of arm. The enrollment period is
    # a comparator candidate anyway, because the rule drops week 2 before it
    # classifies.
    list("4" = c(FALSE, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 5L),
      as.character(5:9)
    )
  ))
  design <- .period_design()
  entry_period <- .period_ids(d)[1]

  # Fixture precondition: exactly one week of the enrollment period is out of
  # arm, and every week of the enrollment period is eligible.
  entry_weeks <- unique(d$isoyearweek)[1:4]
  person_4_entry <- d[id == 4L & isoyearweek %in% entry_weeks]
  expect_equal(nrow(person_4_entry), 4L)
  expect_true(all(person_4_entry$eligible))
  expect_equal(sum(is.na(person_4_entry$exposed)), 1L)

  scout <- .period_scout_path(d, design)
  direct <- .period_direct_path(d, design, ratio = 20)

  expect_identical(scout[id == 4L & trial_id == entry_period]$intervention, FALSE)
  expect_identical(
    direct[id == 4L & trial_id == entry_period]$candidate_treatment,
    FALSE
  )
})


test_that("period_width = 1 leaves every enrollment period with one week, so the rule is trivial", {
  d <- .period_fixture(c(
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 5L),
      as.character(2:6)
    )
  ))
  design <- TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 1L,
    period_width = 1L
  )
  sk <- data.table::copy(d)
  sk[, rd_intervention := exposed]
  scout <- swereg:::.s1_eligible_tuples(sk, design)

  # Eight weeks, eight enrollment periods, and the arm follows the week.
  expect_equal(nrow(scout[id == 1L]), 8L)
  expect_identical(
    scout[id == 1L][order(trial_id)]$intervention,
    c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
})


test_that("the seeded comparator draw does not depend on input row order", {
  # This test guards `setorderv(candidates, ...)` in enroll() Phase C.
  # `sample()` there runs `.SD[sample(.N, n_to_sample)]`, which draws ROW
  # INDICES inside a group, so the identity of the sampled comparators follows
  # the row order of `candidates`. That sort is load-bearing and not tidy.
  # Delete it and this test fails.
  #
  # Twelve weeks, because the count below covers the first TWO enrollment
  # periods and the second one needs a third enrollment period of data to follow
  # up into.
  d <- .period_fixture(
    c(
      list("1" = rep(TRUE, 8L)),
      stats::setNames(
        rep(list(rep(FALSE, 8L)), 20L),
        as.character(2:21)
      )
    ),
    n_weeks = 12L
  )
  design <- .period_design()

  # A fixed permutation, never sample(). This test is itself about seeded
  # reproducibility, so a random shuffle inside it could not be trusted.
  reversed <- d[rev(seq_len(nrow(d)))]

  arms <- function(x) {
    sort(paste0(x$id, ".", x$trial_id, ":", x$candidate_treatment))
  }
  from_sorted <- arms(.period_direct_path(d, design, ratio = 2, seed = 99))
  from_reversed <- arms(
    .period_direct_path(reversed, design, ratio = 2, seed = 99)
  )

  # Two enrollment periods, one initiator per enrollment period, two comparators
  # drawn per enrollment period.
  expect_equal(length(from_sorted), 6L)
  expect_identical(from_sorted, from_reversed)
})
