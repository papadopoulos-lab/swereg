# Pin the (person, band) -> baseline treatment rule.
#
# `.band_baseline_treatment()` is the single source of truth for that mapping,
# and both enrollment paths call it: `.s1_eligible_tuples()` (the s1a scout
# path) and `enroll()` Phase C (the direct `TTEEnrollment$new(..., ratio =)`
# path). vignettes/tte-methods.Rmd states the same rule.
#
# The helper reads only the weeks of the band that are eligible and hold TRUE
# or FALSE. It drops every other week of the band first. It then reports
# intervention when at least one week it reads holds TRUE. It reports
# comparator when every week it reads holds FALSE. It returns no row at all
# when it reads no week.
#
# The drop comes first, so a band of FALSE, NA, FALSE, FALSE is a comparator
# band. "Comparator in every eligible week" is a different rule, and it is not
# the one the code implements.
#
# The fixture below is the discriminating case, and no other fixture in the
# suite carries it: a four-week band with FOUR eligible weeks, in which the
# person is untreated in weeks 1 and 2 and treated in weeks 3 and 4.
# `.make_person_week_data()` marks only the first week of each person
# eligible, so first() and any() agree on it and it cannot separate the two
# rules.

# Return `n_weeks` consecutive ISO year-weeks starting on a band boundary, so
# that weeks 1 to 4 form one whole band under period_width = 4.
.band_fixture_weeks <- function(n_weeks, period_width = 4L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% period_width == 0L
  ]$idx[1]
  wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)]
}

# One person-week row per (id, week), every week eligible.
.band_fixture <- function(tx_by_person) {
  weeks <- .band_fixture_weeks(8L)
  d <- data.table::rbindlist(lapply(
    names(tx_by_person),
    function(nm) {
      data.table::data.table(
        id = as.integer(nm),
        isoyearweek = weeks,
        exposed = tx_by_person[[nm]]
      )
    }
  ))
  d[, eligible := TRUE]
  d[, age := 50L]
  d[, death := 0L]
  d[]
}

.band_design <- function() {
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

# follow_up_time == period_width, so one follow-up band per trial and the
# panel's trial_id is the entry band.
.band_direct_path <- function(d, design, ratio, seed = 4) {
  trial <- TTEEnrollment$new(
    data.table::copy(d),
    design,
    ratio = ratio,
    seed = seed,
    extra_cols = "isoyearweek"
  )
  trial$data[,
    list(band_treatment = exposed[1]),
    by = list(id, trial_id)
  ]
}

.band_scout_path <- function(d, design) {
  sk <- data.table::copy(d)
  sk[, rd_intervention := exposed]
  swereg:::.s1_eligible_tuples(sk, design)
}

# The two band ids the fixture spans, read from the mapping itself rather
# than hard-coded.
.band_ids <- function(d) {
  probe <- data.table::data.table(isoyearweek = unique(d$isoyearweek))
  swereg:::.assign_trial_ids(probe, period_width = 4L)
  probe$trial_id
}


test_that("fixture precondition: weeks 1-4 are one band and weeks 5-8 another", {
  d <- .band_fixture(list("1" = rep(FALSE, 8L)))
  band <- .band_ids(d)

  expect_equal(length(band), 8L)
  expect_equal(data.table::uniqueN(band[1:4]), 1L)
  expect_equal(data.table::uniqueN(band[5:8]), 1L)
  expect_false(band[1] == band[5])
})


test_that("both paths read every eligible week of the band, not only the first", {
  d <- .band_fixture(c(
    # Person 1 initiates in week 3 of the entry band. first() reads FALSE off
    # week 1; any() reads TRUE. This is the whole discriminator.
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 6L),
      as.character(2:7)
    )
  ))
  design <- .band_design()
  entry_band <- .band_ids(d)[1]

  scout <- .band_scout_path(d, design)
  scout_val <- scout[id == 1L & trial_id == entry_band]$intervention

  direct <- .band_direct_path(d, design, ratio = 1)
  direct_val <- direct[id == 1L & trial_id == entry_band]$band_treatment

  # Ground truth: treated in weeks 3 and 4 of the band, so the band is
  # intervention. Asserted as a value, not only as an agreement: both paths
  # now share one helper, so a wrong rule moves both of them together and an
  # agreement-only test would stay green.
  expect_length(scout_val, 1L)
  expect_length(direct_val, 1L)
  expect_true(scout_val)
  expect_true(direct_val)
  expect_identical(scout_val, direct_val)
})


test_that("both paths agree on every band the direct path enrolls", {
  d <- .band_fixture(c(
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    list("2" = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 6L),
      as.character(3:8)
    )
  ))
  design <- .band_design()

  scout <- .band_scout_path(d, design)
  # ratio = 20 exhausts the comparator pool, so every classified band enrolls
  # and the comparison covers all of them rather than a random sample.
  direct <- .band_direct_path(d, design, ratio = 20)

  both <- merge(
    direct,
    scout,
    by = c("id", "trial_id"),
    all.x = TRUE
  )
  expect_equal(nrow(both), nrow(direct))
  expect_false(anyNA(both$intervention))
  expect_identical(both$band_treatment, both$intervention)
})


test_that("a band whose eligible weeks are all out of arm enters neither arm", {
  d <- .band_fixture(c(
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    # Person 7 has no protocol arm in the entry band, and is a comparator in
    # the second band.
    list("7" = c(NA, NA, NA, NA, FALSE, FALSE, FALSE, FALSE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 5L),
      as.character(2:6)
    )
  ))
  design <- .band_design()
  band <- .band_ids(d)
  entry_band <- band[1]
  second_band <- band[5]

  scout <- .band_scout_path(d, design)
  # ratio = 20 takes every comparator band, so an absent enrollment is the
  # rule and never an unlucky draw.
  direct <- .band_direct_path(d, design, ratio = 20)

  # State 3: not returned by either path.
  expect_equal(nrow(scout[id == 7L & trial_id == entry_band]), 0L)
  expect_equal(nrow(direct[id == 7L & trial_id == entry_band]), 0L)

  # The same person IS classified in the band where a protocol arm is
  # present, so the drop is per band and not per person.
  expect_identical(scout[id == 7L & trial_id == second_band]$intervention, FALSE)
  expect_identical(
    direct[id == 7L & trial_id == second_band]$band_treatment,
    FALSE
  )
})


test_that("out-of-arm weeks are dropped, and the band keeps its in-arm weeks", {
  d <- .band_fixture(c(
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    # Person 8 has no protocol arm in weeks 1 and 2, then initiates in week 3.
    # Under first() the whole band read NA and vanished from both arms.
    list("8" = c(NA, NA, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 5L),
      as.character(2:6)
    )
  ))
  design <- .band_design()
  entry_band <- .band_ids(d)[1]

  scout <- .band_scout_path(d, design)
  direct <- .band_direct_path(d, design, ratio = 20)

  scout_val <- scout[id == 8L & trial_id == entry_band]$intervention
  direct_val <- direct[id == 8L & trial_id == entry_band]$band_treatment

  expect_length(scout_val, 1L)
  expect_length(direct_val, 1L)
  expect_true(scout_val)
  expect_true(direct_val)
})


test_that("a band whose first week is out of arm is classified from the weeks that remain", {
  d <- .band_fixture(c(
    # Person 1 anchors the entry band with an initiator, so enroll() always has
    # an intervention band to match comparators against.
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    # Person 2 reads NA, FALSE, FALSE, FALSE in the entry band. first() returns
    # NA and the band vanishes from both arms. The rule drops the NA week and
    # reads FALSE, so the band is a comparator band.
    list("2" = c(NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
    # Person 3 reads NA, TRUE, FALSE, FALSE in the entry band. first() again
    # returns NA. The rule reads TRUE, so the band is an intervention band.
    list("3" = c(NA, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 4L),
      as.character(4:7)
    )
  ))
  design <- .band_design()
  entry_band <- .band_ids(d)[1]

  # Fixture precondition: week 1 of the entry band is out of arm for both
  # persons, so first() would return NA for both bands.
  first_week <- d[isoyearweek == unique(d$isoyearweek)[1]]
  expect_true(is.na(first_week[id == 2L]$exposed))
  expect_true(is.na(first_week[id == 3L]$exposed))

  scout <- .band_scout_path(d, design)
  # ratio = 20 exhausts the comparator pool, so an absent enrollment is the
  # rule and never an unlucky draw.
  direct <- .band_direct_path(d, design, ratio = 20)

  expect_identical(scout[id == 2L & trial_id == entry_band]$intervention, FALSE)
  expect_identical(
    direct[id == 2L & trial_id == entry_band]$band_treatment,
    FALSE
  )
  expect_identical(scout[id == 3L & trial_id == entry_band]$intervention, TRUE)
  expect_identical(
    direct[id == 3L & trial_id == entry_band]$band_treatment,
    TRUE
  )
})


test_that("an out-of-arm week inside the band does not stop a comparator classification", {
  d <- .band_fixture(c(
    list("1" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    # Person 4 reads FALSE, NA, FALSE, FALSE in the entry band. The person is
    # NOT on the comparator treatment in every eligible week of that band,
    # because week 2 is out of arm. The band is a comparator band anyway,
    # because the rule drops week 2 before it classifies.
    list("4" = c(FALSE, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 5L),
      as.character(5:9)
    )
  ))
  design <- .band_design()
  entry_band <- .band_ids(d)[1]

  # Fixture precondition: exactly one week of the entry band is out of arm, and
  # every week of the band is eligible.
  entry_weeks <- unique(d$isoyearweek)[1:4]
  person_4_entry <- d[id == 4L & isoyearweek %in% entry_weeks]
  expect_equal(nrow(person_4_entry), 4L)
  expect_true(all(person_4_entry$eligible))
  expect_equal(sum(is.na(person_4_entry$exposed)), 1L)

  scout <- .band_scout_path(d, design)
  direct <- .band_direct_path(d, design, ratio = 20)

  expect_identical(scout[id == 4L & trial_id == entry_band]$intervention, FALSE)
  expect_identical(
    direct[id == 4L & trial_id == entry_band]$band_treatment,
    FALSE
  )
})


test_that("period_width = 1 leaves every band with one week, so the rule is trivial", {
  d <- .band_fixture(c(
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

  # Eight weeks, eight bands, and the arm follows the week.
  expect_equal(nrow(scout[id == 1L]), 8L)
  expect_identical(
    scout[id == 1L][order(trial_id)]$intervention,
    c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
})


test_that("the seeded comparator draw does not depend on input row order", {
  # This test guards `setorderv(band_summary, ...)` in enroll() Phase C.
  # `sample()` there runs `.SD[sample(.N, n_to_sample)]`, which draws ROW
  # INDICES inside a group, so the identity of the sampled comparators follows
  # the row order of `band_summary`. That sort is load-bearing and not tidy.
  # Delete it and this test fails.
  d <- .band_fixture(c(
    list("1" = rep(TRUE, 8L)),
    stats::setNames(
      rep(list(rep(FALSE, 8L)), 20L),
      as.character(2:21)
    )
  ))
  design <- .band_design()

  # A fixed permutation, never sample(). This test is itself about seeded
  # reproducibility, so a random shuffle inside it could not be trusted.
  reversed <- d[rev(seq_len(nrow(d)))]

  arms <- function(x) {
    sort(paste0(x$id, ".", x$trial_id, ":", x$band_treatment))
  }
  from_sorted <- arms(.band_direct_path(d, design, ratio = 2, seed = 99))
  from_reversed <- arms(
    .band_direct_path(reversed, design, ratio = 2, seed = 99)
  )

  # Two bands, one initiator per band, two comparators drawn per band.
  expect_equal(length(from_sorted), 6L)
  expect_identical(from_sorted, from_reversed)
})
