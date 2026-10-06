# Regression tests for the CONSORT cascade double-counting bug.
#
# Background: `.s1_compute_attrition()` returns per-trial summary rows
# (one per enrollment_period_id) plus a single global row tagged
# `enrollment_period_id = NA` carrying `uniqueN(person_id)` across all trials.
# The downstream per-batch aggregation in `s1_generate_enrollments_and_ipw()`
# sums by `.(enrollment_period_id, criterion)`. Before the fix, the per-trial
# summary's per-trial aggregation produced an *additional*
# `enrollment_period_id = NA` group covering person-weeks that fall outside
# any trial period (where the week's `period_id` is genuinely NA). Both rows were rbinded into the
# per-batch attrition table, then summed together by the aggregation
# step, roughly doubling the reported global cohort size in CONSORT.

skip_if_not_installed("data.table")

# Helper: minimal skeleton with NA period_ids mixed in, plus one criterion
.fake_skeleton_with_na_trials <- function() {
  data.table::data.table(
    person_id      = c("A","A","A", "B","B","B", "C","C", "D"),
    period_id      = c(1L, 2L, NA_integer_,
                       1L, NA_integer_, 2L,
                       NA_integer_, NA_integer_,
                       1L),
    incl_age       = c(TRUE, TRUE, TRUE,
                       TRUE, TRUE, TRUE,
                       TRUE, TRUE,
                       TRUE),
    rd_intervention = c(TRUE,  TRUE,  FALSE,
                        FALSE, FALSE, FALSE,
                        FALSE, FALSE,
                        TRUE)
  )
}

test_that("`.s1_compute_attrition()` returns exactly one enrollment_period_id=NA row per criterion", {
  skel <- .fake_skeleton_with_na_trials()
  attr <- swereg:::.s1_compute_attrition(
    skeleton      = skel,
    eligible_cols = "incl_age",
    pid           = "person_id"
  )

  # Exactly one global row per criterion (one for "before_exclusions",
  # one for the cumulative "incl_age" criterion).
  na_rows_per_crit <- attr[is.na(enrollment_period_id), .N, by = criterion]
  expect_equal(sort(na_rows_per_crit$criterion),
               sort(c("before_exclusions", "incl_age")))
  expect_true(all(na_rows_per_crit$N == 1L),
              info = paste("enrollment_period_id=NA rows per criterion:",
                           paste(na_rows_per_crit$N, collapse = ",")))
})

test_that("per-trial summary contains only real (non-NA) enrollment_period_ids", {
  skel <- .fake_skeleton_with_na_trials()
  attr <- swereg:::.s1_compute_attrition(
    skeleton      = skel,
    eligible_cols = "incl_age",
    pid           = "person_id"
  )

  per_trial <- attr[!is.na(enrollment_period_id)]
  # Real period_ids in the input were 1 and 2.
  expect_setequal(unique(per_trial$enrollment_period_id), c(1L, 2L))
})

test_that("global row's n_persons equals true uniqueN, not double", {
  skel <- .fake_skeleton_with_na_trials()
  attr <- swereg:::.s1_compute_attrition(
    skeleton      = skel,
    eligible_cols = "incl_age",
    pid           = "person_id"
  )

  # The input has 4 unique persons (A, B, C, D). Person C only appears in
  # NA-period-id weeks, so C enters no trial and the global row counts 3
  # (A, B, D). The pre-fix bug would have surfaced an extra
  # enrollment_period_id=NA per-trial row covering C and friends.
  before_global <- attr[criterion == "before_exclusions" & is.na(enrollment_period_id)]
  expect_equal(nrow(before_global), 1L)
  expect_equal(before_global$n_persons, 3L)
})

test_that("per-batch aggregation (sum by enrollment_period_id, criterion) does NOT double the global", {
  # Simulates the centralized matching aggregation in
  # `.s1b_worker()` (R/tteplan_s1_workers.R):
  #   attrition_summary <- all_attrition[,
  #     .(n_persons = sum(n_persons), ...),
  #     by = .(enrollment_period_id, criterion)]
  # Two person-disjoint batches; check the post-aggregation global row.
  skel <- .fake_skeleton_with_na_trials()
  batch1 <- skel[person_id %in% c("A", "B")]
  batch2 <- skel[person_id %in% c("C", "D")]

  a1 <- swereg:::.s1_compute_attrition(batch1, "incl_age", "person_id")
  a2 <- swereg:::.s1_compute_attrition(batch2, "incl_age", "person_id")
  combined <- data.table::rbindlist(list(a1, a2), use.names = TRUE)

  agg <- combined[, .(n_persons = sum(n_persons)),
                  by = .(enrollment_period_id, criterion)]
  global_before <- agg[is.na(enrollment_period_id) & criterion == "before_exclusions"]
  expect_equal(nrow(global_before), 1L)
  # Pre-fix: 8 (=4 *2). Post-fix: 3 unique persons across batches. Person C
  # has only NA-period-id weeks and enters no trial (swereg#51).
  expect_equal(global_before$n_persons, 3L)
})
