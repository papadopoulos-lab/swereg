# Each row of `$irr_by_subgroup()` names the formula of its own fit.
#
# `.tte_fit_irr()` builds the time terms from the rows it fits. The
# whole-cohort row and each stratum row can therefore use different formulas,
# and one formula for the whole table would misstate some of them. A row that
# no fit produced names none.

skip_if_not_installed("survey")

# Three strata of `Z`. Stratum 0 follows each person-trial for 6 intervals,
# stratum 1 for 2, so the time since time zero takes 6 and 2 distinct values.
# Stratum 2 holds no event in the comparator arm.
.smf_trial <- function() {
  one <- function(z, n_int, n_pt, ev_tx, ev_cmp, offset) {
    ids <- sprintf("z%d_%03d", z, seq_len(n_pt))
    tx <- rep(c(TRUE, FALSE), length.out = n_pt)
    d <- data.table::data.table(
      id = rep(ids, each = n_int),
      tstart = rep(seq_len(n_int) - 1L, n_pt),
      tstop = rep(seq_len(n_int), n_pt),
      treatment = rep(tx, each = n_int),
      Z = z,
      person_weeks = 1L,
      w = 1,
      event = 0L
    )
    # Events on the last interval of the first person-trials of each arm.
    last <- d$tstop == n_int
    hit_tx <- utils::head(ids[tx], ev_tx)
    hit_cmp <- utils::head(ids[!tx], ev_cmp)
    data.table::set(d, which(last & d$id %in% hit_tx), "event", 1L)
    data.table::set(d, which(last & d$id %in% hit_cmp), "event", 1L)
    return(d)
  }
  d <- data.table::rbindlist(list(
    one(0L, 6L, 200L, 30L, 20L),
    one(1L, 2L, 200L, 25L, 15L),
    one(2L, 3L, 60L, 5L, 0L)
  ))
  design <- TTEDesign$new(
    id_var = "id",
    person_id_var = "id",
    treatment_var = "treatment",
    outcome_vars = "event",
    confounder_vars = "Z",
    subgroup_vars = "Z",
    follow_up_time = 6L
  )
  return(TTEEnrollment$new(d, design, data_level = "trial"))
}

test_that("each subgroup IRR row carries the formula of its own fit", {
  trial <- .smf_trial()
  out <- NULL
  expect_warning(
    out <- trial$irr_by_subgroup("w", "Z"),
    "stratum '2' has no events"
  )
  expect_identical(out$level, c("all", "0", "1", "2"))
  expect_type(out$model_formula, "character")
  expect_identical(
    out$model_formula,
    c(
      "event ~ treatment + splines::ns(tstart, df = 3) + offset(log(person_weeks))",
      "event ~ treatment + splines::ns(tstart, df = 3) + offset(log(person_weeks))",
      "event ~ treatment + factor(tstart) + offset(log(person_weeks))",
      NA_character_
    )
  )
  # The whole-cohort row and the stratum-1 row differ, so one table-level
  # formula could not describe both.
  expect_false(identical(out$model_formula[1], out$model_formula[3]))
})
