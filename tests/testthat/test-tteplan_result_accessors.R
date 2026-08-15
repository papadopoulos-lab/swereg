# Pin the five result accessors on TTEPlan.
#
# Each accessor returns everything the plan stores, in one flat table, and
# computes nothing. Six properties are pinned here:
#
#   1. one row per key combination, on a fixture with known contents;
#   2. row keys are unique;
#   3. every schema column present, by name;
#   4. effect and decision fields carry no formatting;
#   5. a missing slot yields absent rows, and the accessors compute nothing;
#   6. the two subgroup p-values are different quantities and never swap.
#
# Baseline CELL values are exempt from (4). `overall`, `comparator` and
# `intervention` are display strings such as "12.3 (4.5)", stored that way by
# the producer.

skip_if_not_installed("data.table")

# --- fixture -----------------------------------------------------------------

.acc_fix_rates <- function() {
  dt <- data.table::data.table(
    rd_tx = c(TRUE, FALSE),
    n_persons = c(300, 700),
    n_trials = c(3000, 7000),
    events_weighted = c(10.4, 20.6),
    py_weighted = c(62816, 98765),
    rate_per_100000py = c(16.9, 12.0)
  )
  data.table::setattr(dt, "swereg_type", "rates")
  data.table::setattr(dt, "treatment_var", "rd_tx")
  dt
}

# The shape `$irr()` returns. `stored = TRUE` runs it through the same
# producer-side marker `$s3_analyze()` uses, so `irr_estimable` is a STORED
# field. `stored = FALSE` is a result cached before that field existed.
.acc_fix_irr <- function(stored = TRUE) {
  dt <- data.table::data.table(
    IRR = 0.54,
    IRR_lower = 0.40,
    IRR_upper = 0.71,
    IRR_pvalue = 0.0000001,
    warn = FALSE
  )
  data.table::setattr(dt, "swereg_type", "irr")
  if (stored) swereg:::.s3_mark_irr_estimable(dt) else dt
}

# The same columns and the same attribute `.tte_rd_curve()` returns, so the
# accessor meets the shape the producer writes.
.acc_fix_curve <- function(conf_level = 0.95) {
  cv <- data.table::data.table(
    tstop = c(26, 52),
    surv_comparator = c(0.990, 0.980),
    surv_intervention = c(0.995, 0.990),
    rd = c(-0.005, -0.010),
    rd_lo = c(-0.008, -0.014),
    rd_hi = c(-0.002, -0.006),
    interval_status = c("ok", "ok"),
    nnt = c(200, 100),
    nnt_lo = c(125, 71.42857142857143),
    nnt_hi = c(500, 166.66666666666666),
    nnt_direction = c("benefit", "benefit"),
    n_persons_with_event_comparator = c(10, 20),
    n_persons_with_event_intervention = c(5, 10),
    n_persons_at_risk_comparator = c(900, 850),
    n_persons_at_risk_intervention = c(700, 640)
  )
  data.table::setattr(cv, "conf_level", conf_level)
  cv
}

.acc_fix_table1 <- function() {
  d <- data.table::data.table(
    rd_tx = rep(c(TRUE, FALSE), each = 50L),
    rd_age_continuous = as.numeric(rep(50:99, times = 2L)),
    rd_edu = rep(c("low", "high"), times = 50L)
  )
  swereg:::.swereg_table1(
    data = d,
    vars = c("rd_age_continuous", "rd_edu"),
    strata = "rd_tx",
    include_smd = TRUE,
    show_missing = "always",
    arm_labels = c(comparator = "Untreated", intervention = "Treated")
  )
}

# The shape `$irr_by_subgroup()` returns. It attaches its OWN `em_pvalue` and
# `ratio_of_irrs` attributes, from a second run of the interaction test. The
# accessor MUST NOT read those attributes, so the fixture sets them to values
# no `emtest_*` slot carries.
.acc_fix_subgroup <- function(levels, irr, irr_lo, irr_hi, irr_p) {
  dt <- data.table::data.table(
    level = levels,
    IRR = irr,
    IRR_lower = irr_lo,
    IRR_upper = irr_hi,
    IRR_pvalue = irr_p,
    warn = rep(FALSE, length(levels))
  )
  data.table::setattr(dt, "em_pvalue", 0.99)
  data.table::setattr(dt, "ratio_of_irrs", 9.99)
  data.table::setattr(dt, "n_na_subgroup", 3L)
  data.table::setattr(dt, "swereg_type", "irr_by_subgroup")
  dt
}

# The shape `$effect_modification_test()` returns, which is what
# `$s3_analyze()` stores in an `emtest_*` slot.
.acc_fix_emtest <- function(p_value, ratio, ratio_lo, ratio_hi, subgroup_var) {
  list(
    p_value = p_value,
    subgroup_var = subgroup_var,
    n_levels = 2L,
    interaction_coefs = data.table::data.table(
      term = "x",
      estimate = log(ratio),
      se = 0.1,
      p = p_value
    ),
    ratio_of_irrs = ratio,
    ratio_lower = ratio_lo,
    ratio_upper = ratio_hi
  )
}

# ETT00001 carries every slot of all three combinations.
# ETT00002 deliberately carries fewer:
#   - per-protocol truncated: rates and a LEGACY ratio with no stored
#     `irr_estimable`;
#   - per-protocol untruncated: NOTHING, so that combination gets no row;
#   - intention-to-treat: the risk difference only, with a SKIP envelope in
#     `irr_itt` and no rates.
#
# ETT00001 carries SIX subgroup variables, which sweeps all four permutations
# of the two stored slot families across both estimands:
#
#   variable      estimand  subgroup_*      emtest_*   permutation
#   rd_age_band   pp        table, 3 levels present    both stored
#   rd_age_band   itt       table, 3 levels present    both stored
#   rd_parity     pp        table, 3 levels present    both stored
#   rd_bmi        pp        table, 3 levels ABSENT     stratified only
#   rd_smoking    itt       table, 3 levels ABSENT     stratified only
#   rd_bmi        itt       ABSENT          present    interaction only
#   rd_smoking    pp        ABSENT          present    interaction only
#   rd_skipped    pp        SKIP envelope   present    interaction only
#   rd_absent     both      ABSENT          ABSENT     neither stored
#
# `rd_absent` appears in the ETT's `subgroup_vars` specification column and in
# no slot at all. `.write_effect_modification()` emits one all-NA row for it.
# The accessor emits none, because nothing was stored.
#
# EVERY stratified table carries poisoned `em_pvalue = 0.99` and
# `ratio_of_irrs = 9.99` attributes, which `$irr_by_subgroup()` writes from its
# own second run of the interaction test. No row may report either value.
#
# Every variable has an `"all"` row, so a key without `subgroup_var` duplicates.
#
# The enrollment deliberately carries no `table1_raw`.
# The enrollment DOES carry `matching` counts and `n_baseline`, so a test can
# show that `$get_attrition()` turns neither into a step.
.acc_fix_plan <- function() {
  ett <- data.table::data.table(
    enrollment_id = c("01", "01"),
    ett_id = c("ETT00001", "ETT00002"),
    age_group = c("50_59", "50_59"),
    age_min = 50L,
    age_max = 59L,
    follow_up = c(52L, 104L),
    outcome_var = c("osd_a", "osd_b"),
    outcome_name = c("Outcome A", "Outcome B"),
    outcome_role = c("primary", "secondary"),
    description = c("ETT00001", "ETT00002"),
    confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    file_raw = "raw_01.qs2",
    file_imp = "imp_01.qs2",
    file_analysis = "analysis_001.qs2",
    # The SPECIFICATION names `rd_absent`, and no slot stores a result for it.
    # `.write_effect_modification()` iterates this column and emits one all-NA
    # row for it. `$get_subgroups()` iterates the stored slots and emits none.
    subgroup_vars = list(
      c(
        "rd_age_band",
        "rd_parity",
        "rd_bmi",
        "rd_smoking",
        "rd_skipped",
        "rd_absent"
      ),
      character(0)
    )
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$spec <- list(
    study = list(implementation = list(project_prefix = "test")),
    enrollments = list(list(
      id = "01",
      name = "Enrollment one",
      treatment = list(arms = list(
        intervention = "Treated",
        comparator = "Untreated"
      ))
    ))
  )

  rates <- .acc_fix_rates()
  rd_pp <- swereg:::.s3_rd_result(
    "rd_pp_trunc",
    .acc_fix_curve(),
    "ETT00001",
    "tstop"
  )
  rd_itt_1 <- swereg:::.s3_rd_result(
    "rd_itt",
    .acc_fix_curve(),
    "ETT00001",
    "tstop"
  )
  rd_itt_2 <- swereg:::.s3_rd_result(
    "rd_itt",
    .acc_fix_curve(),
    "ETT00002",
    "tstop"
  )

  plan$results_ett <- list(
    ETT00001 = c(
      list(
        enrollment_id = "01",
        description = "ETT00001",
        summary = list(n_events = 42L),
        rates_pp_trunc = rates,
        rates_pp = rates,
        rates_itt = rates,
        irr_pp_trunc = .acc_fix_irr(),
        irr_pp = .acc_fix_irr(),
        irr_itt = .acc_fix_irr(),
        subgroup_rd_age_band_pp = .acc_fix_subgroup(
          c("all", "younger", "older"),
          c(0.54, 0.61, 0.48),
          c(0.40, 0.42, 0.31),
          c(0.71, 0.88, 0.74),
          c(0.0000001, 0.008, 0.001)
        ),
        emtest_rd_age_band_pp = .acc_fix_emtest(
          0.42,
          0.79,
          0.55,
          1.14,
          "rd_age_band"
        ),
        subgroup_rd_age_band_itt = .acc_fix_subgroup(
          c("all", "younger", "older"),
          c(0.66, 0.70, 0.59),
          c(0.50, 0.49, 0.39),
          c(0.87, 1.00, 0.89),
          c(0.003, 0.049, 0.012)
        ),
        emtest_rd_age_band_itt = .acc_fix_emtest(
          0.61,
          0.88,
          0.60,
          1.29,
          "rd_age_band"
        ),
        subgroup_rd_parity_pp = .acc_fix_subgroup(
          c("all", "nulliparous", "parous"),
          c(0.54, 0.71, 0.45),
          c(0.40, 0.50, 0.29),
          c(0.71, 1.01, 0.70),
          c(0.0000001, 0.056, 0.0004)
        ),
        emtest_rd_parity_pp = .acc_fix_emtest(
          0.07,
          1.35,
          0.97,
          1.88,
          "rd_parity"
        ),
        # STRATIFIED ONLY. No `emtest_rd_bmi_pp`.
        subgroup_rd_bmi_pp = .acc_fix_subgroup(
          c("all", "low", "high"),
          c(0.54, 0.52, 0.57),
          c(0.40, 0.33, 0.38),
          c(0.71, 0.81, 0.85),
          c(0.0000001, 0.004, 0.006)
        ),
        # STRATIFIED ONLY, on the other estimand.
        subgroup_rd_smoking_itt = .acc_fix_subgroup(
          c("all", "never", "ever"),
          c(0.72, 0.75, 0.68),
          c(0.55, 0.54, 0.45),
          c(0.94, 1.04, 1.03),
          c(0.016, 0.084, 0.071)
        ),
        # INTERACTION ONLY. No `subgroup_rd_bmi_itt`.
        emtest_rd_bmi_itt = .acc_fix_emtest(0.31, 1.12, 0.83, 1.51, "rd_bmi"),
        # INTERACTION ONLY, on the other estimand.
        emtest_rd_smoking_pp = .acc_fix_emtest(
          0.88,
          1.02,
          0.76,
          1.37,
          "rd_smoking"
        ),
        # INTERACTION ONLY, because the stratified worker FAILED and stored a
        # skip envelope rather than a table.
        subgroup_rd_skipped_pp = list(skipped = TRUE, reason = "no events"),
        emtest_rd_skipped_pp = .acc_fix_emtest(
          0.05,
          2.40,
          1.01,
          5.70,
          "rd_skipped"
        )
      ),
      rd_pp,
      rd_itt_1
    ),
    ETT00002 = c(
      list(
        enrollment_id = "01",
        description = "ETT00002",
        summary = list(n_events = 7L),
        rates_pp_trunc = rates,
        irr_pp_trunc = .acc_fix_irr(stored = FALSE),
        irr_itt = list(skipped = TRUE, reason = "no events")
      ),
      rd_itt_2
    )
  )

  t1 <- .acc_fix_table1()
  plan$results_enrollment <- list(
    `01` = list(
      table1_raw = NULL,
      table1_unweighted = t1,
      table1_ipw = t1,
      table1_ipw_trunc = t1,
      table1_ipw_trunc_main = t1,
      n_baseline = 1000L,
      n_baseline_intervention = 300L,
      n_baseline_comparator = 700L
    )
  )

  plan$enrollment_counts <- list(
    `01` = list(
      # The stored shape: per-trial rows, plus ONE GLOBAL ROW per criterion
      # carrying the true overall count of distinct people. `prior_disease`
      # deliberately has NO global row, which is what a file written before
      # the global row existed looks like.
      attrition = data.table::data.table(
        trial_id = c(1L, 2L, NA, 1L, 2L, NA, 1L, 2L),
        criterion = c(
          "before_exclusions", "before_exclusions", "before_exclusions",
          "age", "age", "age",
          "prior_disease", "prior_disease"
        ),
        n_persons = c(3000, 3200, 5000, 2500, 2600, 4000, 1800, 1900),
        n_person_trials = c(
          25000, 25000, 50000, 20000, 20000, 40000, 15000, 15000
        ),
        n_intervention = c(500, 500, 1000, 450, 450, 900, 400, 400),
        n_comparator = c(2000, 2000, 4000, 1550, 1550, 3100, 1100, 1100)
      ),
      matching = data.table::data.table(
        trial_id = 1L,
        n_intervention_total = 800,
        n_comparator_total = 2200,
        n_intervention_enrolled = 300,
        n_comparator_enrolled = 700
      )
    )
  )
  plan
}

# `[[.TTEPlan` is overloaded and looks an enrollment up, so `plan[["get_x"]]`
# does NOT reach the method. Call each accessor by name.
.acc_call <- function(plan, nm) {
  switch(
    nm,
    estimates = plan$get_estimates(),
    curves = plan$get_curves(),
    baselines = plan$get_baselines(),
    attrition = plan$get_attrition(),
    matching = plan$get_matching(),
    subgroups = plan$get_subgroups(),
    stop("unknown accessor: ", nm)
  )
}

# The schema each accessor promises, written out here rather than read from the
# package. A test that read `.ACC_SCHEMA` would agree with any change to it.
.ACC_EXPECT <- list(
  estimates = c(
    "ett_id", "enrollment_id", "enrollment_name", "outcome_var",
    "outcome_name", "outcome_role", "follow_up", "age_group",
    "intervention_name", "comparator_name", "estimand", "weights",
    "n_events", "rates_stored",
    "events_int", "py_int", "rate_int", "events_cmp", "py_cmp",
    "rate_cmp", "persons_event_int", "persons_event_cmp", "irr", "irr_lo",
    "irr_hi", "irr_pvalue", "irr_estimable", "irr_stored",
    "irr_interval_stored", "rd_stored", "rd", "rd_lo", "rd_hi",
    "interval_status", "nnt", "nnt_lo", "nnt_hi", "nnt_direction", "n_boot",
    "seed", "conf_level"
  ),
  curves = c(
    "ett_id", "estimand", "weights", "arm", "band", "surv",
    "n_persons_at_risk"
  ),
  baselines = c(
    "enrollment_id", "imputation", "weighting", "variant", "variable",
    "level", "overall", "comparator", "intervention", "comparator_label",
    "intervention_label", "smd_stored", "smd_numeric",
    "n_baseline", "n_baseline_intervention", "n_baseline_comparator"
  ),
  attrition = c(
    "enrollment_id", "trial_id", "step_order", "step_name", "n_persons",
    "n_person_trials", "n_arm_intervention", "n_arm_comparator"
  ),
  matching = c(
    "enrollment_id", "trial_id", "n_intervention_total", "n_comparator_total",
    "n_intervention_enrolled", "n_comparator_enrolled"
  ),
  subgroups = c(
    "ett_id", "estimand", "weights", "subgroup_var", "subgroup_level",
    "strata_stored", "irr",
    "irr_lo", "irr_hi", "irr_pvalue", "em_pvalue", "ratio_of_irrs",
    "ratio_lo", "ratio_hi"
  )
)

# The key of each accessor's grain. Used by the uniqueness test.
.ACC_KEYS <- list(
  estimates = c("ett_id", "estimand", "weights"),
  curves = c("ett_id", "estimand", "weights", "arm", "band"),
  baselines = c(
    "enrollment_id", "imputation", "weighting", "variant", "variable", "level"
  ),
  # `trial_id` is in the key. `$get_attrition()` returns the per-trial rows and
  # the global row of each criterion, so a key without it duplicates.
  attrition = c("enrollment_id", "trial_id", "step_order"),
  matching = c("enrollment_id", "trial_id"),
  # `subgroup_var` is in the key. Production allows several subgroup variables
  # per ETT, and every variable has its own `"all"` row, so a key without it
  # duplicates on correct data.
  subgroups = c(
    "ett_id", "estimand", "weights", "subgroup_var", "subgroup_level"
  )
)


# --- the six methods exist and take no arguments -----------------------------

test_that("the six methods exist and take no arguments", {
  gen <- swereg::TTEPlan$public_methods
  for (nm in c(
    "get_estimates",
    "get_curves",
    "get_baselines",
    "get_attrition",
    "get_matching",
    "get_subgroups"
  )) {
    expect_true(nm %in% names(gen), info = nm)
    expect_identical(length(formals(gen[[nm]])), 0L, info = nm)
  }
  plan <- .acc_fix_plan()
  for (nm in names(.ACC_EXPECT)) {
    out <- .acc_call(plan, nm)
    expect_true(data.table::is.data.table(out), info = nm)
  }
})


# --- (3) every schema column present, by name --------------------------------

test_that("the schema is complete", {
  plan <- .acc_fix_plan()
  for (nm in names(.ACC_EXPECT)) {
    out <- .acc_call(plan, nm)
    expect_gt(nrow(out), 0L)
    # Exact set AND exact order. A missing column and a stray column are both
    # failures, because a consumer selects by name.
    expect_identical(names(out), .ACC_EXPECT[[nm]], info = nm)
  }
})

test_that("an empty plan returns the same columns as a full one", {
  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52"
  )
  for (nm in names(.ACC_EXPECT)) {
    out <- .acc_call(plan, nm)
    expect_identical(nrow(out), 0L, info = nm)
    expect_identical(names(out), .ACC_EXPECT[[nm]], info = nm)
  }
})


# --- (2) row keys are unique -------------------------------------------------

test_that("row keys are unique", {
  plan <- .acc_fix_plan()
  for (nm in names(.ACC_KEYS)) {
    out <- .acc_call(plan, nm)
    expect_gt(nrow(out), 0L)
    keys <- unique(out[, .ACC_KEYS[[nm]], with = FALSE])
    expect_identical(nrow(keys), nrow(out), info = nm)
  }
})


# --- (1) one row per key combination -----------------------------------------

test_that("get_estimates returns one row per ETT, estimand and weighting", {
  e <- .acc_fix_plan()$get_estimates()
  # ETT00001 holds all three combinations. ETT00002 holds two: it stores
  # nothing at all for per-protocol untruncated.
  expect_identical(nrow(e), 5L)
  expect_identical(
    paste(e$ett_id, e$estimand, e$weights),
    c(
      "ETT00001 pp truncated",
      "ETT00001 pp untruncated",
      "ETT00001 itt untruncated",
      "ETT00002 pp truncated",
      "ETT00002 itt untruncated"
    )
  )
  # Labels join from `plan$ett` and `plan$spec`, which are inputs.
  first <- e[ett_id == "ETT00001" & estimand == "pp" & weights == "truncated"]
  expect_identical(first$enrollment_name, "Enrollment one")
  expect_identical(first$intervention_name, "Treated")
  expect_identical(first$comparator_name, "Untreated")
  expect_identical(first$outcome_role, "primary")
  expect_identical(first$n_events, 42)
  # Values are READ from the stored slots, not recomputed.
  expect_identical(first$events_int, 10.4)
  expect_identical(first$py_cmp, 98765)
  expect_identical(first$irr, 0.54)
  expect_true(first$irr_estimable)
  expect_identical(first$rd, -0.010)
  expect_identical(first$nnt_direction, "benefit")
  expect_identical(first$n_boot, 500)
  expect_identical(first$seed, 1)
  expect_identical(first$conf_level, 0.95)
})

test_that("get_curves returns one row per ETT, estimand, weighting, arm, band", {
  cv <- .acc_fix_plan()$get_curves()
  # ETT00001 carries both curves, ETT00002 carries the ITT curve only.
  # 2 arms x 2 bands x 3 stored curves.
  expect_identical(nrow(cv), 12L)
  expect_setequal(cv$arm, c("comparator", "intervention"))
  expect_setequal(cv$band, c(26, 52))
  expect_identical(
    cv[
      ett_id == "ETT00001" & estimand == "pp" &
        arm == "intervention" & band == 52
    ]$surv,
    0.990
  )
  expect_identical(
    cv[
      ett_id == "ETT00001" & estimand == "pp" &
        arm == "comparator" & band == 52
    ]$surv,
    0.980
  )
})

test_that("get_baselines returns one row per enrollment, panel and table row", {
  bl <- .acc_fix_plan()$get_baselines()
  panel_rows <- nrow(.acc_fix_table1())
  # Four panels are stored. `table1_raw` is not.
  expect_identical(nrow(bl), 4L * panel_rows)
  expect_setequal(
    paste(bl$imputation, bl$weighting, bl$variant),
    c(
      "imputed none supplementary",
      "imputed ipw supplementary",
      "imputed ipw_trunc supplementary",
      "imputed ipw_trunc main"
    )
  )
  expect_true(all(bl$n_baseline == 1000))
  expect_true(all(bl$n_baseline_intervention == 300))
  expect_true(all(bl$n_baseline_comparator == 700))
})

test_that("get_attrition returns one row per enrollment and stored row", {
  at <- .acc_fix_plan()$get_attrition()
  # The fixture stores 8 rows over 3 criteria: two per-trial rows for each
  # criterion, and a global row for two of the three.
  expect_identical(nrow(at), 8L)
  expect_identical(at$trial_id, c(1L, 2L, NA, 1L, 2L, NA, 1L, 2L))
  # `step_order` is the criterion's position, so every row of one criterion
  # shares it whatever its `trial_id`.
  expect_identical(at$step_order, c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L))
  expect_identical(
    at$step_name,
    c(
      "before_exclusions", "before_exclusions", "before_exclusions",
      "age", "age", "age",
      "prior_disease", "prior_disease"
    )
  )
  expect_identical(
    at$n_persons,
    c(3000, 3200, 5000, 2500, 2600, 4000, 1800, 1900)
  )
  expect_identical(
    at$n_person_trials,
    c(25000, 25000, 50000, 20000, 20000, 40000, 15000, 15000)
  )
  expect_identical(
    at$n_arm_intervention,
    c(500, 500, 1000, 450, 450, 900, 400, 400)
  )
  expect_identical(
    at$n_arm_comparator,
    c(2000, 2000, 4000, 1550, 1550, 3100, 1100, 1100)
  )
  # Nothing stores a step kind, so the table carries none.
  expect_false("kind" %in% names(at))

  # The GLOBAL rows are what a caller filtering on `trial_id` gets, and they
  # carry the stored global counts.
  g <- at[is.na(at$trial_id)]
  expect_identical(nrow(g), 2L)
  expect_identical(g$step_name, c("before_exclusions", "age"))
  expect_identical(g$n_persons, c(5000, 4000))
  expect_identical(g$n_person_trials, c(50000, 40000))
})

test_that("get_matching returns one row per enrollment and trial", {
  mt <- .acc_fix_plan()$get_matching()
  # The fixture stores one matching row, for enrollment 01, trial 1.
  expect_identical(nrow(mt), 1L)
  expect_identical(mt$enrollment_id, "01")
  expect_identical(mt$trial_id, 1L)
  expect_identical(mt$n_intervention_total, 800)
  expect_identical(mt$n_comparator_total, 2200)
  expect_identical(mt$n_intervention_enrolled, 300)
  expect_identical(mt$n_comparator_enrolled, 700)
  # The accessor sums nothing across trials and derives no ratio.
  expect_false(any(c("ratio", "n_enrolled") %in% names(mt)))
})

test_that("get_subgroups returns one row per variable, estimand and level", {
  sg <- .acc_fix_plan()$get_subgroups()
  # Five stratified results of 3 levels each, plus three interaction-only
  # results of one row each.
  expect_identical(nrow(sg), 18L)
  expect_identical(unique(sg$ett_id), "ETT00001")
  # The slot name is the only record of the variable and the estimand.
  expect_setequal(
    unique(sg$subgroup_var),
    c("rd_age_band", "rd_parity", "rd_bmi", "rd_smoking", "rd_skipped")
  )
  expect_setequal(
    unique(paste(sg$subgroup_var, sg$estimand, sg$weights)),
    c(
      "rd_age_band pp truncated",
      "rd_age_band itt untruncated",
      "rd_parity pp truncated",
      "rd_bmi pp truncated",
      "rd_bmi itt untruncated",
      "rd_smoking pp truncated",
      "rd_smoking itt untruncated",
      "rd_skipped pp truncated"
    )
  )
  # Every variable and estimand has an "all" row. That is why `subgroup_var`
  # is in the key.
  expect_identical(sum(sg$subgroup_level == "all"), 8L)

  age_pp <- sg[subgroup_var == "rd_age_band" & estimand == "pp"]
  expect_identical(age_pp$subgroup_level, c("all", "younger", "older"))
  expect_identical(age_pp$irr, c(0.54, 0.61, 0.48))
  expect_identical(age_pp$irr_lo, c(0.40, 0.42, 0.31))
  expect_identical(age_pp$irr_hi, c(0.71, 0.88, 0.74))
  expect_identical(age_pp$irr_pvalue, c(0.0000001, 0.008, 0.001))
})


# --- (4) effect and decision fields carry no formatting ----------------------

test_that("effect and decision fields carry no formatting", {
  plan <- .acc_fix_plan()
  e <- plan$get_estimates()
  cv <- plan$get_curves()
  sg <- plan$get_subgroups()

  numeric_cols <- list(
    e = c(
      "n_events", "events_int", "py_int", "rate_int", "events_cmp", "py_cmp",
      "rate_cmp", "persons_event_int", "persons_event_cmp", "irr", "irr_lo",
      "irr_hi", "irr_pvalue", "rd", "rd_lo", "rd_hi", "nnt", "nnt_lo",
      "nnt_hi", "n_boot", "seed", "conf_level"
    ),
    cv = c("band", "surv", "n_persons_at_risk"),
    sg = c(
      "irr", "irr_lo", "irr_hi", "irr_pvalue", "em_pvalue", "ratio_of_irrs",
      "ratio_lo", "ratio_hi"
    )
  )
  tabs <- list(e = e, cv = cv, sg = sg)
  for (tab in names(numeric_cols)) {
    for (nm in numeric_cols[[tab]]) {
      expect_true(
        is.numeric(tabs[[tab]][[nm]]),
        info = paste(tab, nm)
      )
    }
  }
  expect_true(is.logical(e$irr_estimable))

  # The two decision columns are stored vocabulary, not rendered text.
  expect_true(all(
    e$interval_status %in% c("ok", "spans null", "zero-event arm", NA)
  ))
  expect_true(all(e$nnt_direction %in% c("benefit", "harm", NA)))

  # No renderer's marks anywhere in the effect and decision fields. Baseline
  # CELL values are exempt and are not tested here.
  formatted <- "NNTB|NNTH| to |%|<|,|\\("
  for (nm in c("interval_status", "nnt_direction")) {
    expect_false(any(grepl(formatted, e[[nm]])), info = nm)
  }
  for (nm in c(numeric_cols$e, "irr_estimable")) {
    expect_false(
      any(grepl(formatted, format(e[[nm]], scientific = FALSE))),
      info = nm
    )
  }
})


# --- (5a) a missing slot yields absent rows ----------------------------------

test_that("a missing slot yields absent rows", {
  plan <- .acc_fix_plan()

  # ETT00002 stores nothing for per-protocol untruncated, so that combination
  # has NO row. The row count and the key set both say so.
  e <- plan$get_estimates()
  expect_identical(nrow(e), 5L)
  expect_identical(
    nrow(e[ett_id == "ETT00002" & estimand == "pp" & weights == "untruncated"]),
    0L
  )
  expect_setequal(
    paste(e$ett_id, e$estimand, e$weights),
    c(
      "ETT00001 pp truncated",
      "ETT00001 pp untruncated",
      "ETT00001 itt untruncated",
      "ETT00002 pp truncated",
      "ETT00002 itt untruncated"
    )
  )

  # The enrollment stores no `table1_raw`, so there are no raw rows at all.
  bl <- plan$get_baselines()
  expect_identical(nrow(bl), 4L * nrow(.acc_fix_table1()))
  expect_identical(nrow(bl[imputation == "raw"]), 0L)

  # ETT00002 stores no per-protocol curve, so it contributes no pp curve rows.
  cv <- plan$get_curves()
  expect_identical(nrow(cv), 12L)
  expect_setequal(
    unique(paste(cv$ett_id, cv$estimand)),
    c("ETT00001 pp", "ETT00001 itt", "ETT00002 itt")
  )
  expect_identical(nrow(cv[ett_id == "ETT00002" & estimand == "pp"]), 0L)

  # An ETT with no stratified result contributes no subgroup rows.
  sg <- plan$get_subgroups()
  expect_identical(nrow(sg), 18L)
  expect_identical(nrow(sg[ett_id == "ETT00002"]), 0L)
  # `rd_parity` stores neither family on intention-to-treat, so it contributes
  # no intention-to-treat row.
  expect_identical(nrow(sg[subgroup_var == "rd_parity" & estimand == "itt"]), 0L)
  # `rd_absent` is named in the specification and stored nowhere. It gets no
  # row at all. `.write_effect_modification()` emits one all-NA row for it,
  # and that row is a rendering choice the accessor MUST NOT copy.
  expect_true("rd_absent" %in% plan$ett$subgroup_vars[[1L]])
  expect_identical(nrow(sg[subgroup_var == "rd_absent"]), 0L)

  # A plan with no stored attrition yields no attrition rows.
  plan$enrollment_counts <- NULL
  expect_identical(nrow(plan$get_attrition()), 0L)
})


# --- (6) the two subgroup p-values are different quantities ------------------

test_that("the interaction p-value is returned and is not the stratum p-value", {
  sg <- .acc_fix_plan()$get_subgroups()

  # `em_pvalue` is the interaction test: do the strata differ from each other?
  # `irr_pvalue` is the stratum's own p-value: is this stratum's rate ratio
  # distinguishable from the null? The fixture gives them disjoint values, so a
  # substitution shows up as a WRONG NUMBER, not as a missing column.
  age_pp <- sg[subgroup_var == "rd_age_band" & estimand == "pp"]
  expect_identical(age_pp$em_pvalue, rep(0.42, 3L))
  expect_identical(age_pp$irr_pvalue, c(0.0000001, 0.008, 0.001))
  expect_identical(age_pp$ratio_of_irrs, rep(0.79, 3L))
  expect_identical(age_pp$ratio_lo, rep(0.55, 3L))
  expect_identical(age_pp$ratio_hi, rep(1.14, 3L))

  # The interaction test is stored per estimand, so the two estimands report
  # different interaction p-values for the same subgroup variable.
  age_itt <- sg[subgroup_var == "rd_age_band" & estimand == "itt"]
  expect_identical(age_itt$em_pvalue, rep(0.61, 3L))
  expect_identical(age_itt$ratio_of_irrs, rep(0.88, 3L))

  # A second variable carries its own interaction test.
  parity <- sg[subgroup_var == "rd_parity"]
  expect_identical(parity$em_pvalue, rep(0.07, 3L))
  expect_identical(parity$ratio_of_irrs, rep(1.35, 3L))

  # No row anywhere reports the stratum p-value as the interaction p-value.
  expect_false(any(sg$em_pvalue == sg$irr_pvalue, na.rm = TRUE))

  # `rd_bmi` on per-protocol has no stored interaction test. Its stratified
  # table carries POISONED `em_pvalue` and `ratio_of_irrs` attributes, which
  # `$irr_by_subgroup()` writes from its own second run of the test. The
  # accessor reads the stored slot, never the attributes, so all four
  # interaction columns are NA and neither 0.99 nor 9.99 appears.
  bmi_pp <- sg[subgroup_var == "rd_bmi" & estimand == "pp"]
  expect_identical(nrow(bmi_pp), 3L)
  expect_true(all(is.na(bmi_pp$em_pvalue)))
  expect_true(all(is.na(bmi_pp$ratio_of_irrs)))
  expect_true(all(is.na(bmi_pp$ratio_lo)))
  expect_true(all(is.na(bmi_pp$ratio_hi)))
  expect_false(any(sg$em_pvalue == 0.99, na.rm = TRUE))
  expect_false(any(sg$ratio_of_irrs == 9.99, na.rm = TRUE))
  # The stratum columns are unaffected: they come from the stratified table.
  expect_identical(bmi_pp$irr, c(0.54, 0.52, 0.57))
})


# --- (7) the union of the two stored slot families ---------------------------

test_that("an interaction test survives without its stratified companion", {
  plan <- .acc_fix_plan()
  sg <- plan$get_subgroups()

  # `$s3_analyze()` dispatches the stratified rate ratios and the interaction
  # test as separate work items, in separate subprocesses. Either can fail
  # alone, so the accessor iterates the UNION of the two slot families.
  #
  # PERMUTATION SWEEP. Each of the four states, over both estimands.
  #
  # 1. BOTH STORED. Three results, three levels each, every column populated.
  both <- sg[!is.na(irr) & !is.na(em_pvalue)]
  expect_identical(nrow(both), 9L)
  expect_setequal(
    unique(paste(both$subgroup_var, both$estimand)),
    c("rd_age_band pp", "rd_age_band itt", "rd_parity pp")
  )

  # 2. STRATIFIED ONLY. Two results, three levels each, interaction all NA.
  strata_only <- sg[!is.na(irr) & is.na(em_pvalue)]
  expect_identical(nrow(strata_only), 6L)
  expect_setequal(
    unique(paste(strata_only$subgroup_var, strata_only$estimand)),
    c("rd_bmi pp", "rd_smoking itt")
  )
  expect_true(all(is.na(strata_only$ratio_of_irrs)))

  # 3. INTERACTION ONLY. This is the state the accessor used to drop.
  only <- sg[is.na(irr) & !is.na(em_pvalue)]
  expect_identical(nrow(only), 3L)
  expect_identical(
    paste(only$subgroup_var, only$estimand),
    c("rd_bmi itt", "rd_smoking pp", "rd_skipped pp")
  )
  # ONE row each, and its level reads "all". No stored table names the levels,
  # so the accessor invents no stratum row. This matches the single "all" row
  # `.write_effect_modification()` emits in the same state.
  expect_identical(only$subgroup_level, rep("all", 3L))
  # The interaction result is returned BY VALUE, not merely by column.
  expect_identical(only$em_pvalue, c(0.31, 0.88, 0.05))
  expect_identical(only$ratio_of_irrs, c(1.12, 1.02, 2.40))
  expect_identical(only$ratio_lo, c(0.83, 0.76, 1.01))
  expect_identical(only$ratio_hi, c(1.51, 1.37, 5.70))
  # Every stratum column is NA. Nothing invents a rate ratio.
  expect_true(all(is.na(only$irr_lo)))
  expect_true(all(is.na(only$irr_hi)))
  expect_true(all(is.na(only$irr_pvalue)))
  # A SKIPPED stratified worker reads as ABSENT, not as a table. `rd_skipped`
  # stores `list(skipped = TRUE, reason = ...)` and its interaction test.
  expect_true(isTRUE(
    plan$results_ett[["ETT00001"]]$subgroup_rd_skipped_pp$skipped
  ))
  expect_identical(sg[subgroup_var == "rd_skipped"]$em_pvalue, 0.05)

  # 4. NEITHER STORED. No rows, even though the specification names it.
  expect_true("rd_absent" %in% plan$ett$subgroup_vars[[1L]])
  expect_identical(nrow(sg[subgroup_var == "rd_absent"]), 0L)

  # The sweep accounts for every row.
  expect_identical(nrow(both) + nrow(strata_only) + nrow(only), nrow(sg))
})


# --- (5b) the accessors compute nothing --------------------------------------

test_that("the accessors compute nothing", {
  plan <- .acc_fix_plan()
  e <- plan$get_estimates()

  # ETT00002's per-protocol ratio predates the stored decision. Its `IRR` is
  # 0.54, so the estimability RULE would return TRUE. The accessor applies no
  # rule, so it reports NA.
  pp2 <- e[ett_id == "ETT00002" & estimand == "pp" & weights == "truncated"]
  expect_identical(pp2$irr, 0.54)
  expect_true(is.na(pp2$irr_estimable))
  # ETT00001 stored the decision, so the same column reports it.
  expect_true(
    e[ett_id == "ETT00001" & estimand == "pp" & weights == "truncated"]$irr_estimable
  )
  # A skip envelope is a record of a failure, not a result. `irr_itt` on
  # ETT00002 holds one, so the ITT row reports NA in every ratio column.
  itt2 <- e[ett_id == "ETT00002" & estimand == "itt"]
  expect_true(is.na(itt2$irr))
  expect_true(is.na(itt2$irr_lo))
  expect_true(is.na(itt2$irr_estimable))

  # `$get_attrition()` creates no step. The enrollment stores matching counts
  # and a baseline size, and neither becomes an attrition row. Building those
  # two rows is the CONSORT renderer's job.
  at <- plan$get_attrition()
  expect_false(any(at$step_name %in% c(
    "enrolled_after_comparator_draw",
    "analysis_dataset"
  )))
  # The counts are the stored ones, unchanged. The global rows still carry the
  # stored global numbers.
  expect_identical(at[is.na(at$trial_id)]$n_person_trials, c(50000, 40000))
  # `$get_matching()` reports the matching counts and creates no step either.
  expect_false("step_name" %in% names(plan$get_matching()))
})


# --- (5c) attrition returns the stored rows and sums nothing -----------------

test_that("attrition returns the stored rows and sums nothing", {
  plan <- .acc_fix_plan()
  at <- plan$get_attrition()

  # Every stored row reaches the table, and none is created.
  expect_identical(nrow(plan$enrollment_counts[["01"]]$attrition), 8L)
  expect_identical(nrow(at), 8L)

  # `prior_disease` has per-trial rows and NO global row. The accessor returns
  # the two per-trial rows and creates no global row for it. Summing them would
  # give n_persons 3700, counting a person once per trial she enters, and that
  # sum is `.attrition_overall()`'s decision to make.
  pd <- at[at$step_name == "prior_disease"]
  expect_identical(nrow(pd), 2L)
  expect_identical(pd$trial_id, c(1L, 2L))
  expect_false(any(is.na(pd$trial_id)))
  expect_identical(pd$n_persons, c(1800, 1900))
  expect_false(3700 %in% at$n_persons)

  # A global row carries the stored global count, not a sum over its per-trial
  # rows. Summing every stored row for `before_exclusions` gives 11200.
  g <- at[is.na(at$trial_id)]
  expect_identical(g$n_persons, c(5000, 4000))
  expect_identical(g$n_arm_intervention, c(1000, 900))
  expect_false(11200 %in% at$n_persons)
})

test_that("matching returns the stored rows and sums nothing", {
  plan <- .acc_fix_plan()
  stored <- plan$enrollment_counts[["01"]]$matching
  mt <- plan$get_matching()

  expect_identical(nrow(mt), nrow(stored))
  expect_identical(mt$n_intervention_enrolled, as.numeric(
    stored$n_intervention_enrolled
  ))

  # An enrollment that stored no matching table gets no row, and the accessor
  # does not create one from the attrition rows.
  plan$enrollment_counts[["01"]]$matching <- NULL
  expect_identical(nrow(plan$get_matching()), 0L)
  expect_identical(names(plan$get_matching()), .ACC_EXPECT$matching)
})


# --- a spec reload does not overwrite a description ---------------------------

test_that("a spec reload does not overwrite a description", {
  plan <- .acc_fix_plan()
  plan$results_ett[["ETT00001"]]$description <- "as computed"
  new_spec <- plan$spec
  new_spec$outcomes <- list(list(
    name = "Outcome A renamed",
    implementation = list(variable = "osd_a")
  ))
  swereg:::.apply_cosmetic_spec_updates(plan, new_spec)
  # `plan$ett` takes the new label. The cached result keeps its own.
  expect_identical(plan$ett$outcome_name[1], "Outcome A renamed")
  expect_identical(plan$results_ett[["ETT00001"]]$description, "as computed")
  # The accessor reports the CURRENT label, joined from `plan$ett`.
  e <- plan$get_estimates()
  expect_identical(
    unique(e[ett_id == "ETT00001"]$outcome_name),
    "Outcome A renamed"
  )
})


# --- (5d) a stored result with no measurement still reports itself -----------

test_that("a stored result with no estimate slot still yields a row", {
  plan <- .acc_fix_plan()
  # ETT00002 keeps its summary and loses every estimate slot.
  plan$results_ett[["ETT00002"]] <- list(
    enrollment_id = "01",
    description = "ETT00002",
    summary = list(n_events = 7L)
  )
  e <- plan$get_estimates()
  row <- e[e$ett_id == "ETT00002"]
  expect_identical(nrow(row), 1L)
  # The summary is a stored slot, so the count reaches a reader.
  expect_identical(row$n_events, 7)
  # It names no estimand, so nothing measured is claimed for it.
  expect_true(is.na(row$estimand))
  expect_true(is.na(row$weights))
  expect_true(is.na(row$irr))
  expect_false(row$irr_stored)
  # And a consumer that filters on an estimand never sees it.
  expect_identical(nrow(e[which(e$estimand == "pp")][ett_id == "ETT00002"]), 0L)

  # An emulated trial the plan stores NOTHING for still yields no row.
  plan$results_ett[["ETT00002"]] <- list()
  expect_identical(nrow(plan$get_estimates()[ett_id == "ETT00002"]), 0L)
})

test_that("the stored shape of a rate ratio is reported, not guessed", {
  plan <- .acc_fix_plan()
  e <- plan$get_estimates()
  # ETT00001 stores a full ratio for every combination.
  full <- e[ett_id == "ETT00001" & estimand == "pp" & weights == "truncated"]
  expect_true(full$irr_stored)
  expect_true(full$irr_interval_stored)

  # ETT00002 stores a SKIP envelope under `irr_itt`, which reads as absent.
  skipped <- e[ett_id == "ETT00002" & estimand == "itt"]
  expect_false(skipped$irr_stored)
  expect_false(skipped$irr_interval_stored)

  # A stored ratio with no interval columns is stored, and has no interval.
  plan$results_ett[["ETT00001"]]$irr_pp_trunc <- list(IRR = 1)
  e2 <- plan$get_estimates()
  partial <- e2[ett_id == "ETT00001" & estimand == "pp" & weights == "truncated"]
  expect_true(partial$irr_stored)
  expect_false(partial$irr_interval_stored)

  # A stored ratio whose VALUES are all NA is still stored, and still has its
  # interval columns. Absence and a missing value are different facts.
  na_irr <- data.table::data.table(
    IRR = NA_real_, IRR_lower = NA_real_, IRR_upper = NA_real_,
    IRR_pvalue = NA_real_, warn = FALSE
  )
  plan$results_ett[["ETT00001"]]$irr_pp_trunc <- na_irr
  e3 <- plan$get_estimates()
  allna <- e3[ett_id == "ETT00001" & estimand == "pp" & weights == "truncated"]
  expect_true(allna$irr_stored)
  expect_true(allna$irr_interval_stored)
  expect_true(is.na(allna$irr))
})

test_that("an enrollment with counts and no panel still reports its counts", {
  plan <- .acc_fix_plan()
  for (nm in c(
    "table1_raw", "table1_unweighted", "table1_ipw",
    "table1_ipw_trunc", "table1_ipw_trunc_main"
  )) {
    plan$results_enrollment[["01"]][[nm]] <- NULL
  }
  bl <- plan$get_baselines()
  expect_identical(nrow(bl), 1L)
  expect_identical(bl$enrollment_id, "01")
  expect_identical(bl$n_baseline, 1000)
  expect_identical(bl$n_baseline_intervention, 300)
  expect_identical(bl$n_baseline_comparator, 700)
  # It names no panel, so nothing rendered is claimed for it.
  expect_true(is.na(bl$imputation))
  expect_true(is.na(bl$variable))
  expect_true(is.na(bl$comparator_label))

  # An enrollment that stored neither a panel nor a count yields no row.
  plan$results_enrollment[["01"]]$n_baseline <- NULL
  plan$results_enrollment[["01"]]$n_baseline_intervention <- NULL
  plan$results_enrollment[["01"]]$n_baseline_comparator <- NULL
  expect_identical(nrow(plan$get_baselines()), 0L)
})

test_that("the baseline arm labels are the stored panel headers", {
  plan <- .acc_fix_plan()
  bl <- plan$get_baselines()
  # The fixture builds its panels with these two labels.
  expect_identical(unique(bl$comparator_label), "Untreated")
  expect_identical(unique(bl$intervention_label), "Treated")

  # The specification is not consulted. Removing its arm names moves nothing,
  # because the header belongs to the numbers the panel already holds.
  plan$spec$enrollments[[1]]$treatment$arms <- NULL
  bl2 <- plan$get_baselines()
  expect_identical(unique(bl2$comparator_label), "Untreated")
  expect_identical(unique(bl2$intervention_label), "Treated")
})


# --- (5e) absence is a stored SHAPE, never a missing value -------------------

test_that("a stored table with NA values is still a stored table", {
  plan <- .acc_fix_plan()

  # A rates table of the right shape whose six numbers are all NA.
  na_rates <- data.table::data.table(
    rd_tx = c(TRUE, FALSE),
    events_weighted = c(NA_real_, NA_real_),
    py_weighted = c(NA_real_, NA_real_),
    rate_per_100000py = c(NA_real_, NA_real_)
  )
  data.table::setattr(na_rates, "treatment_var", "rd_tx")
  plan$results_ett[["ETT00001"]]$rates_pp_trunc <- na_rates
  e <- plan$get_estimates()
  row <- e[ett_id == "ETT00001" & estimand == "pp" & weights == "truncated"]
  expect_true(row$rates_stored)
  expect_true(is.na(row$events_int))

  # A rates table with only ONE arm row fails the shape check.
  plan$results_ett[["ETT00001"]]$rates_pp_trunc <- na_rates[1L]
  row2 <- plan$get_estimates()[
    ett_id == "ETT00001" & estimand == "pp" & weights == "truncated"
  ]
  expect_false(row2$rates_stored)

  # A risk-difference row is stored whatever its values hold.
  plan2 <- .acc_fix_plan()
  rd <- plan2$results_ett[["ETT00001"]]$rd_pp_trunc
  for (nm in c("rd", "rd_lo", "rd_hi", "nnt")) {
    data.table::set(rd, j = nm, value = NA_real_)
  }
  plan2$results_ett[["ETT00001"]]$rd_pp_trunc <- rd
  rowrd <- plan2$get_estimates()[
    ett_id == "ETT00001" & estimand == "pp" & weights == "truncated"
  ]
  expect_true(rowrd$rd_stored)
  expect_true(is.na(rowrd$rd))

  # A stratified table is stored whatever its estimates hold.
  plan3 <- .acc_fix_plan()
  plan3$results_ett[["ETT00001"]]$subgroup_rd_age_band_pp <- .acc_fix_subgroup(
    c("all", "younger", "older"),
    rep(NA_real_, 3L), rep(NA_real_, 3L),
    rep(NA_real_, 3L), rep(NA_real_, 3L)
  )
  sg <- plan3$get_subgroups()
  st <- sg[
    ett_id == "ETT00001" & subgroup_var == "rd_age_band" & estimand == "pp"
  ]
  expect_identical(nrow(st), 3L)
  expect_true(all(st$strata_stored))
  expect_true(all(is.na(st$irr)))

  # An interaction-only row names no stratum, so it is NOT a stored table.
  io <- sg[
    ett_id == "ETT00001" & subgroup_var == "rd_smoking" & estimand == "pp"
  ]
  expect_identical(nrow(io), 1L)
  expect_false(io$strata_stored)

  # A panel carries its SMD column whatever the values hold.
  plan4 <- .acc_fix_plan()
  t1 <- data.table::copy(plan4$results_enrollment[["01"]]$table1_ipw_trunc)
  data.table::set(t1, j = "smd_numeric", value = NA_real_)
  plan4$results_enrollment[["01"]]$table1_ipw_trunc <- t1
  bl <- plan4$get_baselines()
  panel <- bl[bl$weighting == "ipw_trunc" & bl$variant == "supplementary"]
  expect_true(all(panel$smd_stored))
  expect_true(all(is.na(panel$smd_numeric)))
})
