# A person-trial that deviates from its assigned strategy at time zero
# =============================================================================
#
# With a 0-week tolerance, a person-trial that is discordant in its first
# follow-up week gets `censor_week == 0` in `s5_prepare_outcome()`, so the
# per-protocol analysis file keeps no row of it. The intention-to-treat file
# keeps it, and `$time_zero_deviation` records it for the time-zero censoring
# factor. So the per-protocol analysis set is smaller than the enrolled set.
#
# This file pins:
#
#   1. set membership, on the real `$s4_prepare_for_analysis()` path: the
#      deviator is absent from the per-protocol set, present in the
#      intention-to-treat set, and present in the time-zero record;
#   2. the CONSORT flow data: the step between "Enrolled" and the per-protocol
#      analysis set carries the time-zero deviation label and the per-arm
#      count, which counts the deviator once;
#   3. the attrition sheet and the enrollment summary sentence;
#   4. the drawn CONSORT diagram: the deviation is its own censoring box
#      between the "Enrolled" box and the per-protocol analysis box.
#
# The fixture is the worked skeleton of `vignette("tte-timing")`: 16 weeks,
# `period_width = 4`, `follow_up_time = 12`, one enrollment period. It holds
# three named intervention person-trials and 20 concordant fillers:
#
#   ZERO      discordant from follow-up week 1, so it deviates at time zero
#   LOST      absent from follow-up week 7, so loss censors it at week 6
#   ADHERENT  concordant and observed through the horizon

skip_if_not_installed("data.table")
skip_if_not_installed("openxlsx")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")

.TZD_LABEL <- paste0(
  "Deviated from assigned strategy at time zero: ",
  "no per-protocol follow-up; included in intention-to-treat"
)
.TZD_PW <- 4L
.TZD_FU <- 12L

.tzd_weeks <- function(n = 16L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  s <- wk[isoyearweek >= "2020-01" & (idx - 1L) %% .TZD_PW == 0L]$idx[1]
  return(wk$isoyearweek[s:(s + n - 1L)])
}

# One person, one row per week. `discordant_fu` and `absent_fu` name
# 1-indexed follow-up weeks of the first enrollment period. `period` is the
# enrollment period the person is eligible in: 0 for weeks 1 to 4, 1 for weeks
# 5 to 8.
.tzd_person <- function(
  id,
  arm,
  discordant_fu = integer(0),
  absent_fu = integer(0),
  period = 0L
) {
  weeks <- .tzd_weeks()
  n <- length(weeks)
  fu <- seq_len(n) - .TZD_PW
  on_tx <- rep(arm, n)
  on_tx[fu %in% discordant_fu] <- !arm
  d <- data.table::data.table(
    id = id,
    isoyearweek = weeks,
    exposed = arm,
    eligible = seq_len(n) > period * .TZD_PW &
      seq_len(n) <= (period + 1L) * .TZD_PW,
    died = FALSE,
    on_tx = on_tx,
    age = 50 + seq_len(n)
  )
  return(d[!(fu %in% absent_fu)])
}

# `late = TRUE` adds LATE_I and LATE_C, one person-trial in each arm of the
# second enrollment period. Its time zero is week 9.
.tzd_skeleton <- function(late = FALSE) {
  fillers <- c(
    lapply(seq_len(8L), function(i) .tzd_person(paste0("FI", i), TRUE)),
    lapply(seq_len(12L), function(i) .tzd_person(paste0("FC", i), FALSE))
  )
  named <- list(
    .tzd_person("ZERO", TRUE, discordant_fu = 1:12),
    .tzd_person("LOST", TRUE, absent_fu = 7:12),
    .tzd_person("ADHERENT", TRUE)
  )
  if (late) {
    named <- c(
      named,
      list(
        .tzd_person("LATE_I", TRUE, period = 1L),
        .tzd_person("LATE_C", FALSE, period = 1L)
      )
    )
  }
  return(data.table::rbindlist(c(named, fillers)))
}

.tzd_design <- function(admin = NULL) {
  return(TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = .TZD_FU,
    period_width = .TZD_PW,
    intervention_tolerance_weeks = 0L,
    comparator_tolerance_weeks = 0L,
    admin_censor_isoyearweek = admin
  ))
}

# Enroll, then prepare one estimand. `enrolled` is the person-trial table at
# enrollment, before `s5_prepare_outcome()` drops any row. The fixture is
# small, so the weight models separate and warn. The warning is about the toy
# data, and in the late scenario it also reports the trials that enter at or
# after the administrative end.
.tzd_prepare <- function(estimand, late = FALSE, admin = NULL) {
  trial <- TTEEnrollment$new(
    data = .tzd_skeleton(late),
    design = .tzd_design(admin),
    ratio = 2,
    seed = 4,
    extra_cols = "isoyearweek"
  )
  enrolled <- unique(trial$data[, list(
    id,
    enrollment_person_trial_id,
    enrollment_period_id,
    exposed
  )])
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    # s3 reads `ipw_trunc` on the intention-to-treat file.
    trial$s3_truncate_weights()
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = .TZD_FU,
      estimand = estimand,
      estimate_ipcw_pp_with_gam = FALSE
    )
  })
  return(list(trial = trial, enrolled = enrolled))
}

# One scenario: both estimands, the counts the plan stores, and a stand-in for
# the plan. The production code builds every count:
#   - `n_baseline` and its arm split come from `.s3_enrollment_worker()` on the
#     saved per-protocol file, as s3 stores them;
#   - the intention-to-treat rates come from `.s3_ett_worker(method =
#     "rates")` on the saved intention-to-treat file, as s3 stores
#     `rates_itt`, and `.acc_rates_arms()` reads `n_trials` from them as
#     `$get_estimates()` does;
#   - the enrolled counts are the per-arm person-trials at enrollment, as s1
#     stores them in the comparator-draw table.
# The stand-in answers the four accessors the renderers read. It carries no
# spec, so every label comes from the fixed lookup.
.tzd_scenario <- function(late = FALSE, admin = NULL) {
  pp <- .tzd_prepare("pp", late, admin)
  itt <- .tzd_prepare("itt", late, admin)
  dir <- withr::local_tempdir(.local_envir = teardown_env())
  pp_path <- file.path(dir, "analysis_pp.qs2")
  itt_path <- file.path(dir, "analysis_itt.qs2")
  swereg::qs2_write_atomic(pp$trial, pp_path)
  swereg::qs2_write_atomic(itt$trial, itt_path)
  res <- suppressWarnings(swereg:::.s3_enrollment_worker(
    analysis_path = pp_path,
    raw_path = file.path(dir, "absent_raw.qs2"),
    enrollment_id = "E1",
    n_threads = 1L
  ))
  rates_itt <- swereg:::.s3_ett_worker(
    analysis_path = itt_path,
    ett_id = "ETT1",
    n_threads = 1L,
    method = "rates",
    weight_col = "ipw_trunc",
    subgroup_var = NULL,
    conf_level = 0.95
  )[["rates_itt"]]
  itt_arms <- swereg:::.acc_rates_arms(rates_itt)
  enr <- pp$enrolled
  matching <- enr[,
    list(
      n_intervention_enrolled = sum(exposed == TRUE),
      n_comparator_enrolled = sum(exposed == FALSE)
    ),
    keyby = enrollment_period_id
  ]
  n_int <- sum(enr$exposed == TRUE)
  n_cmp <- sum(enr$exposed == FALSE)
  counts <- list(
    attrition = data.table::data.table(
      enrollment_id = "E1",
      enrollment_period_id = NA_integer_,
      step_order = 1:2,
      step_name = c("before_exclusions", "landmark_event_free"),
      n_persons = c(40, n_int + n_cmp),
      n_person_trials = c(40, n_int + n_cmp),
      n_arm_intervention = c(15, n_int),
      n_arm_comparator = c(25, n_cmp)
    ),
    matching = data.table::data.table(
      enrollment_id = "E1",
      enrollment_period_id = matching$enrollment_period_id,
      n_intervention_total = matching$n_intervention_enrolled,
      n_comparator_total = matching$n_comparator_enrolled,
      n_intervention_enrolled = matching$n_intervention_enrolled,
      n_comparator_enrolled = matching$n_comparator_enrolled
    ),
    baselines = data.table::data.table(
      enrollment_id = "E1",
      n_baseline = res$n_baseline,
      n_baseline_intervention = res$n_baseline_intervention,
      n_baseline_comparator = res$n_baseline_comparator
    ),
    estimates = data.table::data.table(
      ett_id = "ETT1",
      enrollment_id = "E1",
      estimand = "itt",
      n_trials_int = itt_arms$n_trials_int,
      n_trials_cmp = itt_arms$n_trials_cmp
    )
  )
  plan <- list(
    spec = NULL,
    get_attrition = function() counts$attrition,
    get_matching = function() counts$matching,
    get_baselines = function() counts$baselines,
    get_estimates = function() counts$estimates
  )
  return(list(pp = pp, itt = itt, counts = counts, plan = plan))
}

.tzd <- .tzd_scenario()
.tzd_pp <- .tzd$pp
.tzd_itt <- .tzd$itt
.tzd_counts <- .tzd$counts
.tzd_plan <- .tzd$plan

# The late scenario. The administrative end is the last week of the second
# enrollment period, week 8. The first period's trials keep 4 weeks of
# follow-up. LATE_I and LATE_C enter at week 9, after it.
.tzd_late <- .tzd_scenario(late = TRUE, admin = .tzd_weeks()[8L])

.tzd_pt <- function(person, sc = .tzd) {
  return(sc$pp$enrolled[id == person]$enrollment_person_trial_id)
}

.tzd_flow <- function(sc = .tzd) {
  return(swereg:::.plan_cohort_flow(sc$plan, "E1"))
}

.tzd_dot <- function(sc = .tzd) {
  return(swereg:::.build_consort_dot(
    flow = .tzd_flow(sc),
    eid = "E1",
    label = "Test enrollment",
    intervention_label = "Treated",
    comparator_label = "Untreated"
  ))
}


test_that("a time-zero deviator leaves the per-protocol set and stays in the ITT set", {
  pp_ids <- unique(.tzd_pp$trial$data$enrollment_person_trial_id)
  itt_ids <- unique(.tzd_itt$trial$data$enrollment_person_trial_id)
  zero <- .tzd_pt("ZERO")
  lost <- .tzd_pt("LOST")
  adherent <- .tzd_pt("ADHERENT")
  expect_length(zero, 1L)

  # Every enrolled person-trial is in the ITT set. The per-protocol set is the
  # ITT set less the one deviator.
  expect_setequal(itt_ids, .tzd_pp$enrolled$enrollment_person_trial_id)
  expect_true(all(pp_ids %in% itt_ids))
  expect_identical(setdiff(itt_ids, pp_ids), zero)

  expect_false(zero %in% pp_ids)
  expect_true(zero %in% itt_ids)
  expect_true(lost %in% pp_ids)
  expect_true(adherent %in% pp_ids)

  # The time-zero record holds the deviator with `deviation_time_zero = 1`, and
  # the two other named person-trials with 0. ITT keeps no record.
  tz <- .tzd_pp$trial$time_zero_deviation
  expect_identical(tz[enrollment_person_trial_id == zero]$deviation_time_zero, 1L)
  expect_identical(tz[enrollment_person_trial_id == lost]$deviation_time_zero, 0L)
  expect_identical(
    tz[enrollment_person_trial_id == adherent]$deviation_time_zero,
    0L
  )
  expect_identical(sum(tz$deviation_time_zero), 1L)
  expect_null(.tzd_itt$trial$time_zero_deviation)

  # LOST is censored for another reason in a later week, and stays in the
  # per-protocol set.
  lost_rows <- .tzd_pp$trial$data[enrollment_person_trial_id == lost]
  expect_identical(max(lost_rows$tstop), 6L)
  expect_identical(sum(lost_rows$censor_loss), 1L)
  expect_identical(sum(lost_rows$censor_deviation), 0L)
})


test_that("the CONSORT flow labels the step as deviation at time zero and counts it per arm", {
  b <- .tzd_counts$baselines
  # The production worker counts the per-protocol set: 10 intervention and
  # 12 comparator person-trials, out of 11 and 12 enrolled.
  expect_identical(as.integer(b$n_baseline), 22L)
  expect_identical(as.integer(b$n_baseline_intervention), 10L)
  expect_identical(as.integer(b$n_baseline_comparator), 12L)

  flow <- .tzd_flow()
  expect_identical(
    flow$step,
    c(
      "before_exclusions",
      "landmark_event_free",
      "enrolled_after_comparator_draw",
      "analysis_dataset"
    )
  )
  # The intention-to-treat set equals the enrolled set, so the flow holds no
  # administrative-end step.
  expect_false("itt_analysis" %in% flow$kind)
  a <- flow[kind == "analysis"]
  expect_identical(a$change_kind, .TZD_LABEL)
  # The deviator is counted once, in its own arm.
  expect_identical(a$change_person_trials, 1)
  expect_identical(a$change_intervention, 1)
  expect_identical(a$change_comparator, 0)
  expect_false(any(grepl("administrative end", flow$change_kind, fixed = TRUE)))
})


test_that("the attrition sheet and the enrollment summary carry the label and the per-arm count", {
  wb <- openxlsx::createWorkbook()
  expect_true(swereg:::.write_attrition_sheet(wb, "Attrition_E1", .tzd_plan, "E1"))
  sheet <- openxlsx::readWorkbook(wb, "Attrition_E1", startRow = 3L)
  expect_identical(
    names(sheet),
    c(
      "step",
      "kind",
      "n_persons",
      "n_person_trials",
      "change_persons",
      "change_person_trials",
      "change_kind",
      "n_intervention",
      "n_comparator",
      "step_label",
      "change_intervention",
      "change_comparator"
    )
  )
  a <- sheet[sheet$step == "analysis_dataset", ]
  expect_identical(nrow(a), 1L)
  expect_identical(a$change_kind, .TZD_LABEL)
  expect_identical(as.numeric(a$change_person_trials), 1)
  expect_identical(as.numeric(a$change_intervention), 1)
  expect_identical(as.numeric(a$change_comparator), 0)

  summary <- swereg:::.format_enrollment_summary(.tzd_plan, "E1")
  expect_match(
    summary,
    paste0(
      "Deviated from assigned strategy at time zero: 1 person-trials ",
      "(intervention: 1 / comparator: 0). No per-protocol follow-up; ",
      "included in intention-to-treat."
    ),
    fixed = TRUE
  )
  expect_false(grepl("administrative end", summary, fixed = TRUE))
  expect_false(grepl("first-period censoring", summary, fixed = TRUE))
})


test_that("the drawn CONSORT diagram shows the deviation as its own censoring box", {
  dot <- .tzd_dot()
  lines <- strsplit(dot, "\n", fixed = TRUE)[[1]]
  # No trial enters after the administrative end, so that box is absent.
  expect_false(any(grepl("admin_end", lines, fixed = TRUE)))
  node <- grep("^  deviated \\[label = ", lines, value = TRUE)
  expect_length(node, 1L)
  expect_match(
    node,
    paste0(
      "label = 'Deviated from assigned strategy at time zero:\\n",
      "no per-protocol follow-up; included in intention-to-treat\\n",
      "1 person-trials\\n",
      "(Treated: 1 person-trials, Untreated: 0 person-trials)'"
    ),
    fixed = TRUE
  )
  # A censoring box, not the red exclusion fill.
  expect_false(grepl("#FDEAEA", node, fixed = TRUE))
  # It hangs off the edge between the "Enrolled" box and the analysis box.
  expect_true("  drawn -> tz_split [arrowhead = none];" %in% lines)
  expect_true("  tz_split -> deviated [constraint = false];" %in% lines)
  expect_true("  tz_split -> analysis;" %in% lines)
  expect_false("  drawn -> analysis;" %in% lines)
  # The red "Excluded" box does not hold it.
  excluded <- grep("^  e1 \\[label = ", lines, value = TRUE)
  expect_false(grepl("Deviated", excluded, fixed = TRUE))

  # Graphviz draws the box. The text reaches the SVG.
  skip_if_not_installed("DiagrammeR")
  skip_if_not_installed("DiagrammeRsvg")
  # The SVG writes each hyphen as the entity `&#45;`.
  svg <- gsub(
    "&#45;",
    "-",
    DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot)),
    fixed = TRUE
  )
  expect_match(svg, "Deviated from assigned strategy at time zero:", fixed = TRUE)
  expect_match(
    svg,
    "no per-protocol follow-up; included in intention-to-treat",
    fixed = TRUE
  )
})


test_that("a trial that enters at or after the administrative end is its own step", {
  sc <- .tzd_late
  pp_ids <- unique(sc$pp$trial$data$enrollment_person_trial_id)
  itt_ids <- unique(sc$itt$trial$data$enrollment_person_trial_id)
  late_i <- .tzd_pt("LATE_I", sc)
  late_c <- .tzd_pt("LATE_C", sc)
  zero <- .tzd_pt("ZERO", sc)
  expect_length(late_i, 1L)
  expect_length(late_c, 1L)

  # Both late person-trials are enrolled, and neither analysis file keeps
  # them. ZERO is in the intention-to-treat set and not in the per-protocol
  # set.
  expect_false(late_i %in% itt_ids)
  expect_false(late_c %in% itt_ids)
  expect_false(late_i %in% pp_ids)
  expect_true(zero %in% itt_ids)
  expect_false(zero %in% pp_ids)
  expect_identical(
    sort(setdiff(sc$pp$enrolled$enrollment_person_trial_id, itt_ids)),
    sort(c(late_i, late_c))
  )

  flow <- .tzd_flow(sc)
  expect_identical(
    flow$step,
    c(
      "before_exclusions",
      "landmark_event_free",
      "enrolled_after_comparator_draw",
      "analysis_dataset_itt",
      "analysis_dataset"
    )
  )
  adm <- flow[kind == "itt_analysis"]
  expect_identical(
    adm$change_kind,
    paste0(
      "Entered at or after the administrative end of follow-up: ",
      "no follow-up in either analysis"
    )
  )
  expect_identical(adm$change_person_trials, 2)
  expect_identical(adm$change_intervention, 1)
  expect_identical(adm$change_comparator, 1)
  # The deviation count is exact: the intention-to-treat set minus the
  # per-protocol set. The two late person-trials are not in it.
  dev <- flow[kind == "analysis"]
  expect_identical(dev$change_kind, .TZD_LABEL)
  expect_identical(dev$change_person_trials, 1)
  expect_identical(dev$change_intervention, 1)
  expect_identical(dev$change_comparator, 0)

  # The attrition sheet carries the step and its label.
  wb <- openxlsx::createWorkbook()
  expect_true(swereg:::.write_attrition_sheet(wb, "Attrition_E1", sc$plan, "E1"))
  sheet <- openxlsx::readWorkbook(wb, "Attrition_E1", startRow = 3L)
  row <- sheet[sheet$step == "analysis_dataset_itt", ]
  expect_identical(row$change_kind, adm$change_kind)
  expect_identical(row$step_label, "Analysis dataset (intention-to-treat)")
  expect_identical(as.numeric(row$change_intervention), 1)
  expect_identical(as.numeric(row$change_comparator), 1)

  # The diagram draws it as its own censoring box above the deviation box.
  lines <- strsplit(.tzd_dot(sc), "\n", fixed = TRUE)[[1]]
  node <- grep("^  admin_end \\[label = ", lines, value = TRUE)
  expect_length(node, 1L)
  expect_match(
    node,
    paste0(
      "label = 'Entered at or after the administrative end of follow-up:\\n",
      "no follow-up in either analysis\\n",
      "2 person-trials\\n",
      "(Treated: 1 person-trials, Untreated: 1 person-trials)'"
    ),
    fixed = TRUE
  )
  expect_true("  drawn -> admin_split [arrowhead = none];" %in% lines)
  expect_true("  admin_split -> tz_split [arrowhead = none];" %in% lines)
  expect_true("  tz_split -> analysis;" %in% lines)
})


test_that("a missing intention-to-treat count leaves both groups in one step", {
  # The emulated trial stored no intention-to-treat rates, so
  # `$get_estimates()` reports `NA` for both arm counts.
  missing <- swereg:::.acc_rates_arms(NULL)
  expect_true(is.na(missing$n_trials_int))
  plan <- .tzd_plan
  plan$get_estimates <- function() {
    return(data.table::data.table(
      ett_id = "ETT1",
      enrollment_id = "E1",
      estimand = "itt",
      n_trials_int = missing$n_trials_int,
      n_trials_cmp = missing$n_trials_cmp
    ))
  }
  expect_null(swereg:::.itt_analysis_counts(plan, "E1"))

  flow <- swereg:::.plan_cohort_flow(plan, "E1")
  expect_false("itt_analysis" %in% flow$kind)
  a <- flow[kind == "analysis"]
  expect_identical(
    a$change_kind,
    paste0(
      "Deviated from assigned strategy at time zero, or entered at or after ",
      "the administrative end of follow-up: no per-protocol follow-up"
    )
  )
  # The count is enrolled minus per-protocol, which is all it can be.
  expect_identical(a$change_person_trials, 1)
})
