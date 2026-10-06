# Regression tests for the swereg#51 fixes. One block per fixed behaviour:
#
# 1. The global attrition rows count only rows with a trial key.
# 2. `windowed_no_obs` treats a missing week as not the value.
# 3. `tteplan_read_spec()` refuses an enrollment without a seed.
# 4. The ratio intervals honour `conf_level`, through the s3 worker too.
# 5. The CONSORT analysis step is labelled administrative censoring.
# 7. TARGET item 7a lists the inclusion criteria of the spec.

skip_if_not_installed("data.table")

# --- 1. global attrition = sum of the per-trial attrition --------------------

# Every person has an annual row, which carries no trial key. Person C has
# only annual rows and enters no trial.
.s51_attrition_skeleton <- function() {
  return(data.table::data.table(
    person_id = c("A", "A", "A", "B", "B", "B", "C", "C", "D", "D"),
    period_id = c(NA, 1L, 2L, NA, 1L, 2L, NA, NA, NA, 1L),
    incl_age = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    excl_x = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    rd_intervention = c(
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE
    )
  ))
}

test_that("global attrition person-trials equal the per-trial sum at every step", {
  attr <- swereg:::.s1_compute_attrition(
    skeleton = .s51_attrition_skeleton(),
    eligible_cols = c("incl_age", "excl_x"),
    pid = "person_id"
  )
  glob <- attr[is.na(enrollment_period_id)]
  per <- attr[
    !is.na(enrollment_period_id),
    .(
      n_person_trials = sum(n_person_trials),
      n_intervention = sum(n_intervention),
      n_comparator = sum(n_comparator)
    ),
    by = criterion
  ]
  m <- merge(glob, per, by = "criterion", suffixes = c("_glob", "_per"))
  expect_setequal(m$criterion, c("before_exclusions", "incl_age", "excl_x"))
  expect_identical(m$n_person_trials_glob, m$n_person_trials_per)
  expect_identical(m$n_intervention_glob, m$n_intervention_per)
  expect_identical(m$n_comparator_glob, m$n_comparator_per)
  # Person C has no trial, so the global head count is A, B and D.
  expect_identical(
    glob[criterion == "before_exclusions", n_persons],
    3L
  )
})

# --- 2. windowed_no_obs: a missing week is not the value ---------------------

test_that("windowed_no_obs does not count a missing week as the value", {
  dt <- data.table::data.table(
    id = c(1L, 1L, 1L, 2L, 2L, 2L),
    isoyearweek = c(
      "2008-01",
      "2008-02",
      "2008-03",
      "2008-01",
      "2008-02",
      "2008-03"
    ),
    grp = c(NA, "b", "b", "a", "b", NA)
  )
  specs <- list(
    list(
      col_name = "no_a",
      type = "windowed_no_obs",
      source_var = "grp",
      value = "a",
      window_weeks = 9L
    )
  )
  dt <- swereg:::.tte_apply_eligibility_batch(dt, specs, id_col = "id")
  # Person 1 never has "a": the missing first week MUST NOT make the later
  # weeks NA. Person 2 has "a" in week 1, so weeks 2 and 3 fail.
  expect_identical(dt$no_a, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

# --- 3. the seed is required --------------------------------------------------

test_that("tteplan_read_spec() refuses an enrollment without a seed", {
  skip_if_not_installed("yaml")
  path <- withr::local_tempfile(fileext = ".yaml")
  ttm_write_spec(path, "s51", "ri_highrisk")
  spec <- yaml::read_yaml(path)
  expect_false(is.null(spec$enrollments[[1]]$treatment$implementation$seed))
  expect_no_error(swereg::tteplan_read_spec(path))
  spec$enrollments[[1]]$treatment$implementation$seed <- NULL
  yaml::write_yaml(spec, path)
  expect_error(
    swereg::tteplan_read_spec(path),
    "enrollments[1] 'Treated vs control' is missing treatment$implementation$seed",
    fixed = TRUE
  )
})

# --- 4. conf_level sets every ratio interval ----------------------------------

# A binary subgroup Z with a planted interaction, as in
# test-tte_effect_modification.R. Treatment is randomised.
.s51_em_trial <- function() {
  set.seed(51)
  n <- 4000L
  z <- stats::rbinom(n, 1, 0.5)
  a <- stats::rbinom(n, 1, 0.5)
  out <- lapply(seq_len(10L), function(t) {
    haz <- stats::plogis(-3.5 + log(2) * a + 0.3 * z + log(2) * a * z)
    return(data.table::data.table(
      id = seq_len(n),
      tstart = t - 1L,
      tstop = t,
      treatment = as.logical(a),
      Z = z,
      event = stats::rbinom(n, 1, haz),
      person_weeks = 1L,
      w = 1
    ))
  })
  design <- TTEDesign$new(
    id_var = "id",
    person_id_var = "id",
    treatment_var = "treatment",
    outcome_vars = "event",
    confounder_vars = "Z",
    follow_up_time = 10L
  )
  return(TTEEnrollment$new(
    data.table::rbindlist(out),
    design,
    data_level = "trial"
  ))
}

# The width of a Wald interval on the log scale is proportional to its
# normal critical value. At a fixed fit, the 90 percent width over the 95
# percent width is therefore qnorm(0.95) / qnorm(0.975).
.s51_width_ratio <- qnorm(0.95) / qnorm(0.975)

.s51_log_width <- function(lo, hi) {
  return(log(hi) - log(lo))
}

test_that("$irr(), $irr_by_subgroup() and $effect_modification_test() honour conf_level", {
  skip_on_cran()
  skip_if_not_installed("survey")
  trial <- .s51_em_trial()

  i95 <- trial$irr(weight_col = "w")
  i90 <- trial$irr(weight_col = "w", conf_level = 0.90)
  expect_equal(i90$IRR, i95$IRR)
  expect_equal(
    .s51_log_width(i90$IRR_lower, i90$IRR_upper) /
      .s51_log_width(i95$IRR_lower, i95$IRR_upper),
    .s51_width_ratio,
    tolerance = 1e-10
  )

  s95 <- trial$irr_by_subgroup("w", "Z")
  s90 <- trial$irr_by_subgroup("w", "Z", conf_level = 0.90)
  expect_equal(s90$IRR, s95$IRR)
  expect_equal(
    .s51_log_width(s90$IRR_lower, s90$IRR_upper) /
      .s51_log_width(s95$IRR_lower, s95$IRR_upper),
    rep(.s51_width_ratio, 3L),
    tolerance = 1e-10
  )

  e95 <- trial$effect_modification_test("w", "Z")
  e90 <- trial$effect_modification_test("w", "Z", conf_level = 0.90)
  expect_equal(e90$ratio_of_irrs, e95$ratio_of_irrs)
  expect_equal(
    .s51_log_width(e90$ratio_lower, e90$ratio_upper) /
      .s51_log_width(e95$ratio_lower, e95$ratio_upper),
    .s51_width_ratio,
    tolerance = 1e-10
  )
})

test_that("the s3 worker passes conf_level to every ratio method", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("qs2")
  trial <- .s51_em_trial()
  path <- withr::local_tempfile(fileext = ".qs2")
  swereg::qs2_write_atomic(trial, path)
  worker <- function(method, subgroup_var = NULL) {
    return(suppressWarnings(swereg:::.s3_ett_worker(
      analysis_path = path,
      method = method,
      weight_col = "w",
      ett_id = "ETT00001",
      n_threads = 1L,
      subgroup_var = subgroup_var,
      conf_level = 0.90
    )))
  }

  irr <- worker("irr")[["irr_w"]]
  ref <- trial$irr(weight_col = "w", conf_level = 0.90)
  expect_equal(irr$IRR_lower, ref$IRR_lower, tolerance = 1e-10)
  expect_equal(irr$IRR_upper, ref$IRR_upper, tolerance = 1e-10)

  sub <- worker("irr_by_subgroup", "Z")[["subgroup_Z_pp"]]
  ref <- trial$irr_by_subgroup("w", "Z", conf_level = 0.90)
  expect_equal(sub$IRR_lower, ref$IRR_lower, tolerance = 1e-10)
  expect_equal(sub$IRR_upper, ref$IRR_upper, tolerance = 1e-10)

  emt <- worker("effect_modification_test", "Z")[["emtest_Z_pp"]]
  ref <- trial$effect_modification_test("w", "Z", conf_level = 0.90)
  expect_equal(emt$ratio_lower, ref$ratio_lower, tolerance = 1e-10)
  expect_equal(emt$ratio_upper, ref$ratio_upper, tolerance = 1e-10)
})

# --- 5. the CONSORT analysis step is administrative censoring -----------------

test_that("the CONSORT analysis step reads administrative end of follow-up", {
  ec <- list(
    attrition = data.table::data.table(
      enrollment_period_id = NA_integer_,
      criterion = c("before_exclusions", "eligible_age"),
      n_persons = c(100, 80),
      n_person_trials = c(500, 400),
      n_intervention = c(100, 80),
      n_comparator = c(400, 320)
    )
  )
  flow <- swereg:::.build_cohort_flow(ec, analysis_n = 390)
  expect_identical(
    flow$change_kind[flow$kind == "analysis"],
    "censored (administrative end of follow-up)"
  )
})

# --- 7. TARGET item 7a lists the spec's inclusion criteria -------------------

test_that("TARGET item 7a lists the inclusion criteria of the spec", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  dir_spec <- file.path(dir, "spec")
  dir_tteplan <- file.path(dir, "tteplan")
  dir_results <- file.path(dir, "results")
  dir_meta <- file.path(dir, "meta")
  for (d in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  sk <- ttm_skeleton(
    "A",
    n_persons = 20L,
    date_max = "2016-12-31",
    n_init_periods = 4L
  )
  skel <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel)
  ttm_write_spec(file.path(dir_spec, "spec_v001.yaml"), "s51", "ri_highrisk")
  plan <- suppressMessages(swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = max(sk$isoyearweek, na.rm = TRUE),
    check_skeletons = FALSE
  ))
  txt <- paste(
    utils::capture.output(plan$print_target_checklist()),
    collapse = "\n"
  )
  m <- regexpr(
    "Eligibility \\(6a\\):.*?(?=Treatment strategies \\(6b\\):)",
    txt,
    perl = TRUE
  )
  seg <- regmatches(txt, m)
  expect_length(seg, 1L)
  # The spec holds global ISO years 2016-2021 and, in enrollment '01', an
  # age range of 40-80 on `rd_age_continuous`.
  expect_true(grepl(
    paste0(
      "The inclusion criteria were: ISO years: 2016-2021; in enrollment ",
      "'01', Age: 40-80 (variable: rd_age_continuous). "
    ),
    seg,
    fixed = TRUE
  ))
  expect_false(grepl("calendar year range, age", seg, fixed = TRUE))
})
