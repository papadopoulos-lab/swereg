# Time zero moves to the landmark, and baseline covariates are read at the
# recruiting week.
#
# Phase 8 removed the person-bands that cannot qualify at the landmark. It left
# follow-up starting at the entry band, so a person-band still carried
# within-band immortal time of up to `period_width - 1` weeks. Worse: landmark
# qualification makes the WHOLE entry band immortal, because a person must
# survive it, event-free and under observation, to enroll at all.
#
# This file pins four properties.
#
# 1. The panel starts one band after the entry band, and its first row has
#    `tstart == 0`. `entry_band_id` stays on the panel and names the trial.
# 2. Each confounder reaches the panel twice. `.tte_entry__<v>` holds the value
#    at the recruiting week. `<v>` holds the time-updated value of the
#    follow-up band.
# 3. `$s2_ipw()` fits the propensity score on `.tte_entry__<v>`, and not on the
#    landmark-band value that now sits at `tstart == 0`.
# 4. `.s1d_worker()` hands `impute_fn` the `.tte_entry__` names. Imputation is
#    name-list driven, so the plain names would leave the snapshot unimputed.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")

.lpb_pw <- 4L

# Consecutive ISO year-weeks starting on a band boundary. Twelve of them make
# three whole bands under `period_width = 4`.
.lpb_weeks <- function(n_weeks = 12L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% .lpb_pw == 0L
  ]$idx[1]
  wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)]
}

# The trial_id of the first band, read from `.assign_trial_ids()` itself rather
# than hard-coded.
.lpb_band0 <- function(weeks) {
  d <- data.table::data.table(id = 1L, isoyearweek = weeks)
  swereg:::.assign_trial_ids(d, .lpb_pw)
  min(d$trial_id)
}

# One person, one row per week.
#
# `tx_week` is the 1-indexed week she initiates. She is outside both arms
# before it, exactly as production data is: `rd_intervention` reads NA while
# `rd_tx` is "none". `NA` is what makes the recruiting week her initiation
# week. A comparator holds FALSE in every week and never initiates.
#
# `elig_from` is the first week she is eligible. Eligibility stops at the end
# of the entry band, so only band 0 recruits anybody and every person-trial in
# the fixture is a band-0 trial.
#
# `age` rises by `age_step` every week, so the band start, the recruiting week
# and the landmark band are three different values.
.lpb_person <- function(
  id,
  weeks,
  tx_week = NA_integer_,
  elig_from = 1L,
  age_base = 100L,
  age_step = 1L
) {
  w <- seq_along(weeks)
  exposed <- if (is.na(tx_week)) {
    rep(FALSE, length(w))
  } else {
    data.table::fifelse(w < tx_week, NA, TRUE)
  }
  data.table::data.table(
    id = id,
    isoyearweek = weeks,
    exposed = exposed,
    eligible = w >= elig_from & w <= .lpb_pw,
    died = FALSE,
    age = age_base + age_step * (w - 1L)
  )
}

.lpb_design <- function(follow_up_time = 8L) {
  TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = follow_up_time,
    period_width = .lpb_pw
  )
}

.lpb_enroll <- function(d, design, ratio = 2, seed = 4) {
  TTEEnrollment$new(
    data = data.table::copy(d),
    design = design,
    ratio = ratio,
    seed = seed,
    extra_cols = "isoyearweek"
  )
}

# The two women who separate a correct read from a band-start read, plus enough
# plain comparators for the ratio to draw from.
#
# LATE is not eligible until week 3 and initiates in that same week, so her
# recruiting week is week 3 and NOT the band start. EARLY is eligible from week
# 1 and initiates there, so her recruiting week IS the band start. The pair is
# what makes the fixture discriminating.
.lpb_three_instant_data <- function(weeks) {
  data.table::rbindlist(list(
    .lpb_person("LATE", weeks, tx_week = 3L, elig_from = 3L),
    .lpb_person("EARLY", weeks, tx_week = 1L, elig_from = 1L),
    data.table::rbindlist(lapply(
      paste0("C", 1:8),
      function(nm) .lpb_person(nm, weeks)
    ))
  ))
}


# ---------------------------------------------------------------------------
# PROOF 1
# ---------------------------------------------------------------------------

test_that("the panel starts after the entry band", {
  weeks <- .lpb_weeks()
  band0 <- .lpb_band0(weeks)
  d <- .lpb_three_instant_data(weeks)

  trial <- .lpb_enroll(d, .lpb_design(follow_up_time = 8L))
  panel <- trial$data
  expect_gt(nrow(panel), 0L)

  # `entry_band_id` names the trial. Dropping it would leave nothing on the
  # panel that says which band recruited the person.
  expect_true("entry_band_id" %in% names(panel))
  expect_true(all(panel$entry_band_id == band0))

  # Every person-trial opens at the landmark, which is the band after the
  # entry band, and its first row is `tstart == 0`.
  first_rows <- panel[
    order(enrollment_person_trial_id, trial_id),
    .SD[1L],
    by = enrollment_person_trial_id
  ]
  expect_gt(nrow(first_rows), 0L)
  expect_identical(
    first_rows$trial_id,
    first_rows$entry_band_id + 1L
  )
  expect_true(all(first_rows$tstart == 0L))

  # No row anywhere in the panel sits in the entry band. That band is the
  # immortal one, and this is the assertion that says it is gone.
  expect_equal(sum(panel$trial_id <= panel$entry_band_id), 0L)
  expect_true(all(panel$trial_id >= panel$entry_band_id + 1L))

  # Follow-up is still `follow_up_time` weeks long: two bands of four weeks.
  per_trial <- panel[, .N, by = enrollment_person_trial_id]
  expect_true(all(per_trial$N == 2L))
})


# ---------------------------------------------------------------------------
# PROOF 2
# ---------------------------------------------------------------------------

test_that("the entry covariate is read at the recruiting week", {
  weeks <- .lpb_weeks()
  band0 <- .lpb_band0(weeks)
  d <- .lpb_three_instant_data(weeks)

  # The three instants, stated as the fixture builds them. `age` starts at 100
  # in week 1 and rises by one each week.
  age_at_band_start <- 100L # fixture week 1
  age_at_recruiting_week <- 102L # fixture week 3, LATE's first eligible week
  age_at_landmark_band <- 104L # fixture week 5, the first follow-up band
  expect_false(age_at_recruiting_week == age_at_band_start)
  expect_false(age_at_recruiting_week == age_at_landmark_band)

  trial <- .lpb_enroll(d, .lpb_design(follow_up_time = 8L))
  panel <- trial$data
  expect_true(".tte_entry__age" %in% names(panel))

  late <- panel[id == "LATE" & tstart == 0L]
  expect_identical(nrow(late), 1L)

  # The entry snapshot reads the recruiting week.
  expect_identical(late$.tte_entry__age, age_at_recruiting_week)
  # It is NOT the first week of the entry window.
  expect_false(late$.tte_entry__age == age_at_band_start)
  # It is NOT the landmark-band value, which is what `age` itself holds.
  expect_false(late$.tte_entry__age == age_at_landmark_band)
  expect_identical(late$age, age_at_landmark_band)

  # EARLY is eligible from week 1, so her recruiting week IS the band start.
  # The two women therefore hold different entry values, and a read at the
  # band start cannot match both.
  early <- panel[id == "EARLY" & tstart == 0L]
  expect_identical(nrow(early), 1L)
  expect_identical(early$.tte_entry__age, age_at_band_start)
  expect_false(late$.tte_entry__age == early$.tte_entry__age)

  # The follow-up column keeps the time-updated value on every row, so the
  # snapshot did not overwrite it.
  expect_identical(
    panel[id == "LATE"][order(trial_id)]$age,
    c(104L, 108L)
  )
  expect_true(all(panel[trial_id == band0 + 1L]$age == age_at_landmark_band))
})


# ---------------------------------------------------------------------------
# PROOF 3
# ---------------------------------------------------------------------------

# Ten intervention and twenty comparator persons, each recruited in a different
# week of the entry band. `age` rises five per week, so the entry value and the
# landmark value differ by five to twenty. Both arms span the same age range,
# which is what keeps the logistic fit away from separation.
.lpb_ipw_data <- function(weeks) {
  int <- lapply(seq_len(10L), function(i) {
    .lpb_person(
      paste0("I", i),
      weeks,
      tx_week = 1L + ((i - 1L) %% 4L),
      elig_from = 1L,
      age_base = 40L + 3L * i,
      age_step = 5L
    )
  })
  cmp <- lapply(seq_len(20L), function(j) {
    .lpb_person(
      paste0("K", j),
      weeks,
      tx_week = NA_integer_,
      elig_from = 1L + ((j - 1L) %% 4L),
      age_base = 40L + 2L * j,
      age_step = 5L
    )
  })
  data.table::rbindlist(c(int, cmp))
}

test_that("baseline IPW fits the entry snapshot, not the landmark value", {
  weeks <- .lpb_weeks()
  d <- .lpb_ipw_data(weeks)

  trial <- .lpb_enroll(d, .lpb_design(follow_up_time = 8L), ratio = 2, seed = 7)
  trial$s2_ipw()

  b <- trial$data[tstart == 0L][order(enrollment_person_trial_id)]
  # Eight per arm is the floor that keeps the logistic fit away from
  # separation. Assert it, so a fixture change cannot quietly drop below it.
  expect_gte(sum(b$exposed == TRUE), 8L)
  expect_gte(sum(b$exposed == FALSE), 8L)
  expect_false(anyNA(b$.tte_entry__age))
  expect_false(anyNA(b$age))
  # The two candidate covariates genuinely differ, so the two fits below
  # cannot agree by accident.
  expect_gt(sum(b$.tte_entry__age != b$age), 0L)

  # The reference fit: the entry-window snapshot, under a local name.
  ref_entry <- data.frame(y = b$exposed, x = b$.tte_entry__age)
  fit_entry <- stats::glm(y ~ x, data = ref_entry, family = stats::binomial)
  ps_entry <- unname(stats::predict(fit_entry, type = "response"))
  expect_equal(b$ps, ps_entry, tolerance = 1e-10)

  # The wrong fit: the landmark-band value that now sits at `tstart == 0`.
  ref_follow <- data.frame(y = b$exposed, x = b$age)
  fit_follow <- stats::glm(y ~ x, data = ref_follow, family = stats::binomial)
  ps_follow <- unname(stats::predict(fit_follow, type = "response"))
  expect_gt(max(abs(ps_entry - ps_follow)), 0.01)
  expect_gt(max(abs(b$ps - ps_follow)), 0.01)

  # The weights follow the propensity score, so they move with it.
  p_int <- mean(b$exposed, na.rm = TRUE)
  ipw_entry <- data.table::fifelse(
    b$exposed == TRUE,
    p_int / ps_entry,
    (1 - p_int) / (1 - ps_entry)
  )
  expect_equal(b$ipw, ipw_entry, tolerance = 1e-10)
})


# ---------------------------------------------------------------------------
# PROOF 4
# ---------------------------------------------------------------------------

# `.s1d_worker()` resolves both its destinations through
# `.batch_where_to_write_output()`, so it only runs inside a real
# `staged_writer` dispatch. The fixture therefore runs s1a, s1b and s1c for
# real, then issues the s1d dispatch exactly as the call site does.
#
# The capture has to cross a process boundary, so `impute_fn` writes the names
# it is handed to a file and the test reads the file back.

.lpb_s1a_run_real <- function(skel_path, es_list, spec, work_dir) {
  bn <- basename(skel_path)
  id <- paste0("s1a_", bn)
  items <- list(list(
    file_path = skel_path,
    enrollment_specs = es_list,
    spec = spec
  ))
  names(items) <- id
  eids <- unlist(lapply(es_list, function(e) e$enrollment_id))
  outputs <- list(swereg:::.s1a_outputs_for_skeleton(work_dir, eids, bn))
  names(outputs) <- id
  invisible(utils::capture.output(
    swereg:::.batch_run_and_write(
      target = swereg:::.batch_target("swereg", ".s1a_worker_multi"),
      items = items,
      outputs = outputs,
      style = "staged_writer",
      n_workers = 1L,
      dev_path = swereg:::.swereg_dev_path(),
      label = "s1a"
    ),
    type = "output"
  ))
}

.lpb_s1d_fixture <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dir_spec <- file.path(root, "spec")
  dir_tteplan <- file.path(root, "tteplan")
  dir_results <- file.path(root, "results")
  dir_meta <- file.path(root, "meta")
  for (dd in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(dd, recursive = TRUE, showWarnings = FALSE)
  }

  sk <- ttm_skeleton(
    scenario = "A",
    n_persons = 40L,
    date_min = "2018-01-01",
    date_max = "2019-06-30",
    n_init_bands = 8L,
    seed = 4242L
  )
  skel_path <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dir_spec, "spec_v001.yaml"),
    "lpbentry",
    "rd_age_continuous"
  )

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)]
  )

  work_dir <- swereg:::.s1_work_dir(plan, ensure_exists = FALSE)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  es <- plan$enrollment_spec(1)
  es$n_threads <- 1L
  bn <- basename(skel_path)

  .lpb_s1a_run_real(skel_path, list(es), plan$spec, work_dir)
  s1b <- swereg:::.s1b_worker(
    enrollment_spec = es,
    spec = plan$spec,
    work_dir = work_dir,
    skel_basenames = bn
  )
  qs2::qs_save(
    s1b$enrolled_ids,
    swereg:::.s1b_enrolled_ids_path(work_dir, es$enrollment_id)
  )
  s1c <- swereg:::.s1c_worker(
    enrollment_spec = es,
    file_path = skel_path,
    spec = plan$spec,
    work_dir = work_dir
  )
  qs2::qs_save(
    s1c$panel,
    swereg:::.s1c_panel_path(work_dir, es$enrollment_id, bn)
  )

  dir_out <- file.path(root, "out")
  dir.create(dir_out, showWarnings = FALSE)

  list(
    plan = plan,
    spec = plan$spec,
    es = es,
    skel_basenames = bn,
    work_dir = work_dir,
    dir_out = dir_out,
    file_raw = file.path(dir_out, "enr_raw.qs2"),
    file_imp = file.path(dir_out, "enr_imp.qs2")
  )
}

.lpb_s1d_dispatch <- function(fx, impute_fn, id = "s1d_entry_names") {
  items <- list(list(
    enrollment_spec = fx$es,
    spec = fx$spec,
    work_dir = fx$work_dir,
    skel_basenames = fx$skel_basenames,
    impute_fn = impute_fn,
    stabilize = TRUE
  ))
  names(items) <- id
  outputs <- list(c(raw = fx$file_raw, imp = fx$file_imp))
  names(outputs) <- id

  invisible(utils::capture.output(
    res <- swereg:::.batch_run_and_write(
      target = swereg:::.batch_target("swereg", ".s1d_worker"),
      items = items,
      outputs = outputs,
      style = "staged_writer",
      n_workers = 1L,
      dev_path = swereg:::.swereg_dev_path(),
      label = "s1d"
    ),
    type = "output"
  ))
  res
}

test_that("imputation receives the entry-snapshot names", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  skip_if_not_installed("yaml")
  skip_if_not_installed("withr")
  skip_if_not_installed("processx")

  fx <- .lpb_s1d_fixture()
  conf <- fx$es$design$confounder_vars
  expect_identical(conf, "rd_age_continuous")

  cap_path <- file.path(fx$dir_out, "..", "impute_names.txt")
  capture_fn <- local({
    p <- cap_path
    function(x, cv) {
      writeLines(as.character(cv), p)
      x
    }
  })

  .lpb_s1d_dispatch(fx, impute_fn = capture_fn)

  # The worker ran and reached `impute_fn`.
  expect_true(file.exists(cap_path))
  supplied <- readLines(cap_path)

  expect_identical(supplied, ".tte_entry__rd_age_continuous")
  expect_false("rd_age_continuous" %in% supplied)

  # The column the worker named is really on the panel it handed over, so the
  # name list is not merely well-formed.
  raw <- swereg:::qs2_read(fx$file_raw)
  expect_true(all(supplied %in% names(raw$data)))
  expect_true(conf %in% names(raw$data))
})


# ---------------------------------------------------------------------------
# PROOF 5
# ---------------------------------------------------------------------------

# `$s6_ipcw_pp()` fits censoring on the follow-up rows, so it reads the
# TIME-UPDATED confounder. Imputation now fills `.tte_entry__<v>` only, so a
# missing follow-up value survives to this step.
#
# The old call hid it. `$s1_impute_confounders()` update-joins the baseline
# value onto every row of a person-trial, so an imputed column came out
# NA-free and flattened across follow-up.
#
# An NA here is not a silent NA weight. `stats::predict()` returns NA for the
# row, `cumprod()` carries it through the rest of the person-trial, and the NA
# reaches the survey fit far from its cause.

# A trial-level panel, 60 person-trials over 3 bands. `.tte_entry__age` is
# always present, so the entry snapshot cannot stand in for the follow-up
# value. Every fourth person-trial deviates at the third band, so censoring
# really fires and the model is fitted rather than falling back to the
# marginal rate.
.lpb_ipcw_panel <- function(n = 60L) {
  d <- data.table::rbindlist(lapply(seq_len(n), function(i) {
    tx <- i <= (n %/% 2L)
    data.table::data.table(
      enrollment_person_trial_id = as.character(i),
      trial_id = 1L,
      tstart = c(0L, 4L, 8L),
      tstop = c(4L, 8L, 12L),
      exposed = tx,
      on_tx = c(tx, tx, if (i %% 4L == 0L) !tx else tx),
      event = 0L,
      person_weeks = 4L,
      age = 50 + i %% 10 + c(0, 1, 2)
    )
  }))
  d[, .tte_entry__age := age[1], by = enrollment_person_trial_id]
  d[]
}

.lpb_ipcw_design <- function() {
  TTEDesign$new(
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    outcome_vars = "event",
    confounder_vars = "age",
    follow_up_time = 12L
  )
}

test_that("IPCW fails loudly on a missing time-updated confounder", {
  d <- .lpb_ipcw_panel()
  # 12 of 180 rows lose their follow-up `age`, across 12 of 60 person-trials.
  # The entry snapshot stays complete, so nothing can quietly stand in.
  d[
    tstart == 4L & as.integer(enrollment_person_trial_id) %% 5L == 0L,
    age := NA_real_
  ]
  expect_equal(sum(is.na(d$age)), 12L)
  expect_false(anyNA(d$.tte_entry__age))

  trial <- TTEEnrollment$new(d, .lpb_ipcw_design())
  trial$s2_ipw()
  # Baseline IPW reads the entry snapshot, so it is unaffected and reports no
  # missing weight. The failure below is about follow-up alone.
  expect_false(anyNA(trial$data$ipw))

  err <- tryCatch(
    trial$s4_prepare_for_analysis(
      outcome = "event",
      follow_up = 12L,
      estimand = "pp",
      estimate_ipcw_pp_with_gam = FALSE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  # Read the message only when there IS one, so a missing stop shows up as
  # four failed assertions rather than as one failure and one error.
  msg <- if (inherits(err, "condition")) conditionMessage(err) else ""

  # The message names the step, the confounder, the rows and the
  # person-trials, so a user can act on it without reading the source.
  expect_match(msg, "s6_ipcw_pp() cannot fit the censoring model", fixed = TRUE)
  expect_match(msg, "age: 12 of 180 rows, 12 of 60 person-trials", fixed = TRUE)
  expect_match(
    msg,
    "swereg MUST NOT substitute the entry-window value",
    fixed = TRUE
  )
})

test_that("IPCW runs through when the time-updated confounder is complete", {
  # The same panel with no planted gap. The loud failure MUST fire only where
  # the old code was quietly wrong.
  d <- .lpb_ipcw_panel()
  expect_false(anyNA(d$age))

  trial <- TTEEnrollment$new(d, .lpb_ipcw_design())
  trial$s2_ipw()
  suppressWarnings(trial$s4_prepare_for_analysis(
    outcome = "event",
    follow_up = 12L,
    estimand = "pp",
    estimate_ipcw_pp_with_gam = FALSE
  ))
  expect_true("ipcw_pp" %in% names(trial$data))
  expect_false(anyNA(trial$data$ipcw_pp))
})


# ---------------------------------------------------------------------------
# Supporting behaviour, tested and not mutation-proven
# ---------------------------------------------------------------------------

test_that("a confounder MUST NOT take the reserved entry prefix", {
  expect_error(
    TTEDesign$new(
      treatment_var = "exposed",
      outcome_vars = "died",
      confounder_vars = c("age", ".tte_entry__age"),
      follow_up_time = 8L
    ),
    "MUST NOT start with"
  )
})

test_that("a panel with a partial entry snapshot stops the IPW fit", {
  weeks <- .lpb_weeks()
  d <- .lpb_ipw_data(weeks)
  trial <- .lpb_enroll(d, .lpb_design(follow_up_time = 8L), ratio = 2, seed = 7)
  trial$design$confounder_vars <- c("age", "died")
  expect_error(trial$s2_ipw(), "at the same instant")
})

test_that("a panel built without a recruiting week keeps the old read", {
  weeks <- .lpb_weeks()
  d <- .lpb_three_instant_data(weeks)
  design <- .lpb_design(follow_up_time = 8L)

  # `enrolled_ids` built by hand, exactly as a caller outside the plan chain
  # writes it. It carries no `recruit_week_index`, so no snapshot exists and
  # `$s2_ipw()` falls back to the follow-up column.
  band0 <- .lpb_band0(weeks)
  enrolled_ids <- data.table::data.table(
    id = c("LATE", "EARLY", paste0("C", 1:4)),
    trial_id = band0,
    intervention = c(TRUE, TRUE, rep(FALSE, 4L)),
    enrollment_person_trial_id = paste0(
      c("LATE", "EARLY", paste0("C", 1:4)),
      ".",
      band0
    )
  )
  trial <- TTEEnrollment$new(
    data = data.table::copy(d),
    design = design,
    enrolled_ids = enrolled_ids,
    seed = 4,
    extra_cols = "isoyearweek"
  )
  expect_false(".tte_entry__age" %in% names(trial$data))
  expect_true("entry_band_id" %in% names(trial$data))
  # Time zero still moves. Only the covariate read falls back.
  expect_true(all(trial$data$trial_id >= trial$data$entry_band_id + 1L))
  expect_true(all(trial$data[tstart == 0L]$trial_id == band0 + 1L))

  # `$s2_ipw()` reads the follow-up column, and reports no error.
  trial$s2_ipw()
  expect_true("ipw" %in% names(trial$data))
})

test_that("both Table 1 routes read the entry snapshot", {
  weeks <- .lpb_weeks()
  d <- .lpb_ipw_data(weeks)
  trial <- .lpb_enroll(d, .lpb_design(follow_up_time = 8L), ratio = 2, seed = 7)

  by_method <- trial$table1()
  by_worker <- swereg:::.s3_enrollment_table1(trial)
  expect_equal(by_method, by_worker)

  # The number the panel reports is the mean of the entry snapshot, and not
  # the mean of the landmark-band column.
  b <- trial$data[tstart == 0L]
  overall <- by_method[startsWith(Variable, "age ")]$Overall
  expect_length(overall, 1L)
  expect_true(startsWith(
    overall,
    format(round(mean(b$.tte_entry__age), 2), nsmall = 2)
  ))
  expect_false(startsWith(
    overall,
    format(round(mean(b$age), 2), nsmall = 2)
  ))
  expect_false(
    isTRUE(all.equal(mean(b$.tte_entry__age), mean(b$age)))
  )
})
