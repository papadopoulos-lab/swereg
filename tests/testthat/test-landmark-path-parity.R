# The two enrollment paths MUST qualify the same person-bands.
#
# swereg builds a trial panel by two routes, and both must land on the same
# qualified enrollment.
#
#   production : .s1a_worker_multi -> .s1b_worker -> .s1c_worker
#   direct     : TTEEnrollment$new(..., ratio = )
#
# The production route is the one a project runs. There is no `s1a.R`,
# `s1b.R` or `s1c.R` script: `s1.R` calls
# `$s1_generate_enrollments_and_ipw()`, and the three sub-stages are internal.
# This file therefore drives the internal workers, and the s1a stage runs as a
# real subprocess through the batchit commit engine.
#
# The two routes classify the arm from different columns. The scout reads
# `rd_intervention`, the weekly logical that `.s1_prepare_loaded()` derives
# from the spec's treatment implementation. The direct route reads
# `design$treatment_var`. Both then call `.band_baseline_treatment()` and
# `.tte_qualify_bands()`, so a divergence in either one shows up here.
#
# The intervention arm is what makes this test causal. No sampling touches it:
# every qualified initiator enrolls. So the two intervention sets agree if and
# only if the two routes qualify identically. The comparator arm passes through
# a seeded draw whose row order differs between the routes, so it is not
# compared as a set.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")
skip_if_not_installed("processx")

# One fixture, built once per test. It carries independent loss and
# discontinuation, so some person-bands genuinely fail to qualify. A fixture
# where qualification drops nothing would pass this test with the step removed.
.lpp_fixture <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dir_spec <- file.path(root, "spec")
  dir_tteplan <- file.path(root, "tteplan")
  dir_results <- file.path(root, "results")
  dir_meta <- file.path(root, "meta")
  for (d in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  sk <- ttm_skeleton(
    scenario = "A",
    n_persons = 60L,
    loss = "independent",
    disc_hazard = 0.05,
    date_min = "2018-01-01",
    date_max = "2019-06-30",
    n_init_bands = 8L,
    seed = 909L
  )
  skel_path <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dir_spec, "spec_v001.yaml"),
    "ppar",
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

  list(
    plan = plan,
    es = es,
    skel_path = skel_path,
    bn = basename(skel_path),
    work_dir = work_dir
  )
}

# The production route, exactly as `$s1_generate_enrollments_and_ipw()` issues
# it. s1a goes through `.batch_run_and_write()`, so it runs in a fresh R
# subprocess and resolves its output paths by name.
.lpp_production <- function(fx) {
  id <- paste0("s1a_", fx$bn)
  items <- list(list(
    file_path = fx$skel_path,
    enrollment_specs = list(fx$es),
    spec = fx$plan$spec
  ))
  names(items) <- id
  outputs <- list(swereg:::.s1a_outputs_for_skeleton(
    fx$work_dir,
    fx$es$enrollment_id,
    fx$bn
  ))
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

  pre_path <- swereg:::.s1a_pre_path(fx$work_dir, fx$es$enrollment_id, fx$bn)
  pre <- swereg::qs2_read(pre_path)

  s1b <- swereg:::.s1b_worker(
    enrollment_spec = fx$es,
    spec = fx$plan$spec,
    work_dir = fx$work_dir,
    skel_basenames = fx$bn
  )
  qs2::qs_save(
    s1b$enrolled_ids,
    swereg:::.s1b_enrolled_ids_path(fx$work_dir, fx$es$enrollment_id)
  )
  s1c <- swereg:::.s1c_worker(
    enrollment_spec = fx$es,
    file_path = fx$skel_path,
    spec = fx$plan$spec,
    work_dir = fx$work_dir
  )

  list(
    pre_path = pre_path,
    tuples = pre$tuples,
    enrolled_ids = s1b$enrolled_ids,
    panel = s1c$panel$data
  )
}

# The prepared skeleton both routes see. `.s1a_worker_multi()` runs these two
# calls before it hands the table to the scout.
.lpp_prepared <- function(fx) {
  canonical <- swereg:::.s1_load_skeleton(fx$skel_path, 1L)
  swereg:::.s1_prepare_loaded(
    canonical,
    fx$es,
    fx$plan$spec,
    derive_confounders = FALSE
  )
}

# The direct route. `rd_intervention` is the weekly logical the scout reads,
# so the direct design names it too. Any other column would compare two
# different treatment definitions and prove nothing about qualification.
.lpp_direct <- function(fx, prepared) {
  d <- fx$es$design
  design <- TTEDesign$new(
    person_id_var = d$person_id_var,
    treatment_var = "rd_intervention",
    time_treatment_var = d$time_treatment_var,
    eligible_var = d$eligible_var,
    observed_var = d$observed_var,
    outcome_vars = d$outcome_vars,
    confounder_vars = d$confounder_vars,
    follow_up_time = d$follow_up_time,
    period_width = d$period_width,
    admin_censor_isoyearweek = d$admin_censor_isoyearweek,
    intervention_tolerance_weeks = d$intervention_tolerance_weeks,
    comparator_tolerance_weeks = d$comparator_tolerance_weeks
  )
  TTEEnrollment$new(
    data = data.table::copy(prepared),
    design = design,
    ratio = fx$es$matching_ratio,
    seed = fx$es$seed,
    extra_cols = "isoyearweek"
  )
}

# One plain (person, band) table, free of keys and column order, so two of
# them compare on content alone.
.lpp_bands <- function(dt, id_col, band_col) {
  out <- unique(data.table::as.data.table(dt)[,
    list(id = as.character(get(id_col)), band = as.integer(get(band_col)))
  ])
  data.table::setorderv(out, c("id", "band"))
  as.data.frame(out, stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# PROOF -- the direct and production paths agree on qualified enrollment
# ---------------------------------------------------------------------------

test_that("the direct and production paths agree on qualified enrollment", {
  skip_on_cran()
  fx <- .lpp_fixture()
  prod <- .lpp_production(fx)
  prepared <- .lpp_prepared(fx)
  direct <- .lpp_direct(fx, prepared)

  # The reachability witness. The s1a subprocess wrote the file that s1b
  # reads, so the production classification really ran out of process.
  expect_true(file.exists(prod$pre_path))
  expect_gt(nrow(prod$tuples), 0L)
  expect_gt(nrow(prod$panel), 0L)
  expect_gt(nrow(direct$data), 0L)

  # Qualification MUST drop person-bands on this fixture. Without a drop the
  # comparison below would hold with the whole step removed, and this file
  # would prove nothing.
  unqualified <- swereg:::.s1_eligible_tuples(
    data.table::copy(prepared),
    fx$es$design
  )
  expect_gt(nrow(unqualified), nrow(prod$tuples))
  expect_gt(
    nrow(unqualified[intervention == TRUE]),
    nrow(prod$tuples[intervention == TRUE])
  )

  # The qualified intervention arm. No sampling touches it, so the two routes
  # agree here if and only if they qualify identically.
  prod_int <- .lpp_bands(
    prod$tuples[intervention == TRUE],
    "id",
    "trial_id"
  )
  direct_int <- .lpp_bands(
    direct$data[rd_intervention == TRUE],
    "id",
    "entry_band_id"
  )
  expect_identical(direct_int, prod_int)

  # Every enrolled id the production route hands to s1c is qualified too.
  enrolled_int <- .lpp_bands(
    prod$enrolled_ids[intervention == TRUE],
    "id",
    "trial_id"
  )
  expect_identical(enrolled_int, prod_int)
})

test_that("both paths open follow-up one band after the entry band", {
  skip_on_cran()
  fx <- .lpp_fixture()
  prod <- .lpp_production(fx)
  direct <- .lpp_direct(fx, .lpp_prepared(fx))

  # Timing semantics. The first row of every person-trial sits at the
  # landmark: `tstart == 0` and one band after the entry band.
  for (panel in list(prod$panel, direct$data)) {
    first_rows <- panel[tstart == 0L]
    expect_gt(nrow(first_rows), 0L)
    expect_true(all(first_rows$trial_id == first_rows$entry_band_id + 1L))
    expect_identical(
      panel[, min(tstart), by = enrollment_person_trial_id]$V1,
      rep(0L, data.table::uniqueN(panel$enrollment_person_trial_id))
    )
  }

  # The entry snapshot is the recruiting-week value, and the two routes MUST
  # read it at the same instant for the same person-trial.
  # The two routes key the panel differently. `enrollment_person_trial_id` is
  # "<enrollment_id>.<person>.<band>" on the production route, because the
  # scout writes the enrollment id into the tuples, and "<person>.<band>" on
  # the direct route. So the join runs on (person, entry band).
  snap <- ".tte_entry__rd_age_continuous"
  expect_true(snap %in% names(prod$panel))
  expect_true(snap %in% names(direct$data))
  a <- unique(prod$panel[
    tstart == 0L,
    list(id = as.character(id), band = entry_band_id, v = get(snap))
  ])
  b <- unique(direct$data[
    tstart == 0L,
    list(id = as.character(id), band = entry_band_id, v = get(snap))
  ])
  both <- merge(a, b, by = c("id", "band"), suffixes = c("_prod", "_direct"))
  expect_gt(nrow(both), 0L)
  expect_equal(both$v_prod, both$v_direct)
})
