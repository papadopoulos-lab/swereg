# The fill summary must survive the whole way from s1d to s3.
#
# `.s1d_worker()` computes the table and stores it on the enrollment object.
# Nothing recomputes it after that. `.s2_worker()` loads that object, runs
# `$s4_prepare_for_analysis()` on it and returns it, so the table rides along.
# `.s3_enrollment_worker()` loads the analysis file and copies the table into
# its returned list. s3 never sees the imputed panel, so that copy is the only
# route the table has into the results.
#
# Each stage below is the real worker, called by name. A test that rebuilt the
# table at each stage would pass even when the propagation is broken.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")
skip_if_not_installed("processx")

# s1a takes no `work_dir` and resolves its destinations by name, so it is not
# callable outside a dispatch. This issues the real one.
.fsp_s1a_run_real <- function(skel_path, es_list, spec, work_dir) {
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

# Build a small real plan and run s1a, s1b and s1c, so the panel chunk s1d
# reads is genuinely on disk. Two confounders, so the summary carries a row the
# plant never touches.
.fsp_fixture <- function(env = parent.frame(), confounder_vars) {
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
    "fspprop",
    confounder_vars
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

  .fsp_s1a_run_real(skel_path, list(es), plan$spec, work_dir)
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

# The real s1d dispatch, exactly as the call site issues it.
.fsp_s1d_dispatch <- function(fx, id = "s1d_fill_summary") {
  items <- list(list(
    enrollment_spec = fx$es,
    spec = fx$spec,
    work_dir = fx$work_dir,
    skel_basenames = fx$skel_basenames,
    impute_fn = NULL,
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

# Plant one NA in the panel chunk `.s1c_worker()` wrote, so the summary holds a
# value the fill produced. A summary of zeros would let a stage that returned an
# empty table pass.
#
# A plant that fails to apply looks exactly like a passing check, so this
# digests the file either side of the write and returns both digests.
.fsp_plant_na <- function(panel_path, conf) {
  md5_before <- unname(tools::md5sum(panel_path))
  panel <- qs2::qs_read(panel_path)
  d <- panel$data
  data.table::setorderv(d, c("enrollment_person_trial_id", "tstart"))
  plant_id <- d$enrollment_person_trial_id[1L]
  plant_rows <- which(d$enrollment_person_trial_id == plant_id)
  data.table::set(
    d,
    i = plant_rows[2L],
    j = conf,
    value = d[[conf]][NA_integer_]
  )
  qs2::qs_save(panel, panel_path)
  return(list(
    md5_before = md5_before,
    md5_after = unname(tools::md5sum(panel_path)),
    plant_rows_n = length(plant_rows)
  ))
}


test_that("the fill summary reaches s2 and s3 unchanged", {
  skip_on_cran()
  filled_var <- "rd_age_continuous"
  untouched_var <- "ri_highrisk"
  fx <- .fsp_fixture(confounder_vars = c(filled_var, untouched_var))

  panel_path <- swereg:::.s1c_panel_path(
    fx$work_dir,
    fx$es$enrollment_id,
    fx$skel_basenames
  )
  plant <- .fsp_plant_na(panel_path, filled_var)
  expect_gt(plant$plant_rows_n, 1L)
  # THE PLANT REACHED THE FILE.
  expect_false(identical(plant$md5_before, plant$md5_after))

  .fsp_s1d_dispatch(fx)
  imp <- swereg:::qs2_read(fx$file_imp)
  expected <- imp$fill_summary

  # The table is the one s1d produced, and it is not a table of zeros.
  expect_s3_class(expected, "data.table")
  expect_setequal(expected$confounder, c(filled_var, untouched_var))
  expect_identical(expected$rows_filled_n[
    match(filled_var, expected$confounder)
  ], 1L)
  expect_identical(expected$rows_filled_n[
    match(untouched_var, expected$confounder)
  ], 0L)

  # s2: the analysis object it returns carries the same table.
  s2 <- swereg:::.s2_worker(
    outcome = "osd_a",
    follow_up = 52L,
    file_imp_path = fx$file_imp,
    n_threads = 1L,
    sep_by_tx = FALSE,
    with_gam = FALSE,
    estimand = "itt"
  )
  expect_true("prepare_outcome" %in% s2$analysis$steps_completed)
  expect_identical(s2$analysis$fill_summary, expected)

  # s3: the list it returns carries the same table. s3 reads the analysis file
  # and never opens the imputed panel, so this is the only route.
  analysis_path <- file.path(fx$dir_out, "enr_analysis.qs2")
  qs2::qs_save(s2$analysis, analysis_path)
  s3 <- swereg:::.s3_enrollment_worker(
    analysis_path = analysis_path,
    raw_path = fx$file_raw,
    enrollment_id = fx$es$enrollment_id,
    n_threads = 1L
  )

  expect_true("fill_summary" %in% names(s3))
  expect_false(is.null(s3$fill_summary))
  expect_identical(s3$fill_summary, expected)
})
