# The per-trial counts carry the trial as `enrollment_period_id`.
#
# s1 stores the attrition cascade and the comparator-draw counts per trial.
# `$get_attrition()` and `$get_matching()` return those rows with the trial in
# `enrollment_period_id`, and `NA` on a global row. `$export_tables()` reads
# that key to find the global rows. No output carries the retired column name,
# which is built from two parts so that a search of the source for it finds no
# live use.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")

.epk_retired <- paste0("trial", "_id")

# A small real plan, after s1 only.
.epk_s1_plan <- function(env = parent.frame()) {
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
    n_persons = 40L,
    date_min = "2018-01-01",
    date_max = "2019-06-30",
    n_init_periods = 8L,
    seed = 4242L
  )
  skel_path <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dir_spec, "spec_v001.yaml"),
    "epkey",
    "rd_age_continuous"
  )
  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  )
  invisible(utils::capture.output(
    suppressWarnings(plan$s1_generate_enrollments_and_ipw(
      n_workers = 1L,
      swereg_dev_path = ttm_dev_path(),
      check_skeletons = FALSE
    )),
    type = "output"
  ))
  plan
}

.epk_expect_key <- function(x, label) {
  expect_true("enrollment_period_id" %in% names(x), info = label)
  expect_false(.epk_retired %in% names(x), info = label)
  expect_type(x$enrollment_period_id, "integer")
}

test_that("s1 stores the trial as enrollment_period_id, and both accessors return it", {
  skip_on_cran()
  plan <- .epk_s1_plan()

  att <- plan$get_attrition()
  mat <- plan$get_matching()
  expect_gt(nrow(att), 0L)
  expect_gt(nrow(mat), 0L)
  .epk_expect_key(att, "get_attrition()")
  .epk_expect_key(mat, "get_matching()")

  # The global rows carry NA, and the per-trial rows carry the trial.
  expect_true(any(is.na(att$enrollment_period_id)))
  expect_true(any(!is.na(att$enrollment_period_id)))
  expect_false(anyNA(mat$enrollment_period_id))

  # What s1 stored, under the producer's names.
  for (eid in names(plan$enrollment_counts)) {
    counts <- plan$enrollment_counts[[eid]]
    .epk_expect_key(counts$attrition, paste("stored attrition", eid))
    .epk_expect_key(counts$matching, paste("stored matching", eid))
  }
})

# The person-trial count one step of an attrition sheet reports, or NA when the
# sheet or the step is absent. The header row is the row that names
# `step_label`. Reading through NA keeps a missing sheet an assertion failure
# rather than an error.
.epk_sheet_person_trials <- function(sheet, step_label) {
  if (is.null(sheet)) {
    return(NA_real_)
  }
  m <- as.matrix(sheet)
  hdr <- which(apply(m, 1L, function(r) any(r %in% "step_label")))[1L]
  if (is.na(hdr)) {
    return(NA_real_)
  }
  label_col <- match("step_label", m[hdr, ])
  count_col <- match("n_person_trials", m[hdr, ])
  row <- which(m[, label_col] %in% step_label)[1L]
  if (is.na(row) || is.na(count_col)) {
    return(NA_real_)
  }
  as.numeric(m[row, count_col])
}

test_that("export_tables() reads the renamed key and writes no retired key", {
  # The export writes aggregated sheets only, so no sheet carries a per-trial
  # key. What it MUST do is READ the key. `.attrition_overall()` selects the
  # global rows by `is.na(enrollment_period_id)`. Selected by any other column,
  # it finds no global row and enrollment 01 gets no attrition sheet.
  plan <- .xp_plan("new")
  att <- plan$get_attrition()
  global_before <- att[
    enrollment_id == "01" &
      is.na(enrollment_period_id) &
      step_name == "before_exclusions"
  ]
  expect_identical(nrow(global_before), 1L)

  dir <- withr::local_tempdir()
  path <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = path)))
  sheets <- .xp_read_sheets(path)

  expect_true("Attrition_01" %in% sheets$sheet_names)
  expect_identical(
    .epk_sheet_person_trials(sheets$sheets[["Attrition_01"]], "Before exclusions"),
    global_before$n_person_trials
  )

  cells <- unlist(lapply(sheets$sheets, function(s) {
    as.character(unlist(s, use.names = FALSE))
  }))
  cells <- cells[!is.na(cells)]
  expect_gt(length(cells), 0L)
  expect_false(any(grepl(.epk_retired, cells, fixed = TRUE)))
})
