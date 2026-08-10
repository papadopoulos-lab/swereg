# s3 computes the whole absolute scale, for EVERY emulated trial and with
# nothing to switch it off.
#
# The defect this pins. The risk difference was computed in the EXPORT path,
# behind a figure option (`spec$risk_difference`), from the analysis panel on
# disk. The production export script did not set the option, so every figure
# was drawn without the quantity, with no error and no warning. Making the
# quantity conditional is what made it possible to forget.
#
# So the assertion here is over the WHOLE grid, never a featured subset. Every
# ETT must carry `rd`, `rd_lo`, `rd_hi`, `nnt`, `nnt_direction` and
# `interval_status`, plus the band-by-band curve. Gating the computation on
# anything at all -- a flag, a featured list, an outcome role -- must break it.
#
# `rd_lo` and `rd_hi` are `NA` on a zero-event arm, by design and not by
# failure. `interval_status` says so. The test therefore accepts a missing
# bound ONLY when the status names the reason. That is what separates an
# absent number from a number nobody computed.
#
# Fixture. Four ETTs, built to reach every `interval_status` in one grid.
#   ett01  protective intervention, events in both arms.
#   ett02  harmful intervention, events in both arms.
#   ett03  comparator arm carries NO event, so "zero-event arm".
#   ett04  small effect, events in both arms, so "spans null".
# The three named `interval_status` values are asserted to be PRESENT across
# the grid. A status column stuck on one constant therefore cannot pass.
#
# The dispatcher is mocked to run the REAL worker in this process. Only the
# subprocess transport is replaced: the item builder, `.s3_ett_worker` and the
# result assembly are all production code. `test-batch_s3_production.R` drives
# the transport itself, and the last test in this file drives it again for the
# risk-difference item specifically.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")

# --- fixture ----------------------------------------------------------------

# One trial-level analysis panel. `h_int` and `h_cmp` are the per-band event
# probabilities in the intervention and comparator arms. Every person holds two
# person-trials, so the person and the person-trial differ, which is the shape
# the person-level bootstrap needs.
.abs_panel <- function(n_persons, n_bands, h_int, h_cmp, seed) {
  set.seed(seed)
  persons <- sprintf("p%04d", seq_len(n_persons))
  arm <- rep(c(TRUE, FALSE), length.out = n_persons)
  rows <- list()
  for (k in seq_along(persons)) {
    h <- if (arm[k]) h_int else h_cmp
    for (trial in 1:2) {
      ev <- stats::rbinom(n_bands, 1L, h)
      first <- which(ev == 1L)
      keep <- if (length(first) > 0L) seq_len(first[1]) else seq_len(n_bands)
      rows[[length(rows) + 1L]] <- data.table::data.table(
        id = persons[k],
        enrollment_person_trial_id = paste0(persons[k], "_t", trial),
        rd_intervention = arm[k],
        trial_id = trial,
        tstart = (keep - 1L) * 4L,
        tstop = keep * 4L,
        event = ev[keep],
        person_weeks = 4
      )
    }
  }
  d <- data.table::rbindlist(rows)
  d[, analysis_weight_pp_trunc := 1 + 0.2 * (seq_len(.N) %% 3L)]
  d[, analysis_weight_pp := analysis_weight_pp_trunc]
  d[, ipw_trunc := analysis_weight_pp_trunc]
  d[]
}

.abs_design <- function() {
  swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "rd_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "rd_age_continuous",
    follow_up_time = 24L
  )
}

# The four ETT cells, and what each one is for.
.ABS_CELLS <- list(
  ett01 = list(h_int = 0.010, h_cmp = 0.070, seed = 11L),
  ett02 = list(h_int = 0.070, h_cmp = 0.010, seed = 12L),
  ett03 = list(h_int = 0.070, h_cmp = 0.000, seed = 13L),
  ett04 = list(h_int = 0.030, h_cmp = 0.032, seed = 14L)
)

# Write one PP and one ITT analysis file per cell, and return the plan whose
# grid points at them. `conf_level` writes the study property s3 reads; NULL
# leaves the plan with no specification at all, which is the default path.
.abs_plan <- function(
  output_dir,
  n_persons = 60L,
  n_bands = 6L,
  conf_level = NULL
) {
  design <- .abs_design()
  ids <- names(.ABS_CELLS)
  for (k in seq_along(ids)) {
    cell <- .ABS_CELLS[[k]]
    d <- .abs_panel(n_persons, n_bands, cell$h_int, cell$h_cmp, cell$seed)
    enr <- swereg::TTEEnrollment$new(d, design, data_level = "trial")
    qs2::qs_save(enr, file.path(output_dir, sprintf("analysis_%03d.qs2", k)))
    qs2::qs_save(enr, file.path(output_dir, sprintf("analysis_itt_%03d.qs2", k)))
  }
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = ids,
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    follow_up = 24L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "id",
    treatment_var = "rd_intervention",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = sprintf("analysis_%03d.qs2", seq_along(ids)),
    file_analysis_itt = sprintf("analysis_itt_%03d.qs2", seq_along(ids)),
    description = paste0(ids, ": Outcome A")
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "abs",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  if (!is.null(conf_level)) {
    plan$spec <- list(
      study = list(implementation = list(conf_level = conf_level))
    )
  }
  plan
}

# Run s3 with the REAL worker but no subprocess. The enrollment loop returns
# one empty slot per item so s3 proceeds to the ETT loop.
.abs_run_s3 <- function(plan, output_dir) {
  testthat::local_mocked_bindings(
    .batch_run = function(target, items, n_workers, ...) {
      if (!identical(target$symbol, ".s3_ett_worker")) {
        return(stats::setNames(vector("list", length(items)), names(items)))
      }
      lapply(items, function(it) do.call(swereg:::.s3_ett_worker, it))
    },
    .package = "swereg"
  )
  suppressWarnings(suppressMessages(utils::capture.output(
    plan$s3_analyze(output_dir = output_dir, n_workers = 1L),
    type = "output"
  )))
  plan
}

.ABS_ROW_FIELDS <- c(
  "rd",
  "rd_lo",
  "rd_hi",
  "nnt",
  "nnt_direction",
  "interval_status"
)
.ABS_STATUSES <- c("ok", "spans null", "zero-event arm")

# --- the invariant ----------------------------------------------------------

test_that("every ETT carries the absolute scale, ungated", {
  output_dir <- withr::local_tempdir()
  plan <- .abs_run_s3(.abs_plan(output_dir), output_dir)

  # Nothing anywhere asked for this. The plan holds no spec at all, so no
  # option could have enabled it.
  expect_null(plan$spec)

  ids <- plan$ett$ett_id
  expect_identical(length(ids), 4L)
  expect_identical(length(plan$results_ett), 4L)

  seen_status <- character(0)
  for (slot in c("rd_pp_trunc", "rd_itt")) {
    for (eid in ids) {
      row <- plan$results_ett[[eid]][[slot]]
      info <- paste0(eid, " / ", slot)
      expect_true(data.table::is.data.table(row), info = info)
      expect_identical(nrow(row), 1L, info = info)
      expect_true(all(.ABS_ROW_FIELDS %in% names(row)), info = info)

      # The point estimate is always a real number. It stays a valid
      # descriptive quantity even where no interval exists.
      expect_true(is.finite(row$rd), info = info)

      # A missing bound is allowed ONLY with a stated reason. This is the
      # clause that separates "no interval, and here is why" from "nobody
      # computed one".
      status <- as.character(row$interval_status)
      expect_true(status %in% .ABS_STATUSES, info = paste0(info, ": ", status))
      if (identical(status, "zero-event arm")) {
        expect_true(is.na(row$rd_lo), info = info)
        expect_true(is.na(row$rd_hi), info = info)
      } else {
        expect_true(is.finite(row$rd_lo), info = info)
        expect_true(is.finite(row$rd_hi), info = info)
        expect_true(isTRUE(row$rd_lo <= row$rd_hi), info = info)
      }

      # The decision columns travel with the numbers.
      expect_true(is.finite(row$nnt), info = info)
      expect_true(
        as.character(row$nnt_direction) %in% c("benefit", "harm"),
        info = info
      )

      # Provenance: the bootstrap settings s3 fixed, recorded on the row.
      expect_identical(as.integer(row$n_boot), 500L, info = info)
      expect_identical(as.integer(row$seed), 1L, info = info)
      expect_equal(as.numeric(row$conf_level), 0.95, info = info)

      seen_status <- c(seen_status, status)
    }
  }

  # All three states are reached by this grid, so a status column frozen on
  # one constant cannot satisfy the loop above.
  expect_setequal(unique(seen_status), .ABS_STATUSES)

  # The sign convention, on the two cells built to disagree about it.
  expect_lt(plan$results_ett[["ett01"]]$rd_pp_trunc$rd, 0)
  expect_identical(
    as.character(plan$results_ett[["ett01"]]$rd_pp_trunc$nnt_direction),
    "benefit"
  )
  expect_gt(plan$results_ett[["ett02"]]$rd_pp_trunc$rd, 0)
  expect_identical(
    as.character(plan$results_ett[["ett02"]]$rd_pp_trunc$nnt_direction),
    "harm"
  )
})

test_that("every ETT carries the stored survival curve", {
  output_dir <- withr::local_tempdir()
  plan <- .abs_run_s3(.abs_plan(output_dir), output_dir)

  for (slot in c("rd_curve_pp_trunc", "rd_curve_itt")) {
    for (eid in plan$ett$ett_id) {
      curve <- plan$results_ett[[eid]][[slot]]
      info <- paste0(eid, " / ", slot)
      expect_true(data.table::is.data.table(curve), info = info)
      expect_true(isTRUE(nrow(curve) > 1L), info = info)

      # WIDE: one row per band, both arms as columns. A long form would give
      # two rows per band and every reader would have to reshape it.
      expect_true(
        all(c("surv_comparator", "surv_intervention") %in% names(curve)),
        info = info
      )
      expect_identical(nrow(curve), data.table::uniqueN(curve$tstop))

      # It IS a survival curve: bounded in [0, 1] and non-increasing.
      for (arm in c("surv_comparator", "surv_intervention")) {
        s <- curve[[arm]]
        expect_true(all(s >= 0 & s <= 1), info = paste0(info, " / ", arm))
        expect_true(all(diff(s) <= 1e-12), info = paste0(info, " / ", arm))
      }

      # The stored curve is the one the risk difference was read off:
      # RD(t) = S_comparator(t) - S_intervention(t), at every band.
      expect_equal(
        curve$rd,
        curve$surv_comparator - curve$surv_intervention,
        info = info
      )

      # The row is the LAST band of this curve, not the first. Storing the
      # curve where the one-row summary belongs would report band 1 under the
      # header for the end of follow-up.
      row <- plan$results_ett[[eid]][[sub("^rd_curve_", "rd_", slot)]]
      expect_equal(row$rd, curve$rd[which.max(curve$tstop)], info = info)

      # The replicate matrix is dropped. Kept, it is about 156 KB per curve.
      expect_null(attr(curve, "rd_boot", exact = TRUE))
      expect_identical(attr(curve, "n_boot", exact = TRUE), 500L)
      expect_identical(attr(curve, "seed", exact = TRUE), 1L)
      expect_equal(attr(curve, "conf_level", exact = TRUE), 0.95)
    }
  }
})

test_that("s3 reads the confidence level from the study specification", {
  # The level is a STUDY property. A study that wants 80 percent intervals
  # writes 80 percent once, and every stored result carries it. Fixing the
  # level at 0.95 inside s3 would take that capability away silently, which is
  # the class of defect this whole phase removes.
  wide_dir <- withr::local_tempdir()
  wide <- .abs_run_s3(.abs_plan(wide_dir, conf_level = 0.80), wide_dir)

  default_dir <- withr::local_tempdir()
  default <- .abs_run_s3(.abs_plan(default_dir), default_dir)

  # The specification names 0.80, so 0.80 is on every row and every curve.
  for (slot in c("rd_pp_trunc", "rd_itt")) {
    for (eid in wide$ett$ett_id) {
      row <- wide$results_ett[[eid]][[slot]]
      expect_equal(
        as.numeric(row$conf_level),
        0.80,
        info = paste0(eid, " / ", slot)
      )
      curve <- wide$results_ett[[eid]][[sub("^rd_", "rd_curve_", slot)]]
      expect_equal(
        attr(curve, "conf_level", exact = TRUE),
        0.80,
        info = paste0(eid, " / ", slot)
      )
    }
  }

  # No specification at all, so the default applies.
  expect_null(default$spec)
  expect_equal(
    as.numeric(default$results_ett[["ett01"]]$rd_pp_trunc$conf_level),
    0.95
  )

  # The level reached the ESTIMATOR, not only the record of it. An 80 percent
  # interval is strictly inside the 95 percent one on the same data, and the
  # two runs share n_boot, seed and panel, so nothing else can move the
  # bounds. A `conf_level` that were recorded but not applied would give
  # identical bounds and pass every assertion above.
  narrower <- 0L
  for (eid in wide$ett$ett_id) {
    w <- wide$results_ett[[eid]]$rd_pp_trunc
    d <- default$results_ett[[eid]]$rd_pp_trunc
    if (!is.finite(w$rd_lo) || !is.finite(d$rd_lo)) {
      next
    }
    expect_gte(w$rd_lo, d$rd_lo)
    expect_lte(w$rd_hi, d$rd_hi)
    if (w$rd_lo > d$rd_lo || w$rd_hi < d$rd_hi) {
      narrower <- narrower + 1L
    }
  }
  expect_gt(narrower, 0L)

  # A level outside (0, 1) is refused in the parent, BEFORE any dispatch, and
  # the message names the field. Left to the worker it would surface as a
  # subprocess failure with no field name in it. The dispatcher is mocked to
  # raise, so reaching it at all is a distinguishable failure.
  bad_dir <- withr::local_tempdir()
  bad <- .abs_plan(bad_dir, conf_level = 95)
  local({
    testthat::local_mocked_bindings(
      .batch_run = function(...) stop("__ABS_DISPATCHED_BEFORE_VALIDATION__"),
      .package = "swereg"
    )
    expect_error(
      suppressMessages(utils::capture.output(
        bad$s3_analyze(output_dir = bad_dir, n_workers = 1L),
        type = "output"
      )),
      "study\\$implementation\\$conf_level"
    )
  })
})

test_that("s3 emits exactly two risk-difference work items per ETT", {
  # The cost of the invariant, measured rather than assumed. Each item is its
  # own worker process with its own read of the analysis file, so this count
  # is the count of extra reads a production grid pays.
  output_dir <- withr::local_tempdir()
  plan <- .abs_plan(output_dir)

  captured <- NULL
  testthat::local_mocked_bindings(
    .batch_run = function(target, items, n_workers, ...) {
      if (!identical(target$symbol, ".s3_ett_worker")) {
        return(stats::setNames(vector("list", length(items)), names(items)))
      }
      captured <<- items
      stop("__ABS_CAPTURE__")
    },
    .package = "swereg"
  )
  expect_error(
    suppressMessages(utils::capture.output(
      plan$s3_analyze(output_dir = output_dir, n_workers = 1L),
      type = "output"
    )),
    "__ABS_CAPTURE__"
  )

  methods <- vapply(captured, function(it) it$method, character(1))
  rd_items <- captured[methods == "risk_difference"]
  n_ett <- nrow(plan$ett)
  expect_identical(length(rd_items), 2L * n_ett)
  expect_identical(length(captured), 7L * n_ett)

  # One per estimand and weight combination. Per-protocol on the untruncated
  # weight carries rates and the IRR only.
  weights <- vapply(rd_items, function(it) it$weight_col, character(1))
  expect_setequal(
    unique(weights),
    c("analysis_weight_pp_trunc", "ipw_trunc")
  )

  # The ITT item reads the ITT panel. Reading the PP panel under the ITT
  # weight would report a per-protocol quantity under an ITT heading.
  itt <- rd_items[weights == "ipw_trunc"]
  expect_true(all(grepl("analysis_itt_", vapply(
    itt,
    function(it) basename(it$analysis_path),
    character(1)
  ), fixed = TRUE)))

  # Every formal of the target, named on every item -- the contract
  # .batch_run enforces at the far end.
  fml <- names(formals(swereg:::.s3_ett_worker))
  for (it in rd_items) {
    expect_true(setequal(names(it), fml))
  }
})

test_that("the risk-difference item survives the real batch worker boundary", {
  # The transport, not a mock: .batch_run -> batchit's generic worker
  # subprocess -> .s3_ett_worker -> result envelope -> plan$results_ett. A
  # mocked dispatcher cannot see a serialization or formal-validation failure
  # on the new item, and both would surface only here.
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("yaml")
  dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(dev_tree, "R", "batch_adapter.R")),
    "package source tree not available"
  )

  sk <- ttm_skeleton("A", n_persons = 1200L, seed = 2026L)
  root <- withr::local_tempdir()
  dirs <- list(
    spec = file.path(root, "spec"),
    tteplan = file.path(root, "tteplan"),
    results = file.path(root, "results"),
    meta = file.path(root, "meta")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  skel_path <- file.path(dirs$tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dirs$spec, "spec_v001.yaml"),
    "absprod",
    "rd_age_continuous"
  )

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs$meta),
    candidate_dir_spec = dirs$spec,
    candidate_dir_tteplan = dirs$tteplan,
    candidate_dir_results = dirs$results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)]
  )
  dev_path <- ttm_dev_path()
  invisible(utils::capture.output(
    {
      plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = dev_path
      )
      plan$s2_generate_analysis_files_and_ipcw_pp(
        n_workers = 1L,
        swereg_dev_path = dev_path
      )
      plan$s3_analyze(n_workers = 1L, swereg_dev_path = dev_path)
    },
    type = "output"
  ))

  expect_gt(length(plan$results_ett), 0L)
  for (eid in plan$ett$ett_id) {
    r <- plan$results_ett[[eid]]
    for (slot in c("rd_pp_trunc", "rd_itt")) {
      row <- r[[slot]]
      info <- paste0(eid, " / ", slot)
      expect_true(data.table::is.data.table(row), info = info)
      expect_true(all(.ABS_ROW_FIELDS %in% names(row)), info = info)
      expect_true(is.finite(row$rd), info = info)
      expect_true(
        as.character(row$interval_status) %in% .ABS_STATUSES,
        info = info
      )
    }
    for (slot in c("rd_curve_pp_trunc", "rd_curve_itt")) {
      curve <- r[[slot]]
      expect_true(data.table::is.data.table(curve), info = slot)
      expect_true(
        all(c("surv_comparator", "surv_intervention") %in% names(curve)),
        info = slot
      )
      expect_null(attr(curve, "rd_boot", exact = TRUE))
    }
  }
})
