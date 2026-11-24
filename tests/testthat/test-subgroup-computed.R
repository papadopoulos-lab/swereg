# A computed lookback confounder as a subgroup.
#
# A computed confounder has no skeleton column. `tteplan_read_spec()` names
# its column `rd_no_<source>_<window>`, and s1 derives that column. The
# subgroup check of `tteplan_validate_spec()` accepts that derived name.
#
# The fixture is the study of helper-sparse_rule.R with two confounders, Age
# and "Prior common condition" (computed on `osd_common`, lifetime before
# baseline), and no rare_condition_rule. The subgroup is the derived column
# `rd_no_osd_common_everbefore`. It is TRUE when no earlier week holds
# `osd_common`, so the subgroup is named for the TRUE level. The threshold of
# the minimum-events rule is 1, so a level with no events fails it.

SGC_NAME <- "No prior common condition"
SGC_VAR <- spr_var("osd_common")
SGC_EIDS <- c("01", "02")

.sgc_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("survey")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
}

# The spec list. `subgroup` is the subgroup variable, or NULL for none.
.sgc_spec <- function(subgroup = SGC_VAR) {
  spec <- spr_spec_list(NULL, project_prefix = "sgc")
  # Age and "Prior common condition".
  spec$confounders <- spec$confounders[c(1L, 3L)]
  spec$min_expected_events_per_arm_per_year <- 1
  if (!is.null(subgroup)) {
    spec$subgroups <- list(list(
      name = SGC_NAME,
      implementation = list(variable = subgroup)
    ))
  }
  return(spec)
}

# The spec read back through the real reader, as s0 reads it.
.sgc_read <- function(subgroup) {
  dir <- withr::local_tempdir()
  path <- spr_write_spec(.sgc_spec(subgroup), dir)
  return(suppressMessages(swereg::tteplan_read_spec(path)))
}

# Run the validator, and return its error message, or NULL.
.sgc_validate_error <- function(subgroup, sk) {
  spec <- .sgc_read(subgroup)
  return(tryCatch(
    {
      suppressMessages(suppressWarnings(
        swereg::tteplan_validate_spec(spec, sk)
      ))
      NULL
    },
    error = function(e) conditionMessage(e)
  ))
}

.sgc_small_skeleton <- function() {
  if (is.null(.sgc_cache$small)) {
    .sgc_cache$small <- spr_skeleton(n_persons = 40L)
  }
  return(.sgc_cache$small)
}

# s0 (which validates the spec), s1, s2, `$s3_analyze()`,
# `$s3_feasibility()` and `$export_tables()`, through the production
# methods. Built once per file.
.sgc_cache <- new.env()
.sgc_fixture <- function() {
  if (!is.null(.sgc_cache$fx) && dir.exists(.sgc_cache$fx$root)) {
    return(.sgc_cache$fx)
  }
  sk <- spr_skeleton()
  root <- tempfile("sgc_")
  withr::defer(unlink(root, recursive = TRUE), envir = testthat::teardown_env())
  dirs <- file.path(root, c("spec", "tteplan", "results", "meta"))
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  skel_path <- file.path(dirs[2], SPR_SKELETON)
  qs2::qs_save(sk, skel_path)
  spec <- .sgc_spec()
  spec$admin_censoring_isoyearweek <- sk[, max(isoyearweek, na.rm = TRUE)]
  spr_write_spec(spec, dirs[1])
  dev <- swereg:::.swereg_dev_path()
  fz_xlsx <- file.path(root, "s3_feasibility.xlsx")
  xlsx <- file.path(dirs[3], "tables.xlsx")
  plan <- NULL
  suppressMessages(suppressWarnings(invisible(utils::capture.output(
    {
      plan <- swereg::tteplan_from_spec_and_registrystudy(
        study = list(skeleton_files = skel_path, data_meta_dir = dirs[4]),
        candidate_dir_spec = dirs[1],
        candidate_dir_tteplan = dirs[2],
        candidate_dir_results = dirs[3],
        spec_version = "v001",
        check_skeletons = FALSE
      )
      plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = dev,
        check_skeletons = FALSE
      )
      plan$s2_generate_analysis_files_and_ipcw_pp(
        n_workers = 1L,
        swereg_dev_path = dev
      )
      plan$s3_analyze(n_workers = 1L, swereg_dev_path = dev)
      plan$s3_feasibility(n_workers = 1L, swereg_dev_path = dev)
      file.copy(file.path(plan$dir_results, "feasibility.xlsx"), fz_xlsx)
      plan$export_tables(path = xlsx)
    },
    type = "output"
  ))))
  .sgc_cache$fx <- list(
    root = root,
    dir = dirs[2],
    sk = sk,
    plan = plan,
    books = c(
      s3_feasibility = fz_xlsx,
      export_feasibility = file.path(dirs[3], "feasibility.xlsx"),
      tables = xlsx
    )
  )
  return(.sgc_cache$fx)
}

# The baseline rows (`tstart == 0`) of the s1 raw file of enrollment `eid`.
.sgc_baseline <- function(fx, eid) {
  ett <- fx$plan$ett
  f <- unique(ett$file_raw[ett$enrollment_id == eid])
  stopifnot(length(f) == 1L)
  enr <- qs2::qs_read(file.path(fx$dir, f))
  return(list(
    base = enr$data[enr$data$tstart == 0],
    treatment_var = enr$design$treatment_var
  ))
}

# The enrolled person-trials per arm of each level, counted here from the
# baseline rows of the raw file. The count reads the entry-snapshot value
# of the subgroup when the raw file holds that column, and SGC_VAR when it
# does not. A missing value counts as the level `missing`, which follows
# the other levels.
.sgc_raw_counts <- function(fx, eid) {
  b <- .sgc_baseline(fx, eid)
  entry <- swereg:::.tte_entry_col(SGC_VAR)
  x <- b$base[[if (entry %in% names(b$base)) entry else SGC_VAR]]
  arm <- b$base[[b$treatment_var]]
  lv <- c("FALSE", "TRUE", if (anyNA(x)) "missing")
  hit <- function(l) {
    return(if (l == "missing") is.na(x) else as.character(x) %in% l)
  }
  return(data.frame(
    level = lv,
    n_intervention = vapply(lv, function(l) sum(hit(l) & arm %in% TRUE), 1L),
    n_comparator = vapply(lv, function(l) sum(hit(l) & arm %in% FALSE), 1L),
    row.names = NULL
  ))
}

.sgc_label <- function(fx, eid, level) {
  return(paste0(
    swereg:::.enrollment_label(fx$plan, eid),
    " | ",
    SGC_NAME,
    ": ",
    level
  ))
}

# The rows of the Arm sizes sheet, under its header row, as text.
.sgc_arm <- function(path) {
  m <- .ff_cells(path, "Arm sizes")
  h <- which(m[[1L]] %in% "Enrollment")[1L]
  out <- m[-seq_len(h), , drop = FALSE]
  names(out) <- as.character(unlist(m[h, ]))
  out <- out[!is.na(out$Comparison), , drop = FALSE]
  rownames(out) <- NULL
  return(out)
}

# The labels of the rows of a grid sheet, Minimum events or Longest
# reportable, in sheet order.
.sgc_grid_labels <- function(path, sheet) {
  g <- .ff_grid(path, sheet)
  m <- g$m
  rows <- seq.int(g$first_row, nrow(m))
  return(m[[2L]][rows[!is.na(m[[2L]][rows])]])
}


# ---- (a), (b): the validator --------------------------------------------

test_that("(a) validation accepts a subgroup that is a computed confounder's derived variable", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  sk <- .sgc_small_skeleton()
  spec <- .sgc_read(SGC_VAR)
  # The reader derives the subgroup name, and no skeleton column holds it.
  expect_identical(spec$confounders[[2L]]$implementation$variable, SGC_VAR)
  expect_false(SGC_VAR %in% names(sk))
  expect_null(.sgc_validate_error(SGC_VAR, sk))
})

test_that("(b) validation still refuses a subgroup that is no skeleton column and no confounder variable, and a skeleton column that is no confounder", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  sk <- .sgc_small_skeleton()

  # Neither a skeleton column nor a confounder variable.
  err <- .sgc_validate_error("rd_not_a_column", sk)
  expect_match(err, "not found in skeleton columns", fixed = TRUE)
  expect_match(err, "must also be a confounder", fixed = TRUE)

  # The derived name of a computed lookback whose source is not a
  # confounder. It has the form of a derived name, and no confounder
  # derives it.
  rare <- spr_var("osd_rare_a")
  expect_false(rare %in% names(sk))
  err <- .sgc_validate_error(rare, sk)
  expect_match(err, "not found in skeleton columns", fixed = TRUE)
  expect_match(err, "must also be a confounder", fixed = TRUE)

  # A skeleton column that is not a confounder.
  expect_true("osd_rare_a" %in% names(sk))
  err <- .sgc_validate_error("osd_rare_a", sk)
  expect_match(err, "must also be a confounder", fixed = TRUE)
  expect_no_match(err, "not found in skeleton columns", fixed = TRUE)
})


# ---- (c), (d): end to end -----------------------------------------------

test_that("(c) end to end, $get_subgroups() has both levels of the derived column, with per-level horizon_status", {
  .sgc_skip()
  fx <- .sgc_fixture()
  expect_false(is.null(fx$plan))
  sg <- fx$plan$get_subgroups()
  expect_identical(unique(sg$subgroup_var), SGC_VAR)
  ids <- sort(unique(fx$plan$ett$ett_id))
  expect_identical(sort(unique(sg$ett_id)), ids)

  for (id in ids) {
    for (est in c("pp", "itt")) {
      rows <- sg[sg$ett_id == id & sg$estimand == est]
      info <- paste(id, est)
      expect_true(all(rows$strata_stored), info = info)
      expect_setequal(setdiff(rows$subgroup_level, "all"), c("FALSE", "TRUE"))
      recs <- fx$plan$results_ett[[id]][[
        swereg:::.tte_horizon_subgroup_slot(SGC_VAR, est)
      ]]
      for (lv in c("FALSE", "TRUE")) {
        r <- rows[rows$subgroup_level == lv]
        expect_identical(nrow(r), 1L, info = paste(info, lv))
        # The status of the level row is the status of its own record.
        rec <- swereg:::.tte_horizon_level_of(recs, lv)
        expect_false(is.null(rec), info = paste(info, lv))
        expect_identical(r$horizon_status, rec$status, info = paste(info, lv))
        expect_true(nzchar(r$horizon_status), info = paste(info, lv))
      }
    }
  }

  # ETT00001 (enrollment 01): stratified IRRs in both levels, and the
  # interaction test.
  e1 <- sg[sg$ett_id == "ETT00001" & sg$estimand == "pp"]
  expect_true(all(is.finite(e1$irr[e1$subgroup_level %in% c("FALSE", "TRUE")])))
  expect_true(all(is.finite(e1$em_pvalue)))
  expect_true(all(is.finite(e1$ratio_of_irrs)))

  # ETT00002 (enrollment 02, ages 40-55): the level FALSE has no events in
  # one or both arms, so it fails the rule at 1, and the level TRUE passes.
  e2 <- sg[sg$ett_id == "ETT00002" & sg$estimand == "pp"]
  expect_identical(
    e2$horizon_status[e2$subgroup_level == "FALSE"],
    "below minimum-events rule: both arms, weeks 0-52"
  )
  expect_identical(
    e2$horizon_status[e2$subgroup_level == "TRUE"],
    "all windows pass"
  )
})

test_that("(c) the derived column reaches the s1 raw file and the s2 analysis files, TRUE for no earlier event", {
  .sgc_skip()
  fx <- .sgc_fixture()
  ett <- fx$plan$ett
  for (f in c(ett$file_analysis, ett$file_analysis_itt)) {
    enr <- qs2::qs_read(file.path(fx$dir, f))
    expect_true(SGC_VAR %in% names(enr$data), info = f)
  }
  first_week <- min(fx$sk$isoyearweek)
  early <- unique(fx$sk$id[
    fx$sk$osd_common %in% TRUE & fx$sk$isoyearweek == first_week
  ])
  never <- setdiff(unique(fx$sk$id), fx$sk$id[fx$sk$osd_common %in% TRUE])
  # Enrollment 02 takes ages 40-55, and the early holders are aged 55 or
  # more, so it MAY hold none of them. Both enrollments together hold some.
  n_early <- 0L
  for (eid in SGC_EIDS) {
    base <- .sgc_baseline(fx, eid)$base
    expect_true(SGC_VAR %in% names(base), info = eid)
    x <- base[[SGC_VAR]]
    # An event in the first skeleton week precedes every recruiting week.
    n_early <- n_early + sum(base$id %in% early)
    expect_true(all(x[base$id %in% early] %in% FALSE), info = eid)
    expect_gt(sum(base$id %in% never), 0L)
    expect_true(all(x[base$id %in% never] %in% TRUE), info = eid)
  }
  expect_gt(n_early, 0L)
})

test_that("(d) the feasibility workbooks have per-level rows of the derived column, and Arm sizes counts equal the raw baseline rows", {
  .sgc_skip()
  fx <- .sgc_fixture()
  for (b in names(fx$books)) {
    path <- fx$books[[b]]
    expect_true(file.exists(path), info = b)
    want <- lapply(stats::setNames(nm = SGC_EIDS), function(eid) {
      return(.sgc_raw_counts(fx, eid))
    })

    # Minimum events and Longest reportable: each comparison, then its
    # levels.
    labels <- unlist(lapply(SGC_EIDS, function(eid) {
      return(c(
        swereg:::.enrollment_label(fx$plan, eid),
        .sgc_label(fx, eid, want[[eid]]$level)
      ))
    }))
    for (sh in FF_MIN_SHEETS) {
      expect_identical(
        .sgc_grid_labels(path, sh),
        labels,
        info = paste(b, sh)
      )
    }

    # Arm sizes: the enrolled person-trials of each level.
    rows <- .sgc_arm(path)
    for (eid in SGC_EIDS) {
      w <- want[[eid]]
      expect_gt(min(w$n_intervention + w$n_comparator), 0L)
      at <- which(rows$Enrollment %in% eid)
      expect_length(at, 1L)
      got <- rows[at + seq_len(nrow(w)), , drop = FALSE]
      info <- paste(b, eid)
      expect_identical(
        got$Comparison,
        .sgc_label(fx, eid, w$level),
        info = info
      )
      expect_identical(
        as.numeric(got$`Enrolled intervention person-trials`),
        as.numeric(w$n_intervention),
        info = info
      )
      expect_identical(
        as.numeric(got$`Enrolled comparator person-trials`),
        as.numeric(w$n_comparator),
        info = info
      )
    }
    n_levels <- vapply(want, nrow, 1L)
    expect_identical(nrow(rows), as.integer(sum(1L + n_levels)), info = b)
  }
})
