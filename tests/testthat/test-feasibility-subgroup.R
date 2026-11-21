# Subgroup rows in the feasibility sheets. Arm sizes, Minimum events and
# Longest reportable carry one row per subgroup level below the row of each
# comparison, in feasibility.xlsx of `$s3_feasibility()`, and in
# feasibility.xlsx and tables.xlsx of `$export_tables()`.
#
# The fixture is the plan of `.ff_plan()`: enrollments 01 and 02, one
# outcome, follow-ups of 52 and 104 weeks, and the subgroup variable `rd_sg`,
# which is also a confounder. Every analysis file and raw file holds the
# panel of `.hl_panel(thin_events = 2)` without the persons of `FS_DROP`. So
# the level "thin" has 2 events per arm and fails the rule at 5, and the
# level "big" passes it. Persons with `rd_sg = NA` belong to no level of the
# rule, and the Arm sizes sheet counts them as `missing`.

FS_NAME <- "Risk group"
FS_LEVELS <- c("big", "thin")
# One person with no subgroup value and one "big" person of the intervention
# arm, so the arms differ in the counts.
FS_DROP <- c("p0109", "p0150")
FS_SHEETS <- c("Arm sizes", FF_MIN_SHEETS)

.fs_skip <- function() {
  .ff_skip()
  skip_if_not_installed("qs2")
}

# The spec of `.ff_spec()` with `rd_sg` as a confounder, threshold 5 and no
# feasibility follow-up. `subgroups = FALSE` drops the `subgroups:` entry.
.fs_spec <- function(subgroups = TRUE) {
  spec <- .ff_spec(NULL, min_events = 5)
  spec$confounders <- c(
    spec$confounders,
    list(list(name = FS_NAME, implementation = list(variable = "rd_sg")))
  )
  spec$subgroups[[1L]]$name <- FS_NAME
  if (!subgroups) {
    spec$subgroups <- NULL
  }
  return(spec)
}

# The plan, its files, `$s3_feasibility()`, and with subgroups also
# `$s3_analyze()` and `$export_tables()`. Built once per variant.
.fs_cache <- new.env()
.fs_fixture <- function(subgroups = TRUE) {
  name <- if (subgroups) "sg" else "none"
  if (!is.null(.fs_cache[[name]]) && dir.exists(.fs_cache[[name]]$root)) {
    return(.fs_cache[[name]])
  }
  root <- tempfile(paste0("fs_", name, "_"))
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = testthat::teardown_env())
  plan <- .ff_plan(.fs_spec(subgroups), root)
  dir <- file.path(root, "tteplan")
  panel <- .hl_panel(thin_events = 2L)
  panel <- panel[!panel$id %in% FS_DROP]
  .ff_write_files(plan, dir, panel, c("rd_age_continuous", "rd_sg"))
  plan$output_dir <- dir
  counts <- function(n) {
    return(list(
      matching = data.table::data.table(
        enrollment_period_id = 1L,
        n_intervention_total = n,
        n_comparator_total = 3 * n,
        n_intervention_enrolled = n,
        n_comparator_enrolled = 2 * n
      )
    ))
  }
  plan$enrollment_counts <- list(`01` = counts(111), `02` = counts(50))
  tte_stamp_stages(plan)
  dev <- swereg:::.swereg_dev_path()
  out <- utils::capture.output(suppressWarnings(plan$s3_feasibility(
    output_dir = dir,
    n_workers = 2L,
    swereg_dev_path = dev
  )))
  fz_xlsx <- file.path(root, "s3_feasibility.xlsx")
  file.copy(file.path(plan$dir_results, "feasibility.xlsx"), fz_xlsx)
  fx <- list(root = root, dir = dir, plan = plan, fz_xlsx = fz_xlsx, out = out)
  if (subgroups) {
    out_s3 <- utils::capture.output(suppressWarnings(plan$s3_analyze(
      output_dir = dir,
      n_workers = 2L,
      swereg_dev_path = dev
    )))
    xlsx <- file.path(root, "results", "tables.xlsx")
    suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
    fx$xlsx <- xlsx
    fx$ex_xlsx <- file.path(dirname(xlsx), "feasibility.xlsx")
    fx$out_s3 <- out_s3
  }
  .fs_cache[[name]] <- fx
  return(fx)
}

# The baseline counts per arm of each level, computed here from the raw file
# of enrollment `eid`. The level `missing` counts the rows with no value.
.fs_raw_counts <- function(fx, eid) {
  ett <- fx$plan$ett
  f <- unique(ett$file_raw[ett$enrollment_id == eid])
  enr <- qs2::qs_read(file.path(fx$dir, f))
  b <- enr$data[enr$data$tstart == 0]
  x <- b$rd_sg
  arm <- b$rd_intervention
  lv <- c(FS_LEVELS, "missing")
  hit <- function(l) {
    return(if (l == "missing") is.na(x) else x %in% l)
  }
  return(data.frame(
    level = lv,
    n_intervention = vapply(lv, function(l) sum(hit(l) & arm), 1L),
    n_comparator = vapply(lv, function(l) sum(hit(l) & !arm), 1L),
    row.names = NULL
  ))
}

# The rows of the Arm sizes sheet, under its header row, as text.
.fs_arm <- function(path) {
  m <- .ff_cells(path, "Arm sizes")
  h <- which(m[[1L]] %in% "Enrollment")[1L]
  out <- m[-seq_len(h), , drop = FALSE]
  names(out) <- as.character(unlist(m[h, ]))
  out <- out[!is.na(out$Comparison), , drop = FALSE]
  rownames(out) <- NULL
  # The note on the level rows is the row below the table.
  from <- h + 1L + nrow(out)
  below <- if (from <= nrow(m)) m[[1L]][from:nrow(m)] else character(0)
  return(list(rows = out, note = below[!is.na(below)][1L]))
}

# The rows of a grid sheet, Minimum events or Longest reportable: the label
# in column 2 and every cell, as text. Subgroup rows have no ID, so the rows
# are found by their label.
.fs_grid <- function(path, sheet) {
  g <- .ff_grid(path, sheet)
  m <- g$m
  rows <- seq.int(g$first_row, nrow(m))
  rows <- rows[!is.na(m[[2L]][rows])]
  return(list(g = g, label = m[[2L]][rows], m = m[rows, , drop = FALSE]))
}

# The cell of the row `label`, outcome "Outcome A" and column `col`.
.fs_cell <- function(grid, label, col) {
  r <- which(grid$label %in% label)
  stopifnot(length(r) == 1L)
  return(grid$m[[.ff_grid_col(grid$g, "Outcome A", col)]][r])
}

# The comparison label of enrollment `eid`, as the Arm sizes sheet writes it.
.fs_cmp <- function(fx, eid) {
  return(swereg:::.enrollment_label(fx$plan, eid))
}

.fs_label <- function(fx, eid, level) {
  return(paste0(.fs_cmp(fx, eid), " | ", FS_NAME, ": ", level))
}

# The per-protocol record of level `level` in the ETT of enrollment `eid`
# with follow-up `weeks`. `$s3_feasibility()` reads its own records. The
# workbooks of `$export_tables()` read the `hsub_rd_sg_pp` slot that s3
# stored. So each workbook is checked against the records it was built from.
.fs_level_rec <- function(fx, eid, weeks, level, export) {
  ett <- fx$plan$ett
  id <- ett$ett_id[ett$enrollment_id == eid & ett$follow_up == weeks]
  if (export) {
    return(fx$plan$results_ett[[id]]$hsub_rd_sg_pp[[level]])
  }
  return(fx$plan$feasibility$records[[id]]$pp$subgroups$rd_sg[[level]])
}

# The expected rows of a grid sheet: each comparison, then its levels.
# The expected rows of a grid sheet: each comparison, then its levels, then
# the level `missing` when the baseline holds an NA subgroup value.
.fs_grid_labels <- function(fx, missing = TRUE) {
  lv <- c(FS_LEVELS, if (missing) "missing")
  return(unlist(lapply(c("01", "02"), function(eid) {
    return(c(.fs_cmp(fx, eid), .fs_label(fx, eid, lv)))
  })))
}

# The sentence of both grid sheets on the level `missing`.
FS_MISSING_NOTE <- "because no estimate is made for it, so its cells read n/a."

# Every cell of the `missing` row of each comparison reads n/a, in both grid
# sheets, and both notes say why.
.fs_expect_missing_rows <- function(fx, path) {
  for (sh in FF_MIN_SHEETS) {
    grid <- .fs_grid(path, sh)
    for (eid in c("01", "02")) {
      r <- which(grid$label %in% .fs_label(fx, eid, "missing"))
      expect_length(r, 1L)
      cells <- unlist(grid$m[r, -(1:2)], use.names = FALSE)
      expect_gt(length(cells), 0L)
      expect_identical(cells, rep("n/a", length(cells)), info = paste(sh, eid))
    }
    note <- .ff_cells(path, sh)[[1L]][2L]
    expect_match(note, FS_MISSING_NOTE, fixed = TRUE, info = sh)
  }
}

# The workbooks of the three paths, by name.
.fs_books <- function(fx) {
  return(c(
    s3_feasibility = fx$fz_xlsx,
    export_feasibility = fx$ex_xlsx,
    tables = fx$xlsx
  ))
}

# Every assertion on the subgroup rows of one workbook.
.fs_expect_book <- function(fx, path, export) {
  # Minimum events and Longest reportable: the levels follow the comparison.
  for (sh in FF_MIN_SHEETS) {
    grid <- .fs_grid(path, sh)
    expect_identical(grid$label, .fs_grid_labels(fx), info = sh)
  }
  me <- .fs_grid(path, "Minimum events")
  lr <- .fs_grid(path, "Longest reportable")
  years <- c("1 year" = 52, "2 years" = 104)
  for (eid in c("01", "02")) {
    for (lv in FS_LEVELS) {
      label <- .fs_label(fx, eid, lv)
      pass <- logical(0)
      for (y in names(years)) {
        rec <- .fs_level_rec(fx, eid, years[[y]], lv, export)
        want <- if (is.na(rec$max_passing_cut)) {
          "-"
        } else {
          format(rec$max_passing_cut, trim = TRUE)
        }
        want <- sub("^20$", "≥20", want)
        expect_identical(.fs_cell(me, label, y), want, info = label)
        pass <- c(pass, !isTRUE(rec$withheld))
      }
      want_lr <- if (!pass[1L]) {
        "none"
      } else if (!pass[2L]) {
        "1 year"
      } else {
        "2 years"
      }
      expect_identical(
        .fs_cell(lr, label, "threshold 5"),
        want_lr,
        info = label
      )
    }
    # The thin level passes a lower cut than the big level.
    cut_of <- function(lv) {
      v <- .fs_cell(me, .fs_label(fx, eid, lv), "1 year")
      v <- sub("≥", "", v)
      return(if (v == "-") 0 else as.numeric(v))
    }
    expect_lt(cut_of("thin"), cut_of("big"))
  }

  # Arm sizes: enrolled counts of each level, from the raw baseline rows.
  arm <- .fs_arm(path)
  expect_match(arm$note, "n/a", fixed = TRUE)
  rows <- arm$rows
  for (eid in c("01", "02")) {
    want <- .fs_raw_counts(fx, eid)
    expect_gt(want$n_intervention[want$level == "missing"], 0L)
    at <- which(rows$Enrollment %in% eid)
    expect_length(at, 1L)
    got <- rows[at + seq_len(nrow(want)), , drop = FALSE]
    expect_identical(got$Comparison, .fs_label(fx, eid, want$level))
    expect_identical(
      as.numeric(got$`Enrolled intervention person-trials`),
      as.numeric(want$n_intervention)
    )
    expect_identical(
      as.numeric(got$`Enrolled comparator person-trials`),
      as.numeric(want$n_comparator)
    )
    for (nm in c(
      "Eligible intervention person-trials",
      "Eligible comparator person-trials",
      "Comparators per intervention (eligible)",
      "Comparators per intervention (target)"
    )) {
      expect_identical(got[[nm]], rep("n/a", nrow(want)), info = nm)
    }
  }
  expect_identical(nrow(rows), 2L * (1L + length(FS_LEVELS) + 1L))
}


# ---- the three workbooks -------------------------------------------------

test_that("$s3_feasibility(): feasibility.xlsx has a row per subgroup level in all three sheets", {
  .fs_skip()
  fx <- .fs_fixture()
  .fs_expect_book(fx, fx$fz_xlsx, export = FALSE)
  .fs_expect_missing_rows(fx, fx$fz_xlsx)
})

test_that("$export_tables(): feasibility.xlsx has a row per subgroup level in all three sheets", {
  .fs_skip()
  fx <- .fs_fixture()
  .fs_expect_book(fx, fx$ex_xlsx, export = TRUE)
  .fs_expect_missing_rows(fx, fx$ex_xlsx)
})

test_that("$export_tables(): tables.xlsx has a row per subgroup level in all three sheets", {
  .fs_skip()
  fx <- .fs_fixture()
  .fs_expect_book(fx, fx$xlsx, export = TRUE)
  .fs_expect_missing_rows(fx, fx$xlsx)
})

test_that("the subgroup rows agree between $s3_feasibility() and both workbooks of $export_tables()", {
  .fs_skip()
  fx <- .fs_fixture()
  books <- .fs_books(fx)
  for (sh in FF_MIN_SHEETS) {
    ref <- .fs_grid(books[["s3_feasibility"]], sh)
    for (b in names(books)[-1L]) {
      got <- .fs_grid(books[[b]], sh)
      expect_identical(got$label, ref$label, info = paste(b, sh))
      expect_identical(
        unname(as.list(got$m)),
        unname(as.list(ref$m)),
        info = paste(b, sh)
      )
    }
  }
  ref <- .fs_arm(books[["s3_feasibility"]])$rows
  for (b in names(books)[-1L]) {
    got <- .fs_arm(books[[b]])$rows
    lv <- grepl(" | ", got$Comparison, fixed = TRUE)
    expect_identical(got[lv, ], ref[lv, ], info = b)
  }
})


# ---- outcome-blindness ----------------------------------------------------

test_that("no per-level record of either path holds an event, rate or expected-events field", {
  .fs_skip()
  fx <- .fs_fixture()
  feas <- lapply(fx$plan$feasibility$records, function(r) r$pp)
  export <- swereg:::.acc_horizon_records(fx$plan, "pp", subgroups = TRUE)
  for (path in list(feas, export)) {
    n <- 0L
    for (rec in path) {
      for (lv in rec$subgroups$rd_sg) {
        n <- n + 1L
        expect_length(intersect(names(lv), .FZ_STRIPPED), 0L)
        expect_length(intersect(names(lv$windows), .FZ_STRIPPED), 0L)
        expect_identical(
          names(lv$windows),
          c("window_lo", "window_hi", "arm", "py_weighted", "pass")
        )
      }
    }
    # Four ETTs, two levels each.
    expect_identical(n, 8L)
  }
  for (b in .fs_books(fx)) {
    for (sh in FS_SHEETS) {
      cells <- unlist(.ff_cells(b, sh), use.names = FALSE)
      expect_length(intersect(cells, .FZ_STRIPPED), 0L)
    }
  }
})


# ---- the passing direction ------------------------------------------------

test_that("a plan without subgroups: keeps one row per comparison in all three sheets", {
  .fs_skip()
  fx <- .fs_fixture(subgroups = FALSE)
  for (rec in fx$plan$feasibility$records) {
    expect_null(rec$pp$subgroups)
  }
  for (sh in FF_MIN_SHEETS) {
    grid <- .fs_grid(fx$fz_xlsx, sh)
    expect_identical(grid$label, c(.fs_cmp(fx, "01"), .fs_cmp(fx, "02")))
  }
  arm <- .fs_arm(fx$fz_xlsx)
  expect_identical(arm$rows$Enrollment, c("01", "02"))
  expect_true(is.na(arm$note))
})


test_that("subgroup counts with no NA value: no missing row in any of the three sheets", {
  .fs_skip()
  fx <- .fs_fixture()
  # The stored counts of each enrollment without the level `missing`, as
  # s3 stores them for a subgroup variable with a value in every row.
  counts <- lapply(c(`01` = "01", `02` = "02"), function(eid) {
    sc <- fx$plan$results_enrollment[[eid]]$subgroup_counts
    expect_true("missing" %in% sc$level)
    return(sc[sc$level != "missing"])
  })
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))
  swereg:::.write_feasibility_xlsx(
    fx$plan,
    swereg:::.tte_feasibility_baselines(fx$plan, c("01", "02")),
    path,
    adherence = list(),
    records = swereg:::.acc_horizon_records(fx$plan, "pp", subgroups = TRUE),
    subgroup_counts = counts
  )
  for (sh in FF_MIN_SHEETS) {
    expect_identical(
      .fs_grid(path, sh)$label,
      .fs_grid_labels(fx, missing = FALSE),
      info = sh
    )
    note <- .ff_cells(path, sh)[[1L]][2L]
    expect_false(grepl(FS_MISSING_NOTE, note, fixed = TRUE), info = sh)
  }
  arm <- .fs_arm(path)$rows
  expect_false(any(grepl(": missing", arm$Comparison, fixed = TRUE)))
  expect_identical(nrow(arm), 2L * (1L + length(FS_LEVELS)))
})

test_that("results with subgroups but no subgroup_counts are refused on both paths", {
  .fs_skip()
  fx <- .fs_fixture()
  plan <- fx$plan
  stored <- plan$results_enrollment[["02"]]$subgroup_counts
  expect_true(data.table::is.data.table(stored))
  withr::defer(plan$results_enrollment[["02"]]$subgroup_counts <- stored)
  plan$results_enrollment[["02"]]$subgroup_counts <- NULL
  # The feasibility path: the writer of $s3_feasibility(), with no counts
  # from a worker and none stored.
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))
  expect_error(
    swereg:::.write_feasibility_xlsx(
      plan,
      list(),
      path,
      adherence = list(),
      subgroup_counts = list(`01` = stored, `02` = NULL)
    ),
    "enrollment 02 has subgroup variables.*Rerun \\$s3_feasibility\\(\\)"
  )
  # The export path: tables.xlsx and its feasibility.xlsx.
  xlsx <- file.path(tempfile("fs_refuse_"), "tables.xlsx")
  dir.create(dirname(xlsx))
  withr::defer(unlink(dirname(xlsx), recursive = TRUE))
  expect_error(
    suppressMessages(suppressWarnings(plan$export_tables(path = xlsx))),
    "enrollment 02 has subgroup variables.*Rerun \\$s3_analyze\\(\\)"
  )
  expect_error(
    swereg:::.plan_export_feasibility(
      plan,
      file.path(dirname(xlsx), "feasibility.xlsx"),
      c("01", "02")
    ),
    "enrollment 02 has subgroup variables.*Rerun \\$s3_analyze\\(\\)"
  )
  expect_false(file.exists(xlsx))
})


test_that("the enrollment worker returns subgroup_counts only when it counts", {
  .fs_skip()
  fx <- .fs_fixture()
  item <- swereg:::.tte_s3_enrollment_item(fx$plan, "01", fx$dir, 1L)$item
  expect_identical(item$subgroup_vars, "rd_sg")
  got <- do.call(swereg:::.s3_enrollment_worker, item)
  expect_identical(
    got$subgroup_counts,
    fx$plan$results_enrollment[["01"]]$subgroup_counts
  )
  # `$recompute_baselines()` passes no subgroup variables. The result then
  # holds no name `subgroup_counts`, so the stored counts are carried.
  item$subgroup_vars <- character(0)
  got <- do.call(swereg:::.s3_enrollment_worker, item)
  expect_false("subgroup_counts" %in% names(got))
})


test_that("a missing row reads n/a in the columns of an outcome not in the data", {
  .fs_skip()
  fx <- .fs_fixture()
  # "Outcome A" counts 0 persons in the whole dataset, so its columns are
  # blank on every other row.
  testthat::local_mocked_bindings(
    .tte_not_in_data = function(plan) {
      return(list(outcomes = "Outcome A", enrollments = character(0)))
    },
    .package = "swereg"
  )
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))
  swereg:::.write_feasibility_xlsx(
    fx$plan,
    swereg:::.tte_feasibility_baselines(fx$plan, c("01", "02")),
    path,
    adherence = list(),
    records = swereg:::.acc_horizon_records(fx$plan, "pp", subgroups = TRUE),
    subgroup_counts = lapply(c(`01` = "01", `02` = "02"), function(eid) {
      return(fx$plan$results_enrollment[[eid]]$subgroup_counts)
    })
  )
  for (sh in FF_MIN_SHEETS) {
    grid <- .fs_grid(path, sh)
    cols <- which(grid$g$outcome %in% "Outcome A")
    expect_gt(length(cols), 0L)
    for (eid in c("01", "02")) {
      miss <- which(grid$label %in% .fs_label(fx, eid, "missing"))
      expect_length(miss, 1L)
      expect_identical(
        unlist(grid$m[miss, cols], use.names = FALSE),
        rep("n/a", length(cols)),
        info = paste(sh, eid)
      )
      # The row of the comparison is blank there: the outcome is absent.
      cmp <- which(grid$label %in% .fs_cmp(fx, eid))
      got <- unlist(grid$m[cmp, cols], use.names = FALSE)
      expect_true(all(is.na(got) | got == ""), info = paste(sh, eid))
    }
  }
})


# ---- the count helper -----------------------------------------------------

test_that(".tte_subgroup_baseline_counts() counts NA as missing and stops on an absent column", {
  base <- data.table::data.table(
    tx = c(TRUE, TRUE, FALSE, FALSE, FALSE, NA),
    g = c("b", NA, "a", NA, NA, "a")
  )
  got <- swereg:::.tte_subgroup_baseline_counts(base, "g", "tx")
  expect_identical(got$level, c("a", "b", "missing"))
  expect_identical(got$n_intervention, c(0L, 1L, 1L))
  expect_identical(got$n_comparator, c(1L, 0L, 2L))
  expect_null(swereg:::.tte_subgroup_baseline_counts(base, character(0), "tx"))
  expect_error(
    swereg:::.tte_subgroup_baseline_counts(base, "absent", "tx"),
    "absent"
  )
})
