# TTEPlan$s3_feasibility(): the minimum-events rule after s2, before any
# outcome model.
#
# The step answers one question per ETT and estimand: will s3 report this
# estimand, and to which horizon? It reads the same analysis files, weights,
# follow-up and rule as `$s3_analyze()`, because both resolve them through
# `.tte_s3_resolve_ett()`. It calls the same `.tte_horizon_record()`, so it
# cannot decide differently.
#
# The record holds the weighted events, the pooled rate and the expected
# events of each arm and window. `.tte_horizon_decision()` needs the expected
# events, so the worker builds the full record. The worker then keeps only the
# columns of `.FEASIBILITY_WINDOW_COLS` and the fields of
# `.FEASIBILITY_RECORD_FIELDS`, before anything leaves the subprocess.

#' The file name of the feasibility workbook, in `plan$dir_results`.
#' @noRd
.FILENAME_FEASIBILITY_XLSX <- "feasibility.xlsx"

#' The window columns that leave the feasibility worker.
#'
#' The worker drops every other column: `events_weighted`, `rate_pooled` and
#' `events_expected`.
#' @noRd
.FEASIBILITY_WINDOW_COLS <- c(
  "window_lo",
  "window_hi",
  "arm",
  "py_weighted",
  "pass"
)

#' The record fields that leave the feasibility worker.
#' @noRd
.FEASIBILITY_RECORD_FIELDS <- c(
  "rule",
  "weight_col",
  "follow_up",
  "windows",
  "reportable_weeks",
  "withheld",
  "fail_lo",
  "fail_hi",
  "fail_arm",
  "status",
  "max_passing_cut",
  "exempt",
  "by_year"
)

#' @include r6_tteplan.R
#' @description Decide after s2 which ETTs and estimands pass the
#' minimum-events rule, and to which horizon. The method fits no outcome
#' model, and `self$feasibility` holds no event count, rate or expected events.
#'
#' @details
#' The method reads the analysis files that s2 wrote. It resolves the file,
#' the weight column, the follow-up and the rule of each estimand with the
#' same code as `$s3_analyze()`. It then calls the same window computation and
#' decision. `reportable_weeks`, `withheld` and `status` are therefore the
#' values that `$s3_analyze()` stores in `horizon_pp` and `horizon_itt`.
#'
#' The worker subprocess computes the expected events, because the decision
#' needs them. It drops `events_weighted`, `rate_pooled` and `events_expected`
#' before it returns. The method stores the result in `self$feasibility`.
#' Each window keeps `window_lo`, `window_hi`, `arm`, `py_weighted` and
#' `pass`.
#'
#' The decision depends on the observed events, through the pooled rate.
#' Neither `self$feasibility` nor `feasibility.xlsx` holds an event count, a
#' rate or expected events after time zero, or an effect estimate. The
#' record keeps `max_passing_cut`: the largest cut of a coarse grid at which
#' the estimand passes. The
#' Attrition sheet keeps the eligibility counts by arm, including people
#' excluded for an outcome event before time zero.
#'
#' The intention-to-treat record of each ETT takes the decision of its
#' per-protocol record, as in `$s3_analyze()`. Some ETTs have a follow-up
#' that is not a whole number of years. The per-protocol record of such an ETT
#' first takes the decision of the ETT at the next whole year.
#'
#' The method writes `feasibility.xlsx` to `self$dir_results`, the directory
#' of `$export_tables()`. `$export_tables()` also writes it, from the s3
#' results. The workbook holds these sheets, in this order:
#' \describe{
#'   \item{Study Specification}{The sheet of `$export_tables()`.}
#'   \item{Rare conditions}{The sheet of `$export_tables()`. It is absent when
#'     the plan holds no rare-condition decision.}
#'   \item{Minimum events (Y or -)}{One row per outcome and comparison, one
#'     column per whole follow-up year. Y: the per-protocol estimate passes the
#'     rule. -: it fails the rule, and the estimate is withheld unless the
#'     comparison is exempt. The comparison of an exempt enrollment ends with
#'     ` (exempt)`.}
#'   \item{Minimum events (#)}{The same layout. Each cell is the largest cut
#'     of 1, 2, 3, 4, 5, 10 and 20 at which the estimate passes the rule.}
#'   \item{Attrition}{The table of `$get_attrition()`.}
#'   \item{Table 1}{The unweighted baseline table of each enrollment. One
#'     panel reads the s1 raw file, before imputation. The other reads the
#'     smallest s2 analysis file of the enrollment.}
#' }
#'
#' The method stops when the spec has no
#' `min_expected_events_per_arm_per_year`. It also stops unless s1 completed
#' and s2 wrote every analysis file after that s1 run.
#'
#' @param n_workers Integer >= 1. The number of concurrent worker
#'   subprocesses. Each worker reads one analysis file. The default reads
#'   `default_n_workers("feasibility")`.
#' @param output_dir Optional directory of the analysis files. If `NULL`
#'   (default), the method uses `self$dir_tteplan`, then the legacy
#'   `self$output_dir`.
#' @param swereg_dev_path Path to a local swereg source tree for the
#'   workers, or `NULL`.
#' @return The plan, invisibly. `self$feasibility` is a list:
#'   \describe{
#'     \item{records}{A list keyed by ETT id, then by estimand (`pp`, `itt`).
#'       Each record holds `rule`, `weight_col`, `follow_up`, `windows`,
#'       `reportable_weeks`, `withheld`, `fail_lo`, `fail_hi`, `fail_arm`,
#'       `status`, `max_passing_cut`, `exempt` and `by_year`. `by_year` holds
#'       one row per follow-up year: `year`, `reportable` and
#'       `max_passing_cut`, the decision up to the end of that year. It holds
#'       no expected events. `exempt` is `TRUE` when the
#'       spec sets `minimum_events_rule_exempt: true` on the enrollment of the
#'       ETT. `withheld` then still holds the verdict of the rule, and `$s3_analyze()`
#'       will withhold none of the estimates. The `itt` record also holds
#'       `decided_by = "pp"`.}
#'     \item{s1_run_id}{The s1 run of the analysis files.}
#'     \item{swereg_version}{The swereg version that ran the method.}
#'     \item{computed_at}{When the method ran.}
#'   }
TTEPlan$set(
  "public",
  "s3_feasibility",
  function(
    n_workers = default_n_workers("feasibility"),
    output_dir = NULL,
    swereg_dev_path = NULL
  ) {
    n_workers <- .validate_n_workers(n_workers, "s3_feasibility()")
    where <- "s3_feasibility()"
    horizon_rule <- .tte_assert_horizon_rule_present(self$spec, where)
    .tte_assert_sparse_decision_current(self, where)
    .tte_assert_s1_completed(self, where)
    output_dir <- .tte_s3_output_dir(self, output_dir)
    ett <- self$ett
    .tte_assert_s2_current(self, where, ett$ett_id)
    n_threads <- .threads_per_worker(n_workers)

    # One item per ETT and estimand. The file, the weight, the follow-up and
    # the rule come from the resolver that `$s3_analyze()` calls.
    items <- list()
    item_map <- list()
    for (i in seq_len(nrow(ett))) {
      r <- .tte_s3_resolve_ett(ett, i, output_dir, horizon_rule, self$spec)
      for (est in names(r$estimands)) {
        items[[length(items) + 1L]] <- list(
          analysis_path = r$estimands[[est]]$path,
          ett_id = r$ett_id,
          n_threads = n_threads,
          weight_col = r$estimands[[est]]$weight,
          follow_up = r$follow_up,
          horizon_rule = r$rule
        )
        item_map[[length(item_map) + 1L]] <- list(
          ett_id = r$ett_id,
          estimand = est
        )
      }
    }
    names(items) <- vapply(
      item_map,
      function(m) {
        return(paste(m$ett_id, m$estimand, sep = "__"))
      },
      character(1)
    )

    # One baseline item per enrollment, as s3 builds it.
    enr_ids <- unique(ett$enrollment_id)
    enr_items <- lapply(enr_ids, function(eid) {
      return(.tte_s3_enrollment_item(self, eid, output_dir, n_threads)$item)
    })
    names(enr_items) <- enr_ids

    p <- progressr::progressor(steps = length(items) + length(enr_items))
    recs <- list()
    if (length(items) > 0L) {
      recs <- .batch_run(
        target = .batch_target("swereg", ".s3_feasibility_worker"),
        items = items,
        n_workers = n_workers,
        dev_path = swereg_dev_path,
        p = p
      )
    }
    baselines <- list()
    if (length(enr_items) > 0L) {
      baselines <- .batch_run(
        target = .batch_target("swereg", ".s3_enrollment_worker"),
        items = enr_items,
        n_workers = n_workers,
        dev_path = swereg_dev_path,
        p = p
      )
      names(baselines) <- enr_ids
    }

    records <- list()
    for (j in seq_along(item_map)) {
      m <- item_map[[j]]
      rec <- recs[[j]]
      if (!is.list(rec) || !is.logical(rec$withheld) || is.na(rec$withheld)) {
        stop(
          where,
          ": ETT ",
          m$ett_id,
          " has no decision of the minimum-events rule for the ",
          .TTE_ESTIMANDS[[m$estimand]],
          " estimand. Its worker failed. Fix the cause and rerun ",
          "$s3_feasibility().",
          call. = FALSE
        )
      }
      if (is.null(records[[m$ett_id]])) {
        records[[m$ett_id]] <- list()
      }
      records[[m$ett_id]][[m$estimand]] <- rec
    }

    # A part-year ETT takes the per-protocol decision of its source ETT, as
    # in `$s3_analyze()`. Pass 1 covers every other ETT, and pass 2 the
    # part-year ETTs.
    sources <- .tte_horizon_sources(ett)
    part <- names(records) %in% names(sources)
    for (eid in c(names(records)[!part], names(records)[part])) {
      src <- sources[[eid]]
      if (!is.null(src)) {
        records[[eid]]$pp <- .tte_horizon_from_source(
          records[[eid]]$pp,
          records[[src$ett_id]]$pp,
          eid,
          src,
          where
        )
      }
      records[[eid]]$itt <- .tte_horizon_follow_pp(
        records[[eid]]$pp,
        records[[eid]]$itt
      )
    }

    self$feasibility <- list(
      records = records,
      s1_run_id = self$s1_run_id,
      swereg_version = as.character(utils::packageVersion("swereg")),
      computed_at = Sys.time()
    )

    path <- file.path(self$dir_results, .FILENAME_FEASIBILITY_XLSX)
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    .write_feasibility_xlsx(self, baselines, path)
    cat("feasibility: wrote ", path, "\n", sep = "")
    .feasibility_report(records)
    return(invisible(self))
  }
)


#' Worker of `$s3_feasibility()`: the decision of one ETT and one estimand
#'
#' Reads one analysis file and builds the horizon record with
#' `.tte_horizon_record()`, the function the s3 risk-difference item calls.
#' It returns the record without `events_weighted`, `rate_pooled` and
#' `events_expected`.
#'
#' @param analysis_path Path to the analysis `.qs2` file of the estimand.
#' @param ett_id Character(1), the ETT id, for the item name.
#' @param n_threads Integer(1), the data.table threads.
#' @param weight_col Character(1), the weight column of the estimand.
#' @param follow_up Numeric(1), the follow-up of the ETT in weeks.
#' @param horizon_rule The rule, a list holding `min_expected_events`.
#' @return The stripped record, from `.tte_feasibility_strip()`.
#' @noRd
.s3_feasibility_worker <- function(
  analysis_path,
  ett_id,
  n_threads,
  weight_col,
  follow_up,
  horizon_rule
) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(analysis_path, nthreads = 1L)
  design <- enrollment$design
  rec <- .tte_horizon_record(
    enrollment$data,
    weight_col,
    follow_up,
    horizon_rule,
    design$treatment_var,
    design$tstart_var,
    design$tstop_var
  )
  return(.tte_feasibility_strip(rec))
}


#' Keep only the outcome-free part of a horizon record
#'
#' @param rec A record from `.tte_horizon_record()`.
#' @return `rec` with the fields of `.FEASIBILITY_RECORD_FIELDS`. Its window
#'   table holds the columns of `.FEASIBILITY_WINDOW_COLS` and no other.
#' @noRd
.tte_feasibility_strip <- function(rec) {
  rec <- rec[.FEASIBILITY_RECORD_FIELDS]
  rec$windows <- rec$windows[, .FEASIBILITY_WINDOW_COLS, with = FALSE]
  return(rec)
}


#' Write feasibility.xlsx from the s3 results
#'
#' `$export_tables()` calls it, so a normal run gives the feasibility
#' workbook beside `tables.xlsx`. The sheets and their order are those of
#' `$s3_feasibility()`, because both call `.write_feasibility_xlsx()`. The
#' Minimum events sheets read the per-protocol horizon records that s3
#' stored, feasibility-horizon ETTs included. Table 1 reads the stored
#' unimputed and imputed unweighted panels of each enrollment, through
#' `$get_baselines()`.
#'
#' @param plan A `TTEPlan` with s3 results.
#' @param path Character(1), the `.xlsx` path.
#' @param enrollment_ids Character, the enrollments of the reported ETTs, in
#'   the order of Table 1.
#' @return `path`, invisibly.
#' @noRd
.plan_export_feasibility <- function(plan, path, enrollment_ids) {
  base_all <- plan$get_baselines()
  baselines <- lapply(enrollment_ids, function(eid) {
    arms <- .baseline_arm_labels(base_all, eid)
    panel <- function(imputation) {
      return(.baseline_panel(
        base_all,
        eid,
        imputation,
        "none",
        "supplementary",
        arms
      ))
    }
    return(list(
      table1_raw = panel("raw"),
      table1_unweighted = panel("imputed")
    ))
  })
  names(baselines) <- enrollment_ids
  .write_feasibility_xlsx(
    plan,
    baselines,
    path,
    records = .acc_horizon_records(plan, "pp")
  )
  return(invisible(path))
}


#' Write the feasibility workbook
#'
#' `$s3_feasibility()` and `$export_tables()` both call it, so the two
#' workbooks have the same sheets in the same order.
#'
#' @param plan A TTEPlan.
#' @param baselines Named list by enrollment id. Each element holds the
#'   panels `table1_raw` and `table1_unweighted`, and the writer reads only
#'   those two.
#' @param path Character(1), the `.xlsx` path.
#' @param records A list keyed by ETT id: the per-protocol horizon record of
#'   each ETT. The default reads `plan$feasibility$records`.
#' @return `path`, invisibly.
#' @noRd
.write_feasibility_xlsx <- function(
  plan,
  baselines,
  path,
  records = lapply(plan$feasibility$records, function(r) r$pp)
) {
  wb <- openxlsx::createWorkbook()
  bold <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12)
  header <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#EFEFEF",
    border = "bottom"
  )

  .write_spec_summary(wb, plan)
  .write_sparse_conditions_sheet(wb, plan)
  .write_minimum_events_sheets(wb, plan, records)

  sht <- "Attrition"
  openxlsx::addWorksheet(wb, sht)
  openxlsx::writeData(wb, sht, "Attrition of the s1 eligibility cascade")
  openxlsx::addStyle(wb, sht, bold, rows = 1L, cols = 1L)
  att <- plan$get_attrition()
  if (nrow(att) == 0L) {
    openxlsx::writeData(wb, sht, "(no attrition rows)", startRow = 3L)
  } else {
    openxlsx::writeData(wb, sht, att, startRow = 3L, headerStyle = header)
  }

  sht <- "Table 1"
  openxlsx::addWorksheet(wb, sht)
  row <- 1L
  panels <- c(
    table1_raw = "unimputed, from the s1 raw file",
    table1_unweighted = "imputed, from the s2 analysis file"
  )
  for (eid in names(baselines)) {
    for (slot in names(panels)) {
      t1 <- .t1_drop_numeric(baselines[[eid]][[slot]])
      openxlsx::writeData(
        wb,
        sht,
        paste0(
          "Table 1, unweighted, ",
          panels[[slot]],
          " -- Enrollment ",
          eid,
          " (",
          .enrollment_label(plan, eid),
          ")"
        ),
        startRow = row
      )
      openxlsx::addStyle(wb, sht, bold, rows = row, cols = 1L)
      if (is.null(t1) || nrow(t1) == 0L) {
        openxlsx::writeData(wb, sht, "(no data)", startRow = row + 1L)
        row <- row + 3L
        next
      }
      openxlsx::writeData(
        wb,
        sht,
        t1,
        startRow = row + 1L,
        headerStyle = header
      )
      row <- row + nrow(t1) + 3L
    }
  }
  openxlsx::setColWidths(wb, sht, cols = 1:2, widths = c(50, 16))

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  return(invisible(path))
}


#' Name every ETT estimand that the rule would withhold, on stdout
#'
#' An estimand of an exempt enrollment is never named as withheld. When the
#' records hold one, a second line names each such estimand as exempt.
#'
#' @param records `plan$feasibility$records`.
#' @return `invisible(NULL)`.
#' @noRd
.feasibility_report <- function(records) {
  hits <- character(0)
  exempt <- character(0)
  n <- 0L
  for (eid in names(records)) {
    for (est in names(records[[eid]])) {
      n <- n + 1L
      rec <- records[[eid]][[est]]
      if (isTRUE(rec$exempt)) {
        exempt <- c(exempt, paste0(eid, " (", est, ")"))
      } else if (isTRUE(rec$withheld)) {
        hits <- c(
          hits,
          paste0(eid, " (", est, ", ", rec$reportable_weeks, " w)")
        )
      }
    }
  }
  if (length(hits) == 0L) {
    cat(sprintf(
      "feasibility: the minimum-events rule would withhold none of %d ETT estimands.\n",
      n
    ))
  } else {
    cat(sprintf(
      "feasibility: the minimum-events rule would withhold %d of %d ETT estimands (reportable weeks): %s\n",
      length(hits),
      n,
      paste(hits, collapse = ", ")
    ))
  }
  if (length(exempt) > 0L) {
    cat(sprintf(
      "feasibility: the specification exempts %d of %d ETT estimands from the minimum-events rule, so the rule would withhold none of their estimates: %s\n",
      length(exempt),
      n,
      paste(exempt, collapse = ", ")
    ))
  }
  return(invisible(NULL))
}
