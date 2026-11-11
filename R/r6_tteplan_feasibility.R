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
#' rate or expected events after time zero by arm, or an effect estimate. The
#' Coverage (whole dataset) sheet counts the persons with each outcome column
#' in the whole skeleton, before eligibility and blind to arm. The
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
#'   \item{Arm sizes}{One row per enrollment: the person-trials eligible for
#'     each arm, summed over trials, the comparator-to-intervention ratio
#'     they give, the ratio the spec asks for, and the person-trials the
#'     comparator draw took. `Flag` reads `Comparator rarer than intervention:
#'     consider reversing the arms` when fewer person-trials are eligible for
#'     the comparator than for the intervention, and `Comparator pool short
#'     of the target ratio` when the comparator pool is larger but cannot fill
#'     the ratio. The sheet holds no outcome.}
#'   \item{Coverage (whole dataset)}{One row per skeleton column that the
#'     spec reads, with its role: outcome, inclusion, exclusion, confounder,
#'     rare-condition candidate or treatment level. The persons with the
#'     column come from the skeleton summary of `$process_skeletons()`, over
#'     every person and week and blind to arm. A column with 0 persons is
#'     flagged as not in the data. A rare-condition candidate held by fewer
#'     than `min_n` persons in the whole dataset is marked as a certain
#'     exclusion.}
#'   \item{Coverage (ETT)}{Per enrollment: the persons each eligibility
#'     criterion removed, flagged when it removes nobody, and the
#'     person-trials of each confounder level per arm at baseline, flagged
#'     when an arm holds none. Per ETT: the largest cut of the minimum-events
#'     rule that the outcome passes, flagged when it passes none.}
#'   \item{Attrition}{The table of `$get_attrition()`.}
#'   \item{Protocol adherence}{One row per comparison and arm. `N (ITT)`
#'     counts the person-trials still observed and `N (PP)` those still on
#'     protocol. `%` is 100 x N (PP) / N (ITT). The sheet shows weeks 13 and
#'     26 and every multiple of 52 up to the last stored week. The counts are
#'     unweighted and come from the s1 raw file. The sheet is absent when no
#'     enrollment has an adherence table.}
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
    # The worker results hold the adherence table of each enrollment.
    adherence <- lapply(baselines, function(b) b[["adherence"]])
    .write_feasibility_xlsx(self, baselines, path, adherence)
    cat("feasibility: wrote ", path, "\n", sep = "")
    .feasibility_report(records)
    .arm_sizes_report(.tte_arm_sizes(self))
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
    adherence = .tte_adherence_by_enrollment(plan),
    records = .acc_horizon_records(plan, "pp")
  )
  return(invisible(path))
}


#' The baseline confounder counts of each enrollment
#'
#' Reads the `coverage` of each worker result in `baselines`, and otherwise
#' the one stored by s3 in `plan$results_enrollment`.
#'
#' @param plan A TTEPlan.
#' @param baselines Named list by enrollment id.
#' @return A named list by enrollment id.
#' @noRd
.tte_coverage_by_enrollment <- function(plan, baselines) {
  out <- lapply(names(baselines), function(eid) {
    return(baselines[[eid]][["coverage"]] %||%
      plan$results_enrollment[[eid]][["coverage"]])
  })
  names(out) <- names(baselines)
  return(out)
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
#' @param adherence A named list by enrollment id, each element a stored
#'   adherence table or `NULL`. `.write_adherence_sheet()` writes it after
#'   the Attrition sheet.
#' @param records A list keyed by ETT id: the per-protocol horizon record of
#'   each ETT. The default reads `plan$feasibility$records`.
#' @return `path`, invisibly.
#' @noRd
.write_feasibility_xlsx <- function(
  plan,
  baselines,
  path,
  adherence,
  records = lapply(plan$feasibility$records, function(r) r$pp),
  coverage = .tte_coverage_by_enrollment(plan, baselines)
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
  .write_arm_sizes_sheet(wb, plan, header)
  .write_coverage_sheets(wb, plan, coverage, records, header)

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

  .write_adherence_sheet(wb, plan, adherence)

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


#' The name of the arm-size sheet of the feasibility workbook.
#' @noRd
.SHEET_ARM_SIZES <- "Arm sizes"

#' The flag texts of the arm-size table.
#' @noRd
.ARM_SIZE_FLAGS <- c(
  rarer = "Comparator rarer than intervention: consider reversing the arms",
  short = "Comparator pool short of the target ratio"
)


#' The eligible and enrolled person-trials of each arm, per enrollment
#'
#' Sums the comparator-draw counts of `$get_matching()` over the trials of
#' each enrollment. The intervention arm enters in full. The draw takes
#' `round(ratio * n)` comparators per trial, or every comparator when the
#' pool is smaller, so a comparator pool smaller than the intervention arm
#' can never fill a ratio of 1 or more.
#'
#' @param plan A TTEPlan with s1 counts.
#' @return A data.table with one row per enrollment that stored counts:
#'   `enrollment_id`, `comparison`, `intervention`, `comparator`,
#'   `n_intervention_eligible`, `n_comparator_eligible`,
#'   `ratio_eligible` (comparator per intervention, `NA` when no intervention
#'   person-trial is eligible), `ratio_target`, `n_intervention_enrolled`,
#'   `n_comparator_enrolled`, `comparator_rarer`, `comparator_short` and
#'   `flag`. Zero rows when no enrollment stored counts.
#' @noRd
.tte_arm_sizes <- function(plan) {
  . <- n_intervention_total <- n_comparator_total <- NULL
  n_intervention_enrolled <- n_comparator_enrolled <- enrollment_id <- NULL
  mat <- plan$get_matching()
  empty <- data.table::data.table(
    enrollment_id = character(0),
    comparison = character(0),
    intervention = character(0),
    comparator = character(0),
    n_intervention_eligible = numeric(0),
    n_comparator_eligible = numeric(0),
    ratio_eligible = numeric(0),
    ratio_target = numeric(0),
    n_intervention_enrolled = numeric(0),
    n_comparator_enrolled = numeric(0),
    comparator_rarer = logical(0),
    comparator_short = logical(0),
    flag = character(0)
  )
  if (nrow(mat) == 0L) {
    return(empty)
  }
  agg <- mat[,
    .(
      n_intervention_eligible = sum(n_intervention_total, na.rm = TRUE),
      n_comparator_eligible = sum(n_comparator_total, na.rm = TRUE),
      n_intervention_enrolled = sum(n_intervention_enrolled, na.rm = TRUE),
      n_comparator_enrolled = sum(n_comparator_enrolled, na.rm = TRUE)
    ),
    keyby = enrollment_id
  ]
  enr_spec <- function(eid) {
    for (enr in plan$spec$enrollments) {
      if (isTRUE(enr$id == eid)) {
        return(enr)
      }
    }
    return(NULL)
  }
  rows <- lapply(seq_len(nrow(agg)), function(i) {
    eid <- agg$enrollment_id[i]
    enr <- enr_spec(eid)
    arms <- .lookup_arm_labels(plan$spec, eid)
    ratio <- enr$treatment$implementation$comparator_to_intervention_ratio
    ratio <- if (is.null(ratio)) NA_real_ else as.numeric(ratio)
    n_int <- agg$n_intervention_eligible[i]
    n_cmp <- agg$n_comparator_eligible[i]
    rarer <- n_int > 0 && n_cmp < n_int
    short <- !rarer && n_int > 0 && is.finite(ratio) && n_cmp < ratio * n_int
    flag <- if (rarer) {
      .ARM_SIZE_FLAGS[["rarer"]]
    } else if (short) {
      .ARM_SIZE_FLAGS[["short"]]
    } else {
      ""
    }
    return(data.table::data.table(
      enrollment_id = eid,
      comparison = .enrollment_label(plan, eid),
      intervention = if (is.null(arms)) NA_character_ else arms[["intervention"]],
      comparator = if (is.null(arms)) NA_character_ else arms[["comparator"]],
      n_intervention_eligible = n_int,
      n_comparator_eligible = n_cmp,
      ratio_eligible = if (n_int > 0) round(n_cmp / n_int, 2) else NA_real_,
      ratio_target = ratio,
      n_intervention_enrolled = agg$n_intervention_enrolled[i],
      n_comparator_enrolled = agg$n_comparator_enrolled[i],
      comparator_rarer = rarer,
      comparator_short = short,
      flag = flag
    ))
  })
  return(data.table::rbindlist(c(list(empty), rows), use.names = TRUE))
}


#' Write the Arm sizes sheet of the feasibility workbook
#'
#' A flagged row is filled red when the comparator is rarer than the
#' intervention, and yellow when the comparator pool is short of the target
#' ratio.
#'
#' @param wb An openxlsx workbook.
#' @param plan A TTEPlan.
#' @param header The header style of the workbook.
#' @return `invisible(NULL)`.
#' @noRd
.write_arm_sizes_sheet <- function(wb, plan, header) {
  sht <- .SHEET_ARM_SIZES
  openxlsx::addWorksheet(wb, sht)
  openxlsx::writeData(
    wb,
    sht,
    "Arm sizes: person-trials eligible for each arm, before the comparator draw"
  )
  openxlsx::addStyle(
    wb,
    sht,
    openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )
  openxlsx::writeData(
    wb,
    sht,
    paste0(
      "Summed over trials. The draw takes every eligible intervention ",
      "person-trial and up to the target ratio of comparators per trial. ",
      "When the comparator is rarer than the intervention, reversing the arms ",
      "lets the draw fill the ratio."
    ),
    startRow = 2L
  )
  tab <- .tte_arm_sizes(plan)
  if (nrow(tab) == 0L) {
    openxlsx::writeData(wb, sht, "(no comparator-draw counts)", startRow = 4L)
    return(invisible(NULL))
  }
  out <- data.table::data.table(
    Enrollment = tab$enrollment_id,
    Comparison = tab$comparison,
    Intervention = tab$intervention,
    Comparator = tab$comparator,
    `Eligible intervention person-trials` = tab$n_intervention_eligible,
    `Eligible comparator person-trials` = tab$n_comparator_eligible,
    `Comparators per intervention (eligible)` = tab$ratio_eligible,
    `Comparators per intervention (target)` = tab$ratio_target,
    `Enrolled intervention person-trials` = tab$n_intervention_enrolled,
    `Enrolled comparator person-trials` = tab$n_comparator_enrolled,
    Flag = tab$flag
  )
  openxlsx::writeData(wb, sht, out, startRow = 4L, headerStyle = header)
  fills <- list(
    rarer = openxlsx::createStyle(fgFill = "#F4B6B6"),
    short = openxlsx::createStyle(fgFill = "#FFE699")
  )
  for (k in names(fills)) {
    hit <- which(tab[[paste0("comparator_", k)]])
    if (length(hit) > 0L) {
      openxlsx::addStyle(
        wb,
        sht,
        fills[[k]],
        rows = 4L + hit,
        cols = seq_len(ncol(out)),
        gridExpand = TRUE
      )
    }
  }
  openxlsx::setColWidths(
    wb,
    sht,
    cols = seq_len(ncol(out)),
    widths = c(12, 50, 30, 30, rep(16, 6), 40)
  )
  return(invisible(NULL))
}


#' Name every enrollment whose comparator is rarer than its intervention
#'
#' @param tab The table of `.tte_arm_sizes()`.
#' @return `invisible(NULL)`.
#' @noRd
.arm_sizes_report <- function(tab) {
  rarer <- tab$enrollment_id[tab$comparator_rarer]
  short <- tab$enrollment_id[tab$comparator_short]
  if (length(rarer) > 0L) {
    cat(sprintf(
      "feasibility: the comparator is rarer than the intervention in %d of %d enrollments; consider reversing the arms: %s\n",
      length(rarer),
      nrow(tab),
      paste(rarer, collapse = ", ")
    ))
  }
  if (length(short) > 0L) {
    cat(sprintf(
      "feasibility: the comparator pool is short of the target ratio in %d of %d enrollments: %s\n",
      length(short),
      nrow(tab),
      paste(short, collapse = ", ")
    ))
  }
  return(invisible(NULL))
}


# ---- Variable coverage --------------------------------------------------------
#
# Two sheets answer "does the data hold what the spec asks for?". The first
# reads the skeleton summary that `$process_skeletons()` writes, so it covers
# the whole raw dataset and runs no new pass over the skeletons. The second
# reads what s1 to s3 stored for each enrollment and ETT: the persons each
# eligibility criterion removed, the baseline counts of each confounder per
# arm, and the minimum-events verdict of each outcome.

#' The names of the two coverage sheets.
#' @noRd
.SHEET_COVERAGE_RAW <- "Coverage (whole dataset)"

#' @noRd
.SHEET_COVERAGE_ETT <- "Coverage (ETT)"

#' The flag texts of the coverage sheets.
#' @noRd
.COVERAGE_FLAGS <- c(
  absent = "Not in the data: 0 persons",
  rare = "Fewer than 5 persons",
  arm_absent = "0 in an arm",
  arm_rare = "Fewer than 5 in an arm",
  removes_nobody = "Removes nobody",
  no_cut = "Passes no cut, not even 1 expected event per arm per year"
)

#' Show a count, with 1 to 4 written as `<5`.
#' @noRd
.coverage_count <- function(n) {
  out <- format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  out[is.na(n)] <- ""
  out[!is.na(n) & n > 0 & n < 5] <- "<5"
  return(out)
}


#' Every skeleton column the spec reads, with its role
#'
#' @param spec A spec from `tteplan_read_spec()`.
#' @return A data.table with one row per item and column: `scope` (`"All"` or
#'   an enrollment id), `role`, `name`, `variable` (the column the pipeline
#'   reads, or the derived confounder), `column` (a skeleton column that the
#'   summary counts) and `level` (a level of a character column, or `NA`).
#' @noRd
.tte_spec_columns <- function(spec) {
  rows <- list()
  add <- function(scope, role, name, impl, levels = NULL, variable = NULL) {
    cols <- unlist(impl[["source_variable"]] %||% impl[["variable"]])
    cols <- as.character(cols)
    if (length(cols) == 0L) {
      return(invisible(NULL))
    }
    variable <- variable %||%
      as.character(impl[["variable_combined"]] %||% impl[["variable"]] %||%
        impl[["source_variable_combined"]] %||% cols[1L])[1L]
    if (!is.null(impl[["value"]]) && is.null(levels)) {
      levels <- as.character(impl[["value"]])
    }
    levels <- if (length(levels) == 0L) NA_character_ else as.character(levels)
    for (col in cols) {
      for (lv in levels) {
        rows[[length(rows) + 1L]] <<- data.table::data.table(
          scope = scope,
          role = role,
          name = as.character(name %||% variable),
          variable = variable,
          column = col,
          level = lv
        )
      }
    }
    return(invisible(NULL))
  }
  for (o in spec$outcomes) {
    add("All", "Outcome", o$name, o$implementation)
  }
  for (cr in spec$inclusion_criteria$criteria) {
    add("All", "Inclusion", cr$name, cr$implementation)
  }
  for (cr in spec$exclusion_criteria) {
    add("All", "Exclusion", cr$name, cr$implementation)
  }
  cand <- as.character(unlist(spec$rare_condition_rule$candidates))
  for (cf in spec$confounders) {
    v <- as.character(cf$implementation[["variable"]] %||% NA_character_)[1L]
    role <- if (!is.na(v) && v %in% cand) {
      "Confounder (rare-condition candidate)"
    } else {
      "Confounder"
    }
    add("All", role, cf$name, cf$implementation, cf$categories, variable = v)
  }
  for (enr in spec$enrollments) {
    for (cr in enr$additional_inclusion) {
      add(enr$id, "Inclusion", cr$name, cr$implementation)
    }
    for (cr in enr$additional_exclusion) {
      add(enr$id, "Exclusion", cr$name, cr$implementation)
    }
    tx <- enr$treatment$implementation
    if (!is.null(tx[["variable"]])) {
      vals <- c(tx[["intervention_value"]], tx[["comparator_value"]])
      if (is.logical(vals)) {
        vals <- NULL
      }
      add(
        enr$id,
        "Treatment",
        paste(enr$treatment$arms$intervention, "vs", enr$treatment$arms$comparator),
        list(variable = tx[["variable"]]),
        vals
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.table::data.table(
      scope = character(0),
      role = character(0),
      name = character(0),
      variable = character(0),
      column = character(0),
      level = character(0)
    ))
  }
  return(unique(data.table::rbindlist(rows)))
}


#' The whole-dataset coverage of every column the spec reads
#'
#' @param plan A TTEPlan.
#' @param summary_columns The `columns` table of the skeleton summary, or
#'   `NULL`. The default reads `plan$registrystudy$summary`.
#' @return A list: `table`, one row per row of `.tte_spec_columns()` with
#'   `n_persons` (`NA` when the summary does not count the column), `flag`
#'   and `rare_rule`; and `built_at`, the time the summary was built, or `NA`.
#' @noRd
.tte_coverage_raw <- function(plan, summary_columns = NULL) {
  n_persons <- flag <- rare_rule <- NULL
  built_at <- NA
  if (is.null(summary_columns)) {
    s <- tryCatch(
      suppressMessages(plan$registrystudy$summary),
      error = function(e) NULL
    )
    summary_columns <- s$columns
    built_at <- s$meta$built_at %||% NA
  }
  tab <- .tte_spec_columns(plan$spec)
  if (is.null(summary_columns) || nrow(tab) == 0L) {
    return(list(table = NULL, built_at = built_at))
  }
  counts <- stats::setNames(
    as.numeric(summary_columns$n_persons_with),
    as.character(summary_columns$column_name)
  )
  key <- ifelse(is.na(tab$level), tab$column, paste0(tab$column, "=", tab$level))
  n <- unname(counts[key])
  # A level the summary does not hold, of a column it counts by level, is a
  # level with no person.
  by_level <- unique(sub("=.*$", "", grep("=", names(counts), value = TRUE)))
  n[is.na(n) & !is.na(tab$level) & tab$column %in% by_level] <- 0
  tab[, n_persons := n]
  tab[,
    flag := data.table::fifelse(
      is.na(n_persons),
      "",
      data.table::fifelse(
        n_persons == 0,
        .COVERAGE_FLAGS[["absent"]],
        data.table::fifelse(n_persons < 5, .COVERAGE_FLAGS[["rare"]], "")
      )
    )
  ]
  # The rare-condition rule turns a candidate into an exclusion when its
  # smallest arm holds fewer than min_n persons. A candidate held by fewer
  # than min_n persons in the whole dataset always becomes one.
  min_n <- plan$spec$rare_condition_rule$min_n
  dec <- plan$sparse_decision
  tab[, rare_rule := ""]
  if (!is.null(min_n)) {
    for (v in unique(tab$variable[grepl("rare-condition", tab$role)])) {
      i <- which(tab$variable == v & grepl("rare-condition", tab$role))
      tot <- sum(tab$n_persons[i], na.rm = TRUE)
      txt <- if (all(!is.na(tab$n_persons[i])) && tot == 0) {
        "Becomes an exclusion that removes nobody"
      } else if (all(!is.na(tab$n_persons[i])) && tot < min_n) {
        paste0("Certain exclusion: fewer than ", min_n, " persons in the whole dataset")
      } else {
        "Decided at s1b from the eligible population"
      }
      if (data.table::is.data.table(dec) && v %in% dec$variable) {
        txt <- paste0(txt, "; s1b decision: ", dec$decision[match(v, dec$variable)])
      }
      tab[i, rare_rule := txt]
    }
  }
  return(list(table = tab, built_at = built_at))
}


#' Baseline counts of each confounder per arm, from one enrollment
#'
#' Reads the baseline rows of an enrollment object. A logical confounder
#' counts its `TRUE` rows, a character or factor confounder each level, and a
#' numeric one its non-missing rows. Every count is of person-trials.
#'
#' @param enrollment A TTEEnrollment, from the raw s1 file.
#' @return A data.table with `variable`, `level`, `n_intervention` and
#'   `n_comparator`, or `NULL` when the object holds no design.
#' @noRd
.tte_confounder_coverage <- function(enrollment) {
  des <- enrollment$design
  d <- enrollment$data
  if (is.null(des) || is.null(d)) {
    return(NULL)
  }
  base <- d[d[[des$tstart_var]] == 0]
  tx <- base[[des$treatment_var]] == TRUE
  tx[is.na(tx)] <- FALSE
  rows <- list()
  for (v in intersect(des$confounder_vars, names(base))) {
    x <- base[[v]]
    if (is.logical(x)) {
      hit <- x %in% TRUE
      rows[[length(rows) + 1L]] <- data.table::data.table(
        variable = v,
        level = "TRUE",
        n_intervention = sum(hit & tx),
        n_comparator = sum(hit & !tx)
      )
    } else if (is.character(x) || is.factor(x)) {
      x <- as.character(x)
      for (lv in sort(unique(stats::na.omit(x)))) {
        hit <- x %in% lv
        rows[[length(rows) + 1L]] <- data.table::data.table(
          variable = v,
          level = lv,
          n_intervention = sum(hit & tx),
          n_comparator = sum(hit & !tx)
        )
      }
    } else {
      hit <- !is.na(x)
      rows[[length(rows) + 1L]] <- data.table::data.table(
        variable = v,
        level = "(non-missing)",
        n_intervention = sum(hit & tx),
        n_comparator = sum(hit & !tx)
      )
    }
  }
  if (length(rows) == 0L) {
    return(NULL)
  }
  return(data.table::rbindlist(rows))
}


#' The coverage of each enrollment and ETT
#'
#' @param plan A TTEPlan.
#' @param coverage A named list by enrollment id of the tables of
#'   `.tte_confounder_coverage()`.
#' @param records A list keyed by ETT id: the per-protocol horizon records.
#' @return A data.table with `enrollment_id`, `ett_id`, `section`, `name`,
#'   `variable`, `level`, `intervention`, `comparator`, `persons`, `flag`
#'   and `note`, all character.
#' @noRd
.tte_coverage_ett <- function(plan, coverage, records) {
  enrollment_period_id <- enrollment_id <- step_order <- section <- NULL
  rows <- list()
  row <- function(...) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(...)
    return(invisible(NULL))
  }
  # Eligibility: the persons each criterion removed, after the criteria
  # above it. The global rows count persons across trials.
  att <- plan$get_attrition()
  if (nrow(att) > 0L) {
    glob <- att[is.na(enrollment_period_id)]
    for (eid in unique(glob$enrollment_id)) {
      g <- glob[enrollment_id == eid][order(step_order)]
      if (nrow(g) < 2L) {
        next
      }
      for (k in 2:nrow(g)) {
        removed <- g$n_persons[k - 1L] - g$n_persons[k]
        row(
          enrollment_id = eid,
          ett_id = "",
          section = "Eligibility",
          name = g$step_name[k],
          variable = "",
          level = "",
          intervention = .coverage_count(g$n_arm_intervention[k - 1L] - g$n_arm_intervention[k]),
          comparator = .coverage_count(g$n_arm_comparator[k - 1L] - g$n_arm_comparator[k]),
          persons = .coverage_count(removed),
          flag = if (isTRUE(removed == 0)) .COVERAGE_FLAGS[["removes_nobody"]] else "",
          note = "Removed by this criterion, after the criteria above it"
        )
      }
    }
  }
  # Confounders at baseline, per arm. A declared category that no baseline
  # row holds counts 0.
  conf_name <- list()
  conf_cats <- list()
  for (cf in plan$spec$confounders) {
    v <- as.character(cf$implementation[["variable"]] %||% NA_character_)[1L]
    if (!is.na(v)) {
      conf_name[[v]] <- cf$name
      conf_cats[[v]] <- as.character(unlist(cf$categories))
    }
  }
  dec <- plan$sparse_decision
  for (eid in names(coverage)) {
    cov <- coverage[[eid]]
    if (!data.table::is.data.table(cov) || nrow(cov) == 0L) {
      next
    }
    for (v in unique(cov$variable)) {
      cats <- conf_cats[[v]]
      if (length(cats) > 0L) {
        miss <- setdiff(cats, cov$level[cov$variable == v])
        if (length(miss) > 0L) {
          cov <- data.table::rbindlist(list(
            cov,
            data.table::data.table(
              variable = v,
              level = miss,
              n_intervention = 0,
              n_comparator = 0
            )
          ))
        }
      }
    }
    for (i in seq_len(nrow(cov))) {
      v <- cov$variable[i]
      ni <- cov$n_intervention[i]
      nc <- cov$n_comparator[i]
      excluded <- data.table::is.data.table(dec) &&
        v %in% dec$variable &&
        identical(dec$decision[match(v, dec$variable)], "excluded")
      flag <- if (excluded || cov$level[i] == "(non-missing)") {
        ""
      } else if (min(ni, nc) == 0) {
        .COVERAGE_FLAGS[["arm_absent"]]
      } else if (min(ni, nc) < 5) {
        .COVERAGE_FLAGS[["arm_rare"]]
      } else {
        ""
      }
      row(
        enrollment_id = eid,
        ett_id = "",
        section = "Confounder at baseline",
        name = as.character(conf_name[[v]] %||% v),
        variable = v,
        level = cov$level[i],
        intervention = .coverage_count(ni),
        comparator = .coverage_count(nc),
        persons = "",
        flag = flag,
        note = if (excluded) {
          "The rare-condition rule made it an exclusion, so every baseline row is FALSE"
        } else {
          "Person-trials at baseline, before imputation"
        }
      )
    }
  }
  # Outcomes: the minimum-events verdict at the horizon of each ETT.
  ett <- plan$ett
  for (eid_ett in names(records)) {
    rec <- records[[eid_ett]]
    j <- match(eid_ett, ett$ett_id)
    if (is.na(j) || !is.list(rec)) {
      next
    }
    cut <- rec$max_passing_cut
    row(
      enrollment_id = as.character(ett$enrollment_id[j]),
      ett_id = eid_ett,
      section = "Outcome",
      name = as.character(ett$outcome_name[j] %||% ett$outcome_var[j]),
      variable = as.character(ett$outcome_var[j]),
      level = paste0(ett$follow_up[j], " weeks"),
      intervention = "",
      comparator = "",
      persons = "",
      flag = if (length(cut) == 1L && is.na(cut)) .COVERAGE_FLAGS[["no_cut"]] else "",
      note = paste0(
        "Largest passing cut: ",
        if (length(cut) == 1L && !is.na(cut)) format(cut) else "none",
        "; ",
        rec$status %||% ""
      )
    )
  }
  if (length(rows) == 0L) {
    return(NULL)
  }
  out <- data.table::rbindlist(rows, use.names = TRUE)
  section_order <- c("Eligibility", "Confounder at baseline", "Outcome")
  out <- out[order(enrollment_id, match(section, section_order))]
  return(out)
}


#' Fill the rows of a sheet that carry a red or a yellow flag
#' @noRd
.coverage_fill <- function(wb, sht, flags, start_row, n_col) {
  red <- c(
    .COVERAGE_FLAGS[["absent"]],
    .COVERAGE_FLAGS[["arm_absent"]],
    .COVERAGE_FLAGS[["removes_nobody"]],
    .COVERAGE_FLAGS[["no_cut"]]
  )
  fills <- list(
    red = list(hit = which(flags %in% red), fill = "#F4B6B6"),
    yellow = list(hit = which(nzchar(flags) & !flags %in% red), fill = "#FFE699")
  )
  for (f in fills) {
    if (length(f$hit) > 0L) {
      openxlsx::addStyle(
        wb,
        sht,
        openxlsx::createStyle(fgFill = f$fill),
        rows = start_row + f$hit,
        cols = seq_len(n_col),
        gridExpand = TRUE
      )
    }
  }
  return(invisible(NULL))
}


#' Write the two coverage sheets of the feasibility workbook
#'
#' @param wb An openxlsx workbook.
#' @param plan A TTEPlan.
#' @param coverage A named list by enrollment id of the tables of
#'   `.tte_confounder_coverage()`.
#' @param records A list keyed by ETT id: the per-protocol horizon records.
#' @param header The header style of the workbook.
#' @return `invisible(NULL)`.
#' @noRd
.write_coverage_sheets <- function(wb, plan, coverage, records, header) {
  bold <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12)

  sht <- .SHEET_COVERAGE_RAW
  openxlsx::addWorksheet(wb, sht)
  openxlsx::writeData(
    wb,
    sht,
    "Coverage in the whole dataset: persons with each skeleton column the specification reads"
  )
  openxlsx::addStyle(wb, sht, bold, rows = 1L, cols = 1L)
  raw <- .tte_coverage_raw(plan)
  openxlsx::writeData(
    wb,
    sht,
    paste0(
      "From the skeleton summary",
      if (!is.na(raw$built_at)) paste0(" built ", format(raw$built_at, "%Y-%m-%d %H:%M")) else "",
      ". Every person in the skeleton, over every week, before any ",
      "eligibility, and blind to arm. A column with 0 persons is not in the ",
      "data delivery or is coded wrongly. An empty count is a column the ",
      "summary does not count, such as a numeric column. 1 to 4 show as <5."
    ),
    startRow = 2L
  )
  tab <- raw$table
  if (is.null(tab)) {
    openxlsx::writeData(wb, sht, "(no skeleton summary found)", startRow = 4L)
  } else {
    out <- data.table::data.table(
      Scope = tab$scope,
      Role = tab$role,
      Name = tab$name,
      `Skeleton column` = tab$column,
      Level = data.table::fifelse(is.na(tab$level), "", tab$level),
      Persons = .coverage_count(tab$n_persons),
      Flag = tab$flag,
      `Rare-condition rule` = tab$rare_rule
    )
    openxlsx::writeData(wb, sht, out, startRow = 4L, headerStyle = header)
    .coverage_fill(wb, sht, tab$flag, 4L, ncol(out))
    openxlsx::setColWidths(
      wb,
      sht,
      cols = seq_len(ncol(out)),
      widths = c(10, 24, 60, 40, 24, 12, 30, 50)
    )
  }

  sht <- .SHEET_COVERAGE_ETT
  openxlsx::addWorksheet(wb, sht)
  openxlsx::writeData(
    wb,
    sht,
    "Coverage in each enrollment and ETT"
  )
  openxlsx::addStyle(wb, sht, bold, rows = 1L, cols = 1L)
  openxlsx::writeData(
    wb,
    sht,
    paste0(
      "Eligibility: persons each criterion removed, after the criteria above ",
      "it, with the person-trials per arm. Confounder at baseline: ",
      "person-trials per arm at time zero, from the s1 raw file. Outcome: ",
      "the largest cut of the minimum-events rule that the per-protocol ",
      "estimate passes at the horizon of the ETT. 1 to 4 show as <5."
    ),
    startRow = 2L
  )
  ett_tab <- .tte_coverage_ett(plan, coverage, records)
  if (is.null(ett_tab)) {
    openxlsx::writeData(wb, sht, "(no stored counts)", startRow = 4L)
  } else {
    out <- data.table::data.table(
      Enrollment = ett_tab$enrollment_id,
      ETT = ett_tab$ett_id,
      Section = ett_tab$section,
      Name = ett_tab$name,
      Variable = ett_tab$variable,
      `Level or follow-up` = ett_tab$level,
      Intervention = ett_tab$intervention,
      Comparator = ett_tab$comparator,
      `Persons removed` = ett_tab$persons,
      Flag = ett_tab$flag,
      Note = ett_tab$note
    )
    openxlsx::writeData(wb, sht, out, startRow = 4L, headerStyle = header)
    .coverage_fill(wb, sht, ett_tab$flag, 4L, ncol(out))
    openxlsx::setColWidths(
      wb,
      sht,
      cols = seq_len(ncol(out)),
      widths = c(12, 12, 22, 60, 36, 18, 14, 14, 14, 30, 60)
    )
  }
  return(invisible(NULL))
}
