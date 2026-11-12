# Output production for a TTEPlan: the results workbook, the standalone
# specification workbook and the ordered exhibit set.
#
# Each function here holds the whole body of the method that carries its
# name, guards included. The method in `R/r6_tteplan_export.R` or
# `R/r6_tteplan_spec.R` is a one-call delegate to it.
#
# `.plan_export()` dispatches each exhibit to `.plan_export_figure()` or
# `.plan_export_table()` by name. Both are in `R/tteplan_export_figure.R`.

#' Write the results workbook
#'
#' The body of `TTEPlan$export_tables()`.
#'
#' @param plan A `TTEPlan`.
#' @param path Character(1) or NULL, the output `.xlsx` path.
#' @param table1_enrollment Character(1) or NULL, the Table 1 enrollment.
#' @param protocol_ett_id Character(1) or NULL, the protocol sheet ETT.
#' @param output_dir Character(1) or NULL, the cached `.qs2` directory.
#' @param power Numeric(1), the power of the minimum detectable effect on the
#'   `PP results` and `ITT results` sheets.
#' @return The written path, invisibly.
#' @noRd
.plan_export_tables <- function(
  plan,
  path = NULL,
  table1_enrollment = NULL,
  protocol_ett_id = NULL,
  output_dir = NULL,
  power = 0.8
) {
  # Checked first. A plan without stored rates never calls `.tte_mde()`.
  .tte_mde_check_level(power, "power")
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package 'openxlsx' is required. Install with: install.packages('openxlsx')",
      call. = FALSE
    )
  }
  .tte_assert_one_s1_run(plan, "export_tables()")
  .tte_assert_one_s2_run(plan, "export_tables()")
  .tte_assert_horizon_rule_current(plan, "export_tables()")
  if (
    is.null(plan$results_enrollment) ||
      length(plan$results_enrollment) == 0L
  ) {
    stop("No enrollment results. Run $s3_analyze() first.", call. = FALSE)
  }
  if (is.null(plan$results_ett) || length(plan$results_ett) == 0L) {
    stop("No ETT results. Run $s3_analyze() first.", call. = FALSE)
  }
  # An unknown id matches no baseline row. The workbook would then omit
  # Table 1, with no error. Checked before anything
  # touches disk.
  if (!is.null(table1_enrollment)) {
    eids_known <- .plan_analysed_enrollment_ids(plan)
    if (
      length(table1_enrollment) != 1L ||
        !as.character(table1_enrollment) %in% eids_known
    ) {
      stop(
        "`table1_enrollment` must be one analysed enrollment id. Got: ",
        paste(format(table1_enrollment), collapse = ", "),
        ". Analysed enrollments: ",
        paste(eids_known, collapse = ", "),
        call. = FALSE
      )
    }
    table1_enrollment <- as.character(table1_enrollment)
  }
  .plan_warn_computing_versions(plan)
  if (is.null(path)) {
    path <- plan$tables_xlsx
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  }

  # Lazy refresh of stale baseline results (pre-swereg_table1 cache, and
  # pre-smd_numeric cache -- see .baseline_panel_is_stale()).
  stale <- vapply(
    plan$results_enrollment,
    .baseline_panel_is_stale,
    logical(1)
  )
  if (any(stale)) {
    message("Refreshing ", sum(stale), " stale baseline table(s) from disk...")
  }
  # A result without the name `adherence` is refreshed too, when its raw file
  # is on disk. See `.tte_adherence_refresh_wanted()`.
  refresh <- stale | .tte_adherence_refresh_wanted(plan, output_dir)
  if (any(refresh)) {
    plan$recompute_baselines(
      output_dir = output_dir,
      enrollment_ids = names(refresh)[refresh]
    )
  }

  # The reported ETTs. Only the Minimum events sheets read the
  # feasibility-horizon ETTs, through `.acc_horizon_records()`.
  ett <- .plan_ett_reported(plan)
  enrollment_ids <- unique(ett$enrollment_id)

  protocol_ett_id <- .plan_protocol_ett_id(plan, protocol_ett_id)

  # Determine table1 enrollment. `$get_baselines()` repeats the baseline
  # size on every row of that enrollment, and it returns a counts-only row
  # for an enrollment that stored no panel, so every analysed enrollment is
  # represented. An enrollment with no stored size counts as 0, which is
  # what the raw read did.
  if (is.null(table1_enrollment)) {
    base_all <- plan$get_baselines()
    # Only an enrollment with a stored IPW-truncated panel can fill Table 1.
    eids_analysed <- intersect(
      .plan_analysed_enrollment_ids(plan),
      unique(base_all$enrollment_id[which(
        base_all$imputation == "imputed" & base_all$weighting == "ipw_trunc"
      )])
    )
    if (length(eids_analysed) == 0L) {
      stop(
        "No enrollment has a stored IPW-truncated baseline panel, so ",
        "Table 1 cannot be drawn. Re-run s3 ($s3_analyze()).",
        call. = FALSE
      )
    }
    n_baselines <- vapply(
      eids_analysed,
      function(eid) {
        n <- .baseline_count(base_all, eid, "n_baseline")
        if (is.na(n)) return(0) else return(n)
      },
      numeric(1)
    )
    names(n_baselines) <- eids_analysed
    table1_enrollment <- names(which.max(n_baselines))
  }

  wb <- openxlsx::createWorkbook()
  toc_names <- character()
  toc_desc <- character()

  # --- Provenance sheet ---
  .write_provenance(wb, plan)
  toc_names <- c(toc_names, "Provenance")
  toc_desc <- c(toc_desc, "Pipeline metadata and table of contents")

  # --- Summary sheet ---
  .write_summary_sheet(wb, plan)
  toc_names <- c(toc_names, "Summary")
  toc_desc <- c(
    toc_desc,
    "One row per ETT - ITT and PP (truncated weights) estimates"
  )

  # --- The feasibility sheets: the same writer, sheets and order as
  # feasibility.xlsx ---
  feas_baselines <- .tte_feasibility_baselines(plan, enrollment_ids)
  feas <- .write_feasibility_sheets(
    wb,
    plan,
    baselines = feas_baselines,
    adherence = .tte_adherence_by_enrollment(plan),
    records = .acc_horizon_records(plan, "pp"),
    coverage = .tte_coverage_by_enrollment(plan, feas_baselines)
  )
  toc_names <- c(toc_names, names(feas))
  toc_desc <- c(toc_desc, unname(feas))

  # --- Target trial protocol sheet ---
  # One sheet documents ONE ETT, so the caller names it through
  # `protocol_ett_id`. Without it, prefer any ETT of the Table 1
  # enrollment, then the first in the grid.
  if (is.null(protocol_ett_id)) {
    t1_rows <- which(ett$enrollment_id == table1_enrollment)
    protocol_ett_id <- if (length(t1_rows) > 0L) {
      ett$ett_id[t1_rows[1]]
    } else {
      ett$ett_id[1]
    }
  }
  .write_protocol_table(
    wb,
    "Target trial protocol",
    plan,
    protocol_ett_id
  )
  toc_names <- c(toc_names, "Target trial protocol")
  toc_desc <- c(
    toc_desc,
    paste0(
      "Target trial specification vs emulation (Dickerman Table S1) -- ",
      protocol_ett_id
    )
  )

  # --- Enrollments overview sheet ---
  .write_enrollment_overview(wb, plan)
  toc_names <- c(toc_names, "Enrollments")
  toc_desc <- c(
    toc_desc,
    "Enrollment overview (treatment, comparator draw, criteria)"
  )

  # --- ETTs overview sheet ---
  .write_ett_overview(wb, plan)
  toc_names <- c(toc_names, "ETTs")
  toc_desc <- c(toc_desc, "ETT overview (outcome, follow-up, events)")

  # --- Table 1: Baseline for chosen enrollment ---
  t1_label <- .enrollment_label(plan, table1_enrollment)
  # Table 1 drops the conditions the rare-condition rule excluded, and its
  # footnote names them.
  t1_baselines <- .tte_sparse_drop_baseline_rows(plan$get_baselines(), plan)
  t1_arms <- .baseline_arm_labels(t1_baselines, table1_enrollment)
  t1_panel <- function(weighting, variant) {
    return(.baseline_panel(
      t1_baselines,
      table1_enrollment,
      "imputed",
      weighting,
      variant,
      t1_arms
    ))
  }
  t1_main <- t1_panel("ipw_trunc", "main") %||%
    t1_panel("ipw_trunc", "supplementary")
  # Without a stored panel the workbook would omit Table 1 with no error.
  if (is.null(t1_main)) {
    stop(
      "Enrollment ",
      table1_enrollment,
      " has no stored IPW-truncated baseline panel, so Table 1 cannot be ",
      "drawn. Re-run s3 ($s3_analyze()), or name another enrollment in ",
      "`table1_enrollment`.",
      call. = FALSE
    )
  }
  .write_tableone_sheet(
    wb,
    "Table 1",
    t1_main,
    title = paste0(
      "Table 1: Baseline characteristics (IPW-weighted, truncated) -- Enrollment ",
      table1_enrollment,
      " (",
      t1_label,
      ")"
    ),
    footnote = .tte_sparse_table1_footnote(plan)
  )
  toc_names <- c(toc_names, "Table 1")
  toc_desc <- c(
    toc_desc,
    paste0(
      "Baseline characteristics (IPW truncated) -- ",
      t1_label
    )
  )

  # Resolve the directory for image sidecars (next to the workbook)
  img_dir <- dirname(path)
  img_basename_root <- tools::file_path_sans_ext(basename(path))

  # --- PP results sheet (per-protocol, truncated weights, all ETTs) ---
  .write_results_single(
    wb,
    "PP results",
    plan,
    rates_slot = "rates_pp_trunc",
    irr_slot = "irr_pp_trunc",
    rd_slot = "rd_pp_trunc",
    title = "Per-protocol results (truncated weights) - all ETTs",
    power = power
  )
  toc_names <- c(toc_names, "PP results")
  toc_desc <- c(
    toc_desc,
    "All ETTs - per-protocol rates and IRRs (truncated weights)"
  )

  # --- ITT results sheet (intention-to-treat, all ETTs) ---
  .write_results_single(
    wb,
    "ITT results",
    plan,
    rates_slot = "rates_itt",
    irr_slot = "irr_itt",
    rd_slot = "rd_itt",
    title = "Intention-to-treat results - all ETTs",
    power = power
  )
  toc_names <- c(toc_names, "ITT results")
  toc_desc <- c(
    toc_desc,
    "All ETTs - intention-to-treat rates and IRRs"
  )

  # --- Weight-truncation robustness (supplementary, all ETTs) ---
  # Per-protocol truncated vs untruncated IPW/IPCW weights, side by side.
  # Moved out of the main sequence: the headline sheets are now per
  # estimand; this stays as a robustness check.
  .write_combined_sensitivity(
    wb,
    "Weight truncation (PP)",
    plan,
    trunc_rates_slot = "rates_pp_trunc",
    trunc_irr_slot = "irr_pp_trunc",
    untrunc_rates_slot = "rates_pp",
    untrunc_irr_slot = "irr_pp",
    trunc_rd_slot = "rd_pp_trunc",
    untrunc_rd_slot = "rd_pp",
    title = paste0(
      "Weight-truncation robustness (per-protocol): truncated (left) vs ",
      "untruncated (right) weights - all ETTs"
    )
  )
  toc_names <- c(toc_names, "Weight truncation (PP)")
  toc_desc <- c(
    toc_desc,
    "Supplementary - PP IRRs, RDs and NNTs, truncated vs untruncated weights"
  )

  # --- Effect modification sheet (only if any subgroups are configured) ---
  has_subgroups <- "subgroup_vars" %in%
    names(ett) &&
    any(vapply(
      ett$subgroup_vars,
      function(x) length(x) > 0L,
      logical(1)
    ))
  if (has_subgroups) {
    .write_effect_modification(
      wb,
      "Effect modification",
      plan,
      title = paste0(
        "Effect modification: stratified IRRs (per-protocol | ",
        "intention-to-treat) and interaction test"
      )
    )
    toc_names <- c(toc_names, "Effect modification")
    toc_desc <- c(
      toc_desc,
      "Stratified IRRs by subgroup (PP and ITT) + interaction test"
    )
  }

  # --- Table S1: what the follow-up confounder fill supplied ---
  # One row per enrollment and confounder. THE SHEET IS ALWAYS WRITTEN, and it
  # holds one line in place of the table when no enrollment carries a summary.
  # That is a plan from an s1 run before the fill existed. An omitted sheet
  # would renumber every supplementary table after it. A reader could then not
  # cite "Table S4" across two plans and mean the same thing.
  fill_summary <- .tteplan_fill_summary(plan)
  fill_sheet <- "Table S1 Missing data"
  openxlsx::addWorksheet(wb, fill_sheet)
  openxlsx::writeData(
    wb,
    fill_sheet,
    paste0(
      "Follow-up values carry forward from the last observed value in the ",
      "person-trial. An entry value with no observation is imputed by the ",
      "plan's impute_fn, and the default is a single hot-deck draw."
    ),
    startRow = 1L
  )
  if (nrow(fill_summary) > 0L) {
    openxlsx::writeData(wb, fill_sheet, fill_summary, startRow = 3L)
  } else {
    openxlsx::writeData(
      wb,
      fill_sheet,
      "No follow-up confounder value was filled.",
      startRow = 3L
    )
  }
  toc_names <- c(toc_names, fill_sheet)
  toc_desc <- c(
    toc_desc,
    "Supplementary - follow-up confounder values the fill supplied"
  )

  # --- Table S2-S(N+1): Combined baselines per enrollment ---
  # `j + 1L` because the missing-data sheet holds S1.
  for (j in seq_along(enrollment_ids)) {
    eid <- enrollment_ids[j]
    sheet_name <- paste0("Table S", j + 1L)
    .write_combined_baseline(wb, sheet_name, plan, eid)
    toc_names <- c(toc_names, sheet_name)
    label <- .enrollment_label(plan, eid)
    toc_desc <- c(
      toc_desc,
      paste0(
        "Enrollment ",
        eid,
        " (",
        label,
        ") -- combined baselines (Unimputed/Imputed/IPW/IPW trunc)"
      )
    )
  }

  # --- CONSORT attrition sheets + sidecar images ---
  # Attrition sheet: tabular form of the per-enrollment CONSORT numbers
  # (criterion x {n_persons, n_person_trials, excluded_*, n_intervention,
  # n_comparator}), so reviewers can cite exact counts instead of reading
  # them off a PNG.
  # CONSORT sidecars: each enrollment still gets a standalone PNG + PDF
  # rendered next to the workbook; Provenance TOC records which were
  # written.
  #
  # ONE condition gates the sheet and its table-of-contents row, and it is
  # the return value of `.write_attrition_sheet()`. A stored attrition
  # table is not enough. The writer also needs a cohort flow, and
  # `.build_cohort_flow()` returns NULL when one criterion carries no
  # global row. A row here that named the sheet on the table alone would
  # advertise a sheet the workbook does not hold.
  #
  # Love plots: one per enrollment, on a `Love_<eid>` sheet after its
  # attrition sheet, with PNG + PDF sidecars. Series: unweighted vs
  # IPW-truncated. The truncated weights are the analysis weights, so the
  # untruncated panel is not plotted. An enrollment with no numeric SMDs
  # gets no sheet, and the sidecar row of the table of contents names it.
  consort_files <- character()
  love_files <- character()
  love_skipped <- character()
  # The results directory MUST hold only the Love plots this workbook names.
  # Before 26.13.1 one `<root>_love_plot` pair stood for the Table 1
  # enrollment, and a later re-export would otherwise leave it beside the
  # per-enrollment files.
  unlink(file.path(
    img_dir,
    paste0(img_basename_root, "_love_plot.", c("png", "pdf"))
  ))
  baselines <- plan$get_baselines()
  # `which()` runs OUTSIDE the data.table subset. Inside `baselines[...]`
  # the arguments would resolve to the COLUMNS of the same name, and the
  # filter would keep every panel. The SUPPLEMENTARY panels are named by
  # three accessor keys rather than by a slot name: `table1_ipw_trunc` is a
  # strict prefix of `table1_ipw_trunc_main`, so a slot name could
  # partial-match the main panel.
  love_rows <- function(eid, want_weighting) {
    hit <- which(
      baselines$enrollment_id == eid &
        baselines$imputation == "imputed" &
        baselines$weighting == want_weighting &
        baselines$variant == "supplementary"
    )
    return(baselines[hit])
  }
  {
    for (eid in enrollment_ids) {
      ec <- .plan_cohort_counts(plan, eid)
      label <- .enrollment_label(plan, eid)
      if (!is.null(ec$attrition)) {
        attrition_sheet <- paste0("Attrition_", eid)
        if (isTRUE(.write_attrition_sheet(wb, attrition_sheet, plan, eid))) {
          toc_names <- c(toc_names, attrition_sheet)
          toc_desc <- c(
            toc_desc,
            paste0(
              "Enrollment ",
              eid,
              " (",
              label,
              ") -- CONSORT attrition (numbers behind the diagram)"
            )
          )
        }

        consort_basename <- paste0(img_basename_root, "_consort_", eid)
        paths <- .render_consort_sidecars(
          plan = plan,
          ec = ec,
          eid = eid,
          label = label,
          output_dir = img_dir,
          img_basename = consort_basename
        )
        if (!is.null(paths)) {
          consort_files <- c(consort_files, basename(paths$png))
        }
      }

      love_sheet <- paste0("Love_", eid)
      paths <- .write_love_plot(
        wb,
        love_sheet,
        t1_unweighted = love_rows(eid, "none"),
        t1_weighted = love_rows(eid, "ipw_trunc"),
        title = sprintf(
          "Love plot: covariate balance before and after weighting -- Enrollment %s (%s)",
          eid,
          label
        ),
        img_dir = img_dir,
        img_basename = paste0(img_basename_root, "_love_plot_", eid),
        labels = .tte_confounder_labels(plan$spec)
      )
      if (is.null(paths)) {
        # A sidecar from an earlier export MUST NOT outlive the skip.
        unlink(file.path(
          img_dir,
          paste0(img_basename_root, "_love_plot_", eid, c(".png", ".pdf"))
        ))
        love_skipped <- c(love_skipped, eid)
      } else {
        love_files <- c(love_files, basename(paths$png))
        toc_names <- c(toc_names, love_sheet)
        toc_desc <- c(
          toc_desc,
          paste0(
            "Enrollment ",
            eid,
            " (",
            label,
            ") -- covariate balance (absolute SMD, unweighted vs IPW truncated)"
          )
        )
      }
    }
  }
  if (length(consort_files) > 0L) {
    toc_names <- c(toc_names, "CONSORT sidecars (standalone files)")
    toc_desc <- c(
      toc_desc,
      paste0(
        length(consort_files),
        " PNG + matching PDF next to the workbook: ",
        paste(consort_files, collapse = ", ")
      )
    )
  }

  if (length(love_files) > 0L || length(love_skipped) > 0L) {
    toc_names <- c(toc_names, "Love plot sidecars (standalone files)")
    toc_desc <- c(
      toc_desc,
      paste0(
        length(love_files),
        " PNG + matching PDF next to the workbook: ",
        paste(love_files, collapse = ", "),
        if (length(love_skipped) > 0L) {
          paste0(
            ". No Love plot, because no numeric SMDs are stored: ",
            paste(love_skipped, collapse = ", "),
            ". Re-run s3 ($s3_analyze())."
          )
        } else {
          ""
        }
      )
    )
  }

  # Write table of contents to Provenance sheet (right side)
  toc <- data.table::data.table(
    Sheet = seq_along(toc_names),
    Name = toc_names,
    Description = toc_desc
  )
  openxlsx::writeData(
    wb,
    "Provenance",
    toc,
    startCol = 4L,
    startRow = 1L,
    headerStyle = openxlsx::createStyle(textDecoration = "bold")
  )
  openxlsx::setColWidths(
    wb,
    "Provenance",
    cols = 4:6,
    widths = c(8, 25, 60)
  )

  # Built before the workbook is saved. A build error then leaves no new
  # workbook beside an older results.qs2.
  results <- .plan_results_object(plan)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  cat("Saved:", path, "\n")
  results_path <- file.path(dirname(path), "results.qs2")
  qs2_write_atomic(results, results_path)
  cat("Saved:", results_path, "\n")
  feasibility_path <- file.path(dirname(path), .FILENAME_FEASIBILITY_XLSX)
  .plan_export_feasibility(plan, feasibility_path, enrollment_ids)
  cat("Saved:", feasibility_path, "\n")
  return(invisible(path))
}


#' The swereg versions that computed the stored results
#'
#' s3 stamps `swereg_version_s3` and `swereg_version_s2` on every result it
#' stores. A stored result MAY carry neither stamp. The s2 stamp
#' of one ETT result holds one version per distinct analysis file version.
#'
#' @param plan A `TTEPlan`.
#' @return A list with `s2` and `s3`, each the sorted unique character vector
#'   of the stamped versions. When some results carry the stamp and others do
#'   not, the vector ends with `"unknown"`. When no result carries the stamp,
#'   the vector is empty.
#' @noRd
.plan_computing_versions <- function(plan) {
  results <- c(plan$results_enrollment, plan$results_ett)
  # A feasibility-horizon ETT reports nothing, so its stamps are not shown.
  feasibility_ids <- .plan_ett_feasibility_ids(plan)
  if (length(feasibility_ids) > 0L) {
    results <- results[!names(results) %in% feasibility_ids]
  }
  pick <- function(key) {
    v <- unlist(
      lapply(results, function(r) {
        return(as.character(r[[key]] %||% NA_character_))
      }),
      use.names = FALSE
    )
    known <- unique(v[!is.na(v)])
    known <- known[order(numeric_version(known, strict = FALSE))]
    if (length(known) > 0L && anyNA(v)) {
      known <- c(known, "unknown")
    }
    return(known)
  }
  return(list(s2 = pick("swereg_version_s2"), s3 = pick("swereg_version_s3")))
}

#' Format a version set for the Provenance sheet and the export warning
#' @param v Character vector of versions.
#' @return Character(1): the versions joined by ", ", or "unknown".
#' @noRd
.plan_format_versions <- function(v) {
  if (length(v) == 0L) {
    return("unknown")
  }
  return(paste(v, collapse = ", "))
}

#' Warn when a stored result was computed by another swereg version
#'
#' It also warns when some stored results carry no recorded version and
#' others do. `$export_tables()` and `$export()` call it once each. It warns
#' and never stops.
#'
#' @param plan A `TTEPlan`.
#' @return NULL, invisibly.
#' @noRd
.plan_warn_computing_versions <- function(plan) {
  export_version <- as.character(utils::packageVersion("swereg"))
  v <- .plan_computing_versions(plan)
  stamps <- c(v$s2, v$s3)
  unrecorded <- "unknown" %in% stamps
  other <- any(setdiff(stamps, "unknown") != export_version)
  if (other || unrecorded) {
    warning(
      "This export runs swereg ",
      export_version,
      if (other) ", but other versions computed the stored results",
      ". ",
      if (unrecorded) "Some stored results carry no recorded version. ",
      "s2: ",
      .plan_format_versions(v$s2),
      ". s3: ",
      .plan_format_versions(v$s3),
      ". export: ",
      export_version,
      ".",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}


#' Write an ordered set of exhibits from a manifest
#'
#' The body of `TTEPlan$export()`. It routes each spec to a producer.
#'
#' @param plan A `TTEPlan`.
#' @param manifest A non-empty list of exhibit specs.
#' @param dir Character(1) or NULL, the output directory.
#' @return Character vector of the written paths, invisibly.
#' @noRd
.plan_export <- function(plan, manifest, dir = NULL) {
  if (!is.list(manifest) || length(manifest) == 0L) {
    stop("manifest must be a non-empty list of exhibit specs", call. = FALSE)
  }
  .tte_assert_one_s1_run(plan, "export()")
  .tte_assert_one_s2_run(plan, "export()")
  .tte_assert_horizon_rule_current(plan, "export()")
  .plan_warn_computing_versions(plan)
  if (is.null(dir)) {
    dir <- plan$dir_results
  }
  figure_types <- c("survival", "forest", "consort")
  table_types <- c("table1")
  paths <- character(0)
  for (i in seq_along(manifest)) {
    spec <- manifest[[i]]
    if (!is.list(spec)) {
      stop(
        "exhibit spec ",
        i,
        " must be a list, got class '",
        class(spec)[1],
        "'",
        call. = FALSE
      )
    }
    spec$.index <- i
    if (is.null(spec$type)) {
      stop("exhibit spec ", i, " must have a 'type'", call. = FALSE)
    }
    if (spec$type %in% figure_types) {
      paths <- c(paths, .plan_export_figure(plan, spec, dir))
    } else if (spec$type %in% table_types) {
      paths <- c(paths, .plan_export_table(plan, spec, dir))
    } else {
      stop(
        "unknown exhibit type '",
        spec$type,
        "' in spec ",
        i,
        ". Figures: survival, forest, consort. Tables: table1.",
        call. = FALSE
      )
    }
  }
  cat("Wrote", length(paths), "exhibit file(s) to", dir, "\n")
  return(invisible(paths))
}


#' Delete an image before an exhibit renders it
#'
#' @param path Character(1), the image path.
#' @return NULL, invisibly.
#' @noRd
.plan_export_clear_image <- function(path) {
  if (file.exists(path) && !file.remove(path)) {
    stop("cannot delete the old image ", path, call. = FALSE)
  }
  return(invisible(NULL))
}


#' Stop when an exhibit did not write its image
#'
#' @param path Character(1), the image path.
#' @param spec One exhibit spec from the manifest.
#' @return NULL, invisibly.
#' @noRd
.plan_export_require_image <- function(path, spec) {
  if (!file.exists(path)) {
    stop(
      spec$type,
      " figure in exhibit spec ",
      spec$.index %||% "(unindexed)",
      " wrote no image at ",
      path,
      call. = FALSE
    )
  }
  return(invisible(NULL))
}


#' Read the estimands of one figure exhibit
#'
#' An absent field means `"pp"`. An empty one is an error, because the loop
#' over it would then write no image and raise no error.
#'
#' @param spec One exhibit spec from the manifest.
#' @return A non-empty character vector.
#' @noRd
.plan_export_estimands <- function(spec) {
  estimands <- spec$estimands %||% "pp"
  if (length(estimands) == 0L) {
    stop(
      "exhibit spec ",
      spec$.index %||% "(unindexed)",
      " has an empty 'estimands'. Omit it for 'pp', or name ",
      "'pp' and/or 'itt'.",
      call. = FALSE
    )
  }
  return(estimands)
}
