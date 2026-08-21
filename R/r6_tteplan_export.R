# These TTEPlan methods write output. Two produce the results workbook and
# the ordered exhibit set, and two are the private exhibit producers they
# dispatch to.

#' @include r6_tteplan.R
#' @description Export analysis results to an Excel workbook.
#'
#' Requires `self$results_enrollment` and `self$results_ett` to be populated
#' (run `$s3_analyze()` first).
#'
#' If the cached baseline tables were produced by an older version of
#' `swereg` (when Table 1 was a `tableone` object), they are automatically
#' refreshed in-process via `$recompute_baselines()` using the analysis
#' files in `output_dir`.
#'
#' The workbook carries no forest plot. The `PP results` and `ITT results`
#' sheets already report every emulated trial with counts, rates, ratios,
#' risk differences, intervals and numbers needed to treat. A forest image
#' repeated a subset of those numbers. `$export()` still draws one for a
#' manuscript.
#'
#' @param path File path for the output `.xlsx` file.
#' @param table1_enrollment Enrollment ID for Table 1 (main baseline table).
#'   Default: the enrollment with the most baseline observations.
#' @param protocol_ett_id Optional character(1) ETT id. The
#'   `Target trial protocol` sheet describes this one emulated trial. An id
#'   the plan does not hold raises a warning and falls back. When `NULL`
#'   (default), the sheet describes the first ETT of the Table 1
#'   enrollment, and otherwise the first ETT in the grid.
#' @param output_dir Optional directory holding the cached `.qs2` files.
#'   Used by the lazy `recompute_baselines()` refresh. Defaults to
#'   `self$output_dir`.
TTEPlan$set(
  "public",
  "export_tables",
  function(
    path = NULL,
    table1_enrollment = NULL,
    protocol_ett_id = NULL,
    output_dir = NULL
  ) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop(
        "Package 'openxlsx' is required. Install with: install.packages('openxlsx')"
      )
    }
    if (
      is.null(self$results_enrollment) ||
        length(self$results_enrollment) == 0L
    ) {
      stop("No enrollment results. Run $s3_analyze() first.")
    }
    if (is.null(self$results_ett) || length(self$results_ett) == 0L) {
      stop("No ETT results. Run $s3_analyze() first.")
    }
    if (is.null(path)) {
      path <- self$tables_xlsx
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    }

    # Lazy refresh of stale baseline results (pre-swereg_table1 cache, and
    # pre-smd_numeric cache -- see .baseline_panel_is_stale()).
    stale <- vapply(
      self$results_enrollment,
      .baseline_panel_is_stale,
      logical(1)
    )
    if (any(stale)) {
      message(
        "Refreshing ",
        sum(stale),
        " stale baseline table(s) from disk..."
      )
      self$recompute_baselines(
        output_dir = output_dir,
        enrollment_ids = names(stale)[stale]
      )
    }

    ett <- self$ett
    enrollment_ids <- unique(ett$enrollment_id)

    # Normalise the requested protocol ETT to one id, or to NULL. An id the
    # plan does not hold falls back, exactly as an absent argument does.
    if (length(protocol_ett_id) > 0L) {
      protocol_ett_id <- as.character(protocol_ett_id)[1L]
      if (!protocol_ett_id %in% ett$ett_id) {
        warning(
          "protocol_ett_id is not an ETT id of this plan (ignored): ",
          protocol_ett_id
        )
        protocol_ett_id <- NULL
      }
    } else {
      protocol_ett_id <- NULL
    }

    # Determine table1 enrollment. `$get_baselines()` repeats the baseline
    # size on every row of that enrollment, and it returns a counts-only row
    # for an enrollment that stored no panel, so every analysed enrollment is
    # represented. An enrollment with no stored size counts as 0, which is
    # what the raw read did.
    if (is.null(table1_enrollment)) {
      eids_analysed <- .plan_analysed_enrollment_ids(self)
      base_all <- self$get_baselines()
      n_baselines <- vapply(
        eids_analysed,
        function(eid) {
          n <- .baseline_count(base_all, eid, "n_baseline")
          if (is.na(n)) 0 else n
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
    .write_provenance(wb, self)
    toc_names <- c(toc_names, "Provenance")
    toc_desc <- c(toc_desc, "Pipeline metadata and table of contents")

    # --- Study Specification sheet ---
    .write_spec_summary(wb, self)
    toc_names <- c(toc_names, "Study Specification")
    toc_desc <- c(toc_desc, "Study design, variables, ICD-10/ATC codes")

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
      self,
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
    .write_enrollment_overview(wb, self)
    toc_names <- c(toc_names, "Enrollments")
    toc_desc <- c(
      toc_desc,
      "Enrollment overview (treatment, comparator draw, criteria)"
    )

    # --- ETTs overview sheet ---
    .write_ett_overview(wb, self)
    toc_names <- c(toc_names, "ETTs")
    toc_desc <- c(toc_desc, "ETT overview (outcome, follow-up, events)")

    # --- Table 1: Baseline for chosen enrollment ---
    t1_label <- .enrollment_label(self, table1_enrollment)
    t1_baselines <- self$get_baselines()
    t1_arms <- .baseline_arm_labels(t1_baselines, table1_enrollment)
    t1_panel <- function(weighting, variant) {
      .baseline_panel(
        t1_baselines,
        table1_enrollment,
        "imputed",
        weighting,
        variant,
        t1_arms
      )
    }
    # The Love plot reads the accessor rows themselves. It needs the
    # unrounded `smd_numeric`, which is a programmatic contract rather than a
    # rendered cell, so it never goes through `.baseline_panel()`.
    # `which()` runs OUTSIDE the data.table subset. Inside `t1_baselines[...]`
    # the two arguments would resolve to the COLUMNS of the same name, and
    # the filter would keep every panel.
    t1_rows <- function(want_weighting, want_variant) {
      hit <- which(
        t1_baselines$enrollment_id == table1_enrollment &
          t1_baselines$imputation == "imputed" &
          t1_baselines$weighting == want_weighting &
          t1_baselines$variant == want_variant
      )
      t1_baselines[hit]
    }
    t1_main <- t1_panel("ipw_trunc", "main") %||%
      t1_panel("ipw_trunc", "supplementary")
    if (!is.null(t1_main)) {
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
        )
      )
      toc_names <- c(toc_names, "Table 1")
      toc_desc <- c(
        toc_desc,
        paste0(
          "Baseline characteristics (IPW truncated) -- ",
          t1_label
        )
      )
    }

    # Resolve the directory for image sidecars (next to the workbook)
    img_dir <- dirname(path)
    img_basename_root <- tools::file_path_sans_ext(basename(path))

    # --- Love plot sheet (covariate balance for the Table 1 enrollment) ---
    # Series: unweighted vs IPW-truncated. The truncated weights are the
    # analysis weights, so the untruncated panel is not plotted.
    .write_love_plot(
      wb,
      "Love plot",
      t1_unweighted = t1_rows("none", "supplementary"),
      # The SUPPLEMENTARY truncated panel, named by three accessor keys
      # rather than by a slot name. A slot name could partial-match:
      # `table1_ipw_trunc` is a strict prefix of `table1_ipw_trunc_main`, and
      # the Love plot would then draw the main panel as the weighted series.
      # `weighting` and `variant` are separate columns, so no such match
      # exists.
      t1_weighted = t1_rows("ipw_trunc", "supplementary"),
      title = paste0(
        "Love plot: covariate balance before and after weighting",
        " -- Enrollment ",
        table1_enrollment,
        " (",
        t1_label,
        ")"
      ),
      img_dir = img_dir,
      img_basename = paste0(img_basename_root, "_love_plot")
    )
    toc_names <- c(toc_names, "Love plot")
    toc_desc <- c(
      toc_desc,
      paste0(
        "Covariate balance (absolute SMD, unweighted vs IPW truncated) -- ",
        t1_label
      )
    )

    # --- PP results sheet (per-protocol, truncated weights, all ETTs) ---
    .write_results_single(
      wb,
      "PP results",
      self,
      rates_slot = "rates_pp_trunc",
      irr_slot = "irr_pp_trunc",
      rd_slot = "rd_pp_trunc",
      title = "Per-protocol results (truncated weights) - all ETTs"
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
      self,
      rates_slot = "rates_itt",
      irr_slot = "irr_itt",
      rd_slot = "rd_itt",
      title = "Intention-to-treat results - all ETTs"
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
      self,
      trunc_rates_slot = "rates_pp_trunc",
      trunc_irr_slot = "irr_pp_trunc",
      untrunc_rates_slot = "rates_pp",
      untrunc_irr_slot = "irr_pp",
      title = paste0(
        "Weight-truncation robustness (per-protocol): truncated (left) vs ",
        "untruncated (right) weights - all ETTs"
      )
    )
    toc_names <- c(toc_names, "Weight truncation (PP)")
    toc_desc <- c(
      toc_desc,
      "Supplementary - PP IRRs, truncated vs untruncated weights"
    )

    # --- Effect modification sheet (only if any subgroups are configured) ---
    has_subgroups <- "subgroup_vars" %in%
      names(self$ett) &&
      any(vapply(
        self$ett$subgroup_vars,
        function(x) length(x) > 0L,
        logical(1)
      ))
    if (has_subgroups) {
      .write_effect_modification(
        wb,
        "Effect modification",
        self,
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

    # --- Table S1-SN: Combined baselines per enrollment ---
    for (j in seq_along(enrollment_ids)) {
      eid <- enrollment_ids[j]
      sheet_name <- paste0("Table S", j)
      .write_combined_baseline(wb, sheet_name, self, eid)
      toc_names <- c(toc_names, sheet_name)
      label <- .enrollment_label(self, eid)
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
    n_s <- length(enrollment_ids)

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
    consort_files <- character()
    {
      for (eid in enrollment_ids) {
        ec <- .plan_cohort_counts(self, eid)
        if (!is.null(ec$attrition)) {
          attrition_sheet <- paste0("Attrition_", eid)
          label <- .enrollment_label(self, eid)
          if (isTRUE(.write_attrition_sheet(wb, attrition_sheet, self, eid))) {
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
            plan = self,
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

    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    cat("Saved:", path, "\n")
    invisible(path)
  }
)

#' @description Produce an ORDERED set of exhibits (figures and/or tables)
#' from a manifest and write them to `dir` with two-digit order prefixes, so
#' the manifest order becomes the exhibit numbering. This is the single
#' programmatic entry point: a project declares its exhibit set once and
#' hands it over; other projects reuse the same driver with a different
#' manifest. Each spec's `type` routes it to a producer:
#' \describe{
#'   \item{figures}{`"survival"` (weighted survival curve for one ETT cell,
#'     one image per estimand), `"forest"` (forest plot over a named
#'     `exposures` set, one image per estimand), and `"consort"` (CONSORT
#'     flow diagram for an enrollment).}
#'   \item{tables}{`"table1"` (baseline characteristics for an enrollment,
#'     written as CSV).}
#' }
#' Full per-type fields are documented on the private `.export_figure()` /
#' `.export_table()` producers.
#'
#' Two `"forest"` and `"survival"` fields carry a decision worth stating
#' here, because both are silent when they go wrong.
#'
#' `"survival"` is drawn on the CUMULATIVE-FAILURE scale, which is one
#' minus survival. A y-axis window is therefore meaningless until it says
#' which scale it is measured on, so `ylim` requires a companion
#' `ylim_scale`, either `"survival"` or `"cumulative_failure"`. A
#' survival-scale window is translated onto the plotted scale:
#' `c(0.95, 1)` becomes `c(0, 0.05)` and shows the same band of the figure
#' it always did. An undeclared window is an error, not a guess. Left
#' undeclared and applied as given, a survival-scale window clips the whole
#' cumulative-failure curve out of view and produces a blank panel with no
#' error and no warning.
#'
#' `"forest"` takes `risk_difference = TRUE` to SHOW the signed
#' cause-specific risk difference per 10,000 people, with its interval.
#' The option computes nothing. `$s3_analyze()` computes the risk
#' difference for every ETT and stores it, so this switch only decides
#' whether the figure carries the two extra columns.
#'
#' The `n_boot`, `seed` and `conf_level` fields are inert and warn.
#' `$s3_analyze()` fixes `n_boot` and `seed`. It reads the confidence level
#' from `study$implementation$conf_level`, so a study sets its level once
#' and every result and header carries it. A figure that could restate the
#' level would print a label the numbers do not have.
#' @param manifest A non-empty list of exhibit specs. Every spec needs a
#'   `type`; other fields depend on the type. Optional `label` (filename
#'   stem) and `title`.
#' @param dir Output directory. Defaults to `self$dir_results`.
#' @return Character vector of all written paths (invisibly).
TTEPlan$set("public", "export", function(manifest, dir = NULL) {
  if (!is.list(manifest) || length(manifest) == 0L) {
    stop("manifest must be a non-empty list of exhibit specs")
  }
  if (is.null(dir)) {
    dir <- self$dir_results
  }
  figure_types <- c("survival", "forest", "consort")
  table_types <- c("table1")
  paths <- character(0)
  for (i in seq_along(manifest)) {
    spec <- manifest[[i]]
    spec$.index <- i
    if (is.null(spec$type)) {
      stop("exhibit spec ", i, " must have a 'type'")
    }
    if (spec$type %in% figure_types) {
      paths <- c(paths, private$.export_figure(spec, dir))
    } else if (spec$type %in% table_types) {
      paths <- c(paths, private$.export_table(spec, dir))
    } else {
      stop(
        "unknown exhibit type '",
        spec$type,
        "' in spec ",
        i,
        ". Figures: survival, forest, consort. Tables: table1."
      )
    }
  }
  cat("Wrote", length(paths), "exhibit file(s) to", dir, "\n")
  invisible(paths)
})

# Produce one FIGURE exhibit (image) from a spec; dispatched by $export().
# Types:
#   "survival": weighted survival curve for one ETT cell (enrollment,
#     outcome, follow_up, age_group). One image per `estimands` entry --
#     "pp" reads rd_curve_pp_trunc, "itt" reads rd_curve_itt, both through
#     $get_curves(). No branch of this method opens an analysis file. The
#     figure is
#     drawn on the CUMULATIVE-FAILURE scale, so an optional `ylim` window
#     must declare its own scale in `ylim_scale` ("survival" or
#     "cumulative_failure"); a survival-scale window is translated onto the
#     plotted one, and an undeclared window is an error.
#   "forest": forest plot over `exposures` (named list label -> ett_id),
#     one image per `estimands` entry. `group_by` ("exposure"/"outcome")
#     picks the grouping; `label_format`/`desc_header` tune the text panel;
#     `role_headers` (named role -> label map, e.g.
#     c(primary = "Primary outcome", secondary = "Secondary outcomes")) adds
#     role sub-headers within each exposure block (group_by = "exposure").
#     `risk_difference = TRUE` SHOWS the signed cause-specific risk
#     difference per 10,000 with its interval, read from the results
#     `$s3_analyze()` stored. It computes nothing. `n_boot`, `seed` and
#     `conf_level` are inert and warn. The header states the level s3 used,
#     which is `study$implementation$conf_level`, so the column cannot
#     state a level the numbers do not have.
TTEPlan$set("private", ".export_figure", function(spec, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stem <- spec$label %||% spec$type
  base <- if (!is.null(spec$.index)) {
    sprintf("%02d_%s", spec$.index, stem)
  } else {
    stem
  }

  if (identical(spec$type, "survival")) {
    ett_row <- self$ett[
      enrollment_id == spec$enrollment &
        outcome_var == spec$outcome &
        follow_up == spec$follow_up &
        age_group == spec$age_group
    ]
    if (nrow(ett_row) != 1L) {
      stop(
        "survival figure needs exactly 1 matching ETT, found ",
        nrow(ett_row),
        " for enrollment=",
        spec$enrollment,
        " outcome=",
        spec$outcome,
        " follow_up=",
        spec$follow_up,
        " age_group=",
        spec$age_group
      )
    }
    estimands <- spec$estimands %||% "pp"
    # A y-axis window is meaningless without the scale it is measured on.
    # This figure plots CUMULATIVE FAILURE, so a survival-scale window such
    # as c(0.95, 1) would clip the whole curve out of view through
    # coord_cartesian(): a blank panel, with no error and no warning. The
    # scale is therefore declared and translated, never guessed. Neither
    # pure convention is safe on its own, because the mirror mistake -- a
    # failure-scale window silently read as a survival-scale one -- blanks
    # the panel just as quietly.
    ylim_plot <- spec$ylim
    if (!is.null(ylim_plot)) {
      if (
        !is.numeric(ylim_plot) ||
          length(ylim_plot) != 2L ||
          any(!is.finite(ylim_plot)) ||
          ylim_plot[1] >= ylim_plot[2]
      ) {
        stop(
          "survival figure 'ylim' must be two increasing finite numbers, ",
          "low bound first"
        )
      }
      ylim_scale <- spec$ylim_scale
      if (is.null(ylim_scale)) {
        stop(
          "survival figure 'ylim' requires 'ylim_scale', either ",
          "'survival' or 'cumulative_failure'. The figure plots ",
          "cumulative failure, so an undeclared survival-scale window ",
          "such as c(0.95, 1) would blank the panel."
        )
      }
      if (
        !identical(ylim_scale, "survival") &&
          !identical(ylim_scale, "cumulative_failure")
      ) {
        stop(
          "survival figure 'ylim_scale' must be 'survival' or ",
          "'cumulative_failure', got '",
          ylim_scale,
          "'"
        )
      }
      if (identical(ylim_scale, "survival")) {
        # 1 - survival, so the bounds swap roles as well as values.
        ylim_plot <- c(1 - ylim_plot[2], 1 - ylim_plot[1])
      }
    }
    # NO ANALYSIS FILE IS OPENED TO RENDER. This branch read one until
    # 26.8.20, and it was the last RENDER read in the export path.
    # `$s3_analyze()` stores S(t) for both arms, and the head count of
    # people at risk in each arm and band. Both panels of this figure
    # therefore come from `$get_curves()`. s3 computes, s4 formats.
    #
    # One analysis read remains in `$export_tables()` and it is a
    # PRODUCER's. A stale baseline panel sends `$recompute_baselines()` to
    # `.s3_enrollment_worker()`, which is s3's own worker computing and
    # storing a Table 1 panel. That is s3 running late, not s4 computing.
    paths <- character(0)
    id_ett <- as.character(ett_row$ett_id[1])
    curves <- self$get_curves()
    arms <- .tte_arm_labels_resolved(
      .lookup_arm_labels(self$spec, spec$enrollment)
    )
    for (est in estimands) {
      slot <- if (identical(est, "pp")) {
        "rd_curve_pp_trunc"
      } else if (identical(est, "itt")) {
        "rd_curve_itt"
      } else {
        stop("survival estimand must be 'pp' or 'itt', got '", est, "'")
      }
      combo <- .tte_slot_combo(slot)
      cv <- curves[
        ett_id == id_ett &
          estimand == combo[["estimand"]] &
          weights == combo[["weights"]]
      ]
      if (nrow(cv) == 0L) {
        stop(
          "no stored survival curve for ",
          id_ett,
          " (",
          est,
          "). Run $s3_analyze(), which stores '",
          slot,
          "'."
        )
      }
      # The risk table refuses to draw on missing counts. A curve stored
      # before s3 carried them gives `NA`, and a row of missing values
      # looks exactly like a drawn risk table.
      if (anyNA(cv$n_persons_at_risk)) {
        stop(
          "the stored '",
          slot,
          "' curve of ",
          id_ett,
          " carries no numbers at risk. Re-run $s3_analyze(), which ",
          "stores the distinct-person count for each arm and band."
        )
      }
      curve <- data.table::data.table(
        band = as.numeric(cv$band),
        surv = as.numeric(cv$surv),
        n_persons_at_risk = as.numeric(cv$n_persons_at_risk),
        group = data.table::fifelse(
          cv$arm == "intervention",
          arms[["intervention"]],
          arms[["comparator"]]
        )
      )
      data.table::setorderv(curve, c("group", "band"))
      q <- .render_survival_curve(
        curve = curve,
        time_var = "band",
        # Cumulative failure, not survival: a rare outcome is unreadable
        # as a curve pinned near 100%.
        scale = "cumulative_failure",
        # Title is just the outcome (the exposure/contrast is in the
        # legend).
        title = spec$title %||% ett_row$outcome_name,
        ylim = ylim_plot,
        int_lab = arms[["intervention"]],
        cmp_lab = arms[["comparator"]]
      )
      out <- file.path(dir, paste0(base, "_", est, ".png"))
      ggplot2::ggsave(out, q, width = 8, height = 6, dpi = 300)
      paths <- c(paths, out)
    }
    return(paths)
  }

  if (identical(spec$type, "consort")) {
    eid <- spec$enrollment
    if (!eid %in% .plan_counted_enrollment_ids(self)) {
      stop("no enrollment counts for '", eid, "'. Run enrollment first.")
    }
    ec <- .plan_cohort_counts(self, eid)
    .render_consort_sidecars(
      plan = self,
      ec = ec,
      eid = eid,
      label = .enrollment_label(self, eid),
      output_dir = dir,
      img_basename = base
    )
    return(file.path(dir, paste0(base, ".png")))
  }

  if (identical(spec$type, "forest")) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required for forest figures.")
    }
    if (is.null(spec$exposures)) {
      stop(
        "forest figure requires 'exposures' (named list of label -> ett_id)"
      )
    }
    exp_names <- names(spec$exposures)
    if (is.null(exp_names) || anyNA(exp_names) || any(!nzchar(exp_names))) {
      stop(
        "forest 'exposures' must be a fully named list (no blank/NA names)"
      )
    }
    # Flatten to ett ids plus a PARALLEL vector of group labels, one per ett
    # id (.write_forest_irr maps ett_id -> label by position). `group_by`
    # chooses the grouping: "exposure" groups by the exposure contrast with
    # outcomes as rows; "outcome" groups by outcome with exposures as rows.
    keep_ids <- unlist(spec$exposures, use.names = FALSE)
    if (length(keep_ids) == 0L) {
      stop("forest 'exposures' resolved to zero ETT ids")
    }
    missing_ids <- setdiff(keep_ids, self$ett$ett_id)
    if (length(missing_ids) > 0L) {
      stop(
        "forest 'exposures' contains unknown ETT ids: ",
        paste(missing_ids, collapse = ", ")
      )
    }
    group_by <- spec$group_by %||% "exposure"
    if (identical(group_by, "exposure")) {
      keep_groups <- rep(
        names(spec$exposures),
        times = lengths(spec$exposures)
      )
      default_label <- "{outcome_name}"
    } else if (identical(group_by, "outcome")) {
      keep_groups <- self$ett$outcome_name[match(keep_ids, self$ett$ett_id)]
      # Reorder so same-outcome rows are consecutive (in spec outcome order);
      # the renderer only merges consecutive same-label rows, and the ett
      # list arrives exposure-major, which would split each outcome into
      # many single-row groups.
      ord <- order(
        match(keep_groups, unique(self$ett$outcome_name)),
        seq_along(keep_ids)
      )
      keep_ids <- keep_ids[ord]
      keep_groups <- keep_groups[ord]
      default_label <- "{enrollment_name}"
    } else {
      stop(
        "forest group_by must be 'exposure' or 'outcome', got '",
        group_by,
        "'"
      )
    }
    # When the spec assigns outcome roles (primary/secondary) and outcomes
    # are the rows (group_by = "exposure"), surface the role from metadata in
    # the default row label -- the spec `name` stays clean; role rides the
    # `role:` field via {outcome_role}. Overridable with an explicit
    # `spec$label_format`.
    spec_roles <- vapply(
      self$spec$outcomes %||% list(),
      function(o) o$role %||% NA_character_,
      character(1)
    )
    if (
      any(!is.na(spec_roles)) &&
        identical(group_by, "exposure") &&
        is.null(spec$label_format)
    ) {
      default_label <- "{outcome_name} ({outcome_role})"
    }
    # Optional role sub-headers ("Primary outcome" / "Secondary outcomes"):
    # a named map role -> label from the manifest, threaded into the forest
    # as an extra grouping tier. Only meaningful when outcomes are the rows
    # (group_by = "exposure"); pairs naturally with a clean
    # `label_format = "{outcome_name}"` so the role isn't also in the label.
    role_headers_vec <- if (
      identical(group_by, "exposure") && !is.null(spec$role_headers)
    ) {
      unlist(spec$role_headers)
    } else {
      NULL
    }
    estimands <- spec$estimands %||% "pp"
    # `risk_difference` is a DISPLAY switch and computes nothing. s3 stores
    # the risk difference for every ETT, so this option only decides
    # whether the figure carries the two extra columns.
    #
    # It used to gate the computation as well. The quantity was rebuilt
    # here from each featured ETT's analysis panel on disk. A script that
    # left the option unset drew every figure without it. There was no
    # error and no warning.
    show_rd <- isTRUE(spec$risk_difference)
    # The level the HEADER states, read from the same study property s3
    # computed the interval at. One study, one level, one place to set it.
    rd_conf_level <- .s3_conf_level(self$spec)
    # `n_boot`, `seed` and `conf_level` do not reach the estimator from
    # here. s3 fixes the first two and reads the third from
    # `study$implementation$conf_level`. Say so rather than accepting them
    # and ignoring them: a setting that looks live and is not is how the
    # first defect stayed invisible.
    inert <- intersect(c("n_boot", "seed", "conf_level"), names(spec))
    if (length(inert) > 0L) {
      warning(
        "forest figure option(s) ",
        paste(inert, collapse = ", "),
        " do not affect the risk difference. $s3_analyze() computes it ",
        "for every ETT at n_boot = ",
        .S3_RD_N_BOOT,
        ", seed = ",
        .S3_RD_SEED,
        ", conf_level = ",
        rd_conf_level,
        ". Set the level at study$implementation$conf_level, and remove ",
        "these from the manifest."
      )
    }
    paths <- character(0)
    for (est in estimands) {
      # Three RESULT slots and no file name. The forest figure reads
      # `plan$results_ett` only. It opened an analysis file to rebuild the
      # risk difference before, and that read is gone.
      slots <- if (identical(est, "pp")) {
        list(
          r = "rates_pp_trunc",
          i = "irr_pp_trunc",
          rd = "rd_pp_trunc"
        )
      } else if (identical(est, "itt")) {
        list(
          r = "rates_itt",
          i = "irr_itt",
          rd = "rd_itt"
        )
      } else {
        stop("forest estimand must be 'pp' or 'itt', got '", est, "'")
      }
      rd_lookup <- NULL
      if (show_rd) {
        # READ, never recompute. `$get_estimates()` carries the stored risk
        # difference on the same row as the ratio it belongs to. A failed
        # emulated trial stored a skip envelope, which the accessor reports
        # as absent, and it renders an empty cell.
        rd_lookup <- .tte_rd_lookup(self, slots$rd, keep_ids)
      }
      img_base <- paste0(base, "_", est)
      .write_forest_irr(
        openxlsx::createWorkbook(),
        sheet_name = paste0("forest_", est),
        plan = self,
        rates_slot = slots$r,
        irr_slot = slots$i,
        title = spec$title,
        keep_ett_ids = keep_ids,
        group_labels = keep_groups,
        label_format = spec$label_format %||% default_label,
        desc_header = spec$desc_header,
        role_headers = role_headers_vec,
        rd_lookup = rd_lookup,
        # The SAME study property s3 computed the interval at, so the
        # header cannot state a level the numbers do not have.
        # `.write_forest_irr` checks it against each row's own
        # `conf_level` and stops on a disagreement. That check now also
        # catches a specification edited between s3 and the export.
        rd_conf_level = rd_conf_level,
        img_dir = dir,
        img_basename = img_base
      )
      paths <- c(paths, file.path(dir, paste0(img_base, ".png")))
    }
    return(paths)
  }

  stop("unknown figure type '", spec$type, "'")
})

# Produce one TABLE exhibit from a spec; dispatched by $export().
#   "table1": IPW-truncated baseline characteristics for `enrollment`,
#     written as CSV (from the computed results_enrollment table1).
TTEPlan$set("private", ".export_table", function(spec, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stem <- spec$label %||% spec$type
  base <- if (!is.null(spec$.index)) {
    sprintf("%02d_%s", spec$.index, stem)
  } else {
    stem
  }

  if (identical(spec$type, "table1")) {
    eid <- spec$enrollment
    if (!eid %in% .plan_analysed_enrollment_ids(self)) {
      stop("no enrollment results for '", eid, "'. Run analysis first.")
    }
    baselines <- self$get_baselines()
    arms <- .baseline_arm_labels(baselines, eid)
    tbl <- .baseline_panel(
      baselines,
      eid,
      "imputed",
      "ipw_trunc",
      "main",
      arms
    ) %||%
      .baseline_panel(
        baselines,
        eid,
        "imputed",
        "ipw_trunc",
        "supplementary",
        arms
      )
    if (is.null(tbl)) {
      stop("no Table 1 available for enrollment '", eid, "'")
    }
    out <- file.path(dir, paste0(base, "_", eid, ".csv"))
    # `.baseline_panel()` composes display columns only, so `smd_numeric`
    # never reaches the file.
    data.table::fwrite(tbl, out)
    return(out)
  }

  stop("unknown table type '", spec$type, "'")
})
