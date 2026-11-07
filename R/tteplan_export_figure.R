# The two exhibit producers that `.plan_export()` in `R/tteplan_export.R`
# dispatches to, one for a figure exhibit and one for a table exhibit.
#
# `.plan_export_figure()` and `.plan_export_table()` are plain functions, and
# `.plan_export()` reaches them by name.

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
#' Produce one figure exhibit
#'
#' `.plan_export()` calls it for each figure exhibit of the manifest.
#'
#' @param plan A `TTEPlan`.
#' @param spec One exhibit spec from the manifest.
#' @param dir Character(1), the output directory.
#' @return Character vector of the written image paths.
#' @noRd
.plan_export_figure <- function(plan, spec, dir) {
  # Local bindings (avoid R CMD check NSE notes)
  enrollment_id <- outcome_var <- follow_up <- age_group <- NULL # nolint
  ett_id <- estimand <- weights <- NULL # nolint

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stem <- spec$label %||% spec$type
  base <- if (!is.null(spec$.index)) {
    sprintf("%02d_%s", spec$.index, stem)
  } else {
    stem
  }

  if (identical(spec$type, "survival")) {
    for (key in c("enrollment", "outcome", "follow_up", "age_group")) {
      if (length(spec[[key]]) != 1L) {
        stop(
          "survival figure in exhibit spec ",
          spec$.index %||% "(unindexed)",
          " needs exactly one '",
          key,
          "'. Use age_group = NA for an ETT without an age group.",
          call. = FALSE
        )
      }
    }
    # `==` drops an NA on either side. An ETT with no age group stores NA,
    # so NA in the spec MUST match NA in the ETT grid.
    spec_age <- spec$age_group
    ett_row <- .plan_ett_reported(plan)[
      enrollment_id == spec$enrollment &
        outcome_var == spec$outcome &
        follow_up == spec$follow_up &
        ((is.na(age_group) & is.na(spec_age)) |
          (!is.na(age_group) & !is.na(spec_age) & age_group == spec_age))
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
        spec$age_group,
        call. = FALSE
      )
    }
    estimands <- .plan_export_estimands(spec)
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
          "low bound first",
          call. = FALSE
        )
      }
      ylim_scale <- spec$ylim_scale
      if (is.null(ylim_scale)) {
        stop(
          "survival figure 'ylim' requires 'ylim_scale', either ",
          "'survival' or 'cumulative_failure'. The figure plots ",
          "cumulative failure, so an undeclared survival-scale window ",
          "such as c(0.95, 1) would blank the panel.",
          call. = FALSE
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
          "'",
          call. = FALSE
        )
      }
      if (identical(ylim_scale, "survival")) {
        # 1 - survival, so the bounds swap roles as well as values.
        ylim_plot <- c(1 - ylim_plot[2], 1 - ylim_plot[1])
      }
    }
    # NO ANALYSIS FILE IS OPENED TO RENDER. This branch read one until 26.8.20,
    # and it was the last RENDER read in the export path. `$s3_analyze()` stores
    # S(t) for both arms, and the head count of people at risk in each arm at
    # each stop time. Both panels of this figure therefore come from
    # `$get_curves()`. s3 computes, s4 formats.
    #
    # One analysis read remains in `$export_tables()` and it is a
    # PRODUCER's. A stale baseline panel sends `$recompute_baselines()` to
    # `.s3_enrollment_worker()`, which is s3's own worker computing and
    # storing a Table 1 panel. A result without the name `adherence` takes
    # the same route when its raw file is on disk, and the worker then also
    # stores the adherence counts. That is s3 running late, not s4 computing.
    paths <- character(0)
    id_ett <- as.character(ett_row$ett_id[1])
    curves <- plan$get_curves()
    # The minimum-events rule: both curves stop at the reportable horizon, and a
    # grey band covers the rest of the follow-up.
    est_all <- plan$get_estimates()
    arms <- .tte_arm_labels_resolved(
      .lookup_arm_labels(plan$spec, spec$enrollment)
    )
    for (est in estimands) {
      slot <- if (identical(est, "pp")) {
        "rd_curve_pp_trunc"
      } else if (identical(est, "itt")) {
        "rd_curve_itt"
      } else {
        stop(
          "survival estimand must be 'pp' or 'itt', got '",
          est,
          "'",
          call. = FALSE
        )
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
          "'.",
          call. = FALSE
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
          "stores the distinct-person count for each arm and stop time.",
          call. = FALSE
        )
      }
      curve <- data.table::data.table(
        follow_up_interval = as.numeric(cv$follow_up_interval),
        surv = as.numeric(cv$surv),
        n_persons_at_risk = as.numeric(cv$n_persons_at_risk),
        group = data.table::fifelse(
          cv$arm == "intervention",
          arms[["intervention"]],
          arms[["comparator"]]
        )
      )
      data.table::setorderv(curve, c("group", "follow_up_interval"))
      q <- .render_survival_curve(
        curve = curve,
        time_var = "follow_up_interval",
        # Cumulative failure, not survival: a rare outcome is unreadable
        # as a curve pinned near 100%.
        scale = "cumulative_failure",
        # Title is just the outcome (the exposure/contrast is in the
        # legend).
        title = spec$title %||% ett_row$outcome_name,
        ylim = ylim_plot,
        int_lab = arms[["intervention"]],
        cmp_lab = arms[["comparator"]],
        horizon_weeks = est_all[
          ett_id == id_ett & estimand == combo[["estimand"]]
        ]$horizon_reportable_weeks[1L],
        follow_up = as.numeric(ett_row$follow_up[1L])
      )
      out <- file.path(dir, paste0(base, "_", est, ".png"))
      ggplot2::ggsave(out, q, width = 8, height = 6, dpi = 300)
      paths <- c(paths, out)
    }
    return(paths)
  }

  if (identical(spec$type, "consort")) {
    eid <- spec$enrollment
    if (!eid %in% .plan_counted_enrollment_ids(plan)) {
      stop(
        "no enrollment counts for '",
        eid,
        "'. Run enrollment first.",
        call. = FALSE
      )
    }
    ec <- .plan_cohort_counts(plan, eid)
    out <- file.path(dir, paste0(base, ".png"))
    # A PNG left by an earlier run would satisfy the existence check below.
    .plan_export_clear_image(out)
    sidecars <- .render_consort_sidecars(
      plan = plan,
      ec = ec,
      eid = eid,
      label = .enrollment_label(plan, eid),
      output_dir = dir,
      img_basename = base
    )
    if (is.null(sidecars)) {
      stop(
        "CONSORT figure for enrollment '",
        eid,
        "' in exhibit spec ",
        spec$.index %||% "(unindexed)",
        " was not rendered. See the warning above.",
        call. = FALSE
      )
    }
    .plan_export_require_image(out, spec)
    return(out)
  }

  if (identical(spec$type, "forest")) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required for forest figures.", call. = FALSE)
    }
    if (is.null(spec$exposures)) {
      stop(
        "forest figure requires 'exposures' (named list of label -> ett_id)",
        call. = FALSE
      )
    }
    exp_names <- names(spec$exposures)
    if (is.null(exp_names) || anyNA(exp_names) || any(!nzchar(exp_names))) {
      stop(
        "forest 'exposures' must be a fully named list (no blank/NA names)",
        call. = FALSE
      )
    }
    # Flatten to ett ids plus a PARALLEL vector of group labels, one per ett
    # id (.write_forest_irr maps ett_id -> label by position). `group_by`
    # chooses the grouping: "exposure" groups by the exposure contrast with
    # outcomes as rows; "outcome" groups by outcome with exposures as rows.
    keep_ids <- unlist(spec$exposures, use.names = FALSE)
    if (length(keep_ids) == 0L) {
      stop("forest 'exposures' resolved to zero ETT ids", call. = FALSE)
    }
    reported <- .plan_ett_reported(plan)
    missing_ids <- setdiff(keep_ids, reported$ett_id)
    if (length(missing_ids) > 0L) {
      stop(
        "forest 'exposures' contains unknown ETT ids: ",
        paste(missing_ids, collapse = ", "),
        call. = FALSE
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
      keep_groups <- reported$outcome_name[match(keep_ids, reported$ett_id)]
      keep_exposures <- rep(
        names(spec$exposures),
        times = lengths(spec$exposures)
      )
      # Reorder so same-outcome rows are consecutive, in the order of the
      # spec's `outcomes` block. The renderer only merges consecutive
      # same-label rows, and the ett list arrives exposure-major, which would
      # split each outcome into many single-row groups. An outcome the spec
      # does not name follows, in ETT grid order.
      spec_outcome_names <- unname(
        .spec_outcome_name_lookup(plan$spec)$name %||% character(0)
      )
      ord <- order(
        match(
          keep_groups,
          unique(c(spec_outcome_names, reported$outcome_name))
        ),
        seq_along(keep_ids)
      )
      keep_ids <- keep_ids[ord]
      keep_groups <- keep_groups[ord]
      # Each row is one exposure, so its label is the manifest's exposure
      # name. A label format keyed by ETT id carries that name per row.
      default_label <- setNames(keep_exposures[ord], keep_ids)
    } else {
      stop(
        "forest group_by must be 'exposure' or 'outcome', got '",
        group_by,
        "'",
        call. = FALSE
      )
    }
    # When the spec assigns outcome roles (primary/secondary) and outcomes
    # are the rows (group_by = "exposure"), surface the role from metadata in
    # the default row label -- the spec `name` stays clean; role rides the
    # `role:` field via {outcome_role}. Overridable with an explicit
    # `spec$label_format`.
    spec_roles <- vapply(
      plan$spec$outcomes %||% list(),
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
    estimands <- .plan_export_estimands(spec)
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
    rd_conf_level <- .s3_conf_level(plan$spec)
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
        "these from the manifest.",
        call. = FALSE
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
        stop(
          "forest estimand must be 'pp' or 'itt', got '",
          est,
          "'",
          call. = FALSE
        )
      }
      rd_lookup <- NULL
      if (show_rd) {
        # READ, never recompute. `$get_estimates()` carries the stored risk
        # difference on the same row as the ratio it belongs to. A failed
        # emulated trial stored a skip envelope, which the accessor reports
        # as absent, and it renders an empty cell.
        rd_lookup <- .tte_rd_lookup(plan, slots$rd, keep_ids)
      }
      img_base <- paste0(base, "_", est)
      out <- file.path(dir, paste0(img_base, ".png"))
      .plan_export_clear_image(out)
      .write_forest_irr(
        openxlsx::createWorkbook(),
        sheet_name = paste0("forest_", est),
        plan = plan,
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
      # `.write_forest_irr()` returns NULL, and writes no image, when no row
      # has a result or the renderer fails.
      .plan_export_require_image(out, spec)
      paths <- c(paths, out)
    }
    return(paths)
  }

  stop("unknown figure type '", spec$type, "'", call. = FALSE)
}


# Produce one TABLE exhibit from a spec; dispatched by $export().
#   "table1": IPW-truncated baseline characteristics for `enrollment`,
#     written as CSV (from the computed results_enrollment table1).
#' Produce one table exhibit
#'
#' `.plan_export()` calls it for each table exhibit of the manifest.
#'
#' @param plan A `TTEPlan`.
#' @param spec One exhibit spec from the manifest.
#' @param dir Character(1), the output directory.
#' @return The written path.
#' @noRd
.plan_export_table <- function(plan, spec, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stem <- spec$label %||% spec$type
  base <- if (!is.null(spec$.index)) {
    sprintf("%02d_%s", spec$.index, stem)
  } else {
    stem
  }

  if (identical(spec$type, "table1")) {
    eid <- spec$enrollment
    if (!eid %in% .plan_analysed_enrollment_ids(plan)) {
      stop(
        "no enrollment results for '",
        eid,
        "'. Run analysis first.",
        call. = FALSE
      )
    }
    baselines <- plan$get_baselines()
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
      stop("no Table 1 available for enrollment '", eid, "'", call. = FALSE)
    }
    out <- file.path(dir, paste0(base, "_", eid, ".csv"))
    # `.baseline_panel()` composes display columns only, so `smd_numeric`
    # never reaches the file.
    data.table::fwrite(tbl, out)
    return(out)
  }

  stop("unknown table type '", spec$type, "'", call. = FALSE)
}
