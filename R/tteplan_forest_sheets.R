# These writers compare estimates side by side. They produce the ITT vs PP
# forest sheet, the effect-modification sheet, and the combined IRR sheet.

#' Write the "ITT vs PP forest" sheet: a numeric head-to-head table (real
#' `ITT IRR` / `PP IRR` columns + CIs + p-values) on top, and the two-colour
#' overlay forest plot (blue intention-to-treat, red per-protocol) embedded
#' below. Plot colours live only in the figure; the table cells are plain
#' numbers.
#' @noRd
.write_itt_vs_pp_forest <- function(
  wb,
  sheet_name,
  plan,
  keep_ett_ids = NULL,
  group_labels = NULL,
  title = NULL,
  label_format = NULL,
  desc_header = NULL,
  role_headers = NULL,
  img_dir,
  img_basename
) {
  outcome_name <- group_label <- follow_up <- NULL # nolint
  irr_pp <- lo_pp <- hi_pp <- pvalue_pp <- NULL # nolint
  irr_itt <- lo_itt <- hi_itt <- pvalue_itt <- NULL # nolint

  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  df <- .build_itt_vs_pp_df(plan, keep_ett_ids, group_labels)
  if (is.null(df) || nrow(df) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid IRR results to plot.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  tab <- df[, .(
    Comparison = group_label,
    Outcome = outcome_name,
    `Follow-up (weeks)` = follow_up,
    `ITT IRR` = irr_itt,
    `ITT 95% CI` = mapply(.ff_ci_only, lo_itt, hi_itt),
    `ITT p` = pvalue_itt,
    `PP IRR` = irr_pp,
    `PP 95% CI` = mapply(.ff_ci_only, lo_pp, hi_pp),
    `PP p` = pvalue_pp
  )]
  openxlsx::writeData(
    wb,
    sheet_name,
    tab,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  tab_rows <- (row_ptr + 1L):(row_ptr + nrow(tab))
  st_irr <- openxlsx::createStyle(numFmt = "0.00")
  st_p <- openxlsx::createStyle(numFmt = "[<0.001]\"<0.001\";0.000")
  for (cc in c(4L, 7L)) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      st_irr,
      rows = tab_rows,
      cols = cc,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  for (cc in c(6L, 9L)) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      st_p,
      rows = tab_rows,
      cols = cc,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = 1:9,
    widths = c(34, 30, 14, 10, 16, 10, 10, 16, 10)
  )

  plot_row <- row_ptr + nrow(tab) + 2L
  rendered <- tryCatch(
    .render_itt_vs_pp_overlay(
      df,
      title = NULL,
      label_format = label_format,
      desc_header = desc_header,
      role_headers = role_headers
    ),
    error = function(e) {
      warning(
        "ITT vs PP overlay rendering failed: ",
        conditionMessage(e),
        call. = FALSE
      )
      return(NULL)
    }
  )
  if (is.null(rendered)) {
    return(invisible(NULL))
  }
  paths <- .save_plot_sidecars(
    rendered$plot,
    rendered$width,
    rendered$height,
    img_dir,
    img_basename
  )
  openxlsx::insertImage(
    wb,
    sheet_name,
    paths$png,
    startRow = plot_row,
    startCol = 1L,
    width = rendered$width,
    height = rendered$height,
    units = "in",
    dpi = 300
  )
  return(invisible(paths))
}


#' Write an "Effect modification" sheet: per ETT x subgroup, the stratum IRRs
#' (per-protocol and intention-to-treat side by side) and the interaction-test
#' p-value / ratio of stratum IRRs.
#'
#' Reads `$get_subgroups()`, which returns the union of the two stored slot
#' families and reports a skipped result as absent.
#'
#' The sheet iterates the SPECIFICATION, `plan$ett$subgroup_vars`, and
#' `$get_subgroups()` iterates what was stored. A variable the specification
#' names and no worker stored therefore gets no accessor row, and this function
#' emits the one all-`NA` row it always did. That row is the consumer's, and
#' the accessor invents nothing.
#' @noRd
.write_effect_modification <- function(wb, sheet_name, plan, title = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  ett <- plan$ett
  if (is.null(ett) || nrow(ett) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No ETTs to report.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  sg <- plan$get_subgroups()
  analysed <- .plan_analysed_ett_ids(plan)

  # `which()` runs OUTSIDE the data.table subset, so `want_estimand` is the
  # argument. Inside `sg[...]` it would resolve to the COLUMN of that name and
  # the filter would keep every estimand.
  slot_rows <- function(eid, sv, want_estimand) {
    if (nrow(sg) == 0L) {
      return(sg)
    }
    hit <- which(
      sg$ett_id == eid &
        sg$subgroup_var == sv &
        sg$estimand == want_estimand
    )
    return(sg[hit])
  }
  # `strata_stored` is the stored SHAPE: the plan holds a stratified table for
  # this subgroup variable and estimand. A row without it is the accessor's
  # INTERACTION-ONLY row, which stands for a stored interaction test with no
  # stored stratified table, and it names no stratum.
  #
  # The test is on the shape and never on the numbers. A stored stratum whose
  # rate ratio is inestimable keeps its level, so a per-protocol level that
  # could not be computed never removes the intention-to-treat result beside
  # it.
  strata_levels <- function(rows) {
    if (nrow(rows) == 0L) {
      return(character(0))
    }
    return(as.character(rows$subgroup_level)[which(rows$strata_stored)])
  }
  irr_cell <- function(rows, lvl) {
    hit <- which(as.character(rows$subgroup_level) == lvl)
    if (nrow(rows) == 0L || length(hit) == 0L) {
      return(list(irr = NA_real_, ci = NA_character_))
    }
    rr <- rows[hit[1L]]
    return(list(
      irr = rr$irr,
      ci = if (is.na(rr$irr)) {
        NA_character_
      } else {
        sprintf("(%.2f, %.2f)", rr$irr_lo, rr$irr_hi)
      }
    ))
  }
  em_val <- function(rows, field) {
    if (nrow(rows) == 0L) {
      return(NA_real_)
    }
    return(as.numeric(rows[[field]][1L]))
  }

  rows <- list()
  for (i in seq_len(nrow(ett))) {
    eid <- ett$ett_id[i]
    if (!eid %in% analysed) {
      next
    }
    sg_vars <- if (
      "subgroup_vars" %in% names(ett) && !is.null(ett$subgroup_vars[[i]])
    ) {
      ett$subgroup_vars[[i]]
    } else {
      character(0)
    }
    for (sv in sg_vars) {
      pp <- slot_rows(eid, sv, "pp")
      itt <- slot_rows(eid, sv, "itt")
      pp_levels <- strata_levels(pp)
      itt_levels <- strata_levels(itt)
      levels <- if (length(pp_levels) > 0L) {
        pp_levels
      } else if (length(itt_levels) > 0L) {
        itt_levels
      } else {
        "all"
      }
      for (lvl in levels) {
        pc <- irr_cell(pp, lvl)
        ic <- irr_cell(itt, lvl)
        is_all <- identical(lvl, "all")
        rows[[length(rows) + 1L]] <- data.frame(
          Enrollment = eid,
          Outcome = ett$outcome_name[i],
          Subgroup = sv,
          Level = as.character(lvl),
          `PP IRR` = pc$irr,
          `PP 95% CI` = pc$ci,
          `ITT IRR` = ic$irr,
          `ITT 95% CI` = ic$ci,
          `EM p (PP)` = if (is_all) em_val(pp, "em_pvalue") else NA_real_,
          `EM ratio (PP)` = if (is_all) {
            em_val(pp, "ratio_of_irrs")
          } else {
            NA_real_
          },
          `EM p (ITT)` = if (is_all) em_val(itt, "em_pvalue") else NA_real_,
          `EM ratio (ITT)` = if (is_all) {
            em_val(itt, "ratio_of_irrs")
          } else {
            NA_real_
          },
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No subgroups configured (add a top-level `subgroups:` block to the spec).",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  df <- do.call(rbind, rows)
  openxlsx::writeData(
    wb,
    sheet_name,
    df,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_len(ncol(df)),
    widths = "auto"
  )
  return(invisible(NULL))
}


#' @noRd
.write_combined_irr <- function(
  wb,
  sheet_name,
  plan,
  slot,
  title = NULL,
  keep_ett_ids = NULL
) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  legend <- .build_treatment_legend(plan, keep_ett_ids)
  row_ptr <- .write_treatment_legend(wb, sheet_name, legend, row_ptr)

  prep <- .prepare_combine_data(plan, slot, keep_ett_ids = keep_ett_ids)
  if (is.null(prep)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid IRR results.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }
  dt <- tryCatch(
    tteenrollment_irr_combine(prep$wrapped, slot, prep$ett_desc),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  return(openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  ))
}
