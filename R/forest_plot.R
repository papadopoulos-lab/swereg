# =============================================================================
# Forest plot for IRR results (Table 3)
# =============================================================================
# Builds a long-format data.table from `plan$results_ett[[*]][[rates/irr
# slots]]` and renders it as a two-panel forest plot:
#
#   Left panel  : text table (description, arm events/PY, IRR (CI))
#   Right panel : point + CI visualisation on a log10 x-axis
#
# The two panels are composed with `patchwork` when available; otherwise we
# fall back to the legacy single-panel layout (IRR point + CI with a
# right-hand IRR (CI) text column, no arm-level counts). High-resolution PNG
# and vector PDF sidecars are saved next to the workbook; the same PNG is
# embedded into the worksheet via `openxlsx::insertImage()`.
# =============================================================================

#' Build a long-format data.table for the forest plot from the cached rates
#' and IRR results on a TTEPlan.
#'
#' Each row combines the `$rates()` output for both arms (weighted events,
#' person-years, rate per 100,000 PY) with the `$irr()` output (point
#' estimate + 95% CI + p-value). The rates slot is optional — if missing,
#' the per-arm columns are filled with `NA_real_`.
#'
#' @noRd
.build_forest_df <- function(plan,
                             rates_slot = "rates_pp_trunc",
                             irr_slot = "irr_pp_trunc",
                             keep_ett_ids = NULL,
                             group_labels = NULL) {
  ett_id <- group_label <- NULL  # nolint
  results <- plan$results_ett
  if (!is.null(keep_ett_ids)) {
    keep <- intersect(keep_ett_ids, names(results))
    # Preserve parallel alignment of group_labels with keep_ett_ids
    if (!is.null(group_labels)) {
      keep_mask <- keep_ett_ids %in% names(results)
      keep_ett_ids <- keep_ett_ids[keep_mask]
      group_labels <- group_labels[keep_mask]
    }
  } else {
    keep <- names(results)
  }

  # Build a lookup from ett_id -> group label so we can attach it to each
  # row after the slot-validity filter runs.
  group_lookup <- if (!is.null(keep_ett_ids) && !is.null(group_labels)) {
    setNames(group_labels, keep_ett_ids)
  } else {
    NULL
  }

  rows <- lapply(keep, function(eid) {
    r <- results[[eid]]
    if (is.null(r)) return(NULL)
    irr_val <- r[[irr_slot]]
    if (is.null(irr_val) || isTRUE(irr_val$skipped)) return(NULL)
    if (!all(c("IRR", "IRR_lower", "IRR_upper") %in% names(irr_val))) return(NULL)

    enr_id <- r$enrollment_id
    enr_name <- .enrollment_label(plan, enr_id)

    # Pull outcome + follow-up from plan$ett (spec-driven, no age-stripping)
    ett_row <- plan$ett[ett_id == eid][1]
    outcome_name <- if (nrow(ett_row) > 0L) ett_row$outcome_name else NA_character_
    outcome_description <- if (
      nrow(ett_row) > 0L && "outcome_description" %in% names(plan$ett)
    ) ett_row$outcome_description else NA_character_
    follow_up <- if (nrow(ett_row) > 0L) as.integer(ett_row$follow_up) else NA_integer_

    # Pull arm names per-enrollment from the spec (fall back to generic)
    arms <- .lookup_arm_labels(plan$spec, enr_id)
    intervention_name <- if (!is.null(arms)) arms[["intervention"]] else "Intervention"
    comparator_name <- if (!is.null(arms)) arms[["comparator"]] else "Comparator"

    rates_val <- r[[rates_slot]]
    events_int <- NA_real_; py_int <- NA_real_; rate_int <- NA_real_
    events_cmp <- NA_real_; py_cmp <- NA_real_; rate_cmp <- NA_real_
    if (!is.null(rates_val) && !isTRUE(rates_val$skipped) &&
        all(c("events_weighted", "py_weighted", "rate_per_100000py") %in%
            names(rates_val))) {
      treatment_var <- attr(rates_val, "treatment_var")
      if (!is.null(treatment_var) && treatment_var %in% names(rates_val)) {
        rv <- rates_val
        row_int <- rv[get(treatment_var) == TRUE]
        row_cmp <- rv[get(treatment_var) == FALSE]
        if (nrow(row_int) == 1L) {
          events_int <- row_int$events_weighted
          py_int     <- row_int$py_weighted
          rate_int   <- row_int$rate_per_100000py
        }
        if (nrow(row_cmp) == 1L) {
          events_cmp <- row_cmp$events_weighted
          py_cmp     <- row_cmp$py_weighted
          rate_cmp   <- row_cmp$rate_per_100000py
        }
      }
    }

    grp <- if (!is.null(group_lookup)) group_lookup[[eid]] else NA_character_
    data.table::data.table(
      ett_id = eid,
      enrollment_id = enr_id,
      enrollment_name = enr_name,
      outcome_name = outcome_name,
      outcome_description = outcome_description,
      follow_up = follow_up,
      intervention_name = intervention_name,
      comparator_name = comparator_name,
      group_label = as.character(grp),
      events_intervention = events_int,
      py_intervention = py_int,
      rate_intervention = rate_int,
      events_comparator = events_cmp,
      py_comparator = py_cmp,
      rate_comparator = rate_cmp,
      irr = irr_val$IRR,
      lo = irr_val$IRR_lower,
      hi = irr_val$IRR_upper,
      pvalue = irr_val$IRR_pvalue,
      warn = isTRUE(irr_val$warn)
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(NULL)
  out <- data.table::rbindlist(rows)

  if (!is.null(keep_ett_ids)) {
    order_keep <- intersect(keep_ett_ids, out$ett_id)
    out <- out[match(order_keep, ett_id)]
  }
  out
}


#' Format a numeric value with locale-style thousands separators and
#' appropriate precision. Returns NA for non-finite inputs.
#' @noRd
.ff_num <- function(x, digits = 0L) {
  if (!is.finite(x)) return(NA_character_)
  if (digits == 0L) {
    formatC(round(x), format = "d", big.mark = ",")
  } else {
    formatC(x, format = "f", digits = digits, big.mark = ",")
  }
}


#' Apply a `{placeholder}` format string to a single row of a data.table.
#'
#' Supported placeholders correspond to the columns produced by
#' `.build_forest_df()`: `{outcome_name}`, `{outcome_description}`,
#' `{enrollment_name}`, `{enrollment_id}`, `{intervention_name}`,
#' `{comparator_name}`, `{follow_up}`, `{ett_id}`. Unknown placeholders are
#' left unchanged.
#'
#' @noRd
.forest_format_label <- function(fmt, row) {
  keys <- c("outcome_name", "outcome_description",
            "enrollment_name", "enrollment_id",
            "intervention_name", "comparator_name",
            "follow_up", "ett_id")
  out <- fmt
  for (key in keys) {
    val <- row[[key]]
    if (is.null(val) || (length(val) == 1L && is.na(val))) val <- ""
    out <- gsub(paste0("{", key, "}"), as.character(val), out, fixed = TRUE)
  }
  out
}


#' Format the IRR (95% CI) cell for a single row. Returns a string.
#' @noRd
.ff_irr_ci <- function(irr, lo, hi, irr_lo_bound = 0.01, irr_hi_bound = 100) {
  if (!is.finite(irr)) return("(no estimate)")
  if (irr < irr_lo_bound) return(sprintf("<%.2f", irr_lo_bound))
  if (irr > irr_hi_bound) return(sprintf(">%.0f", irr_hi_bound))
  if (!is.finite(lo) || !is.finite(hi) || lo <= 0 || hi <= 0) {
    return(sprintf("%.2f (no CI)", irr))
  }
  sprintf("%.2f (%.2f to %.2f)", irr, lo, hi)
}


#' Render the combined forest plot: left text panel + right visualisation.
#'
#' Uses `patchwork` to compose two ggplots side by side. The left panel is a
#' text-only ggplot with `theme_void()`, each column at a fixed x position.
#' The right panel is a point + CI plot on a log10 x-axis.
#'
#' @param df data.table from `.build_forest_df()`.
#' @param arm_labels optional named character vector
#'   `c(comparator = "...", intervention = "...")`. When NULL, falls back to
#'   generic "Intervention" / "Comparator".
#' @param title optional figure title (shown above the text panel).
#' @param label_format optional character(1) format string used to build
#'   the row description in the left text panel. Supports `{placeholder}`
#'   tokens: `{outcome_name}`, `{outcome_description}`, `{enrollment_name}`,
#'   `{enrollment_id}`, `{intervention_name}`, `{comparator_name}`,
#'   `{follow_up}`, `{ett_id}`. Defaults: when grouped,
#'   `"{outcome_name} ({follow_up}w)"`; when ungrouped,
#'   `"{enrollment_name} - {outcome_name} ({follow_up}w)"`.
#' @param desc_header optional character(1) header label for the
#'   description column in the left text panel. Defaults to `"ETT"`.
#' @return list(plot, width, height) for `ggsave()`.
#' @noRd
.render_combined_forest_plot <- function(df,
                                         arm_labels = NULL,
                                         title = NULL,
                                         label_format = NULL,
                                         desc_header = NULL) {
  # Local bindings (avoid R CMD check NSE notes)
  enrollment_id <- description <- ett_id <- ett_label <- NULL            # nolint
  events_intervention <- py_intervention <- rate_intervention <- NULL   # nolint
  events_comparator <- py_comparator <- rate_comparator <- NULL         # nolint
  irr <- lo <- hi <- txt_desc <- txt_int <- txt_cmp <- txt_irr <- NULL  # nolint
  plottable <- NULL                                                      # nolint
  y_num <- row_type <- group_label <- NULL                               # nolint
  outcome_name <- follow_up <- enrollment_name <- NULL                   # nolint

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for forest plots. ",
         "Install with: install.packages('ggplot2')")
  }

  df <- data.table::copy(df)
  if (!"group_label" %in% names(df)) df[, group_label := NA_character_]

  # Arm column headers
  intervention_hdr <- if (!is.null(arm_labels) && !is.na(arm_labels[["intervention"]])) {
    arm_labels[["intervention"]]
  } else {
    "Intervention"
  }
  comparator_hdr <- if (!is.null(arm_labels) && !is.na(arm_labels[["comparator"]])) {
    arm_labels[["comparator"]]
  } else {
    "Comparator"
  }

  # Choose the format: explicit user format, or a sensible default based
  # on whether the rows are grouped.
  has_groups_top <- any(!is.na(df$group_label) & nzchar(df$group_label))
  if (is.null(label_format) || !nzchar(label_format)) {
    label_format <- if (has_groups_top) {
      "{outcome_name} ({follow_up}w)"
    } else {
      "{enrollment_name} - {outcome_name} ({follow_up}w)"
    }
  }

  # Row-level text cells
  df[, txt_desc := vapply(
    seq_len(.N),
    function(i) .forest_format_label(label_format, df[i]),
    character(1)
  )]
  df[, txt_int := mapply(function(e, p) {
    if (!is.finite(e) && !is.finite(p)) return("-")
    paste0(.ff_num(e, 1), " / ", .ff_num(p, 0))
  }, events_intervention, py_intervention)]
  df[, txt_cmp := mapply(function(e, p) {
    if (!is.finite(e) && !is.finite(p)) return("-")
    paste0(.ff_num(e, 1), " / ", .ff_num(p, 0))
  }, events_comparator, py_comparator)]
  df[, txt_irr := mapply(.ff_irr_ci, irr, lo, hi)]

  # Interleave group header rows with data rows. Each header occupies its
  # own y-coordinate so the text panel can render a bold label and the
  # forest panel leaves that slot empty.
  has_groups <- any(!is.na(df$group_label) & nzchar(df$group_label))

  layout_rows <- list()
  layout_y <- 0
  push_row <- function(row) {
    layout_y <<- layout_y + 1
    row$y_num <- layout_y
    layout_rows[[length(layout_rows) + 1L]] <<- row
  }

  if (has_groups) {
    current_group <- NA_character_
    for (i in seq_len(nrow(df))) {
      grp <- df$group_label[i]
      if (!is.na(grp) && !identical(grp, current_group)) {
        push_row(list(
          row_type = "header",
          group_label = grp,
          ett_id = NA_character_,
          enrollment_id = NA_character_,
          txt_desc = grp,
          txt_int = "",
          txt_cmp = "",
          txt_irr = "",
          irr = NA_real_,
          lo = NA_real_,
          hi = NA_real_
        ))
        current_group <- grp
      }
      push_row(list(
        row_type = "data",
        group_label = grp,
        ett_id = df$ett_id[i],
        enrollment_id = df$enrollment_id[i],
        txt_desc = df$txt_desc[i],
        txt_int = df$txt_int[i],
        txt_cmp = df$txt_cmp[i],
        txt_irr = df$txt_irr[i],
        irr = df$irr[i],
        lo = df$lo[i],
        hi = df$hi[i]
      ))
    }
  } else {
    for (i in seq_len(nrow(df))) {
      push_row(list(
        row_type = "data",
        group_label = NA_character_,
        ett_id = df$ett_id[i],
        enrollment_id = df$enrollment_id[i],
        txt_desc = df$txt_desc[i],
        txt_int = df$txt_int[i],
        txt_cmp = df$txt_cmp[i],
        txt_irr = df$txt_irr[i],
        irr = df$irr[i],
        lo = df$lo[i],
        hi = df$hi[i]
      ))
    }
  }

  layout_df <- data.table::rbindlist(layout_rows)
  n_rows <- nrow(layout_df)

  # Plottability for the right-hand visual (only data rows, and only when
  # IRR + CI are finite and within bounds).
  irr_lo_bound <- 0.01
  irr_hi_bound <- 100
  layout_df[,
    plottable := row_type == "data" &
      is.finite(irr) & irr >= irr_lo_bound & irr <= irr_hi_bound &
      is.finite(lo) & is.finite(hi) & lo > 0 & hi > 0
  ]
  plot_df <- layout_df[plottable == TRUE]

  # --- right panel (forest visualisation) ---
  if (nrow(plot_df) == 0L) {
    x_breaks <- c(0.5, 1, 2)
    x_min <- 0.5; x_max <- 2
  } else {
    bounds_lo <- min(plot_df$lo, plot_df$irr, na.rm = TRUE)
    bounds_hi <- max(plot_df$hi, plot_df$irr, na.rm = TRUE)
    x_min <- min(0.5, max(irr_lo_bound, bounds_lo * 0.85))
    x_max <- max(2.0, min(irr_hi_bound, bounds_hi * 1.15))
    candidate_breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 10)
    x_breaks <- candidate_breaks[
      candidate_breaks >= x_min & candidate_breaks <= x_max
    ]
    if (length(x_breaks) == 0L) x_breaks <- 1
  }

  p_right <- ggplot2::ggplot(layout_df, ggplot2::aes(y = y_num)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                        colour = "grey50") +
    ggplot2::geom_linerange(
      data = plot_df,
      ggplot2::aes(xmin = lo, xmax = hi),
      linewidth = 0.5, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(x = irr),
      size = 2.5, shape = 15, na.rm = TRUE
    ) +
    ggplot2::scale_x_log10(
      breaks = x_breaks,
      labels = format(x_breaks, drop0trailing = TRUE)
    ) +
    ggplot2::scale_y_reverse(
      limits = c(n_rows + 1, -0.6),
      breaks = NULL
    ) +
    ggplot2::labs(x = "IRR (log scale)", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )

  # --- left panel: four stacked column ggplots ---
  # Using separate ggplots per column (rather than fixed x positions on one
  # plot) lets patchwork allocate relative widths and prevents long
  # descriptions from overlapping the numeric columns.
  header_y <- 0
  text_plot_df <- layout_df[,
    .(y_num, row_type, txt_desc, txt_int, txt_cmp, txt_irr)
  ]
  data_text_df  <- text_plot_df[row_type == "data"]
  group_text_df <- text_plot_df[row_type == "header"]

  text_col <- function(body_label, header_label, hjust_val = 0,
                        is_desc_column = FALSE) {
    p <- ggplot2::ggplot(data_text_df, ggplot2::aes(y = y_num)) +
      # Column header row (bold)
      ggplot2::geom_text(
        data = data.table::data.table(y_num = header_y, h = header_label),
        ggplot2::aes(x = 0, y = y_num, label = h),
        hjust = hjust_val, vjust = 1, size = 3.3, fontface = "bold"
      ) +
      # Data rows
      ggplot2::geom_text(
        ggplot2::aes(x = 0, label = .data[[body_label]]),
        hjust = hjust_val, size = 3.2
      )
    if (is_desc_column && nrow(group_text_df) > 0L) {
      # Group header rows only appear in the description column (leftmost)
      p <- p + ggplot2::geom_text(
        data = group_text_df,
        ggplot2::aes(x = 0, y = y_num, label = txt_desc),
        hjust = hjust_val, size = 3.4, fontface = "bold"
      )
    }
    p +
      ggplot2::scale_x_continuous(
        limits = if (hjust_val == 0) c(-0.02, 1.05) else c(-1.05, 0.02),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::scale_y_reverse(
        limits = c(n_rows + 1, -0.6),
        breaks = NULL
      ) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(plot.margin = ggplot2::margin(5, 4, 5, 4))
  }

  p_desc <- text_col(
    "txt_desc",
    if (is.null(desc_header) || !nzchar(desc_header)) "ETT" else desc_header,
    hjust_val = 0, is_desc_column = TRUE
  )
  p_int  <- text_col("txt_int",
                     paste0(intervention_hdr, "\nevents / PY"),
                     hjust_val = 0)
  p_cmp  <- text_col("txt_cmp",
                     paste0(comparator_hdr, "\nevents / PY"),
                     hjust_val = 0)
  p_irr  <- text_col("txt_irr", "IRR (95% CI)", hjust_val = 0)

  # --- compose with patchwork when available ---
  has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
  if (has_patchwork) {
    # Relative widths: description gets the most, then forest plot, then
    # the numeric columns.
    combined <- patchwork::wrap_plots(
      p_desc, p_int, p_cmp, p_irr, p_right,
      widths = c(4, 1.6, 1.6, 1.5, 3.5),
      nrow = 1
    )
    if (!is.null(title)) {
      combined <- combined +
        patchwork::plot_annotation(title = title,
                                    theme = ggplot2::theme(
                                      plot.title = ggplot2::element_text(
                                        face = "bold", size = 12
                                      )
                                    ))
    }
    w_in <- 16
  } else {
    # Fallback: right panel only. Add y-axis labels back so rows are
    # identifiable.
    combined <- p_right +
      ggplot2::scale_y_reverse(
        limits = c(n_rows + 1, 0),
        breaks = layout_df$y_num,
        labels = layout_df$txt_desc
      ) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 9),
        axis.ticks.y = ggplot2::element_line()
      ) +
      ggplot2::labs(title = title)
    w_in <- 11
  }

  h_in <- min(40, max(4, 0.4 * n_rows + 2))
  list(plot = combined, height = h_in, width = w_in)
}


#' Save a ggplot to a PNG (300 dpi) and a vector PDF, returning both paths.
#' @noRd
.save_plot_sidecars <- function(p, width, height, img_dir, basename) {
  dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
  png_path <- file.path(img_dir, paste0(basename, ".png"))
  pdf_path <- file.path(img_dir, paste0(basename, ".pdf"))
  ggplot2::ggsave(png_path, p, width = width, height = height,
                  dpi = 300, bg = "white", limitsize = FALSE)
  pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
  ggplot2::ggsave(pdf_path, p, width = width, height = height,
                  device = pdf_device, limitsize = FALSE)
  list(png = png_path, pdf = pdf_path)
}


#' Write the Table 3 merged forest plot sheet.
#'
#' Title row + treatment legend + embedded PNG. PNG and PDF sidecars are
#' saved next to the workbook (`img_dir`). The PNG is reused as the
#' `openxlsx::insertImage()` source.
#'
#' @noRd
.write_forest_irr <- function(wb, sheet_name, plan,
                              rates_slot, irr_slot,
                              title = NULL, keep_ett_ids = NULL,
                              group_labels = NULL,
                              label_format = NULL,
                              desc_header = NULL,
                              img_dir, img_basename) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr, cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  legend <- .build_treatment_legend(plan, keep_ett_ids)
  row_ptr <- .write_treatment_legend(wb, sheet_name, legend, row_ptr)

  df <- .build_forest_df(plan,
                         rates_slot = rates_slot,
                         irr_slot = irr_slot,
                         keep_ett_ids = keep_ett_ids,
                         group_labels = group_labels)
  if (is.null(df) || nrow(df) == 0L) {
    openxlsx::writeData(wb, sheet_name, "No valid IRR results to plot.",
                        startRow = row_ptr)
    return(invisible(NULL))
  }

  arm_labels <- .unique_arm_labels(legend)

  rendered <- tryCatch(
    .render_combined_forest_plot(df,
                                 arm_labels = arm_labels,
                                 title = NULL,
                                 label_format = label_format,
                                 desc_header = desc_header),
    error = function(e) {
      warning("Forest plot rendering failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(rendered)) {
    openxlsx::writeData(
      wb, sheet_name,
      "Forest plot could not be rendered. See the supplementary merged table.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  paths <- .save_plot_sidecars(
    p = rendered$plot,
    width = rendered$width,
    height = rendered$height,
    img_dir = img_dir,
    basename = img_basename
  )

  openxlsx::insertImage(
    wb, sheet_name, paths$png,
    startRow = row_ptr, startCol = 1L,
    width = rendered$width,
    height = rendered$height,
    units = "in",
    dpi = 300
  )
  invisible(paths)
}
