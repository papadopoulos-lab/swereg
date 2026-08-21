# =============================================================================
# Forest plot for IRR results (Table 3)
# =============================================================================
# Builds a long-format data.table from `plan$get_estimates()` and renders it as
# a two-panel forest plot:
#
#   Left panel  : text table (description, arm events/PY, IRR (CI))
#   Right panel : point + CI visualisation on a log10 x-axis
#
# The two panels are composed with `patchwork` when available; otherwise we
# fall back to the legacy single-panel layout (IRR point + CI with a
# right-hand IRR (CI) text column, no arm-level counts). High-resolution PNG
# and vector PDF sidecars are saved next to the workbook; the same PNG is
# embedded into the worksheet via `openxlsx::insertImage()`.
#
# No function in this file reads `plan$results_ett`. The accessor is the one
# route to a stored result, and it returns the same numbers under fixed column
# names, so a slot rename cannot reach a figure.
# =============================================================================

#' Save a ggplot to a PNG (300 dpi) and a vector PDF, returning both paths.
#' @noRd
.save_plot_sidecars <- function(p, width, height, img_dir, basename) {
  dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
  png_path <- file.path(img_dir, paste0(basename, ".png"))
  pdf_path <- file.path(img_dir, paste0(basename, ".pdf"))
  ggplot2::ggsave(
    png_path,
    p,
    width = width,
    height = height,
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )
  pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
  ggplot2::ggsave(
    pdf_path,
    p,
    width = width,
    height = height,
    device = pdf_device,
    limitsize = FALSE
  )
  list(png = png_path, pdf = pdf_path)
}


#' Write the Table 3 merged forest plot sheet.
#'
#' Title row + treatment legend + embedded PNG. PNG and PDF sidecars are
#' saved next to the workbook (`img_dir`). The PNG is reused as the
#' `openxlsx::insertImage()` source.
#'
#' `rd_lookup` and `rd_conf_level` are passed straight through to
#' [.render_combined_forest_plot()]; a NULL `rd_lookup` (default) renders no
#' risk-difference columns.
#'
#' @noRd
.write_forest_irr <- function(
  wb,
  sheet_name,
  plan,
  rates_slot,
  irr_slot,
  title = NULL,
  keep_ett_ids = NULL,
  group_labels = NULL,
  label_format = NULL,
  desc_header = NULL,
  role_headers = NULL,
  rd_lookup = NULL,
  rd_conf_level = 0.95,
  img_dir,
  img_basename
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

  df <- .build_forest_df(
    plan,
    rates_slot = rates_slot,
    irr_slot = irr_slot,
    keep_ett_ids = keep_ett_ids,
    group_labels = group_labels
  )
  if (is.null(df) || nrow(df) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid IRR results to plot.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  arm_labels <- .unique_arm_labels(legend)

  rendered <- tryCatch(
    .render_combined_forest_plot(
      df,
      arm_labels = arm_labels,
      title = NULL,
      label_format = label_format,
      desc_header = desc_header,
      role_headers = role_headers,
      rd_lookup = rd_lookup,
      rd_conf_level = rd_conf_level
    ),
    error = function(e) {
      warning("Forest plot rendering failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(rendered)) {
    openxlsx::writeData(
      wb,
      sheet_name,
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
    wb,
    sheet_name,
    paths$png,
    startRow = row_ptr,
    startCol = 1L,
    width = rendered$width,
    height = rendered$height,
    units = "in",
    dpi = 300
  )
  invisible(paths)
}
