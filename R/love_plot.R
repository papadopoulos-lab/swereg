# =============================================================================
# Love plot: covariate balance before and after weighting
# =============================================================================
# A Love plot reads one covariate per row and shows the absolute standardised
# mean difference (SMD) between the two arms twice: once unweighted, once
# under the analysis weights. Balance improves when the weighted point sits
# left of the unweighted one, and the conventional acceptance threshold is
# drawn at 0.1.
#
# The two series are the `table1_unweighted` and `table1_ipw_trunc` panels of
# one enrollment. The truncated weights are the analysis weights, so
# `table1_ipw` (untruncated) is deliberately not plotted: it is a robustness
# variant, not what the effect estimates were produced under.
#
# The numbers come from the `smd_numeric` column that `.swereg_table1()`
# emits. That is the unrounded double, not the three-decimal `SMD` display
# string, so a point at 0.0999 does not round onto the threshold line.
# =============================================================================


#' Build the long-format balance table behind the Love plot.
#'
#' One row per covariate per weighting series. Covariates are the rows of the
#' Table 1 panels that carry a non-missing `smd_numeric`: the single row of a
#' continuous variable, and the first level row of a categorical variable.
#' The `" (mean (SD))"` suffix is stripped from continuous variable names so
#' both kinds share one axis vocabulary.
#'
#' Row order is descending unweighted SMD, so the worst-balanced covariate is
#' the top row of the plot. A covariate present in only one panel keeps its
#' row and contributes one point.
#'
#' Two input shapes are accepted, and both name the variable and the
#' standardised mean difference:
#' \itemize{
#'   \item the `$get_baselines()` rows of one panel, with `variable` and
#'     `smd_numeric`. `$export_tables()` passes these.
#'   \item a `swereg_table1` panel, with `Variable` and `smd_numeric`.
#' }
#'
#' A row is a covariate when it carries a non-missing `smd_numeric`.
#' `.swereg_table1()` writes that number on the row that names the variable,
#' and on no other. The test therefore picks the single row of a continuous
#' variable. It picks the first level row of a categorical one.
#'
#' @param t1_unweighted The unweighted panel, in either shape.
#' @param t1_weighted The analysis-weighted panel, in either shape.
#' @param label_unweighted Series label for the unweighted panel.
#' @param label_weighted Series label for the weighted panel.
#' @return A `data.table` with columns `variable` (factor, plot order),
#'   `weighting` (factor, series) and `smd` (double). `NULL` when neither
#'   panel carries a usable `smd_numeric` column.
#' @noRd
.build_love_df <- function(
  t1_unweighted,
  t1_weighted,
  label_unweighted = "Unweighted",
  label_weighted = "IPW truncated"
) {
  variable <- smd <- smd_numeric <- weighting <- NULL # nolint
  sort_key <- NULL # nolint

  one_panel <- function(t1, label) {
    if (is.null(t1) || !"smd_numeric" %in% names(t1)) {
      return(NULL)
    }
    name_col <- if ("Variable" %in% names(t1)) "Variable" else "variable"
    if (!name_col %in% names(t1)) {
      return(NULL)
    }
    dt <- data.table::as.data.table(t1)
    dt <- dt[!is.na(smd_numeric)]
    if (nrow(dt) == 0L) {
      return(NULL)
    }
    return(data.table::data.table(
      variable = sub(" \\(mean \\(SD\\)\\)$", "", as.character(dt[[name_col]])),
      weighting = label,
      smd = as.numeric(dt$smd_numeric)
    ))
  }

  unw <- one_panel(t1_unweighted, label_unweighted)
  wtd <- one_panel(t1_weighted, label_weighted)
  if (is.null(unw) && is.null(wtd)) {
    return(NULL)
  }

  df <- data.table::rbindlist(
    Filter(Negate(is.null), list(unw, wtd)),
    use.names = TRUE
  )

  # Plot order: worst unweighted balance first. A covariate absent from the
  # unweighted panel is keyed on its own largest SMD instead.
  ord_key <- df[,
    list(
      sort_key = {
        u <- smd[weighting == label_unweighted]
        if (length(u) > 0L) u[1L] else max(smd)
      }
    ),
    keyby = variable
  ]
  ordered_vars <- ord_key[order(-sort_key)]$variable

  # scale_y_discrete draws the first factor level at the bottom, so reverse
  # the order to put the worst-balanced covariate at the top.
  df[, variable := factor(variable, levels = rev(ordered_vars))]
  df[,
    weighting := factor(
      weighting,
      levels = c(label_unweighted, label_weighted)
    )
  ]
  return(df[])
}


#' Render a Love plot from the long balance table.
#'
#' Pure renderer: takes the data, returns the plot, touches no workbook and
#' writes no file. Mirrors `.render_combined_forest_plot()` in that respect,
#' but returns the `ggplot` itself rather than a list, because the figure has
#' one panel and its size is a function of the row count alone
#' (see [.love_plot_size]).
#'
#' @param df Output of [.build_love_df].
#' @param title Optional figure title.
#' @return A `ggplot` object.
#' @noRd
.render_love_plot <- function(df, title = NULL) {
  variable <- smd <- weighting <- NULL # nolint

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for Love plots. ",
      "Install with: install.packages('ggplot2')",
      call. = FALSE
    )
  }
  if (is.null(df) || nrow(df) == 0L) {
    stop(
      "Love plot needs at least one covariate with a numeric SMD.",
      call. = FALSE
    )
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = smd, y = variable)
  ) +
    # 0.1 is the conventional threshold for acceptable covariate balance.
    ggplot2::geom_vline(
      xintercept = 0.1,
      linetype = "dashed",
      colour = "grey40",
      linewidth = 0.5
    ) +
    ggplot2::geom_path(
      ggplot2::aes(group = variable),
      colour = "grey75",
      linewidth = 0.4,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = weighting, shape = weighting),
      size = 2.6,
      na.rm = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = c("#B2182B", "#2166AC"),
      drop = FALSE
    ) +
    ggplot2::scale_shape_manual(values = c(16, 17), drop = FALSE) +
    ggplot2::expand_limits(x = 0) +
    ggplot2::labs(
      x = "Absolute standardised mean difference",
      y = NULL,
      colour = NULL,
      shape = NULL,
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = "black", linewidth = 0.5),
      axis.ticks.x = ggplot2::element_line(colour = "black", linewidth = 0.5),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(colour = "black"),
      plot.margin = ggplot2::margin(5, 8, 5, 5)
    )
  return(p)
}


#' Figure size in inches for a Love plot with `n_vars` covariate rows.
#' @noRd
.love_plot_size <- function(n_vars) {
  return(list(
    width = 8,
    height = min(40, max(3.5, 0.3 * n_vars + 1.8))
  ))
}


#' Write the Love plot sheet: title, then the embedded PNG.
#'
#' PNG (300 dpi) and vector PDF sidecars are saved next to the workbook by
#' [.save_plot_sidecars], and the PNG is the `openxlsx::insertImage()` source
#' -- the same arrangement the forest plot sheets use.
#'
#' Takes the two panels rather than the plan so the sheet can be exercised
#' without a full `TTEPlan`.
#'
#' @return The sidecar paths, invisibly, or `NULL` when nothing was plotted.
#' @noRd
.write_love_plot <- function(
  wb,
  sheet_name,
  t1_unweighted,
  t1_weighted,
  title = NULL,
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

  df <- .build_love_df(t1_unweighted, t1_weighted)
  if (is.null(df)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      paste0(
        "No numeric SMDs available for this enrollment. ",
        "Re-run $recompute_baselines() to refresh the cached tables."
      ),
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  rendered <- tryCatch(
    .render_love_plot(df, title = NULL),
    error = function(e) {
      warning("Love plot rendering failed: ", conditionMessage(e), call. = FALSE)
      return(NULL)
    }
  )
  if (is.null(rendered)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "Love plot could not be rendered. See the combined baseline sheet.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  size <- .love_plot_size(data.table::uniqueN(df$variable))
  paths <- .save_plot_sidecars(
    p = rendered,
    width = size$width,
    height = size$height,
    img_dir = img_dir,
    basename = img_basename
  )
  openxlsx::insertImage(
    wb,
    sheet_name,
    paths$png,
    startRow = row_ptr,
    startCol = 1L,
    width = size$width,
    height = size$height,
    units = "in",
    dpi = 300
  )
  return(invisible(paths))
}
