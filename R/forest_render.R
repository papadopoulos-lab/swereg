# =============================================================================
# Forest plot: the renderers
# =============================================================================
# Both renderers compose a left text panel and a right visualisation into one
# ggplot. The design note sits at the top of `R/forest_plot.R`.
# =============================================================================

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
#'   description column in the left text panel. Defaults to a blank header.
#' @param role_headers optional named character vector mapping an
#'   `outcome_role` value to a sub-header label (e.g.
#'   `c(primary = "Primary outcome", secondary = "Secondary outcomes")`).
#'   When supplied and the rows are grouped (outcomes as rows), a bold-italic
#'   sub-header is inserted within each exposure group whenever the role
#'   changes, and the outcome rows are indented beneath it. NULL (default)
#'   leaves the two-tier exposure/outcome layout untouched.
#' @param rd_lookup optional data.table keyed by `ett_id` and carrying `rd`,
#'   `rd_lo`, `rd_hi`, `n_persons_with_event_intervention` and
#'   `n_persons_with_event_comparator`, one row per ETT that has a risk
#'   difference. When supplied, two extra text columns are composed: the SIGNED
#'   cause-specific risk difference per 10,000 people with its interval, and
#'   the number needed to treat it inverts to. A row whose `ett_id` the lookup
#'   does not carry renders both cells EMPTY. NULL (default) leaves every cell
#'   empty, and the two columns are then left out of the layout entirely rather
#'   than reserving width for a quantity nobody computed. The per-arm
#'   distinct-person event counts are NOT drawn: they are reported on the
#'   `PP results` and `ITT results` workbook sheets instead.
#' @param rd_conf_level numeric(1) strictly between 0 and 1, the confidence
#'   level the risk-difference intervals were computed at. The header states
#'   this level rather than a hard-coded one, so `0.9` prints `90\% CI`.
#'   Defaults to 0.95, which is what `$risk_difference()` defaults to, so a
#'   caller that does not set it is unaffected. When `rd_lookup` carries its
#'   own `conf_level` (every lookup the export path builds does), the two must
#'   agree or this errors: the printed label and the computed interval cannot
#'   be allowed to disagree.
#' @return list(plot, width, height, text) for `ggsave()`. `text` is the
#'   layout table the text panels were built from, one row per rendered line.
#' @noRd
.render_combined_forest_plot <- function(
  df,
  arm_labels = NULL,
  title = NULL,
  label_format = NULL,
  desc_header = NULL,
  role_headers = NULL,
  rd_lookup = NULL,
  rd_conf_level = 0.95
) {
  # Local bindings (avoid R CMD check NSE notes)
  enrollment_id <- description <- ett_id <- ett_label <- NULL # nolint
  events_intervention <- py_intervention <- rate_intervention <- NULL # nolint
  events_comparator <- py_comparator <- rate_comparator <- NULL # nolint
  irr <- lo <- hi <- txt_desc <- txt_int <- txt_cmp <- txt_irr <- NULL # nolint
  txt_rd <- txt_nnt <- NULL # nolint
  plottable <- estimable <- irr_estimable <- NULL # nolint
  y_num <- row_type <- group_label <- indent <- NULL # nolint
  outcome_name <- follow_up <- enrollment_name <- NULL # nolint

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for forest plots. ",
      "Install with: install.packages('ggplot2')",
      call. = FALSE
    )
  }

  df <- data.table::copy(df)
  if (!"group_label" %in% names(df)) {
    df[, group_label := NA_character_]
  }
  # A hand-built panel MAY omit the stored decision. Resolve it once, here, so
  # every reader below reads one column.
  if (!"irr_estimable" %in% names(df)) {
    df[, irr_estimable := NA]
  }
  df[,
    estimable := mapply(.tte_irr_estimable_stored, irr, irr_estimable)
  ]

  # Arm column headers
  intervention_hdr <- if (
    !is.null(arm_labels) && !is.na(arm_labels[["intervention"]])
  ) {
    arm_labels[["intervention"]]
  } else {
    "Intervention"
  }
  comparator_hdr <- if (
    !is.null(arm_labels) && !is.na(arm_labels[["comparator"]])
  ) {
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
  df[,
    txt_desc := vapply(
      seq_len(.N),
      function(i) .forest_format_label(label_format, df[i]),
      character(1)
    )
  ]
  df[,
    txt_int := mapply(
      function(e, p) {
        if (!is.finite(e) && !is.finite(p)) {
          return("-")
        }
        return(paste0(.ff_num(e, 1), " / ", .ff_num(p, 0)))
      },
      events_intervention,
      py_intervention
    )
  ]
  df[,
    txt_cmp := mapply(
      function(e, p) {
        if (!is.finite(e) && !is.finite(p)) {
          return("-")
        }
        return(paste0(.ff_num(e, 1), " / ", .ff_num(p, 0)))
      },
      events_comparator,
      py_comparator
    )
  ]
  df[, txt_irr := mapply(.ff_irr_ci, irr, lo, hi, estimable)]

  # Signed cause-specific risk difference per 10,000 people, with its interval,
  # and the number needed to treat it inverts to. Both cells are empty on a row
  # the lookup does not carry, and on every row when no lookup was supplied.
  # Resolved BEFORE anything is drawn, so a header that would contradict the
  # numbers stops the render instead of shipping.
  rd_level <- .forest_rd_conf_level(rd_lookup, rd_conf_level)
  rd_cells <- .forest_rd_map(df$ett_id, rd_lookup)
  df[, txt_rd := rd_cells$txt_rd]
  df[, txt_nnt := rd_cells$txt_nnt]

  # The horizon the three time-referenced headers state. Derived here, before
  # anything is drawn, and from the rows themselves: a literal would keep
  # printing 156 weeks on a 52-week figure.
  horizon <- .forest_horizon(df)
  # NULL when the rows mix horizons, or carry none. The three time-referenced
  # headers then drop their time reference rather than state one that is true of
  # only some rows. `over_lbl` and `at_lbl` are the fragments they splice in.
  over_lbl <- if (is.null(horizon)) {
    ""
  } else {
    paste0(" over ", .ff_horizon(horizon), " wks")
  }
  at_lbl <- if (is.null(horizon)) {
    ""
  } else {
    paste0(" at ", .ff_horizon(horizon), " wks")
  }

  # Interleave group header rows with data rows. Each header occupies its
  # own y-coordinate so the text panel can render a bold label and the
  # forest panel leaves that slot empty. When `role_headers` is supplied, an
  # extra bold-italic sub-header tier is threaded in whenever the outcome role
  # changes within a group, and data rows indent beneath it (indent = 0
  # everywhere when roles are off, so the untiered layout is unchanged).
  has_groups <- any(!is.na(df$group_label) & nzchar(df$group_label))
  use_roles <- !is.null(role_headers) &&
    length(role_headers) > 0L &&
    "outcome_role" %in% names(df)
  indent_sub <- if (use_roles) 0.03 else 0
  indent_data <- if (use_roles) 0.06 else 0

  layout_rows <- list()
  layout_y <- 0
  push_row <- function(row) {
    layout_y <<- layout_y + 1
    row$y_num <- layout_y
    return(layout_rows[[length(layout_rows) + 1L]] <<- row)
  }
  data_row <- function(i, grp) {
    return(list(
      row_type = "data",
      group_label = grp,
      indent = indent_data,
      ett_id = df$ett_id[i],
      enrollment_id = df$enrollment_id[i],
      txt_desc = df$txt_desc[i],
      txt_int = df$txt_int[i],
      txt_cmp = df$txt_cmp[i],
      txt_rd = df$txt_rd[i],
      txt_nnt = df$txt_nnt[i],
      txt_irr = df$txt_irr[i],
      irr = df$irr[i],
      lo = df$lo[i],
      hi = df$hi[i],
      estimable = df$estimable[i]
    ))
  }
  blank_row <- function(type, grp, ind, desc) {
    return(list(
      row_type = type,
      group_label = grp,
      indent = ind,
      ett_id = NA_character_,
      enrollment_id = NA_character_,
      txt_desc = desc,
      txt_int = "",
      txt_cmp = "",
      txt_rd = "",
      txt_nnt = "",
      txt_irr = "",
      irr = NA_real_,
      lo = NA_real_,
      hi = NA_real_,
      estimable = FALSE
    ))
  }

  if (has_groups) {
    current_group <- NA_character_
    current_role <- NA_character_
    for (i in seq_len(nrow(df))) {
      grp <- df$group_label[i]
      if (!is.na(grp) && !identical(grp, current_group)) {
        push_row(blank_row("header", grp, 0, grp))
        current_group <- grp
        current_role <- NA_character_
      }
      if (use_roles) {
        role_i <- df$outcome_role[i]
        if (
          !is.na(role_i) && nzchar(role_i) && !identical(role_i, current_role)
        ) {
          lbl <- if (role_i %in% names(role_headers)) {
            role_headers[[role_i]]
          } else {
            NA_character_
          }
          if (!is.na(lbl) && nzchar(lbl)) {
            push_row(blank_row("subheader", grp, indent_sub, lbl))
          }
          current_role <- role_i
        }
      }
      push_row(data_row(i, grp))
    }
  } else {
    for (i in seq_len(nrow(df))) {
      row <- data_row(i, NA_character_)
      row$indent <- 0
      push_row(row)
    }
  }

  layout_df <- data.table::rbindlist(layout_rows)
  n_rows <- nrow(layout_df)

  # Plottability for the right-hand visual. A data row is drawn when the
  # producer called the ratio estimable, the ratio fits the panel window, and
  # both interval bounds are finite and positive. Estimability is READ from
  # `estimable`; the window is `.FOREST_IRR_PANEL_RANGE`.
  irr_lo_bound <- .FOREST_IRR_PANEL_RANGE[["lo"]]
  irr_hi_bound <- .FOREST_IRR_PANEL_RANGE[["hi"]]
  layout_df[,
    plottable := row_type == "data" &
      estimable &
      irr <= irr_hi_bound &
      is.finite(lo) &
      is.finite(hi) &
      lo > 0 &
      hi > 0
  ]
  plot_df <- layout_df[plottable == TRUE]

  # --- right panel (forest visualisation) ---
  if (nrow(plot_df) == 0L) {
    x_breaks <- c(0.5, 1, 2)
    x_min <- 0.5
    x_max <- 2
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
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey50"
    ) +
    ggplot2::geom_linerange(
      data = plot_df,
      ggplot2::aes(xmin = lo, xmax = hi),
      linewidth = 0.5,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(x = irr),
      size = 2.5,
      shape = 15,
      na.rm = TRUE
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

  # --- left panel: one stacked column ggplot per text column ---
  # Using separate ggplots per column (rather than fixed x positions on one
  # plot) lets patchwork allocate relative widths and prevents long
  # descriptions from overlapping the numeric columns.
  header_y <- 0
  text_plot_df <- layout_df[,
    .(
      y_num,
      row_type,
      indent,
      txt_desc,
      txt_int,
      txt_cmp,
      txt_rd,
      txt_nnt,
      txt_irr
    )
  ]
  data_text_df <- text_plot_df[row_type == "data"]
  group_text_df <- text_plot_df[row_type == "header"]
  sub_text_df <- text_plot_df[row_type == "subheader"]

  text_col <- function(
    body_label,
    header_label,
    hjust_val = 0,
    is_desc_column = FALSE
  ) {
    .data <- h <- NULL # nolint
    # Only the description column indents its body text (to reveal the
    # exposure -> role -> outcome hierarchy); numeric columns stay flush.
    body_geom <- if (is_desc_column) {
      ggplot2::geom_text(
        ggplot2::aes(x = indent, label = .data[[body_label]]),
        hjust = hjust_val,
        size = 3.2
      )
    } else {
      ggplot2::geom_text(
        ggplot2::aes(x = 0, label = .data[[body_label]]),
        hjust = hjust_val,
        size = 3.2
      )
    }
    p <- ggplot2::ggplot(data_text_df, ggplot2::aes(y = y_num)) +
      # Column header row (bold)
      ggplot2::geom_text(
        data = data.table::data.table(y_num = header_y, h = header_label),
        ggplot2::aes(x = 0, y = y_num, label = h),
        hjust = hjust_val,
        vjust = 1,
        size = 3.3,
        fontface = "bold"
      ) +
      # Data rows
      body_geom
    if (is_desc_column && nrow(group_text_df) > 0L) {
      # Group (exposure) header rows only appear in the description column
      p <- p +
        ggplot2::geom_text(
          data = group_text_df,
          ggplot2::aes(x = indent, y = y_num, label = txt_desc),
          hjust = hjust_val,
          size = 3.4,
          fontface = "bold"
        )
    }
    if (is_desc_column && nrow(sub_text_df) > 0L) {
      # Role sub-header rows (e.g. "Primary outcome") sit between the exposure
      # header and its outcome rows, indented and bold-italic.
      p <- p +
        ggplot2::geom_text(
          data = sub_text_df,
          ggplot2::aes(x = indent, y = y_num, label = txt_desc),
          hjust = hjust_val,
          size = 3.2,
          fontface = "bold.italic"
        )
    }
    return(p +
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
      ggplot2::theme(plot.margin = ggplot2::margin(5, 4, 5, 4)))
  }

  p_desc <- text_col(
    "txt_desc",
    if (is.null(desc_header) || !nzchar(desc_header)) "" else desc_header,
    hjust_val = 0,
    is_desc_column = TRUE
  )
  # "weighted events" is not decoration. This column is
  # `sum(event * weight)` over event ROWS, so one woman who carries the outcome
  # in two of her sequential trials is counted twice and each time by her
  # weight. `PY` already names the exposure measure, so these two headers take
  # no time reference: five repetitions of the horizon would be noise.
  p_int <- text_col(
    "txt_int",
    paste0(intervention_hdr, "\nweighted events / PY"),
    hjust_val = 0
  )
  p_cmp <- text_col(
    "txt_cmp",
    paste0(comparator_hdr, "\nweighted events / PY"),
    hjust_val = 0
  )
  # `over` marks a PERIOD measure. The incidence rate ratio covers the whole
  # follow-up, where the two columns to its left are read AT one instant. The
  # confidence level here IS a literal, and honestly so: $irr() takes no
  # confidence level and .fit_irr() uses a hard-coded 1.96 multiplier.
  p_irr <- text_col(
    "txt_irr",
    paste0("IRR", over_lbl, "\n(95% CI)"),
    hjust_val = 0
  )

  # The risk-difference pair is composed only when something populated it.
  # A header over a column of empty cells claims a quantity that was never
  # computed.
  has_rd <- any(nzchar(data_text_df$txt_rd))
  if (has_rd) {
    # `at` marks a POINT measure. Both the confidence level and the horizon are
    # built from the rows the intervals were computed on, never from a literal.
    p_rd <- text_col(
      "txt_rd",
      paste0(
        "Risk difference per 10,000\n",
        sub("^ ", "", at_lbl),
        if (nzchar(at_lbl)) " " else "",
        "(",
        .ff_conf_pct(rd_level),
        "% CI)"
      ),
      hjust_val = 0
    )
    p_nnt <- text_col(
      "txt_nnt",
      paste0(
        "Number needed to treat",
        if (nzchar(at_lbl)) paste0("\n", sub("^ ", "", at_lbl)) else ""
      ),
      hjust_val = 0
    )
  }

  # --- compose with patchwork when available ---
  has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
  if (has_patchwork) {
    # The column ORDER is the reading order of the figure: what each arm
    # contributed, then the absolute difference between them, then how many
    # people that difference corresponds to, then the ratio, then the picture.
    # The two absolute measures sit together, and the IRR closes the text block
    # against the panel that draws it.
    #
    # Relative widths: description gets the most, then the forest panel, then
    # the numeric columns. Each width holds its own longest header line. The
    # IRR column is wider than it was because its header now names the horizon.
    cols <- list(p_desc, p_int, p_cmp)
    col_widths <- c(4, 1.6, 1.6)
    if (has_rd) {
      cols <- c(cols, list(p_rd, p_nnt))
      col_widths <- c(col_widths, 2.6, 2)
    }
    cols <- c(cols, list(p_irr, p_right))
    col_widths <- c(col_widths, 1.9, 3.5)
    combined <- patchwork::wrap_plots(
      cols,
      widths = col_widths,
      nrow = 1
    )
    if (!is.null(title)) {
      combined <- combined +
        patchwork::plot_annotation(
          title = title,
          theme = ggplot2::theme(
            plot.title = ggplot2::element_text(
              face = "bold",
              size = 12
            )
          )
        )
    }
    # Two more text columns need canvas, or they take it from the description
    # and the forest panel. Both figures are the total relative width times the
    # inches-per-unit the layout used before this column set changed, so no
    # column is narrower in inches than it was.
    w_in <- if (has_rd) 19.5 else 16.5
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
  return(list(plot = combined, height = h_in, width = w_in, text = layout_df))
}


#' Render the ITT-vs-PP overlay forest plot: intention-to-treat (blue triangles)
#' and per-protocol (red squares) IRR points + CIs, dodged vertically on
#' each outcome row. Left text panels show each estimand's IRR (95% CI) display
#' string (coloured to match), ITT first. Mirrors the layout of
#' [.render_combined_forest_plot] but with two series.
#' @noRd
.render_itt_vs_pp_overlay <- function(
  df,
  title = NULL,
  label_format = NULL,
  desc_header = NULL,
  role_headers = NULL,
  pp_col = "#C0392B",
  itt_col = "#2C5AA0"
) {
  y_num <- row_type <- group_label <- txt_desc <- txt_pp <- txt_itt <- NULL # nolint
  irr_pp <- lo_pp <- hi_pp <- irr_itt <- lo_itt <- hi_itt <- y_plot <- NULL # nolint
  outcome_name <- follow_up <- enrollment_name <- indent <- NULL # nolint
  irr_estimable_pp <- irr_estimable_itt <- NULL # nolint
  estimable_pp <- estimable_itt <- NULL # nolint

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for forest plots.", call. = FALSE)
  }
  df <- data.table::copy(df)
  if (!"group_label" %in% names(df)) {
    df[, group_label := NA_character_]
  }
  has_groups <- any(!is.na(df$group_label) & nzchar(df$group_label))
  if (is.null(label_format) || !nzchar(label_format)) {
    label_format <- if (has_groups) {
      "{outcome_name} ({follow_up}w)"
    } else {
      "{enrollment_name} - {outcome_name} ({follow_up}w)"
    }
  }
  df[,
    txt_desc := vapply(
      seq_len(.N),
      function(i) .forest_format_label(label_format, df[i]),
      character(1)
    )
  ]
  # Estimability is READ from the stored column, once per estimand. A panel
  # built by hand MAY omit it, and the shared rule then answers.
  for (nm in c("irr_estimable_pp", "irr_estimable_itt")) {
    if (!nm %in% names(df)) {
      data.table::set(df, j = nm, value = NA)
    }
  }
  df[,
    estimable_pp := mapply(.tte_irr_estimable_stored, irr_pp, irr_estimable_pp)
  ]
  df[,
    estimable_itt := mapply(
      .tte_irr_estimable_stored,
      irr_itt,
      irr_estimable_itt
    )
  ]
  df[, txt_pp := mapply(.ff_irr_ci, irr_pp, lo_pp, hi_pp, estimable_pp)]
  df[, txt_itt := mapply(.ff_irr_ci, irr_itt, lo_itt, hi_itt, estimable_itt)]

  # Optional role sub-headers, mirroring .render_combined_forest_plot: opt-in
  # via `role_headers`; indent = 0 everywhere when off, so the untiered overlay
  # is unchanged.
  use_roles <- !is.null(role_headers) &&
    length(role_headers) > 0L &&
    "outcome_role" %in% names(df)
  indent_sub <- if (use_roles) 0.03 else 0
  indent_data <- if (use_roles) 0.06 else 0

  layout_rows <- list()
  layout_y <- 0
  push_row <- function(row) {
    layout_y <<- layout_y + 1
    row$y_num <- layout_y
    return(layout_rows[[length(layout_rows) + 1L]] <<- row)
  }
  emit_data <- function(i, grp, ind) {
    return(push_row(list(
      row_type = "data",
      group_label = grp,
      indent = ind,
      ett_id = df$ett_id[i],
      txt_desc = df$txt_desc[i],
      txt_pp = df$txt_pp[i],
      txt_itt = df$txt_itt[i],
      irr_pp = df$irr_pp[i],
      lo_pp = df$lo_pp[i],
      hi_pp = df$hi_pp[i],
      irr_itt = df$irr_itt[i],
      lo_itt = df$lo_itt[i],
      hi_itt = df$hi_itt[i],
      estimable_pp = df$estimable_pp[i],
      estimable_itt = df$estimable_itt[i]
    )))
  }
  blank_row <- function(type, grp, ind, desc) {
    return(push_row(list(
      row_type = type,
      group_label = grp,
      indent = ind,
      ett_id = NA_character_,
      txt_desc = desc,
      txt_pp = "",
      txt_itt = "",
      irr_pp = NA_real_,
      lo_pp = NA_real_,
      hi_pp = NA_real_,
      irr_itt = NA_real_,
      lo_itt = NA_real_,
      hi_itt = NA_real_,
      estimable_pp = FALSE,
      estimable_itt = FALSE
    )))
  }
  if (has_groups) {
    current_group <- NA_character_
    current_role <- NA_character_
    for (i in seq_len(nrow(df))) {
      grp <- df$group_label[i]
      if (!is.na(grp) && !identical(grp, current_group)) {
        blank_row("header", grp, 0, grp)
        current_group <- grp
        current_role <- NA_character_
      }
      if (use_roles) {
        role_i <- df$outcome_role[i]
        if (
          !is.na(role_i) && nzchar(role_i) && !identical(role_i, current_role)
        ) {
          lbl <- if (role_i %in% names(role_headers)) {
            role_headers[[role_i]]
          } else {
            NA_character_
          }
          if (!is.na(lbl) && nzchar(lbl)) {
            blank_row("subheader", grp, indent_sub, lbl)
          }
          current_role <- role_i
        }
      }
      emit_data(i, grp, indent_data)
    }
  } else {
    for (i in seq_len(nrow(df))) {
      emit_data(i, NA_character_, 0)
    }
  }
  layout_df <- data.table::rbindlist(layout_rows)
  n_rows <- nrow(layout_df)

  # Estimability is READ; the panel window is `.FOREST_IRR_PANEL_RANGE`.
  bound_ok <- function(estimable, irr, lo, hi) {
    return(estimable &
      irr <= .FOREST_IRR_PANEL_RANGE[["hi"]] &
      is.finite(lo) &
      is.finite(hi) &
      lo > 0 &
      hi > 0)
  }
  dodge <- 0.18
  pp_df <- layout_df[
    row_type == "data" & bound_ok(estimable_pp, irr_pp, lo_pp, hi_pp)
  ]
  itt_df <- layout_df[
    row_type == "data" & bound_ok(estimable_itt, irr_itt, lo_itt, hi_itt)
  ]
  # ITT is "first": upper point in each dodged pair (y_num - dodge sits higher
  # under scale_y_reverse), matching ITT being the left-hand text column.
  itt_df[, y_plot := y_num - dodge]
  pp_df[, y_plot := y_num + dodge]

  all_irr <- c(
    pp_df$lo_pp,
    pp_df$hi_pp,
    pp_df$irr_pp,
    itt_df$lo_itt,
    itt_df$hi_itt,
    itt_df$irr_itt
  )
  all_irr <- all_irr[is.finite(all_irr) & all_irr > 0]
  if (length(all_irr) == 0L) {
    x_min <- 0.5
    x_max <- 2
    x_breaks <- c(0.5, 1, 2)
  } else {
    x_min <- min(
      0.5,
      max(.FOREST_IRR_PANEL_RANGE[["lo"]], min(all_irr) * 0.85)
    )
    x_max <- max(
      2.0,
      min(.FOREST_IRR_PANEL_RANGE[["hi"]], max(all_irr) * 1.15)
    )
    cand <- c(0.1, 0.25, 0.5, 1, 2, 4, 10)
    x_breaks <- cand[cand >= x_min & cand <= x_max]
    if (length(x_breaks) == 0L) x_breaks <- 1
  }

  p_right <- ggplot2::ggplot(layout_df, ggplot2::aes(y = y_num)) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey50"
    ) +
    ggplot2::geom_linerange(
      data = pp_df,
      ggplot2::aes(y = y_plot, xmin = lo_pp, xmax = hi_pp),
      colour = pp_col,
      linewidth = 0.5,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = pp_df,
      ggplot2::aes(y = y_plot, x = irr_pp),
      colour = pp_col,
      size = 2.3,
      shape = 15,
      na.rm = TRUE
    ) +
    ggplot2::geom_linerange(
      data = itt_df,
      ggplot2::aes(y = y_plot, xmin = lo_itt, xmax = hi_itt),
      colour = itt_col,
      linewidth = 0.5,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = itt_df,
      ggplot2::aes(y = y_plot, x = irr_itt),
      colour = itt_col,
      size = 2.3,
      shape = 17,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_log10(
      breaks = x_breaks,
      labels = format(x_breaks, drop0trailing = TRUE)
    ) +
    ggplot2::scale_y_reverse(limits = c(n_rows + 1, -0.6), breaks = NULL) +
    ggplot2::labs(x = "IRR (log scale)", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )

  header_y <- 0
  text_df <- layout_df[, .(y_num, row_type, indent, txt_desc, txt_pp, txt_itt)]
  data_text <- text_df[row_type == "data"]
  group_text <- text_df[row_type == "header"]
  sub_text <- text_df[row_type == "subheader"]
  text_col <- function(body, header, colour = "black", is_desc = FALSE) {
    .data <- h <- NULL # nolint
    body_geom <- if (is_desc) {
      ggplot2::geom_text(
        ggplot2::aes(x = indent, label = .data[[body]]),
        hjust = 0,
        size = 3.2,
        colour = colour
      )
    } else {
      ggplot2::geom_text(
        ggplot2::aes(x = 0, label = .data[[body]]),
        hjust = 0,
        size = 3.2,
        colour = colour
      )
    }
    p <- ggplot2::ggplot(data_text, ggplot2::aes(y = y_num)) +
      ggplot2::geom_text(
        data = data.table::data.table(y_num = header_y, h = header),
        ggplot2::aes(x = 0, y = y_num, label = h),
        hjust = 0,
        vjust = 1,
        size = 3.3,
        fontface = "bold"
      ) +
      body_geom
    if (is_desc && nrow(group_text) > 0L) {
      p <- p +
        ggplot2::geom_text(
          data = group_text,
          ggplot2::aes(x = indent, y = y_num, label = txt_desc),
          hjust = 0,
          size = 3.4,
          fontface = "bold"
        )
    }
    if (is_desc && nrow(sub_text) > 0L) {
      p <- p +
        ggplot2::geom_text(
          data = sub_text,
          ggplot2::aes(x = indent, y = y_num, label = txt_desc),
          hjust = 0,
          size = 3.2,
          fontface = "bold.italic"
        )
    }
    return(p +
      ggplot2::scale_x_continuous(
        limits = c(-0.02, 1.05),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::scale_y_reverse(limits = c(n_rows + 1, -0.6), breaks = NULL) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(plot.margin = ggplot2::margin(5, 4, 5, 4)))
  }
  p_desc <- text_col(
    "txt_desc",
    if (is.null(desc_header) || !nzchar(desc_header)) "" else desc_header,
    colour = "black",
    is_desc = TRUE
  )
  p_pp <- text_col("txt_pp", "PP IRR (95% CI)", colour = pp_col)
  p_itt <- text_col("txt_itt", "ITT IRR (95% CI)", colour = itt_col)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- patchwork::wrap_plots(
      p_desc,
      p_itt,
      p_pp,
      p_right,
      widths = c(4, 2.4, 2.4, 4),
      nrow = 1
    )
    if (!is.null(title)) {
      combined <- combined +
        patchwork::plot_annotation(
          title = title,
          theme = ggplot2::theme(
            plot.title = ggplot2::element_text(
              face = "bold",
              size = 12
            )
          )
        )
    }
    w_in <- 15
  } else {
    combined <- p_right + ggplot2::labs(title = title)
    w_in <- 11
  }
  h_in <- min(40, max(4, 0.4 * n_rows + 2))
  return(list(plot = combined, height = h_in, width = w_in))
}
