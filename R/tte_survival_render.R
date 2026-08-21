# Rendering one weighted discrete-time survival curve: its risk-table break
# times, its two arm labels, and the figure itself.

#' Pick the band times a risk table labels.
#'
#' The panel can hold one band per follow-up week, and a risk table with
#' fifty-two columns is unreadable. This thins the observed band times down to
#' at most `max_n` of them.
#'
#' The chosen times are ALSO the x breaks of both panels, so every labelled
#' tick has a count under it and every count sits on a tick.
#'
#' The selection counts BACKWARDS from the last band, in steps of one fixed
#' stride. Every gap between adjacent chosen bands is therefore the same
#' number of bands wide, and the last band is always chosen.
#'
#' The direction is the whole point, and it is a defect fix. Counting forwards
#' from the first band and then adding the last one leaves a short final gap of
#' `(n - 1) %% stride` bands. On a real 156-week national-registry panel that
#' gap was 12 weeks against a 20-week stride. Two six-digit counts at adjacent
#' labelled weeks then printed on top of each other, as one unreadable
#' ten-digit run. Counting backwards cannot produce a short gap at either end,
#' because the leftover bands are dropped rather than labelled.
#'
#' Do not fix a collision by shrinking the font instead. The figure is a
#' publication artefact, and a smaller font trades one legibility problem for
#' another.
#'
#' @param times Numeric, the sorted unique band times present in the curve.
#' @param max_n Integer, the most columns the table may carry.
#' @return A numeric subset of `times`, always including the last element.
#' @noRd
.risk_table_break_times <- function(times, max_n = 8L) {
  n <- length(times)
  if (n <= max_n) {
    return(times)
  }
  stride <- ceiling((n - 1L) / (max_n - 1L))
  times[rev(seq(n, 1L, by = -stride))]
}

#' Resolve the two arm labels a survival figure prints.
#'
#' The ONE place the package decides what an unnamed arm is called.
#' `$survival_curve()` and the export path both draw the same figure, so both
#' MUST reach the same two strings. Two copies of the fallback could print
#' `"Intervention"` on one route and a study label on the other.
#'
#' A label that is `NULL`, missing or empty takes the generic word.
#'
#' @param arm_labels A named character vector or list carrying `intervention`
#'   and `comparator`, as `.lookup_arm_labels()` returns it, or `NULL`.
#' @return A named character(2), `intervention` and `comparator`.
#' @noRd
.tte_arm_labels_resolved <- function(arm_labels) {
  one <- function(key, fallback) {
    v <- if (is.null(arm_labels)) NULL else arm_labels[[key]]
    if (is.null(v) || is.na(v) || !nzchar(as.character(v))) {
      fallback
    } else {
      as.character(v)
    }
  }
  c(
    intervention = one("intervention", "Intervention"),
    comparator = one("comparator", "Comparator")
  )
}

#' Render one weighted discrete-time survival curve, with numbers at risk.
#'
#' Pure renderer: it takes the curve `$survival_curve()` already computed,
#' returns a `ggplot`, and writes nothing. Splitting it out of the R6 method
#' lets the two y scales share one code path, so the survival figure and the
#' cumulative-failure figure cannot drift apart.
#'
#' `scale = "cumulative_failure"` plots `1 - surv`. Deaths are censored, not
#' modelled as a competing risk, so that quantity is cause-specific failure
#' under independent censoring and NOT a competing-risk cumulative incidence
#' function. The y label says exactly that.
#'
#' A numbers-at-risk table is drawn beneath the curve panel. It is populated
#' from `n_persons_at_risk`, the count of DISTINCT PERSONS, and never from
#' `at_risk`, which is the weighted risk set `sum(w)` and is the hazard
#' denominator. The two differ on every real panel, because the weights are not
#' 1 and because one person holds several sequential trials. A risk table
#' reports people.
#'
#' Both panels are given the SAME x breaks and the SAME x limits. A risk table
#' whose columns do not sit under the curve's ticks is worse than no risk table
#' at all, so the shared scale is the point of the composition, not a detail of
#' it.
#'
#' @param curve A data.table carrying `time_var`, `surv`, `group` and
#'   `n_persons_at_risk` columns, as built by `$survival_curve()`.
#' @param time_var Character, name of the time column in `curve`.
#' @param scale `"survival"` (default, plots `surv`) or `"cumulative_failure"`
#'   (plots `1 - surv`, starting at 0).
#' @param title Character or NULL. Plot title, left-aligned to the whole plot.
#' @param subtitle Character or NULL. Plot subtitle under the title.
#' @param ylim Numeric length-2 or NULL, passed to `coord_cartesian()`.
#' @param int_lab Legend label for the intervention arm (red, listed first).
#' @param cmp_lab Legend label for the comparator arm (blue).
#' @return A `patchwork` object: the curve panel over the numbers-at-risk
#'   table. It also inherits `ggplot`, and the curve is the composition's own
#'   plot, so `ggplot2::layer_data()` and `ggplot2::get_labs()` applied to the
#'   returned object describe the CURVE.
#' @noRd
.render_survival_curve <- function(
  curve,
  time_var,
  scale = c("survival", "cumulative_failure"),
  title = NULL,
  subtitle = NULL,
  ylim = NULL,
  int_lab = "Intervention",
  cmp_lab = "Comparator"
) {
  surv <- group <- plot_y <- arm_row <- n_at_risk <- tt <- NULL # nolint
  .data <- NULL # nolint

  scale <- match.arg(scale)
  cumulative <- identical(scale, "cumulative_failure")

  if (!"n_persons_at_risk" %in% names(curve)) {
    stop(
      "curve must carry 'n_persons_at_risk' to draw the numbers-at-risk table"
    )
  }

  # Prepend S(0) = 1 per present arm so each step curve starts at full
  # survival rather than mid-air at the first observed period.
  origin <- data.table::data.table(
    tmp_time = 0L,
    surv = 1,
    group = unique(curve$group)
  )
  data.table::setnames(origin, "tmp_time", time_var)
  pd <- data.table::rbindlist(
    list(origin, curve[, c(time_var, "surv", "group"), with = FALSE]),
    use.names = TRUE
  )

  # Transform AFTER the origin row is bound in, so the origin is converted
  # with everything else. An untransformed origin would start a
  # cumulative-failure curve at 1 and send it downwards -- plausible on
  # screen, and completely wrong.
  pd[, plot_y := if (cumulative) 1 - surv else surv]

  y_lab <- if (cumulative) {
    "Weighted cause-specific cumulative failure"
  } else {
    "Weighted probability of event-free survival"
  }

  # One x scale, built once and given to BOTH panels. Sharing the object is
  # what makes the table's columns land under the curve's ticks; two
  # separately-specified scales drift the moment either side is edited.
  times <- sort(unique(curve[[time_var]]))
  x_breaks <- .risk_table_break_times(times)
  x_limits <- range(c(0, times))
  x_scale <- function() {
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      expand = ggplot2::expansion(mult = 0.05)
    )
  }

  p_curve <- ggplot2::ggplot(
    pd,
    ggplot2::aes(x = .data[[time_var]], y = plot_y, color = group)
  ) +
    ggplot2::geom_step(linewidth = 1) +
    ggplot2::scale_color_manual(
      values = stats::setNames(c("blue", "red"), c(cmp_lab, int_lab)),
      breaks = c(int_lab, cmp_lab)
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    x_scale() +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Time (weeks)",
      y = y_lab,
      color = NULL
    ) +
    ggplot2::theme_minimal() +
    # Left-align title/subtitle to the whole plot (incl. the y-axis label
    # region), not just the panel.
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      # The x axis is drawn once, under the risk table at the bottom of the
      # composition.
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  # PERSONS, not the weighted risk set. `at_risk` is sum(w) and is the hazard
  # denominator; `n_persons_at_risk` is uniqueN(person_id). Populating the
  # table from `at_risk` is the plausible wrong turn and would print weights
  # where a reader expects a head count.
  arm_present <- unique(curve$group)
  arm_levels <- rev(intersect(c(int_lab, cmp_lab), arm_present))
  at_risk_tbl <- data.table::data.table(
    tt = curve[[time_var]],
    arm_row = factor(curve$group, levels = arm_levels),
    n_at_risk = curve$n_persons_at_risk
  )[tt %in% x_breaks]

  p_table <- ggplot2::ggplot(
    at_risk_tbl,
    ggplot2::aes(x = tt, y = arm_row, label = n_at_risk)
  ) +
    ggplot2::geom_text(size = 3.2, colour = "black") +
    x_scale() +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::labs(
      title = "Numbers at risk (persons)",
      x = "Time (weeks)",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(hjust = 0, size = ggplot2::rel(0.9)),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title.x = ggplot2::element_text(colour = "black"),
      axis.ticks.x = ggplot2::element_line(colour = "black", linewidth = 0.6),
      axis.ticks.length.x = ggplot2::unit(3.5, "pt")
    )

  # The table is passed FIRST and the design string puts the curve (B) in the
  # top row. patchwork makes the LAST plot the composition's own ggplot, and
  # the curve has to be it: every existing caller treats the return value as
  # the curve, so `layer_data()` and `get_labs()` on it must still describe the
  # curve and not the risk table.
  patchwork::wrap_plots(
    p_table,
    p_curve,
    design = "B\nA",
    heights = c(4, 1),
    guides = "collect"
  )
}
