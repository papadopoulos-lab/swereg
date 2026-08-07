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
.build_forest_df <- function(
  plan,
  rates_slot = "rates_pp_trunc",
  irr_slot = "irr_pp_trunc",
  keep_ett_ids = NULL,
  group_labels = NULL
) {
  ett_id <- group_label <- NULL # nolint
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
    if (is.null(r)) {
      return(NULL)
    }
    irr_val <- r[[irr_slot]]
    if (is.null(irr_val) || isTRUE(irr_val$skipped)) {
      return(NULL)
    }
    if (!all(c("IRR", "IRR_lower", "IRR_upper") %in% names(irr_val))) {
      return(NULL)
    }

    enr_id <- r$enrollment_id
    enr_name <- .enrollment_label(plan, enr_id)

    # Pull outcome + follow-up from plan$ett (spec-driven, no age-stripping)
    ett_row <- plan$ett[ett_id == eid][1]
    outcome_name <- if (nrow(ett_row) > 0L) {
      ett_row$outcome_name
    } else {
      NA_character_
    }
    outcome_description <- if (
      nrow(ett_row) > 0L && "outcome_description" %in% names(plan$ett)
    ) {
      ett_row$outcome_description
    } else {
      NA_character_
    }
    outcome_role <- if (
      nrow(ett_row) > 0L && "outcome_role" %in% names(plan$ett)
    ) {
      ett_row$outcome_role
    } else {
      NA_character_
    }
    follow_up <- if (nrow(ett_row) > 0L) {
      as.integer(ett_row$follow_up)
    } else {
      NA_integer_
    }

    # Pull arm names per-enrollment from the spec (fall back to generic)
    arms <- .lookup_arm_labels(plan$spec, enr_id)
    intervention_name <- if (!is.null(arms)) {
      arms[["intervention"]]
    } else {
      "Intervention"
    }
    comparator_name <- if (!is.null(arms)) {
      arms[["comparator"]]
    } else {
      "Comparator"
    }

    rates_val <- r[[rates_slot]]
    events_int <- NA_real_
    py_int <- NA_real_
    rate_int <- NA_real_
    events_cmp <- NA_real_
    py_cmp <- NA_real_
    rate_cmp <- NA_real_
    if (
      !is.null(rates_val) &&
        !isTRUE(rates_val$skipped) &&
        all(
          c("events_weighted", "py_weighted", "rate_per_100000py") %in%
            names(rates_val)
        )
    ) {
      treatment_var <- attr(rates_val, "treatment_var")
      if (!is.null(treatment_var) && treatment_var %in% names(rates_val)) {
        rv <- rates_val
        row_int <- rv[get(treatment_var) == TRUE]
        row_cmp <- rv[get(treatment_var) == FALSE]
        if (nrow(row_int) == 1L) {
          events_int <- row_int$events_weighted
          py_int <- row_int$py_weighted
          rate_int <- row_int$rate_per_100000py
        }
        if (nrow(row_cmp) == 1L) {
          events_cmp <- row_cmp$events_weighted
          py_cmp <- row_cmp$py_weighted
          rate_cmp <- row_cmp$rate_per_100000py
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
      outcome_role = outcome_role,
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
  if (length(rows) == 0L) {
    return(NULL)
  }
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
  if (!is.finite(x)) {
    return(NA_character_)
  }
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
  keys <- c(
    "outcome_name",
    "outcome_description",
    "outcome_role",
    "enrollment_name",
    "enrollment_id",
    "intervention_name",
    "comparator_name",
    "follow_up",
    "ett_id"
  )
  out <- fmt
  for (key in keys) {
    val <- row[[key]]
    if (is.null(val) || (length(val) == 1L && is.na(val))) {
      val <- ""
    }
    out <- gsub(paste0("{", key, "}"), as.character(val), out, fixed = TRUE)
  }
  out
}


#' Format the IRR (95% CI) cell for a single row. Returns a string.
#' @noRd
.ff_irr_ci <- function(irr, lo, hi, irr_lo_bound = 0.01, irr_hi_bound = 100) {
  if (!is.finite(irr)) {
    return("(no estimate)")
  }
  if (irr < irr_lo_bound) {
    # Effectively-zero IRR (e.g. no events in the intervention arm) is not a
    # useful "<0.01" -- leave the cell blank.
    return("")
  }
  if (irr > irr_hi_bound) {
    return(sprintf(">%.0f", irr_hi_bound))
  }
  if (!is.finite(lo) || !is.finite(hi) || lo <= 0 || hi <= 0) {
    return(sprintf("%.2f (no CI)", irr))
  }
  sprintf("%.2f (%.2f to %.2f)", irr, lo, hi)
}


#' Format the signed cause-specific risk-difference cell for a single row.
#'
#' The risk difference is stored as a probability difference and displayed per
#' `per` people, so a stored `-4.88e-04` reads as `-4.88` per 10,000.
#'
#' The value is SIGNED and stays signed. The sign convention is fixed upstream
#' as `RD(t) = Risk_intervention(t) - Risk_comparator(t)`, so a protective
#' intervention gives a negative number, and that minus sign is the result. The
#' sign is written explicitly on the point estimate and on both bounds, so a
#' harm can never be read as an unsigned magnitude.
#'
#' There is no `abs()` anywhere in this function, and there must never be one.
#'
#' A non-finite risk difference renders as an EMPTY string, never as `"NA"` and
#' never as a zero: an empty cell says nothing was computed, whereas either of
#' those says something was.
#'
#' @param rd Numeric, the signed risk difference as a probability difference.
#' @param rd_lo,rd_hi Numeric, the interval bounds on the same scale.
#' @param per Numeric, the display denominator (default 10,000 people).
#' @return A character(1).
#' @noRd
.ff_rd_ci <- function(rd, rd_lo, rd_hi, per = 10000) {
  if (!is.finite(rd)) {
    return("")
  }
  if (!is.finite(rd_lo) || !is.finite(rd_hi)) {
    # Print NO point estimate. The bounds are NA when an arm carries no event
    # through this horizon, and in that case the point estimate is not a
    # contrast either. The weighted product-limit estimate for an event-free arm
    # is `cumprod(1 - 0/D)`, which is exactly 1, so
    #
    #   RD = S_comparator - S_intervention = S_comparator - 1
    #
    # is the comparator's own cumulative incidence with a minus sign. It carries
    # no information from the other arm. Printing it under a risk-difference
    # header invites a reader to quote a one-arm quantity as a two-arm effect.
    return("not estimable")
  }
  point <- sprintf("%+.2f", rd * per)
  sprintf("%s (%+.2f to %+.2f)", point, rd_lo * per, rd_hi * per)
}


#' Format a confidence level as the percentage a column header prints.
#'
#' The header must never state a level the interval was not computed at. A
#' 90 percent interval headed "95 percent CI" is a worse defect than a wrong
#' number, because a wrong number gets questioned and a mislabelled one gets
#' believed.
#'
#' An integer percentage prints with no decimal point (`0.95` gives `"95"`),
#' and a non-integer one keeps only the digits it needs (`0.975` gives
#' `"97.5"`, `0.9973` gives `"99.73"`). The multiplication is rounded to six
#' decimals first, so binary representation cannot leak a trailing `.00000001`
#' into a figure.
#'
#' @param conf_level Numeric(1) strictly between 0 and 1.
#' @return A character(1) percentage, with no percent sign.
#' @noRd
.ff_conf_pct <- function(conf_level) {
  if (
    length(conf_level) != 1L ||
      !is.numeric(conf_level) ||
      !is.finite(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    stop("conf_level must be a single number strictly between 0 and 1")
  }
  format(
    round(conf_level * 100, 6),
    trim = TRUE,
    scientific = FALSE,
    drop0trailing = TRUE
  )
}


#' Resolve the follow-up horizon the column headers state.
#'
#' Three headers name a time, and the distinction between them is the point.
#' The incidence rate ratio is a PERIOD measure over `0` to the horizon. The
#' risk difference is a POINT measure at the horizon. The number needed to
#' treat inverts that risk difference, so it inherits the same instant.
#'
#' One number heads all three columns, so it must be true of every row. This is
#' the sibling of [.forest_rd_conf_level()] and keeps the same contract. Read
#' the value off the rows. Refuse rather than print a horizon that is true of
#' only some of them.
#'
#' The value comes from `follow_up`, the horizon the ETT grid declares. It does
#' NOT come from the last band of a risk-difference curve. A hand-built test
#' panel can stop short of the declared horizon, and the header states what the
#' study followed people for.
#'
#' @param df A data.table as built by [.build_forest_df()], carrying
#'   `follow_up`.
#' @return Numeric(1), the horizon in the units `follow_up` uses (weeks).
#' @noRd
.forest_horizon <- function(df) {
  # Returns NULL rather than raising when no single horizon governs the rows.
  #
  # The invariant is "never print a horizon true of only some rows", and NULL
  # satisfies it: the caller omits the time reference and the headers read as
  # they did before horizons were added. Raising instead would satisfy the same
  # invariant by destroying the figure, which is out of proportion to a missing
  # four-character label.
  #
  # This is not hypothetical. Two of the four production callers, projects 003
  # and 008, call `$export_tables()` with no `featured_etts`, so their rows span
  # 52, 156 and 260 weeks. An earlier version of this function raised, and their
  # forest plots stopped rendering entirely.
  if (!"follow_up" %in% names(df)) {
    return(NULL)
  }
  seen <- unique(as.numeric(df$follow_up))
  seen <- seen[!is.na(seen)]
  if (length(seen) != 1L) {
    return(NULL)
  }
  seen
}


#' Format a follow-up horizon as the number a column header prints.
#'
#' An integer horizon prints with no decimal point (`156` gives `"156"`).
#' @param horizon Numeric(1), as returned by [.forest_horizon()].
#' @return A character(1), with no unit.
#' @noRd
.ff_horizon <- function(horizon) {
  format(horizon, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
}


#' Reduce one `$risk_difference()` curve to the single forest row it feeds.
#'
#' A forest row is one ETT, and an ETT has one follow-up horizon, so the row
#' takes the LAST band of the curve: the risk difference at the end of
#' follow-up. The person counts are already cumulative through that band.
#'
#' The row also carries `conf_level`, read from the curve's OWN `conf_level`
#' attribute rather than from a value passed alongside it. That attribute is
#' set by the computation that produced these bounds, so the level travels with
#' the numbers it belongs to and the renderer can refuse to print a header that
#' contradicts them.
#'
#' @param ett_id Character(1), the ETT the curve belongs to.
#' @param curve A data.table as returned by `TTEEnrollment$risk_difference()`.
#' @param time_var Character(1), the band column name (the design's
#'   `tstop_var`).
#' @return A one-row data.table with the columns
#'   `.render_combined_forest_plot()` expects in `rd_lookup`, plus `band` and
#'   `conf_level`.
#' @noRd
.forest_rd_row <- function(ett_id, curve, time_var) {
  if (is.null(curve) || nrow(curve) == 0L) {
    return(NULL)
  }
  if (!time_var %in% names(curve)) {
    stop("risk difference curve has no '", time_var, "' column")
  }
  i <- which.max(curve[[time_var]])
  cl <- attr(curve, "conf_level", exact = TRUE)
  data.table::data.table(
    ett_id = as.character(ett_id),
    band = curve[[time_var]][i],
    rd = as.numeric(curve$rd[i]),
    rd_lo = as.numeric(curve$rd_lo[i]),
    rd_hi = as.numeric(curve$rd_hi[i]),
    n_persons_with_event_intervention = as.numeric(
      curve$n_persons_with_event_intervention[i]
    ),
    n_persons_with_event_comparator = as.numeric(
      curve$n_persons_with_event_comparator[i]
    ),
    conf_level = if (is.null(cl)) NA_real_ else as.numeric(cl)
  )
}


#' Columns a `rd_lookup` must carry.
#'
#' The figure draws only `rd`, `rd_lo` and `rd_hi`. The two person-count columns
#' stay required all the same. The export path caches these rows onto
#' `plan$results_ett`, and the `PP results` / `ITT results` sheets report the
#' counts from there.
#' @noRd
.FOREST_RD_COLS <- c(
  "ett_id",
  "rd",
  "rd_lo",
  "rd_hi",
  "n_persons_with_event_intervention",
  "n_persons_with_event_comparator"
)


#' Map an `ett_id -> risk difference` lookup onto a vector of ETT ids.
#'
#' Every id gets a cell. An id the lookup does not carry gets an EMPTY one, and
#' so does every id when `rd_lookup` is NULL. Computing the risk difference
#' costs minutes per ETT, so it is opt-in, and a caller that skipped it must
#' still render.
#'
#' The number needed to treat comes from the SAME three numbers as the risk
#' difference, so the two columns can never disagree. A row whose interval does
#' not strictly exclude the null gets an EMPTY number needed to treat, because
#' [.tte_nntb()] returns `NA` there.
#'
#' @param ett_ids Character vector of ETT ids, in row order.
#' @param rd_lookup A data.table carrying `.FOREST_RD_COLS`, or NULL.
#' @return A list of two character vectors, `txt_rd` and `txt_nnt`, each as
#'   long as `ett_ids`.
#' @noRd
.forest_rd_map <- function(ett_ids, rd_lookup) {
  n <- length(ett_ids)
  blank <- rep("", n)
  if (is.null(rd_lookup) || nrow(rd_lookup) == 0L) {
    return(list(txt_rd = blank, txt_nnt = blank))
  }
  missing_cols <- setdiff(.FOREST_RD_COLS, names(rd_lookup))
  if (length(missing_cols) > 0L) {
    stop(
      "rd_lookup is missing column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  hit <- match(as.character(ett_ids), as.character(rd_lookup$ett_id))
  txt_rd <- blank
  txt_nnt <- blank
  ok <- !is.na(hit)
  if (any(ok)) {
    j <- hit[ok]
    txt_rd[ok] <- mapply(
      .ff_rd_ci,
      rd_lookup$rd[j],
      rd_lookup$rd_lo[j],
      rd_lookup$rd_hi[j]
    )
    nn <- .tte_nntb(
      rd_lookup$rd[j],
      rd_lookup$rd_lo[j],
      rd_lookup$rd_hi[j]
    )
    # Pass the bounds, not the point estimate alone. A number needed to treat
    # printed bare reads as precise, and this one is a reciprocal of a bootstrap
    # interval. `.tte_nntb()` returns NA bounds when the interval spans the null
    # or an arm carries no event, and the cell then renders empty.
    txt_nnt[ok] <- .tte_nntb_cell(nn$nntb, nn$nntb_lo, nn$nntb_hi)
  }
  list(txt_rd = txt_rd, txt_nnt = txt_nnt)
}


#' Resolve the confidence level the risk-difference header will state.
#'
#' The header and the interval must come from ONE value. `.forest_rd_row()`
#' copies the level off the curve the bounds were computed on, so a lookup
#' built by the export path carries the truth with it; this function refuses to
#' print anything that contradicts it. A 90 percent interval headed "95 percent
#' CI" is the defect this exists to make impossible, and it is worse than a
#' wrong number: a wrong number gets questioned, a mislabelled one gets
#' believed.
#'
#' A lookup with no `conf_level` column, or one whose values are all missing,
#' falls back to `rd_conf_level`. That is the hand-built case; the production
#' builder always records it.
#'
#' @param rd_lookup A data.table as passed to `.render_combined_forest_plot()`,
#'   or NULL.
#' @param rd_conf_level Numeric(1) strictly between 0 and 1.
#' @return Numeric(1), the level to state in the header.
#' @noRd
.forest_rd_conf_level <- function(rd_lookup, rd_conf_level) {
  .ff_conf_pct(rd_conf_level) # validates rd_conf_level, discards the string
  if (
    is.null(rd_lookup) ||
      nrow(rd_lookup) == 0L ||
      !"conf_level" %in% names(rd_lookup)
  ) {
    return(rd_conf_level)
  }
  seen <- unique(as.numeric(rd_lookup$conf_level))
  seen <- seen[!is.na(seen)]
  if (length(seen) == 0L) {
    return(rd_conf_level)
  }
  if (length(seen) > 1L) {
    stop(
      "rd_lookup mixes confidence levels (",
      paste(seen, collapse = ", "),
      "); one column cannot carry two."
    )
  }
  if (!isTRUE(all.equal(seen, rd_conf_level))) {
    stop(
      "rd_conf_level (",
      rd_conf_level,
      ") disagrees with the level the intervals were computed at (",
      seen,
      "). The header would state a level the numbers do not have."
    )
  }
  seen
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
  plottable <- NULL # nolint
  y_num <- row_type <- group_label <- indent <- NULL # nolint
  outcome_name <- follow_up <- enrollment_name <- NULL # nolint

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for forest plots. ",
      "Install with: install.packages('ggplot2')"
    )
  }

  df <- data.table::copy(df)
  if (!"group_label" %in% names(df)) {
    df[, group_label := NA_character_]
  }

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
        paste0(.ff_num(e, 1), " / ", .ff_num(p, 0))
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
        paste0(.ff_num(e, 1), " / ", .ff_num(p, 0))
      },
      events_comparator,
      py_comparator
    )
  ]
  df[, txt_irr := mapply(.ff_irr_ci, irr, lo, hi)]

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
  over_lbl <- if (is.null(horizon)) "" else paste0(" over ", .ff_horizon(horizon), " wks")
  at_lbl <- if (is.null(horizon)) "" else paste0(" at ", .ff_horizon(horizon), " wks")

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
    layout_rows[[length(layout_rows) + 1L]] <<- row
  }
  data_row <- function(i, grp) {
    list(
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
      hi = df$hi[i]
    )
  }
  blank_row <- function(type, grp, ind, desc) {
    list(
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
      hi = NA_real_
    )
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

  # Plottability for the right-hand visual (only data rows, and only when
  # IRR + CI are finite and within bounds).
  irr_lo_bound <- 0.01
  irr_hi_bound <- 100
  layout_df[,
    plottable := row_type == "data" &
      is.finite(irr) &
      irr >= irr_lo_bound &
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
      paste0("Number needed to treat", if (nzchar(at_lbl)) paste0("\n", sub("^ ", "", at_lbl)) else ""),
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
  list(plot = combined, height = h_in, width = w_in, text = layout_df)
}


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


#' Format a bare "lo to hi" confidence-interval display string. NA when either
#' bound is non-finite.
#' @noRd
.ff_ci_only <- function(lo, hi) {
  if (!is.finite(lo) || !is.finite(hi)) {
    return(NA_character_)
  }
  sprintf("%.2f to %.2f", lo, hi)
}


#' Build one row per ETT carrying the IRR point + CI + p-value for BOTH
#' estimands (per-protocol truncated and intention-to-treat), keyed off the
#' shared label columns from [.build_forest_df]. Used by the "ITT vs PP forest"
#' overlay sheet. Returns NULL when the per-protocol arm has no plottable rows.
#' @noRd
.build_itt_vs_pp_df <- function(
  plan,
  keep_ett_ids = NULL,
  group_labels = NULL
) {
  ett_id <- irr <- lo <- hi <- pvalue <- NULL # nolint
  irr_itt <- lo_itt <- hi_itt <- pvalue_itt <- NULL # nolint
  pp <- .build_forest_df(
    plan,
    "rates_pp_trunc",
    "irr_pp_trunc",
    keep_ett_ids,
    group_labels
  )
  itt <- .build_forest_df(
    plan,
    "rates_itt",
    "irr_itt",
    keep_ett_ids,
    group_labels
  )
  if (is.null(pp)) {
    return(NULL)
  }
  base <- data.table::copy(pp)
  data.table::setnames(
    base,
    c("irr", "lo", "hi", "pvalue"),
    c("irr_pp", "lo_pp", "hi_pp", "pvalue_pp")
  )
  if (!is.null(itt)) {
    iv <- itt[, .(
      ett_id,
      irr_itt = irr,
      lo_itt = lo,
      hi_itt = hi,
      pvalue_itt = pvalue
    )]
    base[
      iv,
      on = "ett_id",
      `:=`(
        irr_itt = i.irr_itt,
        lo_itt = i.lo_itt,
        hi_itt = i.hi_itt,
        pvalue_itt = i.pvalue_itt
      )
    ]
  } else {
    base[, `:=`(
      irr_itt = NA_real_,
      lo_itt = NA_real_,
      hi_itt = NA_real_,
      pvalue_itt = NA_real_
    )]
  }
  base
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

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for forest plots.")
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
  df[, txt_pp := mapply(.ff_irr_ci, irr_pp, lo_pp, hi_pp)]
  df[, txt_itt := mapply(.ff_irr_ci, irr_itt, lo_itt, hi_itt)]

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
    layout_rows[[length(layout_rows) + 1L]] <<- row
  }
  emit_data <- function(i, grp, ind) {
    push_row(list(
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
      hi_itt = df$hi_itt[i]
    ))
  }
  blank_row <- function(type, grp, ind, desc) {
    push_row(list(
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
      hi_itt = NA_real_
    ))
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

  bound_ok <- function(irr, lo, hi) {
    is.finite(irr) &
      irr >= 0.01 &
      irr <= 100 &
      is.finite(lo) &
      is.finite(hi) &
      lo > 0 &
      hi > 0
  }
  dodge <- 0.18
  pp_df <- layout_df[row_type == "data" & bound_ok(irr_pp, lo_pp, hi_pp)]
  itt_df <- layout_df[row_type == "data" & bound_ok(irr_itt, lo_itt, hi_itt)]
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
    x_min <- min(0.5, max(0.01, min(all_irr) * 0.85))
    x_max <- max(2.0, min(100, max(all_irr) * 1.15))
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
    p +
      ggplot2::scale_x_continuous(
        limits = c(-0.02, 1.05),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::scale_y_reverse(limits = c(n_rows + 1, -0.6), breaks = NULL) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(plot.margin = ggplot2::margin(5, 4, 5, 4))
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
  list(plot = combined, height = h_in, width = w_in)
}
