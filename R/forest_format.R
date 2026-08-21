# =============================================================================
# Forest plot: the cell and header text the panels print
# =============================================================================
# These functions build the strings the panels print. They also resolve the
# confidence level and the horizon a header states. The design note sits at the
# top of `R/forest_plot.R`.
# =============================================================================

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


#' The drawing window of the forest panel, on the ratio scale.
#'
#' A DISPLAY convention, and not the estimability decision. The panel is a
#' log10 axis of fixed extent. A point outside the window falls off the panel,
#' or compresses every other point to a stripe.
#'
#' The lower edge equals the estimability bound in `.tte_irr_estimable()`
#' today. The two are separate decisions that share a number. One answers "may
#' this ratio be reported at all". The other answers "does this ratio fit on
#' the axis". `.ff_irr_ci()` reads the stored estimability decision, and it
#' reads `.FOREST_IRR_PANEL_RANGE` for the upper display cap only.
#' @noRd
.FOREST_IRR_PANEL_RANGE <- c(lo = 0.01, hi = 100)


#' Format the IRR (95% CI) cell for a single row. Returns a string.
#'
#' Estimability is READ, not re-tested. `$s3_analyze()` calls
#' `.tte_irr_estimable()` beside the ratio and stores the answer, and
#' `$get_estimates()` carries it as `irr_estimable`. A ratio the producer
#' called inestimable renders as an EMPTY cell. An effectively-zero ratio is
#' not a useful `"<0.01"`. An intervention arm with no event gives one.
#'
#' A result cached before that column existed passes `NA`, and
#' `.tte_irr_estimable_stored()` then applies the one shared rule. That is the
#' consumer deriving what the producer did not store, and it keeps the
#' threshold in one function.
#'
#' @param irr,lo,hi Numeric(1), the ratio and its interval bounds.
#' @param irr_estimable Logical(1), the stored decision, or `NA`.
#' @param irr_hi_bound Numeric(1), the upper display cap.
#' @return A character(1).
#' @noRd
.ff_irr_ci <- function(
  irr,
  lo,
  hi,
  irr_estimable = NA,
  irr_hi_bound = .FOREST_IRR_PANEL_RANGE[["hi"]]
) {
  if (!is.finite(irr)) {
    return("(no estimate)")
  }
  if (!.tte_irr_estimable_stored(irr, irr_estimable)) {
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
#' The benefit-or-harm label is READ from `rd_lookup$nnt_direction` and is never
#' rebuilt here. [.tte_rd_curve()] decides it, [.forest_rd_row()] copies it onto
#' the row, and this function passes it to the cell builder. [.tte_nntb()]
#' supplies the magnitude and the interval only, and reports no direction, so
#' this path holds no second decision site.
#'
#' A legacy `rd_lookup` lacking the decision columns renders an EMPTY number
#' needed to treat. See `.FOREST_RD_DECISION_COLS` for why that is the chosen
#' behaviour rather than an error and rather than a re-derivation.
#'
#' @param ett_ids Character vector of ETT ids, in row order.
#' @param rd_lookup A data.table carrying `.FOREST_RD_COLS`, or NULL. The
#'   columns in `.FOREST_RD_DECISION_COLS` MAY be absent. Every other column of
#'   `.FOREST_RD_COLS` is required.
#' @return A list of two character vectors, `txt_rd` and `txt_nnt`, each as
#'   long as `ett_ids`.
#' @noRd
.forest_rd_map <- function(ett_ids, rd_lookup) {
  n <- length(ett_ids)
  blank <- rep("", n)
  if (is.null(rd_lookup) || nrow(rd_lookup) == 0L) {
    return(list(txt_rd = blank, txt_nnt = blank))
  }
  # The decision columns are exempt from the requirement, deliberately, so a
  # lookup cached before they existed still renders. See
  # `.FOREST_RD_DECISION_COLS`.
  required_cols <- setdiff(.FOREST_RD_COLS, .FOREST_RD_DECISION_COLS)
  missing_cols <- setdiff(required_cols, names(rd_lookup))
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
    # Magnitude and interval only. `.tte_nntb()` reports no direction.
    nn <- .tte_nntb(
      rd_lookup$rd[j],
      rd_lookup$rd_lo[j],
      rd_lookup$rd_hi[j]
    )
    # The direction, READ from the lookup. A legacy lookup carries no such
    # column, and every row then gets `NA`, which renders an empty cell. The
    # sign of `rd` is NOT consulted, here or anywhere below.
    nnt_direction <- if ("nnt_direction" %in% names(rd_lookup)) {
      as.character(rd_lookup$nnt_direction[j])
    } else {
      rep(NA_character_, length(j))
    }
    # Pass the bounds, not the point estimate alone. A number needed to treat
    # printed bare reads as precise, and this one is a reciprocal of a bootstrap
    # interval. `.tte_nntb()` returns NA bounds when the interval spans the null
    # or an arm carries no event, and the cell then renders empty.
    txt_nnt[ok] <- .tte_nntb_cell(
      nn$nntb,
      nn$nntb_lo,
      nn$nntb_hi,
      nnt_direction
    )
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


#' Format a bare "lo to hi" confidence-interval display string. NA when either
#' bound is non-finite.
#' @noRd
.ff_ci_only <- function(lo, hi) {
  if (!is.finite(lo) || !is.finite(hi)) {
    return(NA_character_)
  }
  sprintf("%.2f to %.2f", lo, hi)
}
