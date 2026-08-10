# =============================================================================
# TTEDesign + TTEEnrollment R6 classes, constructors, and helpers
# =============================================================================
# This file contains the two enrollment-side R6 classes and standalone helpers:
#
#   1. TTEDesign R6 class
#   2. TTEEnrollment R6 class (weight/matching/collapse logic in private methods)
#   3. summary.TTEEnrollment S3 method
#   4. tteenrollment_rbind(), tteenrollment_rates_combine(),
#      tteenrollment_irr_combine(), tteenrollment_impute_confounders()
# =============================================================================

.TTE_DESIGN_SCHEMA_VERSION <- 2L
.TTE_ENROLLMENT_SCHEMA_VERSION <- 2L


#' Assign trial IDs from isoyearweek using period_width
#'
#' Single source of truth for the isoyearweek -> trial_id mapping. Used by
#' `.s1_eligible_tuples()` (s1a scout) and `enroll()` Phase A (s1b full enrollment).
#'
#' @param data A data.table with an `isoyearweek` column. Modified by reference.
#' @param period_width Integer, band width in weeks.
#' @return Invisible data, with `trial_id` column added.
#' @noRd
.assign_trial_ids <- function(data, period_width) {
  . <- isoyearweek <- .tte_week_index <- trial_id <- i.trial_id <- NULL
  cstime_weeks <- cstime::dates_by_isoyearweek[, .(isoyearweek)]
  cstime_weeks[, .tte_week_index := .I]
  cstime_weeks[, trial_id := (.tte_week_index - 1L) %/% period_width]
  data[cstime_weeks, trial_id := i.trial_id, on = "isoyearweek"]
  invisible(data)
}

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

#' Draw one person-level (cluster) bootstrap row index
#'
#' A person contributes several sequential trials, and every row belonging to
#' one person is one block. The block is the resampling unit: `n` persons are
#' drawn with replacement from the `n` distinct persons, and a drawn person
#' brings ALL of her rows, as many times as she was drawn. Rows are never drawn
#' individually, because person-trials from one woman share her baseline
#' covariates and can carry the same outcome event, so they are not
#' exchangeable.
#'
#' @param person A vector of person labels, one element per row of the table
#'   being resampled. Rows sharing a label form one block.
#' @return An integer vector of row positions into `person`. Its length varies
#'   between replicates, because the blocks are unequal.
#' @noRd
.boot_person_index <- function(person) {
  f <- if (is.factor(person)) person else factor(person)
  np <- nlevels(f)
  if (np == 0L) {
    return(integer(0))
  }
  codes <- as.integer(f)
  ord <- order(codes, method = "radix")
  len <- tabulate(codes, nbins = np)
  start <- cumsum(c(1L, len))[seq_len(np)]
  draw <- sample.int(np, np, replace = TRUE)
  ord[sequence(len[draw], from = start[draw])]
}

# How many bootstrap replicates the risk-difference estimator multiplies at
# once. The replicates go through the arm matrices in groups of this many rows,
# so each product is one level-3 BLAS call. One replicate at a time is a
# level-2 call, and the estimator makes two of them per replicate. Measured at
# 500 replicates on a national-registry panel, the grouped form runs 3.1 times
# faster. The arithmetic is memory-bandwidth bound, so this is the lever that
# works.
#
# The value is fixed here and MUST NOT become an argument. Sizes of 50, 100,
# 250 and 500 are within 1 percent of each other on speed. A size of 500 holds
# ten times the multiplicity buffer for no gain. A reachable size would let a
# performance setting move a published confidence interval.
.RD_BOOT_BATCH <- 50L

#' Arm survival for a batch of bootstrap multiplicity rows
#'
#' The weighted hazard of one arm, accumulated over the bands, for every
#' replicate in one batch at once.
#'
#' @param mult An integer matrix. One row per replicate, one column per
#'   person-trial. Row `i` is the multiplicity vector of replicate `i`.
#' @param mats The `num` and `den` matrix pair of one arm. Each is
#'   `n_person_trial` rows by `n_band` columns.
#' @return A numeric matrix. One row per replicate, one column per band. Row
#'   `i` is the survival curve of replicate `i`.
#' @noRd
.rd_surv_batch <- function(mult, mats) {
  numerator <- mult %*% mats$num
  denominator <- mult %*% mats$den
  # A replicate can draw no person for an arm, or empty one band. That is a
  # missing survival, not a zero and not an error; cumprod carries it forward
  # and the percentile step drops it. The rule stays per element, so a batch
  # gives the missing pattern that one replicate at a time gives.
  denominator[!is.finite(denominator) | denominator <= 0] <- NA_real_
  surv <- 1 - numerator / denominator
  # R's own cumprod, one row at a time. It accumulates in long double, so a
  # hand-written column recurrence in double precision would return other bits.
  for (i in seq_len(nrow(surv))) {
    surv[i, ] <- cumprod(surv[i, ])
  }
  surv
}

#' Does an interval strictly exclude the null?
#'
#' The ONE place the package answers that question. `.tte_rd_curve()` uses it to
#' set `interval_status`, and `.tte_nntb()` uses it to guard the reciprocal.
#' Two copies of this test could drift apart, and a figure and a results sheet
#' would then disagree about the same interval.
#'
#' The test is STRICT. A bound of exactly zero touches the null, so the interval
#' does not exclude it. `>=` or `<=` here is a one-character change that reports
#' an interval compatible with no effect as if it excluded no effect.
#'
#' @param rd_lo,rd_hi Numeric bounds of the risk-difference interval, of the
#'   same length. `NA` on either bound means there is no interval to judge.
#' @return A logical vector, `TRUE` where the interval strictly excludes zero.
#' @noRd
.tte_excludes_null <- function(rd_lo, rd_hi) {
  rd_lo <- as.numeric(rd_lo)
  rd_hi <- as.numeric(rd_hi)
  !is.na(rd_lo) &
    !is.na(rd_hi) &
    ((rd_lo > 0 & rd_hi > 0) | (rd_lo < 0 & rd_hi < 0))
}

#' The number needed to treat and its direction, decided once
#'
#' The ONE place a signed risk difference becomes a benefit-or-harm decision.
#' The decision is DATA. `.tte_rd_curve()` stores both returned columns on every
#' band, and every formatter reads `nnt_direction` rather than the sign of a
#' number. A formatter that re-derived the direction could disagree with the
#' formatter beside it, and nothing would report the disagreement.
#'
#' Sign convention, fixed by `.tte_rd_curve()`:
#' `RD(t) = S_comparator(t) - S_intervention(t)`. So a protective intervention
#' gives a negative risk difference, and `-1/rd` is then positive. The value
#' stays signed. `abs()` has no place in this arithmetic, because a magnitude
#' that lost its sign cannot separate benefit from harm.
#'
#' A risk difference of exactly zero has no reciprocal and no direction. Both
#' columns are `NA` there, and so are they for a missing risk difference.
#'
#' @param rd Numeric, the signed cause-specific risk difference.
#' @return A data.table with one row per element of `rd`. Column `nnt` is the
#'   signed number needed to treat, `-1/rd`. Column `nnt_direction` is
#'   `"benefit"`, `"harm"` or `NA_character_`.
#' @noRd
.tte_nnt_from_rd <- function(rd) {
  rd <- as.numeric(rd)
  n <- length(rd)
  usable <- is.finite(rd) & rd != 0

  nnt <- rep(NA_real_, n)
  nnt[usable] <- -1 / rd[usable]

  # The decision, made once, from the risk difference itself. A protective
  # intervention lowers the risk, so its risk difference is negative.
  nnt_direction <- rep(NA_character_, n)
  nnt_direction[usable & rd < 0] <- "benefit"
  nnt_direction[usable & rd > 0] <- "harm"

  data.table::data.table(nnt = nnt, nnt_direction = nnt_direction)
}

#' Cause-specific risk difference with a person-level percentile bootstrap
#'
#' The computation behind `TTEEnrollment$risk_difference()`. Kept separate so a
#' test can drive it directly and ask for the multiplicity vectors it applied.
#'
#' Sign convention, fixed:
#' `RD(t) = Risk_intervention(t) - Risk_comparator(t)`
#' `     = [1 - S_intervention(t)] - [1 - S_comparator(t)]`
#' `     = S_comparator(t) - S_intervention(t)`
#' The stored value is signed. A protective intervention gives a negative risk
#' difference and that minus sign is the result, not a nuisance.
#'
#' Performance. The weighted hazard is `sum(w * event) / sum(w)` over the rows
#' at risk, and both sums decompose additively over persons. So the panel is
#' aggregated ONCE to one number pair per person-trial-band, laid out as two
#' dense `n_person_trial x n_band` matrices per arm. A batch of `.RD_BOOT_BATCH`
#' replicates is then a single matrix product against their multiplicity matrix.
#' Resampling the panel itself costs about a hundred times more per replicate
#' and returns the same numbers.
#' The matrix row is the person-trial rather than the person only because the
#' bootstrap index is taken over the person-trial table; the multiplicity of a
#' person is carried by every one of her person-trials, so the product is the
#' person-level sum written out term by term.
#'
#' One multiplicity vector serves BOTH arms. Persons cross arms: a woman can be
#' a comparator in an early trial and an initiator in a later one. Drawing a
#' separate resample per arm leaves the point estimate unbiased and the variance
#' estimator biased, because it throws away the covariance between the two arms'
#' survival estimates. No point estimate can show that, so the shared vector is
#' the invariant, not an implementation detail.
#'
#' A zero-event arm gets NO interval. When either arm has no positive-weight
#' event through a horizon, `rd_lo` and `rd_hi` are `NA` at that horizon and
#' `interval_status` reads `"zero-event arm"`. An ordinary empirical bootstrap
#' cannot produce an event the sample does not hold, so every replicate assigns
#' that arm a failure risk of exactly zero. The percentiles then describe the
#' other arm alone, which is anti-conservative, and more replicates do not
#' repair it. The point estimate is kept, because it stays a valid descriptive
#' quantity.
#'
#' The condition is evaluated per horizon and per arm, on the events up to and
#' including that band. An arm can have no event by week 52 and several by
#' week 156, and the week-156 interval is then estimable.
#'
#' An interval that CONTAINS the null is a third state, and it is named. A band
#' whose interval is estimable but does not strictly exclude zero reads
#' `"spans null"`. The number needed to treat has no interval there, because
#' `x -> -1/x` is undefined across zero. The old code left that band on `"ok"`
#' and made the reason visible only as an empty cell on a figure.
#'
#' The benefit-or-harm decision is stored, not re-derived. `nnt` holds the
#' signed number needed to treat and `nnt_direction` holds the decision.
#' `.tte_nnt_from_rd()` computes both beside `rd`, from the same numbers.
#' Every formatter reads `nnt_direction`, so a figure and a results sheet
#' cannot reach opposite conclusions about one band.
#'
#' The INTERVAL of the number needed to treat is stored beside the decision.
#' `nnt_lo` and `nnt_hi` come from `.tte_nntb()`, which is the one site that
#' maps a risk-difference interval onto the reciprocal scale. A consumer reads
#' the two columns and never inverts `rd_lo` and `rd_hi` itself.
#'
#' Both bounds are `NA` on a band whose interval does not strictly exclude the
#' null, because `x -> -1/x` is undefined across zero. `interval_status` reads
#' `"spans null"` on exactly those bands, so the `NA` has a stated reason. The
#' point estimate `nnt` stays finite there, and a formatter that prints an
#' interval MUST print nothing rather than the point estimate alone.
#'
#' The head count of people at risk is stored per arm per band, as
#' `n_persons_at_risk_comparator` and `n_persons_at_risk_intervention`. It is
#' `uniqueN()` over the person identifier, the same count `$survival_curve()`
#' returns under the name `n_persons_at_risk`. It is neither the row count,
#' which counts person-trials, nor `sum(w)`, which is the weighted risk set and
#' the denominator of the hazard. A numbers-at-risk row reports people, so it
#' cannot be derived from survival or from any other weighted quantity.
#'
#' @param data A data.table at trial level, one row per person-trial-band.
#' @param person_id_var Character, the person identifier column (the cluster).
#' @param id_var Character, the person-trial identifier column.
#' @param treatment_var Character, the baseline arm column (logical or 0/1).
#' @param time_var Character, the band column.
#' @param weight_col Character, the weight column (time-varying allowed).
#' @param n_boot Integer, number of bootstrap replicates.
#' @param conf_level Numeric in (0, 1), the percentile interval level.
#' @param keep_mult Logical. When TRUE, the multiplicity vector applied to each
#'   arm is recorded and attached as the `mult_intervention` and
#'   `mult_comparator` attributes, one row per replicate. Verification only:
#'   the two matrices are `n_boot x n_person_trial` and are large on real data.
#' @return A data.table, one row per band. The `interval_status` column takes
#'   one of three values.
#'   \itemize{
#'     \item `"ok"`. The bootstrap interval is estimable and strictly excludes
#'       the null.
#'     \item `"spans null"`. The interval is estimable and contains the null.
#'     \item `"zero-event arm"`. An arm has no positive-weight event through
#'       that horizon, so there is no interval.
#'   }
#'   The `nnt` column holds the signed number needed to treat, `-1/rd`. The
#'   `nnt_lo` and `nnt_hi` columns hold its interval, as `.tte_nntb()` returns
#'   it, and both are `NA` unless the risk-difference interval strictly
#'   excludes the null. The `nnt_direction` column holds the stored decision. It
#'   reads `"benefit"`, `"harm"` or `NA_character_`.
#'   The `n_persons_at_risk_comparator` and `n_persons_at_risk_intervention`
#'   columns hold the distinct-person head count of each arm in that band.
#'   Attributes: `rd_boot` (the `n_boot x n_band` replicate matrix the
#'   percentiles were read off), `conf_level`, `n_boot`, `swereg_type`.
#' @noRd
.tte_rd_curve <- function(
  data,
  person_id_var,
  id_var,
  treatment_var,
  time_var,
  weight_col,
  n_boot = 500L,
  conf_level = 0.95,
  keep_mult = FALSE
) {
  . <- arm <- pt <- band <- num <- den <- first_band <- N <- NULL # nolint
  person <- n_persons <- NULL # nolint

  needed <- c(person_id_var, id_var, treatment_var, time_var, weight_col)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) {
    stop("column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  if (!"event" %in% names(data)) {
    stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
  }

  w <- data[[weight_col]]
  if (!is.numeric(w) || anyNA(w) || any(!is.finite(w)) || any(w < 0)) {
    stop(
      "weight_col '",
      weight_col,
      "' must be numeric, finite, non-missing and non-negative"
    )
  }
  ev <- data[["event"]]
  if (anyNA(ev) || !all(ev %in% c(0L, 1L))) {
    stop("'event' must be a non-missing 0/1 indicator")
  }
  if (
    length(n_boot) != 1L ||
      !is.numeric(n_boot) ||
      is.na(n_boot) ||
      n_boot < 1 ||
      n_boot != as.integer(n_boot)
  ) {
    stop("n_boot must be a positive integer")
  }
  n_boot <- as.integer(n_boot)
  if (
    length(conf_level) != 1L ||
      !is.numeric(conf_level) ||
      is.na(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    stop("conf_level must be a single number strictly between 0 and 1")
  }

  tv <- data[[treatment_var]]
  if (anyNA(tv)) {
    stop("treatment_var '", treatment_var, "' must not be missing")
  }
  if (!is.logical(tv)) {
    if (!all(tv %in% c(0L, 1L))) {
      stop(
        "risk_difference() requires a logical (or 0/1) '",
        treatment_var,
        "'; got class '",
        class(tv)[1],
        "'"
      )
    }
    tv <- as.logical(tv)
  }
  if (!any(tv) || !any(!tv)) {
    stop("both arms must be present in '", treatment_var, "'")
  }

  # The person-trial is the matrix row; the person is the resampling unit.
  pt_f <- factor(data[[id_var]])
  pt_code <- as.integer(pt_f)
  n_pt <- nlevels(pt_f)
  person_raw <- as.character(data[[person_id_var]])
  # Factored ONCE, deliberately, because it is the loop-invariant part of the
  # draw. Measured on a large national-registry panel, `factor()` over the
  # character person labels costs 3.5 s; left inside the replicate loop that is
  # half an hour per ETT at 500 replicates, against a 0.09 s budget for the
  # whole replicate.
  pt_person <- factor(person_raw[match(seq_len(n_pt), pt_code)])
  if (
    nrow(unique(data.table::data.table(pt = pt_code, person = person_raw))) !=
      n_pt
  ) {
    stop(
      "each '",
      id_var,
      "' must map to exactly one '",
      person_id_var,
      "'"
    )
  }

  band_vals <- sort(unique(data[[time_var]]))
  n_band <- length(band_vals)
  band_code <- match(data[[time_var]], band_vals)

  # Aggregate ONCE. Both sums are additive over persons, so a person-level
  # resample only needs these totals, never the panel rows again.
  agg <- data.table::data.table(
    arm = tv,
    pt = pt_code,
    band = band_code,
    num = as.numeric(w) * as.numeric(ev),
    den = as.numeric(w)
  )
  agg <- agg[, .(num = sum(num), den = sum(den)), keyby = .(arm, pt, band)]

  arm_mats <- function(sub) {
    mn <- matrix(0, nrow = n_pt, ncol = n_band)
    md <- matrix(0, nrow = n_pt, ncol = n_band)
    ij <- cbind(sub$pt, sub$band)
    mn[ij] <- sub$num
    md[ij] <- sub$den
    list(num = mn, den = md)
  }
  m_int <- arm_mats(agg[arm == TRUE])
  m_cmp <- arm_mats(agg[arm == FALSE])

  mult_store <- if (isTRUE(keep_mult)) {
    list(
      intervention = matrix(0L, nrow = n_boot, ncol = n_pt),
      comparator = matrix(0L, nrow = n_boot, ncol = n_pt)
    )
  } else {
    NULL
  }

  # Recorded at the point of application, so what a test reads back is the
  # vector this arm was actually multiplied by, not a vector standing in for it.
  # `rep_index` names the replicate rows this batch fills, and is `0L` for the
  # point estimate, which records nothing.
  arm_surv <- function(mult, mats, arm_slot, rep_index) {
    if (!is.null(mult_store) && rep_index[1L] > 0L) {
      mult_store[[arm_slot]][rep_index, ] <<- mult
    }
    .rd_surv_batch(mult, mats)
  }

  # The single place the sign convention lives, shared by the point estimate
  # and every replicate so the two cannot disagree.
  rd_of <- function(s_comparator, s_intervention) s_comparator - s_intervention

  one <- matrix(1L, nrow = 1L, ncol = n_pt)
  surv_int <- arm_surv(one, m_int, "intervention", 0L)[1L, ]
  surv_cmp <- arm_surv(one, m_cmp, "comparator", 0L)[1L, ]
  rd <- rd_of(surv_cmp, surv_int)

  boot <- matrix(NA_real_, nrow = n_boot, ncol = n_band)
  for (first in seq.int(1L, n_boot, by = .RD_BOOT_BATCH)) {
    rep_index <- seq.int(first, min(first + .RD_BOOT_BATCH - 1L, n_boot))
    # One draw per replicate, in replicate order, exactly as one replicate at a
    # time drew them. The batch changes what the multiplicities are multiplied
    # by. It never changes how they are drawn, so the RNG stream does not move.
    mult <- matrix(0L, nrow = length(rep_index), ncol = n_pt)
    for (k in seq_along(rep_index)) {
      mult[k, ] <- tabulate(.boot_person_index(pt_person), nbins = n_pt)
    }
    s_cmp <- arm_surv(mult, m_cmp, "comparator", rep_index)
    s_int <- arm_surv(mult, m_int, "intervention", rep_index)
    boot[rep_index, ] <- rd_of(s_cmp, s_int)
  }

  alpha <- (1 - conf_level) / 2
  rd_lo <- apply(
    boot,
    2L,
    stats::quantile,
    probs = alpha,
    na.rm = TRUE,
    names = FALSE
  )
  rd_hi <- apply(
    boot,
    2L,
    stats::quantile,
    probs = 1 - alpha,
    na.rm = TRUE,
    names = FALSE
  )

  # An arm with no positive-weight event has no estimable interval, and more
  # replicates never make one. Every replicate draws from the same event-free
  # set, so every replicate gives that arm a failure risk of exactly zero. The
  # percentiles then carry only the OTHER arm's sampling variation and treat
  # this arm's risk as known with certainty, which is anti-conservative. The
  # degeneracy is in the resampling scheme, not in the sample size.
  #
  # The point estimate stays. It is a valid descriptive quantity, and the
  # `interval_status` column says why nothing accompanies it.
  #
  # PER HORIZON and PER ARM, on the events up to and including the band.
  # `m_int$num` and `m_cmp$num` hold `sum(w * event)` per person-trial and
  # band. A column sum is therefore that arm's weighted event total in the
  # band, and the running sum is its total through the horizon. An arm with no
  # event by band 4 and two events by band 8 is inestimable at band 4 and
  # estimable at band 8.
  weighted_events_int <- cumsum(colSums(m_int$num))
  weighted_events_cmp <- cumsum(colSums(m_cmp$num))
  zero_event_arm <- weighted_events_int <= 0 | weighted_events_cmp <= 0
  rd_lo[zero_event_arm] <- NA_real_
  rd_hi[zero_event_arm] <- NA_real_
  # Three states, and each names its own reason. A band whose interval is
  # estimable but contains the null is NOT "ok": the number needed to treat has
  # no interval there, because `x -> -1/x` is undefined across zero. Leaving it
  # on "ok" put that reason nowhere except an empty cell on a figure.
  # `zero-event arm` wins where both apply, because it is why the bounds are
  # `NA` and an `NA` bound cannot be judged against the null.
  interval_status <- rep("ok", n_band)
  interval_status[!.tte_excludes_null(rd_lo, rd_hi)] <- "spans null"
  interval_status[zero_event_arm] <- "zero-event arm"

  # The benefit-or-harm decision, made ONCE, beside `rd`, from the same
  # numbers. Every formatter reads `nnt_direction` and none re-derives it.
  nnt_fields <- .tte_nnt_from_rd(rd)

  # The interval, from the ONE site that maps a risk-difference interval onto
  # the reciprocal scale. Storing it here is what stops a figure from inverting
  # `rd_lo` and `rd_hi` on its own. `.tte_nntb()` returns `NA` on a band whose
  # interval does not strictly exclude the null, which is the same test
  # `interval_status` reports as "spans null".
  nnt_bounds <- .tte_nntb(rd, rd_lo, rd_hi)

  # Distinct PEOPLE, cumulative through the band -- not rows and not
  # person-trials. One woman can carry the event in two of her sequential
  # trials; she is one person who had the outcome, counted once.
  ev_rows <- which(ev == 1L)
  counts <- if (length(ev_rows)) {
    first_ev <- data.table::data.table(
      arm = tv[ev_rows],
      person = person_raw[ev_rows],
      band = band_code[ev_rows]
    )[, .(first_band = min(band)), keyby = c("arm", "person")]
    first_ev[, .N, keyby = .(arm, first_band)]
  } else {
    # An ETT with no event inside the follow-up window is legitimate for a rare
    # outcome in a small stratum. Skipping the grouping matters: data.table
    # evaluates `min()` once on the empty table to type the result, which warns.
    NULL
  }
  cum_persons <- function(which_arm) {
    n <- integer(n_band)
    if (!is.null(counts)) {
      sub <- counts[arm == which_arm]
      if (nrow(sub)) {
        n[sub$first_band] <- sub$N
      }
    }
    cumsum(n)
  }

  # The head count a numbers-at-risk row reports. Three different numbers live
  # in one arm-band cell of this panel, and only the third belongs here:
  #
  #   .N                     rows       = person-trials in the band
  #   sum(w)                 at_risk    = the weighted risk set, the hazard
  #                                       denominator
  #   uniqueN(person)        persons    = the head count
  #
  # It is the same count `$survival_curve()` returns as `n_persons_at_risk`,
  # taken on the same panel. Survival is a weighted probability, so no head
  # count can be derived from it. Only the panel holds the identifiers.
  at_risk_counts <- data.table::data.table(
    arm = tv,
    person = person_raw,
    band = band_code
  )[, .(n_persons = data.table::uniqueN(person)), keyby = c("arm", "band")]
  persons_at_risk <- function(which_arm) {
    n <- integer(n_band)
    sub <- at_risk_counts[arm == which_arm]
    if (nrow(sub)) {
      n[sub$band] <- sub$n_persons
    }
    n
  }

  out <- data.table::data.table(
    band = band_vals,
    surv_comparator = surv_cmp,
    surv_intervention = surv_int,
    rd = rd,
    rd_lo = rd_lo,
    rd_hi = rd_hi,
    interval_status = interval_status,
    nnt = nnt_fields$nnt,
    nnt_lo = nnt_bounds$nntb_lo,
    nnt_hi = nnt_bounds$nntb_hi,
    nnt_direction = nnt_fields$nnt_direction,
    n_persons_with_event_comparator = cum_persons(FALSE),
    n_persons_with_event_intervention = cum_persons(TRUE),
    n_persons_at_risk_comparator = persons_at_risk(FALSE),
    n_persons_at_risk_intervention = persons_at_risk(TRUE)
  )
  data.table::setnames(out, "band", time_var)

  data.table::setattr(out, "rd_boot", boot)
  data.table::setattr(out, "conf_level", conf_level)
  data.table::setattr(out, "n_boot", n_boot)
  data.table::setattr(out, "swereg_type", "risk_difference")
  if (!is.null(mult_store)) {
    data.table::setattr(out, "mult_intervention", mult_store$intervention)
    data.table::setattr(out, "mult_comparator", mult_store$comparator)
  }
  out
}

#' Number needed to treat for benefit, from a signed risk difference
#'
#' The number needed to treat for benefit is the reciprocal of the risk
#' difference, negated. The negation is not cosmetic. The risk difference this
#' package reports is signed,
#' `RD(t) = Risk_intervention(t) - Risk_comparator(t)`, so a protective
#' intervention gives a NEGATIVE risk difference. Negating the reciprocal makes
#' a benefit read as a positive number of women, which is the direction every
#' reader expects of this quantity.
#'
#' The value is signed and stays signed. A harmful intervention returns a
#' negative number, and that minus sign is the answer: `abs()` has no place
#' anywhere in this arithmetic. It is named `nntb` and never plain "NNT",
#' because a reader who meets a column headed "NNT" assumes the number is
#' positive and means benefit, and a signed reciprocal under that heading would
#' say the opposite of what happened.
#'
#' Deaths are censored rather than modelled as a competing risk, so the risk
#' difference this inverts is cause-specific under independent censoring, and
#' so is the number needed to treat computed from it.
#'
#' The interval must STRICTLY exclude the null. The map `x -> -1/x` is monotone
#' increasing on each side of zero and undefined across it, so an interval that
#' contains zero has no reciprocal interval to report. A bound of EXACTLY zero
#' touches the null and is therefore not exclusion of it. Loosening either
#' comparison to `>=` or `<=` would report a finite number needed to treat for
#' an interval that is compatible with no effect at all.
#'
#' When the interval does not strictly exclude the null, all three values are
#' `NA`. Be clear about what that `NA` is: the quantity is UNDEFINED there, not
#' merely unmeasured, and it does make the displayed value depend on the
#' interval. A band whose interval crosses zero shows nothing, and that is a
#' property of the reciprocal transform rather than a decision to hide a
#' non-significant result.
#'
#' Because the transform is monotone on each side, an interval that excludes
#' the null keeps its ordering: `rd_lo` maps to `nntb_lo`, `rd_hi` maps to
#' `nntb_hi`, and `nntb_lo < nntb_hi` still holds. The bounds are therefore
#' reciprocal-INVERTED in value while keeping their roles.
#'
#' This function returns THREE numbers and no decision. It does not report a
#' direction, on purpose. `.tte_nnt_from_rd()` decides the direction once,
#' `.tte_rd_curve()` stores it, and a formatter reads the stored column. A
#' second producer here would be a second decision site, which is the defect
#' the `nnt_direction` column exists to remove.
#'
#' @param rd Numeric, the signed cause-specific risk difference.
#' @param rd_lo Numeric, the lower confidence bound of `rd`.
#' @param rd_hi Numeric, the upper confidence bound of `rd`.
#' @return A data.table with one row per input element and columns `nntb`,
#'   `nntb_lo` and `nntb_hi`. All three are `NA_real_` on a row whose interval
#'   does not strictly exclude zero.
#' @noRd
.tte_nntb <- function(rd, rd_lo, rd_hi) {
  n <- max(length(rd), length(rd_lo), length(rd_hi))
  if (n == 0L) {
    return(data.table::data.table(
      nntb = numeric(0),
      nntb_lo = numeric(0),
      nntb_hi = numeric(0)
    ))
  }
  rd <- rep_len(as.numeric(rd), n)
  rd_lo <- rep_len(as.numeric(rd_lo), n)
  rd_hi <- rep_len(as.numeric(rd_hi), n)

  # STRICT, and shared with `.tte_rd_curve()`. A bound of exactly zero touches
  # the null, so the interval does not exclude it. One copy of that test, so the
  # guard here and the `interval_status` column cannot drift apart.
  excludes_null <- .tte_excludes_null(rd_lo, rd_hi)

  nntb <- rep(NA_real_, n)
  nntb_lo <- rep(NA_real_, n)
  nntb_hi <- rep(NA_real_, n)
  # Signed throughout. Harm keeps its minus sign.
  nntb[excludes_null] <- -1 / rd[excludes_null]
  # The low bound of the risk difference is the low bound here too: the
  # transform is monotone increasing away from zero, which is exactly what the
  # strict guard above guarantees.
  nntb_lo[excludes_null] <- -1 / rd_lo[excludes_null]
  nntb_hi[excludes_null] <- -1 / rd_hi[excludes_null]

  data.table::data.table(nntb = nntb, nntb_lo = nntb_lo, nntb_hi = nntb_hi)
}

#' Render one number-needed-to-treat cell
#'
#' The STORED DECISION chooses the label, and this function never re-derives it.
#' `nnt_direction` reads `"benefit"` and the cell renders `NNTB <magnitude>`,
#' the number needed to treat for benefit. It reads `"harm"` and the cell
#' renders `NNTH <magnitude>`, the number needed to harm. The two are opposite
#' clinical statements and the label is the only thing that separates them.
#'
#' This function used to test the sign of `nntb` instead. That made every
#' formatter its own decision-maker, and nothing forced two of them to agree.
#' `.tte_nnt_from_rd()` now makes the decision once, and this function reads it.
#' `nnt_direction` has no default. A caller that cannot supply one gets an
#' error. A silent fall back to the sign is the defect this repairs.
#'
#' The magnitude never comes from `abs()`. The harm branch negates the value
#' explicitly, so a reader of this source sees which branch they are in. An
#' `abs()` here would make benefit and harm render the same number under the
#' same label, and the figure would still draw.
#'
#' An empty cell means the quantity is undefined: `.tte_nntb()` returns `NA`
#' whenever the interval does not strictly exclude the null.
#'
#' Supply `nntb_lo` and `nntb_hi` and the cell carries the interval too, as
#' `NNTB 2,000 (1,250 to 5,000)`. The separator is ` to `, the one the
#' risk-difference column in `R/forest_plot.R` uses, so one separator carries
#' one meaning across the figure. Both bounds take the point estimate's
#' thousands separator and its 0 decimal places. A fractional number needed to
#' treat is not a quantity.
#'
#' A row whose bounds are missing renders EMPTY, even when the point estimate
#' is finite. A point estimate printed without its interval invites a reader to
#' treat it as precise. A zero-event arm is exactly where it is not: see
#' `.tte_rd_curve()`, which sets both bounds to `NA` there.
#'
#' Omit both bounds and the cell renders the point estimate alone. No caller in
#' the package does that today. `.forest_rd_map()` in `R/forest_plot.R` supplies
#' both bounds, so the figure never prints a bare point estimate.
#'
#' The bounds print in ascending order on BOTH signs, and the two branches get
#' there differently. `.tte_nntb()` guarantees `nntb_lo < nntb_hi`, so the
#' benefit branch prints them in the order it holds them. The harm branch
#' negates each bound, which reverses the order, so it prints `-nntb_hi` first.
#' The negation is explicit and never `abs()`, so a reader of this source sees
#' which branch they are in.
#'
#' The labels stay `NNTB` and `NNTH` in full. They are the Cochrane and GRADE
#' terms; `B` and `H` are not recognised notation.
#'
#' Every row gets a cell. An earlier version rendered a number for the primary
#' outcome only. That guard is gone, so a secondary outcome now shows its own
#' number needed to treat.
#'
#' @param nntb Numeric, as returned by `.tte_nntb()`. `NA` and non-finite
#'   values render as an empty cell.
#' @param nntb_lo,nntb_hi Numeric bounds, as returned by `.tte_nntb()`, or
#'   `NULL`. Supply both to render the interval. Supply neither to render the
#'   point estimate alone.
#' @param nnt_direction Character, the stored decision, as carried by the
#'   `nnt_direction` column of `.tte_nntb()` or `.tte_rd_curve()`. Each element
#'   MUST be `"benefit"`, `"harm"` or `NA_character_`. There is no default, and
#'   an `NA` element renders an empty cell.
#' @return A character vector as long as `nntb`.
#' @noRd
.tte_nntb_cell <- function(nntb, nntb_lo = NULL, nntb_hi = NULL, nnt_direction) {
  if (missing(nnt_direction)) {
    stop(
      "nnt_direction is required: the cell reads the stored decision and ",
      "never re-derives it from the sign of nntb"
    )
  }
  n <- length(nntb)
  if (n == 0L) {
    return(character(0))
  }
  nntb <- as.numeric(nntb)

  nnt_direction <- rep_len(as.character(nnt_direction), n)
  unknown <- !is.na(nnt_direction) & !nnt_direction %in% c("benefit", "harm")
  if (any(unknown)) {
    stop(
      "nnt_direction must be 'benefit', 'harm' or NA; got '",
      nnt_direction[which(unknown)[1L]],
      "'"
    )
  }

  with_ci <- !is.null(nntb_lo) && !is.null(nntb_hi)
  if (with_ci) {
    lo <- rep_len(as.numeric(nntb_lo), n)
    hi <- rep_len(as.numeric(nntb_hi), n)
    # No interval, no cell. The point estimate alone would read as precise.
    nntb[!is.finite(lo) | !is.finite(hi)] <- NA_real_
  }

  people <- function(x) vapply(x, .ff_num, character(1), digits = 0L)
  # The stored decision, read. NOT the sign of `nntb`, which is what let a
  # figure and a results sheet reach opposite conclusions about one band.
  usable <- is.finite(nntb) & !is.na(nnt_direction)
  benefit <- usable & nnt_direction == "benefit"
  harm <- usable & nnt_direction == "harm"
  out <- rep("", n)

  if (any(benefit)) {
    txt <- paste0("NNTB ", people(nntb[benefit]))
    if (with_ci) {
      # Already ascending: `.tte_nntb()` returns `nntb_lo < nntb_hi`.
      txt <- paste0(
        txt,
        " (",
        people(lo[benefit]),
        " to ",
        people(hi[benefit]),
        ")"
      )
    }
    out[benefit] <- txt
  }
  if (any(harm)) {
    # Negated, not `abs()`ed. The stored value stays signed.
    txt <- paste0("NNTH ", people(-nntb[harm]))
    if (with_ci) {
      # Negation reverses the order, so the high bound is negated first.
      txt <- paste0(
        txt,
        " (",
        people(-hi[harm]),
        " to ",
        people(-lo[harm]),
        ")"
      )
    }
    out[harm] <- txt
  }
  out
}

#' TTEDesign class for target trial emulation
#'
#' Holds column name mappings that define the schema for trial data. This
#' allows specifying variable names once and reusing them across all TTE
#' workflow functions.
#'
#' @param person_id_var Character or NULL, name of the person identifier column
#'   (default: `"id"`). `create_skeleton()` names the person identifier `id`,
#'   and `TTEPlan` passes `"id"` whenever an argset does not override it, so the
#'   default matches what the pipeline already builds. A person contributes many
#'   sequential trials, so this column is what separates a head count of people
#'   from a count of person-trials.
#' @param id_var Character, name of the person-trial identifier column (default: "enrollment_person_trial_id").
#' @param treatment_var Character, name of the baseline treatment column.
#' @param outcome_vars Character vector, names of outcome event indicator columns.
#' @param confounder_vars Character vector, names of confounder columns for
#'   propensity/censoring models.
#' @param follow_up_time Integer, expected follow-up duration in time units.
#' @param tstart_var Character, name of period start time column (default: "tstart").
#' @param tstop_var Character, name of period end time column (default: "tstop").
#' @param time_treatment_var Character or NULL, name of time-varying treatment column
#'   for per-protocol analysis (default: NULL).
#' @param eligible_var Character or NULL, name of eligibility indicator column
#'   (default: NULL).
#' @param admin_censor_var Character or NULL, name of administrative censoring
#'   boundary column (default: NULL). Mutually exclusive with
#'   `admin_censor_isoyearweek`. Not implemented in outcome preparation:
#'   `s5_prepare_outcome()` stops if this is set -- use
#'   `admin_censor_isoyearweek` instead.
#' @param admin_censor_isoyearweek Character or NULL, the study end date in
#'   ISO year-week format (e.g., "2023-52"). When set, administrative censoring
#'   is computed internally as weeks from each trial's entry date to this
#'   global study end date. Requires an `isoyearweek` column in the data.
#'   Mutually exclusive with `admin_censor_var` (default: NULL).
#' @param period_width Integer, band width in weeks for enrollment and
#'   time aggregation (default: 4L). Calendar time is grouped into bands
#'   of this width. Must be a positive integer.
#'
#' @examples
#' # Design for post-panel (trial-level) data
#' design <- TTEDesign$new(
#'   id_var = "enrollment_person_trial_id",
#'   treatment_var = "baseline_intervention",
#'   outcome_vars = c("death", "hosp"),
#'   confounder_vars = c("age", "education"),
#'   follow_up_time = 156L
#' )
#'
#' # Design for pre-panel (person-week) data with full workflow
#' design_prepanel <- TTEDesign$new(
#'   person_id_var = "id",
#'   treatment_var = "baseline_intervention",
#'   outcome_vars = c("death", "hosp"),
#'   confounder_vars = c("age", "education"),
#'   follow_up_time = 156L,
#'   eligible_var = "eligible"
#' )
#'
#' @family tte_classes
#' @seealso [TTEEnrollment] for the trial class
#' @importFrom R6 R6Class
#' @export
TTEDesign <- R6::R6Class(
  "TTEDesign",
  public = list(
    #' @field person_id_var Character or NULL, person identifier column name.
    person_id_var = NULL,
    #' @field id_var Character, person-trial identifier column name.
    id_var = "enrollment_person_trial_id",
    #' @field treatment_var Character, treatment column name.
    treatment_var = NULL,
    #' @field outcome_vars Character vector, outcome column names.
    outcome_vars = NULL,
    #' @field confounder_vars Character vector, confounder column names.
    confounder_vars = NULL,
    #' @field subgroup_vars Character vector or NULL, baseline subgroup
    #'   (effect-modifier) column names; should be a subset of confounder_vars.
    subgroup_vars = NULL,
    #' @field follow_up_time Integer, follow-up duration.
    follow_up_time = NULL,
    #' @field tstart_var Character, period start time column name.
    tstart_var = "tstart",
    #' @field tstop_var Character, period end time column name.
    tstop_var = "tstop",
    #' @field time_treatment_var Character or NULL, time-varying treatment column.
    time_treatment_var = NULL,
    #' @field eligible_var Character or NULL, eligibility column name.
    eligible_var = NULL,
    #' @field admin_censor_var Character or NULL, admin censoring column.
    admin_censor_var = NULL,
    #' @field admin_censor_isoyearweek Character or NULL, admin censoring date.
    admin_censor_isoyearweek = NULL,
    #' @field period_width Integer, band width in weeks for enrollment/aggregation.
    period_width = 4L,

    #' @description Create a new TTEDesign object.
    initialize = function(
      person_id_var = "id",
      id_var = "enrollment_person_trial_id",
      treatment_var,
      outcome_vars,
      confounder_vars,
      follow_up_time,
      subgroup_vars = NULL,
      tstart_var = "tstart",
      tstop_var = "tstop",
      time_treatment_var = NULL,
      eligible_var = NULL,
      admin_censor_var = NULL,
      admin_censor_isoyearweek = NULL,
      period_width = 4L
    ) {
      # Validation
      if (!is.null(person_id_var) && length(person_id_var) != 1) {
        stop("person_id_var must be length 1 or NULL")
      }
      if (length(id_var) != 1) {
        stop("id_var must be length 1")
      }
      if (length(treatment_var) != 1) {
        stop("treatment_var must be length 1")
      }
      if (length(outcome_vars) == 0) {
        stop("outcome_vars cannot be empty")
      }
      if (length(follow_up_time) != 1 || follow_up_time <= 0) {
        stop("follow_up_time must be a positive integer")
      }
      if (length(tstart_var) != 1) {
        stop("tstart_var must be length 1")
      }
      if (length(tstop_var) != 1) {
        stop("tstop_var must be length 1")
      }
      if (!is.null(time_treatment_var) && length(time_treatment_var) != 1) {
        stop("time_treatment_var must be length 1 or NULL")
      }
      if (!is.null(eligible_var) && length(eligible_var) != 1) {
        stop("eligible_var must be length 1 or NULL")
      }
      if (!is.null(admin_censor_var) && length(admin_censor_var) != 1) {
        stop("admin_censor_var must be length 1 or NULL")
      }
      if (
        !is.null(admin_censor_isoyearweek) &&
          length(admin_censor_isoyearweek) != 1
      ) {
        stop("admin_censor_isoyearweek must be length 1 or NULL")
      }
      if (!is.null(admin_censor_var) && !is.null(admin_censor_isoyearweek)) {
        stop(
          "admin_censor_var and admin_censor_isoyearweek are mutually exclusive"
        )
      }
      if (
        length(period_width) != 1 ||
          !is.numeric(period_width) ||
          period_width <= 0 ||
          period_width != as.integer(period_width)
      ) {
        stop("period_width must be a positive integer")
      }

      self$person_id_var <- person_id_var
      self$id_var <- id_var
      self$treatment_var <- treatment_var
      self$outcome_vars <- outcome_vars
      self$confounder_vars <- confounder_vars
      self$subgroup_vars <- subgroup_vars
      self$follow_up_time <- as.integer(follow_up_time)
      self$tstart_var <- tstart_var
      self$tstop_var <- tstop_var
      self$time_treatment_var <- time_treatment_var
      self$eligible_var <- eligible_var
      self$admin_censor_var <- admin_censor_var
      self$admin_censor_isoyearweek <- admin_censor_isoyearweek
      self$period_width <- as.integer(period_width)

      private$.schema_version <- .TTE_DESIGN_SCHEMA_VERSION
    },

    #' @description Check if this object's schema version matches the current class version.
    #' Warns if the object was saved with an older schema version.
    #' @return `invisible(TRUE)` if versions match, `invisible(FALSE)` otherwise.
    check_version = function() {
      current <- .TTE_DESIGN_SCHEMA_VERSION
      saved <- private$.schema_version %||% 0L
      if (saved < current) {
        warning(
          "This ",
          class(self)[1],
          " was saved with schema version ",
          saved,
          " but current version is ",
          current,
          ". Re-create this object.",
          call. = FALSE
        )
      }
      invisible(saved == current)
    },

    #' @description Print the TTEDesign object.
    #' @param ... Ignored.
    print = function(...) {
      cat("<TTEDesign>\n")
      if (!is.null(self$person_id_var)) {
        cat("  Person ID:", self$person_id_var, "\n")
      }
      cat("  Trial ID:", self$id_var, "\n")
      cat("  Treatment:", self$treatment_var, "\n")
      if (!is.null(self$time_treatment_var)) {
        cat("  Time-varying treatment:", self$time_treatment_var, "\n")
      }
      cat("  Outcomes:", paste(self$outcome_vars, collapse = ", "), "\n")
      cat("  Confounders:", paste(self$confounder_vars, collapse = ", "), "\n")
      cat("  Follow-up:", self$follow_up_time, "time units\n")
      cat("  Period width:", self$period_width, "weeks\n")
      cat("  Time vars:", self$tstart_var, "/", self$tstop_var, "\n")
      if (!is.null(self$eligible_var)) {
        cat("  Eligibility:", self$eligible_var, "\n")
      }
      invisible(self)
    }
  ),

  private = list(
    .schema_version = NULL
  )
)


# =============================================================================
# TTEEnrollment: Enrollment data with design and state (R6 class)
# =============================================================================
# Object-oriented trial container with public methods for the TTE workflow.
# Enrollment (enroll), outcome prep (s5_prepare_outcome), IPCW (s6_ipcw_pp), and
# weight combination (combine_weights) are private implementation details.
# Mutating methods return invisible(self) for $-chaining.
#
# Public workflow methods are step-numbered to signal execution order:
#   0. initialize / print             — construction and display (enroll with bands)
#   1. $s1_impute_confounders()       — fill missing confounders
#   2. $s2_ipw()                      — inverse probability of treatment
#   3. $s3_truncate_weights()         — clip extreme weights
#   4. $s4_prepare_for_analysis()     — outcome + IPCW-PP
#   5. extract / summary / etc.       — data access and diagnostics
#
# Also includes standalone helpers: tteenrollment_rbind(),
# tteenrollment_rates_combine(), tteenrollment_irr_combine(),
# tteenrollment_impute_confounders(), and summary.TTEEnrollment S3 method.
# =============================================================================

#' TTEEnrollment class for target trial emulation
#'
#' Holds the enrollment data, design specification, and workflow state. Methods
#' modify in-place and return `invisible(self)` for `$`-chaining.
#' R6 reference semantics mean `trial$data[, := ...]` modifies the data.table
#' in-place without copy-on-write overhead.
#'
#' @param data A data.table containing the trial data.
#' @param design A [TTEDesign] object specifying column mappings.
#' @param data_level Character, either "person_week" for pre-panel data or
#'   "trial" for post-panel data. Determines which methods can be applied.
#' @param steps_completed Character vector of completed workflow steps.
#' @param active_outcome Character or NULL, the current outcome for IPCW-PP analysis.
#' @param weight_cols Character vector of weight column names created.
#' @param ratio Numeric or NULL. If provided, automatically enrolls participants
#'   (sampling comparison group and creating trial panels). Only valid for
#'   person_week data.
#' @param seed Integer or NULL. Random seed for enrollment reproducibility.
#' @param extra_cols Character vector or NULL. Extra columns to include in
#'   trial panels during enrollment.
#'
#' @details
#' The `data_level` property controls which methods are available:
#' - `"person_week"`: Data has one row per person per time unit. Pass `ratio`
#'   to the constructor to enroll and transition to trial level.
#' - `"trial"`: Data has been expanded to trial panels (band-level). Methods
#'   `$s2_ipw()`, `$s4_prepare_for_analysis()`, and `$s3_truncate_weights()` require this level.
#'
#' Enrollment (matching + panel expansion) transitions data from "person_week"
#' to "trial" level and is triggered by passing `ratio` to the constructor.
#'
#' @section Methods:
#' **Mutating (return `invisible(self)` for chaining, step-numbered for execution order):**
#' \describe{
#'   \item{`$s1_impute_confounders(confounder_vars, seed)`}{Step 1: Impute missing confounders}
#'   \item{`$s2_ipw(stabilize)`}{Step 2: Calculate inverse probability of treatment weights}
#'   \item{`$s3_truncate_weights(weight_cols, lower, upper, suffix)`}{Step 3: Truncate extreme weights}
#'   \item{`$s4_prepare_for_analysis(outcome, follow_up, ...)`}{Step 4: Prepare outcome data and calculate IPCW-PP in one step}
#' }
#'
#' **Non-mutating (return data):**
#' \describe{
#'   \item{`$extract()`}{Return the data.table}
#'   \item{`$summary(pretty)`}{Return summary statistics}
#'   \item{`$weight_summary()`}{Print weight distribution diagnostics}
#'   \item{`$table1(ipw_col)`}{Generate baseline characteristics table}
#'   \item{`$rates(weight_col)`}{Calculate events, person-years, and rates}
#'   \item{`$irr(weight_col)`}{Fit Poisson models and extract IRR}
#'   \item{`$survival_curve(weight_col, save_path, title)`}{Weighted discrete-time survival curve from the person-week panel (ITT via baseline IPW, or PP via a time-varying `analysis_weight_pp_trunc`)}
#'   \item{`$risk_difference(weight_col, n_boot, seed, conf_level)`}{Signed cause-specific risk difference per band, with a percentile bootstrap interval resampled at the person level}
#' }
#'
#' **Active bindings:**
#' \describe{
#'   \item{`$enrollment_stage`}{Derived lifecycle stage: `"pre_enrollment"`, `"enrolled"`, or `"analysis_ready"`}
#' }
#'
#' @examples
#' \dontrun{
#' design <- TTEDesign$new(
#'   person_id_var = "id",
#'   treatment_var = "intervention",
#'   outcome_vars = "death",
#'   confounder_vars = c("age", "sex"),
#'   follow_up_time = 52L,
#'   eligible_var = "eligible"
#' )
#'
#' # Enroll via constructor (band-based), then $-chain
#' enrollment <- TTEEnrollment$new(my_skeleton, design,
#'   ratio = 2, seed = 4, extra_cols = "isoyearweek"
#' )
#' enrollment$
#'   s2_ipw()$
#'   s4_prepare_for_analysis(outcome = "death", estimate_ipcw_pp_with_gam = TRUE)
#' }
#'
#' @family tte_classes
#' @seealso [TTEDesign] for design class
#' @export
TTEEnrollment <- R6::R6Class(
  "TTEEnrollment",
  public = list(
    #' @field data A data.table with trial data.
    data = NULL,
    #' @field design A TTEDesign R6 object.
    design = NULL,
    #' @field data_level Character, "person_week" or "trial".
    data_level = "trial",
    #' @field steps_completed Character vector of completed workflow steps.
    steps_completed = character(),
    #' @field active_outcome Character or NULL, current outcome for IPCW-PP.
    active_outcome = NULL,
    #' @field weight_cols Character vector of weight column names.
    weight_cols = character(),
    #' @field estimand Character or NULL. Set to "pp" or "itt" once an analysis
    #'   dataset is prepared; governs which weights are valid in `$irr()`.
    #'   NULL (legacy / unprepared) is treated as per-protocol.
    estimand = NULL,

    #' @description Create a new TTEEnrollment object.
    #' @param data A data.table containing the trial data. A copy is made
    #'   automatically to avoid modifying the caller's data.
    #' @param design A [TTEDesign] object specifying column mappings.
    #' @param data_level Character or NULL. If NULL (default), auto-detects based on
    #'   which identifier column exists in data. "person_week" for pre-panel data
    #'   (requires person_id_var), "trial" for post-panel data (requires id_var).
    #' @param steps_completed Character vector of completed workflow steps.
    #' @param active_outcome Character or NULL, the current outcome for IPCW-PP analysis.
    #' @param weight_cols Character vector of weight column names created.
    #' @param ratio Numeric or NULL. If provided, automatically enrolls participants
    #'   (sampling comparison group and creating trial panels). Only valid for
    #'   person_week data.
    #' @param seed Integer or NULL. Random seed for enrollment reproducibility.
    #' @param extra_cols Character vector or NULL. Extra columns to include in
    #'   trial panels during enrollment.
    #' @param enrolled_ids data.table or NULL. Pre-matched enrollment IDs from
    #'   the two-pass pipeline. When provided, enrollment skips the matching
    #'   phase and uses these IDs directly.
    #' @param own_data Logical. If TRUE, takes ownership of the data.table
    #'   without copying it. Use only when the caller will not reuse the data.
    initialize = function(
      data,
      design,
      data_level = NULL,
      steps_completed = character(),
      active_outcome = NULL,
      weight_cols = character(),
      ratio = NULL,
      seed = NULL,
      extra_cols = NULL,
      enrolled_ids = NULL,
      own_data = FALSE
    ) {
      # Copy input data to avoid modifying the caller's data.table
      if (!data.table::is.data.table(data)) {
        data <- data.table::as.data.table(data)
      } else if (!own_data) {
        data <- data.table::copy(data)
      }

      # Auto-detect data_level if not specified
      if (is.null(data_level)) {
        has_trial_id <- design$id_var %in% names(data)
        has_person_id <- !is.null(design$person_id_var) &&
          design$person_id_var %in% names(data)

        if (has_trial_id && !has_person_id) {
          data_level <- "trial"
        } else if (has_person_id && !has_trial_id) {
          data_level <- "person_week"
        } else if (has_trial_id && has_person_id) {
          data_level <- "trial"
        } else {
          stop(
            "Cannot auto-detect data_level. Data must have either:\n",
            "  - person_id_var ('",
            design$person_id_var,
            "') for person_week data, or\n",
            "  - id_var ('",
            design$id_var,
            "') for trial data"
          )
        }
      }

      # Validation
      if (!data_level %in% c("person_week", "trial")) {
        stop("data_level must be 'person_week' or 'trial'")
      }
      if (data_level == "person_week") {
        if (is.null(design$person_id_var)) {
          stop("person_week data requires person_id_var in design")
        }
        if (!design$person_id_var %in% names(data)) {
          stop(paste(
            "person_week data requires person_id_var column:",
            design$person_id_var
          ))
        }
      } else {
        if (!design$id_var %in% names(data)) {
          stop(paste(
            "trial data requires id_var column:",
            design$id_var
          ))
        }
      }
      if (!design$treatment_var %in% names(data)) {
        stop(paste("Missing required column:", design$treatment_var))
      }
      if (
        !is.null(active_outcome) &&
          !active_outcome %in% design$outcome_vars
      ) {
        stop("active_outcome must be one of design$outcome_vars")
      }

      self$data <- data
      self$design <- design
      self$data_level <- data_level
      self$steps_completed <- steps_completed
      self$active_outcome <- active_outcome
      self$weight_cols <- weight_cols

      private$.schema_version <- .TTE_ENROLLMENT_SCHEMA_VERSION

      if (!is.null(ratio) || !is.null(enrolled_ids)) {
        private$enroll(
          ratio = ratio,
          seed = seed,
          extra_cols = extra_cols,
          enrolled_ids = enrolled_ids
        )
      }
    },

    #' @description Print the TTEEnrollment object.
    #' @param ... Ignored.
    print = function(...) {
      cat("<TTEEnrollment>\n")
      cat("  Stage:", self$enrollment_stage, "\n")
      cat("  Data level:", self$data_level, "\n")
      cat("  Design:", self$design$id_var, "~", self$design$treatment_var, "\n")
      cat("  Outcomes:", paste(self$design$outcome_vars, collapse = ", "), "\n")
      cat(
        "  Data:",
        format(nrow(self$data), big.mark = ","),
        "rows x",
        ncol(self$data),
        "cols\n"
      )
      if (length(self$steps_completed) > 0) {
        cat("  Steps:", paste(self$steps_completed, collapse = " -> "), "\n")
      }
      if (!is.null(self$active_outcome)) {
        cat("  Active outcome:", self$active_outcome, "\n")
      }
      if (length(self$weight_cols) > 0) {
        cat("  Weights:", paste(self$weight_cols, collapse = ", "), "\n")
      }
      invisible(self)
    },

    #' @description Check if this object's schema version matches the current class version.
    #' Warns if the object was saved with an older schema version.
    #' @return `invisible(TRUE)` if versions match, `invisible(FALSE)` otherwise.
    check_version = function() {
      current <- .TTE_ENROLLMENT_SCHEMA_VERSION
      saved <- private$.schema_version %||% 0L
      if (saved < current) {
        warning(
          "This ",
          class(self)[1],
          " was saved with schema version ",
          saved,
          " but current version is ",
          current,
          ". Re-create this object.",
          call. = FALSE
        )
      }
      invisible(saved == current)
    },

    # =========================================================================
    # Mutating methods — ordered by workflow execution sequence
    # =========================================================================

    #' @description Step 1: Impute missing confounders by sampling from observed values.
    #' @param confounder_vars Character vector of confounder column names to impute.
    #' @param seed Integer seed for reproducibility (default: 4L).
    s1_impute_confounders = function(confounder_vars, seed = 4L) {
      id_var <- self$design$id_var

      # Build a trial-level table once. Prefer filtering to baseline rows
      # (tstart_var == 0), which is a single linear scan on the panel.
      # Fall back to a group-by first() collapse only when tstart_var is
      # missing. baseline_dt serves both the NA pre-scan and the
      # update-join below, so we never collapse twice.
      tstart_var <- self$design$tstart_var
      baseline_dt <- if (
        !is.null(tstart_var) && tstart_var %in% names(self$data)
      ) {
        self$data[
          get(tstart_var) == 0,
          .SD,
          .SDcols = c(id_var, confounder_vars)
        ]
      } else {
        self$data[,
          lapply(.SD, data.table::first),
          by = c(id_var),
          .SDcols = confounder_vars
        ]
      }
      needs_impute <- confounder_vars[
        vapply(confounder_vars, \(v) anyNA(baseline_dt[[v]]), logical(1))
      ]
      if (length(needs_impute) == 0L) {
        self$steps_completed <- c(self$steps_completed, "impute")
        return(invisible(self))
      }

      # Sample replacements for missing trial-level confounder values.
      set.seed(seed)
      for (var in needs_impute) {
        missing_trials <- baseline_dt[is.na(get(var)), get(id_var)]
        observed_vals <- baseline_dt[!is.na(get(var)), get(var)]
        sampled_vals <- sample(
          observed_vals,
          length(missing_trials),
          replace = TRUE
        )
        baseline_dt[get(id_var) %in% missing_trials, (var) := sampled_vals]
      }

      # Update-join: overwrite the needs_impute columns in `self$data` in
      # place with the imputed trial-level values. Avoids allocating a
      # new merged table.
      data.table::setkeyv(self$data, id_var)
      data.table::setkeyv(baseline_dt, id_var)
      self$data[
        baseline_dt,
        (needs_impute) := mget(paste0("i.", needs_impute)),
        on = id_var
      ]

      self$steps_completed <- c(self$steps_completed, "impute")
      invisible(self)
    },

    #' @description Step 2: Calculates inverse probability of treatment weights.
    #'
    #' Estimates the propensity score P(A=1 | L_baseline) via logistic
    #' regression on baseline rows only, then computes stabilized (or
    #' unstabilized) IPW. This addresses **baseline** confounding for the
    #' per-protocol analysis pipeline.
    #'
    #' Note: This does NOT estimate time-varying treatment weights
    #' for as-treated analysis (Danaei 2013, Section 4.3). As-treated
    #' analysis is not currently implemented.
    #'
    #' Robust standard errors for within-person correlation are handled
    #' downstream by `survey::svydesign(ids = ~person_id_var)` in
    #' `$irr()` (Hernan 2008, Danaei 2013).
    #'
    #' @param stabilize Logical, default TRUE.
    s2_ipw = function(stabilize = TRUE) {
      if (self$data_level != "trial") {
        stop(
          "s2_ipw() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'\n",
          "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level."
        )
      }

      design <- self$design
      treatment_var <- design$treatment_var
      confounder_vars <- design$confounder_vars
      id_var <- design$id_var

      # --- Inline calculate_ipw logic ---
      baseline <- self$data[get(design$tstart_var) == 0]

      missing_confounders <- setdiff(confounder_vars, names(baseline))
      if (length(missing_confounders) > 0) {
        stop(
          "Confounders not found in data: ",
          paste(missing_confounders, collapse = ", ")
        )
      }

      ps_formula <- stats::as.formula(
        paste(treatment_var, "~", paste(confounder_vars, collapse = " + "))
      )
      ps_model <- stats::glm(
        ps_formula,
        data = baseline,
        family = stats::binomial
      )
      baseline[, ps := stats::predict(ps_model, baseline, type = "response")]

      if (stabilize) {
        p_intervention <- mean(baseline[[treatment_var]], na.rm = TRUE)
        baseline[,
          ipw := data.table::fifelse(
            get(treatment_var) == TRUE,
            p_intervention / ps,
            (1 - p_intervention) / (1 - ps)
          )
        ]
      } else {
        baseline[,
          ipw := data.table::fifelse(
            get(treatment_var) == TRUE,
            1 / ps,
            1 / (1 - ps)
          )
        ]
      }

      data.table::setkeyv(baseline, id_var)
      self$data[baseline, `:=`(ps = i.ps, ipw = i.ipw), on = id_var]

      self$weight_cols <- unique(c(self$weight_cols, "ipw"))
      self$steps_completed <- c(self$steps_completed, "ipw")
      invisible(self)
    },

    #' @description Step 3: Truncates extreme weights at specified quantiles.
    #' @param weight_cols Character vector or NULL.
    #' @param lower Numeric, default 0.01.
    #' @param upper Numeric, default 0.99.
    #' @param suffix Character, default "_trunc".
    s3_truncate_weights = function(
      weight_cols = NULL,
      lower = 0.01,
      upper = 0.99,
      suffix = "_trunc"
    ) {
      if (self$data_level != "trial") {
        stop(
          "s3_truncate_weights() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'\n",
          "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level."
        )
      }

      if (is.null(weight_cols)) {
        weight_cols <- self$weight_cols
      }
      weight_cols <- intersect(weight_cols, names(self$data))

      if (length(weight_cols) == 0) {
        warning("No weight columns to truncate")
        return(invisible(self))
      }

      self$data <- private$.truncate_weights(
        data = self$data,
        weight_cols = weight_cols,
        lower = lower,
        upper = upper,
        suffix = suffix
      )

      new_cols <- paste0(weight_cols, suffix)
      self$weight_cols <- unique(c(self$weight_cols, new_cols))
      self$steps_completed <- c(self$steps_completed, "truncate")
      invisible(self)
    },

    #' @description Step 4: Prepare the outcome/analysis dataset for one estimand.
    #' For `estimand = "pp"` (default) this calls `$s5_prepare_outcome()` then
    #' `$s6_ipcw_pp()`; for `estimand = "itt"` it calls `$s5_prepare_outcome()`
    #' in ITT mode (no censoring at treatment switching) and skips IPCW, since
    #' baseline IPW alone is the valid ITT weight. Either way, censoring-event
    #' rows are then dropped. This is the recommended way to prepare an
    #' enrollment for analysis.
    #'
    #' After `s6_ipcw_pp()` fits the censoring model (which legitimately needs
    #' censoring-event rows to learn from), all rows with
    #' `censor_this_period = 1` are removed from `self$data`. Those rows
    #' represent person-periods at which the individual deviated from the
    #' assigned treatment; including them in a downstream outcome regression
    #' attributes their outcomes to the baseline treatment when in fact they
    #' were observed under the deviated regime, biasing the per-protocol
    #' treatment effect. Matches TrialEmulation's PP behavior on the same
    #' inputs.
    #'
    #' Event-priority convention: when the first outcome event falls in the
    #' same band as the protocol deviation, the band counts as an event, not
    #' a censoring -- the row is kept and the censoring model does not treat
    #' it as censored (since 26.7.3).
    #' @param outcome Character scalar. Must be one of `design$outcome_vars`.
    #' @param follow_up Optional integer. Overrides `design$follow_up_time`.
    #' @param estimand Character, `"pp"` (per-protocol, default) or `"itt"`
    #'   (intention-to-treat). ITT keeps follow-up through treatment switching
    #'   and uses baseline IPW only (no IPCW); analyse it with
    #'   `$irr(weight_col = "ipw_trunc")`.
    #' @param estimate_ipcw_pp_separately_by_treatment Logical, default TRUE.
    #' @param estimate_ipcw_pp_with_gam Logical, default TRUE.
    #' @param censoring_var Character or NULL. Defaults to `"censor_this_period"`.
    s4_prepare_for_analysis = function(
      outcome,
      follow_up = NULL,
      estimand = c("pp", "itt"),
      estimate_ipcw_pp_separately_by_treatment = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      censoring_var = NULL
    ) {
      estimand <- match.arg(estimand)
      self$estimand <- estimand
      private$s5_prepare_outcome(
        outcome = outcome,
        follow_up = follow_up,
        estimand = estimand
      )
      if (is.null(censoring_var)) {
        censoring_var <- "censor_this_period"
      }
      # Per-protocol censors at switching and models the resulting informative
      # censoring (switch + loss) with IPCW. ITT never censors at switching and
      # treats loss as independent, so it needs no IPCW: baseline IPW is the
      # valid weight on its own.
      if (estimand == "pp") {
        private$s6_ipcw_pp(
          estimate_ipcw_pp_separately_by_treatment = estimate_ipcw_pp_separately_by_treatment,
          estimate_ipcw_pp_with_gam = estimate_ipcw_pp_with_gam,
          censoring_var = censoring_var
        )
      }
      if (censoring_var %in% names(self$data)) {
        cz <- self$data[[censoring_var]]
        self$data <- self$data[is.na(cz) | cz != 1L]
      }
      invisible(self)
    },

    # =========================================================================
    # Non-mutating methods — data access, diagnostics, and analysis output
    # =========================================================================

    #' @description Extract the data.table from the trial object.
    #' @return A data.table with the processed trial data.
    extract = function() {
      self$data
    },

    #' @description Summarize trial data statistics.
    #' @param pretty Logical, default FALSE. If TRUE, prints formatted output.
    #' @return If `pretty = FALSE`, a list with summary stats. If TRUE, prints
    #'   formatted output and invisibly returns the list.
    summary = function(pretty = FALSE) {
      design <- self$design
      data <- self$data

      n_rows <- nrow(data)

      person_weeks <- if ("person_weeks" %in% names(data)) {
        sum(data$person_weeks, na.rm = TRUE)
      } else {
        NA_real_
      }

      n_trials <- data.table::uniqueN(data[[design$id_var]])

      n_individuals <- data.table::uniqueN(data[[design$person_id_var]])

      n_events <- if ("event" %in% names(data)) {
        sum(data$event, na.rm = TRUE)
      } else {
        NA_integer_
      }

      size_mb <- as.numeric(utils::object.size(data)) / 1e6

      result <- list(
        n_rows = n_rows,
        person_weeks = person_weeks,
        n_trials = n_trials,
        n_individuals = n_individuals,
        n_events = n_events,
        size_mb = size_mb
      )

      if (pretty) {
        parts <- c(
          paste(format(n_rows, big.mark = ","), "rows")
        )
        if (!is.na(person_weeks)) {
          parts <- c(
            parts,
            paste(format(person_weeks, big.mark = ","), "person-weeks")
          )
        }
        parts <- c(parts, paste(format(n_trials, big.mark = ","), "trials"))
        parts <- c(
          parts,
          paste(format(n_individuals, big.mark = ","), "individuals")
        )
        if (!is.na(n_events)) {
          parts <- c(parts, paste(format(n_events, big.mark = ","), "events"))
        }
        parts <- c(parts, paste(round(size_mb, 1), "MB"))
        cat(paste(parts, collapse = ", "), "\n")
        invisible(result)
      } else {
        result
      }
    },

    #' @description Print weight distribution diagnostics.
    weight_summary = function() {
      cat("TTEEnrollment Weight Summary\n")
      cat("=======================\n\n")

      cat("Design:\n")
      if (!is.null(self$design$person_id_var)) {
        cat("  Person ID variable:", self$design$person_id_var, "\n")
      }
      cat("  Trial ID variable:", self$design$id_var, "\n")
      cat("  Treatment:", self$design$treatment_var, "\n")
      cat("  Outcomes:", paste(self$design$outcome_vars, collapse = ", "), "\n")
      cat("  Follow-up:", self$design$follow_up_time, "time units\n\n")

      cat("Data:\n")
      cat("  Level:", self$data_level, "\n")
      cat("  Rows:", format(nrow(self$data), big.mark = ","), "\n")
      cat("  Columns:", ncol(self$data), "\n\n")

      cat(
        "Steps completed:",
        paste(self$steps_completed, collapse = " -> "),
        "\n\n"
      )

      if (!is.null(self$active_outcome)) {
        cat("Active outcome:", self$active_outcome, "\n\n")
      }

      weight_cols <- intersect(self$weight_cols, names(self$data))
      if (length(weight_cols) > 0) {
        cat("Weight distributions:\n")
        for (col in weight_cols) {
          vals <- self$data[[col]]
          vals <- vals[!is.na(vals)]
          if (length(vals) > 0) {
            cat(sprintf(
              "  %s: mean=%.3f, sd=%.3f, min=%.3f, max=%.3f\n",
              col,
              mean(vals),
              stats::sd(vals),
              min(vals),
              max(vals)
            ))
          }
        }
      }

      invisible(self)
    },

    #' @description Generate baseline characteristics table.
    #'
    #' Returns a long-format `data.table` with one row per categorical level
    #' plus one row per continuous variable. See [.swereg_table1] for the
    #' layout. The result has S3 class `c("swereg_table1", "data.table",
    #' "data.frame")`.
    #'
    #' @param ipw_col Character or NULL. If specified, the table is
    #'   weighted by `ipw_col`.
    #' @param arm_labels Optional named character vector
    #'   `c(comparator = "...", intervention = "...")` used as column headers in
    #'   place of the raw treatment values.
    #' @param include_smd Logical, whether to emit an SMD column
    #'   (default `TRUE`).
    #' @param show_missing One of `"when_present"` (default — emit a Missing
    #'   row only for variables with any missingness), `"always"` (emit a
    #'   Missing row for every variable, even when zero), or `"none"`
    #'   (suppress Missing rows entirely).
    #' @return A `data.table` with class `swereg_table1`.
    table1 = function(
      ipw_col = NULL,
      arm_labels = NULL,
      include_smd = TRUE,
      show_missing = c("when_present", "always", "none")
    ) {
      show_missing <- match.arg(show_missing)
      if (self$data_level != "trial") {
        stop(
          "table1() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'\n",
          "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level."
        )
      }

      design <- self$design
      baseline <- self$data[get(design$tstart_var) == 0]

      if (!is.null(ipw_col) && !ipw_col %in% names(baseline)) {
        stop("ipw_col '", ipw_col, "' not found in data")
      }

      .swereg_table1(
        data = baseline,
        vars = design$confounder_vars,
        strata = design$treatment_var,
        weights = ipw_col,
        include_smd = include_smd,
        show_missing = show_missing,
        arm_labels = arm_labels
      )
    },

    #' @description Calculate events, person-years, and rates by treatment group.
    #' @param weight_col Character, required. Column name for weights.
    #' @return A data.table with events, person-years, and rates.
    rates = function(weight_col) {
      if (self$data_level != "trial") {
        stop(
          "rates() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'"
        )
      }

      design <- self$design
      data <- self$data

      if (!weight_col %in% names(data)) {
        stop("weight_col '", weight_col, "' not found in data")
      }
      if (!"event" %in% names(data)) {
        stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
      }
      if (!"person_weeks" %in% names(data)) {
        stop(
          "'person_weeks' column not found. Enrollment should create this automatically."
        )
      }

      # Sequential TTE inflates person-trial counts relative to unique
      # participants (one person contributes to many weekly trials). Surface
      # `n_persons` alongside `n_trials` so readers and downstream tables see
      # both the analytic denominator and the underlying sample size.
      result <- data[,
        .(
          n_persons = data.table::uniqueN(get(design$person_id_var)),
          n_trials = data.table::uniqueN(get(design$id_var)),
          events_weighted = sum(event * get(weight_col)),
          py_weighted = sum(person_weeks * get(weight_col)) / 52.25,
          rate_per_100000py = sum(event * get(weight_col)) /
            (sum(person_weeks * get(weight_col)) / 52.25) *
            100000
        ),
        by = c(design$treatment_var)
      ]
      data.table::setattr(result, "swereg_type", "rates")
      data.table::setattr(result, "treatment_var", design$treatment_var)
      result
    },

    #' @description Fit weighted Poisson regression and extract incidence rate ratios.
    #'
    #' Uses `survey::svyglm()` with `quasipoisson` family and person-level
    #' clustering (`ids = ~person_id_var`) for robust standard errors. This
    #' accounts for within-person correlation across repeated trial entries
    #' (Hernan 2008, Danaei 2013).
    #'
    #' **IRR vs HR**: For rare events (typical in registry-based TTE studies),
    #' the incidence rate ratio from Poisson regression approximates the hazard
    #' ratio from Cox regression (Thompson 1977). The Poisson model with
    #' `splines::ns(tstop, df=3)` flexibly models the baseline event rate over
    #' follow-up time — analogous to Cox's nonparametric baseline hazard and
    #' to Danaei et al.'s "month of follow-up and its squared terms" in pooled
    #' logistic regression.
    #'
    #' **Computational choice**: `quasipoisson` accounts for overdispersion
    #' from survey weights, and `svyglm` scales to large registry datasets
    #' (unlike `survey::svycoxph()`). This is computationally equivalent to
    #' the pooled logistic approach used by Danaei et al. (2013).
    #'
    #' **Calendar-time adjustment**: When `trial_id` is present in the data
    #' (from band-based enrollment), it is included in the model to adjust for
    #' calendar-time variation in outcome rates across enrollment bands
    #' (Caniglia 2023, Danaei 2013). Uses natural splines for >=5 unique
    #' trial IDs, linear term for 2-4, omitted for 1.
    #'
    #' **Estimand (marginal)**: confounding is removed by the supplied `weights`,
    #' not by adjusting for confounders in this model, so the coefficient is a
    #' *marginal* (population-average) incidence rate ratio, standardised over
    #' the covariate distribution. This contrasts with covariate-adjusted
    #' outcome regressions (e.g. `TrialEmulation`'s pooled logistic), which
    #' target a *conditional* effect. The two coincide for the (collapsible)
    #' rate ratio but differ for the (non-collapsible) odds ratio. See
    #' `vignette("tte-methods")`, "Marginal versus conditional estimands".
    #'
    #' @param weight_col Character, required. Column name for weights.
    #' @return A data.table with IRR estimates and confidence intervals.
    irr = function(weight_col) {
      if (self$data_level != "trial") {
        stop(
          "irr() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'"
        )
      }

      design <- self$design
      data <- self$data

      if (!weight_col %in% names(data)) {
        stop("weight_col '", weight_col, "' not found in data")
      }
      if (!"event" %in% names(data)) {
        stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
      }
      if (!"person_weeks" %in% names(data)) {
        stop(
          "'person_weeks' column not found. Enrollment should create this automatically."
        )
      }

      # Guard: a per-protocol dataset has been censored at protocol deviation,
      # so IPW-only weights (without IPCW) would produce biased ITT-like
      # estimates on it. This does NOT apply to an ITT dataset, which is never
      # censored at switching and for which baseline IPW is the valid weight.
      ipw_only_cols <- c("ipw", "ipw_trunc")
      if (
        weight_col %in%
          ipw_only_cols &&
          "prepare_outcome" %in% self$steps_completed &&
          !identical(self$estimand, "itt")
      ) {
        stop(
          "Cannot use '",
          weight_col,
          "' as weight_col after per-protocol censoring.\n",
          "The dataset has been censored at protocol deviation via $s4_prepare_for_analysis(),\n",
          "so only per-protocol weights (e.g., 'analysis_weight_pp_trunc') are valid.\n",
          "Using IPW-only weights on per-protocol censored data produces biased estimates.\n",
          "For an intention-to-treat analysis, prepare with estimand = \"itt\"."
        )
      }

      private$.fit_irr(data, weight_col)
    },

    #' @description Test for heterogeneity of treatment effects across trials.
    #'
    #' Fits a model with a `trial_id x treatment` interaction term and returns
    #' the Wald test p-value. This tests whether the treatment effect varies
    #' across enrollment bands (Hernan 2008, Danaei 2013).
    #'
    #' @param weight_col Character, required. Column name for weights.
    #' @return A list with `p_value` (Wald test), `n_trials` (unique trial IDs),
    #'   and `interaction_coefs` (data.table of interaction coefficients).
    heterogeneity_test = function(weight_col) {
      if (self$data_level != "trial") {
        stop("heterogeneity_test() requires trial level data.")
      }

      design <- self$design
      data <- self$data

      if (!weight_col %in% names(data)) {
        stop("weight_col '", weight_col, "' not found in data")
      }
      if (!"event" %in% names(data)) {
        stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
      }
      if (!"trial_id" %in% names(data)) {
        stop(
          "'trial_id' column not found. Heterogeneity test requires multiple trials."
        )
      }

      n_trials <- data[, data.table::uniqueN(trial_id)]
      if (n_trials < 2L) {
        stop("Need at least 2 unique trial_ids for heterogeneity test.")
      }

      keep_cols <- unique(c(
        design$person_id_var,
        design$treatment_var,
        design$tstop_var,
        weight_col,
        "event",
        "person_weeks",
        "trial_id"
      ))
      svy_data <- data[, ..keep_cols]

      svy_design <- survey::svydesign(
        ids = as.formula(paste0("~", design$person_id_var)),
        weights = as.formula(paste0("~", weight_col)),
        data = svy_data
      )
      rm(svy_data)

      # Spline interaction: does the treatment effect vary smoothly over
      # calendar time (trial period)? Uses ns(trial_id, df=3) interacted
      # with treatment — 3 interaction terms instead of one per trial period.
      spline_df <- min(3L, n_trials - 1L)
      formula_int <- stats::as.formula(paste0(
        "event ~ ",
        design$treatment_var,
        " * splines::ns(trial_id, df = ",
        spline_df,
        ")",
        " + splines::ns(",
        design$tstop_var,
        ", df = 3)",
        " + offset(log(person_weeks))"
      ))

      fit <- survey::svyglm(
        formula_int,
        design = svy_design,
        family = stats::quasipoisson()
      )
      rm(svy_design)

      # Extract interaction coefficients (treatment:ns(trial_id) terms)
      coef_names <- names(stats::coef(fit))
      interaction_idx <- grep(
        paste0("^", design$treatment_var, "TRUE:"),
        coef_names
      )

      if (length(interaction_idx) == 0) {
        return(list(
          p_value = NA_real_,
          n_trials = n_trials,
          interaction_coefs = data.table::data.table()
        ))
      }

      # Wald test for joint significance of interaction terms
      vcov_mat <- stats::vcov(fit)
      beta_int <- stats::coef(fit)[interaction_idx]
      vcov_int <- vcov_mat[interaction_idx, interaction_idx, drop = FALSE]
      # Guard against non-estimable interactions (NA coefficients or a
      # singular covariance from sparse / separated subgroup cells): drop
      # non-finite terms and return NA rather than crashing on solve().
      finite <- is.finite(beta_int) & is.finite(diag(vcov_int))
      p_value <- if (!any(finite)) {
        NA_real_
      } else {
        beta_f <- beta_int[finite]
        vcov_f <- vcov_int[finite, finite, drop = FALSE]
        wald_stat <- tryCatch(
          as.numeric(t(beta_f) %*% solve(vcov_f) %*% beta_f),
          error = function(e) NA_real_
        )
        if (is.na(wald_stat)) {
          NA_real_
        } else {
          stats::pchisq(wald_stat, df = length(beta_f), lower.tail = FALSE)
        }
      }

      fit_summary <- summary(fit)$coefficients
      interaction_coefs <- data.table::data.table(
        term = coef_names[interaction_idx],
        estimate = fit_summary[interaction_idx, "Estimate"],
        se = fit_summary[interaction_idx, "Std. Error"],
        p = fit_summary[interaction_idx, "Pr(>|t|)"]
      )
      rm(fit)

      list(
        p_value = p_value,
        n_trials = n_trials,
        interaction_coefs = interaction_coefs
      )
    },

    #' @description Test whether the treatment effect is modified by a
    #' categorical baseline subgroup variable.
    #'
    #' Fits one combined model with a `treatment x factor(subgroup_var)`
    #' interaction and runs a Wald test on the interaction terms. This is the
    #' correct test for "do the stratum-specific IRRs differ" -- NOT comparing
    #' the per-stratum confidence intervals. For a binary subgroup the single
    #' interaction coefficient satisfies `exp(coef) = IRR(other) / IRR(ref)`,
    #' where `ref` is the first factor level.
    #'
    #' The subgroup variable should be a confounder (in the PS / IPCW models)
    #' so the marginal weights remain valid within each stratum.
    #'
    #' @param weight_col Character, required. Column name for weights.
    #' @param subgroup_var Character, required. A categorical baseline column.
    #' @return A list with `p_value` (Wald test), `subgroup_var`, `n_levels`,
    #'   `interaction_coefs` (data.table), and, for a binary subgroup,
    #'   `ratio_of_irrs = exp(beta)` with `ratio_lower` / `ratio_upper`
    #'   (NA for multi-level subgroups).
    effect_modification_test = function(weight_col, subgroup_var) {
      if (self$data_level != "trial") {
        stop("effect_modification_test() requires trial level data.")
      }
      design <- self$design
      data <- self$data
      if (!weight_col %in% names(data)) {
        stop("weight_col '", weight_col, "' not found in data")
      }
      if (!subgroup_var %in% names(data)) {
        stop("subgroup_var '", subgroup_var, "' not found in data")
      }
      if (!"event" %in% names(data)) {
        stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
      }
      ipw_only_cols <- c("ipw", "ipw_trunc")
      if (
        weight_col %in%
          ipw_only_cols &&
          "prepare_outcome" %in% self$steps_completed &&
          !identical(self$estimand, "itt")
      ) {
        stop(
          "Cannot use '",
          weight_col,
          "' as weight_col after per-protocol censoring.\n",
          "Use a per-protocol weight (e.g. 'analysis_weight_pp_trunc'), or ",
          "prepare with estimand = \"itt\"."
        )
      }

      d <- data[!is.na(get(subgroup_var))]
      sg_levels <- sort(unique(d[[subgroup_var]]))
      n_levels <- length(sg_levels)
      if (n_levels < 2L) {
        stop(
          "subgroup_var '",
          subgroup_var,
          "' must have >= 2 non-NA levels for an effect-modification test."
        )
      }

      has_trial_id <- "trial_id" %in%
        names(d) &&
        d[, data.table::uniqueN(trial_id)] > 1L
      n_trial_ids <- if (has_trial_id) {
        d[, data.table::uniqueN(trial_id)]
      } else {
        0L
      }
      trial_term <- if (has_trial_id && n_trial_ids >= 5L) {
        " + splines::ns(trial_id, df = 3)"
      } else if (has_trial_id) {
        " + trial_id"
      } else {
        ""
      }

      keep_cols <- unique(c(
        design$person_id_var,
        design$treatment_var,
        design$tstop_var,
        weight_col,
        "event",
        "person_weeks",
        subgroup_var,
        if (has_trial_id) "trial_id"
      ))
      svy_data <- d[, ..keep_cols]
      svy_data[[subgroup_var]] <- factor(svy_data[[subgroup_var]])

      svy_design <- survey::svydesign(
        ids = as.formula(paste0("~", design$person_id_var)),
        weights = as.formula(paste0("~", weight_col)),
        data = svy_data
      )
      rm(svy_data)

      formula_int <- stats::as.formula(paste0(
        "event ~ ",
        design$treatment_var,
        " * factor(",
        subgroup_var,
        ")",
        " + splines::ns(",
        design$tstop_var,
        ", df = 3)",
        trial_term,
        " + offset(log(person_weeks))"
      ))

      fit <- survey::svyglm(
        formula_int,
        design = svy_design,
        family = stats::quasipoisson()
      )
      rm(svy_design)

      coef_names <- names(stats::coef(fit))
      interaction_idx <- grep(
        paste0("^", design$treatment_var, "TRUE:"),
        coef_names
      )

      if (length(interaction_idx) == 0) {
        return(list(
          p_value = NA_real_,
          subgroup_var = subgroup_var,
          n_levels = n_levels,
          interaction_coefs = data.table::data.table(),
          ratio_of_irrs = NA_real_,
          ratio_lower = NA_real_,
          ratio_upper = NA_real_
        ))
      }

      vcov_mat <- stats::vcov(fit)
      beta_int <- stats::coef(fit)[interaction_idx]
      vcov_int <- vcov_mat[interaction_idx, interaction_idx, drop = FALSE]
      # Guard against non-estimable interactions (NA coefficients or a
      # singular covariance from sparse / separated subgroup cells): drop
      # non-finite terms and return NA rather than crashing on solve().
      finite <- is.finite(beta_int) & is.finite(diag(vcov_int))
      p_value <- if (!any(finite)) {
        NA_real_
      } else {
        beta_f <- beta_int[finite]
        vcov_f <- vcov_int[finite, finite, drop = FALSE]
        wald_stat <- tryCatch(
          as.numeric(t(beta_f) %*% solve(vcov_f) %*% beta_f),
          error = function(e) NA_real_
        )
        if (is.na(wald_stat)) {
          NA_real_
        } else {
          stats::pchisq(wald_stat, df = length(beta_f), lower.tail = FALSE)
        }
      }

      fit_summary <- summary(fit)$coefficients
      interaction_coefs <- data.table::data.table(
        term = coef_names[interaction_idx],
        estimate = fit_summary[interaction_idx, "Estimate"],
        se = fit_summary[interaction_idx, "Std. Error"],
        p = fit_summary[interaction_idx, "Pr(>|t|)"]
      )

      # Binary subgroup: one interaction term -> ratio of stratum IRRs.
      if (n_levels == 2L && length(interaction_idx) == 1L) {
        b <- fit_summary[interaction_idx, "Estimate"]
        s <- fit_summary[interaction_idx, "Std. Error"]
        ratio <- exp(b)
        ratio_lower <- exp(b - 1.96 * s)
        ratio_upper <- exp(b + 1.96 * s)
      } else {
        ratio <- NA_real_
        ratio_lower <- NA_real_
        ratio_upper <- NA_real_
      }
      rm(fit)

      list(
        p_value = p_value,
        subgroup_var = subgroup_var,
        n_levels = n_levels,
        interaction_coefs = interaction_coefs,
        ratio_of_irrs = ratio,
        ratio_lower = ratio_lower,
        ratio_upper = ratio_upper
      )
    },

    #' @description Stratified IRRs within each level of a baseline subgroup.
    #'
    #' Returns one table with an `"all"` row (= `irr()`) plus one row per
    #' subgroup level, each fit on that stratum's rows via the shared
    #' estimation core. The effect-modification test p-value (and, for a binary
    #' subgroup, the ratio of stratum IRRs) is attached as an attribute.
    #' Strata with no events or only one treatment arm degrade to NA with a
    #' warning; NA-subgroup rows are dropped (count attached as an attribute).
    #'
    #' @param weight_col Character, required. Column name for weights.
    #' @param subgroup_var Character, required. A categorical baseline column.
    #' @return A data.table with columns `level, IRR, IRR_lower, IRR_upper,
    #'   IRR_pvalue, warn`, with attributes `em_pvalue`, `ratio_of_irrs`, and
    #'   `n_na_subgroup`.
    irr_by_subgroup = function(weight_col, subgroup_var) {
      if (self$data_level != "trial") {
        stop("irr_by_subgroup() requires trial level data.")
      }
      design <- self$design
      data <- self$data
      if (!weight_col %in% names(data)) {
        stop("weight_col '", weight_col, "' not found in data")
      }
      if (!subgroup_var %in% names(data)) {
        stop("subgroup_var '", subgroup_var, "' not found in data")
      }
      if (!"event" %in% names(data)) {
        stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
      }
      ipw_only_cols <- c("ipw", "ipw_trunc")
      if (
        weight_col %in%
          ipw_only_cols &&
          "prepare_outcome" %in% self$steps_completed &&
          !identical(self$estimand, "itt")
      ) {
        stop(
          "Cannot use '",
          weight_col,
          "' as weight_col after per-protocol censoring.\n",
          "Use a per-protocol weight (e.g. 'analysis_weight_pp_trunc'), or ",
          "prepare with estimand = \"itt\"."
        )
      }

      treatment_var <- design$treatment_var
      n_na <- data[is.na(get(subgroup_var)), .N]
      d <- data[!is.na(get(subgroup_var))]
      sg_levels <- sort(unique(d[[subgroup_var]]))
      if (length(sg_levels) < 2L) {
        stop(
          "subgroup_var '",
          subgroup_var,
          "' must have >= 2 non-NA levels."
        )
      }

      na_row <- function(level_label) {
        data.table::data.table(
          level = level_label,
          IRR = NA_real_,
          IRR_lower = NA_real_,
          IRR_upper = NA_real_,
          IRR_pvalue = NA_real_,
          warn = TRUE
        )
      }
      fit_one <- function(subset, level_label) {
        # Need both treatment arms AND >= 1 event in EACH arm. Zero events in
        # one arm causes separation (infinite IRR), not a clean error, so
        # preflight it rather than relying on the fit to fail.
        ev_by_arm <- subset[,
          sum(event, na.rm = TRUE),
          by = c(treatment_var)
        ]
        if (nrow(ev_by_arm) < 2L || any(ev_by_arm$V1 == 0L)) {
          warning(
            "irr_by_subgroup: stratum '",
            level_label,
            "' has no events in one or both treatment arms; returning NA."
          )
          return(na_row(level_label))
        }
        r <- tryCatch(
          private$.fit_irr(subset, weight_col),
          error = function(e) {
            warning(
              "irr_by_subgroup: fit failed for stratum '",
              level_label,
              "': ",
              conditionMessage(e)
            )
            NULL
          }
        )
        if (is.null(r)) {
          return(na_row(level_label))
        }
        data.table::data.table(
          level = level_label,
          IRR = r$IRR,
          IRR_lower = r$IRR_lower,
          IRR_upper = r$IRR_upper,
          IRR_pvalue = r$IRR_pvalue,
          warn = r$warn
        )
      }

      rows <- list(fit_one(data, "all"))
      for (lv in sg_levels) {
        rows[[length(rows) + 1L]] <- fit_one(
          d[get(subgroup_var) == lv],
          as.character(lv)
        )
      }
      out <- data.table::rbindlist(rows)

      emt <- tryCatch(
        self$effect_modification_test(weight_col, subgroup_var),
        error = function(e) NULL
      )
      data.table::setattr(
        out,
        "em_pvalue",
        if (is.null(emt)) NA_real_ else emt$p_value
      )
      data.table::setattr(
        out,
        "ratio_of_irrs",
        if (is.null(emt)) NA_real_ else emt$ratio_of_irrs
      )
      data.table::setattr(out, "n_na_subgroup", n_na)
      data.table::setattr(out, "swereg_type", "irr_by_subgroup")
      out
    },

    #' @description Weighted discrete-time survival curve from the person-week
    #' panel. Per treatment arm and period, forms the weighted hazard
    #' `h(t) = sum(w * event) / sum(w)` from the (possibly time-varying) weight
    #' column `weight_col`, then `S(t) = prod(1 - h(t))`. Because it works on the
    #' full panel (not one row per subject), it accepts time-varying weights:
    #' pass a baseline IPW column for the ITT/IPW curve, or a per-protocol weight
    #' (e.g. `"analysis_weight_pp_trunc"`) for the PP curve. The weight is applied
    #' to each at-risk row exactly as in `$rates()`/`$irr()`, so the curve shares
    #' their weighting convention. Deaths are censored, not modelled as a
    #' competing risk, so `surv` is cause-specific event-free survival under
    #' independent censoring; `1 - surv` is therefore cause-specific failure, NOT
    #' a real-world cumulative incidence (which would require a competing-risk
    #' estimator). This is a descriptive weighted curve, not the MSM-standardised
    #' survival estimator. Returned rows are post-interval survival at each
    #' observed `tstop`.
    #' @param weight_col Character, required. Weight column (time-varying allowed).
    #' @param save_path Character or NULL. If specified, saves the plot.
    #' @param title Character or NULL. Plot title (left-aligned to the whole plot).
    #' @param subtitle Character or NULL. Plot subtitle under the title.
    #' @param ylim Numeric length-2 or NULL. y-axis zoom (e.g. `c(0.95, 1)`) via
    #'   `coord_cartesian`, so steps outside the range are clipped, not dropped.
    #'   `NULL` (default) auto-scales -- which for a rare outcome zooms near 100%
    #'   and can visually exaggerate small absolute differences; set an explicit,
    #'   pre-specified range for publication figures.
    #' @param arm_labels Named character/list with `intervention` and
    #'   `comparator` (e.g. from `.lookup_arm_labels()`), used for the legend
    #'   labels. `NULL` (default) falls back to "Intervention"/"Comparator".
    #' @param scale Character, y scale of the saved plot. `"survival"`
    #'   (default) plots `surv`, starting at full survival.
    #'   `"cumulative_failure"` plots `1 - surv`, starting at 0 --
    #'   cause-specific failure, not a competing-risk cumulative incidence
    #'   function (see above). Ignored when `save_path` is NULL, since no plot
    #'   is built.
    #' @return A data.table with columns `treatment_var`, `tstop`, `events`
    #'   (weighted), `at_risk` (weighted), `n_persons_at_risk`, `hazard`, `surv`
    #'   (invisibly if `save_path` is specified; a `group` column is also added
    #'   when plotting).
    #'
    #'   `at_risk` and `n_persons_at_risk` answer different questions and both
    #'   are returned. `at_risk` is the weighted risk set, `sum(w)`, and is the
    #'   denominator of the hazard. `n_persons_at_risk` is an unweighted count
    #'   of distinct people, taken over `design$person_id_var`, and is the
    #'   number a risk table under a survival panel reports. It is not a row
    #'   count: the panel holds one row per person-trial-band and a person
    #'   contributes several sequential trials, so rows exceed people.
    #'   `$rates()` reports the same idea at whole-arm grain under the name
    #'   `n_persons`; the two names differ because the grain differs.
    survival_curve = function(
      weight_col,
      save_path = NULL,
      title = NULL,
      subtitle = NULL,
      ylim = NULL,
      arm_labels = NULL,
      scale = c("survival", "cumulative_failure")
    ) {
      if (self$data_level != "trial") {
        stop(
          "survival_curve() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'"
        )
      }
      scale <- match.arg(scale)

      design <- self$design
      data <- self$data

      if (!weight_col %in% names(data)) {
        stop("weight_col '", weight_col, "' not found in data")
      }
      if (!"event" %in% names(data)) {
        stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
      }

      tvar <- design$treatment_var
      time_var <- design$tstop_var

      # Validate analytic inputs loudly: a single NA weight/event otherwise
      # silently poisons every downstream survival value via cumprod().
      w <- data[[weight_col]]
      if (!is.numeric(w) || anyNA(w) || any(!is.finite(w)) || any(w < 0)) {
        stop(
          "weight_col '",
          weight_col,
          "' must be numeric, finite, non-missing and non-negative"
        )
      }
      if (anyNA(data$event) || !all(data$event %in% c(0L, 1L))) {
        stop("'event' must be a non-missing 0/1 indicator")
      }

      # Weighted discrete-time hazard per arm-period. The weight is applied to
      # each present (at-risk) row exactly as in $rates()/$irr(), so the curve
      # and the reported IRR share one weighting convention.
      #
      # `n_persons_at_risk` is a plain head count of distinct people, for the
      # risk table a reader expects under a survival panel. It is deliberately
      # NOT `.N`: the panel is one row per person-trial-band, and a person
      # contributes several sequential trials, so `.N` counts person-trials.
      # It is also not `at_risk`, which is the weighted risk set.
      curve <- data[,
        .(
          events = sum(get(weight_col) * event),
          at_risk = sum(get(weight_col)),
          n_persons_at_risk = data.table::uniqueN(get(design$person_id_var))
        ),
        keyby = c(tvar, time_var)
      ]
      if (any(curve$at_risk <= 0)) {
        stop("weighted risk set (sum of weights) is <= 0 in an arm-period")
      }
      curve[, hazard := events / at_risk]
      curve[, surv := cumprod(1 - hazard), by = c(tvar)]

      if (is.null(save_path)) {
        return(curve[])
      }

      tv <- curve[[tvar]]
      if (!is.logical(tv) && !all(tv %in% c(0L, 1L))) {
        stop(
          "plotting requires a logical (or 0/1) '",
          tvar,
          "'; got class '",
          class(tv)[1],
          "'"
        )
      }
      # The study's own arm labels when supplied, else generic ones;
      # intervention is red, comparator blue, intervention first.
      labs <- .tte_arm_labels_resolved(arm_labels)
      int_lab <- labs[["intervention"]]
      cmp_lab <- labs[["comparator"]]
      curve[, group := fifelse(as.logical(get(tvar)), int_lab, cmp_lab)]

      q <- .render_survival_curve(
        curve = curve,
        time_var = time_var,
        scale = scale,
        title = title,
        subtitle = subtitle,
        ylim = ylim,
        int_lab = int_lab,
        cmp_lab = cmp_lab
      )

      ggplot2::ggsave(save_path, q, width = 8, height = 6, dpi = 300)
      invisible(curve[])
    },

    #' @description Signed cause-specific risk difference at each band, with a
    #' percentile bootstrap interval resampled at the person level.
    #'
    #' The two arm-specific curves are the ones `$survival_curve()` builds, from
    #' the same weighted discrete-time hazard, so the point estimate here and
    #' the curve in the figure are the same numbers.
    #'
    #' The sign convention is fixed:
    #'
    #' `RD(t) = Risk_intervention(t) - Risk_comparator(t)`, which equals
    #' `S_comparator(t) - S_intervention(t)`.
    #'
    #' The returned `rd` is signed. A protective intervention gives a negative
    #' risk difference; that minus sign is the result and is never removed.
    #'
    #' The bootstrap resamples PERSONS, not person-trials and not rows. A woman
    #' contributes several sequential trials that share her baseline covariates
    #' and can carry the same outcome event, so her trials are not exchangeable;
    #' the person is the cluster. One multiplicity vector is drawn per replicate
    #' and applied to both arms, because a woman can be a comparator in an early
    #' trial and an initiator in a later one, and a separate draw per arm would
    #' discard the covariance between the two arms and bias the interval while
    #' leaving the point estimate untouched.
    #'
    #' A replicate that draws no person for an arm, or that empties a band,
    #' yields `NA` for that band and onwards. The percentile step drops those.
    #'
    #' A zero-event arm gets no interval. When either arm has no
    #' positive-weight event through a horizon, `rd_lo` and `rd_hi` are `NA`
    #' there and `interval_status` reads `"zero-event arm"`. An ordinary
    #' empirical bootstrap cannot produce an event the sample does not hold, so
    #' every replicate assigns that arm a failure risk of exactly zero. The
    #' percentiles then describe the other arm alone, which is
    #' anti-conservative, and more replicates do not repair it. The condition is
    #' evaluated per horizon and per arm, on the events up to and including that
    #' band.
    #'
    #' Deaths are censored, not modelled as a competing risk, so this is a
    #' cause-specific risk difference under independent censoring, not a
    #' competing-risk one.
    #' @param weight_col Character, required. Weight column (time-varying
    #'   allowed), as in `$survival_curve()`.
    #' @param n_boot Integer, number of bootstrap replicates (default 500).
    #' @param seed Integer or NULL. When given, the draw is reproducible; the
    #'   caller's random stream is restored afterwards.
    #' @param conf_level Numeric in (0, 1), percentile interval level
    #'   (default 0.95).
    #' @return A data.table with one row per band and columns `tstop` (named
    #'   after `design$tstop_var`), `surv_comparator`, `surv_intervention`,
    #'   `rd`, `rd_lo`, `rd_hi`, `interval_status`, `nnt`, `nnt_direction`,
    #'   `n_persons_with_event_comparator` and
    #'   `n_persons_with_event_intervention`.
    #'
    #'   `interval_status` takes one of three values. `"ok"` means the interval
    #'   is estimable and strictly excludes the null. `"spans null"` means the
    #'   interval is estimable and contains the null. `"zero-event arm"` means
    #'   there is no interval. A reader can therefore separate an interval that
    #'   spans the null from one that does not exist.
    #'
    #'   `nnt` is the signed number needed to treat, `-1/rd`. `nnt_direction`
    #'   reads `"benefit"`, `"harm"` or `NA_character_`, and it is the stored
    #'   decision every formatter reads. No formatter re-derives the direction
    #'   from a sign, so a figure and a results sheet cannot disagree about one
    #'   band.
    #'
    #'   The two event columns count distinct PEOPLE who had the outcome at or
    #'   before that band, in that arm. They are deliberately not row counts and
    #'   not person-trial counts: the panel holds one row per
    #'   person-trial-band, and one woman can carry the event in two of her
    #'   sequential trials, which is one person who had the outcome. `$rates()`
    #'   and `$summary()` report the event ROW count instead, and on real data
    #'   the two numbers differ.
    #'
    #'   The replicate matrix the interval was read off is attached as the
    #'   `rd_boot` attribute (`n_boot` rows by one column per band), alongside
    #'   `conf_level` and `n_boot`.
    risk_difference = function(
      weight_col,
      n_boot = 500L,
      seed = NULL,
      conf_level = 0.95
    ) {
      if (self$data_level != "trial") {
        stop(
          "risk_difference() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'"
        )
      }
      design <- self$design

      if (!is.null(seed)) {
        has_old <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
        old_seed <- if (has_old) {
          get(".Random.seed", envir = globalenv(), inherits = FALSE)
        } else {
          NULL
        }
        on.exit(
          {
            if (is.null(old_seed)) {
              if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
                rm(".Random.seed", envir = globalenv())
              }
            } else {
              assign(".Random.seed", old_seed, envir = globalenv())
            }
          },
          add = TRUE
        )
        set.seed(seed)
      }

      .tte_rd_curve(
        data = self$data,
        person_id_var = design$person_id_var,
        id_var = design$id_var,
        treatment_var = design$treatment_var,
        time_var = design$tstop_var,
        weight_col = weight_col,
        n_boot = n_boot,
        conf_level = conf_level
      )
    }
  ),

  private = list(
    .schema_version = NULL,

    # =========================================================================
    # Private methods — internal implementation details
    # =========================================================================

    # --- .fit_irr: weighted Poisson MSM fit for one data subset -------------
    # The estimation core shared by irr() and irr_by_subgroup(). The caller is
    # responsible for the guards (weight validity, required columns); this fits
    # the model on whatever `data` it is handed and returns the one-row IRR
    # data.table. Calendar trial_term matches irr() exactly.
    .fit_irr = function(data, weight_col) {
      design <- self$design

      has_trial_id <- "trial_id" %in%
        names(data) &&
        data[, data.table::uniqueN(trial_id)] > 1L
      n_trial_ids <- if (has_trial_id) {
        data[, data.table::uniqueN(trial_id)]
      } else {
        0L
      }

      # Subset to only needed columns to reduce svydesign memory footprint
      keep_cols <- unique(c(
        design$person_id_var,
        design$treatment_var,
        design$tstop_var,
        weight_col,
        "event",
        "person_weeks",
        if (has_trial_id) "trial_id"
      ))
      svy_data <- data[, ..keep_cols]

      svy_design <- survey::svydesign(
        ids = as.formula(paste0("~", design$person_id_var)),
        weights = as.formula(paste0("~", weight_col)),
        data = svy_data
      )
      rm(svy_data)

      warn <- FALSE
      treatment_coef <- paste0(design$treatment_var, "TRUE")

      trial_term <- if (has_trial_id && n_trial_ids >= 5L) {
        paste0(" + splines::ns(trial_id, df = 3)")
      } else if (has_trial_id) {
        " + trial_id"
      } else {
        ""
      }

      formula <- stats::as.formula(paste0(
        "event ~ ",
        design$treatment_var,
        " + splines::ns(",
        design$tstop_var,
        ", df = 3)",
        trial_term,
        " + offset(log(person_weeks))"
      ))
      poisson_fit <- withCallingHandlers(
        survey::svyglm(
          formula,
          design = svy_design,
          family = stats::quasipoisson()
        ),
        warning = function(w) {
          warn <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
      rm(svy_design)
      fit_summary <- summary(poisson_fit)$coefficients
      if (!treatment_coef %in% rownames(fit_summary)) {
        # logical/factor treatment yields '<var>TRUE'; numeric 0/1 yields
        # just '<var>'
        if (design$treatment_var %in% rownames(fit_summary)) {
          treatment_coef <- design$treatment_var
        } else {
          stop(
            "treatment coefficient '",
            treatment_coef,
            "' not found in the outcome model; available: ",
            paste(rownames(fit_summary), collapse = ", ")
          )
        }
      }
      coef <- fit_summary[treatment_coef, "Estimate"]
      se <- fit_summary[treatment_coef, "Std. Error"]
      pvalue <- fit_summary[treatment_coef, "Pr(>|t|)"]
      rm(poisson_fit)

      result <- data.table::data.table(
        IRR = exp(coef),
        IRR_lower = exp(coef - 1.96 * se),
        IRR_upper = exp(coef + 1.96 * se),
        IRR_pvalue = pvalue,
        warn = warn
      )
      data.table::setattr(result, "swereg_type", "irr")
      result
    },

    # --- enroll: band-based matching + collapse + panel expansion -----------
    # Phase order: A (assign bands) -> C (match on band summary) ->
    #   B (collapse enrolled persons) -> D (expand panels at band level)
    # When enrolled_ids is provided (pre-matched mode from two-pass pipeline),
    # Phase C is skipped entirely.
    enroll = function(
      ratio = 2,
      seed = NULL,
      extra_cols = NULL,
      enrolled_ids = NULL
    ) {
      if (self$data_level != "person_week") {
        stop(
          "enroll() requires person_week level data.\n",
          "Current data_level: '",
          self$data_level,
          "'\n",
          "Hint: Pass ratio to TTEEnrollment$new() with person_id_var in design."
        )
      }

      design <- self$design
      data <- self$data
      person_id_col <- design$person_id_var
      treatment_col <- design$treatment_var
      eligible_col <- design$eligible_var
      follow_up <- design$follow_up_time
      period_width <- design$period_width

      if (!"isoyearweek" %in% names(data)) {
        stop("Band-based enrollment requires 'isoyearweek' column in data")
      }

      if (!is.null(seed)) {
        set.seed(seed)
      }

      # ---- Phase A: Assign universal trial IDs from isoyearweek ----
      .assign_trial_ids(data, period_width)

      id_var <- design$id_var

      if (!is.null(enrolled_ids)) {
        # ---- Pre-matched mode: build entry_dt from enrolled_ids ----
        # Filter to persons in this batch
        enrolled_ids <- data.table::copy(enrolled_ids)
        batch_persons <- unique(data[[person_id_col]])
        entry_dt <- enrolled_ids[get(person_id_col) %in% batch_persons]
        if (nrow(entry_dt) == 0L) {
          # No enrolled persons in this batch — return empty panel
          self$data <- data[0L]
          self$data_level <- "trial"
          self$steps_completed <- c(self$steps_completed, "enroll")
          return(invisible(self))
        }
        data.table::setnames(entry_dt, person_id_col, ".tte_person_id")
        entry_dt[, entry_band_id := trial_id]
        entry_dt[, baseline_tx := intervention]
        entry_dt[,
          (id_var) := stringi::stri_c(.tte_person_id, ".", entry_band_id)
        ]
        enrolled_person_ids <- unique(entry_dt$.tte_person_id)
      } else {
        # ---- Phase C: Per-band stratified matching ----
        # C-prep: Create band-level summary from eligible rows only
        if (is.null(eligible_col)) {
          eligible_rows <- data
        } else {
          eligible_rows <- data[get(eligible_col) == TRUE]
        }

        # Explicit time ordering so first() picks the earliest week in each band
        data.table::setorderv(
          eligible_rows,
          c(person_id_col, "trial_id", "isoyearweek")
        )

        band_summary <- eligible_rows[,
          .(
            band_treatment = data.table::first(get(treatment_col))
          ),
          by = c(person_id_col, "trial_id")
        ]

        # C-match: Within each band, sample comparator at ratio:1
        intervention_bands <- band_summary[band_treatment == TRUE]
        comparator_bands <- band_summary[band_treatment == FALSE]

        if (nrow(intervention_bands) == 0) {
          stop("No intervention person-bands found among eligible rows.")
        }

        # Per-band stratified matching
        intervention_count <- intervention_bands[, .N, by = trial_id]
        data.table::setnames(intervention_count, "N", "n_intervention")

        # Sample comparator within each band independently
        matched_comparator <- comparator_bands[
          intervention_count,
          on = "trial_id",
          nomatch = NULL,
          allow.cartesian = FALSE
        ][,
          {
            n_to_sample <- min(round(ratio * n_intervention), .N)
            .SD[sample(.N, n_to_sample)]
          },
          by = trial_id
        ]
        matched_comparator[, n_intervention := NULL]

        # Combine: entry_dt with (person_id, trial_id, baseline_intervention)
        intervention_bands[, baseline_tx := TRUE]
        matched_comparator[, baseline_tx := FALSE]
        entry_dt <- data.table::rbindlist(list(
          intervention_bands[,
            c(person_id_col, "trial_id", "baseline_tx"),
            with = FALSE
          ],
          matched_comparator[,
            c(person_id_col, "trial_id", "baseline_tx"),
            with = FALSE
          ]
        ))
        data.table::setnames(entry_dt, person_id_col, ".tte_person_id")
        entry_dt[, entry_band_id := trial_id]

        # enrollment_person_trial_id format: "person_id.entry_band_id"
        entry_dt[,
          (id_var) := stringi::stri_c(.tte_person_id, ".", entry_band_id)
        ]

        enrolled_person_ids <- unique(entry_dt$.tte_person_id)
      }

      # ---- Phase B: Full collapse (enrolled persons only) ----
      # If the caller (e.g. .s1c_worker) has already filtered `data` to
      # enrolled persons upstream, skip the filter here -- otherwise the
      # `[i, on = key]` join allocates another ~3 GB identity copy of
      # the panel. The attribute is set on the data.table by the caller.
      if (isTRUE(attr(data, ".tte_filtered_to_enrolled"))) {
        data_enrolled <- data
      } else {
        # Binary-search join on the existing (id, isoyearweek) key beats
        # `%in%` for selecting enrolled persons from a multi-million-row
        # panel: O(M log N) vs O(N + M) hash, but more importantly avoids
        # the temporary hash allocation that drives GC pressure here.
        data_enrolled <- data[
          .(unique(enrolled_person_ids)),
          on = person_id_col,
          nomatch = NULL
        ]
      }

      # Columns to aggregate
      collapse_first_cols <- unique(c(
        design$confounder_vars,
        if (!is.null(design$admin_censor_isoyearweek)) "isoyearweek",
        extra_cols
      ))
      collapse_first_cols <- intersect(
        collapse_first_cols,
        names(data_enrolled)
      )

      collapse_last_cols <- character(0)
      if (!is.null(design$time_treatment_var)) {
        collapse_last_cols <- intersect(
          design$time_treatment_var,
          names(data_enrolled)
        )
      }

      collapse_max_cols <- intersect(design$outcome_vars, names(data_enrolled))

      # Aggregate within each (person_id, trial_id) — single pass.
      # setkeyv sorts in place AND marks the key, replacing the previous
      # setorderv → setkeyv pair (two sorts). Include isoyearweek in the
      # key so first(isoyearweek) inside the aggregation is deterministic.
      # `by = c(pid, trial_id)` still uses binary-search grouping because
      # data.table honors partial-key by clauses.
      by_cols <- c(person_id_col, "trial_id")
      data.table::setkeyv(
        data_enrolled,
        c(person_id_col, "trial_id", "isoyearweek")
      )

      # Build aggregation expression list
      agg_exprs <- list(
        isoyearweek = quote(data.table::first(isoyearweek)),
        .n_source_weeks = quote(.N)
      )
      for (col in collapse_first_cols) {
        if (col != "isoyearweek") {
          agg_exprs[[col]] <- substitute(
            data.table::first(x),
            list(x = as.name(col))
          )
        }
      }
      for (col in collapse_last_cols) {
        agg_exprs[[col]] <- substitute(
          data.table::last(x),
          list(x = as.name(col))
        )
      }
      for (col in collapse_max_cols) {
        agg_exprs[[col]] <- substitute(
          max(x, na.rm = TRUE),
          list(x = as.name(col))
        )
      }

      band_data <- data_enrolled[,
        eval(as.call(c(quote(list), agg_exprs))),
        by = by_cols
      ]

      # ---- Phase D: Panel expansion at band level ----
      n_follow_up_bands <- ceiling(follow_up / period_width)

      data.table::setnames(band_data, person_id_col, ".tte_person_id")

      # CJ-style expansion: for each entry, create one row per follow-up band
      # then join against band_data
      # Remove trial_id from entry_dt before expansion (it's in entry_band_id)
      if ("trial_id" %in% names(entry_dt)) {
        entry_dt[, trial_id := NULL]
      }

      expanded <- entry_dt[,
        .(
          trial_id = seq(entry_band_id, entry_band_id + n_follow_up_bands - 1L)
        ),
        by = c(id_var, ".tte_person_id", "baseline_tx", "entry_band_id")
      ]

      # Keyed binary join replaces hash-based merge for Phase D
      data.table::setkey(expanded, .tte_person_id, trial_id)
      data.table::setkey(band_data, .tte_person_id, trial_id)
      panel <- band_data[expanded, nomatch = NULL]

      # Rename entry_band_id to trial_id (the actual trial identifier)
      # trial_id already exists from the expansion, so just drop entry_band_id
      panel[, entry_band_id := NULL]

      # Clean up join columns
      cols_to_remove <- intersect(
        "band_treatment",
        names(panel)
      )
      if (length(cols_to_remove) > 0) {
        panel[, (cols_to_remove) := NULL]
      }

      data.table::setnames(panel, ".tte_person_id", person_id_col)

      # Override treatment with matching decision
      panel[, (treatment_col) := baseline_tx]
      panel[, baseline_tx := NULL]

      # trial_week: 0-indexed band offset from enrollment band
      panel[, trial_week := (seq_len(.N) - 1L) * period_width, by = c(id_var)]

      # tstart/tstop in week units
      panel[, tstart := trial_week]
      panel[, tstop := trial_week + period_width]
      panel[, person_weeks := .n_source_weeks]
      panel[, .n_source_weeks := NULL]

      self$data <- panel
      self$data_level <- "trial"
      self$steps_completed <- c(self$steps_completed, "enroll")
      invisible(self)
    },

    # --- s5_prepare_outcome: define event, censoring, and follow-up boundaries --
    #
    # Protocol deviation detection uses `time_treatment_var`:
    # - TRUE: person remains on assigned treatment arm
    # - FALSE: person switched to the opposite arm
    # - NA: indeterminate status (treated as protocol deviation)
    #
    # Ensure `time_treatment_var` is non-missing for periods where the person
    # is known to remain on their assigned arm.
    s5_prepare_outcome = function(outcome, follow_up = NULL, estimand = "pp") {
      # admin_censor_var is stored by TTEDesign but has no implementation:
      # failing loudly beats silently skipping the administrative censoring
      # the caller asked for.
      if (!is.null(self$design$admin_censor_var)) {
        stop(
          "admin_censor_var is not implemented in s5_prepare_outcome(); ",
          "use admin_censor_isoyearweek instead"
        )
      }
      if (self$data_level != "trial") {
        stop(
          "s5_prepare_outcome() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'\n",
          "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level."
        )
      }

      if ("prepare_outcome" %in% self$steps_completed) {
        stop(
          "s5_prepare_outcome() can only be run once per trial (it deletes rows)"
        )
      }

      design <- self$design
      data <- self$data

      if (!outcome %in% design$outcome_vars) {
        stop(
          "outcome must be one of: ",
          paste(design$outcome_vars, collapse = ", ")
        )
      }

      self$active_outcome <- outcome

      # weeks_to_event
      data[,
        weeks_to_event := {
          event_rows <- which(get(outcome) == 1)
          if (length(event_rows) > 0) {
            min(get(design$tstop_var)[event_rows])
          } else {
            NA_integer_
          }
        },
        by = c(design$id_var)
      ]

      # weeks_to_protocol_deviation
      # ITT keeps follow-up through treatment switching, so deviation never
      # censors and no switch variable is needed -- set it to NA so it drops
      # out of every pmin below and out of the censor_this_period indicator.
      # PP requires time_treatment_var and censors at the first deviation.
      if (estimand == "itt") {
        data[, weeks_to_protocol_deviation := NA_integer_]
      } else {
        if (is.null(design$time_treatment_var)) {
          stop(
            "design must have time_treatment_var for per-protocol censoring analysis"
          )
        }
        data[,
          .protocol_deviated := data.table::fcase(
            get(design$treatment_var) == TRUE & (get(design$time_treatment_var) == FALSE | is.na(get(design$time_treatment_var))) ,
            TRUE                                                                                                                  ,
            get(design$treatment_var) == FALSE & (get(design$time_treatment_var) == TRUE | is.na(get(design$time_treatment_var))) ,
            TRUE                                                                                                                  ,
            default = FALSE
          )
        ]
        data[,
          weeks_to_protocol_deviation := {
            if (any(.protocol_deviated)) {
              min(get(design$tstop_var)[.protocol_deviated])
            } else {
              NA_integer_
            }
          },
          by = c(design$id_var)
        ]
      }

      # weeks_to_admin_end
      if (!is.null(design$admin_censor_isoyearweek)) {
        if (!"isoyearweek" %in% names(data)) {
          stop("admin_censor_isoyearweek requires 'isoyearweek' column in data")
        }
        study_end_date <- cstime::isoyearweek_to_last_date(
          design$admin_censor_isoyearweek
        )
        data[,
          .baseline_isoyearweek := isoyearweek[get(design$tstart_var) == 0][1],
          by = c(design$id_var)
        ]
        data[,
          weeks_to_admin_end := as.integer(difftime(
            study_end_date,
            cstime::isoyearweek_to_last_date(.baseline_isoyearweek),
            units = "weeks"
          ))
        ]
        data[, .baseline_isoyearweek := NULL]

        period_width <- data[, min(get(design$tstop_var))]
        data[,
          weeks_to_admin_end := (weeks_to_admin_end %/% period_width) *
            period_width
        ]

        n_dropped <- data[
          weeks_to_admin_end < period_width,
          uniqueN(get(design$id_var))
        ]
        if (n_dropped > 0) {
          warning(
            n_dropped,
            " trial(s) will be dropped (entered < ",
            period_width,
            " weeks before admin_censor_isoyearweek)"
          )
        }
      } else {
        data[, weeks_to_admin_end := NA_integer_]
      }

      # weeks_to_loss
      effective_follow_up <- if (!is.null(follow_up)) {
        as.integer(follow_up)
      } else {
        design$follow_up_time
      }
      data[, .max_tstop := max(get(design$tstop_var)), by = c(design$id_var)]
      data[,
        .first_planned_stop := pmin(
          weeks_to_event,
          weeks_to_protocol_deviation,
          weeks_to_admin_end,
          effective_follow_up,
          na.rm = TRUE
        )
      ]
      data[,
        weeks_to_loss := data.table::fifelse(
          .max_tstop < .first_planned_stop,
          .max_tstop,
          NA_integer_
        )
      ]

      # censor_week
      data[,
        censor_week := pmin(
          weeks_to_event,
          weeks_to_protocol_deviation,
          weeks_to_loss,
          weeks_to_admin_end,
          effective_follow_up,
          na.rm = TRUE
        )
      ]

      # Filter
      data <- data[get(design$tstop_var) <= censor_week | is.na(censor_week)]

      # event indicator
      data[, event := as.integer(get(design$tstop_var) == weeks_to_event)]
      data[is.na(event), event := 0L]

      # censor_this_period indicator
      data[,
        censor_this_period := as.integer(
          get(design$tstop_var) == weeks_to_protocol_deviation |
            get(design$tstop_var) == weeks_to_loss
        )
      ]
      data[is.na(censor_this_period), censor_this_period := 0L]
      # Event takes precedence over same-band protocol deviation: in discrete
      # time the outcome is measured over the interval before within-interval
      # censoring is applied, so a person-trial whose first event falls in the
      # same band as its deviation exits the risk set through the event.
      # Without this, s4_prepare_for_analysis() drops the row as censored and
      # the event is silently lost (and the IPCW model sees a spurious
      # censoring where the trial actually ended in an event).
      data[event == 1L, censor_this_period := 0L]

      # Clean up (.protocol_deviated only exists for PP)
      tmp_cols <- intersect(
        c(".max_tstop", ".first_planned_stop", ".protocol_deviated"),
        names(data)
      )
      data[, (tmp_cols) := NULL]
      data.table::setorderv(data, c(design$id_var, design$tstop_var))

      self$data <- data
      self$steps_completed <- c(self$steps_completed, "prepare_outcome")
      invisible(self)
    },

    # --- s6_ipcw_pp: inverse probability of censoring weights (per-protocol) ----
    # Weight stabilization note:
    # Danaei (2013) describes stabilized IPCW with a numerator conditioned on
    # baseline covariates: P(uncensored | baseline covariates). Our implementation
    # uses the simpler marginal (population-average) censoring probability as the
    # numerator: cum_marginal = cumprod(mean(p_uncensored)). This is a common,
    # valid simplification that is equivalent when baseline covariates have
    # limited predictive power for censoring. The full Danaei approach would
    # require fitting a second model for the numerator.
    s6_ipcw_pp = function(
      estimate_ipcw_pp_separately_by_treatment = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      censoring_var = NULL
    ) {
      if (self$data_level != "trial") {
        stop(
          "s6_ipcw_pp() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'\n",
          "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level."
        )
      }

      if (!"ipw" %in% names(self$data)) {
        stop("s6_ipcw_pp() requires 'ipw' column. Run $s2_ipw() first.")
      }

      design <- self$design

      if (is.null(censoring_var)) {
        if ("prepare_outcome" %in% self$steps_completed) {
          censoring_var <- "censor_this_period"
        } else {
          censoring_var <- "censored"
        }
      }

      if (!censoring_var %in% names(self$data)) {
        stop(
          "censoring_var '",
          censoring_var,
          "' not found. Run $s4_prepare_for_analysis() first."
        )
      }

      working_data <- self$data[!is.na(get(design$treatment_var))]

      # --- Inline calculate_ipcw logic ---
      treatment_var <- design$treatment_var
      confounder_vars <- design$confounder_vars
      id_var <- design$id_var
      tstop_var <- design$tstop_var
      use_gam <- estimate_ipcw_pp_with_gam
      separate_by_treatment <- estimate_ipcw_pp_separately_by_treatment

      if (use_gam && !requireNamespace("mgcv", quietly = TRUE)) {
        stop(
          "Package 'mgcv' is required for use_gam = TRUE. ",
          "Install it with: install.packages('mgcv')"
        )
      }

      # Include trial_id in censoring model if available (calendar-time adjustment)
      has_trial_id <- "trial_id" %in%
        names(working_data) &&
        working_data[, data.table::uniqueN(trial_id)] > 1L

      if (use_gam) {
        trial_term <- if (
          has_trial_id && working_data[, data.table::uniqueN(trial_id)] >= 5L
        ) {
          "+ s(trial_id)"
        } else if (has_trial_id) {
          "+ trial_id"
        } else {
          ""
        }
        formula_str <- paste(
          censoring_var,
          "~ s(",
          tstop_var,
          ")",
          trial_term,
          "+",
          paste(confounder_vars, collapse = " + ")
        )
      } else {
        trial_term <- if (has_trial_id) "+ trial_id" else ""
        formula_str <- paste(
          censoring_var,
          "~",
          tstop_var,
          trial_term,
          "+",
          paste(confounder_vars, collapse = " + ")
        )
      }
      ipcw_formula <- stats::as.formula(formula_str)

      fit_and_predict <- function(mask) {
        subset_data <- working_data[mask]
        n_censor <- sum(subset_data[[censoring_var]], na.rm = TRUE)
        n_rows <- nrow(subset_data)

        # Fall back to marginal rate when model cannot be fit:
        # no censoring events, or too few rows for the model
        if (n_censor == 0L || n_censor == n_rows || n_rows < 10L) {
          working_data[mask, p_censor := mean(get(censoring_var), na.rm = TRUE)]
          return(invisible(NULL))
        }

        fit <- tryCatch(
          {
            if (use_gam) {
              mgcv::bam(
                ipcw_formula,
                data = subset_data,
                family = stats::binomial,
                discrete = TRUE
              )
            } else {
              stats::glm(
                ipcw_formula,
                data = subset_data,
                family = stats::binomial
              )
            }
          },
          error = function(e) {
            warning(
              "IPCW model failed (",
              conditionMessage(e),
              "); using marginal censoring rate as fallback."
            )
            NULL
          }
        )

        if (is.null(fit)) {
          working_data[mask, p_censor := mean(get(censoring_var), na.rm = TRUE)]
        } else {
          working_data[
            mask,
            p_censor := stats::predict(fit, .SD, type = "response")
          ]
          rm(fit)
        }
        rm(subset_data)
        gc()
      }

      if (separate_by_treatment) {
        tx_mask <- working_data[[treatment_var]] == TRUE
        fit_and_predict(tx_mask)
        fit_and_predict(!tx_mask)
      } else {
        fit_and_predict(rep(TRUE, nrow(working_data)))
      }

      working_data[, p_uncensored := 1 - p_censor]
      data.table::setorderv(working_data, c(id_var, tstop_var))
      working_data[, cum_p_uncensored := cumprod(p_uncensored), by = c(id_var)]

      if (separate_by_treatment) {
        marginal <- working_data[,
          .(marginal_p = mean(p_uncensored)),
          by = c(tstop_var, treatment_var)
        ]
        working_data[
          marginal,
          marginal_p := i.marginal_p,
          on = c(tstop_var, treatment_var)
        ]
      } else {
        marginal <- working_data[,
          .(marginal_p = mean(p_uncensored)),
          by = c(tstop_var)
        ]
        working_data[marginal, marginal_p := i.marginal_p, on = tstop_var]
      }
      data.table::setorderv(working_data, c(id_var, tstop_var))
      working_data[, cum_marginal := cumprod(marginal_p), by = c(id_var)]
      working_data[, ipcw_pp := cum_marginal / cum_p_uncensored]

      ipcw_value_cols <- intersect(
        c(
          "p_censor",
          "p_uncensored",
          "cum_p_uncensored",
          "marginal_p",
          "cum_marginal",
          "ipcw_pp"
        ),
        names(working_data)
      )

      for (col in ipcw_value_cols) {
        if (col %in% names(self$data)) self$data[, (col) := NULL]
      }

      join_on <- c(design$id_var, design$tstop_var)
      self$data[
        working_data,
        (ipcw_value_cols) := mget(paste0("i.", ipcw_value_cols)),
        on = join_on
      ]

      rm(working_data)

      self$data[, analysis_weight_pp := ipw * ipcw_pp]

      self$data <- private$.truncate_weights(
        data = self$data,
        weight_cols = "analysis_weight_pp",
        lower = 0.01,
        upper = 0.99,
        suffix = "_trunc"
      )

      self$weight_cols <- unique(c(
        self$weight_cols,
        "ipcw_pp",
        "analysis_weight_pp",
        "analysis_weight_pp_trunc"
      ))
      self$steps_completed <- c(
        self$steps_completed,
        "ipcw",
        "weights",
        "truncate"
      )

      drop_cols <- intersect(
        c(
          "p_censor",
          "p_uncensored",
          "cum_p_uncensored",
          "marginal_p",
          "cum_marginal"
        ),
        names(self$data)
      )
      if (length(drop_cols) > 0) {
        self$data[, (drop_cols) := NULL]
      }

      invisible(self)
    },

    # --- combine_weights: multiply IPW x IPCW into a single column ----------
    combine_weights = function(
      ipw_col = "ipw",
      ipcw_col = "ipcw_pp",
      name = "analysis_weight_pp"
    ) {
      if (self$data_level != "trial") {
        stop(
          "combine_weights() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'\n",
          "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level."
        )
      }

      if (!ipw_col %in% names(self$data)) {
        stop("ipw_col '", ipw_col, "' not found in data")
      }
      if (!ipcw_col %in% names(self$data)) {
        stop("ipcw_col '", ipcw_col, "' not found in data")
      }
      self$data[, (name) := get(ipw_col) * get(ipcw_col)]

      self$weight_cols <- unique(c(self$weight_cols, name))
      self$steps_completed <- c(self$steps_completed, "weights")
      invisible(self)
    },

    # =========================================================================
    # Private weight/matching/collapse helpers
    # =========================================================================

    # --- .truncate_weights: clip extreme weights at quantile bounds ----------
    .truncate_weights = function(
      data,
      weight_cols,
      lower = 0.01,
      upper = 0.99,
      suffix = "_trunc"
    ) {
      if (!data.table::is.data.table(data)) {
        stop("data must be a data.table")
      }
      if (!is.character(weight_cols) || length(weight_cols) == 0) {
        stop("weight_cols must be a non-empty character vector")
      }
      missing_cols <- setdiff(weight_cols, names(data))
      if (length(missing_cols) > 0) {
        stop(
          "Columns not found in data: ",
          paste(missing_cols, collapse = ", ")
        )
      }
      if (
        !is.numeric(lower) ||
          !is.numeric(upper) ||
          lower < 0 ||
          upper > 1 ||
          lower >= upper
      ) {
        stop("lower and upper must be numeric with 0 <= lower < upper <= 1")
      }

      for (col in weight_cols) {
        bounds <- stats::quantile(data[[col]], c(lower, upper), na.rm = TRUE)
        new_col <- paste0(col, suffix)
        data[, (new_col) := pmin(pmax(get(col), bounds[1]), bounds[2])]
      }

      data
    }
  ),

  active = list(
    #' @field enrollment_stage Derived lifecycle stage (read-only).
    #' Returns `"pre_enrollment"` when `data_level == "person_week"`,
    #' `"analysis_ready"` when `s5_prepare_outcome` has been run,
    #' or `"enrolled"` otherwise.
    enrollment_stage = function() {
      if (self$data_level == "person_week") {
        return("pre_enrollment")
      }
      if ("prepare_outcome" %in% self$steps_completed) {
        return("analysis_ready")
      }
      "enrolled"
    }
  )
)


# =============================================================================
# S3 method: summary.TTEEnrollment
# =============================================================================

#' @export
summary.TTEEnrollment <- function(object, ..., pretty = FALSE) {
  object$summary(pretty = pretty)
}


# =============================================================================
# Standalone helpers (operate on lists of trials/results)
# =============================================================================

#' Combine multiple enrollment objects
#'
#' Combines multiple [TTEEnrollment] objects by row-binding their data. Used for
#' batched processing where data is too large to fit in memory at once.
#'
#' @param trials A list of [TTEEnrollment] objects to combine.
#'
#' @return A new [TTEEnrollment] object with combined data.
#'
#' @details
#' All trials must have the same design and data_level. The combined trial inherits:
#' - The design and data_level from the first trial
#' - The intersection of steps_completed from all trials
#' - The union of weight_cols from all trials
#'
#' @examples
#' \dontrun{
#' trials <- lapply(files, function(f) {
#'   TTEEnrollment$new(load_data(f), design, ratio = 2)
#' })
#' combined <- tteenrollment_rbind(trials)
#' combined$s2_ipw()
#' }
#'
#' @family tte_methods
#' @export
tteenrollment_rbind <- function(trials) {
  if (!is.list(trials) || length(trials) == 0) {
    stop("trials must be a non-empty list")
  }

  for (i in seq_along(trials)) {
    if (!inherits(trials[[i]], "TTEEnrollment")) {
      stop("All elements must be TTEEnrollment objects")
    }
  }

  data_level <- trials[[1]]$data_level
  for (i in seq_along(trials)[-1]) {
    if (trials[[i]]$data_level != data_level) {
      stop(
        "All trials must have the same data_level.\n",
        "First trial: '",
        data_level,
        "', trial ",
        i,
        ": '",
        trials[[i]]$data_level,
        "'"
      )
    }
  }

  design <- trials[[1]]$design

  combined_data <- data.table::rbindlist(
    lapply(trials, function(t) t$data),
    use.names = TRUE,
    fill = TRUE
  )

  steps <- trials[[1]]$steps_completed
  for (t in trials[-1]) {
    steps <- intersect(steps, t$steps_completed)
  }

  weight_cols <- unique(unlist(lapply(trials, function(t) t$weight_cols)))

  # Preserve the estimand tag (set by s4_prepare_for_analysis). Without this,
  # a combined ITT object would lose its tag and the irr() guard would wrongly
  # block its valid IPW-only weight. Combining different estimands is an error;
  # NULL (unprepared, the usual pre-s4 rbind case) is fine.
  estimands <- unique(Filter(
    Negate(is.null),
    lapply(trials, function(t) t$estimand)
  ))
  if (length(estimands) > 1L) {
    stop(
      "Cannot rbind TTEEnrollment objects with different estimands: ",
      paste(unlist(estimands), collapse = ", ")
    )
  }

  result <- TTEEnrollment$new(
    data = combined_data,
    design = design,
    data_level = data_level,
    steps_completed = steps,
    weight_cols = weight_cols
  )
  if (length(estimands) == 1L) {
    result$estimand <- estimands[[1]]
  }
  result
}


#' Combine and format multiple rates outputs into a publication-ready table
#'
#' @param results Named list of per-ETT result lists.
#' @param slot Character scalar: name of the slot with `$rates()` output.
#' @param descriptions Optional named character vector mapping ett_id to descriptions.
#'
#' @return A data.table in wide format.
#'
#' @family tte_methods
#' @export
tteenrollment_rates_combine <- function(results, slot, descriptions = NULL) {
  ett_id <- arm <- events_weighted <- py_weighted <- rate_per_100000py <- description <- NULL
  rates_list <- lapply(results, `[[`, slot)

  first_non_null <- Find(Negate(is.null), rates_list)
  treatment_col <- attr(first_non_null, "treatment_var")
  if (is.null(treatment_col)) {
    stop(
      "results$*$",
      slot,
      " must be $rates() outputs (missing 'treatment_var' attribute)"
    )
  }

  dt <- rbindlist(rates_list, idcol = "ett_id")
  dt[, arm := fifelse(get(treatment_col), "Intervention", "Comparator")]
  dt[, (treatment_col) := NULL]

  dt[, `:=`(
    events_weighted = format(round(events_weighted, 1), nsmall = 1),
    py_weighted = format(round(py_weighted, 0), big.mark = ","),
    rate_per_100000py = format(round(rate_per_100000py, 1), nsmall = 1)
  )]

  if (!is.null(descriptions)) {
    dt[, description := descriptions[ett_id]]
    cast_formula <- stats::as.formula("ett_id + description ~ arm")
  } else {
    cast_formula <- stats::as.formula("ett_id ~ arm")
  }

  dcast(
    dt,
    cast_formula,
    value.var = c("events_weighted", "py_weighted", "rate_per_100000py")
  )
}


#' Combine and format multiple irr outputs into a publication-ready table
#'
#' @param results Named list of per-ETT result lists.
#' @param slot Character scalar: name of the slot with `$irr()` output.
#' @param descriptions Optional named character vector mapping ett_id to descriptions.
#'
#' @return A data.table with formatted IRR estimates.
#'
#' @family tte_methods
#' @export
tteenrollment_irr_combine <- function(results, slot, descriptions = NULL) {
  ett_id <- warn <- IRR <- IRR_lower <- IRR_upper <- IRR_pvalue <- description <- . <- NULL
  irr_list <- lapply(results, `[[`, slot)
  dt <- rbindlist(irr_list, idcol = "ett_id")

  warn_ids <- dt[warn == TRUE, ett_id]
  if (length(warn_ids) > 0L) {
    message("Convergence warnings in: ", paste(warn_ids, collapse = ", "))
  }

  result <- dt[, .(
    ett_id,
    IRR = format(round(IRR, 2), nsmall = 2),
    `95% CI` = paste0(
      format(round(IRR_lower, 2), nsmall = 2),
      " to ",
      format(round(IRR_upper, 2), nsmall = 2)
    ),
    `p-value` = format.pval(IRR_pvalue, digits = 3)
  )]

  # Flag convergence warnings
  if (any(dt$warn)) {
    warn_flags <- dt[, fifelse(warn, "*", "")]
    result[, IRR := paste0(IRR, warn_flags)]
  }

  if (!is.null(descriptions)) {
    result[, description := descriptions[ett_id]]
    setcolorder(result, c("ett_id", "description"))
  }

  result
}


#' Combine rates + IRR outputs into a single wide publication-ready table
#'
#' Calls [tteenrollment_rates_combine()] and [tteenrollment_irr_combine()]
#' with shared `descriptions`, then left-joins on `ett_id` so that each row
#' carries per-arm event counts, person-years, rates, and the incidence rate
#' ratio (with 95% CI and p-value) in one place.
#'
#' The returned data.table still uses the generic `_Intervention`/`_Comparator`
#' column suffixes from [tteenrollment_rates_combine()]. The workbook writer
#' in `.write_combined_rates_irr()` applies `.rename_treatment_columns()`
#' afterwards when the featured ETTs share a single enrollment.
#'
#' @param results Named list of per-ETT result lists.
#' @param rates_slot Character scalar, name of the slot with `$rates()` output
#'   (e.g. `"rates_pp_trunc"`).
#' @param irr_slot Character scalar, name of the slot with `$irr()` output
#'   (e.g. `"irr_pp_trunc"`).
#' @param descriptions Optional named character vector mapping `ett_id` to
#'   descriptions.
#'
#' @return A wide `data.table` with one row per ETT.
#'
#' @family tte_methods
#' @export
tteenrollment_combined_combine <- function(
  results,
  rates_slot,
  irr_slot,
  descriptions = NULL
) {
  ett_id <- `95% CI` <- `p-value` <- IRR <- NULL
  rates_dt <- tteenrollment_rates_combine(results, rates_slot, descriptions)
  irr_dt <- tteenrollment_irr_combine(results, irr_slot, descriptions)
  irr_slim <- irr_dt[, .(ett_id, IRR, `95% CI`, `p-value`)]
  merge(rates_dt, irr_slim, by = "ett_id", all.x = TRUE, sort = FALSE)
}


#' Impute missing confounders by sampling from observed values
#'
#' Thin standalone wrapper that delegates to `trial$s1_impute_confounders()`.
#' Exists as a standalone function so it can be used as the default
#' `impute_fn` callback in `$s1_generate_enrollments_and_ipw()`.
#'
#' @param trial A [TTEEnrollment] object.
#' @param confounder_vars Character vector of confounder column names to impute.
#' @param seed Integer seed for reproducibility (default: 4L).
#' @return The modified [TTEEnrollment] object (invisibly).
#' @export
tteenrollment_impute_confounders <- function(
  trial,
  confounder_vars,
  seed = 4L
) {
  trial$s1_impute_confounders(confounder_vars, seed)
  invisible(trial)
}
