# The cause-specific risk difference and the numbers needed to treat: the
# person-level bootstrap, the curve, and the tables the curve feeds.

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
  return(ord[sequence(len[draw], from = start[draw])])
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
  # A band where the ARM ITSELF holds nobody at risk carries the survival
  # forward. Its column of `den` is zero for every person-trial, so no draw can
  # put a person there: the missing denominator is structural and says nothing
  # about the replicate. A denominator that only THIS replicate emptied stays
  # missing, and the percentile step drops it.
  exhausted <- colSums(mats$den) <= 0
  if (any(exhausted)) {
    surv[, exhausted] <- 1
  }
  # R's own cumprod, one row at a time. It accumulates in long double, so a
  # hand-written column recurrence in double precision would return other bits.
  for (i in seq_len(nrow(surv))) {
    surv[i, ] <- cumprod(surv[i, ])
  }
  return(surv)
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
  return(!is.na(rd_lo) &
    !is.na(rd_hi) &
    ((rd_lo > 0 & rd_hi > 0) | (rd_lo < 0 & rd_hi < 0)))
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

  return(data.table::data.table(nnt = nnt, nnt_direction = nnt_direction))
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
#' The risk set SPANS the band. A person-trial is at risk at band `t` when its
#' row covers `t`. That is `tstart < t <= tstop`, and not only `tstop == t`.
#' The event still lands at the stop of its own row.
#' `.tte_span_risk_sets()` states both rules, and `$survival_curve()` reads
#' them, so the curve in the figure and the point estimate here are the same
#' numbers. The bootstrap reads the same two matrices as the point estimate.
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
#' @param tstart_var Character, the period start column. Where the panel omits
#'   it, `.tte_interval_start()` reads each row as covering the one band that
#'   ends at its own stop.
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
  keep_mult = FALSE,
  tstart_var = "tstart"
) {
  . <- arm <- pt <- band <- num <- den <- first_band <- N <- NULL # nolint
  person <- n_persons <- NULL # nolint

  needed <- c(person_id_var, id_var, treatment_var, time_var, weight_col)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) {
    stop(
      "column(s) not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!"event" %in% names(data)) {
    stop(
      "'event' column not found. Run $s4_prepare_for_analysis() first.",
      call. = FALSE
    )
  }

  w <- data[[weight_col]]
  if (!is.numeric(w) || anyNA(w) || any(!is.finite(w)) || any(w < 0)) {
    stop(
      "weight_col '",
      weight_col,
      "' must be numeric, finite, non-missing and non-negative",
      call. = FALSE
    )
  }
  ev <- data[["event"]]
  if (anyNA(ev) || !all(ev %in% c(0L, 1L))) {
    stop("'event' must be a non-missing 0/1 indicator", call. = FALSE)
  }
  if (
    length(n_boot) != 1L ||
      !is.numeric(n_boot) ||
      is.na(n_boot) ||
      n_boot < 1 ||
      n_boot != as.integer(n_boot)
  ) {
    stop("n_boot must be a positive integer", call. = FALSE)
  }
  n_boot <- as.integer(n_boot)
  if (
    length(conf_level) != 1L ||
      !is.numeric(conf_level) ||
      is.na(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    stop(
      "conf_level must be a single number strictly between 0 and 1",
      call. = FALSE
    )
  }

  tv <- data[[treatment_var]]
  if (anyNA(tv)) {
    stop("treatment_var '", treatment_var, "' must not be missing", call. = FALSE)
  }
  if (!is.logical(tv)) {
    if (!all(tv %in% c(0L, 1L))) {
      stop(
        "risk_difference() requires a logical (or 0/1) '",
        treatment_var,
        "'; got class '",
        class(tv)[1],
        "'",
        call. = FALSE
      )
    }
    tv <- as.logical(tv)
  }
  if (!any(tv) || !any(!tv)) {
    stop("both arms must be present in '", treatment_var, "'", call. = FALSE)
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
      "'",
      call. = FALSE
    )
  }

  band_vals <- sort(unique(data[[time_var]]))
  n_band <- length(band_vals)
  band_code <- match(data[[time_var]], band_vals)
  tstart <- .tte_interval_start(data, tstart_var, time_var, band_vals)
  span <- .tte_span_index(tstart, data[[time_var]], band_vals)

  # Aggregate ONCE. Both sums are additive over persons, so a person-level
  # resample only needs these totals, never the panel rows again.
  #
  # The two sums read different rows, and that difference is the estimand.
  # The numerator holds the events at the stop of their own row. The
  # denominator holds the weight of every row that SPANS the band, which is
  # the risk set `.tte_span_risk_sets()` defines and `$survival_curve()`
  # reports. The point estimate and every replicate read these same two
  # matrices, so the bootstrap cannot resample one definition while the point
  # estimate uses another.
  agg_num <- data.table::data.table(
    arm = tv,
    pt = pt_code,
    band = band_code,
    num = as.numeric(w) * as.numeric(ev)
  )[num != 0, .(num = sum(num)), keyby = .(arm, pt, band)]

  n_span <- pmax(span$hi - span$lo + 1L, 0L)
  spanned <- rep.int(seq_along(n_span), n_span)
  agg_den <- data.table::data.table(
    arm = tv[spanned],
    pt = pt_code[spanned],
    band = sequence(n_span, from = span$lo),
    den = as.numeric(w)[spanned]
  )[, .(den = sum(den)), keyby = .(arm, pt, band)]

  arm_mats <- function(which_arm) {
    mn <- matrix(0, nrow = n_pt, ncol = n_band)
    md <- matrix(0, nrow = n_pt, ncol = n_band)
    sub_n <- agg_num[arm == which_arm]
    sub_d <- agg_den[arm == which_arm]
    mn[cbind(sub_n$pt, sub_n$band)] <- sub_n$num
    md[cbind(sub_d$pt, sub_d$band)] <- sub_d$den
    return(list(num = mn, den = md))
  }
  m_int <- arm_mats(TRUE)
  m_cmp <- arm_mats(FALSE)

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
    return(.rd_surv_batch(mult, mats))
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
    return(cumsum(n))
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
  # because both call `.tte_span_risk_sets()`. Survival is a weighted
  # probability, so no head count can be derived from it. Only the panel holds
  # the identifiers. A woman is at risk at every time her rows span, so a row
  # that opens before the time and closes after it counts her there.
  spans <- .tte_span_risk_sets(
    arm = tv,
    person = person_raw,
    weight = w,
    event = ev,
    tstart = tstart,
    tstop = data[[time_var]],
    times = band_vals
  )
  persons_at_risk <- function(which_arm) {
    return(spans[arm == which_arm]$n_persons_at_risk)
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
  return(out)
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

  return(data.table::data.table(nntb = nntb, nntb_lo = nntb_lo, nntb_hi = nntb_hi))
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
.tte_nntb_cell <- function(
  nntb,
  nntb_lo = NULL,
  nntb_hi = NULL,
  nnt_direction
) {
  if (missing(nnt_direction)) {
    stop(
      "nnt_direction is required: the cell reads the stored decision and ",
      "never re-derives it from the sign of nntb",
      call. = FALSE
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
      "'",
      call. = FALSE
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
  return(out)
}
