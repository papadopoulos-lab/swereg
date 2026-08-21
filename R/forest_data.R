# =============================================================================
# Forest plot: the data the figure draws
# =============================================================================
# These functions assemble one row per emulated trial from the stored results.
# `.build_forest_df()` and `.build_itt_vs_pp_df()` read them through
# `plan$get_estimates()`. The design note sits at the top of `R/forest_plot.R`.
# =============================================================================

#' Read the estimand and the weighting a result slot name stands for.
#'
#' `$s3_analyze()` names its slots `<measure>_<combination>`, and
#' `$get_estimates()` keys the same result on two columns. This translates one
#' into the other, so a caller that still speaks slot names reaches the right
#' accessor rows.
#'
#' @param slot Character(1), a `rates_*`, `irr_*` or `rd_*` slot name.
#' @return A named character(2), `estimand` and `weights`.
#' @noRd
.tte_slot_combo <- function(slot) {
  suffix <- sub("^(rates|irr|rd|rd_curve)_", "", slot)
  switch(
    suffix,
    pp_trunc = c(estimand = "pp", weights = "truncated"),
    pp = c(estimand = "pp", weights = "untruncated"),
    itt = c(estimand = "itt", weights = "untruncated"),
    stop("unknown result slot: ", slot)
  )
}


#' The accessor rows of one estimand and weighting combination.
#'
#' @param plan A TTEPlan.
#' @param slot Character(1), any slot name of the wanted combination.
#' @return A data.table with the `$get_estimates()` columns, one row per
#'   emulated trial that stored something for this combination.
#' @noRd
.tte_estimates_for_slot <- function(plan, slot) {
  estimand <- weights <- NULL # nolint
  combo <- .tte_slot_combo(slot)
  est <- plan$get_estimates()
  est[estimand == combo[["estimand"]] & weights == combo[["weights"]]]
}


#' Build a long-format data.table for the forest plot from the stored results
#' on a TTEPlan.
#'
#' Each row combines the `$rates()` output for both arms (weighted events,
#' person-years, rate per 100,000 PY) with the `$irr()` output (point
#' estimate + interval + p-value). Both come from `$get_estimates()`, which
#' returns one row per emulated trial, estimand and weighting.
#'
#' The rates part is optional. A combination that stored no rates fills the
#' per-arm columns with `NA_real_`.
#'
#' A row is dropped when the plan stores no rate ratio for that combination,
#' and when the stored ratio carries no `IRR_lower` and `IRR_upper` columns.
#' `$get_estimates()` reports both facts as `irr_stored` and
#' `irr_interval_stored`, so this is the rule the raw-slot reader kept. A
#' stored ratio whose VALUES are all `NA` keeps its row, as it always did, and
#' renders `(no estimate)`.
#'
#' `irr_estimable` travels with the ratio. `$s3_analyze()` decides it and
#' stores it, and every formatter below reads that column.
#'
#' `outcome_description` is read from `plan$ett`, which is an INPUT rather than
#' a result. `$get_estimates()` carries no such column.
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
  combo_rates <- .tte_slot_combo(rates_slot)
  combo_irr <- .tte_slot_combo(irr_slot)
  if (!identical(combo_rates, combo_irr)) {
    stop(
      "rates_slot '",
      rates_slot,
      "' and irr_slot '",
      irr_slot,
      "' name different estimand and weighting combinations"
    )
  }
  est <- .tte_estimates_for_slot(plan, irr_slot)
  # The emulated trials that stored something for this combination, in the
  # order the plan stores them. A trial with nothing stored produced no forest
  # row before either, so this is the same set.
  stored_ids <- unique(as.character(est$ett_id))

  if (!is.null(keep_ett_ids)) {
    keep <- intersect(keep_ett_ids, stored_ids)
    # Preserve parallel alignment of group_labels with keep_ett_ids
    if (!is.null(group_labels)) {
      keep_mask <- keep_ett_ids %in% stored_ids
      keep_ett_ids <- keep_ett_ids[keep_mask]
      group_labels <- group_labels[keep_mask]
    }
  } else {
    keep <- stored_ids
  }

  # Build a lookup from ett_id -> group label so we can attach it to each
  # row after the slot-validity filter runs.
  group_lookup <- if (!is.null(keep_ett_ids) && !is.null(group_labels)) {
    setNames(group_labels, keep_ett_ids)
  } else {
    NULL
  }

  rows <- lapply(keep, function(eid) {
    row <- est[ett_id == eid]
    if (nrow(row) == 0L) {
      return(NULL)
    }
    row <- row[1L]
    # No stored ratio, or a stored ratio with no interval columns, no forest
    # row. See the note above the function.
    if (!isTRUE(row$irr_stored) || !isTRUE(row$irr_interval_stored)) {
      return(NULL)
    }

    # `outcome_description` is spec-driven and lives on the ETT grid.
    ett_row <- plan$ett[ett_id == eid][1]
    outcome_description <- if (
      nrow(ett_row) > 0L && "outcome_description" %in% names(plan$ett)
    ) {
      ett_row$outcome_description
    } else {
      NA_character_
    }

    grp <- if (!is.null(group_lookup)) group_lookup[[eid]] else NA_character_
    data.table::data.table(
      ett_id = eid,
      enrollment_id = row$enrollment_id,
      enrollment_name = row$enrollment_name,
      outcome_name = row$outcome_name,
      outcome_description = outcome_description,
      outcome_role = row$outcome_role,
      follow_up = as.integer(row$follow_up),
      intervention_name = row$intervention_name,
      comparator_name = row$comparator_name,
      group_label = as.character(grp),
      events_intervention = row$events_int,
      py_intervention = row$py_int,
      rate_intervention = row$rate_int,
      events_comparator = row$events_cmp,
      py_comparator = row$py_cmp,
      rate_comparator = row$rate_cmp,
      irr = row$irr,
      lo = row$irr_lo,
      hi = row$irr_hi,
      pvalue = row$irr_pvalue,
      irr_estimable = row$irr_estimable
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
#' The row carries the two DECISION columns for the same reason. `nnt` and
#' `nnt_direction` are copied off the curve, never recomputed here. The export
#' path caches this row onto `plan$results_ett`, so the cache holds the decision
#' `.tte_rd_curve()` made, and no reader has to rebuild one.
#'
#' `nnt_lo` and `nnt_hi` are copied for the same reason. `.tte_nntb()` maps the
#' risk-difference interval onto the reciprocal scale, `.tte_rd_curve()` stores
#' the result, and this function carries it onto the row. A reader of
#' `plan$results_ett` then has the interval as data, and `$get_estimates()`
#' returns it.
#'
#' A curve that predates those columns yields `NA` in all four. That is
#' deliberate. A missing decision renders nothing, and this function MUST NOT
#' derive a direction from `rd`, or bounds from `rd_lo` and `rd_hi`, to fill
#' the gap.
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
  # Copied, never derived. A curve written before the decision columns existed
  # gives `NA`, and `NA` renders an empty cell downstream.
  from_curve <- function(nm, empty) {
    if (nm %in% names(curve)) curve[[nm]][i] else empty
  }
  data.table::data.table(
    ett_id = as.character(ett_id),
    band = curve[[time_var]][i],
    rd = as.numeric(curve$rd[i]),
    rd_lo = as.numeric(curve$rd_lo[i]),
    rd_hi = as.numeric(curve$rd_hi[i]),
    nnt = as.numeric(from_curve("nnt", NA_real_)),
    nnt_lo = as.numeric(from_curve("nnt_lo", NA_real_)),
    nnt_hi = as.numeric(from_curve("nnt_hi", NA_real_)),
    nnt_direction = as.character(from_curve("nnt_direction", NA_character_)),
    n_persons_with_event_intervention = as.numeric(
      curve$n_persons_with_event_intervention[i]
    ),
    n_persons_with_event_comparator = as.numeric(
      curve$n_persons_with_event_comparator[i]
    ),
    conf_level = if (is.null(cl)) NA_real_ else as.numeric(cl)
  )
}


#' Columns a `rd_lookup` carries.
#'
#' The figure draws only `rd`, `rd_lo` and `rd_hi`. The two person-count columns
#' stay in the contract all the same. The export path caches these rows onto
#' `plan$results_ett`, and the `PP results` / `ITT results` sheets report the
#' counts from there.
#'
#' `nnt` and `nnt_direction` are the decision columns. `.tte_rd_curve()` decides
#' them and `.forest_rd_row()` copies them onto the row. The cache then holds
#' the decision itself, not the numbers a reader would have to decide from.
#' @noRd
.FOREST_RD_COLS <- c(
  "ett_id",
  "rd",
  "rd_lo",
  "rd_hi",
  "nnt",
  "nnt_direction",
  "n_persons_with_event_intervention",
  "n_persons_with_event_comparator"
)


#' The decision columns of `.FOREST_RD_COLS`, which a legacy lookup may lack.
#'
#' `.forest_rd_map()` does NOT require these two. A `rd_lookup` cached before
#' the decision columns existed carries neither, and three live projects hold
#' exactly such results. Rejecting them would stop an export that worked before.
#'
#' The fallback is to render NOTHING. A row with no stored direction gets an
#' empty number-needed-to-treat cell. `.forest_rd_map()` MUST NOT derive a
#' direction from the sign of `rd` to fill the gap, because that reinstates the
#' defect exactly where nobody looks. Rendering nothing is safe here: the
#' `PP results` and `ITT results` sheets print no benefit-or-harm label today.
#' @noRd
.FOREST_RD_DECISION_COLS <- c("nnt", "nnt_direction")


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
  ett_id <- irr <- lo <- hi <- pvalue <- irr_estimable <- NULL # nolint
  irr_itt <- lo_itt <- hi_itt <- pvalue_itt <- NULL # nolint
  i.irr_itt <- i.lo_itt <- i.hi_itt <- i.pvalue_itt <- NULL # nolint
  i.irr_estimable_itt <- NULL # nolint
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
    c("irr", "lo", "hi", "pvalue", "irr_estimable"),
    c("irr_pp", "lo_pp", "hi_pp", "pvalue_pp", "irr_estimable_pp")
  )
  if (!is.null(itt)) {
    iv <- itt[, .(
      ett_id,
      irr_itt = irr,
      lo_itt = lo,
      hi_itt = hi,
      pvalue_itt = pvalue,
      irr_estimable_itt = irr_estimable
    )]
    base[
      iv,
      on = "ett_id",
      `:=`(
        irr_itt = i.irr_itt,
        lo_itt = i.lo_itt,
        hi_itt = i.hi_itt,
        pvalue_itt = i.pvalue_itt,
        irr_estimable_itt = i.irr_estimable_itt
      )
    ]
  } else {
    base[, `:=`(
      irr_itt = NA_real_,
      lo_itt = NA_real_,
      hi_itt = NA_real_,
      pvalue_itt = NA_real_,
      irr_estimable_itt = NA
    )]
  }
  base
}
