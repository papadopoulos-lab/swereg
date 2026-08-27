# =============================================================================
# Result accessors: the one route from a TTEPlan to its stored results
# =============================================================================
#
# `$s3_analyze()` writes `plan$results_ett` and `plan$results_enrollment`.
# `$s1_generate_enrollments_and_ipw()` writes `plan$enrollment_counts`. Those
# three lists are nested, and every slot in them has a different shape. A
# reader had to know the slot name, the estimand the slot stands for, and the
# column names inside it.
#
# The six accessors below return that same content as six flat tables. Each
# takes no argument and returns EVERYTHING it can find. The caller filters.
#
# THE RULE THESE ACCESSORS KEEP
#
# An accessor MUST NOT compute a result. It MUST NOT apply a rule or a
# threshold. It MUST NOT read an analysis panel. It MUST NOT create a row that
# no slot backs, and it MUST NOT fill a gap from a neighbouring slot.
#
# A missing slot yields ABSENT ROWS. The reason is staleness. A plan saved
# before a stage ran holds fewer slots than a plan saved after it. An accessor
# that recovered the gap would report a complete table over an incomplete plan.
# The reader would then have no way to see the difference.
#
# Three decisions used to live here and now live elsewhere:
#
#   - `irr_estimable`. `$s3_analyze()` calls `.tte_irr_estimable()` and stores
#     the answer beside the ratio, as it stores `nnt_direction` beside the risk
#     difference. A result stored before that column existed gives NA.
#   - the participant flow. `.build_cohort_flow()` creates the comparator-draw
#     row and the analysis row and derives the change columns. It is a renderer, that
#     work is a renderer's, and no accessor calls it.
#   - the legacy attrition fallback. `.attrition_overall()` sums the per-trial
#     attrition rows when a criterion has no global row, and its own
#     documentation calls that number inflated. That sum is a renderer's
#     decision and it stays one. `$get_attrition()` RETURNS the per-trial rows
#     so the renderer can make it, and no accessor calls
#     `.attrition_overall()`.
#
# Returning a stored row is not summing it. The accessors do three things:
#
#   - they melt a wide curve into one row per arm;
#   - they select the stored rows and rename their columns;
#   - they join the descriptive labels from `plan$ett` and `plan$spec`.
#
# Every number they return was stored by a producer.
# =============================================================================


#' The three estimand and weighting combinations `$s3_analyze()` stores.
#'
#' `estimand` and `weights` are two axes, not one. `"pp"` and `"itt"` name the
#' estimand. `"truncated"` and `"untruncated"` name the weighting choice inside
#' per-protocol. Only these three combinations occur.
#'
#' `rd` is `NA` for per-protocol on the untruncated weight. s3 computes the
#' absolute scale on the truncated per-protocol weight and on the baseline
#' inverse-probability weight only.
#' @noRd
.ACC_ESTIMATE_SLOTS <- list(
  list(
    estimand = "pp",
    weights = "truncated",
    rates = "rates_pp_trunc",
    irr = "irr_pp_trunc",
    rd = "rd_pp_trunc"
  ),
  list(
    estimand = "pp",
    weights = "untruncated",
    rates = "rates_pp",
    irr = "irr_pp",
    rd = NA_character_
  ),
  list(
    estimand = "itt",
    weights = "untruncated",
    rates = "rates_itt",
    irr = "irr_itt",
    rd = "rd_itt"
  )
)


#' The five Table 1 panels `$s3_analyze()` stores, on three axes.
#'
#' `imputation` says which dataset the panel was built on. `weighting` says
#' which weight column it used. `variant` separates the headline panel from the
#' supplementary panels: the main panel drops the missing rows and takes its
#' percentages over the non-missing denominator.
#'
#' `table1_raw` reads a separate pre-imputation file. That file is optional, so
#' the slot is absent whenever the file is absent. `$get_baselines()` then
#' returns no `"raw"` rows at all.
#' @noRd
.ACC_TABLE1_SLOTS <- list(
  list(
    slot = "table1_raw",
    imputation = "raw",
    weighting = "none",
    variant = "supplementary"
  ),
  list(
    slot = "table1_unweighted",
    imputation = "imputed",
    weighting = "none",
    variant = "supplementary"
  ),
  list(
    slot = "table1_ipw",
    imputation = "imputed",
    weighting = "ipw",
    variant = "supplementary"
  ),
  list(
    slot = "table1_ipw_trunc",
    imputation = "imputed",
    weighting = "ipw_trunc",
    variant = "supplementary"
  ),
  list(
    slot = "table1_ipw_trunc_main",
    imputation = "imputed",
    weighting = "ipw_trunc",
    variant = "main"
  )
)


#' The two risk-difference curve slots, and the combination each stands for.
#' @noRd
.ACC_CURVE_SLOTS <- list(
  list(slot = "rd_curve_pp_trunc", estimand = "pp", weights = "truncated"),
  list(slot = "rd_curve_itt", estimand = "itt", weights = "untruncated")
)


#' Column names and types of each accessor's return value.
#'
#' The schema is DATA, so a test can name every column and an empty result can
#' carry the same columns as a full one. An accessor that found nothing returns
#' a table with no rows and every column, never `NULL`.
#' @noRd
.ACC_SCHEMA <- list(
  estimates = c(
    ett_id = "character",
    enrollment_id = "character",
    enrollment_name = "character",
    outcome_var = "character",
    outcome_name = "character",
    outcome_role = "character",
    follow_up = "numeric",
    age_group = "character",
    intervention_name = "character",
    comparator_name = "character",
    estimand = "character",
    weights = "character",
    n_events = "numeric",
    rates_stored = "logical",
    events_int = "numeric",
    py_int = "numeric",
    rate_int = "numeric",
    events_cmp = "numeric",
    py_cmp = "numeric",
    rate_cmp = "numeric",
    persons_event_int = "numeric",
    persons_event_cmp = "numeric",
    irr = "numeric",
    irr_lo = "numeric",
    irr_hi = "numeric",
    irr_pvalue = "numeric",
    irr_estimable = "logical",
    irr_stored = "logical",
    irr_interval_stored = "logical",
    rd_stored = "logical",
    rd = "numeric",
    rd_lo = "numeric",
    rd_hi = "numeric",
    interval_status = "character",
    nnt = "numeric",
    nnt_lo = "numeric",
    nnt_hi = "numeric",
    nnt_direction = "character",
    n_boot = "numeric",
    seed = "numeric",
    conf_level = "numeric"
  ),
  curves = c(
    ett_id = "character",
    estimand = "character",
    weights = "character",
    arm = "character",
    band = "numeric",
    surv = "numeric",
    n_persons_at_risk = "numeric"
  ),
  baselines = c(
    enrollment_id = "character",
    imputation = "character",
    weighting = "character",
    variant = "character",
    variable = "character",
    level = "character",
    overall = "character",
    comparator = "character",
    intervention = "character",
    comparator_label = "character",
    intervention_label = "character",
    smd_stored = "logical",
    smd_numeric = "numeric",
    n_baseline = "numeric",
    n_baseline_intervention = "numeric",
    n_baseline_comparator = "numeric"
  ),
  attrition = c(
    enrollment_id = "character",
    trial_id = "integer",
    step_order = "integer",
    step_name = "character",
    n_persons = "numeric",
    n_person_trials = "numeric",
    n_arm_intervention = "numeric",
    n_arm_comparator = "numeric"
  ),
  matching = c(
    enrollment_id = "character",
    trial_id = "integer",
    n_intervention_total = "numeric",
    n_comparator_total = "numeric",
    n_intervention_enrolled = "numeric",
    n_comparator_enrolled = "numeric"
  ),
  subgroups = c(
    ett_id = "character",
    estimand = "character",
    weights = "character",
    subgroup_var = "character",
    subgroup_level = "character",
    strata_stored = "logical",
    irr = "numeric",
    irr_lo = "numeric",
    irr_hi = "numeric",
    irr_pvalue = "numeric",
    em_pvalue = "numeric",
    ratio_of_irrs = "numeric",
    ratio_lo = "numeric",
    ratio_hi = "numeric"
  )
)


#' An empty data.table with one accessor's schema.
#' @param schema One element of `.ACC_SCHEMA`.
#' @return A data.table with no rows and every schema column.
#' @noRd
.acc_empty <- function(schema) {
  proto <- list(
    character = character(0),
    numeric = numeric(0),
    integer = integer(0),
    logical = logical(0)
  )
  return(data.table::as.data.table(lapply(schema, function(ty) proto[[ty]])))
}


#' One row carrying EVERY schema column, with the named values filled in.
#'
#' A fallback row reports a stored fact that no slot of the usual grain backs.
#' An enrollment that stored a baseline size and no panel is one. Such a row
#' MUST still carry every schema column. It is the only row of its table when
#' nothing else was stored, and `.acc_bind()` would then have nothing to
#' order.
#'
#' @param schema One element of `.ACC_SCHEMA`.
#' @param values A named list of the columns this row can fill.
#' @return A one-row data.table with every schema column.
#' @noRd
.acc_row <- function(schema, values) {
  na_of <- list(
    character = NA_character_,
    numeric = NA_real_,
    integer = NA_integer_,
    logical = NA
  )
  row <- lapply(schema, function(ty) na_of[[ty]])
  for (nm in names(values)) {
    row[[nm]] <- values[[nm]]
  }
  return(data.table::as.data.table(row))
}


#' Stack the per-key rows an accessor built, and put the schema columns in
#' schema order.
#'
#' @param rows A list of data.tables, `NULL` entries allowed.
#' @param schema One element of `.ACC_SCHEMA`.
#' @return A data.table carrying exactly the schema columns.
#' @noRd
.acc_bind <- function(rows, schema) {
  rows <- Filter(function(x) !is.null(x) && nrow(x) > 0L, rows)
  if (length(rows) == 0L) {
    return(.acc_empty(schema))
  }
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  data.table::setcolorder(out, names(schema))
  return(out[])
}


#' Read one stored result slot, and report a failed one as absent.
#'
#' A worker that failed stores a skip envelope, `list(skipped = TRUE, reason =
#' ...)`, in place of the result. The envelope is a record of the failure, not
#' a result, so an accessor treats it as absent.
#'
#' @param res One element of `plan$results_ett` or `plan$results_enrollment`.
#' @param slot Character(1), the slot name. `NA` reads nothing.
#' @return The stored value, or `NULL`.
#' @noRd
.acc_slot <- function(res, slot) {
  if (is.null(res) || length(slot) != 1L || is.na(slot)) {
    return(NULL)
  }
  value <- res[[slot]]
  if (is.null(value) || isTRUE(value$skipped)) {
    return(NULL)
  }
  return(value)
}


#' First element of a stored field, as one number.
#' @param x A list or data.table, or `NULL`.
#' @param nm Character(1), the field name.
#' @return Numeric(1). `NA_real_` when the field is absent or empty.
#' @noRd
.acc_num1 <- function(x, nm) {
  if (is.null(x) || !nm %in% names(x)) {
    return(NA_real_)
  }
  v <- suppressWarnings(as.numeric(x[[nm]]))
  if (length(v) == 0L) return(NA_real_) else return(v[1L])
}


#' First element of a stored field, as one string.
#' @param x A list or data.table, or `NULL`.
#' @param nm Character(1), the field name.
#' @return Character(1). `NA_character_` when the field is absent or empty.
#' @noRd
.acc_chr1 <- function(x, nm) {
  if (is.null(x) || !nm %in% names(x)) {
    return(NA_character_)
  }
  v <- as.character(x[[nm]])
  if (length(v) == 0L) return(NA_character_) else return(v[1L])
}


#' Split one stored rates table into the six per-arm numbers.
#'
#' The arm column is named by the design, so the table carries its name in the
#' `treatment_var` attribute. The intervention arm is `TRUE` and the comparator
#' arm is `FALSE`, which is the convention `$rates()` writes.
#'
#' `stored` reports whether the stored table passed every SHAPE check. It is a
#' data.table. It carries the three measurement columns. It carries its
#' `treatment_var` attribute as a column. It holds exactly one row per arm.
#'
#' A table that passes all four still reports `NA` numbers when the producer
#' stored `NA` numbers. Those two states are different facts.
#'
#' @param rv One stored rates table, or `NULL`.
#' @return A named list of six numbers and one logical. Each number is
#'   `NA_real_` when unavailable.
#' @noRd
.acc_rates_arms <- function(rv) {
  out <- list(
    stored = FALSE,
    events_int = NA_real_,
    py_int = NA_real_,
    rate_int = NA_real_,
    events_cmp = NA_real_,
    py_cmp = NA_real_,
    rate_cmp = NA_real_
  )
  if (is.null(rv) || !data.table::is.data.table(rv)) {
    return(out)
  }
  need <- c("events_weighted", "py_weighted", "rate_per_100000py")
  if (!all(need %in% names(rv))) {
    return(out)
  }
  treatment_var <- attr(rv, "treatment_var", exact = TRUE)
  if (is.null(treatment_var) || !treatment_var %in% names(rv)) {
    return(out)
  }
  arm <- rv[[treatment_var]]
  int <- rv[which(arm %in% TRUE)]
  cmp <- rv[which(arm %in% FALSE)]
  out$stored <- nrow(int) == 1L && nrow(cmp) == 1L
  if (nrow(int) == 1L) {
    out$events_int <- .acc_num1(int, "events_weighted")
    out$py_int <- .acc_num1(int, "py_weighted")
    out$rate_int <- .acc_num1(int, "rate_per_100000py")
  }
  if (nrow(cmp) == 1L) {
    out$events_cmp <- .acc_num1(cmp, "events_weighted")
    out$py_cmp <- .acc_num1(cmp, "py_weighted")
    out$rate_cmp <- .acc_num1(cmp, "rate_per_100000py")
  }
  return(out)
}


#' First element of a stored field, as one logical.
#'
#' Used for `irr_estimable`, which `$s3_analyze()` decides and stores. The
#' accessor READS it. A result stored before that column existed gives `NA`,
#' and the accessor MUST NOT apply the rule itself to fill the gap.
#'
#' @param x A list or data.table, or `NULL`.
#' @param nm Character(1), the field name.
#' @return Logical(1). `NA` when the field is absent or empty.
#' @noRd
.acc_lgl1 <- function(x, nm) {
  if (is.null(x) || !nm %in% names(x)) {
    return(NA)
  }
  v <- as.logical(x[[nm]])
  if (length(v) == 0L) return(NA) else return(v[1L])
}


#' The descriptive labels one ETT carries, read from `plan$ett` and
#' `plan$spec`.
#'
#' Both are INPUTS, not results. They are already public and every consumer
#' reads them the same way.
#'
#' The arm names fall back to `"Intervention"` and `"Comparator"` when the
#' specification names no arms. That is the fallback `.write_results_single()`
#' already uses, so a consumer that moves onto this accessor keeps its labels.
#'
#' @param plan A TTEPlan.
#' @param ett_id Character(1).
#' @return A named list of the identifier and label fields.
#' @noRd
.acc_ett_labels <- function(plan, ett_id) {
  out <- list(
    enrollment_id = NA_character_,
    enrollment_name = NA_character_,
    outcome_var = NA_character_,
    outcome_name = NA_character_,
    outcome_role = NA_character_,
    follow_up = NA_real_,
    age_group = NA_character_,
    intervention_name = "Intervention",
    comparator_name = "Comparator"
  )
  ett <- plan$ett
  if (!is.null(ett) && nrow(ett) > 0L && "ett_id" %in% names(ett)) {
    i <- match(ett_id, as.character(ett$ett_id))
    if (!is.na(i)) {
      row <- ett[i]
      out$enrollment_id <- .acc_chr1(row, "enrollment_id")
      out$outcome_var <- .acc_chr1(row, "outcome_var")
      out$outcome_name <- .acc_chr1(row, "outcome_name")
      out$outcome_role <- .acc_chr1(row, "outcome_role")
      out$follow_up <- .acc_num1(row, "follow_up")
      out$age_group <- .acc_chr1(row, "age_group")
    }
  }
  if (is.na(out$enrollment_id)) {
    res <- plan$results_ett[[ett_id]]
    out$enrollment_id <- .acc_chr1(res, "enrollment_id")
  }
  if (!is.na(out$enrollment_id)) {
    out$enrollment_name <- .enrollment_label(plan, out$enrollment_id)
    arms <- .lookup_arm_labels(plan$spec, out$enrollment_id)
    if (!is.null(arms)) {
      out$intervention_name <- as.character(arms[["intervention"]])
      out$comparator_name <- as.character(arms[["comparator"]])
    }
  }
  return(out)
}


#' Build the estimate rows of one ETT, one per combination that has a result.
#'
#' A combination gets a row when the plan holds at least one of its three slots.
#' A combination the plan holds nothing for gets NO row. So a complete grid
#' gives three rows per ETT, and that is an upper bound rather than a promise.
#'
#' `irr_stored` and `irr_interval_stored` report the stored SHAPE of the rate
#' ratio, and they are facts rather than derivations. Every numeric column
#' reports an absent slot and a stored `NA` the same way, as `NA`, and two
#' consumers must separate the two. `.sensitivity_row_measurements()` renders
#' the arm counts of a combination whose ratio failed, and it renders nothing
#' for a combination that has no ratio slot. `.build_forest_df()` drops a row
#' whose stored ratio carries no `IRR_lower` and `IRR_upper` columns.
#'
#' A trial that stored a result and NO estimate slot gets ONE row, with
#' `estimand` and `weights` both `NA`. `n_events` rides on that row, and the
#' `ETTs` sheet reports it. The row reports a stored result, so no consumer
#' that filters on an estimand ever sees it: `NA == "pp"` is `NA`, which
#' selects nothing.
#' @noRd
.acc_estimate_rows <- function(plan, ett_id) {
  res <- plan$results_ett[[ett_id]]
  labels <- .acc_ett_labels(plan, ett_id)
  n_events <- .acc_num1(res$summary, "n_events")

  rows <- lapply(.ACC_ESTIMATE_SLOTS, function(combo) {
    rates_value <- .acc_slot(res, combo$rates)
    irr_value <- .acc_slot(res, combo$irr)
    rd_row <- .acc_slot(res, combo$rd)
    # No slot, no row. A row of identifiers with nothing measured under them
    # reports a combination nobody computed, and the reader cannot tell it from
    # one that failed.
    if (is.null(rates_value) && is.null(irr_value) && is.null(rd_row)) {
      return(NULL)
    }
    rates <- .acc_rates_arms(rates_value)
    return(data.table::data.table(
      ett_id = as.character(ett_id),
      enrollment_id = labels$enrollment_id,
      enrollment_name = labels$enrollment_name,
      outcome_var = labels$outcome_var,
      outcome_name = labels$outcome_name,
      outcome_role = labels$outcome_role,
      follow_up = labels$follow_up,
      age_group = labels$age_group,
      intervention_name = labels$intervention_name,
      comparator_name = labels$comparator_name,
      estimand = combo$estimand,
      weights = combo$weights,
      n_events = n_events,
      rates_stored = rates$stored,
      events_int = rates$events_int,
      py_int = rates$py_int,
      rate_int = rates$rate_int,
      events_cmp = rates$events_cmp,
      py_cmp = rates$py_cmp,
      rate_cmp = rates$rate_cmp,
      persons_event_int = .acc_num1(
        rd_row,
        "n_persons_with_event_intervention"
      ),
      persons_event_cmp = .acc_num1(rd_row, "n_persons_with_event_comparator"),
      irr = .acc_num1(irr_value, "IRR"),
      irr_lo = .acc_num1(irr_value, "IRR_lower"),
      irr_hi = .acc_num1(irr_value, "IRR_upper"),
      irr_pvalue = .acc_num1(irr_value, "IRR_pvalue"),
      irr_estimable = .acc_lgl1(irr_value, "irr_estimable"),
      irr_stored = !is.null(irr_value),
      irr_interval_stored = !is.null(irr_value) &&
        all(c("IRR_lower", "IRR_upper") %in% names(irr_value)),
      rd_stored = data.table::is.data.table(rd_row) && nrow(rd_row) > 0L,
      rd = .acc_num1(rd_row, "rd"),
      rd_lo = .acc_num1(rd_row, "rd_lo"),
      rd_hi = .acc_num1(rd_row, "rd_hi"),
      interval_status = .acc_chr1(rd_row, "interval_status"),
      nnt = .acc_num1(rd_row, "nnt"),
      nnt_lo = .acc_num1(rd_row, "nnt_lo"),
      nnt_hi = .acc_num1(rd_row, "nnt_hi"),
      nnt_direction = .acc_chr1(rd_row, "nnt_direction"),
      n_boot = .acc_num1(rd_row, "n_boot"),
      seed = .acc_num1(rd_row, "seed"),
      conf_level = .acc_num1(rd_row, "conf_level")
    ))
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    # The trial stored a result and no estimate slot. The summary is a stored
    # slot, so the trial gets one row and `n_events` reaches a reader.
    if (is.null(res) || length(res) == 0L) {
      return(NULL)
    }
    return(.acc_row(.ACC_SCHEMA$estimates, list(
      ett_id = as.character(ett_id),
      enrollment_id = labels$enrollment_id,
      enrollment_name = labels$enrollment_name,
      outcome_var = labels$outcome_var,
      outcome_name = labels$outcome_name,
      outcome_role = labels$outcome_role,
      follow_up = labels$follow_up,
      age_group = labels$age_group,
      intervention_name = labels$intervention_name,
      comparator_name = labels$comparator_name,
      n_events = n_events,
      rates_stored = FALSE,
      irr_stored = FALSE,
      irr_interval_stored = FALSE,
      rd_stored = FALSE
    )))
  }
  return(data.table::rbindlist(rows, use.names = TRUE))
}


#' Every stored effect estimate, one row per ETT, estimand and weighting.
#' @param plan A TTEPlan.
#' @return A data.table with the `estimates` schema.
#' @noRd
.acc_estimates <- function(plan) {
  ett_ids <- names(plan$results_ett)
  if (is.null(ett_ids) || length(ett_ids) == 0L) {
    return(.acc_empty(.ACC_SCHEMA$estimates))
  }
  rows <- lapply(ett_ids, function(id) .acc_estimate_rows(plan, id))
  return(.acc_bind(rows, .ACC_SCHEMA$estimates))
}


#' Column names the stored risk-difference curve carries beside its band.
#'
#' `$get_curves()` finds the band column by elimination, because the design
#' names it. It is `tstop` in every current study.
#' @noRd
.ACC_CURVE_KNOWN_COLS <- c(
  "surv_comparator",
  "surv_intervention",
  "surv",
  "rd",
  "rd_lo",
  "rd_hi",
  "interval_status",
  "nnt",
  "nnt_lo",
  "nnt_hi",
  "nnt_direction",
  "n_persons_with_event_comparator",
  "n_persons_with_event_intervention",
  "n_persons_at_risk_comparator",
  "n_persons_at_risk_intervention",
  "events",
  "hazard",
  "at_risk",
  "n_persons_at_risk",
  "group"
)


#' Melt one stored curve into one row per arm per band.
#' @noRd
.acc_curve_rows <- function(ett_id, combo, curve) {
  if (!data.table::is.data.table(curve) || nrow(curve) == 0L) {
    return(NULL)
  }
  band_col <- setdiff(names(curve), .ACC_CURVE_KNOWN_COLS)
  if (length(band_col) == 0L) {
    return(NULL)
  }
  band_col <- if ("tstop" %in% band_col) "tstop" else band_col[1L]
  arms <- list(
    list(
      arm = "comparator",
      surv = "surv_comparator",
      at_risk = "n_persons_at_risk_comparator"
    ),
    list(
      arm = "intervention",
      surv = "surv_intervention",
      at_risk = "n_persons_at_risk_intervention"
    )
  )
  # `n_persons_at_risk` is the head count a numbers-at-risk row reports.
  # `$s3_analyze()` stores it per arm per band and this method melts it. It is
  # READ and never derived: survival is a weighted probability, and no head
  # count follows from one.
  #
  # A curve stored before the producer carried those columns gives `NA`. Every
  # other accessor gives that same answer for a slot that predates a column. A
  # consumer that draws a risk table MUST check the column and refuse to draw.
  # A row of missing values is worse than no risk table.
  rows <- lapply(arms, function(a) {
    if (!a$surv %in% names(curve)) {
      return(NULL)
    }
    at_risk <- if (a$at_risk %in% names(curve)) {
      as.numeric(curve[[a$at_risk]])
    } else {
      NA_real_
    }
    return(data.table::data.table(
      ett_id = as.character(ett_id),
      estimand = combo$estimand,
      weights = combo$weights,
      arm = a$arm,
      band = as.numeric(curve[[band_col]]),
      surv = as.numeric(curve[[a$surv]]),
      n_persons_at_risk = at_risk
    ))
  })
  return(data.table::rbindlist(Filter(Negate(is.null), rows), use.names = TRUE))
}


#' Every stored survival curve, one row per ETT, estimand, weighting, arm and
#' band.
#' @param plan A TTEPlan.
#' @return A data.table with the `curves` schema.
#' @noRd
.acc_curves <- function(plan) {
  ett_ids <- names(plan$results_ett)
  if (is.null(ett_ids) || length(ett_ids) == 0L) {
    return(.acc_empty(.ACC_SCHEMA$curves))
  }
  rows <- list()
  for (id in ett_ids) {
    res <- plan$results_ett[[id]]
    for (combo in .ACC_CURVE_SLOTS) {
      rows[[length(rows) + 1L]] <- .acc_curve_rows(
        id,
        combo,
        .acc_slot(res, combo$slot)
      )
    }
  }
  return(.acc_bind(rows, .ACC_SCHEMA$curves))
}


#' Carry the last non-empty value of a layout column down its block.
#'
#' @param x A character vector.
#' @return A character vector of the same length. An empty or missing element
#'   takes the last non-empty value above it.
#' @noRd
.acc_carry_down <- function(x) {
  x <- as.character(x)
  x[!nzchar(trimws(x))] <- NA_character_
  seen <- which(!is.na(x))
  if (length(seen) == 0L) {
    return(x)
  }
  idx <- cumsum(!is.na(x))
  out <- rep(NA_character_, length(x))
  out[idx > 0L] <- x[seen][idx[idx > 0L]]
  return(out)
}


#' Melt one stored Table 1 panel into the baseline schema.
#'
#' The panel names its arm columns after the arms, so the two column names
#' differ between studies. Position identifies them: column 3 is the whole
#' cohort, column 4 is the comparator, and column 5 is the intervention. That
#' is the order `.swereg_table1()` writes.
#'
#' The two arm NAMES are returned as `comparator_label` and
#' `intervention_label`. They are a stored fact. `$s3_analyze()` built the
#' panel with the arm labels the specification held at that time. A renderer
#' that re-read the specification would head the columns with today's labels
#' over yesterday's numbers.
#'
#' `.swereg_table1()` leaves the variable cell EMPTY on a continuation row. The
#' layout prints the variable name once, then indents its levels under it. That
#' blank is a rendering convention, so this function carries the name down each
#' block and `variable` becomes a real key. A renderer that wants the indent
#' MUST blank the repeat itself.
#' @noRd
.acc_baseline_rows <- function(eid, combo, panel, counts) {
  if (!data.table::is.data.table(panel) || nrow(panel) == 0L) {
    return(NULL)
  }
  if (ncol(panel) < 5L) {
    return(NULL)
  }
  smd <- if ("smd_numeric" %in% names(panel)) {
    as.numeric(panel[["smd_numeric"]])
  } else {
    NA_real_
  }
  return(data.table::data.table(
    enrollment_id = as.character(eid),
    imputation = combo$imputation,
    weighting = combo$weighting,
    variant = combo$variant,
    variable = .acc_carry_down(panel[[1L]]),
    level = as.character(panel[[2L]]),
    overall = as.character(panel[[3L]]),
    comparator = as.character(panel[[4L]]),
    intervention = as.character(panel[[5L]]),
    comparator_label = names(panel)[4L],
    intervention_label = names(panel)[5L],
    smd_stored = "smd_numeric" %in% names(panel),
    smd_numeric = smd,
    n_baseline = counts$n_baseline,
    n_baseline_intervention = counts$n_baseline_intervention,
    n_baseline_comparator = counts$n_baseline_comparator
  ))
}


#' Every stored baseline panel, one row per enrollment, panel and table row.
#'
#' The three enrollment COUNTS ride on every row of that enrollment's panels.
#' An enrollment that stored counts and NO panel gets ONE row instead, with
#' every panel column `NA`. The counts are stored, so they reach a reader.
#'
#' That state is supported rather than degenerate. `.baseline_panel_is_stale()`
#' calls a result with no panel CURRENT, so `$export_tables()` never refreshes
#' it and the `Enrollments` sheet still reports its baseline size.
#'
#' No consumer that asks for a panel sees the counts-only row. Each one filters
#' on `imputation`, `weighting` and `variant`, and `NA == "imputed"` is `NA`,
#' which selects nothing.
#'
#' @param plan A TTEPlan.
#' @return A data.table with the `baselines` schema.
#' @noRd
.acc_baselines <- function(plan) {
  eids <- names(plan$results_enrollment)
  if (is.null(eids) || length(eids) == 0L) {
    return(.acc_empty(.ACC_SCHEMA$baselines))
  }
  rows <- list()
  for (eid in eids) {
    res <- plan$results_enrollment[[eid]]
    counts <- list(
      n_baseline = .acc_num1(res, "n_baseline"),
      n_baseline_intervention = .acc_num1(res, "n_baseline_intervention"),
      n_baseline_comparator = .acc_num1(res, "n_baseline_comparator")
    )
    panel_rows <- lapply(.ACC_TABLE1_SLOTS, function(combo) {
      return(.acc_baseline_rows(eid, combo, .acc_slot(res, combo$slot), counts))
    })
    panel_rows <- Filter(Negate(is.null), panel_rows)
    if (length(panel_rows) > 0L) {
      rows <- c(rows, panel_rows)
      next
    }
    if (all(vapply(counts, is.na, logical(1)))) {
      next
    }
    rows[[length(rows) + 1L]] <- .acc_row(.ACC_SCHEMA$baselines, list(
      enrollment_id = as.character(eid),
      n_baseline = counts$n_baseline,
      n_baseline_intervention = counts$n_baseline_intervention,
      n_baseline_comparator = counts$n_baseline_comparator,
      smd_stored = FALSE
    ))
  }
  return(.acc_bind(rows, .ACC_SCHEMA$baselines))
}


#' Columns the stored attrition table must carry.
#' @noRd
.ACC_ATTRITION_COLS <- c(
  "trial_id",
  "criterion",
  "n_persons",
  "n_person_trials",
  "n_intervention",
  "n_comparator"
)


#' The stored eligibility cascade, one row per enrollment and stored row.
#'
#' `$s1_generate_enrollments_and_ipw()` stores
#' `plan$enrollment_counts[[eid]]$attrition` at one row per trial and criterion,
#' plus ONE GLOBAL ROW per criterion. The global row carries `trial_id = NA` and
#' the true overall `uniqueN(persons)`.
#'
#' This accessor returns EVERY STORED ROW, in stored order. `trial_id` is `NA`
#' on a global row and the trial index on a per-trial row, so the caller filters
#' on that column.
#'
#' It returns the stored rows and nothing else. It does not sum the per-trial
#' rows. It does not create a global row for a criterion that has none. It does
#' not fill a value down. A criterion with per-trial rows and no global row
#' therefore yields per-trial rows and no global row.
#'
#' The reason for the whole table, rather than the global rows alone, is
#' `.attrition_overall()`. That renderer sums the per-trial rows when a
#' criterion has no global row, and it needs the rows to do it. Summing is a
#' renderer's decision because the sum counts a person once per sequential trial
#' she enters, and its own documentation calls the number inflated. For a
#' CONSORT diagram an inflated number beats no number. This accessor makes no
#' such decision: it reports what is stored.
#'
#' `step_order` is the position of the criterion in stored order, so every row
#' of one criterion carries the same value, whatever its `trial_id`.
#'
#' The table holds the ELIGIBILITY CASCADE only. It holds no comparator-draw
#' step and no analysis step, because `$s1_generate_enrollments_and_ipw()` stores neither
#' as a step. `.build_cohort_flow()` builds them from `$get_matching()` and from
#' `n_baseline`. Building a row is a renderer's job, so this accessor calls that
#' builder nowhere.
#'
#' The table carries no step KIND, because nothing stores one. The first stored
#' criterion is the cohort start and every later one is an exclusion. A consumer
#' labels them from `step_order`, and the accessor decides nothing.
#'
#' @param plan A TTEPlan.
#' @return A data.table with the `attrition` schema.
#' @noRd
.acc_attrition <- function(plan) {
  eids <- names(plan$enrollment_counts)
  if (is.null(eids) || length(eids) == 0L) {
    return(.acc_empty(.ACC_SCHEMA$attrition))
  }
  rows <- lapply(eids, function(eid) {
    att <- plan$enrollment_counts[[eid]]$attrition
    if (!data.table::is.data.table(att) || nrow(att) == 0L) {
      return(NULL)
    }
    if (!all(.ACC_ATTRITION_COLS %in% names(att))) {
      return(NULL)
    }
    criterion <- as.character(att$criterion)
    return(data.table::data.table(
      enrollment_id = as.character(eid),
      trial_id = as.integer(att$trial_id),
      step_order = match(criterion, unique(criterion)),
      step_name = criterion,
      n_persons = as.numeric(att$n_persons),
      n_person_trials = as.numeric(att$n_person_trials),
      n_arm_intervention = as.numeric(att$n_intervention),
      n_arm_comparator = as.numeric(att$n_comparator)
    ))
  })
  return(.acc_bind(rows, .ACC_SCHEMA$attrition))
}


#' Columns the stored comparator-draw table must carry.
#' @noRd
.ACC_MATCHING_COLS <- c(
  "trial_id",
  "n_intervention_total",
  "n_comparator_total",
  "n_intervention_enrolled",
  "n_comparator_enrolled"
)


#' The stored comparator-draw counts, one row per enrollment and trial.
#'
#' `$s1_generate_enrollments_and_ipw()` stores
#' `plan$enrollment_counts[[eid]]$matching` at one row per trial. `*_total`
#' counts every person-trial that was eligible for an arm, and `*_enrolled`
#' counts the person-trials the draw took.
#'
#' This is a SIXTH accessor rather than four more columns on
#' `$get_attrition()`. The comparator-draw table has one row per enrollment and
#' trial.
#' The attrition table has one row per enrollment, trial and criterion. Joining
#' them would repeat one comparator-draw count on every criterion row, and
#' report a
#' grain that neither producer stored.
#'
#' The accessor computes nothing. It does not sum across trials, and it derives
#' no enrolment ratio.
#'
#' @param plan A TTEPlan.
#' @return A data.table with the `matching` schema.
#' @noRd
.acc_matching <- function(plan) {
  eids <- names(plan$enrollment_counts)
  if (is.null(eids) || length(eids) == 0L) {
    return(.acc_empty(.ACC_SCHEMA$matching))
  }
  rows <- lapply(eids, function(eid) {
    mat <- plan$enrollment_counts[[eid]]$matching
    if (!data.table::is.data.table(mat) || nrow(mat) == 0L) {
      return(NULL)
    }
    if (!all(.ACC_MATCHING_COLS %in% names(mat))) {
      return(NULL)
    }
    return(data.table::data.table(
      enrollment_id = as.character(eid),
      trial_id = as.integer(mat$trial_id),
      n_intervention_total = as.numeric(mat$n_intervention_total),
      n_comparator_total = as.numeric(mat$n_comparator_total),
      n_intervention_enrolled = as.numeric(mat$n_intervention_enrolled),
      n_comparator_enrolled = as.numeric(mat$n_comparator_enrolled)
    ))
  })
  return(.acc_bind(rows, .ACC_SCHEMA$matching))
}


#' Read the estimand, the weighting and the subgroup variable out of a
#' stratified result's slot name.
#'
#' `.s3_ett_worker()` names the slot `subgroup_<variable>_<pp|itt>`, and it
#' names the companion interaction test `emtest_<variable>_<pp|itt>`. The slot
#' name is the ONLY record of which variable and which estimand the result
#' belongs to, because the stored table carries neither.
#'
#' One ETT can carry several subgroup variables. `subgroup_var` is therefore
#' part of the key, not a label. Every variable has its own `"all"` row, so a
#' key without `subgroup_var` duplicates on correct data.
#'
#' The function reads EITHER prefix, because either slot alone identifies the
#' key. `$s3_analyze()` dispatches the two as separate work items in separate
#' subprocesses, so one can be stored while the other is not.
#'
#' @param slot Character(1), a slot name.
#' @return A named list, or `NULL` when the name matches neither prefix.
#'   `subgroup_slot` and `emtest_slot` name both members of the pair.
#' @noRd
.acc_subgroup_key <- function(slot) {
  m <- regmatches(
    slot,
    regexec("^(subgroup|emtest)_(.+)_(pp|itt)$", slot)
  )[[1L]]
  if (length(m) != 4L) {
    return(NULL)
  }
  subgroup_var <- m[3L]
  suffix <- m[4L]
  return(list(
    subgroup_var = subgroup_var,
    estimand = suffix,
    weights = if (identical(suffix, "pp")) "truncated" else "untruncated",
    subgroup_slot = paste0("subgroup_", subgroup_var, "_", suffix),
    emtest_slot = paste0("emtest_", subgroup_var, "_", suffix)
  ))
}


#' The UNION of the two stratified slot families on one ETT result.
#'
#' One key per `(subgroup_var, estimand)` pair that either family names, in the
#' order the slots appear. `$get_subgroups()` iterates this union rather than
#' one family keyed off the other.
#'
#' @param res One element of `plan$results_ett`.
#' @return A list of keys, as `.acc_subgroup_key()` builds them.
#' @noRd
.acc_subgroup_keys <- function(res) {
  keys <- Filter(Negate(is.null), lapply(names(res), .acc_subgroup_key))
  if (length(keys) == 0L) {
    return(keys)
  }
  seen <- vapply(
    keys,
    function(k) paste(k$subgroup_var, k$estimand, sep = "\r"),
    character(1)
  )
  return(keys[!duplicated(seen)])
}


#' Every stored stratified estimate, one row per ETT, estimand, weighting,
#' subgroup variable and subgroup level.
#'
#' TWO p-values, and they answer different questions. `irr_pvalue` is the
#' stratum's own p-value, from `$irr_by_subgroup()`: is this stratum's rate
#' ratio distinguishable from the null? `em_pvalue` is the interaction test,
#' from `$effect_modification_test()`: do the strata differ from each other?
#' A consumer that renders one where the other belongs reports a different
#' finding, so the two never share a name.
#'
#' `em_pvalue`, `ratio_of_irrs`, `ratio_lo` and `ratio_hi` all come from the
#' `emtest_*` slot. That slot is what the Effect modification exhibit reads, so
#' the accessor and the exhibit report one stored number rather than two
#' separately computed ones. `$irr_by_subgroup()` runs the interaction test a
#' SECOND time and attaches its own `em_pvalue` and `ratio_of_irrs` attributes
#' to the stratified table. Those attributes are NOT read here. One of the two
#' calls can fail while the other succeeds, and then the exhibit and the
#' accessor would disagree.
#'
#' THE UNION OF TWO SLOT FAMILIES. `$s3_analyze()` dispatches
#' `irr_by_subgroup` and `effect_modification_test` as separate work items, in
#' separate subprocesses. Either can fail alone. The accessor therefore
#' iterates every `(subgroup_var, estimand)` pair that EITHER family names, and
#' it keys neither family off the other.
#'
#' The four states, and what each returns:
#' \itemize{
#'   \item Both stored. Full rows, with the stratum columns and the interaction
#'     columns populated.
#'   \item Stratified only. One row per stored level, with `em_pvalue`,
#'     `ratio_of_irrs`, `ratio_lo` and `ratio_hi` all `NA`.
#'   \item Interaction only. ONE row, with `subgroup_level` reading `"all"` and
#'     the four stratum columns `NA`. No stored table names the levels, so the
#'     accessor MUST NOT invent a stratum row. `"all"` is the level
#'     `$irr_by_subgroup()` gives the whole-cohort row, and it is what
#'     `.write_effect_modification()` emits in this state.
#'   \item Neither stored. No rows.
#' }
#'
#' A skipped stratified result reads as absent. `.acc_slot()` rejects the skip
#' envelope a failed worker stores, so a skipped `subgroup_*` beside a stored
#' `emtest_*` gives the interaction-only row.
#'
#' The accessor MUST NOT fall back to the `em_pvalue` and `ratio_of_irrs`
#' attributes that `$irr_by_subgroup()` attaches to the stratified table.
#'
#' The four interaction columns are one number each for the whole stratified
#' result, so they repeat on every row of that result. A renderer that wants
#' them once shows them on the `"all"` row.
#'
#' `ratio_of_irrs` is `NA` unless the subgroup variable has exactly two levels.
#' `$effect_modification_test()` reports a ratio for a binary subgroup only.
#'
#' @param plan A TTEPlan.
#' @return A data.table with the `subgroups` schema.
#' @noRd
.acc_subgroups <- function(plan) {
  ett_ids <- names(plan$results_ett)
  if (is.null(ett_ids) || length(ett_ids) == 0L) {
    return(.acc_empty(.ACC_SCHEMA$subgroups))
  }
  rows <- list()
  for (id in ett_ids) {
    res <- plan$results_ett[[id]]
    for (key in .acc_subgroup_keys(res)) {
      value <- .acc_slot(res, key$subgroup_slot)
      # The interaction test, READ from the slot the exhibit reads. Never from
      # the attributes `$irr_by_subgroup()` attaches to `value`.
      em <- .acc_slot(res, key$emtest_slot)
      has_strata <- data.table::is.data.table(value) && nrow(value) > 0L
      if (!has_strata && is.null(em)) {
        next
      }
      # One `"all"` row when only the interaction test survives. Nothing names
      # the levels, so nothing invents them.
      level <- if (has_strata) as.character(value[["level"]]) else "all"
      pick <- function(nm) {
        if (has_strata) return(as.numeric(value[[nm]])) else return(NA_real_)
      }
      rows[[length(rows) + 1L]] <- data.table::data.table(
        ett_id = as.character(id),
        estimand = key$estimand,
        weights = key$weights,
        subgroup_var = key$subgroup_var,
        subgroup_level = level,
        strata_stored = has_strata,
        irr = pick("IRR"),
        irr_lo = pick("IRR_lower"),
        irr_hi = pick("IRR_upper"),
        irr_pvalue = pick("IRR_pvalue"),
        em_pvalue = .acc_num1(em, "p_value"),
        ratio_of_irrs = .acc_num1(em, "ratio_of_irrs"),
        ratio_lo = .acc_num1(em, "ratio_lower"),
        ratio_hi = .acc_num1(em, "ratio_upper")
      )
    }
  }
  return(.acc_bind(rows, .ACC_SCHEMA$subgroups))
}
