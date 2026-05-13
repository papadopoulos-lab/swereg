# Phase 2: column cross-walk + bridge function.
#
# Cross-walk:
#   TrialEmulation column   <->  swereg skeleton equivalent
#   id                      <->  id
#   period (integer)        <->  isoyearweek (string) [filter is_isoyear == FALSE]
#   eligible                <->  user-derived column (e.g., rd_eligible)
#   outcome                 <->  add_diagnoses() output column (e.g., f64_diag)
#   treatment               <->  add_rx() output column (e.g., antidepressants)
#   catvarA/B, nvarA/B/C    <->  rd_* / ri_* confounders
#
# Shape differences:
#   - swereg has BOTH year-aggregate rows (is_isoyear=TRUE) and weekly rows;
#     filter to weekly for TTE.
#   - swereg's `isoyearweek` is a string; convert to integer `period` by factor index.
#   - swereg adds `personyears`, `isoyearweeksun` etc. — drop these for TE.

#' Convert a swereg-style skeleton to TrialEmulation long-format input.
#'
#' @param skeleton data.table. A swereg skeleton OR any long-format
#'   person-period dataset with the columns named below.
#' @param id_col character. Name of the person ID column in skeleton.
#' @param period_col character. Name of the period column. If it's
#'   `isoyearweek` (or any character/factor), it's converted to integer
#'   period via sorted unique levels. If already integer, used as-is.
#' @param eligible_col character. Name of the eligibility flag.
#' @param outcome_col character. Name of the outcome indicator.
#' @param treatment_col character. Name of the treatment indicator.
#' @param covariate_cols character vector. Confounder columns.
#' @param drop_year_aggregate logical. If TRUE and `is_isoyear` column
#'   exists, filter to rows where `is_isoyear == FALSE`.
#'
#' @return data.table with columns: id, period, eligible, outcome,
#'   treatment, and the covariate columns (unchanged).
skeleton_to_trialemulation <- function(
  skeleton,
  id_col = "id",
  period_col = "isoyearweek",
  eligible_col,
  outcome_col,
  treatment_col,
  covariate_cols,
  drop_year_aggregate = TRUE
) {
  stopifnot(data.table::is.data.table(skeleton))
  dt <- data.table::copy(skeleton)

  if (drop_year_aggregate && "is_isoyear" %in% names(dt)) {
    dt <- dt[is_isoyear == FALSE]
  }

  # Convert period to integer if needed
  if (!is.integer(dt[[period_col]])) {
    levs <- sort(unique(dt[[period_col]]))
    dt[, period := as.integer(factor(get(period_col), levels = levs))]
  } else {
    data.table::setnames(dt, period_col, "period")
  }

  keep <- c(id_col, "period", eligible_col, outcome_col, treatment_col, covariate_cols)
  out <- dt[, keep, with = FALSE]
  data.table::setnames(
    out,
    c(id_col, eligible_col, outcome_col, treatment_col),
    c("id", "eligible", "outcome", "treatment")
  )
  # TrialEmulation expects integer 0/1
  out[, eligible := as.integer(eligible)]
  out[, outcome := as.integer(outcome)]
  out[, treatment := as.integer(treatment)]
  out[]
}
