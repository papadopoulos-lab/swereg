# Trial bands: the band each person-week falls in, and the treatment a
# person carries at the start of a band.

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

#' Decide the baseline treatment of each person-band
#'
#' Single source of truth for the `(person, band) -> baseline treatment`
#' mapping. `.s1_eligible_tuples()` (s1a scout) and `enroll()` Phase C (direct
#' enrollment) both call it. `vignette("tte-methods")` states the same rule.
#'
#' The function reads only the weeks of the band that are eligible and carry one
#' of the two protocol arms. It drops every other week of the band first. It
#' then classifies the person-band into one of three states.
#'
#' 1. Intervention, when at least one week it reads holds `TRUE`.
#' 2. Comparator, when every week it reads holds `FALSE`.
#' 3. Ineligible for that band, when it reads no week at all.
#'
#' The drop comes first, so a week outside the two arms neither creates nor
#' prevents a comparator classification. A band of `FALSE`, `NA`, `FALSE`,
#' `FALSE` is therefore a comparator band. A band of `NA`, `TRUE`, `FALSE`,
#' `FALSE` is an intervention band.
#'
#' This function returns no row for a band in state 3. The caller then counts
#' that band as excluded, and never as a comparator.
#'
#' `any()` reads every week it keeps, so the caller does not sort the rows
#' first. Initiation in any week of the band assigns the person to that band.
#' Follow-up then opens at the landmark, so the band carries no within-band
#' immortal time (Caniglia et al. 2023).
#'
#' @section The recruiting week:
#' The result also carries `recruit_week_index`, the week that recruited the
#' person into that band. It is the EARLIEST week the function reads, which is
#' the earliest week that is both eligible and in an arm. For an initiator that
#' is her initiation week. For a comparator it is her first eligible comparator
#' week. The rule is symmetric across the arms, which no rule keyed to
#' initiation can be.
#'
#' Eligibility at the recruiting week is true by construction, because
#' eligibility is part of what makes a week survive the `keep` mask. There is
#' therefore no eligibility criterion to re-assess later, and
#' `.tte_qualify_bands()` has none.
#'
#' `min()` is order-independent, exactly as `any()` is, so this adds no sort.
#' It reads `isoyearweek` as a string. That is safe: every week in
#' `cstime::dates_by_isoyearweek` matches `YYYY-WW` with a zero-padded week, so
#' the strings sort chronologically. `.tte_week_index0()` then converts one
#' value per person-band, and not one per person-week.
#'
#' `recruit_week_index` reports WHEN the person qualified.
#' `.tte_entry_snapshot()` reads her confounders at that instant, into the
#' `.tte_entry__` columns of the panel.
#'
#' @param data A data.table with a `trial_id` column and an `isoyearweek`
#'   column. This function does not modify it.
#' @param person_id_col Character, the person identifier column.
#' @param treatment_col Character, the treatment column. It holds `TRUE` for the
#'   intervention arm, `FALSE` for the comparator arm, and `NA` outside the two
#'   arms.
#' @param eligible_col Character or NULL, the eligibility column. The function
#'   keeps a week only when this column holds `TRUE`, and it treats `NA` as not
#'   eligible. `NULL` keeps every week of `data`.
#' @param out_col Character, the name of the treatment column in the result.
#' @return A data.table with one row per person-band that holds at least one
#'   eligible in-arm week. Its columns are `person_id_col`, `trial_id`,
#'   `out_col` and `recruit_week_index`.
#' @noRd
.band_baseline_treatment <- function(
  data,
  person_id_col,
  treatment_col,
  eligible_col = NULL,
  out_col = "band_treatment"
) {
  recruit_isoyearweek <- recruit_week_index <- NULL # nolint
  if (!"isoyearweek" %in% names(data)) {
    stop(
      "`.band_baseline_treatment()` needs an `isoyearweek` column. It reports ",
      "the week that recruited each person into her band.",
      call. = FALSE
    )
  }
  keep <- !is.na(data[[treatment_col]])
  if (!is.null(eligible_col)) {
    elig <- data[[eligible_col]]
    keep <- keep & !is.na(elig) & as.logical(elig)
  }
  # The j expression names the treatment column directly, rather than reaching
  # it with get(). data.table runs j once per group, so a get() there costs one
  # symbol lookup per group. Neither form reaches GForce, which does not cover
  # any(). On a 2M-row, 500k-group probe the direct form ran 2.3x faster, and
  # the scout path groups a 17M-row skeleton.
  j <- substitute(
    list(any(v), min(isoyearweek)),
    list(v = as.name(treatment_col))
  )
  res <- data[keep, eval(j), by = c(person_id_col, "trial_id")]
  data.table::setnames(res, c("V1", "V2"), c(out_col, "recruit_isoyearweek"))
  res[, recruit_week_index := .tte_week_index0(recruit_isoyearweek)]
  res[, recruit_isoyearweek := NULL]
  res[]
}
