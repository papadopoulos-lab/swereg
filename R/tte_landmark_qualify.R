# =============================================================================
# Landmark qualification
# =============================================================================

#' Read the 0-indexed week index of each row.
#'
#' The index is the position of `isoyearweek` in
#' `cstime::dates_by_isoyearweek`, minus one. `.assign_trial_ids()` reads the
#' same scale. It sets `trial_id` to `(position - 1) %/% period_width`. So
#' `week_index %/% period_width` is the band, and `week_index %% period_width`
#' is the offset inside it.
#'
#' @param isoyearweek Character vector of ISO year-weeks.
#' @return An integer vector the same length as `isoyearweek`. A week the
#'   calendar does not carry reads `NA`.
#' @noRd
.tte_week_index0 <- function(isoyearweek) {
  return(data.table::chmatch(
    as.character(isoyearweek),
    cstime::dates_by_isoyearweek$isoyearweek
  ) -
    1L)
}


#' Keep the person-bands that qualify at the landmark.
#'
#' The landmark of a person-band is the week that closes its entry band. Band
#' `b` covers week indices `b * period_width` to `(b + 1) * period_width - 1`.
#' Its landmark sits at week index `(b + 1) * period_width`. That week is the
#' first week of band `b + 1`. `.tte_week_index0()` defines the scale.
#'
#' A person-band qualifies when both statements hold.
#'
#' 1. The person is under observation at the landmark.
#' 2. No outcome occurrence of the enrollment stops at or before the landmark.
#'
#' Statement 1 reads `design$observed_var`. The `row_presence` sentinel reads
#' the row being there as the observation. A named column has to hold `TRUE`
#' on that row.
#'
#' The last band of the data has no landmark, because no week follows it. No
#' band there qualifies, and no trial opens in it. That is the intended
#' behaviour: a trial whose landmark falls past the end of the data has no
#' follow-up to contribute.
#'
#' A week is a half-open interval. An outcome occurrence in week index `w`
#' therefore stops at `w + 1`. Statement 2 excludes the band when
#' `w + 1 <= (b + 1) * period_width`. That covers every week of the entry band
#' and every week before it.
#'
#' A woman may have the event in her entry band and start treatment later in
#' the same band. She is excluded. The earlier code enrolled her into the
#' intervention arm with the event already behind her.
#'
#' Statement 2 reads EVERY column in `design$outcome_vars`, and not the one
#' outcome a later step analyses. One enrollment serves several outcomes:
#' `$enrollment_spec()` collects every ETT that shares an `enrollment_id`, and
#' the s2 worker fans out over them. One enrolled set therefore has to be
#' event-free for all of them.
#'
#' @section Eligibility is a baseline property, and it is NOT re-read here:
#' `design$eligible_var` is assessed on the entry band, by
#' `.band_baseline_treatment()`. It is not assessed again at the landmark, and
#' re-reading it there would empty the intervention arm.
#'
#' swereg requires a new-user or washout exclusion on the treatment variable.
#' `tteplan_read_spec()` warns when an enrollment declares none. That exclusion
#' sets `eligible` to `FALSE` from the week after initiation. An initiator
#' starts inside her entry band, and her landmark always falls after that week,
#' so she is ineligible at her own landmark by construction.
#'
#' Measured on the `ttm_skeleton()` fixture that `test-s1a_declared_outputs.R`
#' builds: 21 of 21 intervention person-bands were ineligible at the landmark.
#' Of the 361 comparator bands that reached a landmark, 0 were ineligible
#' there.
#'
#' The criterion that defines the intervention arm is therefore the same
#' criterion that would delete it. Sequential-trial designs assess eligibility
#' at the start of a trial's eligibility window. They assess survival and
#' event-freedom through the grace window (Danaei et al. 2013, Caniglia et al.
#' 2023). This function follows that split.
#'
#' Run this AFTER the arm classification and BEFORE the comparator draw. The
#' order carries two properties. Attrition can report the arms, because each
#' band already carries one. Sampling refills the ratio from qualified
#' comparators, because the pool it draws from holds nothing else.
#'
#' **Qualification needs the observation contract, so it runs only when
#' `design$observed_var` is set.** A design that declares no encoding cannot
#' say whether an absent week is an unobserved week or a week outside the
#' study. `tteplan_read_spec()` makes the declaration mandatory, so every
#' spec-driven enrollment qualifies. A [TTEDesign] built by hand without
#' `observed_var` does not, and this function returns its input unchanged.
#'
#' @param bands A data.table with one row per candidate person-band. It MUST
#'   carry `person_id_col`, `trial_id` and `arm_col`. Row order is preserved,
#'   which is what keeps the seeded comparator draw reproducible.
#' @param data The person-week source data. It MUST carry `person_id_col`,
#'   `isoyearweek`, every column in `design$outcome_vars`, and the observation
#'   column when the design names one.
#' @param design A [TTEDesign].
#' @param person_id_col Character, the person identifier column.
#' @param arm_col Character, the logical arm column of `bands`. `TRUE` is the
#'   intervention arm and `FALSE` is the comparator arm.
#' @return A list with two elements. `bands` holds the qualified rows, in the
#'   order they arrived. `attrition` holds the criterion-level counts, or
#'   `NULL` when the design declares no `observed_var`.
#' @noRd
.tte_qualify_bands <- function(
  bands,
  data,
  design,
  person_id_col,
  arm_col
) {
  lm_pid <- lm_band <- lm_obs <- i.lm_obs <- NULL # nolint
  fe_pid <- fe_w <- fe_week <- i.fe_week <- NULL # nolint
  .tte_landmark <- trial_id <- NULL # nolint

  if (is.null(design$observed_var)) {
    return(list(bands = bands, attrition = NULL))
  }

  period_width <- as.integer(design$period_width)
  observed_col <- .tte_observed_column(design$observed_var)
  outcome_cols <- design$outcome_vars

  missing_cols <- setdiff(
    c(outcome_cols, observed_col, "isoyearweek", person_id_col),
    names(data)
  )
  if (length(missing_cols) > 0L) {
    stop(
      "Landmark qualification cannot read column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Every outcome in the design MUST reach the enrollment data, or a ",
      "person with the event before the landmark enrolls unnoticed.",
      call. = FALSE
    )
  }

  week_index <- .tte_week_index0(data[["isoyearweek"]])
  person <- data[[person_id_col]]

  # --- landmark rows -------------------------------------------------------
  # A landmark sits on a band boundary, so only a row whose week index is a
  # multiple of `period_width` can be one. The row at week index
  # `(b + 1) * period_width` is the landmark of band `b`, so it serves the
  # band one below its own.
  is_boundary <- !is.na(week_index) & (week_index %% period_width == 0L)
  landmark <- data.table::data.table(
    lm_pid = person[is_boundary],
    lm_band = (week_index[is_boundary] %/% period_width) - 1L,
    lm_obs = if (is.null(observed_col)) {
      # The `row_presence` sentinel. The caller has already deleted every
      # unobserved person-week, so the row being here IS the observation.
      TRUE
    } else {
      .tte_is_true(data[[observed_col]][is_boundary])
    }
  )
  # A person-week skeleton holds one row per (person, week), so this grouping
  # is an identity on well-formed data. It is here so that a duplicated week
  # cannot duplicate a candidate band through the join below.
  landmark <- landmark[, list(lm_obs = any(lm_obs)), by = list(lm_pid, lm_band)]

  # --- first outcome occurrence per person ---------------------------------
  has_event <- rep(FALSE, nrow(data))
  for (oc in outcome_cols) {
    has_event <- has_event | .tte_is_true(data[[oc]])
  }
  # data.table evaluates `j` once on an empty table to learn its types, so a
  # cohort with no event at all would reach `min(integer(0))` and warn. Build
  # the empty result directly instead.
  event_keep <- has_event & !is.na(week_index)
  first_event <- if (any(event_keep)) {
    data.table::data.table(
      fe_pid = person[event_keep],
      fe_w = week_index[event_keep]
    )[, list(fe_week = min(fe_w)), by = fe_pid]
  } else {
    data.table::data.table(fe_pid = person[0L], fe_week = integer(0))
  }

  # --- apply, in cascade order ---------------------------------------------
  # Update-joins, so the row order of `bands` survives untouched. The `on`
  # names are columns of `qb` and the values are columns of the joined table,
  # so both mappings are built by name rather than written as literals.
  qb <- data.table::copy(bands)
  qb[, .tte_landmark := (as.integer(trial_id) + 1L) * period_width]
  # A band with no row at its landmark keeps the FALSE default and so fails
  # observation. That is the one place an absent landmark row is reported.
  qb[, lm_obs := FALSE]
  on_landmark <- stats::setNames(
    c("lm_pid", "lm_band"),
    c(person_id_col, "trial_id")
  )
  qb[landmark, on = on_landmark, lm_obs := i.lm_obs]
  qb[, fe_week := NA_integer_]
  qb[
    first_event,
    on = stats::setNames("fe_pid", person_id_col),
    fe_week := i.fe_week
  ]

  # Both vectors are logical and never NA. A band whose `isoyearweek` is
  # outside the calendar has an NA `trial_id`, and so an NA landmark. It fails
  # observation, because no landmark row can carry an NA band. Writing the
  # event test so it cannot return NA either keeps the second vector clean:
  # `bands[NA]` returns a row of NAs rather than dropping it.
  pass_observed <- qb$lm_obs
  # `w + 1 <= landmark` for the first occurrence is `fe_week < landmark`.
  event_free <- is.na(qb$fe_week) |
    (!is.na(qb$.tte_landmark) & qb$fe_week >= qb$.tte_landmark)
  pass_event_free <- pass_observed & event_free

  arm <- .tte_is_true(bands[[arm_col]])
  attrition <- data.table::rbindlist(
    list(
      .tte_qualify_attrition_rows(
        bands,
        person_id_col,
        arm,
        rep(TRUE, nrow(bands)),
        "landmark_candidates"
      ),
      .tte_qualify_attrition_rows(
        bands,
        person_id_col,
        arm,
        pass_observed,
        "landmark_observed"
      ),
      .tte_qualify_attrition_rows(
        bands,
        person_id_col,
        arm,
        pass_event_free,
        "landmark_event_free"
      )
    ),
    use.names = TRUE
  )

  return(list(bands = bands[pass_event_free], attrition = attrition))
}


#' Read a column as a strict logical, with `NA` as `FALSE`.
#'
#' @param x A logical, numeric or character vector.
#' @return A logical vector the same length as `x`, and never `NA`.
#' @noRd
.tte_is_true <- function(x) {
  if (is.logical(x)) {
    return(!is.na(x) & x)
  }
  y <- suppressWarnings(as.logical(x))
  return(!is.na(y) & y)
}


#' Read a column as a strict logical false, with `NA` as `FALSE`.
#'
#' This is not the negation of [.tte_is_true()]. A value that is neither true
#' nor false reads `FALSE` under both functions.
#'
#' @param x A logical, numeric or character vector.
#' @return A logical vector the same length as `x`, and never `NA`.
#' @noRd
.tte_is_false <- function(x) {
  if (is.logical(x)) {
    return(!is.na(x) & !x)
  }
  y <- suppressWarnings(as.logical(x))
  return(!is.na(y) & !y)
}


#' Count one step of the landmark cascade.
#'
#' @param bands The candidate person-bands.
#' @param person_id_col Character, the person identifier column.
#' @param arm Logical vector, `TRUE` for the intervention arm.
#' @param keep Logical vector, the rows this step still holds.
#' @param label Character, the criterion name to report.
#' @return A data.table with one row per `trial_id`, plus one row carrying
#'   `trial_id = NA` for the whole cohort. The columns match
#'   `.s1_compute_attrition()`, so both tables stack.
#' @noRd
.tte_qualify_attrition_rows <- function(
  bands,
  person_id_col,
  arm,
  keep,
  label
) {
  qa_pid <- qa_arm <- trial_id <- criterion <- NULL # nolint
  x <- data.table::data.table(
    qa_pid = bands[[person_id_col]][keep],
    trial_id = bands[["trial_id"]][keep],
    qa_arm = arm[keep]
  )
  j <- quote(list(
    n_persons = data.table::uniqueN(qa_pid),
    n_person_trials = .N,
    n_intervention = sum(qa_arm),
    n_comparator = sum(!qa_arm)
  ))
  per_trial <- x[!is.na(trial_id), eval(j), by = trial_id]
  overall <- x[, eval(j)]
  overall[, trial_id := NA_integer_]
  out <- data.table::rbindlist(list(per_trial, overall), use.names = TRUE)
  out[, criterion := label]
  return(out[])
}
