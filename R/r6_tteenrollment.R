# =============================================================================
# TTEDesign + TTEEnrollment R6 classes, constructors, and helpers
# =============================================================================
# This file contains the two enrollment-side R6 classes and standalone helpers:
#
#   1. TTEDesign R6 class
#   2. TTEEnrollment R6 class (weight/draw/collapse logic in private methods)
#   3. summary.TTEEnrollment S3 method
#   4. tteenrollment_rbind(), tteenrollment_rates_combine(),
#      tteenrollment_irr_combine(), tteenrollment_impute_confounders()
# =============================================================================

.TTE_DESIGN_SCHEMA_VERSION <- 3L
.TTE_ENROLLMENT_SCHEMA_VERSION <- 3L


# =============================================================================
# The observation contract
# =============================================================================
# One definition of "observed", shared by the spec parser, TTEDesign and the
# s1 cache. An enrollment states how observation is encoded. It never lets the
# reader infer it.
#
#   observed_var: {column: rd_observed}      a real logical person-week column
#   observed_var: {sentinel: row_presence}   the skeleton is trimmed
#
# `row_presence` asserts that the caller already deleted every unobserved
# person-week, so a row exists if and only if the person was observed that
# week. The production skeleton is built this way. It deletes every
# person-week up to and including first immigration, every person-week on or
# after emigration, and every person-week after death. It keeps the death week
# itself. A real `observed` column there would hold TRUE on every retained row
# and could not represent an absent week. The sentinel makes that assumption
# explicit and testable. Row presence as a silent proxy stays forbidden.

# The sentinel values this version of swereg understands.
.TTE_OBSERVED_SENTINELS <- "row_presence"

#' Build a normalised observation encoding.
#'
#' @param column Character scalar or `NA_character_`, the logical column name.
#' @param sentinel Character scalar or `NA_character_`, the sentinel name.
#' @return A `tte_observed_var` list with `column` and `sentinel`.
#' @noRd
.tte_new_observed_var <- function(
  column = NA_character_,
  sentinel = NA_character_
) {
  structure(
    list(column = column, sentinel = sentinel),
    class = "tte_observed_var"
  )
}

#' Normalise one `observed_var` declaration.
#'
#' The single entry point for the observation contract. The spec parser,
#' `TTEDesign$new()` and any later landmark code all go through it, so one
#' declaration cannot mean two things in two places.
#'
#' @param x The declaration. `NULL` when the caller declares nothing. A list
#'   with exactly one of `column` or `sentinel`. An already-normalised
#'   `tte_observed_var` passes through unchanged, so the function is
#'   idempotent.
#' @param context Character, the name to report in an error message.
#' @return `NULL` when `x` is `NULL`. Otherwise a `tte_observed_var` list.
#' @noRd
.tte_observed_var <- function(x, context = "observed_var") {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "tte_observed_var")) {
    return(x)
  }
  if (!is.list(x) || length(x) == 0L || is.null(names(x))) {
    stop(
      context,
      " must be a mapping with exactly one of `column` or `sentinel`. ",
      "Write `",
      context,
      ": {column: <name>}` for a real logical column, or `",
      context,
      ": {sentinel: row_presence}` for a trimmed skeleton.",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(x), c("column", "sentinel"))
  if (length(unknown) > 0L) {
    stop(
      context,
      " has unknown key(s): ",
      paste(unknown, collapse = ", "),
      ". Use `column` or `sentinel`.",
      call. = FALSE
    )
  }
  # Test KEY PRESENCE, not value presence. `observed_var: {column: null,
  # sentinel: row_presence}` is valid YAML and parses to a two-key list whose
  # `column` value is NULL. A `!is.null()` test reads that as one key and
  # accepts it. A reader of the YAML sees two claims, so swereg MUST reject
  # it. `[[` is used throughout, because `$` does partial name matching.
  has_column <- "column" %in% names(x)
  has_sentinel <- "sentinel" %in% names(x)
  if (has_column && has_sentinel) {
    stop(
      context,
      " gives both `column` and `sentinel`. Give exactly one. ",
      "A named column and a trimmed skeleton are different claims.",
      call. = FALSE
    )
  }
  if (!has_column && !has_sentinel) {
    stop(
      context,
      " must give exactly one of `column` or `sentinel`.",
      call. = FALSE
    )
  }
  if (has_column) {
    value <- x[["column"]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)
    ) {
      stop(
        context,
        "$column must be a single non-empty column name.",
        call. = FALSE
      )
    }
    return(.tte_new_observed_var(column = value))
  }
  value <- x[["sentinel"]]
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop(context, "$sentinel must be a single sentinel name.", call. = FALSE)
  }
  if (!value %in% .TTE_OBSERVED_SENTINELS) {
    stop(
      context,
      "$sentinel is '",
      value,
      "', which swereg does not know. The known sentinel(s): ",
      paste(.TTE_OBSERVED_SENTINELS, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  .tte_new_observed_var(sentinel = value)
}

#' Read the column name out of an observation encoding.
#'
#' @param x A `tte_observed_var`, or `NULL`.
#' @return The column name, or `NULL` when the encoding names no column.
#' @noRd
.tte_observed_column <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  # `[[` is exact. `$` does partial name matching, which is unsafe on a field
  # read from a user's YAML file.
  col <- x[["column"]]
  if (is.null(col) || is.na(col)) {
    return(NULL)
  }
  col
}

#' Check one arm tolerance.
#'
#' A tolerance MUST be a finite, representable, non-negative whole number of
#' weeks. The function NEVER returns `NA`.
#'
#' `is.finite()` carries three of the rejections at once: `NA`, `NaN`, `Inf`
#' and `-Inf` are all not finite. The upper bound carries the fourth.
#' `as.integer(3e9)` returns `NA` with only a warning, and `Inf` does the same,
#' so a value that passes the whole-number test can still land as `NA`. An `NA`
#' tolerance compares as neither tolerated nor discordant in every later
#' adherence rule, which is worse than a loud error here.
#'
#' @param x The declared value, or `NULL` for the default of zero weeks.
#' @param context Character, the name to report in an error message.
#' @return An integer scalar between 0 and `.Machine$integer.max`. Never `NA`.
#' @noRd
.tte_tolerance_weeks <- function(x, context) {
  if (is.null(x)) {
    return(0L)
  }
  ok <- is.numeric(x) &&
    length(x) == 1L &&
    is.finite(x) &&
    x >= 0 &&
    x <= .Machine$integer.max &&
    x == trunc(x)
  if (!ok) {
    stop(
      context,
      " must be a single whole number of weeks, at least 0 and at most ",
      .Machine$integer.max,
      ". It MUST be finite. Got a ",
      class(x)[1],
      " of length ",
      length(x),
      ": ",
      paste(format(x), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  as.integer(x)
}


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
  data.table::chmatch(
    as.character(isoyearweek),
    cstime::dates_by_isoyearweek$isoyearweek
  ) -
    1L
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

  list(bands = bands[pass_event_free], attrition = attrition)
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
  !is.na(y) & y
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
  !is.na(y) & !y
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
  out[]
}


# =============================================================================
# Entry-window snapshots
# =============================================================================

#' The prefix that marks an entry-window snapshot column.
#'
#' A confounder name MUST NOT start with it. [TTEDesign] rejects one that does.
#' @noRd
.TTE_ENTRY_PREFIX <- ".tte_entry__"

#' Name the entry-window snapshot column of each confounder.
#'
#' @param vars Character vector of confounder names.
#' @return A character vector of the same length as `vars`.
#' @noRd
.tte_entry_col <- function(vars) {
  if (length(vars) == 0L) {
    return(character(0))
  }
  paste0(.TTE_ENTRY_PREFIX, vars)
}

#' Stop on a confounder name that takes the reserved prefix.
#'
#' @param vars Character vector of confounder names.
#' @return `vars`, invisibly.
#' @noRd
.tte_check_entry_names <- function(vars) {
  if (length(vars) == 0L) {
    return(invisible(vars))
  }
  bad <- vars[startsWith(as.character(vars), .TTE_ENTRY_PREFIX)]
  if (length(bad) > 0L) {
    stop(
      "A confounder name MUST NOT start with '",
      .TTE_ENTRY_PREFIX,
      "'. swereg reserves that prefix for the entry-window snapshot of each ",
      "confounder. Rename: ",
      paste(bad, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(vars)
}

#' Report whether a trial panel carries a complete entry-window snapshot.
#'
#' The answer is `TRUE` when every confounder has its `.tte_entry__` column. It
#' is `FALSE` when no confounder has one. A partial set stops the run, because
#' baseline adjustment MUST read every confounder at the same instant.
#'
#' A panel with no snapshot reads the collapsed value of the follow-up band.
#' That is what every release before this one did. Two panels reach that state:
#' one built by an earlier release, and one whose entry rows carried no
#' `recruit_week_index`.
#'
#' @param data A data.table.
#' @param confounder_vars Character vector of confounder names.
#' @return `TRUE` or `FALSE`.
#' @noRd
.tte_has_entry_snapshot <- function(data, confounder_vars) {
  if (length(confounder_vars) == 0L) {
    return(FALSE)
  }
  cols <- .tte_entry_col(confounder_vars)
  present <- cols %in% names(data)
  if (all(present)) {
    return(TRUE)
  }
  if (!any(present)) {
    return(FALSE)
  }
  stop(
    "The trial panel holds an entry-window snapshot for some confounders and ",
    "not for others. Missing: ",
    paste(cols[!present], collapse = ", "),
    ". Baseline adjustment MUST read every confounder at the same instant.",
    call. = FALSE
  )
}

#' Read every confounder at the recruiting week of each person-trial.
#'
#' The recruiting week is the earliest week of the entry band that is both
#' eligible and in an arm. `.band_baseline_treatment()` computes it, and
#' `entry_dt` carries it as `recruit_week_index`.
#'
#' The first week of the entry window is the wrong instant. A woman need not be
#' eligible there, and she need not be in an arm there. A covariate read there
#' can describe a woman who was not yet in the trial.
#'
#' A person-trial with no row at its recruiting week reads `NA`. This function
#' never substitutes a nearby week.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id` and `id_var`. It MUST also carry `recruit_week_index`,
#'   or this function returns `NULL`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col` and `isoyearweek`.
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param confounder_vars Character vector of confounder names.
#' @param id_var Character, the person-trial identifier column.
#' @return A data.table keyed by `id_var`, with one `.tte_entry__<v>` column
#'   per confounder. `NULL` when there is nothing to read.
#' @noRd
.tte_entry_snapshot <- function(
  entry_dt,
  data_enrolled,
  person_id_col,
  confounder_vars,
  id_var
) {
  .tte_pid <- .tte_week <- NULL # nolint
  conf <- intersect(confounder_vars, names(data_enrolled))
  if (length(conf) == 0L) {
    return(NULL)
  }
  if (!"recruit_week_index" %in% names(entry_dt)) {
    return(NULL)
  }
  if (!"isoyearweek" %in% names(data_enrolled)) {
    return(NULL)
  }

  # The instant each person-trial is read at. It is the recruiting week, and
  # not the first week of the entry window.
  week_index <- as.integer(entry_dt[["recruit_week_index"]])

  # Match on the week STRING rather than on a week index. `data_enrolled` can
  # hold millions of rows, and an index column would allocate one integer
  # vector that long. `.tte_week_index0()` defines the inverse mapping.
  want <- data.table::data.table(
    .tte_pid = entry_dt[[".tte_person_id"]],
    .tte_week = cstime::dates_by_isoyearweek$isoyearweek[week_index + 1L]
  )
  row_of <- data_enrolled[
    want,
    on = stats::setNames(
      c(".tte_pid", ".tte_week"),
      c(person_id_col, "isoyearweek")
    ),
    which = TRUE,
    mult = "first"
  ]

  # `data_enrolled[NA_integer_]` returns a row of NA, so a person-trial with no
  # row at its recruiting week reads NA on every confounder.
  out <- data_enrolled[row_of, conf, with = FALSE]
  data.table::setnames(out, conf, .tte_entry_col(conf))
  data.table::set(out, j = id_var, value = entry_dt[[id_var]])
  data.table::setkeyv(out, id_var)
  out[]
}


# =============================================================================
# Deviation boundary
# =============================================================================

#' Place the deviation boundary of every enrolled person-trial.
#'
#' The boundary is the week that follow-up stops at, counted from the landmark.
#' It is exact to the week, and it comes from the weekly assessments. It is an
#' exclusive stop. See the interval convention section of [TTEDesign].
#'
#' `enroll()` collapses each band to one row, so the weekly sequence is gone by
#' the time `s5_prepare_outcome()` runs. This function reads the sequence here,
#' where it still exists. It returns one integer per person-trial, and never a
#' weekly panel.
#'
#' @section Discordance and the arm tolerance:
#'
#' An assessment is discordant when `design$time_treatment_var` does not hold
#' the assigned arm of that person-trial. `NA` is discordant in both arms.
#'
#' A tolerance is the number of CONSECUTIVE discordant assessments an arm
#' allows. A concordant assessment resets the run. Each arm reads its own
#' tolerance: `design$intervention_tolerance_weeks` and
#' `design$comparator_tolerance_weeks`.
#'
#' For tolerance `k`, the boundary is the right edge of the `(k + 1)`th
#' consecutive discordant week. A run that starts at week `u0` therefore gives
#' `(u0 + k + 1) - L`, where `L` is the landmark week. A tolerance of 0 censors
#' at the first discordant week.
#'
#' A run that starts before the landmark counts only its weeks at or after it.
#' The `u >= L + k` test below is what enforces that.
#'
#' @section Loss of observation:
#'
#' Loss of observation is not discordance, and no tolerance applies to it. An
#' internal gap in the weekly sequence stops follow-up at the first absent
#' week. The person may return in a later week. She is censored at the gap.
#'
#' A record that simply ends carries no internal gap. `s5_prepare_outcome()`
#' already reports that case as `weeks_to_loss`, read from the panel itself.
#'
#' @section Runs are read over the observed weeks only:
#'
#' A run is a set of discordant weeks with consecutive week indices. An absent
#' week therefore breaks a run, and the gap boundary always falls at or before
#' the boundary the unbroken run would give.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id`, `entry_band_id`, `baseline_tx` and `id_var`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col` and `isoyearweek`.
#' @param design A [TTEDesign].
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param id_var Character, the person-trial identifier column.
#' @param n_follow_up_bands Integer, the number of follow-up bands the panel
#'   holds. A boundary past the last band reads `NA`.
#' @return A data.table keyed by `id_var`, with one integer column
#'   `weeks_to_protocol_deviation`. `NULL` when the design cannot support the
#'   weekly read.
#' @noRd
.tte_deviation_boundary <- function(
  entry_dt,
  data_enrolled,
  design,
  person_id_col,
  id_var,
  n_follow_up_bands
) {
  dv_pid <- dv_week <- dv_next <- dv_run <- dv_start <- NULL # nolint
  dv_len <- dv_hit <- q_week <- NULL # nolint

  tx_col <- design$time_treatment_var
  # Without an observation encoding swereg cannot tell an absent week from a
  # week outside the study, so it cannot report a gap. Phase 8 gates landmark
  # qualification on the same field, and the two MUST agree.
  if (is.null(design$observed_var)) {
    return(NULL)
  }
  if (is.null(tx_col) || !tx_col %in% names(data_enrolled)) {
    return(NULL)
  }
  if (!"isoyearweek" %in% names(data_enrolled)) {
    return(NULL)
  }
  if (nrow(entry_dt) == 0L) {
    return(NULL)
  }

  period_width <- as.integer(design$period_width)
  span <- as.integer(n_follow_up_bands) * period_width

  # --- the observed weekly assessments -------------------------------------
  # A row that fails the observation test is dropped here, so a week is
  # present in `w` if and only if the person was under observation in it. The
  # `row_presence` sentinel keeps every row, because the caller has already
  # deleted the unobserved ones.
  observed_col <- .tte_observed_column(design$observed_var)
  week_index <- .tte_week_index0(data_enrolled[["isoyearweek"]])
  keep <- !is.na(week_index)
  if (!is.null(observed_col)) {
    keep <- keep & .tte_is_true(data_enrolled[[observed_col]])
  }
  w <- data.table::data.table(
    dv_pid = data_enrolled[[person_id_col]][keep],
    dv_week = week_index[keep],
    dv_tx = data_enrolled[[tx_col]][keep]
  )
  data.table::setkeyv(w, c("dv_pid", "dv_week"))

  # --- the person-trials ---------------------------------------------------
  # The landmark of a person-band is the first week of the band after it.
  lm_week <- (as.integer(entry_dt[["entry_band_id"]]) + 1L) * period_width
  arm <- .tte_is_true(entry_dt[["baseline_tx"]])
  n_pt <- length(lm_week)
  stop_week <- rep(NA_integer_, n_pt)

  # --- internal observation gaps -------------------------------------------
  # `dv_next` is the next OBSERVED week of the same person. A gap opens at
  # `dv_week + 1` when that next week is further away than one week. The last
  # week of a record has no next week, so it opens no gap here.
  w[, dv_next := data.table::shift(dv_week, type = "lead"), by = dv_pid]
  gaps <- w[
    !is.na(dv_next) & dv_next > dv_week + 1L,
    list(dv_pid, dv_week = dv_week + 1L)
  ]
  w[, dv_next := NULL]
  # A duplicated person-week would otherwise duplicate a gap, and a duplicate
  # in the joined table below would return two rows for one person-trial.
  gaps <- unique(gaps, by = c("dv_pid", "dv_week"))
  if (nrow(gaps) > 0L) {
    data.table::setkeyv(gaps, c("dv_pid", "dv_week"))
    gaps[, dv_hit := dv_week]
    q <- data.table::data.table(dv_pid = entry_dt[[".tte_person_id"]])
    q[, q_week := lm_week]
    stop_week <- gaps[
      q,
      on = c("dv_pid", dv_week = "q_week"),
      roll = -Inf,
      dv_hit
    ]
  }

  # --- discordant runs, one arm at a time ----------------------------------
  # A person can be an initiator in one band and a comparator in another, so
  # the runs are read per arm and not per person.
  for (this_arm in c(TRUE, FALSE)) {
    idx <- which(arm == this_arm)
    if (length(idx) == 0L) {
      next
    }
    k <- as.integer(
      if (this_arm) {
        design$intervention_tolerance_weeks
      } else {
        design$comparator_tolerance_weeks
      }
    )

    # Concordance for the intervention arm is `TRUE`, and for the comparator
    # arm it is `FALSE`. Every other value, `NA` included, is discordant.
    is_disc <- if (this_arm) {
      !.tte_is_true(w[["dv_tx"]])
    } else {
      !.tte_is_false(w[["dv_tx"]])
    }
    dw <- unique(w[is_disc, list(dv_pid, dv_week)], by = c("dv_pid", "dv_week"))
    if (nrow(dw) == 0L) {
      next
    }
    data.table::setkeyv(dw, c("dv_pid", "dv_week"))

    # A run starts at a discordant week whose previous week is not the week
    # before it. That covers a concordant week and an absent week alike,
    # because neither reaches `dw`.
    dw[,
      dv_start := {
        prev <- data.table::shift(dv_week)
        is.na(prev) | prev != dv_week - 1L
      },
      by = dv_pid
    ]
    # The first discordant week of every person starts a run, so the running
    # sum never joins two people.
    dw[, dv_run := cumsum(dv_start)]
    dw[, dv_len := seq_len(.N), by = dv_run]

    # A week qualifies when the run ending there is at least `k + 1` weeks
    # long. The boundary is the right edge of the earliest qualifying week
    # that is at or after `L + k`, which is the earliest week whose whole run
    # of `k + 1` sits inside follow-up.
    qk <- dw[dv_len >= k + 1L, list(dv_pid, dv_week)]
    if (nrow(qk) == 0L) {
      next
    }
    data.table::setkeyv(qk, c("dv_pid", "dv_week"))
    qk[, dv_hit := dv_week]
    q <- data.table::data.table(dv_pid = entry_dt[[".tte_person_id"]][idx])
    q[, q_week := lm_week[idx] + k]
    hit <- qk[q, on = c("dv_pid", dv_week = "q_week"), roll = -Inf, dv_hit]
    stop_week[idx] <- pmin(stop_week[idx], hit + 1L, na.rm = TRUE)
  }

  # --- the boundary, counted from the landmark -----------------------------
  weeks <- stop_week - lm_week
  weeks[!is.na(weeks) & weeks > span] <- NA_integer_

  out <- data.table::data.table(weeks_to_protocol_deviation = weeks)
  data.table::set(out, j = id_var, value = entry_dt[[id_var]])
  data.table::setkeyv(out, id_var)
  out[]
}


#' Place the record-end boundary of every enrolled person-trial.
#'
#' The boundary is the week the weekly record stops at, counted from the
#' landmark. It is exact to the week, and it comes from the weekly sequence. It
#' is an exclusive stop. See the interval convention section of [TTEDesign].
#'
#' A record that simply ends carries no internal gap, so
#' `.tte_deviation_boundary()` never reports it. `s5_prepare_outcome()` reports
#' it as `weeks_to_loss`, and reads `.max_tstop` for the value. `.max_tstop` is
#' the stop of the LAST BAND, so a record that ends inside a band overshoots by
#' up to `period_width - 1` weeks. This function reads the exact week instead.
#'
#' A record that reaches the end of the panel returns `NA`. Nothing is left for
#' it to stop, and the person completed the follow-up the panel holds.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id`, `entry_band_id` and `id_var`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col` and `isoyearweek`.
#' @param design A [TTEDesign].
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param id_var Character, the person-trial identifier column.
#' @param n_follow_up_bands Integer, the number of follow-up bands the panel
#'   holds.
#' @return A data.table keyed by `id_var`, with one integer column
#'   `weeks_to_record_end`. `NULL` when the design cannot support the weekly
#'   read.
#' @noRd
.tte_record_end_boundary <- function(
  entry_dt,
  data_enrolled,
  design,
  person_id_col,
  id_var,
  n_follow_up_bands
) {
  re_pid <- re_week <- NULL # nolint
  re_last <- NULL # nolint

  # The same gate as `.tte_deviation_boundary()`. Without an observation
  # encoding swereg cannot say whether the record ended or the person is
  # simply absent from these weeks, and the two boundaries MUST agree on that.
  if (is.null(design$observed_var)) {
    return(NULL)
  }
  if (!"isoyearweek" %in% names(data_enrolled)) {
    return(NULL)
  }
  if (nrow(entry_dt) == 0L) {
    return(NULL)
  }

  period_width <- as.integer(design$period_width)
  span <- as.integer(n_follow_up_bands) * period_width

  observed_col <- .tte_observed_column(design$observed_var)
  week_index <- .tte_week_index0(data_enrolled[["isoyearweek"]])
  keep <- !is.na(week_index)
  if (!is.null(observed_col)) {
    keep <- keep & .tte_is_true(data_enrolled[[observed_col]])
  }
  if (!any(keep)) {
    return(NULL)
  }

  last_week <- data.table::data.table(
    re_pid = data_enrolled[[person_id_col]][keep],
    re_week = week_index[keep]
  )[, list(re_last = max(re_week)), by = re_pid]
  data.table::setkeyv(last_week, "re_pid")

  lm_week <- (as.integer(entry_dt[["entry_band_id"]]) + 1L) * period_width
  hit <- last_week[
    data.table::data.table(re_pid = entry_dt[[".tte_person_id"]]),
    on = "re_pid",
    re_last
  ]

  # A week is a half-open interval, so a record whose last observed week is
  # `u` stops at `u + 1`.
  weeks <- (hit + 1L) - lm_week
  weeks[!is.na(weeks) & weeks >= span] <- NA_integer_

  out <- data.table::data.table(weeks_to_record_end = weeks)
  data.table::set(out, j = id_var, value = entry_dt[[id_var]])
  data.table::setkeyv(out, id_var)
  out[]
}


#' Place the outcome boundary of every enrolled person-trial.
#'
#' The boundary is the week the outcome falls in, counted from the landmark. It
#' is exact to the week, and it comes from the weekly sequence. It is an
#' exclusive stop. See the interval convention section of [TTEDesign].
#'
#' The band collapse keeps one outcome flag per band. After it the week is
#' gone, and the only boundary left to read is the stop of the band. That
#' overshoots by up to `period_width - 1` weeks. It also disagrees with
#' `weeks_to_record_end` and `weeks_to_protocol_deviation`, which are exact.
#' The disagreement changes the winner. A woman whose record ends in week 10,
#' and whose outcome falls in week 10, loses her event to the record end.
#'
#' The active outcome is chosen later, in `s5_prepare_outcome()`, so this
#' returns one column per outcome the design names.
#'
#' An outcome week before the landmark is not a follow-up event and never
#' becomes the boundary. A boundary past the last band of the panel reads `NA`.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id`, `entry_band_id` and `id_var`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col`, `isoyearweek` and the outcome columns.
#' @param design A [TTEDesign].
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param id_var Character, the person-trial identifier column.
#' @param n_follow_up_bands Integer, the number of follow-up bands the panel
#'   holds.
#' @return A data.table keyed by `id_var`, with one integer column
#'   `weeks_to_event_<outcome>` per outcome column. `NULL` when the design
#'   cannot support the weekly read.
#' @noRd
.tte_event_boundary <- function(
  entry_dt,
  data_enrolled,
  design,
  person_id_col,
  id_var,
  n_follow_up_bands
) {
  ev_pid <- ev_week <- ev_hit <- q_week <- NULL # nolint

  # The same gate as `.tte_deviation_boundary()` and
  # `.tte_record_end_boundary()`. Without an observation encoding swereg
  # cannot say whether a week without the outcome was observed at all, and the
  # three boundaries MUST agree on that.
  if (is.null(design$observed_var)) {
    return(NULL)
  }
  if (!"isoyearweek" %in% names(data_enrolled)) {
    return(NULL)
  }
  if (nrow(entry_dt) == 0L) {
    return(NULL)
  }
  outcome_cols <- intersect(design$outcome_vars, names(data_enrolled))
  if (length(outcome_cols) == 0L) {
    return(NULL)
  }

  period_width <- as.integer(design$period_width)
  span <- as.integer(n_follow_up_bands) * period_width

  observed_col <- .tte_observed_column(design$observed_var)
  week_index <- .tte_week_index0(data_enrolled[["isoyearweek"]])
  keep <- !is.na(week_index)
  if (!is.null(observed_col)) {
    keep <- keep & .tte_is_true(data_enrolled[[observed_col]])
  }
  if (!any(keep)) {
    return(NULL)
  }

  pid_kept <- data_enrolled[[person_id_col]][keep]
  week_kept <- week_index[keep]
  lm_week <- (as.integer(entry_dt[["entry_band_id"]]) + 1L) * period_width
  q <- data.table::data.table(
    ev_pid = entry_dt[[".tte_person_id"]],
    q_week = lm_week
  )

  out <- data.table::data.table(seq_len(nrow(entry_dt)))
  data.table::set(out, j = 1L, value = entry_dt[[id_var]])
  data.table::setnames(out, 1L, id_var)

  for (col in outcome_cols) {
    weeks <- rep(NA_integer_, nrow(entry_dt))
    hit_rows <- .tte_is_true(data_enrolled[[col]][keep])
    if (any(hit_rows)) {
      # The first outcome week at or after the landmark. A duplicated
      # person-week would return two rows for one person-trial.
      ew <- unique(data.table::data.table(
        ev_pid = pid_kept[hit_rows],
        ev_week = week_kept[hit_rows]
      ))
      data.table::setkeyv(ew, c("ev_pid", "ev_week"))
      ew[, ev_hit := ev_week]
      hit <- ew[q, on = c("ev_pid", ev_week = "q_week"), roll = -Inf, ev_hit]
      # A week is a half-open interval, so an outcome in week `u` stops at
      # `u + 1`.
      weeks <- (hit + 1L) - lm_week
      weeks[!is.na(weeks) & weeks > span] <- NA_integer_
    }
    data.table::set(
      out,
      j = paste0("weeks_to_event_", col),
      value = as.integer(weeks)
    )
  }
  data.table::setkeyv(out, id_var)
  out[]
}


#' Stop when a time-updated confounder is missing on the IPCW fitting rows.
#'
#' `$s6_ipcw_pp()` fits censoring on the follow-up rows, so it reads the
#' time-updated confounder. An `NA` there makes `stats::predict()` return `NA`,
#' and `cumprod()` carries that `NA` through the rest of the person-trial. The
#' weight then reaches the survey fit as `NA`, far from the cause.
#'
#' swereg MUST NOT substitute the `.tte_entry__` value. That value describes the
#' recruiting week, and reading it during follow-up is the confounding the
#' landmark design removes.
#'
#' @param data The rows the censoring model fits.
#' @param confounder_vars Character vector of confounder names.
#' @param id_var Character, the person-trial identifier column.
#' @return `invisible(NULL)`, or an error.
#' @noRd
.tte_stop_on_missing_ipcw_confounders <- function(
  data,
  confounder_vars,
  id_var
) {
  cols <- intersect(confounder_vars, names(data))
  if (length(cols) == 0L || nrow(data) == 0L) {
    return(invisible(NULL))
  }
  n_missing <- vapply(cols, function(v) sum(is.na(data[[v]])), integer(1))
  if (all(n_missing == 0L)) {
    return(invisible(NULL))
  }

  ids <- data[[id_var]]
  n_trials <- data.table::uniqueN(ids)
  detail <- vapply(
    cols[n_missing > 0L],
    function(v) {
      na_rows <- is.na(data[[v]])
      sprintf(
        "  %s: %d of %d rows, %d of %d person-trials",
        v,
        sum(na_rows),
        nrow(data),
        data.table::uniqueN(ids[na_rows]),
        n_trials
      )
    },
    character(1)
  )
  stop(
    "s6_ipcw_pp() cannot fit the censoring model.\n",
    "A time-updated confounder is missing on the rows it fits:\n",
    paste(detail, collapse = "\n"),
    "\nAn NA there gives an NA weight, and cumprod() carries it through the ",
    "rest of the person-trial.\n",
    "Fill those follow-up values before this step, or drop the affected ",
    "person-trials.\n",
    "swereg MUST NOT substitute the entry-window value. That value describes ",
    "the recruiting week.",
    call. = FALSE
  )
}

#' Name the follow-up-time term of the censoring model.
#'
#' The term reads the interval START. The weight of a row is the probability of
#' remaining uncensored through that start, so the start is the follow-up time
#' the model conditions on.
#'
#' The ladder steps down as the fit sees fewer distinct values. `mgcv::s()`
#' asks for 10 basis functions by default, and it stops when the covariate
#' holds fewer than 10 distinct values. A natural cubic spline of 3 degrees of
#' freedom needs 4. A factor needs 2.
#'
#' @param var Character, the column the term reads.
#' @param n_distinct Integer, the number of distinct values the fit sees.
#' @param use_gam Logical. `TRUE` asks for a penalised spline.
#' @return A character scalar. It is `""` when one distinct value leaves
#'   nothing to fit.
#' @noRd
.tte_ipcw_time_term <- function(var, n_distinct, use_gam) {
  if (use_gam && n_distinct >= 10L) {
    return(paste0("s(", var, ")"))
  }
  if (n_distinct >= 4L) {
    return(paste0("splines::ns(", var, ", df = 3)"))
  }
  if (n_distinct >= 2L) {
    return(paste0("factor(", var, ")"))
  }
  ""
}

#' Read the confounders of a baseline slice at the entry window.
#'
#' The returned table names each confounder exactly as the design does, and
#' holds its entry-window value under that name. Every step that fits or
#' tabulates baseline confounders MUST read the panel through this function.
#'
#' The rename is local to the returned table. The panel keeps the follow-up
#' value under the confounder name, and the entry-window value under the
#' `.tte_entry__` name.
#'
#' @param data A data.table, one row per person-trial.
#' @param confounder_vars Character vector of confounder names.
#' @param keep_cols Character vector of other columns to carry, such as the
#'   identifier, the treatment column and a weight column.
#' @return A new data.table. It shares no column with `data`.
#' @noRd
.tte_entry_view <- function(data, confounder_vars, keep_cols = character(0)) {
  use_entry <- .tte_has_entry_snapshot(data, confounder_vars)
  conf <- intersect(confounder_vars, names(data))
  entry <- .tte_entry_col(conf)
  cols <- unique(c(keep_cols, conf, if (use_entry) entry))
  out <- data.table::copy(data[, intersect(cols, names(data)), with = FALSE])
  if (use_entry) {
    for (i in seq_along(conf)) {
      data.table::set(out, j = conf[i], value = out[[entry[i]]])
    }
  }
  out
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

#' The reporting times one panel row spans
#'
#' A row spans time `t` when `tstart < t <= tstop`. The interval is half open:
#' the row covers the weeks from `tstart` to `tstop - 1`, and the event of the
#' row lands at `tstop`.
#'
#' A survival risk set at `t` therefore holds every row that spans `t`, and not
#' only the rows that stop at `t`. The two sets agreed while every stop sat on
#' the band grid. `s5_prepare_outcome()` clips the terminal row at the exact
#' censoring week, so a stop now falls between two band boundaries and the two
#' sets differ.
#'
#' @param tstart Numeric, the exclusive start of each row.
#' @param tstop Numeric, the inclusive stop of each row.
#' @param times Numeric, the reporting times. Sorted, unique, without `NA`.
#' @return A list of two integer vectors. `lo` is the first position in `times`
#'   that the row spans, and `hi` is the last one. `hi < lo` means the row
#'   spans no reporting time.
#' @noRd
.tte_span_index <- function(tstart, tstop, times) {
  list(
    lo = findInterval(tstart, times) + 1L,
    hi = findInterval(tstop, times)
  )
}

#' The exclusive start of every panel row
#'
#' Reads `tstart_var` where the panel carries it. Every panel that `$enroll()`
#' builds carries it, so that is the production path.
#'
#' A panel built by hand can omit the column, and it then states no interval at
#' all. The row is read as covering the one step that ends at its own stop. The
#' start is then the previous reporting time, and 0 for the first. The
#' estimator read every row that way before this release, so a panel with no
#' start column keeps the numbers it had.
#'
#' @param data A data.table at trial level, one row per person-trial-band.
#' @param tstart_var Character, the period start column.
#' @param tstop_var Character, the period stop column.
#' @param times Numeric, the reporting times. Sorted, unique, and holding every
#'   value of `tstop_var`.
#' @return A numeric vector, one element per row of `data`.
#' @noRd
.tte_interval_start <- function(data, tstart_var, tstop_var, times) {
  if (tstart_var %in% names(data)) {
    return(as.numeric(data[[tstart_var]]))
  }
  as.numeric(c(0, times)[match(data[[tstop_var]], times)])
}

#' Weighted risk sets, weighted events and head counts at every reporting time
#'
#' The ONE site that decides which rows enter a survival risk set.
#'
#' `Y_a(t) = sum_i w_i * I(A_i = a, tstart_i < t <= tstop_i)` is the weighted
#' risk set. It is a weighted COUNT of the person-trials at risk at `t`, and it
#' is never a sum of person-time. `$rates()` owns the person-time quantity and
#' forms it as `sum(person_weeks * w)`.
#'
#' `d_a(t) = sum_i w_i * I(A_i = a, event_i = 1, tstop_i = t)` is the weighted
#' event count. Note the asymmetry against `Y_a(t)` and keep it: the risk set
#' SPANS the time, and the event LANDS at the stop of its own row.
#'
#' `N_a(t)` counts the distinct people who span `t`. A person holds several
#' sequential trials, so her rows are merged into runs first and she is then
#' counted once.
#'
#' Every arm gets a row at every reporting time, including a time where it
#' holds no row of its own. That is what lets a survival curve carry its latest
#' exact value forward. It also lets both arms of a risk difference be read at
#' one time.
#'
#' @param arm A vector of arm labels, one element per panel row.
#' @param person A vector of person labels, one element per panel row.
#' @param weight Numeric, the analysis weight of each panel row.
#' @param event Numeric or integer, the 0/1 outcome indicator of each row.
#' @param tstart Numeric, the exclusive start of each row.
#' @param tstop Numeric, the inclusive stop of each row.
#' @param times Numeric, the reporting times. Sorted, unique, and holding every
#'   value of `tstop`.
#' @return A data.table with one row per arm and reporting time, sorted by arm
#'   and then by time. Columns `arm`, `time`, `events`, `at_risk` and
#'   `n_persons_at_risk`.
#' @noRd
.tte_span_risk_sets <- function(
  arm,
  person,
  weight,
  event,
  tstart,
  tstop,
  times
) {
  . <- arm_i <- t_i <- lo <- hi <- w <- ev <- dw <- dn <- run <- NULL # nolint
  events <- at_risk <- n_persons_at_risk <- person_i <- t_event <- NULL # nolint
  i.events <- i.dw <- i.dn <- NULL # nolint

  arms <- sort(unique(arm), na.last = TRUE)
  n_arm <- length(arms)
  n_time <- length(times)
  span <- .tte_span_index(tstart, tstop, times)

  d <- data.table::data.table(
    arm_i = match(arm, arms),
    person_i = person,
    w = as.numeric(weight),
    ev = as.numeric(event),
    t_event = match(tstop, times),
    lo = span$lo,
    hi = span$hi
  )
  if (anyNA(d$t_event)) {
    stop("every 'tstop' must be one of the reporting times")
  }

  out <- data.table::CJ(arm_i = seq_len(n_arm), t_i = seq_len(n_time))
  out[, `:=`(events = 0, dw = 0, dn = 0L)]

  # The event lands at the stop of its own row.
  e <- d[ev > 0, .(events = sum(w * ev)), keyby = .(arm_i, t_i = t_event)]
  out[e, events := i.events, on = c("arm_i", "t_i")]

  # The risk set spans. One `+w` where the row enters and one `-w` after it
  # leaves; the running sum is then the risk set at every reporting time. The
  # panel is millions of rows, so this stays linear in the rows.
  s <- d[hi >= lo]
  edges <- data.table::rbindlist(list(
    s[, .(arm_i, t_i = lo, dw = w)],
    s[, .(arm_i, t_i = hi + 1L, dw = -w)]
  ))[t_i <= n_time, .(dw = sum(dw)), keyby = .(arm_i, t_i)]
  out[edges, dw := i.dw, on = c("arm_i", "t_i")]
  out[, at_risk := cumsum(dw), by = "arm_i"]

  # The head count spans over the UNION of a person's rows. Merging her rows
  # into runs first is what stops two overlapping trials counting her twice.
  # The guard skips the grouping on an empty table: data.table evaluates
  # `min()` once on the empty group to type the result, and that warns.
  if (nrow(s)) {
    data.table::setorder(s, arm_i, person_i, lo, hi)
    s[,
      run := cumsum(lo > data.table::shift(cummax(hi), fill = 0L)),
      by = c("arm_i", "person_i")
    ]
    runs <- s[,
      .(lo = min(lo), hi = max(hi)),
      by = c("arm_i", "person_i", "run")
    ]
    head_edges <- data.table::rbindlist(list(
      runs[, .(arm_i, t_i = lo, dn = 1L)],
      runs[, .(arm_i, t_i = hi + 1L, dn = -1L)]
    ))[t_i <= n_time, .(dn = sum(dn)), keyby = .(arm_i, t_i)]
    out[head_edges, dn := i.dn, on = c("arm_i", "t_i")]
  }
  out[, n_persons_at_risk := cumsum(dn), by = "arm_i"]

  # A running sum over weights leaves a residue of about 1e-16 where the risk
  # set is empty. The head count is an integer and is exact, so it decides.
  out[n_persons_at_risk == 0L, at_risk := 0]

  out[, `:=`(arm = arms[arm_i], time = times[t_i])]
  out[, c("arm_i", "t_i", "dw", "dn") := NULL]
  data.table::setcolorder(
    out,
    c("arm", "time", "events", "at_risk", "n_persons_at_risk")
  )
  out[]
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
    list(num = mn, den = md)
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
    spans[arm == which_arm]$n_persons_at_risk
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
.tte_nntb_cell <- function(
  nntb,
  nntb_lo = NULL,
  nntb_hi = NULL,
  nnt_direction
) {
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
#' @param treatment_var Character, name of the baseline treatment column. It
#'   holds `TRUE` for the intervention arm, `FALSE` for the comparator arm, and
#'   `NA` outside the two arms. Enrollment reads every eligible week of the
#'   entry band, not only its first week. See the Baseline treatment section
#'   of TTEEnrollment for the full rule.
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
#' @param observed_var The observation encoding, or NULL (default: NULL). It
#'   states how the data records that a person was under observation in a
#'   week. Give a list with exactly one of two keys.
#'   `list(column = "rd_observed")` names a real logical person-week column.
#'   `list(sentinel = "row_presence")` asserts that the caller already deleted
#'   every unobserved person-week. A row then exists if and only if the person
#'   was observed that week. Person-week data MUST declare one of the two
#'   forms. Already-expanded trial data MAY leave this NULL. One row there is
#'   one trial, and not one week of observation.
#' @param intervention_tolerance_weeks Integer, the tolerance in weeks for the
#'   intervention arm (default: 0L). It MUST be a whole number of at least 0.
#' @param comparator_tolerance_weeks Integer, the tolerance in weeks for the
#'   comparator arm (default: 0L). It MUST be a whole number of at least 0.
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
#'   time aggregation (default: 4L). The input is a person-week skeleton, so
#'   eligibility and treatment status are assessed weekly. `period_width` then
#'   collapses consecutive weeks into bands, and each band opens exactly one
#'   trial. With `period_width = 4L`, one trial opens every four weeks, not one
#'   trial per week. Initiation in any week of a band is attributed to the
#'   start of that band. Must be a positive integer.
#'
#' @section The interval convention:
#'
#' Every interval is `[tstart, tstop)`. The stop is exclusive. The person
#' leaves the risk set at `tstop`, and the row holds no part of that week.
#'
#' Every duration is `tstop - tstart`. It never adds one. Three complete
#' four-week bands span `[0, 12)`. That is 12 person-weeks, and the bands bill
#' 4, 4 and 4. The inclusive convention bills 5, 5 and 5.
#'
#' Every `weeks_to_*` column is a boundary on the same scale, counted from the
#' landmark at week 0. `weeks_to_event`, `weeks_to_protocol_deviation`,
#' `weeks_to_loss`, `weeks_to_admin_end` and `weeks_to_record_end` each name
#' the first week the person no longer contributes. A `weeks_to_record_end` of
#' 9 means the person held follow-up weeks 1 to 9 and bills 9 person-weeks.
#'
#' The `+ 1` belongs to the inclusive convention, where weeks 1 through 4 is
#' `4 - 1 + 1 = 4`. Both are correct arithmetic. The two differ in whether the
#' stop belongs to the interval. A mix of them makes a silently wrong
#' denominator, so swereg MUST read every stop as exclusive.
#'
#' One place adds a week, and it converts a calendar reading into a stop.
#' `admin_censor_isoyearweek` names the last week under study, and
#' `difftime()` returns the whole weeks between that week and the landmark
#' week. The stop is one week later, because the person holds the whole of the
#' administrative week.
#'
#' `tests/testthat/test-interval-convention.R` pins each of the five
#' boundaries.
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
#'   eligible_var = "eligible",
#'   observed_var = list(column = "rd_observed")
#' )
#'
#' # The same design on a trimmed skeleton. A row exists if and only if the
#' # person was observed that week, so there is no column to name.
#' design_trimmed <- TTEDesign$new(
#'   person_id_var = "id",
#'   treatment_var = "baseline_intervention",
#'   outcome_vars = c("death", "hosp"),
#'   confounder_vars = c("age", "education"),
#'   follow_up_time = 156L,
#'   eligible_var = "eligible",
#'   observed_var = list(sentinel = "row_presence")
#' )
#'
#' @family tte_classes
#' @seealso [TTEEnrollment] for the trial class.
#'   `vignette("tte-nomenclature")` for the enrollment band vocabulary.
#' @importFrom R6 R6Class
#' @export
TTEDesign <- R6::R6Class(
  "TTEDesign",
  public = list(
    #' @field person_id_var Character or NULL, person identifier column name.
    person_id_var = NULL,
    #' @field id_var Character, person-trial identifier column name.
    id_var = "enrollment_person_trial_id",
    #' @field treatment_var Character, treatment column name. Enrollment reads
    #'   every eligible week of the entry band, not only its first week. See
    #'   the Baseline treatment section of TTEEnrollment for the full rule.
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
    #' @field observed_var The observation encoding, or NULL. It is a
    #'   `tte_observed_var` list with a `column` and a `sentinel`, exactly one
    #'   of which is set. `column` names a real logical person-week column.
    #'   `sentinel` of `"row_presence"` asserts a trimmed skeleton, where a
    #'   row exists if and only if the person was observed that week.
    observed_var = NULL,
    #' @field intervention_tolerance_weeks Integer, the tolerance in weeks for
    #'   the intervention arm.
    intervention_tolerance_weeks = 0L,
    #' @field comparator_tolerance_weeks Integer, the tolerance in weeks for
    #'   the comparator arm.
    comparator_tolerance_weeks = 0L,
    #' @field admin_censor_var Character or NULL, admin censoring column.
    admin_censor_var = NULL,
    #' @field admin_censor_isoyearweek Character or NULL, admin censoring date.
    admin_censor_isoyearweek = NULL,
    #' @field period_width Integer, band width in weeks for enrollment and
    #'   aggregation. Eligibility and treatment status are assessed weekly.
    #'   `period_width` collapses consecutive weeks into bands, and each band
    #'   opens exactly one trial. Initiation in any week of a band is
    #'   attributed to the start of that band.
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
      observed_var = NULL,
      intervention_tolerance_weeks = 0L,
      comparator_tolerance_weeks = 0L,
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
      observed_var <- .tte_observed_var(observed_var, "observed_var")
      intervention_tolerance_weeks <- .tte_tolerance_weeks(
        intervention_tolerance_weeks,
        "intervention_tolerance_weeks"
      )
      comparator_tolerance_weeks <- .tte_tolerance_weeks(
        comparator_tolerance_weeks,
        "comparator_tolerance_weeks"
      )
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

      .tte_check_entry_names(confounder_vars)

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
      self$observed_var <- observed_var
      self$intervention_tolerance_weeks <- intervention_tolerance_weeks
      self$comparator_tolerance_weeks <- comparator_tolerance_weeks
      self$admin_censor_var <- admin_censor_var
      self$admin_censor_isoyearweek <- admin_censor_isoyearweek
      self$period_width <- as.integer(period_width)

      private$.schema_version <- .TTE_DESIGN_SCHEMA_VERSION
    },

    #' @description Check this object's schema version against the current
    #' class version. It stops when the object carries an older schema.
    #' @details swereg 26.9.0 moved time zero to the landmark. A `tstart == 0`
    #' row of a schema-2 object is an entry band row, and a 26.9.0 reader
    #' takes it for a landmark row. The check refuses the object, so that
    #' reinterpretation cannot happen in silence.
    #' @return `invisible(TRUE)` when the versions match. It stops otherwise.
    check_version = function() {
      current <- .TTE_DESIGN_SCHEMA_VERSION
      saved <- private$.schema_version %||% 0L
      if (saved < current) {
        stop(
          class(self)[1],
          " on disk has schema version ",
          saved,
          " but this swereg requires version ",
          current,
          ".\n",
          "Time zero moved to the landmark in swereg 26.9.0, so a `tstart == 0` ",
          "row of an older object does not mean what it used to. Re-create this ",
          "object by re-running the project's s0_init.R.",
          call. = FALSE
        )
      }
      invisible(TRUE)
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
      if (!is.null(self$observed_var)) {
        col <- .tte_observed_column(self$observed_var)
        if (is.null(col)) {
          cat("  Observation: sentinel", self$observed_var$sentinel, "\n")
        } else {
          cat("  Observation: column", col, "\n")
        }
      }
      cat(
        "  Arm tolerance:",
        self$intervention_tolerance_weeks,
        "weeks intervention /",
        self$comparator_tolerance_weeks,
        "weeks comparator\n"
      )
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
#'   person_week data. The Baseline treatment section states the rule that
#'   decides the arm of each person-band.
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
#' Enrollment (the comparator draw + panel expansion) transitions data from "person_week"
#' to "trial" level and is triggered by passing `ratio` to the constructor.
#'
#' @section Baseline treatment:
#' The input is a person-week skeleton, so eligibility and treatment status are
#' assessed weekly. `period_width` collapses consecutive weeks into bands, and
#' each band opens one trial.
#'
#' swereg reads only the weeks of a band that are eligible and hold `TRUE` or
#' `FALSE` in the treatment column. It drops every other week of the band
#' first, and then applies three rules.
#' \itemize{
#'   \item A person is an initiator when at least one week it reads holds
#'     `TRUE`.
#'   \item A person is a comparator when every week it reads holds `FALSE`.
#'   \item A person-band with no such week is ineligible, and enters neither
#'     arm.
#' }
#'
#' The drop comes first, so an `NA` week does not stop a comparator
#' classification. A band of `FALSE`, `NA`, `FALSE`, `FALSE` is a comparator
#' band.
#'
#' Time zero is the landmark, which is the first week of the band AFTER the
#' entry band. The panel therefore starts one band after the entry band, and
#' the entry band carries no follow-up. `entry_band_id` names the trial and
#' `trial_id` names the follow-up band.
#'
#' Each confounder reaches the panel twice. The `.tte_entry__<v>` column holds
#' its value at the recruiting week, and `<v>` holds the time-updated value of
#' the follow-up band. `$s2_ipw()` and `$table1()` read the entry column. See
#' `vignette("tte-methods")` for the full rule and
#' `vignette("tte-nomenclature")` for the trade-off between bias and statistical
#' power.
#'
#' @inheritSection TTEDesign The interval convention
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
#' @seealso [TTEDesign] for design class.
#'   `vignette("tte-nomenclature")` for the enrollment band vocabulary.
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
    #' @field landmark_attrition A data.table or NULL. It reports why landmark
    #'   qualification dropped each candidate person-band, by criterion and by
    #'   arm. Its columns are `trial_id`, `criterion`, `n_persons`,
    #'   `n_person_trials`, `n_intervention` and `n_comparator`. The row with
    #'   `trial_id = NA` covers the whole cohort. The three criteria are
    #'   `landmark_candidates`, `landmark_observed` and `landmark_event_free`,
    #'   and each count is cumulative. It stays `NULL` when the design declares
    #'   no `observed_var`, and when the caller supplies `enrolled_ids` from
    #'   the two-pass pipeline.
    landmark_attrition = NULL,

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
    #'   person_week data. The Baseline treatment section of TTEEnrollment
    #'   states the rule that decides the arm of each person-band.
    #' @param seed Integer or NULL. Random seed for enrollment reproducibility.
    #' @param extra_cols Character vector or NULL. Extra columns to include in
    #'   trial panels during enrollment.
    #' @param enrolled_ids data.table or NULL. Pre-drawn enrollment IDs from
    #'   the two-pass pipeline. When provided, enrollment skips the comparator
    #'   draw and uses these IDs directly.
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

    #' @description Check this object's schema version against the current
    #' class version. It stops when the object carries an older schema.
    #' @details swereg 26.9.0 moved time zero to the landmark. A `tstart == 0`
    #' row of a schema-2 panel is an entry band row, and a 26.9.0 reader takes
    #' it for a landmark row. The check refuses the object, so that
    #' reinterpretation cannot happen in silence.
    #' @return `invisible(TRUE)` when the versions match. It stops otherwise.
    check_version = function() {
      current <- .TTE_ENROLLMENT_SCHEMA_VERSION
      saved <- private$.schema_version %||% 0L
      if (saved < current) {
        stop(
          class(self)[1],
          " on disk has schema version ",
          saved,
          " but this swereg requires version ",
          current,
          ".\n",
          "Time zero moved to the landmark in swereg 26.9.0, so a `tstart == 0` ",
          "row of an older object does not mean what it used to. Re-create this ",
          "object by re-running the project's s0_init.R.",
          call. = FALSE
        )
      }
      invisible(TRUE)
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

      # Fit on the ENTRY-WINDOW snapshot. `tstart == 0` is the LANDMARK band
      # row, so the confounder columns there hold follow-up values and not
      # baseline ones. `fit_dt` is local, and the rename inside it never
      # reaches the panel.
      use_entry <- .tte_has_entry_snapshot(baseline, confounder_vars)
      entry_cols <- .tte_entry_col(confounder_vars)
      fit_cols <- unique(c(
        id_var,
        treatment_var,
        confounder_vars,
        if (use_entry) entry_cols
      ))
      fit_dt <- data.table::copy(
        baseline[, intersect(fit_cols, names(baseline)), with = FALSE]
      )
      if (use_entry) {
        for (i in seq_along(confounder_vars)) {
          data.table::set(
            fit_dt,
            j = confounder_vars[i],
            value = fit_dt[[entry_cols[i]]]
          )
        }
      }

      ps_formula <- stats::as.formula(
        paste(treatment_var, "~", paste(confounder_vars, collapse = " + "))
      )
      ps_model <- stats::glm(
        ps_formula,
        data = fit_dt,
        family = stats::binomial
      )
      fit_dt[, ps := stats::predict(ps_model, fit_dt, type = "response")]

      if (stabilize) {
        p_intervention <- mean(fit_dt[[treatment_var]], na.rm = TRUE)
        fit_dt[,
          ipw := data.table::fifelse(
            get(treatment_var) == TRUE,
            p_intervention / ps,
            (1 - p_intervention) / (1 - ps)
          )
        ]
      } else {
        fit_dt[,
          ipw := data.table::fifelse(
            get(treatment_var) == TRUE,
            1 / ps,
            1 / (1 - ps)
          )
        ]
      }

      data.table::setkeyv(fit_dt, id_var)
      self$data[fit_dt, `:=`(ps = i.ps, ipw = i.ipw), on = id_var]

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
    #' `$s6_ipcw_pp()`. For `estimand = "itt"` it calls `$s5_prepare_outcome()`
    #' in ITT mode, which never censors at treatment switching. ITT skips IPCW,
    #' because baseline IPW alone is the valid ITT weight. This is the
    #' recommended way to prepare an enrollment for analysis.
    #'
    #' The censoring row stays in `self$data`, and it carries only the exposure
    #' before its boundary. `s5_prepare_outcome()` clips that row at the exact
    #' censoring week, and sets `person_weeks` to the clipped width. The
    #' deviated regime therefore contributes no person-time and no outcome, so
    #' the row cannot attribute a post-deviation outcome to the baseline
    #' treatment. Releases before 26.9.0 deleted the row instead, which threw
    #' away every valid week it held.
    #'
    #' Event-priority convention: an outcome event that stops in the deviation
    #' band wins. The row then counts as an event and not as a censoring. The
    #' deviation does not clip it, `censor_this_period` is 0, and the censoring
    #' model does not treat it as censored (since 26.7.3). The row still stops
    #' at the exact event week, which can fall inside the band.
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
      # The censoring row is retained. `s5_prepare_outcome()` has already
      # clipped it at the exact boundary, so it carries the pre-censor
      # exposure and nothing after. Deleting it would throw away that exposure
      # and shrink every offset denominator.
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

      # Table 1 describes the cohort at time zero, so it reads the same
      # entry-window snapshot that `$s2_ipw()` fits on.
      .swereg_table1(
        data = .tte_entry_view(
          baseline,
          design$confounder_vars,
          keep_cols = c(design$treatment_var, ipw_col)
        ),
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
    #' panel. Per treatment arm and reporting time, forms the weighted hazard
    #' `h(t) = d(t) / Y(t)`, then `S(t) = prod(1 - h(t))`. The risk set `Y(t)`
    #' is `sum(w)` over every row that SPANS `t`, which is
    #' `tstart < t <= tstop`. The event count `d(t)` is `sum(w * event)` over
    #' the rows that stop at `t`. The weight column `weight_col` may vary over
    #' time. Because it works on the
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
    #' observed `tstop`, one row per arm and time. Where an arm holds nobody at
    #' risk, the hazard is `NA` and the survival carries its latest exact value
    #' forward.
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

      # Weighted discrete-time hazard per arm and reporting time. The weight is
      # applied to each at-risk row exactly as in $rates()/$irr(), so the curve
      # and the reported IRR share one weighting convention.
      #
      # `.tte_span_risk_sets()` owns the two rules: the risk set holds every
      # row that SPANS the time, and the event LANDS at the stop of its own
      # row. The risk set stays a weighted COUNT of the person-trials at risk.
      # It is not a sum of person-time; `$rates()` owns that quantity.
      #
      # `n_persons_at_risk` is a plain head count of distinct people, for the
      # risk table a reader expects under a survival panel. It is deliberately
      # NOT `.N`: the panel is one row per person-trial-band, and a person
      # contributes several sequential trials, so `.N` counts person-trials.
      # It is also not `at_risk`, which is the weighted risk set.
      times <- sort(unique(data[[time_var]]))
      curve <- .tte_span_risk_sets(
        arm = data[[tvar]],
        person = data[[design$person_id_var]],
        weight = data[[weight_col]],
        event = data[["event"]],
        tstart = .tte_interval_start(
          data,
          design$tstart_var,
          time_var,
          times
        ),
        tstop = data[[time_var]],
        times = times
      )
      data.table::setnames(curve, c("arm", "time"), c(tvar, time_var))
      data.table::setkeyv(curve, c(tvar, time_var))
      # An empty risk set is legitimate once an arm runs out of follow-up. A
      # positive head count with no weight behind it is not, and neither is an
      # event at a time no row covers.
      if (any(curve$at_risk <= 0 & curve$n_persons_at_risk > 0L)) {
        stop("weighted risk set (sum of weights) is <= 0 in an arm-period")
      }
      if (any(curve$at_risk <= 0 & curve$events > 0)) {
        stop("an event falls at a time whose risk set is empty")
      }
      curve[, hazard := events / at_risk]
      # Nobody at risk: the hazard is undefined and reads NA, and the survival
      # carries its latest exact value forward. `cumprod` is valid over these
      # exact event boundaries, and a band hazard over unequal intervals is
      # not, so a time between two boundaries multiplies by exactly 1.
      curve[at_risk <= 0, hazard := NA_real_]
      curve[,
        surv := cumprod(1 - data.table::fifelse(is.na(hazard), 0, hazard)),
        by = c(tvar)
      ]

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
              if (
                exists(".Random.seed", envir = globalenv(), inherits = FALSE)
              ) {
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
        conf_level = conf_level,
        tstart_var = design$tstart_var
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

    # --- enroll: band-based comparator draw + collapse + panel expansion ----
    # Phase order: A (assign bands) -> C (draw on band summary) ->
    #   B (collapse enrolled persons) -> D (expand panels at band level)
    # When enrolled_ids is provided (pre-drawn mode from two-pass pipeline),
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

      # Pre-drawn mode leaves this NULL. The two-pass pipeline qualifies in
      # the s1a scout, so the ids it hands down are already qualified and this
      # object has no cascade of its own to report.
      landmark_attrition <- NULL

      if (!is.null(enrolled_ids)) {
        # ---- Pre-drawn mode: build entry_dt from enrolled_ids ----
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
        # ---- Phase C: Per-band stratified comparator draw ----
        # C-prep: one row per (person, band), from the single source of
        # truth. `.band_baseline_treatment()` drops the weeks that are not
        # eligible or not in an arm, then reads every week that is left. It
        # returns no row for a band with no eligible in-arm week, so such a
        # band reaches neither `intervention_bands` nor `comparator_bands`
        # below. It needs no week ordering, because any() is
        # order-independent.
        band_summary <- .band_baseline_treatment(
          data = data,
          person_id_col = person_id_col,
          treatment_col = treatment_col,
          eligible_col = eligible_col,
          out_col = "band_treatment"
        )

        # C-order: this sort serves the seeded comparator draw, and NOT
        # first(). `sample()` at the C-draw step below draws row indices
        # inside each `.SD` group, so the draw follows the row order of
        # `band_summary`. Sorting the helper's OWN output makes the draw
        # independent of the row order of `data`. A maintainer MUST NOT
        # delete this sort as dead code. Without it, one seed gives two
        # different comparator sets from the same rows in a different order.
        # The sort cannot reach the scout path, which never builds
        # `band_summary`.
        data.table::setorderv(band_summary, c(person_id_col, "trial_id"))

        # C-qualify: drop every person-band that does not reach its landmark
        # under observation and event-free. This runs BETWEEN the arm
        # classification and the comparator draw, and the position is part of
        # the rule. After the classification, so the attrition table can
        # report both arms. Before the draw, so `sample()` below refills the
        # ratio from qualified comparators and an unqualified one cannot
        # shrink the enrolled set. Filtering preserves row order, so the sort
        # above still governs the seeded draw.
        qualified <- .tte_qualify_bands(
          bands = band_summary,
          data = data,
          design = design,
          person_id_col = person_id_col,
          arm_col = "band_treatment"
        )
        band_summary <- qualified$bands
        landmark_attrition <- qualified$attrition

        # C-draw: Within each band, draw comparators at ratio:1
        intervention_bands <- band_summary[band_treatment == TRUE]
        comparator_bands <- band_summary[band_treatment == FALSE]

        if (nrow(intervention_bands) == 0) {
          stop("No intervention person-bands found among eligible rows.")
        }

        # Per-band stratified comparator draw
        intervention_count <- intervention_bands[, .N, by = trial_id]
        data.table::setnames(intervention_count, "N", "n_intervention")

        # Sample comparator within each band independently
        drawn_comparator <- comparator_bands[
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
        drawn_comparator[, n_intervention := NULL]

        # Combine: entry_dt with (person_id, trial_id, baseline_intervention).
        # `recruit_week_index` travels with each row. It names the week that
        # recruited that person into that band, and a later step reads her
        # covariates there. The pre-drawn branch above gets the same column
        # from `enrolled_ids`, which the s1a scout wrote.
        intervention_bands[, baseline_tx := TRUE]
        drawn_comparator[, baseline_tx := FALSE]
        entry_cols <- c(
          person_id_col,
          "trial_id",
          "baseline_tx",
          "recruit_week_index"
        )
        entry_dt <- data.table::rbindlist(list(
          intervention_bands[, entry_cols, with = FALSE],
          drawn_comparator[, entry_cols, with = FALSE]
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

      # ---- Entry-window snapshot ----
      # Read every confounder at the recruiting week, BEFORE the expansion
      # drops the entry band. The follow-up rows below carry the time-updated
      # value of the same confounder, so the two live in separate columns.
      entry_snapshot <- .tte_entry_snapshot(
        entry_dt = entry_dt,
        data_enrolled = data_enrolled,
        person_id_col = person_id_col,
        confounder_vars = design$confounder_vars,
        id_var = id_var
      )

      n_follow_up_bands <- ceiling(follow_up / period_width)

      # ---- Deviation boundary ----
      # Read the weekly assessments BEFORE the expansion, which is the last
      # step that can. The collapse above keeps one value per band, so the
      # sequence a tolerance run needs no longer exists after it.
      deviation_dt <- .tte_deviation_boundary(
        entry_dt = entry_dt,
        data_enrolled = data_enrolled,
        design = design,
        person_id_col = person_id_col,
        id_var = id_var,
        n_follow_up_bands = n_follow_up_bands
      )

      # ---- Record-end boundary ----
      # Read here for the same reason: a record that ends inside a band is
      # invisible once the band collapses to one row.
      record_end_dt <- .tte_record_end_boundary(
        entry_dt = entry_dt,
        data_enrolled = data_enrolled,
        design = design,
        person_id_col = person_id_col,
        id_var = id_var,
        n_follow_up_bands = n_follow_up_bands
      )

      # ---- Event boundary ----
      # Read here for the same reason again. The collapse keeps one outcome
      # flag per band, and the week the outcome fell in is gone after it. One
      # column per outcome, because `s5_prepare_outcome()` picks the active
      # one later.
      event_dt <- .tte_event_boundary(
        entry_dt = entry_dt,
        data_enrolled = data_enrolled,
        design = design,
        person_id_col = person_id_col,
        id_var = id_var,
        n_follow_up_bands = n_follow_up_bands
      )

      # ---- Phase D: Panel expansion at band level ----
      data.table::setnames(band_data, person_id_col, ".tte_person_id")

      # CJ-style expansion: for each entry, create one row per follow-up band
      # then join against band_data
      # Remove trial_id from entry_dt before expansion (it's in entry_band_id)
      if ("trial_id" %in% names(entry_dt)) {
        entry_dt[, trial_id := NULL]
      }

      # Follow-up opens at the LANDMARK, which is the first week of the band
      # AFTER the entry band. `.tte_qualify_bands()` has already dropped every
      # person-band that is not observed and event-free there, so follow-up
      # starts at the instant qualification is established. Expanding from
      # `entry_band_id` instead would give back within-band immortal time of up
      # to `period_width - 1` weeks.
      expanded <- entry_dt[,
        .(
          trial_id = seq(entry_band_id + 1L, entry_band_id + n_follow_up_bands)
        ),
        by = c(id_var, ".tte_person_id", "baseline_tx", "entry_band_id")
      ]

      # Keyed binary join replaces hash-based merge for Phase D
      data.table::setkey(expanded, .tte_person_id, trial_id)
      data.table::setkey(band_data, .tte_person_id, trial_id)
      panel <- band_data[expanded, nomatch = NULL]

      # `entry_band_id` names the trial and `trial_id` names the follow-up
      # band. The two now differ on every row, so the panel keeps both.
      if (!is.null(entry_snapshot)) {
        ecols <- setdiff(names(entry_snapshot), id_var)
        for (col in ecols) {
          # Type the column first, so an empty panel still carries it and
          # `tteenrollment_rbind()` sees one column set across the chunks.
          data.table::set(
            panel,
            j = col,
            value = entry_snapshot[[col]][NA_integer_]
          )
        }
        if (nrow(panel) > 0L) {
          panel[
            entry_snapshot,
            (ecols) := mget(paste0("i.", ecols)),
            on = id_var
          ]
        }
      }

      # Clean up join columns
      cols_to_remove <- intersect(
        "band_treatment",
        names(panel)
      )
      if (length(cols_to_remove) > 0) {
        panel[, (cols_to_remove) := NULL]
      }

      data.table::setnames(panel, ".tte_person_id", person_id_col)

      # Override treatment with the comparator-draw decision
      panel[, (treatment_col) := baseline_tx]
      panel[, baseline_tx := NULL]

      # trial_week: 0-indexed band offset from enrollment band
      panel[, trial_week := (seq_len(.N) - 1L) * period_width, by = c(id_var)]

      # tstart/tstop in week units
      panel[, tstart := trial_week]
      panel[, tstop := trial_week + period_width]
      # Person-time is the width of the row, and never the number of source
      # weeks the band collapsed. `s5_prepare_outcome()` clips the terminal
      # row at the boundary and recomputes this column from the clipped
      # width, so the two agree on every retained row.
      panel[, person_weeks := tstop - tstart]
      panel[, .n_source_weeks := NULL]

      # The boundary travels as one integer per person-trial.
      # `s5_prepare_outcome()` reads it instead of the collapsed treatment
      # value. Type the column first, so an empty panel still carries it and
      # `tteenrollment_rbind()` sees one column set across the chunks.
      if (!is.null(deviation_dt)) {
        data.table::set(
          panel,
          j = "weeks_to_protocol_deviation",
          value = NA_integer_
        )
        if (nrow(panel) > 0L) {
          panel[
            deviation_dt,
            weeks_to_protocol_deviation := i.weeks_to_protocol_deviation,
            on = id_var
          ]
        }
      }

      if (!is.null(record_end_dt)) {
        data.table::set(panel, j = "weeks_to_record_end", value = NA_integer_)
        if (nrow(panel) > 0L) {
          panel[
            record_end_dt,
            weeks_to_record_end := i.weeks_to_record_end,
            on = id_var
          ]
        }
      }

      if (!is.null(event_dt)) {
        ev_cols <- setdiff(names(event_dt), id_var)
        for (ev_col in ev_cols) {
          data.table::set(panel, j = ev_col, value = NA_integer_)
        }
        if (nrow(panel) > 0L) {
          panel[
            event_dt,
            (ev_cols) := mget(paste0("i.", ev_cols)),
            on = id_var
          ]
        }
      }

      self$data <- panel
      self$data_level <- "trial"
      self$landmark_attrition <- landmark_attrition
      self$steps_completed <- c(self$steps_completed, "enroll")
      invisible(self)
    },

    # --- s5_prepare_outcome: define event, censoring, and follow-up boundaries --
    #
    # `weeks_to_protocol_deviation` is the exact week follow-up stops at, and
    # `enroll()` writes it from the weekly assessments. This method keeps that
    # value, and computes the boundary itself only for a panel that arrives
    # without it. See `.tte_deviation_boundary()` for the rule.
    #
    # The fallback reads the band-collapsed `time_treatment_var`:
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
      # `enroll()` writes the exact week of the outcome into
      # `weeks_to_event_<outcome>`, one column per outcome. Read it where it
      # exists. It is an exclusive stop on the same scale as
      # `weeks_to_protocol_deviation` and `weeks_to_record_end`, so the three
      # boundaries can be compared and the earliest one wins.
      #
      # A panel built outside `enroll()` carries no weekly boundary, so read
      # the band-collapsed outcome instead. That read places the event at the
      # stop of its band, which is what every release before this one did.
      exact_event_col <- paste0("weeks_to_event_", outcome)
      if (exact_event_col %in% names(data)) {
        data[, weeks_to_event := as.integer(get(exact_event_col))]
      } else {
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
      }

      # weeks_to_protocol_deviation
      # ITT keeps follow-up through treatment switching, so deviation never
      # censors and no switch variable is needed -- set it to NA so it drops
      # out of every pmin below and out of the censor_this_period indicator.
      # PP requires time_treatment_var and censors at the deviation.
      if (estimand == "itt") {
        data[, weeks_to_protocol_deviation := NA_integer_]
      } else {
        if (is.null(design$time_treatment_var)) {
          stop(
            "design must have time_treatment_var for per-protocol censoring analysis"
          )
        }
        if (!"weeks_to_protocol_deviation" %in% names(data)) {
          # A panel built outside `enroll()` carries no weekly boundary, so
          # read the band-collapsed value instead. That read cannot see a
          # switch inside a band, and it is why `.tte_deviation_boundary()`
          # exists. It stays here for a caller who hands in trial data
          # directly.
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
      }

      # The band that carries the censoring.
      # `weeks_to_protocol_deviation` is exact to the week, so it can fall
      # INSIDE a band. The band that censors is then the first one that
      # reaches it, and every earlier band is complete follow-up. A boundary
      # that already sits on a band edge picks that band, so the fallback
      # above keeps the behaviour of every earlier release.
      # A boundary past the last band of the panel reads NA. There is no row
      # left for it to censor, and `weeks_to_loss` reports the short panel.
      data[, .deviation_band := get(design$tstop_var)[NA_integer_]]
      # data.table evaluates `j` once on an empty table to learn its types, so
      # an estimand or a cohort with no deviation at all would reach
      # `min(integer(0))` and warn. Test the rows first instead.
      dev_rows <- !is.na(data[["weeks_to_protocol_deviation"]]) &
        data[[design$tstop_var]] >= data[["weeks_to_protocol_deviation"]]
      if (any(dev_rows)) {
        dev_band <- data[
          dev_rows,
          list(dev_band_stop = min(get(design$tstop_var))),
          by = c(design$id_var)
        ]
        data[
          dev_band,
          .deviation_band := i.dev_band_stop,
          on = c(design$id_var)
        ]
      }

      # The band that carries the event, read the same way. `weeks_to_event`
      # is exact to the week now, so comparing it against `.deviation_band`
      # would compare a week against a band stop and almost never meet. The
      # two bands are on one footing here, so the same-band rule below keeps
      # the meaning it had. On the band-collapsed fallback the event already
      # sits on a band stop, and this returns that stop.
      data[, .event_band := get(design$tstop_var)[NA_integer_]]
      ev_band_rows <- !is.na(data[["weeks_to_event"]]) &
        data[[design$tstop_var]] >= data[["weeks_to_event"]]
      if (any(ev_band_rows)) {
        ev_band <- data[
          ev_band_rows,
          list(ev_band_stop = min(get(design$tstop_var))),
          by = c(design$id_var)
        ]
        data[
          ev_band,
          .event_band := i.ev_band_stop,
          on = c(design$id_var)
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

        # `difftime()` measures last date to last date, so it counts the whole
        # weeks BETWEEN the two. The person is under study to the end of the
        # administrative week itself, and `tstart = 0` opens at the START of
        # the baseline week. The stop is therefore one week later.
        data[, weeks_to_admin_end := weeks_to_admin_end + 1L]

        # The end is exact. It is not rounded to a band boundary, so a trial
        # that enters two weeks before it keeps those two weeks instead of
        # losing them. Only a trial that enters at or after the
        # administrative week now has nothing to contribute.
        n_dropped <- data[
          weeks_to_admin_end <= 0L,
          uniqueN(get(design$id_var))
        ]
        if (n_dropped > 0) {
          warning(
            n_dropped,
            " trial(s) will be dropped (entered at or after ",
            "admin_censor_isoyearweek)"
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
      # Boundary priority 3: the administrative end and the requested
      # follow-up end. Both are exact to the week. Neither is rounded to a
      # band boundary, so a six-week requested follow-up stops at week six and
      # not at week eight.
      data[,
        .planned_end := pmin(
          weeks_to_admin_end,
          effective_follow_up,
          na.rm = TRUE
        )
      ]

      # Boundary priority 1 beats priority 2 inside one band. `.deviation_band`
      # names the band the deviation falls in and `.event_band` names the band
      # the event falls in. When the two are the same band, the event wins: the
      # deviation does not clip the row, and `censor_this_period` stays 0
      # below. The row still stops at the exact event week, which can fall
      # inside the band. A woman who deviates in week 6 and has the outcome in
      # week 7 stops at week 7, and the band runs to week 8.
      #
      # Every other deviation clips at its own exact week.
      data[, .deviation_clip := weeks_to_protocol_deviation]
      data[
        !is.na(.event_band) &
          !is.na(.deviation_band) &
          .deviation_band == .event_band,
        .deviation_clip := NA_integer_
      ]

      data[, .max_tstop := max(get(design$tstop_var)), by = c(design$id_var)]
      data[,
        .first_planned_stop := pmin(
          weeks_to_event,
          .deviation_clip,
          .planned_end,
          na.rm = TRUE
        )
      ]
      # Boundary priority 2: the week the record stops at. `.max_tstop` is the
      # stop of the LAST BAND, so it credits a record that ends inside a band
      # with weeks the person was never observed for. `enroll()` writes the
      # exact week into `weeks_to_record_end`, and this reads it where it has
      # one. A panel built outside `enroll()` keeps the band-level read.
      data[, .record_end := .max_tstop]
      if ("weeks_to_record_end" %in% names(data)) {
        data[,
          .record_end := pmin(.max_tstop, weeks_to_record_end, na.rm = TRUE)
        ]
      }
      data[,
        weeks_to_loss := data.table::fifelse(
          .record_end < .first_planned_stop,
          .record_end,
          NA_integer_
        )
      ]

      # censor_week
      data[,
        censor_week := pmin(
          weeks_to_event,
          .deviation_clip,
          weeks_to_loss,
          .planned_end,
          na.rm = TRUE
        )
      ]

      # Clip the terminal row at the boundary, and keep every week before it.
      # A row that opens at or after the boundary contributes no exposure, so
      # it is dropped. Every retained row then has `tstop > tstart`, and
      # `log(person_weeks)` is finite in every offset model.
      data <- data[get(design$tstart_var) < censor_week | is.na(censor_week)]
      new_tstop <- pmin(
        data[[design$tstop_var]],
        data[["censor_week"]],
        na.rm = TRUE
      )
      storage.mode(new_tstop) <- storage.mode(data[[design$tstop_var]])
      data.table::set(data, j = design$tstop_var, value = new_tstop)
      data[,
        person_weeks := get(design$tstop_var) - get(design$tstart_var)
      ]

      # event indicator
      data[, event := as.integer(get(design$tstop_var) == weeks_to_event)]
      data[is.na(event), event := 0L]

      # censor_this_period indicator
      data[,
        censor_this_period := as.integer(
          get(design$tstop_var) == .deviation_clip |
            get(design$tstop_var) == weeks_to_loss
        )
      ]
      data[is.na(censor_this_period), censor_this_period := 0L]
      # Event takes precedence over same-band protocol deviation: in discrete
      # time the outcome is measured over the interval before within-interval
      # censoring is applied, so a person-trial whose first event falls in the
      # same band as its deviation exits the risk set through the event.
      # `.deviation_clip` above already stops the deviation clipping that band,
      # and the row then stops at the exact event week. This line makes the
      # label agree, so the IPCW model never sees a spurious censoring where
      # the trial actually ended in an event.
      data[event == 1L, censor_this_period := 0L]

      # Clean up (.protocol_deviated only exists for the fallback read)
      tmp_cols <- intersect(
        c(
          ".max_tstop",
          ".record_end",
          ".first_planned_stop",
          ".protocol_deviated",
          ".deviation_band",
          ".event_band",
          ".deviation_clip",
          ".planned_end"
        ),
        names(data)
      )
      data[, (tmp_cols) := NULL]
      data.table::setorderv(data, c(design$id_var, design$tstop_var))

      self$data <- data
      self$steps_completed <- c(self$steps_completed, "prepare_outcome")
      invisible(self)
    },

    # --- s6_ipcw_pp: inverse probability of censoring weights (per-protocol) ----
    #
    # The censoring model is complementary log-log with a person-time offset:
    #
    #   cloglog{Pr(C_i = 1)} = eta_i + log(person_weeks_i)
    #
    # so the probability of staying uncensored over the row is
    # `q_i = exp(-exp(eta_i) * person_weeks_i)`. One linear predictor then
    # gives `q(4) = q(1)^4`, which is what makes a four-week band and a
    # one-week band comparable. A logit link carries no such identity, so a
    # clipped terminal band would take a whole band's censoring risk.
    #
    # The weight is LAGGED. It is the probability of remaining uncensored
    # through the START of the row, so the product stops at the row before.
    # The first row of every person-trial then weighs exactly 1. A censored
    # band stays in the risk set (`s5_prepare_outcome()` clips it and keeps
    # it), and an inclusive product would count that band's own censoring
    # probability inside its own weight.
    #
    # The numerator is a second fitted model. It carries the same follow-up
    # and calendar time terms as the denominator and drops the confounders.
    # That is the stabilisation of Danaei (2013), read for a marginal outcome
    # model: the numerator conditions on time and not on the confounders,
    # because the outcome model carries no confounder to condition on.
    #
    # A stratum that cannot be estimated stops. swereg substitutes no marginal
    # censoring rate for a model it could not fit.
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
      tstart_var <- design$tstart_var
      tstop_var <- design$tstop_var
      use_gam <- estimate_ipcw_pp_with_gam
      separate_by_treatment <- estimate_ipcw_pp_separately_by_treatment

      # The censoring model reads the TIME-UPDATED confounder, and never the
      # entry-window snapshot. A missing value there makes `predict()` return
      # NA, `p_uncensored` becomes NA, and `cumprod()` below carries that NA
      # through the rest of the person-trial. Stop, and name what is missing.
      #
      # swereg MUST NOT substitute the entry-window value here. That value
      # describes the recruiting week, and reading it during follow-up is the
      # confounding this design removes.
      .tte_stop_on_missing_ipcw_confounders(
        working_data,
        confounder_vars,
        id_var
      )

      if (use_gam && !requireNamespace("mgcv", quietly = TRUE)) {
        stop(
          "Package 'mgcv' is required for use_gam = TRUE. ",
          "Install it with: install.packages('mgcv')"
        )
      }

      # Person-time carries the offset. `s5_prepare_outcome()` writes
      # `person_weeks`, and a panel that arrives without it holds the same
      # quantity in its own interval.
      if (!"person_weeks" %in% names(working_data)) {
        working_data[,
          person_weeks := get(tstop_var) - get(tstart_var)
        ]
      }
      # `log(0)` is `-Inf`, so a zero-width row MUST NOT enter the offset. It
      # holds no person-time, so nothing can censor it, and its uncensoring
      # probability is exactly 1 in both models.
      working_data[,
        .ipcw_has_time := !is.na(person_weeks) & person_weeks > 0
      ]

      # The calendar-time term. `mgcv::s()` asks for 10 basis functions by
      # default and stops below 10 distinct values, so a shorter trial index
      # takes a linear term instead.
      n_trials <- if ("trial_id" %in% names(working_data)) {
        working_data[.ipcw_has_time == TRUE, data.table::uniqueN(trial_id)]
      } else {
        0L
      }
      calendar_term <- if (use_gam && n_trials >= 10L) {
        "s(trial_id)"
      } else if (n_trials > 1L) {
        "trial_id"
      } else {
        ""
      }

      # One stratum: fit the denominator and the numerator, and write the two
      # per-row uncensoring probabilities. `label` names the stratum in every
      # error message, because a stratum that stops must say which one it was.
      fit_stratum <- function(mask, label) {
        keep <- mask & working_data[[".ipcw_has_time"]]
        rows <- which(keep)
        n_rows <- length(rows)
        if (n_rows == 0L) {
          return(invisible(NULL))
        }
        fit_data <- working_data[rows]
        n_censor <- sum(fit_data[[censoring_var]], na.rm = TRUE)

        # No censoring anywhere in the stratum. Every row stays uncensored
        # with probability 1, in the numerator and in the denominator, so the
        # weight is 1. That is the exact answer and not a fallback.
        if (n_censor == 0L) {
          data.table::set(
            working_data,
            i = rows,
            j = "q_denominator",
            value = 1
          )
          data.table::set(working_data, i = rows, j = "q_numerator", value = 1)
          return(invisible(NULL))
        }
        if (n_censor == n_rows) {
          stop(
            "s6_ipcw_pp() cannot estimate the censoring model for ",
            label,
            ".\n",
            "Every one of its ",
            n_rows,
            " rows is censored, so the model has no uncensored row to ",
            "contrast them with.\n",
            "swereg substitutes no marginal censoring rate here. A weight ",
            "built from one is not the weight the analysis reports.\n",
            "Widen the stratum, or drop it from the analysis.",
            call. = FALSE
          )
        }

        n_starts <- data.table::uniqueN(fit_data[[tstart_var]])
        time_term <- .tte_ipcw_time_term(tstart_var, n_starts, use_gam)

        # `role` is "denominator" or "numerator". The two models differ only
        # in whether they carry the confounders.
        fit_one <- function(terms, role) {
          terms <- terms[nzchar(terms)]
          rhs <- if (length(terms) == 0L) {
            "1"
          } else {
            paste(terms, collapse = " + ")
          }
          model_formula <- stats::as.formula(paste0(
            censoring_var,
            " ~ ",
            rhs,
            " + offset(log(person_weeks))"
          ))
          fit <- tryCatch(
            if (use_gam) {
              mgcv::bam(
                model_formula,
                data = fit_data,
                family = stats::binomial(link = "cloglog"),
                discrete = TRUE
              )
            } else {
              stats::glm(
                model_formula,
                data = fit_data,
                family = stats::binomial(link = "cloglog")
              )
            },
            error = function(e) {
              stop(
                "s6_ipcw_pp() cannot fit the ",
                role,
                " censoring model for ",
                label,
                ".\n",
                "  formula: ",
                deparse1(model_formula),
                "\n",
                "  rows: ",
                n_rows,
                ", censored: ",
                n_censor,
                "\n",
                "  the model reported: ",
                conditionMessage(e),
                "\n",
                "swereg substitutes no marginal censoring rate here.",
                call. = FALSE
              )
            }
          )
          q <- 1 -
            as.numeric(stats::predict(
              fit,
              newdata = fit_data,
              type = "response"
            ))
          rm(fit)
          if (anyNA(q) || any(!is.finite(q)) || any(q <= 0)) {
            stop(
              "s6_ipcw_pp() fitted the ",
              role,
              " censoring model for ",
              label,
              ", and it predicts an uncensoring probability that is not ",
              "usable.\n",
              "  formula: ",
              deparse1(model_formula),
              "\n",
              "  rows: ",
              n_rows,
              ", not finite: ",
              sum(is.na(q) | !is.finite(q)),
              ", not positive: ",
              sum(!is.na(q) & is.finite(q) & q <= 0),
              "\n",
              "A weight divides by this probability, so swereg stops rather ",
              "than carry an infinite or missing weight into the analysis.",
              call. = FALSE
            )
          }
          q
        }

        data.table::set(
          working_data,
          i = rows,
          j = "q_denominator",
          value = fit_one(
            c(time_term, calendar_term, confounder_vars),
            "denominator"
          )
        )
        data.table::set(
          working_data,
          i = rows,
          j = "q_numerator",
          value = fit_one(c(time_term, calendar_term), "numerator")
        )
        rm(fit_data)
        gc()
      }

      working_data[, q_denominator := NA_real_]
      working_data[, q_numerator := NA_real_]
      if (separate_by_treatment) {
        tx_mask <- working_data[[treatment_var]] == TRUE
        fit_stratum(tx_mask, "the intervention arm")
        fit_stratum(!tx_mask, "the comparator arm")
      } else {
        fit_stratum(rep(TRUE, nrow(working_data)), "the pooled cohort")
      }
      # A zero-width row was held out of both fits. Nothing happens over an
      # empty interval, so it stays uncensored with probability 1.
      working_data[.ipcw_has_time == FALSE, q_denominator := 1]
      working_data[.ipcw_has_time == FALSE, q_numerator := 1]
      if (
        anyNA(working_data$q_denominator) || anyNA(working_data$q_numerator)
      ) {
        stop(
          "s6_ipcw_pp() left ",
          sum(
            is.na(working_data$q_denominator) | is.na(working_data$q_numerator)
          ),
          " of ",
          nrow(working_data),
          " rows without an uncensoring probability.",
          call. = FALSE
        )
      }

      # The weight on the row of band k is the probability of remaining
      # uncensored through the START of band k, so the product stops at band
      # k - 1. `shift()` supplies the empty product of 1 on the first row of
      # each person-trial, which makes that row weigh exactly 1.
      data.table::setorderv(working_data, c(id_var, tstart_var))
      working_data[,
        cum_q_denominator := cumprod(
          data.table::shift(q_denominator, n = 1L, fill = 1)
        ),
        by = c(id_var)
      ]
      working_data[,
        cum_q_numerator := cumprod(
          data.table::shift(q_numerator, n = 1L, fill = 1)
        ),
        by = c(id_var)
      ]
      working_data[, ipcw_pp := cum_q_numerator / cum_q_denominator]

      if ("ipcw_pp" %in% names(self$data)) {
        self$data[, ipcw_pp := NULL]
      }
      # The band, not the band stop. A zero-width row shares its stop with the
      # row before it, so a stop alone does not name one row.
      join_on <- c(design$id_var, design$tstart_var, design$tstop_var)
      self$data[
        working_data,
        ipcw_pp := i.ipcw_pp,
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
    # Private weight/draw/collapse helpers
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
