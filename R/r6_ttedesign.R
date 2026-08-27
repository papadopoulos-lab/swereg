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
        stop("person_id_var must be length 1 or NULL", call. = FALSE)
      }
      if (length(id_var) != 1) {
        stop("id_var must be length 1", call. = FALSE)
      }
      if (length(treatment_var) != 1) {
        stop("treatment_var must be length 1", call. = FALSE)
      }
      if (length(outcome_vars) == 0) {
        stop("outcome_vars cannot be empty", call. = FALSE)
      }
      if (length(follow_up_time) != 1 || follow_up_time <= 0) {
        stop("follow_up_time must be a positive integer", call. = FALSE)
      }
      if (length(tstart_var) != 1) {
        stop("tstart_var must be length 1", call. = FALSE)
      }
      if (length(tstop_var) != 1) {
        stop("tstop_var must be length 1", call. = FALSE)
      }
      if (!is.null(time_treatment_var) && length(time_treatment_var) != 1) {
        stop("time_treatment_var must be length 1 or NULL", call. = FALSE)
      }
      if (!is.null(eligible_var) && length(eligible_var) != 1) {
        stop("eligible_var must be length 1 or NULL", call. = FALSE)
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
        stop("admin_censor_var must be length 1 or NULL", call. = FALSE)
      }
      if (
        !is.null(admin_censor_isoyearweek) &&
          length(admin_censor_isoyearweek) != 1
      ) {
        stop("admin_censor_isoyearweek must be length 1 or NULL", call. = FALSE)
      }
      if (!is.null(admin_censor_var) && !is.null(admin_censor_isoyearweek)) {
        stop(
          "admin_censor_var and admin_censor_isoyearweek are mutually exclusive",
          call. = FALSE
        )
      }
      if (
        length(period_width) != 1 ||
          !is.numeric(period_width) ||
          period_width <= 0 ||
          period_width != as.integer(period_width)
      ) {
        stop("period_width must be a positive integer", call. = FALSE)
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

      return(private$.schema_version <- .TTE_DESIGN_SCHEMA_VERSION)
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
      return(invisible(TRUE))
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
      return(invisible(self))
    }
  ),

  private = list(
    .schema_version = NULL
  )
)
