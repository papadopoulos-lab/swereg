# =============================================================================
# TTEDesign: Trial design specification (R6 class)
# =============================================================================
# Holds column name mappings that define the schema for trial data.
# Extracted from tte_classes.R during standalone → R6 method migration.
# =============================================================================

#' TTEDesign class for target trial emulation
#'
#' Holds column name mappings that define the schema for trial data. This
#' allows specifying variable names once and reusing them across all TTE
#' workflow functions.
#'
#' @param person_id_var Character or NULL, name of the person identifier column
#'   for pre-panel (person-week) data. Required for [tte_match()] and
#'   [tte_expand()] operations. (default: NULL).
#' @param id_var Character, name of the trial identifier column. Auto-generated
#'   by [tte_expand()] as "trial_id" (default: "trial_id").
#' @param exposure_var Character, name of the baseline exposure/treatment column.
#' @param outcome_vars Character vector, names of outcome event indicator columns.
#' @param confounder_vars Character vector, names of confounder columns for
#'   propensity/censoring models.
#' @param follow_up_time Integer, expected follow-up duration in time units.
#' @param tstart_var Character, name of period start time column (default: "tstart").
#' @param tstop_var Character, name of period end time column (default: "tstop").
#' @param time_exposure_var Character or NULL, name of time-varying exposure column
#'   for per-protocol analysis (default: NULL).
#' @param eligible_var Character or NULL, name of eligibility indicator column
#'   (default: NULL).
#' @param admin_censor_var Character or NULL, name of administrative censoring
#'   boundary column (default: NULL). Mutually exclusive with
#'   `admin_censor_isoyearweek`.
#' @param admin_censor_isoyearweek Character or NULL, the study end date in
#'   ISO year-week format (e.g., "2023-52"). When set, administrative censoring
#'   is computed internally as weeks from each trial's entry date to this
#'   global study end date. Requires an `isoyearweek` column in the data.
#'   Mutually exclusive with `admin_censor_var` (default: NULL).
#'
#' @examples
#' # Design for post-panel (trial-level) data
#' design <- tte_design(
#'   id_var = "trial_id",
#'   exposure_var = "baseline_exposed",
#'   outcome_vars = c("death", "hosp"),
#'   confounder_vars = c("age", "education"),
#'   follow_up_time = 156L
#' )
#'
#' # Design for pre-panel (person-week) data with full workflow
#' design_prepanel <- tte_design(
#'   person_id_var = "id",
#'   exposure_var = "baseline_exposed",
#'   outcome_vars = c("death", "hosp"),
#'   confounder_vars = c("age", "education"),
#'   follow_up_time = 156L,
#'   eligible_var = "eligible"
#' )
#'
#' @family tte_classes
#' @seealso [tte_trial()] for creating trial objects, [TTETrial] for the trial class
#' @importFrom R6 R6Class
#' @export
TTEDesign <- R6::R6Class("TTEDesign",
  public = list(
    #' @field person_id_var Character or NULL, person identifier column name.
    person_id_var = NULL,
    #' @field id_var Character, trial identifier column name.
    id_var = "trial_id",
    #' @field exposure_var Character, exposure/treatment column name.
    exposure_var = NULL,
    #' @field outcome_vars Character vector, outcome column names.
    outcome_vars = NULL,
    #' @field confounder_vars Character vector, confounder column names.
    confounder_vars = NULL,
    #' @field follow_up_time Integer, follow-up duration.
    follow_up_time = NULL,
    #' @field tstart_var Character, period start time column name.
    tstart_var = "tstart",
    #' @field tstop_var Character, period end time column name.
    tstop_var = "tstop",
    #' @field time_exposure_var Character or NULL, time-varying exposure column.
    time_exposure_var = NULL,
    #' @field eligible_var Character or NULL, eligibility column name.
    eligible_var = NULL,
    #' @field admin_censor_var Character or NULL, admin censoring column.
    admin_censor_var = NULL,
    #' @field admin_censor_isoyearweek Character or NULL, admin censoring date.
    admin_censor_isoyearweek = NULL,

    #' @description Create a new TTEDesign object.
    initialize = function(person_id_var = NULL, id_var = "trial_id",
                          exposure_var, outcome_vars, confounder_vars,
                          follow_up_time, tstart_var = "tstart",
                          tstop_var = "tstop", time_exposure_var = NULL,
                          eligible_var = NULL, admin_censor_var = NULL,
                          admin_censor_isoyearweek = NULL) {
      # Validation
      if (!is.null(person_id_var) && length(person_id_var) != 1) {
        stop("person_id_var must be length 1 or NULL")
      }
      if (length(id_var) != 1) stop("id_var must be length 1")
      if (length(exposure_var) != 1) stop("exposure_var must be length 1")
      if (length(outcome_vars) == 0) stop("outcome_vars cannot be empty")
      if (length(follow_up_time) != 1 || follow_up_time <= 0) {
        stop("follow_up_time must be a positive integer")
      }
      if (length(tstart_var) != 1) stop("tstart_var must be length 1")
      if (length(tstop_var) != 1) stop("tstop_var must be length 1")
      if (!is.null(time_exposure_var) && length(time_exposure_var) != 1) {
        stop("time_exposure_var must be length 1 or NULL")
      }
      if (!is.null(eligible_var) && length(eligible_var) != 1) {
        stop("eligible_var must be length 1 or NULL")
      }
      if (!is.null(admin_censor_var) && length(admin_censor_var) != 1) {
        stop("admin_censor_var must be length 1 or NULL")
      }
      if (!is.null(admin_censor_isoyearweek) &&
          length(admin_censor_isoyearweek) != 1) {
        stop("admin_censor_isoyearweek must be length 1 or NULL")
      }
      if (!is.null(admin_censor_var) && !is.null(admin_censor_isoyearweek)) {
        stop("admin_censor_var and admin_censor_isoyearweek are mutually exclusive")
      }

      self$person_id_var <- person_id_var
      self$id_var <- id_var
      self$exposure_var <- exposure_var
      self$outcome_vars <- outcome_vars
      self$confounder_vars <- confounder_vars
      self$follow_up_time <- as.integer(follow_up_time)
      self$tstart_var <- tstart_var
      self$tstop_var <- tstop_var
      self$time_exposure_var <- time_exposure_var
      self$eligible_var <- eligible_var
      self$admin_censor_var <- admin_censor_var
      self$admin_censor_isoyearweek <- admin_censor_isoyearweek
    },

    #' @description Print the TTEDesign object.
    print = function(...) {
      cat("<TTEDesign>\n")
      if (!is.null(self$person_id_var)) {
        cat("  Person ID:", self$person_id_var, "\n")
      }
      cat("  Trial ID:", self$id_var, "\n")
      cat("  Exposure:", self$exposure_var, "\n")
      if (!is.null(self$time_exposure_var)) {
        cat("  Time-varying exposure:", self$time_exposure_var, "\n")
      }
      cat("  Outcomes:", paste(self$outcome_vars, collapse = ", "), "\n")
      cat("  Confounders:", paste(self$confounder_vars, collapse = ", "), "\n")
      cat("  Follow-up:", self$follow_up_time, "time units\n")
      cat("  Time vars:", self$tstart_var, "/", self$tstop_var, "\n")
      if (!is.null(self$eligible_var)) {
        cat("  Eligibility:", self$eligible_var, "\n")
      }
      invisible(self)
    }
  )
)

#' Create a TTE design specification
#'
#' Constructor function for [TTEDesign] objects. Defines the column name
#' mappings for a target trial emulation analysis.
#'
#' @param exposure_var Character, name of the baseline exposure/treatment column.
#' @param outcome_vars Character vector, names of outcome event indicator columns.
#' @param confounder_vars Character vector, names of confounder columns.
#' @param follow_up_time Integer, expected follow-up duration in time units.
#' @param person_id_var Character or NULL, name of the person identifier column
#'   for pre-panel (person-week) data. Required for [tte_match()] and
#'   [tte_expand()] operations. (default: NULL).
#' @param id_var Character, name of the trial identifier column. Auto-generated
#'   by [tte_expand()] as "trial_id" (default: "trial_id").
#' @param tstart_var Character, period start time column (default: "tstart").
#' @param tstop_var Character, period end time column (default: "tstop").
#' @param time_exposure_var Character or NULL, time-varying exposure column
#'   for per-protocol analysis (default: NULL).
#' @param eligible_var Character or NULL, eligibility indicator column
#'   (default: NULL).
#' @param admin_censor_var Character or NULL, administrative censoring
#'   boundary column (default: NULL). Mutually exclusive with
#'   `admin_censor_isoyearweek`.
#' @param admin_censor_isoyearweek Character or NULL, the study end date in
#'   ISO year-week format (e.g., "2023-52"). When set, administrative censoring
#'   is computed internally by `$prepare_outcome()`. Mutually exclusive with
#'   `admin_censor_var` (default: NULL).
#'
#' @return A [TTEDesign] object.
#'
#' @examples
#' # Design for post-panel (trial-level) data
#' design <- tte_design(
#'   exposure_var = "baseline_exposed",
#'   time_exposure_var = "current_exposed",
#'   outcome_vars = c("death", "hospitalization"),
#'   confounder_vars = c("age", "sex", "education"),
#'   follow_up_time = 156L
#' )
#'
#' # Design for pre-panel (person-week) data
#' design_prepanel <- tte_design(
#'   person_id_var = "id",
#'   exposure_var = "baseline_exposed",
#'   outcome_vars = c("death", "hospitalization"),
#'   confounder_vars = c("age", "sex"),
#'   follow_up_time = 156L,
#'   eligible_var = "eligible"
#' )
#'
#' @family tte_classes
#' @seealso [TTEDesign] for class details, [tte_trial()] for creating trials
#' @export
tte_design <- function(
    exposure_var,
    outcome_vars,
    confounder_vars,
    follow_up_time,
    person_id_var = NULL,
    id_var = "trial_id",
    tstart_var = "tstart",
    tstop_var = "tstop",
    time_exposure_var = NULL,
    eligible_var = NULL,
    admin_censor_var = NULL,
    admin_censor_isoyearweek = NULL
) {
  TTEDesign$new(
    person_id_var = person_id_var,
    id_var = id_var,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars,
    confounder_vars = confounder_vars,
    follow_up_time = as.integer(follow_up_time),
    tstart_var = tstart_var,
    tstop_var = tstop_var,
    time_exposure_var = time_exposure_var,
    eligible_var = eligible_var,
    admin_censor_var = admin_censor_var,
    admin_censor_isoyearweek = admin_censor_isoyearweek
  )
}
