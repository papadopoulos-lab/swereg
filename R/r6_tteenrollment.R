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

#' TTEDesign class for target trial emulation
#'
#' Holds column name mappings that define the schema for trial data. This
#' allows specifying variable names once and reusing them across all TTE
#' workflow functions.
#'
#' @param person_id_var Character or NULL, name of the person identifier column
#'   for pre-panel (person-week) data (default: NULL).
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
#'   `admin_censor_isoyearweek`.
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
      person_id_var = NULL,
      id_var = "enrollment_person_trial_id",
      treatment_var,
      outcome_vars,
      confounder_vars,
      follow_up_time,
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
#'   \item{`$km(ipw_col, save_path, title)`}{Fit Kaplan-Meier curves}
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
      trial_level <- self$data[,
        lapply(.SD, data.table::first),
        by = c(id_var),
        .SDcols = confounder_vars
      ]

      set.seed(seed)
      for (var in confounder_vars) {
        missing_trials <- trial_level[is.na(get(var)), get(id_var)]
        if (length(missing_trials) > 0) {
          observed_vals <- trial_level[!is.na(get(var)), get(var)]
          sampled_vals <- sample(
            observed_vals,
            length(missing_trials),
            replace = TRUE
          )
          trial_level[get(id_var) %in% missing_trials, (var) := sampled_vals]
        }
      }

      self$data[, (confounder_vars) := NULL]
      data.table::setkeyv(self$data, id_var)
      data.table::setkeyv(trial_level, id_var)
      self$data <- merge(
        self$data,
        trial_level[, .SD, .SDcols = c(id_var, confounder_vars)],
        by = id_var,
        all.x = TRUE
      )

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
    #' `$irr()` and `$km()` (Hernan 2008, Danaei 2013).
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

    #' @description Step 4: Prepare outcome data and calculate IPCW-PP in one step.
    #' Calls `$s5_prepare_outcome()` followed by `$s6_ipcw_pp()`. This is the
    #' recommended way to prepare an enrollment for analysis.
    #' @param outcome Character scalar. Must be one of `design$outcome_vars`.
    #' @param follow_up Optional integer. Overrides `design$follow_up_time`.
    #' @param estimate_ipcw_pp_separately_by_treatment Logical, default TRUE.
    #' @param estimate_ipcw_pp_with_gam Logical, default TRUE.
    #' @param censoring_var Character or NULL. Defaults to `"censor_this_period"`.
    s4_prepare_for_analysis = function(
      outcome,
      follow_up = NULL,
      estimate_ipcw_pp_separately_by_treatment = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      censoring_var = NULL
    ) {
      private$s5_prepare_outcome(outcome = outcome, follow_up = follow_up)
      if (is.null(censoring_var)) {
        censoring_var <- "censor_this_period"
      }
      private$s6_ipcw_pp(
        estimate_ipcw_pp_separately_by_treatment = estimate_ipcw_pp_separately_by_treatment,
        estimate_ipcw_pp_with_gam = estimate_ipcw_pp_with_gam,
        censoring_var = censoring_var
      )
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

      n_individuals <- if (
        !is.null(design$person_id_var) &&
          design$person_id_var %in% names(data)
      ) {
        data.table::uniqueN(data[[design$person_id_var]])
      } else {
        NA_integer_
      }

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
        if (!is.na(n_individuals)) {
          parts <- c(
            parts,
            paste(format(n_individuals, big.mark = ","), "individuals")
          )
        }
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

      result <- data[,
        .(
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

      # Guard: the swereg pipeline applies per-protocol censoring, so the
      # dataset is only valid for per-protocol analysis. Using IPW-only weights
      # (without IPCW) on a per-protocol censored dataset is methodologically
      # incorrect — it would produce biased ITT-like estimates on a dataset
      # that has already been censored at protocol deviation.
      ipw_only_cols <- c("ipw", "ipw_trunc")
      if (
        weight_col %in%
          ipw_only_cols &&
          "prepare_outcome" %in% self$steps_completed
      ) {
        stop(
          "Cannot use '",
          weight_col,
          "' as weight_col after per-protocol censoring.\n",
          "The dataset has been censored at protocol deviation via $s4_prepare_for_analysis(),\n",
          "so only per-protocol weights (e.g., 'analysis_weight_pp_trunc') are valid.\n",
          "Using IPW-only weights on per-protocol censored data produces biased estimates."
        )
      }

      # Include trial_id in outcome model if available (calendar-time adjustment)
      # Caniglia 2023: "include 'trial' as an adjustment variable"
      # Danaei 2013: "month at the trial's baseline" as covariate
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
        " * splines::ns(trial_id, df = ", spline_df, ")",
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
      wald_stat <- as.numeric(t(beta_int) %*% solve(vcov_int) %*% beta_int)
      p_value <- stats::pchisq(
        wald_stat,
        df = length(interaction_idx),
        lower.tail = FALSE
      )

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

    #' @description Fit Kaplan-Meier curves and optionally plot.
    #' Uses IPW only (not IPCW) because IPCW is time-varying.
    #' @param ipw_col Character, required. Column name for IPW weights.
    #' @param save_path Character or NULL. If specified, saves the plot.
    #' @param title Character or NULL. Plot title.
    #' @return A svykm object (invisibly if save_path is specified).
    km = function(ipw_col, save_path = NULL, title = NULL) {
      if (self$data_level != "trial") {
        stop(
          "km() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'"
        )
      }

      design <- self$design
      data <- self$data

      if (!ipw_col %in% names(data)) {
        stop("ipw_col '", ipw_col, "' not found in data")
      }
      if (!"event" %in% names(data)) {
        stop("'event' column not found. Run $s4_prepare_for_analysis() first.")
      }

      keep_cols <- unique(c(
        design$id_var,
        design$person_id_var,
        design$treatment_var,
        design$tstop_var,
        ipw_col,
        "event"
      ))
      final <- data[, ..keep_cols][, .SD[.N], by = c(design$id_var)]

      svy_design <- survey::svydesign(
        ids = as.formula(paste0("~", design$person_id_var)),
        weights = as.formula(paste0("~", ipw_col)),
        data = final
      )
      rm(final)

      km_formula <- stats::as.formula(paste0(
        "survival::Surv(",
        design$tstop_var,
        ", event) ~ ",
        design$treatment_var
      ))

      km_fit <- survey::svykm(km_formula, design = svy_design)

      if (!is.null(save_path)) {
        strata_names <- names(km_fit)
        comparator_name <- strata_names[grepl("FALSE", strata_names)]
        intervention_name <- strata_names[grepl("TRUE", strata_names)]
        if (length(comparator_name) == 0) {
          comparator_name <- "FALSE"
        }
        if (length(intervention_name) == 0) {
          intervention_name <- "TRUE"
        }

        df_comparator <- data.frame(
          time = km_fit[[comparator_name]]$time,
          surv = km_fit[[comparator_name]]$surv,
          group = "Comparator"
        )
        df_intervention <- data.frame(
          time = km_fit[[intervention_name]]$time,
          surv = km_fit[[intervention_name]]$surv,
          group = "Intervention"
        )
        df <- rbind(df_comparator, df_intervention)

        q <- ggplot2::ggplot(
          df,
          ggplot2::aes(x = time, y = surv, color = group)
        ) +
          ggplot2::geom_step(linewidth = 1) +
          ggplot2::scale_color_manual(
            values = c("Comparator" = "blue", "Intervention" = "red")
          ) +
          ggplot2::scale_y_continuous(
            limits = c(0.99, 1),
            labels = scales::percent
          ) +
          ggplot2::labs(
            title = title,
            x = "Time (weeks)",
            y = "Event-free survival",
            color = "Treatment"
          ) +
          ggplot2::theme_minimal()

        ggplot2::ggsave(save_path, q, width = 8, height = 6, dpi = 300)
        invisible(km_fit)
      } else {
        km_fit
      }
    }
  ),

  private = list(
    .schema_version = NULL,

    # =========================================================================
    # Private methods — internal implementation details
    # =========================================================================

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
        entry_dt[, (id_var) := paste0(.tte_person_id, ".", entry_band_id)]
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
        entry_dt[, (id_var) := paste0(.tte_person_id, ".", entry_band_id)]

        enrolled_person_ids <- unique(entry_dt$.tte_person_id)
      }

      # ---- Phase B: Full collapse (enrolled persons only) ----
      data_enrolled <- data[get(person_id_col) %in% enrolled_person_ids]

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

      # Aggregate within each (person_id, trial_id) — single pass
      by_cols <- c(person_id_col, "trial_id")
      data.table::setorderv(
        data_enrolled,
        c(person_id_col, "trial_id", "isoyearweek")
      )
      # Data is sorted by (pid, trial_id, isoyearweek) from setorderv above,
      # so setkeyv on the first two columns is an O(n) verification, not a full
      # sort. Enables binary-search grouping for the Phase B aggregation below.
      data.table::setkeyv(data_enrolled, c(person_id_col, "trial_id"))

      # Build aggregation expression list
      agg_exprs <- list(
        isoyearweek = quote(data.table::first(isoyearweek)),
        .n_source_weeks = quote(.N)
      )
      for (col in collapse_first_cols) {
        if (col != "isoyearweek")
          agg_exprs[[col]] <- substitute(
            data.table::first(x), list(x = as.name(col))
          )
      }
      for (col in collapse_last_cols) {
        agg_exprs[[col]] <- substitute(
          data.table::last(x), list(x = as.name(col))
        )
      }
      for (col in collapse_max_cols) {
        agg_exprs[[col]] <- substitute(
          max(x, na.rm = TRUE), list(x = as.name(col))
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
    s5_prepare_outcome = function(outcome, follow_up = NULL) {
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
      if (is.null(design$time_treatment_var)) {
        stop(
          "design must have time_treatment_var for per-protocol censoring analysis"
        )
      }

      data[,
        .protocol_deviated := data.table::fcase(
          get(design$treatment_var) == TRUE & (get(design$time_treatment_var) == FALSE | is.na(get(design$time_treatment_var))) ,
          TRUE                                                                                                               ,
          get(design$treatment_var) == FALSE & (get(design$time_treatment_var) == TRUE | is.na(get(design$time_treatment_var))) ,
          TRUE                                                                                                               ,
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

      # Clean up
      data[,
        c(".max_tstop", ".first_planned_stop", ".protocol_deviated") := NULL
      ]
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
                ipcw_formula, data = subset_data, family = stats::binomial
              )
            }
          },
          error = function(e) {
            warning(
              "IPCW model failed (", conditionMessage(e),
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

  TTEEnrollment$new(
    data = combined_data,
    design = design,
    data_level = data_level,
    steps_completed = steps,
    weight_cols = weight_cols
  )
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
