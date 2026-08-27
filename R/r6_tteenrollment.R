# =============================================================================
# TTEEnrollment R6 class, and the two enrollment-side schema versions
# =============================================================================

.TTE_DESIGN_SCHEMA_VERSION <- 3L
.TTE_ENROLLMENT_SCHEMA_VERSION <- 3L

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
# The standalone helpers moved to tteenrollment_combine.R. Two sibling files
# carry the rest of the class through $set(). The weight members are in
# r6_tteenrollment_weighting.R, the estimate members in
# r6_tteenrollment_estimation.R.
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
            "') for trial data",
            call. = FALSE
          )
        }
      }

      # Validation
      if (!data_level %in% c("person_week", "trial")) {
        stop("data_level must be 'person_week' or 'trial'", call. = FALSE)
      }
      if (data_level == "person_week") {
        if (is.null(design$person_id_var)) {
          stop(
            "person_week data requires person_id_var in design",
            call. = FALSE
          )
        }
        if (!design$person_id_var %in% names(data)) {
          stop(paste(
            "person_week data requires person_id_var column:",
            design$person_id_var
          ), call. = FALSE)
        }
      } else {
        if (!design$id_var %in% names(data)) {
          stop(paste(
            "trial data requires id_var column:",
            design$id_var
          ), call. = FALSE)
        }
      }
      if (!design$treatment_var %in% names(data)) {
        stop(
          paste("Missing required column:", design$treatment_var),
          call. = FALSE
        )
      }
      if (
        !is.null(active_outcome) &&
          !active_outcome %in% design$outcome_vars
      ) {
        stop("active_outcome must be one of design$outcome_vars", call. = FALSE)
      }

      self$data <- data
      self$design <- design
      self$data_level <- data_level
      self$steps_completed <- steps_completed
      self$active_outcome <- active_outcome
      self$weight_cols <- weight_cols

      private$.schema_version <- .TTE_ENROLLMENT_SCHEMA_VERSION

      if (!is.null(ratio) || !is.null(enrolled_ids)) {
        return(private$enroll(
          ratio = ratio,
          seed = seed,
          extra_cols = extra_cols,
          enrolled_ids = enrolled_ids
        ))
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
      return(invisible(self))
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
      return(invisible(TRUE))
    },

    # =========================================================================
    # Mutating methods — ordered by workflow execution sequence
    # =========================================================================

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
      return(invisible(self))
    },

    # =========================================================================
    # Non-mutating methods — data access, diagnostics, and analysis output
    # =========================================================================

    #' @description Extract the data.table from the trial object.
    #' @return A data.table with the processed trial data.
    extract = function() {
      return(self$data)
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
        return(invisible(result))
      } else {
        return(result)
      }
    },

    #' @description Generate baseline characteristics table.
    #'
    #' Returns a long-format `data.table` with one row per categorical level
    #' plus one row per continuous variable. See `.swereg_table1()` for the
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
      return(
        .tte_est_table1(self, ipw_col, arm_labels, include_smd, show_missing)
      )
    }
  ),

  private = list(
    .schema_version = NULL,

    # =========================================================================
    # Private methods — internal implementation details
    # =========================================================================

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
          "Hint: Pass ratio to TTEEnrollment$new() with person_id_var in design.",
          call. = FALSE
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
        stop(
          "Band-based enrollment requires 'isoyearweek' column in data",
          call. = FALSE
        )
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
          stop(
            "No intervention person-bands found among eligible rows.",
            call. = FALSE
          )
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
      return(invisible(self))
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
          "use admin_censor_isoyearweek instead",
          call. = FALSE
        )
      }
      if (self$data_level != "trial") {
        stop(
          "s5_prepare_outcome() requires trial level data.\n",
          "Current data_level: '",
          self$data_level,
          "'\n",
          "Hint: Pass ratio to TTEEnrollment$new() to convert person_week data to trial level.",
          call. = FALSE
        )
      }

      if ("prepare_outcome" %in% self$steps_completed) {
        stop(
          "s5_prepare_outcome() can only be run once per trial (it deletes rows)",
          call. = FALSE
        )
      }

      design <- self$design
      data <- self$data

      if (!outcome %in% design$outcome_vars) {
        stop(
          "outcome must be one of: ",
          paste(design$outcome_vars, collapse = ", "),
          call. = FALSE
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
            "design must have time_treatment_var for per-protocol censoring analysis",
            call. = FALSE
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
          stop(
            "admin_censor_isoyearweek requires 'isoyearweek' column in data",
            call. = FALSE
          )
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
            "admin_censor_isoyearweek)",
            call. = FALSE
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
      return(invisible(self))
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
      return("enrolled")
    }
  )
)
