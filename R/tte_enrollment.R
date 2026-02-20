# =============================================================================
# TTEEnrollment: Enrollment data with design and state (R6 class)
# =============================================================================
# Object-oriented trial container with 15 inline methods for the full TTE
# workflow: enroll, collapse, ipw, ipcw_pp, prepare_outcome, etc.
# Mutating methods return invisible(self) for $-chaining.
#
# Also includes standalone helpers: tte_enrollment(), tte_rbind(),
# tte_rates_combine(), tte_irr_combine(), and summary.TTEEnrollment S3 method.
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
#'
#' @details
#' The `data_level` property controls which methods are available:
#' - `"person_week"`: Data has one row per person per time unit. Method
#'   `$enroll()` requires this level.
#' - `"trial"`: Data has been expanded to trial panels. Methods `$collapse()`,
#'   `$ipw()`, `$prepare_outcome()`, `$ipcw_pp()`, `$combine_weights()`, and
#'   `$truncate()` require this level.
#'
#' The `$enroll()` method transitions data from "person_week" to "trial" level.
#'
#' @section Methods:
#' **Mutating (return `invisible(self)` for chaining):**
#' \describe{
#'   \item{`$enroll(ratio, seed, extra_cols)`}{Sample comparison group and create trial panels}
#'   \item{`$collapse(period_width, ...)`}{Collapse time intervals to coarser periods}
#'   \item{`$ipw(stabilize)`}{Calculate inverse probability of treatment weights}
#'   \item{`$ipcw_pp(separate_by_exposure, use_gam, censoring_var)`}{Calculate IPCW for per-protocol}
#'   \item{`$combine_weights(ipw_col, ipcw_col, name)`}{Combine IPW and IPCW weights}
#'   \item{`$truncate(weight_cols, lower, upper, suffix)`}{Truncate extreme weights}
#'   \item{`$prepare_outcome(outcome, follow_up)`}{Prepare outcome-specific data}
#'   \item{`$impute_confounders(confounder_vars, seed)`}{Impute missing confounders}
#'   \item{`$weight_summary()`}{Print weight distribution diagnostics}
#' }
#'
#' **Non-mutating (return data):**
#' \describe{
#'   \item{`$extract()`}{Return the data.table}
#'   \item{`$summary(pretty)`}{Return summary statistics}
#'   \item{`$table1(ipw_col)`}{Generate baseline characteristics table}
#'   \item{`$rates(weight_col)`}{Calculate events, person-years, and rates}
#'   \item{`$irr(weight_col)`}{Fit Poisson models and extract IRR}
#'   \item{`$km(ipw_col, save_path, title)`}{Fit Kaplan-Meier curves}
#' }
#'
#' @examples
#' \dontrun{
#' design <- tte_design(
#'   exposure_var = "exposed",
#'   outcome_vars = "death",
#'   confounder_vars = c("age", "sex"),
#'   follow_up_time = 52L
#' )
#' trial <- tte_enrollment(my_trial_data, design)
#'
#' # $-chaining
#' trial$
#'   collapse(period_width = 4)$
#'   ipw()$
#'   prepare_outcome(outcome = "death")$
#'   ipcw_pp(use_gam = TRUE)
#' }
#'
#' @family tte_classes
#' @seealso [tte_enrollment()] for creating trial objects, [TTEDesign] for design class
#' @export
TTEEnrollment <- R6::R6Class("TTEEnrollment",
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
    initialize = function(data, design, data_level = "trial",
                          steps_completed = character(),
                          active_outcome = NULL,
                          weight_cols = character()) {
      # Validation
      if (!data.table::is.data.table(data)) {
        stop("data must be a data.table")
      }
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
      if (!design$exposure_var %in% names(data)) {
        stop(paste("Missing required column:", design$exposure_var))
      }
      if (!is.null(active_outcome) &&
          !active_outcome %in% design$outcome_vars) {
        stop("active_outcome must be one of design$outcome_vars")
      }

      self$data <- data
      self$design <- design
      self$data_level <- data_level
      self$steps_completed <- steps_completed
      self$active_outcome <- active_outcome
      self$weight_cols <- weight_cols
    },

    #' @description Print the TTEEnrollment object.
    print = function(...) {
      cat("<TTEEnrollment>\n")
      cat("  Data level:", self$data_level, "\n")
      cat("  Design:", self$design$id_var, "~", self$design$exposure_var, "\n")
      cat("  Outcomes:", paste(self$design$outcome_vars, collapse = ", "), "\n")
      cat("  Data:", format(nrow(self$data), big.mark = ","), "rows x",
          ncol(self$data), "cols\n")
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

    # =========================================================================
    # Mutating methods (return invisible(self))
    # =========================================================================

    #' @description Enroll participants into trials with matching and panel expansion.
    #' Combines sampling (matching unexposed to exposed) and panel expansion into
    #' a single step. Transitions `data_level` from "person_week" to "trial".
    #' @param ratio Numeric, default 2. Sampling ratio for unexposed:exposed.
    #' @param seed Integer or NULL. Random seed for reproducibility.
    #' @param extra_cols Character vector of additional columns to include.
    enroll = function(ratio = 2, seed = NULL, extra_cols = NULL) {
      if (self$data_level != "person_week") {
        stop(
          "enroll() requires person_week level data.\n",
          "Current data_level: '", self$data_level, "'\n",
          "Hint: Use tte_enrollment(data, design) with person_id_var in design."
        )
      }

      design <- self$design
      data <- self$data

      # Step 1: Match (sample unexposed at specified ratio)
      data <- tte_match_ratio(
        data = data,
        exposure_var = design$exposure_var,
        eligible_var = design$eligible_var,
        ratio = ratio,
        id_var = design$person_id_var,
        seed = seed,
        mark_unsampled = "na"
      )

      data[,
        to_include := get(design$eligible_var) == TRUE &
          !is.na(get(design$exposure_var))
      ]

      # Step 2: Expand to trial panels
      panel_cols <- unique(c(
        design$person_id_var,
        design$confounder_vars,
        design$outcome_vars,
        if (!is.null(design$time_exposure_var)) design$time_exposure_var,
        extra_cols
      ))
      panel_cols <- intersect(panel_cols, names(data))

      data[, .tte_row_id := .I]
      entry_rows <- which(data$to_include == TRUE)
      if (length(entry_rows) == 0) {
        stop("No eligible entry rows found (to_include == TRUE).")
      }

      person_id_col <- design$person_id_var
      exposure_col <- design$exposure_var
      follow_up <- design$follow_up_time

      # Vectorized panel expansion using non-equi join
      entry_dt <- data[to_include == TRUE, .(
        .tte_person_id = get(person_id_col),
        entry_row_id = .tte_row_id,
        baseline_exp = get(exposure_col)
      )]
      entry_dt[, `:=`(
        trial_id = paste0(.tte_person_id, ".", entry_row_id),
        max_row_id = entry_row_id + follow_up - 1L
      )]

      join_cols <- unique(c(person_id_col, ".tte_row_id", panel_cols))
      data_slim <- data[, .SD, .SDcols = join_cols]
      data.table::setnames(data_slim, person_id_col, ".tte_person_id")
      data.table::setkeyv(data_slim, c(".tte_person_id", ".tte_row_id"))
      data.table::setkeyv(entry_dt, c(".tte_person_id", "entry_row_id"))

      panel <- data_slim[
        entry_dt,
        on = .(.tte_person_id, .tte_row_id >= entry_row_id, .tte_row_id <= max_row_id),
        nomatch = NULL,
        allow.cartesian = TRUE
      ]

      cols_to_remove <- grep(
        "^\\.tte_row_id|^entry_row_id$|^max_row_id$",
        names(panel),
        value = TRUE
      )
      if (length(cols_to_remove) > 0) {
        panel[, (cols_to_remove) := NULL]
      }

      data.table::setnames(panel, ".tte_person_id", person_id_col)
      panel[, (exposure_col) := baseline_exp]
      panel[, baseline_exp := NULL]
      panel[, trial_week := seq_len(.N) - 1L, by = trial_id]
      data[, .tte_row_id := NULL]

      self$data <- panel
      self$data_level <- "trial"
      self$steps_completed <- c(self$steps_completed, "enroll")
      invisible(self)
    },

    #' @description Collapse time intervals to coarser periods.
    #' Wraps [tte_collapse_periods()].
    #' @param period_width Integer, default 4.
    #' @param time_var Character or NULL; defaults to "trial_week" if present.
    #' @param first_cols Character vector of additional first-aggregation columns.
    #' @param last_cols Character vector of additional last-aggregation columns.
    #' @param max_cols Character vector of additional max-aggregation columns.
    #' @param sum_cols Character vector of additional sum-aggregation columns.
    collapse = function(
        period_width = 4L,
        time_var = NULL,
        first_cols = NULL,
        last_cols = NULL,
        max_cols = NULL,
        sum_cols = NULL
    ) {
      if (self$data_level != "trial") {
        stop(
          "collapse() requires trial level data.\n",
          "Current data_level: '", self$data_level, "'\n",
          "Hint: Run $enroll() first to convert person_week data to trial level."
        )
      }

      design <- self$design

      if (is.null(time_var)) {
        if ("trial_week" %in% names(self$data)) {
          time_var <- "trial_week"
        } else {
          time_var <- design$tstop_var
        }
      }

      design_first_cols <- c(design$confounder_vars, design$exposure_var)
      if (!is.null(design$person_id_var)) {
        design_first_cols <- c(design_first_cols, design$person_id_var)
      }
      if (!is.null(design$admin_censor_isoyearweek) && "isoyearweek" %in% names(self$data)) {
        design_first_cols <- c(design_first_cols, "isoyearweek")
      }
      all_first <- unique(c(design_first_cols, first_cols))

      all_last <- last_cols
      if (!is.null(design$time_exposure_var)) {
        all_last <- unique(c(design$time_exposure_var, last_cols))
      }

      all_max <- unique(c(design$outcome_vars, max_cols))

      all_first <- intersect(all_first, names(self$data))
      all_last <- intersect(all_last, names(self$data))
      all_max <- intersect(all_max, names(self$data))
      sum_cols <- intersect(sum_cols, names(self$data))

      self$data <- tte_collapse_periods(
        data = self$data,
        id_var = design$id_var,
        time_var = time_var,
        period_width = as.integer(period_width),
        first_cols = if (length(all_first) > 0) all_first else NULL,
        last_cols = if (length(all_last) > 0) all_last else NULL,
        max_cols = if (length(all_max) > 0) all_max else NULL,
        sum_cols = if (length(sum_cols) > 0) sum_cols else NULL
      )

      self$data[, person_weeks := get(design$tstop_var) - get(design$tstart_var)]

      self$steps_completed <- c(self$steps_completed, "collapse")
      invisible(self)
    },

    #' @description Calculate inverse probability of treatment weights.
    #' Wraps [tte_calculate_ipw()].
    #' @param stabilize Logical, default TRUE.
    ipw = function(stabilize = TRUE) {
      if (self$data_level != "trial") {
        stop(
          "ipw() requires trial level data.\n",
          "Current data_level: '", self$data_level, "'\n",
          "Hint: Run $enroll() first to convert person_week data to trial level."
        )
      }

      design <- self$design

      baseline <- self$data[get(design$tstart_var) == 0]
      baseline <- tte_calculate_ipw(
        data = baseline,
        exposure_var = design$exposure_var,
        confounder_vars = design$confounder_vars,
        id_var = design$id_var,
        stabilize = stabilize
      )

      data.table::setkeyv(baseline, design$id_var)
      self$data[baseline, `:=`(ps = i.ps, ipw = i.ipw), on = design$id_var]

      self$weight_cols <- unique(c(self$weight_cols, "ipw"))
      self$steps_completed <- c(self$steps_completed, "ipw")
      invisible(self)
    },

    #' @description Calculate IPCW for per-protocol analysis.
    #' Also combines weights (ipw * ipcw_pp), truncates, and drops
    #' intermediate IPCW columns. Wraps [tte_calculate_ipcw()].
    #' @param separate_by_exposure Logical, default TRUE.
    #' @param use_gam Logical, default TRUE.
    #' @param censoring_var Character or NULL. If NULL, auto-detected.
    ipcw_pp = function(
        separate_by_exposure = TRUE,
        use_gam = TRUE,
        censoring_var = NULL
    ) {
      if (self$data_level != "trial") {
        stop(
          "ipcw_pp() requires trial level data.\n",
          "Current data_level: '", self$data_level, "'\n",
          "Hint: Run $enroll() first to convert person_week data to trial level."
        )
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
        stop("censoring_var '", censoring_var, "' not found. Run $prepare_outcome() first.")
      }

      working_data <- self$data[!is.na(get(design$exposure_var))]

      working_data <- tte_calculate_ipcw(
        data = working_data,
        exposure_var = design$exposure_var,
        censoring_var = censoring_var,
        confounder_vars = design$confounder_vars,
        id_var = design$id_var,
        tstart_var = design$tstart_var,
        tstop_var = design$tstop_var,
        separate_by_exposure = separate_by_exposure,
        use_gam = use_gam
      )

      ipcw_value_cols <- intersect(
        c("p_censor", "p_uncensored", "cum_p_uncensored",
          "marginal_p", "cum_marginal", "ipcw_pp"),
        names(working_data)
      )

      for (col in ipcw_value_cols) {
        if (col %in% names(self$data)) self$data[, (col) := NULL]
      }

      join_on <- c(design$id_var, design$tstop_var)
      self$data[working_data, (ipcw_value_cols) := mget(paste0("i.", ipcw_value_cols)), on = join_on]

      rm(working_data)

      if (!"ipw" %in% names(self$data)) {
        stop("ipcw_pp() requires 'ipw' column. Run $ipw() first.")
      }
      self$data[, analysis_weight_pp := ipw * ipcw_pp]

      bounds <- stats::quantile(self$data$analysis_weight_pp, c(0.01, 0.99), na.rm = TRUE)
      self$data[, analysis_weight_pp_trunc := pmin(pmax(analysis_weight_pp, bounds[1]), bounds[2])]

      self$weight_cols <- unique(c(
        self$weight_cols, "ipcw_pp", "analysis_weight_pp", "analysis_weight_pp_trunc"
      ))
      self$steps_completed <- c(self$steps_completed, "ipcw", "weights", "truncate")

      drop_cols <- intersect(
        c("p_censor", "p_uncensored", "cum_p_uncensored",
          "marginal_p", "cum_marginal"),
        names(self$data)
      )
      if (length(drop_cols) > 0) self$data[, (drop_cols) := NULL]

      invisible(self)
    },

    #' @description Combine IPW and IPCW weights.
    #' Wraps [tte_combine_weights()].
    #' @param ipw_col Character, default "ipw".
    #' @param ipcw_col Character, default "ipcw_pp".
    #' @param name Character, default "analysis_weight_pp".
    combine_weights = function(
        ipw_col = "ipw",
        ipcw_col = "ipcw_pp",
        name = "analysis_weight_pp"
    ) {
      if (self$data_level != "trial") {
        stop(
          "combine_weights() requires trial level data.\n",
          "Current data_level: '", self$data_level, "'\n",
          "Hint: Run $enroll() first to convert person_week data to trial level."
        )
      }

      self$data <- tte_combine_weights(
        data = self$data,
        ipw_col = ipw_col,
        ipcw_col = ipcw_col,
        output_col = name
      )

      self$weight_cols <- unique(c(self$weight_cols, name))
      self$steps_completed <- c(self$steps_completed, "weights")
      invisible(self)
    },

    #' @description Truncate extreme weights.
    #' Wraps [tte_truncate_weights()].
    #' @param weight_cols Character vector or NULL.
    #' @param lower Numeric, default 0.01.
    #' @param upper Numeric, default 0.99.
    #' @param suffix Character, default "_trunc".
    truncate = function(
        weight_cols = NULL,
        lower = 0.01,
        upper = 0.99,
        suffix = "_trunc"
    ) {
      if (self$data_level != "trial") {
        stop(
          "truncate() requires trial level data.\n",
          "Current data_level: '", self$data_level, "'\n",
          "Hint: Run $enroll() first to convert person_week data to trial level."
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

      self$data <- tte_truncate_weights(
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

    #' @description Prepare outcome-specific data for per-protocol analysis.
    #' Computes event times, censoring times, filters data, creates indicators.
    #' Can only be run once per trial object (it deletes rows).
    #' @param outcome Character scalar. Must be one of `design$outcome_vars`.
    #' @param follow_up Optional integer. Overrides `design$follow_up_time`.
    prepare_outcome = function(outcome, follow_up = NULL) {
      if (self$data_level != "trial") {
        stop(
          "prepare_outcome() requires trial level data.\n",
          "Current data_level: '", self$data_level, "'\n",
          "Hint: Run $enroll() first to convert person_week data to trial level."
        )
      }

      if ("prepare_outcome" %in% self$steps_completed) {
        stop("prepare_outcome() can only be run once per trial (it deletes rows)")
      }

      design <- self$design
      data <- self$data

      if (!outcome %in% design$outcome_vars) {
        stop("outcome must be one of: ", paste(design$outcome_vars, collapse = ", "))
      }

      self$active_outcome <- outcome

      # weeks_to_event
      data[, weeks_to_event := {
        event_rows <- which(get(outcome) == 1)
        if (length(event_rows) > 0) {
          min(get(design$tstop_var)[event_rows])
        } else {
          NA_integer_
        }
      }, by = c(design$id_var)]

      # weeks_to_protocol_deviation
      if (is.null(design$time_exposure_var)) {
        stop("design must have time_exposure_var for per-protocol censoring analysis")
      }

      data[, .protocol_deviated := data.table::fcase(
        get(design$exposure_var) == TRUE &
          (get(design$time_exposure_var) == FALSE | is.na(get(design$time_exposure_var))),
        TRUE,
        get(design$exposure_var) == FALSE &
          (get(design$time_exposure_var) == TRUE | is.na(get(design$time_exposure_var))),
        TRUE,
        default = FALSE
      )]
      data[, weeks_to_protocol_deviation := {
        if (any(.protocol_deviated)) {
          min(get(design$tstop_var)[.protocol_deviated])
        } else {
          NA_integer_
        }
      }, by = c(design$id_var)]

      # weeks_to_admin_end
      if (!is.null(design$admin_censor_isoyearweek)) {
        if (!"isoyearweek" %in% names(data)) {
          stop("admin_censor_isoyearweek requires 'isoyearweek' column in data")
        }
        study_end_date <- cstime::isoyearweek_to_last_date(
          design$admin_censor_isoyearweek
        )
        data[, .baseline_isoyearweek := isoyearweek[get(design$tstart_var) == 0][1],
             by = c(design$id_var)]
        data[, weeks_to_admin_end := as.integer(difftime(
          study_end_date,
          cstime::isoyearweek_to_last_date(.baseline_isoyearweek),
          units = "weeks"
        ))]
        data[, .baseline_isoyearweek := NULL]

        period_width <- data[, min(get(design$tstop_var))]
        data[, weeks_to_admin_end := (weeks_to_admin_end %/% period_width) * period_width]

        n_dropped <- data[weeks_to_admin_end < period_width, uniqueN(get(design$id_var))]
        if (n_dropped > 0) {
          warning(
            n_dropped, " trial(s) will be dropped (entered < ", period_width,
            " weeks before admin_censor_isoyearweek)"
          )
        }
      } else {
        data[, weeks_to_admin_end := NA_integer_]
      }

      # weeks_to_loss
      effective_follow_up <- if (!is.null(follow_up)) as.integer(follow_up) else design$follow_up_time
      data[, .max_tstop := max(get(design$tstop_var)), by = c(design$id_var)]
      data[, .first_planned_stop := pmin(
        weeks_to_event,
        weeks_to_protocol_deviation,
        weeks_to_admin_end,
        effective_follow_up,
        na.rm = TRUE
      )]
      data[, weeks_to_loss := data.table::fifelse(
        .max_tstop < .first_planned_stop,
        .max_tstop,
        NA_integer_
      )]

      # censor_week
      data[, censor_week := pmin(
        weeks_to_event,
        weeks_to_protocol_deviation,
        weeks_to_loss,
        weeks_to_admin_end,
        effective_follow_up,
        na.rm = TRUE
      )]

      # Filter
      data <- data[get(design$tstop_var) <= censor_week | is.na(censor_week)]

      # event indicator
      data[, event := as.integer(get(design$tstop_var) == weeks_to_event)]
      data[is.na(event), event := 0L]

      # censor_this_period indicator
      data[, censor_this_period := as.integer(
        get(design$tstop_var) == weeks_to_protocol_deviation |
          get(design$tstop_var) == weeks_to_loss
      )]
      data[is.na(censor_this_period), censor_this_period := 0L]

      # Clean up
      data[, c(".max_tstop", ".first_planned_stop", ".protocol_deviated") := NULL]
      data.table::setorderv(data, c(design$id_var, design$tstop_var))

      self$data <- data
      self$steps_completed <- c(self$steps_completed, "prepare_outcome")
      invisible(self)
    },

    #' @description Impute missing confounders by sampling from observed values.
    #' @param confounder_vars Character vector of confounder column names to impute.
    #' @param seed Integer seed for reproducibility (default: 4L).
    impute_confounders = function(confounder_vars, seed = 4L) {
      trial_level <- self$data[,
        lapply(.SD, data.table::first),
        by = trial_id,
        .SDcols = confounder_vars
      ]

      set.seed(seed)
      for (var in confounder_vars) {
        missing_trials <- trial_level[is.na(get(var)), trial_id]
        if (length(missing_trials) > 0) {
          observed_vals <- trial_level[!is.na(get(var)), get(var)]
          sampled_vals <- sample(observed_vals, length(missing_trials), replace = TRUE)
          trial_level[trial_id %in% missing_trials, (var) := sampled_vals]
        }
      }

      self$data[, (confounder_vars) := NULL]
      data.table::setkey(self$data, trial_id)
      data.table::setkey(trial_level, trial_id)
      self$data <- merge(
        self$data,
        trial_level[, .SD, .SDcols = c("trial_id", confounder_vars)],
        by = "trial_id",
        all.x = TRUE
      )

      invisible(self)
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
      cat("  Exposure:", self$design$exposure_var, "\n")
      cat("  Outcomes:", paste(self$design$outcome_vars, collapse = ", "), "\n")
      cat("  Follow-up:", self$design$follow_up_time, "time units\n\n")

      cat("Data:\n")
      cat("  Level:", self$data_level, "\n")
      cat("  Rows:", format(nrow(self$data), big.mark = ","), "\n")
      cat("  Columns:", ncol(self$data), "\n\n")

      cat("Steps completed:", paste(self$steps_completed, collapse = " -> "), "\n\n")

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
              col, mean(vals), stats::sd(vals), min(vals), max(vals)
            ))
          }
        }
      }

      invisible(self)
    },

    # =========================================================================
    # Non-mutating methods (return data)
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

      n_individuals <- if (!is.null(design$person_id_var) &&
                           design$person_id_var %in% names(data)) {
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
          parts <- c(parts, paste(format(person_weeks, big.mark = ","), "person-weeks"))
        }
        parts <- c(parts, paste(format(n_trials, big.mark = ","), "trials"))
        if (!is.na(n_individuals)) {
          parts <- c(parts, paste(format(n_individuals, big.mark = ","), "individuals"))
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

    #' @description Generate baseline characteristics table.
    #' Wraps [tableone::CreateTableOne()] or [tableone::svyCreateTableOne()].
    #' @param ipw_col Character or NULL. If specified, creates weighted table.
    #' @return A tableone object.
    table1 = function(ipw_col = NULL) {
      if (self$data_level != "trial") {
        stop(
          "table1() requires trial level data.\n",
          "Current data_level: '", self$data_level, "'\n",
          "Hint: Run $enroll() first to convert person_week data to trial level."
        )
      }

      design <- self$design
      baseline <- self$data[get(design$tstart_var) == 0]

      if (is.null(ipw_col)) {
        tableone::CreateTableOne(
          vars = design$confounder_vars,
          strata = design$exposure_var,
          data = baseline,
          includeNA = TRUE,
          addOverall = TRUE
        )
      } else {
        if (!ipw_col %in% names(baseline)) {
          stop("ipw_col '", ipw_col, "' not found in data")
        }
        svy_design <- survey::svydesign(
          ids = as.formula(paste0("~", design$person_id_var)),
          weights = as.formula(paste0("~", ipw_col)),
          data = baseline
        )
        tableone::svyCreateTableOne(
          vars = design$confounder_vars,
          strata = design$exposure_var,
          data = svy_design,
          includeNA = TRUE,
          addOverall = TRUE
        )
      }
    },

    #' @description Calculate events, person-years, and rates by exposure group.
    #' @param weight_col Character, required. Column name for weights.
    #' @return A data.table with events, person-years, and rates.
    rates = function(weight_col) {
      if (self$data_level != "trial") {
        stop(
          "rates() requires trial level data.\n",
          "Current data_level: '", self$data_level, "'"
        )
      }

      design <- self$design
      data <- self$data

      if (!weight_col %in% names(data)) {
        stop("weight_col '", weight_col, "' not found in data")
      }
      if (!"event" %in% names(data)) {
        stop("'event' column not found. Run $prepare_outcome() first.")
      }
      if (!"person_weeks" %in% names(data)) {
        stop("'person_weeks' column not found. Run $collapse() first.")
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
        by = c(design$exposure_var)
      ]
      data.table::setattr(result, "swereg_type", "rates")
      data.table::setattr(result, "exposure_var", design$exposure_var)
      result
    },

    #' @description Fit Poisson models and extract incidence rate ratios.
    #' @param weight_col Character, required. Column name for weights.
    #' @return A data.table with IRR estimates and confidence intervals.
    irr = function(weight_col) {
      if (self$data_level != "trial") {
        stop(
          "irr() requires trial level data.\n",
          "Current data_level: '", self$data_level, "'"
        )
      }

      design <- self$design
      data <- self$data

      if (!weight_col %in% names(data)) {
        stop("weight_col '", weight_col, "' not found in data")
      }
      if (!"event" %in% names(data)) {
        stop("'event' column not found. Run $prepare_outcome() first.")
      }
      if (!"person_weeks" %in% names(data)) {
        stop("'person_weeks' column not found. Run $collapse() first.")
      }

      svy_design <- survey::svydesign(
        ids = as.formula(paste0("~", design$person_id_var)),
        weights = as.formula(paste0("~", weight_col)),
        data = data
      )

      warn_const <- FALSE
      warn_flex <- FALSE
      exposure_coef <- paste0(design$exposure_var, "TRUE")

      formula_const <- stats::as.formula(paste0(
        "event ~ ", design$exposure_var, " + offset(log(person_weeks))"
      ))
      poisson_const <- withCallingHandlers(
        survey::svyglm(
          formula_const,
          design = svy_design,
          family = stats::quasipoisson()
        ),
        warning = function(w) {
          warn_const <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
      const_summary <- summary(poisson_const)$coefficients
      const_coef <- const_summary[exposure_coef, "Estimate"]
      const_se <- const_summary[exposure_coef, "Std. Error"]
      const_pvalue <- const_summary[exposure_coef, "Pr(>|t|)"]

      formula_flex <- stats::as.formula(paste0(
        "event ~ ", design$exposure_var,
        " + splines::ns(", design$tstop_var, ", df = 3)",
        " + offset(log(person_weeks))"
      ))
      poisson_flex <- withCallingHandlers(
        survey::svyglm(
          formula_flex,
          design = svy_design,
          family = stats::quasipoisson()
        ),
        warning = function(w) {
          warn_flex <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
      flex_summary <- summary(poisson_flex)$coefficients
      flex_coef <- flex_summary[exposure_coef, "Estimate"]
      flex_se <- flex_summary[exposure_coef, "Std. Error"]
      flex_pvalue <- flex_summary[exposure_coef, "Pr(>|t|)"]

      result <- data.table::data.table(
        IRR_const = exp(const_coef),
        IRR_const_lower = exp(const_coef - 1.96 * const_se),
        IRR_const_upper = exp(const_coef + 1.96 * const_se),
        IRR_const_pvalue = const_pvalue,
        IRR_flex = exp(flex_coef),
        IRR_flex_lower = exp(flex_coef - 1.96 * flex_se),
        IRR_flex_upper = exp(flex_coef + 1.96 * flex_se),
        IRR_flex_pvalue = flex_pvalue,
        warn_const = warn_const,
        warn_flex = warn_flex
      )
      data.table::setattr(result, "swereg_type", "irr")
      result
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
          "Current data_level: '", self$data_level, "'"
        )
      }

      design <- self$design
      data <- self$data

      if (!ipw_col %in% names(data)) {
        stop("ipw_col '", ipw_col, "' not found in data")
      }
      if (!"event" %in% names(data)) {
        stop("'event' column not found. Run $prepare_outcome() first.")
      }

      final <- data[, .SD[.N], by = c(design$id_var)]

      svy_design <- survey::svydesign(
        ids = as.formula(paste0("~", design$person_id_var)),
        weights = as.formula(paste0("~", ipw_col)),
        data = final
      )

      km_formula <- stats::as.formula(paste0(
        "survival::Surv(", design$tstop_var, ", event) ~ ", design$exposure_var
      ))

      km_fit <- survey::svykm(km_formula, design = svy_design)

      if (!is.null(save_path)) {
        strata_names <- names(km_fit)
        unexposed_name <- strata_names[grepl("FALSE", strata_names)]
        exposed_name <- strata_names[grepl("TRUE", strata_names)]
        if (length(unexposed_name) == 0) unexposed_name <- "FALSE"
        if (length(exposed_name) == 0) exposed_name <- "TRUE"

        df_unexposed <- data.frame(
          time = km_fit[[unexposed_name]]$time,
          surv = km_fit[[unexposed_name]]$surv,
          group = "Unexposed"
        )
        df_exposed <- data.frame(
          time = km_fit[[exposed_name]]$time,
          surv = km_fit[[exposed_name]]$surv,
          group = "Exposed"
        )
        df <- rbind(df_unexposed, df_exposed)

        q <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = surv, color = group)) +
          ggplot2::geom_step(linewidth = 1) +
          ggplot2::scale_color_manual(
            values = c("Unexposed" = "blue", "Exposed" = "red")
          ) +
          ggplot2::scale_y_continuous(limits = c(0.99, 1), labels = scales::percent) +
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
  )
)


# =============================================================================
# Constructor
# =============================================================================

#' Create a TTE enrollment object
#'
#' Constructor function for [TTEEnrollment] objects. Wraps enrollment data with a design
#' specification to enable fluent `$`-chaining.
#'
#' @param data A data.table containing the trial data.
#' @param design A [TTEDesign] object specifying column mappings.
#' @param data_level Character or NULL. If NULL (default), auto-detects based on
#'   which identifier column exists in data. "person_week" for pre-panel data
#'   (requires person_id_var), "trial" for post-panel data (requires id_var).
#'
#' @return A [TTEEnrollment] object.
#'
#' @examples
#' \dontrun{
#' # Trial-level data (auto-detected)
#' design <- tte_design(
#'   exposure_var = "exposed",
#'   outcome_vars = "death",
#'   confounder_vars = c("age", "sex"),
#'   follow_up_time = 52L
#' )
#' trial <- tte_enrollment(my_trial_data, design)
#' trial$
#'   collapse(period_width = 4)$
#'   ipw()
#' }
#'
#' @family tte_classes
#' @seealso [TTEEnrollment] for class details, [tte_design()] for creating designs
#' @export
tte_enrollment <- function(data, design, data_level = NULL) {
  # Make a copy to avoid modifying original
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  } else {
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
        "  - person_id_var ('", design$person_id_var, "') for person_week data, or\n",
        "  - id_var ('", design$id_var, "') for trial data"
      )
    }
  }

  TTEEnrollment$new(
    data = data,
    design = design,
    data_level = data_level
  )
}


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
#'   tte_enrollment(load_data(f), design)$enroll(ratio = 2)
#' })
#' combined <- tte_rbind(trials)
#' combined$collapse(period_width = 4)$ipw()
#' }
#'
#' @family tte_methods
#' @export
tte_rbind <- function(trials) {
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
        "First trial: '", data_level, "', trial ", i, ": '",
        trials[[i]]$data_level, "'"
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
tte_rates_combine <- function(results, slot, descriptions = NULL) {
  rates_list <- lapply(results, `[[`, slot)

  first_non_null <- Find(Negate(is.null), rates_list)
  exposure_col <- attr(first_non_null, "exposure_var")
  if (is.null(exposure_col)) {
    stop("results$*$", slot, " must be $rates() outputs (missing 'exposure_var' attribute)")
  }

  dt <- rbindlist(rates_list, idcol = "ett_id")
  dt[, exposed := fifelse(get(exposure_col), "Exposed", "Unexposed")]
  dt[, (exposure_col) := NULL]

  dt[, `:=`(
    events_weighted = format(round(events_weighted, 1), nsmall = 1),
    py_weighted = format(round(py_weighted, 0), big.mark = ","),
    rate_per_100000py = format(round(rate_per_100000py, 1), nsmall = 1)
  )]

  if (!is.null(descriptions)) {
    dt[, description := descriptions[ett_id]]
    cast_formula <- stats::as.formula("ett_id + description ~ exposed")
  } else {
    cast_formula <- stats::as.formula("ett_id ~ exposed")
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
tte_irr_combine <- function(results, slot, descriptions = NULL) {
  irr_list <- lapply(results, `[[`, slot)
  dt <- rbindlist(irr_list, idcol = "ett_id")

  warn_ids <- dt[warn_const == TRUE | warn_flex == TRUE, ett_id]
  if (length(warn_ids) > 0L) {
    message("Convergence warnings in: ", paste(warn_ids, collapse = ", "))
  }

  result <- dt[, .(
    ett_id,
    `IRR (constant)` = paste0(
      format(round(IRR_const, 2), nsmall = 2),
      fifelse(warn_const, "#", "")
    ),
    `95% CI (constant)` = paste0(
      format(round(IRR_const_lower, 2), nsmall = 2),
      "-",
      format(round(IRR_const_upper, 2), nsmall = 2)
    ),
    `p (constant)` = format.pval(IRR_const_pvalue, digits = 3),
    `IRR (flexible)` = paste0(
      format(round(IRR_flex, 2), nsmall = 2),
      fifelse(warn_flex, "#", "")
    ),
    `95% CI (flexible)` = paste0(
      format(round(IRR_flex_lower, 2), nsmall = 2),
      "-",
      format(round(IRR_flex_upper, 2), nsmall = 2)
    ),
    `p (flexible)` = format.pval(IRR_flex_pvalue, digits = 3)
  )]

  if (!is.null(descriptions)) {
    result[, description := descriptions[ett_id]]
    setcolorder(result, c("ett_id", "description"))
  }

  result
}
