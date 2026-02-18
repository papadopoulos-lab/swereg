# =============================================================================
# S7 Methods for Target Trial Emulation
# =============================================================================
# Methods for TTETrial objects that wrap existing TTE functions.
# All workflow methods return self for chaining.
# =============================================================================

# -----------------------------------------------------------------------------
# tte_enroll: Sample comparison group and create trial panels
# -----------------------------------------------------------------------------

#' Enroll participants into trials with matching and panel expansion
#'
#' S7 method that combines sampling (matching unexposed to exposed) and panel
#' expansion into a single step. This method transitions `data_level` from
#' "person_week" to "trial".
#'
#' @param trial A [TTETrial] object with person_week level data.
#' @param ... Method arguments:
#'   \describe{
#'     \item{ratio}{Numeric, default 2. Sampling ratio for unexposed:exposed.}
#'     \item{seed}{Integer or NULL. Random seed for reproducibility.}
#'     \item{extra_cols}{Character vector of additional columns to include
#'       beyond design variables (e.g., "isoyearweek").}
#'   }
#'
#' @return The modified [TTETrial] object with trial level data (for chaining).
#'
#' @details
#' The method performs two operations:
#'
#' **1. Matching (sampling)**
#' - Samples unexposed individuals at the specified ratio relative to exposed
#' - Uses `eligible_var` from design to identify eligible entries
#' - Marks sampled rows via `to_include` column
#'
#' **2. Panel expansion**
#' - For each `to_include == TRUE` row, extracts `follow_up_time` rows of follow-up
#' - Creates `trial_id` as `"<person_id>.<row_id>"`
#' - Carries forward the baseline exposure value
#' - Creates `trial_week` (0-indexed time within each trial)
#'
#' Columns included automatically from design:
#' - `person_id_var` (kept in panel for reference)
#' - `time_exposure_var` (e.g., "rd_exposed")
#' - `exposure_var` (baseline, carried forward)
#' - `confounder_vars`
#' - `outcome_vars`
#'
#' If you need to prefix trial_id with file info for batching, do it after
#' enrollment:
#' ```r
#' trial <- tte_trial(data, design) |>
#'   tte_enroll(ratio = 2, seed = 42)
#' trial@data[, trial_id := paste0(enrollment_id, ".", trial_id)]
#' ```
#'
#' @examples
#' \dontrun{
#' design <- tte_design(
#'   person_id_var = "id",
#'   exposure_var = "baseline_exposed",
#'   time_exposure_var = "rd_exposed",
#'   outcome_vars = c("death", "hosp"),
#'   confounder_vars = c("age", "sex"),
#'   follow_up_time = 156L,
#'   eligible_var = "eligible"
#' )
#'
#' trial <- tte_trial(person_week_data, design) |>
#'   tte_enroll(ratio = 2, seed = 42, extra_cols = "isoyearweek")
#' }
#'
#' @family tte_methods
#' @seealso [tte_match_ratio()] for the underlying sampling function,
#'   [tte_collapse()] for the next step in the workflow
#' @export
tte_enroll <- S7::new_generic("tte_enroll", "trial")

S7::method(tte_enroll, TTETrial) <- function(
    trial,
    ratio = 2,
    seed = NULL,
    extra_cols = NULL
) {
  # Guard: require person_week data
  if (trial@data_level != "person_week") {
    stop(
      "tte_enroll() requires person_week level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Use tte_trial(data, design) with person_id_var in design."
    )
  }

  design <- trial@design
  data <- trial@data

  # ===========================================================================
  # Step 1: Match (sample unexposed at specified ratio)
  # ===========================================================================
  data <- tte_match_ratio(
    data = data,
    exposure_var = design@exposure_var,
    eligible_var = design@eligible_var,
    ratio = ratio,
    id_var = design@person_id_var,
    seed = seed,
    mark_unsampled = "na"
  )

  # Add to_include column to mark eligible entries for expansion
  data[,
    to_include := get(design@eligible_var) == TRUE &
      !is.na(get(design@exposure_var))
  ]

  # ===========================================================================
  # Step 2: Expand to trial panels
  # ===========================================================================

  # Columns to include in panel (from design + extra)
  panel_cols <- unique(c(
    design@person_id_var,
    design@confounder_vars,
    design@outcome_vars,
    if (length(design@time_exposure_var) > 0) design@time_exposure_var,
    extra_cols
  ))

  # Filter to columns that exist
  panel_cols <- intersect(panel_cols, names(data))

  # Create row_id for tracking
  data[, .tte_row_id := .I]

  # Find eligible entry rows
  entry_rows <- which(data$to_include == TRUE)

  if (length(entry_rows) == 0) {
    stop("No eligible entry rows found (to_include == TRUE).")
  }

  # Get column names and follow-up time from design
  person_id_col <- design@person_id_var
  exposure_col <- design@exposure_var
  follow_up <- design@follow_up_time

  # ===========================================================================

  # Vectorized panel expansion using non-equi join
  # ===========================================================================

  # Step 2a: Create entry metadata table with trial info
  entry_dt <- data[to_include == TRUE, .(
    .tte_person_id = get(person_id_col),
    entry_row_id = .tte_row_id,
    baseline_exp = get(exposure_col)
  )]
  entry_dt[, `:=`(
    trial_id = paste0(.tte_person_id, ".", entry_row_id),
    max_row_id = entry_row_id + follow_up - 1L
  )]

  # Step 2b: Prepare data for join (select only needed columns)
  # Include person_id and row_id for the join, plus panel columns
  join_cols <- unique(c(person_id_col, ".tte_row_id", panel_cols))
  data_slim <- data[, .SD, .SDcols = join_cols]

  # Rename person_id column to internal name for join
  data.table::setnames(data_slim, person_id_col, ".tte_person_id")

  # Set key for efficient join
  data.table::setkeyv(data_slim, c(".tte_person_id", ".tte_row_id"))
  data.table::setkeyv(entry_dt, c(".tte_person_id", "entry_row_id"))

  # Step 2c: Non-equi join to expand all panels at once
  # Join condition: same person, row_id within [entry_row_id, max_row_id]
  # In data.table non-equi join x[i, on=...], result has x columns + i columns
  panel <- data_slim[
    entry_dt,
    on = .(.tte_person_id, .tte_row_id >= entry_row_id, .tte_row_id <= max_row_id),
    nomatch = NULL,
    allow.cartesian = TRUE
  ]

  # Step 2d: Clean up columns from join
  # Non-equi join creates extra columns:
  # - .tte_row_id contains entry_row_id (first inequality bound)
  # - .tte_row_id.1 contains max_row_id (second inequality bound)
  # Remove all join helper columns
  cols_to_remove <- grep(
    "^\\.tte_row_id|^entry_row_id$|^max_row_id$",
    names(panel),
    value = TRUE
  )
  if (length(cols_to_remove) > 0) {
    panel[, (cols_to_remove) := NULL]
  }

  # Rename .tte_person_id back to original name
  data.table::setnames(panel, ".tte_person_id", person_id_col)

  # Step 2e: Apply baseline exposure (overwrite current exposure with baseline)
  panel[, (exposure_col) := baseline_exp]
  panel[, baseline_exp := NULL]

  # Add trial week (0-indexed time within each trial)
  panel[, trial_week := seq_len(.N) - 1L, by = trial_id]

  # Clean up temporary columns from original data
  data[, .tte_row_id := NULL]

  # Update trial object
  trial@data <- panel
  trial@data_level <- "trial"  # Key transition!
  trial@steps_completed <- c(trial@steps_completed, "enroll")
  trial
}


# -----------------------------------------------------------------------------
# tte_collapse: Collapse time periods
# -----------------------------------------------------------------------------

#' Collapse time intervals to coarser periods
#'
#' S7 method that aggregates fine-grained longitudinal data into coarser
#' time periods. Wraps [tte_collapse_periods()].
#'
#' This method requires `data_level == "trial"`.
#'
#' Column aggregation is inferred from the design:
#' - `confounder_vars`, `exposure_var`, `person_id_var` -> first (baseline values)
#' - `time_exposure_var` -> last (current status)
#' - `outcome_vars` -> max (any event in period)
#' - `isoyearweek` -> first (auto-included when `admin_censor_isoyearweek` is set)
#'
#' The method also creates a `person_weeks` column (tstop - tstart).
#'
#' @param trial A [TTETrial] object with trial level data.
#' @param ... Method arguments: `period_width` (integer, default 4),
#'   `time_var` (character or NULL; defaults to "trial_week" if it exists from
#'   [tte_enroll()], otherwise uses `tstop_var`), `first_cols`, `last_cols`,
#'   `max_cols`, `sum_cols` (character vectors for additional aggregation columns).
#'
#' @return The modified [TTETrial] object (for chaining).
#'
#' @examples
#' \dontrun{
#' trial <- tte_trial(data, design) |>
#'   tte_collapse(period_width = 4)
#' }
#'
#' @family tte_methods
#' @seealso [tte_enroll()] for the preceding step,
#'   [tte_collapse_periods()] for the underlying function
#' @export
tte_collapse <- S7::new_generic("tte_collapse", "trial")

S7::method(tte_collapse, TTETrial) <- function(
    trial,
    period_width = 4L,
    time_var = NULL,
    first_cols = NULL,
    last_cols = NULL,
    max_cols = NULL,
    sum_cols = NULL
) {
  # Guard: require trial level data
  if (trial@data_level != "trial") {
    stop(
      "tte_collapse() requires trial level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Run tte_enroll() first to convert person_week data to trial level."
    )
  }

  design <- trial@design

  # Use trial_week as time_var if it exists (from tte_enroll), else tstop_var
  if (is.null(time_var)) {
    if ("trial_week" %in% names(trial@data)) {
      time_var <- "trial_week"
    } else {
      time_var <- design@tstop_var
    }
  }

  # Build aggregation columns from design
  # Confounders + baseline exposure -> first (constant within trial)
  # Also include person_id_var if set (for linking back to person-level data)
  # Also include isoyearweek if admin_censor_isoyearweek is set (needed for admin censoring)
  design_first_cols <- c(
    design@confounder_vars,
    design@exposure_var
  )
  if (length(design@person_id_var) > 0) {
    design_first_cols <- c(design_first_cols, design@person_id_var)
  }
  if (length(design@admin_censor_isoyearweek) > 0 && "isoyearweek" %in% names(trial@data)) {
    design_first_cols <- c(design_first_cols, "isoyearweek")
  }
  all_first <- unique(c(design_first_cols, first_cols))

  # Time-varying exposure -> last (current status)
  all_last <- last_cols
  if (length(design@time_exposure_var) > 0) {
    all_last <- unique(c(design@time_exposure_var, last_cols))
  }

  # Outcomes -> max (any event in period)
  all_max <- unique(c(design@outcome_vars, max_cols))

  # Filter to columns that exist in data
  all_first <- intersect(all_first, names(trial@data))
  all_last <- intersect(all_last, names(trial@data))
  all_max <- intersect(all_max, names(trial@data))
  sum_cols <- intersect(sum_cols, names(trial@data))

  trial@data <- tte_collapse_periods(
    data = trial@data,
    id_var = design@id_var,
    time_var = time_var,
    period_width = as.integer(period_width),
    first_cols = if (length(all_first) > 0) all_first else NULL,
    last_cols = if (length(all_last) > 0) all_last else NULL,
    max_cols = if (length(all_max) > 0) all_max else NULL,
    sum_cols = if (length(sum_cols) > 0) sum_cols else NULL
  )

  # Add person_weeks column (counting-process property)
  trial@data[, person_weeks := get(design@tstop_var) - get(design@tstart_var)]

  trial@steps_completed <- c(trial@steps_completed, "collapse")
  trial
}


# -----------------------------------------------------------------------------
# tte_ipw: Calculate inverse probability weights
# -----------------------------------------------------------------------------

#' Calculate inverse probability of treatment weights
#'
#' S7 method that calculates IPW for baseline confounding adjustment.
#' Wraps [tte_calculate_ipw()].
#'
#' This method requires `data_level == "trial"`.
#'
#' @param trial A [TTETrial] object with trial level data.
#' @param ... Method arguments: `stabilize` (logical, default TRUE).
#'
#' @return The modified [TTETrial] object (for chaining).
#'
#' @details
#' **Problem:** Exposed and unexposed groups may differ in baseline
#' characteristics (e.g., age, education) that also affect the outcome.
#' This confounds the treatment-outcome relationship.
#'
#' **Solution:** Reweight the sample so that confounders are balanced across
#' groups. People who are unlikely to be exposed (given their characteristics)
#' but ARE exposed get upweighted; people likely to be exposed who ARE exposed
#' get downweighted. This creates a "pseudo-population" where treatment is
#' independent of confounders.
#'
#' **Notation:**
#' \itemize{
#'   \item A = treatment (exposed vs unexposed)
#'   \item L = confounders (age, education, etc.)
#'   \item P(A) = marginal probability of treatment (overall % exposed)
#'   \item P(A|L) = probability of treatment given confounders (propensity score)
#' }
#'
#' **Stabilized weights:**
#' \itemize{
#'   \item Exposed: P(A) / P(A|L) - upweight if unlikely to be exposed
#'   \item Unexposed: (1-P(A)) / (1-P(A|L)) - upweight if likely to be exposed
#' }
#'
#' Unlike IPCW-PP (see [tte_ipcw_pp()]), the marginal probability here is computed
#' across everyone (not within exposure groups), because we're modeling
#' treatment assignment, not censoring.
#'
#' IPW is calculated at baseline (tstart == 0) and then merged back to all
#' time periods. This ensures baseline confounding adjustment is applied
#' consistently across follow-up.
#'
#' @examples
#' \dontrun{
#' trial <- tte_trial(data, design) |>
#'   tte_collapse(period_width = 4) |>
#'   tte_ipw(stabilize = TRUE)
#' }
#'
#' @family tte_methods
#' @seealso [tte_calculate_ipw()] for the underlying function
#' @export
tte_ipw <- S7::new_generic("tte_ipw", "trial")

S7::method(tte_ipw, TTETrial) <- function(trial, stabilize = TRUE) {
  # Guard: require trial level data
  if (trial@data_level != "trial") {
    stop(
      "tte_ipw() requires trial level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Run tte_enroll() first to convert person_week data to trial level."
    )
  }

  design <- trial@design

  # Get baseline data (tstart == 0)
  baseline <- trial@data[get(design@tstart_var) == 0]

  # Calculate IPW on baseline
  baseline <- tte_calculate_ipw(
    data = baseline,
    exposure_var = design@exposure_var,
    confounder_vars = design@confounder_vars,
    id_var = design@id_var,
    stabilize = stabilize
  )

  # Merge IPW back to all time periods
  ipw_cols <- c(design@id_var, "ps", "ipw")
  trial@data <- merge(
    trial@data,
    baseline[, ..ipw_cols],
    by = design@id_var,
    all.x = TRUE
  )

  trial@weight_cols <- unique(c(trial@weight_cols, "ipw"))
  trial@steps_completed <- c(trial@steps_completed, "ipw")
  trial
}


# -----------------------------------------------------------------------------
# tte_rbind: Combine batched trials
# -----------------------------------------------------------------------------

#' Combine multiple trial objects
#'
#' Combines multiple [TTETrial] objects by row-binding their data. Used for
#' batched processing where data is too large to fit in memory at once.
#'
#' @param trials A list of [TTETrial] objects to combine.
#'
#' @return A new [TTETrial] object with combined data.
#'
#' @details
#' All trials must have the same design and data_level. The combined trial inherits:
#' - The design and data_level from the first trial
#' - The intersection of steps_completed from all trials
#' - The union of weight_cols from all trials
#'
#' @examples
#' \dontrun{
#' # Process batches through enrollment
#' trials <- lapply(files, function(f) {
#'   tte_trial(load_data(f), design) |>
#'     tte_enroll(ratio = 2)
#' })
#'
#' # Combine for collapse and weighting
#' combined <- tte_rbind(trials) |>
#'   tte_collapse(period_width = 4) |>
#'   tte_ipw()
#' }
#'
#' @family tte_methods
#' @export
tte_rbind <- function(trials) {
  if (!is.list(trials) || length(trials) == 0) {
    stop("trials must be a non-empty list")
  }

  # Validate all are TTETrial
  for (i in seq_along(trials)) {
    if (!S7::S7_inherits(trials[[i]], TTETrial)) {
      stop("All elements must be TTETrial objects")
    }
  }

  # Validate all have same data_level
  data_level <- trials[[1]]@data_level
  for (i in seq_along(trials)[-1]) {
    if (trials[[i]]@data_level != data_level) {
      stop(
        "All trials must have the same data_level.\n",
        "First trial: '", data_level, "', trial ", i, ": '",
        trials[[i]]@data_level, "'"
      )
    }
  }

  # Use first trial's design
  design <- trials[[1]]@design

  # Combine data
  combined_data <- data.table::rbindlist(
    lapply(trials, function(t) t@data),
    use.names = TRUE,
    fill = TRUE
  )

  # Intersect steps (only keep steps done by all)
  steps <- trials[[1]]@steps_completed
  for (t in trials[-1]) {
    steps <- intersect(steps, t@steps_completed)
  }

  # Union of weight columns
  weight_cols <- unique(unlist(lapply(trials, function(t) t@weight_cols)))

  TTETrial(
    data = combined_data,
    design = design,
    data_level = data_level,
    steps_completed = steps,
    weight_cols = weight_cols
  )
}


# -----------------------------------------------------------------------------
# tte_ipcw_pp: Calculate inverse probability of censoring weights (per-protocol)
# -----------------------------------------------------------------------------

#' Calculate inverse probability of censoring weights for per-protocol analysis
#'
#' S7 method that calculates IPCW-PP for informative censoring adjustment in
#' per-protocol analysis. Wraps [tte_calculate_ipcw()].
#'
#' This method requires `data_level == "trial"`.
#'
#' @param trial A [TTETrial] object with trial level data. Must have a censoring
#'   indicator column from [tte_prepare_outcome()].
#' @param ... Method arguments: `separate_by_exposure` (logical, default TRUE),
#'   `use_gam` (logical, default TRUE), `censoring_var` (character or NULL).
#'   If `censoring_var` is NULL (default), uses "censor_this_period" from
#'   [tte_prepare_outcome()].
#'
#' @return The modified [TTETrial] object (for chaining).
#'
#' @details
#' **Problem:** In a per-protocol analysis, we censor people when they deviate
#' from their assigned treatment or are lost to follow-up (death/emigration).
#' Both types of censoring may be related to prognosis - sicker people might be
#' more likely to start treatment or to die. This "informative censoring" biases
#' results.
#'
#' **Solution:** At each time point, model the probability of being censored
#' given confounders. Upweight people who were likely to be censored but weren't,
#' creating a pseudo-population where censoring is independent of confounders.
#'
#' **How it works (at each time period, separately for exposed/unexposed):**
#' \enumerate{
#'   \item Conditional probability: P(remain | confounders) - predicted from model
#'   \item Marginal probability: P(remain) - average within exposure group at that time
#'   \item Cumulative conditional = product of conditional probs up to this time
#'     (person's predicted probability of still being in study)
#'   \item Cumulative marginal = product of marginal probs up to this time
#'     (expected if censoring depended only on time and exposure, not confounders)
#'   \item Stabilized IPCW-PP = cumulative marginal / cumulative conditional
#'     (upweights people who "should have" been censored but weren't)
#' }
#'
#' Note: This function must be run separately for each outcome because censoring
#' times differ by outcome (the event stops follow-up).
#'
#' We fit separate models for exposed and unexposed because the hazard of
#' censoring (protocol deviation + loss to follow-up) may differ by treatment.
#' For example, unexposed individuals may be more likely to start treatment
#' if they develop symptoms, while exposed may be more likely to discontinue
#' due to side effects. GAM with smooth term s(tstop) allows flexible,
#' non-parametric time trends in the hazard of censoring.
#'
#' @examples
#' \dontrun{
#' trial <- trial |>
#'   tte_prepare_outcome(outcome = "death") |>
#'   tte_ipcw_pp(use_gam = TRUE)
#' }
#'
#' @family tte_methods
#' @seealso [tte_calculate_ipcw()] for the underlying function,
#'   [tte_prepare_outcome()] for outcome-specific preparation,
#'   [tte_ipw()] for baseline confounding adjustment
#' @export
tte_ipcw_pp <- S7::new_generic("tte_ipcw_pp", "trial")

S7::method(tte_ipcw_pp, TTETrial) <- function(
    trial,
    separate_by_exposure = TRUE,
    use_gam = TRUE,
    censoring_var = NULL
) {
  # Guard: require trial level data
  if (trial@data_level != "trial") {
    stop(
      "tte_ipcw_pp() requires trial level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Run tte_enroll() first to convert person_week data to trial level."
    )
  }

 design <- trial@design

  # Auto-detect censoring_var based on workflow
  if (is.null(censoring_var)) {
    if ("prepare_outcome" %in% trial@steps_completed) {
      censoring_var <- "censor_this_period"
    } else {
      censoring_var <- "censored"
    }
  }

  if (!censoring_var %in% names(trial@data)) {
    stop("censoring_var '", censoring_var, "' not found. Run tte_prepare_outcome() first.")
  }

  # Work on rows that are not NA for exposure (sampled rows)
  working_data <- trial@data[!is.na(get(design@exposure_var))]

  working_data <- tte_calculate_ipcw(
    data = working_data,
    exposure_var = design@exposure_var,
    censoring_var = censoring_var,
    confounder_vars = design@confounder_vars,
    id_var = design@id_var,
    tstart_var = design@tstart_var,
    tstop_var = design@tstop_var,
    separate_by_exposure = separate_by_exposure,
    use_gam = use_gam
  )

  # Merge IPCW columns back
  ipcw_cols <- c(
    design@id_var, design@tstop_var,
    "p_censor", "p_uncensored", "cum_p_uncensored",
    "marginal_p", "cum_marginal", "ipcw_pp"
  )
  ipcw_cols <- intersect(ipcw_cols, names(working_data))

  # Remove old columns if they exist
  for (col in setdiff(ipcw_cols, c(design@id_var, design@tstop_var))) {
    if (col %in% names(trial@data)) {
      trial@data[, (col) := NULL]
    }
  }

  trial@data <- merge(
    trial@data,
    working_data[, ..ipcw_cols],
    by = c(design@id_var, design@tstop_var),
    all.x = TRUE
  )

  trial@weight_cols <- unique(c(trial@weight_cols, "ipcw_pp"))
  trial@steps_completed <- c(trial@steps_completed, "ipcw")
  trial
}


# -----------------------------------------------------------------------------
# tte_weights: Combine IPW and IPCW
# -----------------------------------------------------------------------------

#' Combine IPW and IPCW weights for per-protocol analysis
#'
#' S7 method that creates combined weights. Wraps [tte_combine_weights()].
#'
#' This method requires `data_level == "trial"`.
#'
#' @param trial A [TTETrial] object with trial level data, IPW and IPCW columns.
#' @param ... Method arguments: `ipw_col` (character, default "ipw"),
#'   `ipcw_col` (character, default "ipcw_pp"), `output_col` (character,
#'   default "analysis_weight_pp").
#'
#' @return The modified [TTETrial] object (for chaining).
#'
#' @examples
#' \dontrun{
#' trial <- trial |>
#'   tte_ipw() |>
#'   tte_ipcw_pp() |>
#'   tte_weights()
#' }
#'
#' @family tte_methods
#' @seealso [tte_combine_weights()] for the underlying function
#' @export
tte_weights <- S7::new_generic("tte_weights", "trial")

S7::method(tte_weights, TTETrial) <- function(
    trial,
    ipw_col = "ipw",
    ipcw_col = "ipcw_pp",
    output_col = "analysis_weight_pp"
) {
  # Guard: require trial level data
  if (trial@data_level != "trial") {
    stop(
      "tte_weights() requires trial level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Run tte_enroll() first to convert person_week data to trial level."
    )
  }

  trial@data <- tte_combine_weights(
    data = trial@data,
    ipw_col = ipw_col,
    ipcw_col = ipcw_col,
    output_col = output_col
  )

  trial@weight_cols <- unique(c(trial@weight_cols, output_col))
  trial@steps_completed <- c(trial@steps_completed, "weights")
  trial
}


# -----------------------------------------------------------------------------
# tte_truncate: Truncate extreme weights
# -----------------------------------------------------------------------------

#' Truncate extreme weights
#'
#' S7 method that truncates weights at specified quantiles.
#' Wraps [tte_truncate_weights()].
#'
#' This method requires `data_level == "trial"`.
#'
#' @param trial A [TTETrial] object with trial level data and weight columns.
#' @param ... Method arguments: `weight_cols` (character vector or NULL),
#'   `lower` (numeric, default 0.01), `upper` (numeric, default 0.99),
#'   `suffix` (character, default "_trunc").
#'
#' @return The modified [TTETrial] object (for chaining).
#'
#' @examples
#' \dontrun{
#' trial <- trial |>
#'   tte_weights() |>
#'   tte_truncate(lower = 0.01, upper = 0.99)
#' }
#'
#' @family tte_methods
#' @seealso [tte_truncate_weights()] for the underlying function
#' @export
tte_truncate <- S7::new_generic("tte_truncate", "trial")

S7::method(tte_truncate, TTETrial) <- function(
    trial,
    weight_cols = NULL,
    lower = 0.01,
    upper = 0.99,
    suffix = "_trunc"
) {
  # Guard: require trial level data
  if (trial@data_level != "trial") {
    stop(
      "tte_truncate() requires trial level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Run tte_enroll() first to convert person_week data to trial level."
    )
  }

  # Use tracked weight columns if not specified
  if (is.null(weight_cols)) {
    weight_cols <- trial@weight_cols
  }

  # Filter to columns that exist
  weight_cols <- intersect(weight_cols, names(trial@data))

  if (length(weight_cols) == 0) {
    warning("No weight columns to truncate")
    return(trial)
  }

  trial@data <- tte_truncate_weights(
    data = trial@data,
    weight_cols = weight_cols,
    lower = lower,
    upper = upper,
    suffix = suffix
  )

  # Track new truncated columns
  new_cols <- paste0(weight_cols, suffix)
  trial@weight_cols <- unique(c(trial@weight_cols, new_cols))
  trial@steps_completed <- c(trial@steps_completed, "truncate")
  trial
}


# -----------------------------------------------------------------------------
# tte_extract: Get the data.table
# -----------------------------------------------------------------------------

#' Extract the data.table from a trial object
#'
#' Returns the processed data.table from a [TTETrial] object.
#'
#' @param trial A [TTETrial] object.
#' @param ... Additional arguments (unused).
#'
#' @return A data.table with the processed trial data.
#'
#' @examples
#' \dontrun{
#' analysis_data <- trial |>
#'   tte_ipw() |>
#'   tte_extract()
#' }
#'
#' @family tte_methods
#' @export
tte_extract <- S7::new_generic("tte_extract", "trial")

S7::method(tte_extract, TTETrial) <- function(trial) {
  trial@data
}


# -----------------------------------------------------------------------------
# tte_summary: Data summary statistics
# -----------------------------------------------------------------------------

#' Summarize trial data statistics
#'
#' Returns key statistics about a [TTETrial] object: row count, person-weeks,
#' number of trials, individuals, events, and memory size.
#'
#' @param trial A [TTETrial] object.
#' @param ... Method arguments: `pretty` (logical, default FALSE). If TRUE,
#'   prints a formatted summary to the console instead of returning a list.
#'
#' @return If `pretty = FALSE` (default), returns a list with:
#' \describe{
#'   \item{n_rows}{Number of rows in the data}
#'   \item{person_weeks}{Total person-weeks (sum of person_weeks column, or NA if not available)}
#'   \item{n_trials}{Number of unique trials}
#'   \item{n_individuals}{Number of unique individuals (or NA if person_id_var not set)}
#'   \item{n_events}{Number of events (sum of event column, or NA if not available)}
#'   \item{size_mb}{Object size in megabytes}
#' }
#' If `pretty = TRUE`, prints formatted output and invisibly returns the list.
#'
#' @examples
#' \dontrun{
#' # Get statistics as a list
#' stats <- trial |> tte_summary()
#' cat("Rows:", stats$n_rows, "\n")
#'
#' # Print formatted summary
#' trial |> tte_summary(pretty = TRUE)
#' }
#'
#' @family tte_methods
#' @export
tte_summary <- S7::new_generic("tte_summary", "trial")

S7::method(tte_summary, TTETrial) <- function(trial, pretty = FALSE) {
  design <- trial@design
  data <- trial@data

  # Compute statistics
  n_rows <- nrow(data)

  person_weeks <- if ("person_weeks" %in% names(data)) {
    sum(data$person_weeks, na.rm = TRUE)
  } else {
    NA_real_
  }

  n_trials <- data.table::uniqueN(data[[design@id_var]])

  n_individuals <- if (length(design@person_id_var) > 0 &&
                       design@person_id_var %in% names(data)) {
    data.table::uniqueN(data[[design@person_id_var]])
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
}


# -----------------------------------------------------------------------------
# tte_weight_summary: Weight diagnostics
# -----------------------------------------------------------------------------

#' Summarize weight distributions
#'
#' Prints diagnostic summary of weight columns in a [TTETrial] object.
#'
#' @param trial A [TTETrial] object.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns the trial object.
#'
#' @examples
#' \dontrun{
#' trial |>
#'   tte_ipw() |>
#'   tte_ipcw_pp() |>
#'   tte_weight_summary()
#' }
#'
#' @family tte_methods
#' @export
tte_weight_summary <- S7::new_generic("tte_weight_summary", "trial")

S7::method(tte_weight_summary, TTETrial) <- function(trial) {
  cat("TTETrial Weight Summary\n")
  cat("=======================\n\n")

  cat("Design:\n")
  if (length(trial@design@person_id_var) > 0) {
    cat("  Person ID variable:", trial@design@person_id_var, "\n")
  }
  cat("  Trial ID variable:", trial@design@id_var, "\n")
  cat("  Exposure:", trial@design@exposure_var, "\n")
  cat("  Outcomes:", paste(trial@design@outcome_vars, collapse = ", "), "\n")
  cat("  Follow-up:", trial@design@follow_up_time, "time units\n\n")

  cat("Data:\n")
  cat("  Level:", trial@data_level, "\n")
  cat("  Rows:", format(nrow(trial@data), big.mark = ","), "\n")
  cat("  Columns:", ncol(trial@data), "\n\n")

  cat("Steps completed:", paste(trial@steps_completed, collapse = " -> "), "\n\n")

  if (length(trial@active_outcome) > 0) {
    cat("Active outcome:", trial@active_outcome, "\n\n")
  }

  weight_cols <- intersect(trial@weight_cols, names(trial@data))
  if (length(weight_cols) > 0) {
    cat("Weight distributions:\n")
    for (col in weight_cols) {
      vals <- trial@data[[col]]
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) {
        cat(sprintf(
          "  %s: mean=%.3f, sd=%.3f, min=%.3f, max=%.3f\n",
          col, mean(vals), stats::sd(vals), min(vals), max(vals)
        ))
      }
    }
  }

  invisible(trial)
}


# -----------------------------------------------------------------------------
# print method for TTETrial
# -----------------------------------------------------------------------------

#' @name print.TTETrial
#' @title Print method for TTETrial
#' @description Prints a summary of a TTETrial object.
#' @param x A [TTETrial] object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the object.
#' @keywords internal
S7::method(print, TTETrial) <- function(x, ...) {
  cat("<TTETrial>\n")
  cat("  Data level:", x@data_level, "\n")
  cat("  Design:", x@design@id_var, "~", x@design@exposure_var, "\n")
  cat("  Outcomes:", paste(x@design@outcome_vars, collapse = ", "), "\n")
  cat("  Data:", format(nrow(x@data), big.mark = ","), "rows x",
      ncol(x@data), "cols\n")
  if (length(x@steps_completed) > 0) {
    cat("  Steps:", paste(x@steps_completed, collapse = " -> "), "\n")
  }
  if (length(x@active_outcome) > 0) {
    cat("  Active outcome:", x@active_outcome, "\n")
  }
  if (length(x@weight_cols) > 0) {
    cat("  Weights:", paste(x@weight_cols, collapse = ", "), "\n")
  }
  invisible(x)
}


# -----------------------------------------------------------------------------
# print method for TTEDesign
# -----------------------------------------------------------------------------

#' @name print.TTEDesign
#' @title Print method for TTEDesign
#' @description Prints a summary of a TTEDesign object.
#' @param x A [TTEDesign] object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the object.
#' @keywords internal
S7::method(print, TTEDesign) <- function(x, ...) {
  cat("<TTEDesign>\n")
  if (length(x@person_id_var) > 0) {
    cat("  Person ID:", x@person_id_var, "\n")
  }
  cat("  Trial ID:", x@id_var, "\n")
  cat("  Exposure:", x@exposure_var, "\n")
  if (length(x@time_exposure_var) > 0) {
    cat("  Time-varying exposure:", x@time_exposure_var, "\n")
  }
  cat("  Outcomes:", paste(x@outcome_vars, collapse = ", "), "\n")
  cat("  Confounders:", paste(x@confounder_vars, collapse = ", "), "\n")
  cat("  Follow-up:", x@follow_up_time, "time units\n")
  cat("  Time vars:", x@tstart_var, "/", x@tstop_var, "\n")
  if (length(x@eligible_var) > 0) {
    cat("  Eligibility:", x@eligible_var, "\n")
  }
  invisible(x)
}


# -----------------------------------------------------------------------------
# TTEPlan methods
# -----------------------------------------------------------------------------

#' Extract a task from a TTE plan
#'
#' Returns a list with the TTEDesign object and metadata for the i-th
#' enrollment_id group. Design parameters (confounders, exposure columns) are
#' read from the per-ETT columns in the `ett` data.table. Useful for
#' interactive testing of the process_fn callback.
#'
#' @param plan A [TTEPlan] object.
#' @param i Integer index (1-based) into the unique enrollment_id groups.
#'
#' @return A list with components:
#' \describe{
#'   \item{design}{A [TTEDesign] object configured for this task}
#'   \item{enrollment_id}{The enrollment_id string for this task}
#'   \item{age_range}{Numeric vector of length 2 (min, max age)}
#'   \item{n_threads}{Number of threads (from [parallel::detectCores()])}
#' }
#'
#' @examples
#' \dontrun{
#' task <- tte_plan_task(plan, 1)
#' # or equivalently: task <- plan[[1]]
#' result <- process_fn(task, plan@skeleton_files[1])
#' }
#'
#' @family tte_classes
#' @seealso [TTEPlan], [tte_plan()]
#' @export
tte_plan_task <- function(plan, i = 1L) {
  enrollment_ids <- unique(plan@ett$enrollment_id)
  eid <- enrollment_ids[i]
  rows <- plan@ett[plan@ett$enrollment_id == eid]
  first <- rows[1]

  # Convert NA back to NULL for TTEDesign
  x_person_id <- first$person_id_var
  x_time_exp <- first$time_exposure_var
  if (is.na(x_time_exp)) x_time_exp <- NULL
  x_eligible <- first$eligible_var
  if (is.na(x_eligible)) x_eligible <- NULL

  list(
    design = tte_design(
      person_id_var = x_person_id,
      exposure_var = first$exposure_var,
      time_exposure_var = x_time_exp,
      eligible_var = x_eligible,
      outcome_vars = rows$outcome_var,
      confounder_vars = first$confounder_vars[[1]],
      follow_up_time = as.integer(first$follow_up),
      admin_censor_isoyearweek = plan@global_max_isoyearweek
    ),
    enrollment_id = eid,
    age_range = c(first$age_min, first$age_max),
    n_threads = parallel::detectCores()
  )
}

#' @name extract-TTEPlan
#' @title Extract task from TTEPlan
#' @description `plan[[i]]` extracts the i-th task. See [tte_plan_task()].
#' @param x A [TTEPlan] object.
#' @param i Integer index.
#' @keywords internal
S7::method(`[[`, TTEPlan) <- function(x, i) {
  tte_plan_task(x, i)
}

#' @name length-TTEPlan
#' @title Number of tasks in TTEPlan
#' @description Returns the number of unique enrollment_id groups.
#' @param x A [TTEPlan] object.
#' @keywords internal
S7::method(length, TTEPlan) <- function(x) {
  if (is.null(x@ett) || nrow(x@ett) == 0) return(0L)
  data.table::uniqueN(x@ett$enrollment_id)
}

#' @name print.TTEPlan
#' @title Print method for TTEPlan
#' @description Prints a summary of a TTEPlan object.
#' @param x A [TTEPlan] object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the object.
#' @keywords internal
S7::method(print, TTEPlan) <- function(x, ...) {
  cat("<TTEPlan>", x@project_prefix, "\n")
  if (is.null(x@ett) || nrow(x@ett) == 0) {
    cat("  ETTs: (none)\n")
  } else {
    n_enrollments <- length(x)
    n_etts <- nrow(x@ett)
    n_skeletons <- length(x@skeleton_files)

    cat(sprintf(
      "  %d enrollment(s) x %d skeleton files -> %d ETT(s)\n\n",
      n_enrollments, n_skeletons, n_etts
    ))

    # Enrollment grid
    enroll_grid <- x@ett[,
      .(
        follow_up = paste0(follow_up[1], "w"),
        n_ett = .N
      ),
      by = enrollment_id
    ]
    cat("  Enrollments:\n")
    print(enroll_grid, row.names = FALSE, class = FALSE)

    # ETT grid
    ett_grid <- x@ett[, .(
      ett_id,
      outcome_name = fifelse(
        nchar(outcome_name) > 45,
        paste0(substr(outcome_name, 1, 42), "..."),
        outcome_name
      ),
      follow_up = paste0(follow_up, "w"),
      enrollment_id
    )]
    cat("\n  ETTs:\n")
    print(ett_grid, row.names = FALSE, class = FALSE)
  }
  invisible(x)
}


# -----------------------------------------------------------------------------
# tte_prepare_outcome: Prepare outcome-specific data for IPCW analysis
# -----------------------------------------------------------------------------

#' Prepare outcome-specific data for per-protocol analysis
#'
#' S7 method that combines all outcome-specific preparation into one step:
#' computing event times, censoring times, filtering data, and creating
#' indicators for IPCW analysis.
#'
#' This method requires `data_level == "trial"` and can only be run once
#' per trial object because it deletes rows.
#'
#' @param trial A [TTETrial] object with trial level data.
#' @param ... Method arguments: `outcome` (character, must be in
#'   design@outcome_vars).
#'
#' @return The modified [TTETrial] object (for chaining).
#'
#' @details
#' The method computes four `weeks_to_X` values per trial:
#' - `weeks_to_event`: tstop of first period where outcome == 1
#' - `weeks_to_protocol_deviation`: tstop of first period with treatment deviation
#' - `weeks_to_admin_end`: weeks from entry isoyearweek to admin_censor_isoyearweek
#' - `weeks_to_loss`: max tstop if trial ended before any planned stop
#'
#' All `weeks_to_X` variables are aligned to period boundaries (multiples of the
#' collapsed period width). This ensures consistent behavior in `pmin()` comparisons.
#' `weeks_to_admin_end` is rounded DOWN to the nearest period boundary, meaning
#' partial periods at study end are excluded (conservative approach). A warning
#' is issued if any trials entered less than one period before the administrative
#' end date (these trials have no complete periods and will be dropped).
#'
#' Then computes:
#' - `censor_week`: pmin of the four weeks_to values
#' - `event`: 1 if tstop == weeks_to_event, else 0
#' - `censor_this_period`: 1 if tstop == weeks_to_protocol_deviation OR
#'   tstop == weeks_to_loss, else 0
#'
#' Data is filtered to rows where tstop <= censor_week.
#'
#' @examples
#' \dontrun{
#' # Load trial object and prepare for specific outcome
#' trial <- qs2::qs_read("trial_with_ipw.qs2") |>
#'   tte_prepare_outcome(outcome = "death") |>
#'   tte_ipcw(censoring_var = "censor_this_period") |>
#'   tte_weights() |>
#'   tte_truncate()
#' }
#'
#' @family tte_methods
#' @seealso [tte_ipcw_pp()] for the next step in the per-protocol workflow
#' @export
tte_prepare_outcome <- S7::new_generic("tte_prepare_outcome", "trial")

S7::method(tte_prepare_outcome, TTETrial) <- function(trial, outcome) {
  # Guard: require trial level data
  if (trial@data_level != "trial") {
    stop(
      "tte_prepare_outcome() requires trial level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Run tte_enroll() first to convert person_week data to trial level."
    )
  }

  # Guard: prevent running twice (this method deletes rows!)
  if ("prepare_outcome" %in% trial@steps_completed) {
    stop("tte_prepare_outcome() can only be run once per trial (it deletes rows)")
  }

  design <- trial@design
  data <- trial@data

  # 1. Validate outcome
  if (!outcome %in% design@outcome_vars) {
    stop("outcome must be one of: ", paste(design@outcome_vars, collapse = ", "))
  }

  # 2. Set active outcome
  trial@active_outcome <- outcome

  # 3. Compute weeks_to_event (first tstop where outcome == 1)
  data[, weeks_to_event := {
    event_rows <- which(get(outcome) == 1)
    if (length(event_rows) > 0) {
      min(get(design@tstop_var)[event_rows])
    } else {
      NA_integer_
    }
  }, by = c(design@id_var)]

  # 4. Compute weeks_to_protocol_deviation
  # Require time_exposure_var for per-protocol
  if (length(design@time_exposure_var) == 0) {
    stop("design must have time_exposure_var for per-protocol censoring analysis")
  }

  data[, .protocol_deviated := data.table::fcase(
    get(design@exposure_var) == TRUE &
      (get(design@time_exposure_var) == FALSE | is.na(get(design@time_exposure_var))),
    TRUE,
    get(design@exposure_var) == FALSE &
      (get(design@time_exposure_var) == TRUE | is.na(get(design@time_exposure_var))),
    TRUE,
    default = FALSE
  )]
  data[, weeks_to_protocol_deviation := {
    if (any(.protocol_deviated)) {
      min(get(design@tstop_var)[.protocol_deviated])
    } else {
      NA_integer_
    }
  }, by = c(design@id_var)]

  # 5. Compute weeks_to_admin_end (if admin_censor_isoyearweek is set)
  if (length(design@admin_censor_isoyearweek) > 0) {
    if (!"isoyearweek" %in% names(data)) {
      stop("admin_censor_isoyearweek requires 'isoyearweek' column in data")
    }
    study_end_date <- cstime::isoyearweek_to_last_date(
      design@admin_censor_isoyearweek
    )
    # Get baseline isoyearweek (at tstart == 0) for each trial
    data[, .baseline_isoyearweek := isoyearweek[get(design@tstart_var) == 0][1],
         by = c(design@id_var)]
    data[, weeks_to_admin_end := as.integer(difftime(
      study_end_date,
      cstime::isoyearweek_to_last_date(.baseline_isoyearweek),
      units = "weeks"
    ))]
    data[, .baseline_isoyearweek := NULL]

    # Round down to period boundary for consistency with other weeks_to_X variables
    # (which are all tstop values, i.e., multiples of period_width)
    # This is conservative: partial periods at study end are excluded
    # Note: After tte_collapse(), minimum tstop equals the period_width
    # (periods are (0, period_width], (period_width, 2*period_width], ...)
    period_width <- data[, min(get(design@tstop_var))]
    data[, weeks_to_admin_end := (weeks_to_admin_end %/% period_width) * period_width]

    # Warn if trials will be dropped due to entering too close to admin end
    n_dropped <- data[weeks_to_admin_end < period_width, uniqueN(get(design@id_var))]
    if (n_dropped > 0) {
      warning(
        n_dropped, " trial(s) will be dropped (entered < ", period_width,
        " weeks before admin_censor_isoyearweek)"
      )
    }
  } else {
    data[, weeks_to_admin_end := NA_integer_]
  }

  # 6. Compute weeks_to_loss
  # Loss = trial ended before any planned stopping time
  data[, .max_tstop := max(get(design@tstop_var)), by = c(design@id_var)]
  data[, .first_planned_stop := pmin(
    weeks_to_event,
    weeks_to_protocol_deviation,
    weeks_to_admin_end,
    design@follow_up_time,
    na.rm = TRUE
  )]
  data[, weeks_to_loss := data.table::fifelse(
    .max_tstop < .first_planned_stop,
    .max_tstop,
    NA_integer_
  )]

  # 7. Compute censor_week
  data[, censor_week := pmin(
    weeks_to_event,
    weeks_to_protocol_deviation,
    weeks_to_loss,
    weeks_to_admin_end,
    na.rm = TRUE
  )]

  # 8. Filter to rows up to censor_week
  data <- data[get(design@tstop_var) <= censor_week | is.na(censor_week)]

  # 9. Create event indicator
  data[, event := as.integer(get(design@tstop_var) == weeks_to_event)]
  data[is.na(event), event := 0L]

  # 10. Create censor_this_period indicator
  data[, censor_this_period := as.integer(
    get(design@tstop_var) == weeks_to_protocol_deviation |
      get(design@tstop_var) == weeks_to_loss
  )]
  data[is.na(censor_this_period), censor_this_period := 0L]

  # 11. Clean up temporary columns
  data[, c(".max_tstop", ".first_planned_stop", ".protocol_deviated") := NULL]

  # 12. Order by trial ID and time (for consistent output)
  data.table::setorderv(data, c(design@id_var, design@tstop_var))

  trial@data <- data
  trial@steps_completed <- c(trial@steps_completed, "prepare_outcome")
  trial
}


# -----------------------------------------------------------------------------
# tte_table1: Baseline characteristics table
# -----------------------------------------------------------------------------

#' Generate baseline characteristics table
#'
#' S7 method that creates a baseline characteristics table, either weighted
#' (using IPW) or unweighted. Wraps [tableone::CreateTableOne()] for unweighted
#' tables and [tableone::svyCreateTableOne()] for weighted tables.
#'
#' This method requires `data_level == "trial"`.
#'
#' @param trial A [TTETrial] object with trial level data.
#' @param ... Method arguments: `ipw_col` (character or NULL, default NULL).
#'   If NULL, creates an unweighted table. If specified, creates a weighted
#'   table using the specified column as survey weights.
#'
#' @return A tableone object with baseline characteristics by exposure group.
#'
#' @details
#' The table is stratified by the exposure variable from the design. Baseline
#' data is extracted as the first row per trial (where tstart == 0).
#'
#' Variables included are the `confounder_vars` from the design. The table
#' includes NA counts and an overall column.
#'
#' @examples
#' \dontrun{
#' # Unweighted table
#' table1_unweighted <- trial |> tte_table1()
#'
#' # IPW-weighted table
#' table1_weighted <- trial |> tte_table1(ipw_col = "ipw_trunc")
#' }
#'
#' @family tte_methods
#' @seealso [tableone::CreateTableOne()], [tableone::svyCreateTableOne()]
#' @export
tte_table1 <- S7::new_generic("tte_table1", "trial")

S7::method(tte_table1, TTETrial) <- function(trial, ipw_col = NULL) {
  # Guard: require trial level data
  if (trial@data_level != "trial") {
    stop(
      "tte_table1() requires trial level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Run tte_enroll() first to convert person_week data to trial level."
    )
  }

  design <- trial@design

  # Extract baseline (tstart == 0)
  baseline <- trial@data[get(design@tstart_var) == 0]

  if (is.null(ipw_col)) {
    # Unweighted table
    tableone::CreateTableOne(
      vars = design@confounder_vars,
      strata = design@exposure_var,
      data = baseline,
      includeNA = TRUE,
      addOverall = TRUE
    )
  } else {
    # Validate ipw_col exists
    if (!ipw_col %in% names(baseline)) {
      stop("ipw_col '", ipw_col, "' not found in data")
    }

    # Weighted table
    svy_design <- survey::svydesign(
      ids = as.formula(paste0("~", design@person_id_var)),
      weights = as.formula(paste0("~", ipw_col)),
      data = baseline
    )
    tableone::svyCreateTableOne(
      vars = design@confounder_vars,
      strata = design@exposure_var,
      data = svy_design,
      includeNA = TRUE,
      addOverall = TRUE
    )
  }
}


# -----------------------------------------------------------------------------
# tte_rates: Events, person-years, and rates by exposure
# -----------------------------------------------------------------------------

#' Calculate events, person-years, and rates by exposure group
#'
#' S7 method that computes weighted events, person-years, and incidence rates
#' per 100,000 person-years, stratified by exposure group.
#'
#' This method requires `data_level == "trial"`.
#'
#' @param trial A [TTETrial] object with trial level data.
#' @param ... Method arguments: `weight_col` (character, required) - the column
#'   name containing per-protocol weights (e.g., "weight_pp_trunc").
#'
#' @return A data.table with columns:
#' \describe{
#'   \item{baseline_exposed}{Exposure group (TRUE/FALSE)}
#'   \item{n_trials}{Number of unique trials}
#'   \item{events_weighted}{Weighted sum of events}
#'   \item{py_weighted}{Weighted person-years (person_weeks / 52.25)}
#'   \item{rate_per_100000py}{Incidence rate per 100,000 person-years}
#' }
#'
#'   The result carries attributes `swereg_type = "rates"` and `exposure_var`
#'   (the exposure column name), used by [tte_rates_combine()].
#'
#' @examples
#' \dontrun{
#' table2 <- trial |> tte_rates(weight_col = "weight_pp_trunc")
#' }
#'
#' @family tte_methods
#' @export
tte_rates <- S7::new_generic("tte_rates", "trial")

S7::method(tte_rates, TTETrial) <- function(trial, weight_col) {
  # Guard: require trial level data
  if (trial@data_level != "trial") {
    stop(
      "tte_rates() requires trial level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Run tte_enroll() first to convert person_week data to trial level."
    )
  }

  design <- trial@design
  data <- trial@data

  # Validate weight_col exists
  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data")
  }

  # Validate required columns
  if (!"event" %in% names(data)) {
    stop("'event' column not found. Run tte_prepare_outcome() first.")
  }
  if (!"person_weeks" %in% names(data)) {
    stop("'person_weeks' column not found. Run tte_collapse() first.")
  }

  result <- data[,
    .(
      n_trials = data.table::uniqueN(get(design@id_var)),
      events_weighted = sum(event * get(weight_col)),
      py_weighted = sum(person_weeks * get(weight_col)) / 52.25,
      rate_per_100000py = sum(event * get(weight_col)) /
        (sum(person_weeks * get(weight_col)) / 52.25) *
        100000
    ),
    by = c(design@exposure_var)
  ]
  data.table::setattr(result, "swereg_type", "rates")
  data.table::setattr(result, "exposure_var", design@exposure_var)
  result
}


# -----------------------------------------------------------------------------
# tte_irr: Fit Poisson models and extract IRR estimates
# -----------------------------------------------------------------------------

#' Fit Poisson models and extract incidence rate ratios
#'
#' S7 method that fits quasipoisson regression models and extracts IRR estimates.
#' Fits two models: (1) constant hazard, (2) flexible hazard with natural splines.
#' Uses survey-weighted GLM with cluster-robust standard errors.
#'
#' This method requires `data_level == "trial"`.
#'
#' @param trial A [TTETrial] object with trial level data.
#' @param ... Method arguments: `weight_col` (character, required) - the column
#'   name containing per-protocol weights.
#'
#' @return A data.table with one row containing: `IRR_const` (IRR from constant
#'   hazard model), `IRR_const_lower` and `IRR_const_upper` (95\% CI),
#'   `IRR_const_pvalue`, `IRR_flex` (IRR from flexible hazard model),
#'   `IRR_flex_lower` and `IRR_flex_upper` (95\% CI), `IRR_flex_pvalue`,
#'   `warn_const` and `warn_flex` (logical flags for convergence warnings).
#'
#'   The result carries attribute `swereg_type = "irr"`, used by
#'   [tte_irr_combine()].
#'
#' @details
#' The constant hazard model assumes a constant baseline hazard over time.
#' The flexible hazard model uses natural splines (df=3) to allow non-constant
#' baseline hazard, testing whether the constant hazard assumption is reasonable.
#'
#' Both models use quasipoisson family to account for overdispersion and
#' offset(log(person_weeks)) for the person-time denominator.
#'
#' @examples
#' \dontrun{
#' irr <- trial |> tte_irr(weight_col = "weight_pp_trunc")
#' cat("IRR:", irr$IRR_const, "(", irr$IRR_const_lower, "-", irr$IRR_const_upper, ")\n")
#' }
#'
#' @family tte_methods
#' @export
tte_irr <- S7::new_generic("tte_irr", "trial")

S7::method(tte_irr, TTETrial) <- function(trial, weight_col) {
  # Guard: require trial level data
  if (trial@data_level != "trial") {
    stop(
      "tte_irr() requires trial level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Run tte_enroll() first to convert person_week data to trial level."
    )
  }

  design <- trial@design
  data <- trial@data

  # Validate weight_col exists
  if (!weight_col %in% names(data)) {
    stop("weight_col '", weight_col, "' not found in data")
  }

  # Validate required columns
  if (!"event" %in% names(data)) {
    stop("'event' column not found. Run tte_prepare_outcome() first.")
  }
  if (!"person_weeks" %in% names(data)) {
    stop("'person_weeks' column not found. Run tte_collapse() first.")
  }

  # Create survey design
  svy_design <- survey::svydesign(
    ids = as.formula(paste0("~", design@person_id_var)),
    weights = as.formula(paste0("~", weight_col)),
    data = data
  )

  # Track warnings for each model
  warn_const <- FALSE
  warn_flex <- FALSE

  # Build formula for exposure coefficient name
  exposure_coef <- paste0(design@exposure_var, "TRUE")

  # Model 1: Constant hazard (primary)
  formula_const <- stats::as.formula(paste0(
    "event ~ ", design@exposure_var, " + offset(log(person_weeks))"
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

  # Model 2: Flexible hazard (sensitivity analysis)
  formula_flex <- stats::as.formula(paste0(
    "event ~ ", design@exposure_var,
    " + splines::ns(", design@tstop_var, ", df = 3)",
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
}


# -----------------------------------------------------------------------------
# tte_km: Kaplan-Meier curves
# -----------------------------------------------------------------------------

#' Fit Kaplan-Meier curves and optionally plot
#'
#' S7 method that fits weighted Kaplan-Meier curves by exposure group and
#' optionally creates a step plot. Uses IPW only (not IPCW) because IPCW is
#' time-varying and cannot be directly applied to KM estimation.
#'
#' This method requires `data_level == "trial"`.
#'
#' @param trial A [TTETrial] object with trial level data.
#' @param ... Method arguments:
#'   \describe{
#'     \item{ipw_col}{Character, required. Column name for IPW weights.}
#'     \item{save_path}{Character or NULL. If specified, saves the plot to this path.}
#'     \item{title}{Character or NULL. Plot title. If NULL, no title is shown.}
#'   }
#'
#' @return A svykm object (invisibly if save_path is specified).
#'
#' @details
#' The method extracts the last row per trial (final follow-up status) and
#' creates a survey design using the specified IPW column. The svykm object
#' contains survival curves for each exposure group.
#'
#' If `save_path` is specified, creates a step plot with:
#' - Exposed group in red, Unexposed group in blue
#' - Y-axis scaled to 0.99-1.0 (appropriate for rare events)
#' - X-axis showing time in weeks
#'
#' Note: We use IPW only (not weight_pp = IPW × IPCW-PP) because IPCW-PP is
#' time-varying and cannot be directly applied to KM estimation. The IPW
#' adjusts for baseline confounding, providing an ITT-like visualization
#' of event-free survival by treatment group.
#'
#' @examples
#' \dontrun{
#' # Just fit the KM curves
#' km <- trial |> tte_km(ipw_col = "ipw_trunc")
#'
#' # Fit and save plot
#' km <- trial |> tte_km(
#'   ipw_col = "ipw_trunc",
#'   save_path = "results/km_plot.png",
#'   title = "Event-free survival by treatment group"
#' )
#' }
#'
#' @family tte_methods
#' @seealso [survey::svykm()]
#' @export
tte_km <- S7::new_generic("tte_km", "trial")

S7::method(tte_km, TTETrial) <- function(
    trial,
    ipw_col,
    save_path = NULL,
    title = NULL
) {
  # Guard: require trial level data
  if (trial@data_level != "trial") {
    stop(
      "tte_km() requires trial level data.\n",
      "Current data_level: '", trial@data_level, "'\n",
      "Hint: Run tte_enroll() first to convert person_week data to trial level."
    )
  }

  design <- trial@design
  data <- trial@data

  # Validate ipw_col exists
  if (!ipw_col %in% names(data)) {
    stop("ipw_col '", ipw_col, "' not found in data")
  }

  # Validate required columns
  if (!"event" %in% names(data)) {
    stop("'event' column not found. Run tte_prepare_outcome() first.")
  }

  # Extract final row per trial (last follow-up)
  final <- data[, .SD[.N], by = c(design@id_var)]

  # Create survey design
  svy_design <- survey::svydesign(
    ids = as.formula(paste0("~", design@person_id_var)),
    weights = as.formula(paste0("~", ipw_col)),
    data = final
  )

  # Build Surv formula
  km_formula <- stats::as.formula(paste0(
    "survival::Surv(", design@tstop_var, ", event) ~ ", design@exposure_var
  ))

  # Fit KM curves

km_fit <- survey::svykm(km_formula, design = svy_design)

  # Plot if save_path is specified
  if (!is.null(save_path)) {
    # Extract data from svykm object
    # svykm returns a list with elements named by strata levels
    strata_names <- names(km_fit)

    # Find exposed and unexposed strata
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

    # Create plot
    p <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = surv, color = group)) +
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

    # Save plot
    ggplot2::ggsave(save_path, p, width = 8, height = 6, dpi = 300)

    invisible(km_fit)
  } else {
    km_fit
  }
}


# -----------------------------------------------------------------------------
# tte_rates_combine: Format multiple tte_rates() outputs into a wide table
# -----------------------------------------------------------------------------

#' Combine and format multiple tte_rates() outputs into a publication-ready table
#'
#' Extracts [tte_rates()] outputs from a named results list and returns a
#' formatted wide data.table with events, person-years, and rates by exposure.
#'
#' @param results Named list of per-ETT result lists. Each element must contain
#'   a slot named by `slot` holding a [tte_rates()] output. Names are used as
#'   `ett_id` values.
#' @param slot Character scalar: name of the slot within each result that
#'   contains the [tte_rates()] output (e.g., `"table2"`).
#' @param descriptions Optional named character vector mapping ett_id to
#'   descriptions (e.g., `c(ETT01 = "Psychosis, 52w, 40-44")`). If provided,
#'   a `description` column is included in the output.
#'
#' @return A data.table in wide format with columns: `ett_id`,
#'   `description` (if provided), `events_weighted_Exposed`,
#'   `events_weighted_Unexposed`, `py_weighted_Exposed`,
#'   `py_weighted_Unexposed`, `rate_per_100000py_Exposed`,
#'   `rate_per_100000py_Unexposed`.
#'
#'   Formatting: events to 1 decimal place, person-years as comma-separated
#'   integers, rates to 1 decimal place.
#'
#' @examples
#' \dontrun{
#' ett_desc <- setNames(ett$description, ett$ett_id)
#' table2 <- tte_rates_combine(results, "table2", ett_desc)
#' }
#'
#' @family tte_methods
#' @export
tte_rates_combine <- function(results, slot, descriptions = NULL) {
  rates_list <- lapply(results, `[[`, slot)

  # Read exposure_var from attribute set by tte_rates()
  first_non_null <- Find(Negate(is.null), rates_list)
  exposure_col <- attr(first_non_null, "exposure_var")
  if (is.null(exposure_col)) {
    stop("results$*$", slot, " must be tte_rates() outputs (missing 'exposure_var' attribute)")
  }

  # rbindlist with idcol — names become ett_id values
  dt <- rbindlist(rates_list, idcol = "ett_id")

  # Create "Exposed"/"Unexposed" label and remove the original column
  dt[, exposed := fifelse(get(exposure_col), "Exposed", "Unexposed")]
  dt[, (exposure_col) := NULL]

  # Format numbers
  dt[, `:=`(
    events_weighted = format(round(events_weighted, 1), nsmall = 1),
    py_weighted = format(round(py_weighted, 0), big.mark = ","),
    rate_per_100000py = format(round(rate_per_100000py, 1), nsmall = 1)
  )]

  # Add descriptions if provided
  if (!is.null(descriptions)) {
    dt[, description := descriptions[ett_id]]
    cast_formula <- stats::as.formula("ett_id + description ~ exposed")
  } else {
    cast_formula <- stats::as.formula("ett_id ~ exposed")
  }

  # Pivot to wide format
  dcast(
    dt,
    cast_formula,
    value.var = c("events_weighted", "py_weighted", "rate_per_100000py")
  )
}


# -----------------------------------------------------------------------------
# tte_irr_combine: Format multiple tte_irr() outputs into a summary table
# -----------------------------------------------------------------------------

#' Combine and format multiple tte_irr() outputs into a publication-ready table
#'
#' Extracts [tte_irr()] outputs from a named results list and returns a
#' formatted data.table with IRR estimates, confidence intervals, and p-values.
#' Prints a message listing any ETTs with convergence warnings.
#'
#' @param results Named list of per-ETT result lists. Each element must contain
#'   a slot named by `slot` holding a [tte_irr()] output. Names are used as
#'   `ett_id` values.
#' @param slot Character scalar: name of the slot within each result that
#'   contains the [tte_irr()] output (e.g., `"table3"`).
#' @param descriptions Optional named character vector mapping ett_id to
#'   descriptions. If provided, a `description` column is included.
#'
#' @return A data.table with columns: `ett_id`, `description` (if provided),
#'   `IRR (constant)`, `95% CI (constant)`, `p (constant)`,
#'   `IRR (flexible)`, `95% CI (flexible)`, `p (flexible)`.
#'
#'   IRR values are formatted to 2 decimal places with "#" appended if the
#'   model had convergence warnings. CIs are formatted as "lower-upper" (2dp).
#'   P-values use [format.pval()] with 3 significant digits.
#'
#' @examples
#' \dontrun{
#' ett_desc <- setNames(ett$description, ett$ett_id)
#' table3 <- tte_irr_combine(results, "table3", ett_desc)
#' }
#'
#' @family tte_methods
#' @export
tte_irr_combine <- function(results, slot, descriptions = NULL) {
  irr_list <- lapply(results, `[[`, slot)
  dt <- rbindlist(irr_list, idcol = "ett_id")

  # Report convergence warnings
  warn_ids <- dt[warn_const == TRUE | warn_flex == TRUE, ett_id]
  if (length(warn_ids) > 0L) {
    message("Convergence warnings in: ", paste(warn_ids, collapse = ", "))
  }

  # Format
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

  # Add descriptions if provided
  if (!is.null(descriptions)) {
    result[, description := descriptions[ett_id]]
    setcolorder(result, c("ett_id", "description"))
  }

  result
}
