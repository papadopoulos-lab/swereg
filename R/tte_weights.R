# =============================================================================
# Target Trial Emulation Weight Functions
# =============================================================================
# Functions for calculating inverse probability weights for target trial
# emulation in observational studies using longitudinal registry data.
#
# These functions implement:
# - IPW (Inverse Probability of Treatment Weighting) for baseline confounding
# - IPCW (Inverse Probability of Censoring Weighting) for informative censoring
# - Weight truncation for variance reduction
# - Combined per-protocol weights
# =============================================================================

# -----------------------------------------------------------------------------
# tte_truncate_weights: Truncate extreme weights
# -----------------------------------------------------------------------------

#' Truncate extreme inverse probability weights
#'
#' Truncates weights at specified quantiles to reduce variance inflation from
#' extreme values. This is a common practice in causal inference to balance
#' the bias-variance tradeoff when using inverse probability weights.
#'
#' @param data A data.table containing the weight columns to truncate.
#' @param weight_cols Character vector of column names containing weights to
#'   truncate.
#' @param lower Numeric, lower quantile cutoff (default: 0.01 for 1st percentile).
#' @param upper Numeric, upper quantile cutoff (default: 0.99 for 99th percentile).
#' @param suffix Character, suffix to append to column names for truncated weights
#'   (default: "_trunc").
#'
#' @return The input data.table with added columns for truncated weights. Column
#'   names follow the pattern \code{original_col + suffix}.
#'
#' @details
#' Weight truncation addresses the variance inflation that can occur with
#' inverse probability weights. Extreme weights (very large or very small)
#' can dramatically increase the variance of effect estimates. Truncating
#' at the 1st and 99th percentiles is a common choice that typically
#' introduces minimal bias while substantially reducing variance.
#'
#' The function calculates quantiles across all non-NA values in each column,
#' then clips values outside these bounds to the boundary values.
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   id = 1:100,
#'   ipw = c(0.1, rep(1, 98), 10),  # Extreme values at ends
#'   ipcw = runif(100, 0.5, 2)
#' )
#' result <- tte_truncate_weights(dt, weight_cols = c("ipw", "ipcw"))
#' # Creates ipw_trunc and ipcw_trunc columns
#'
#' @family tte_weights
#' @seealso \code{\link{tte_calculate_ipw}}, \code{\link{tte_calculate_ipcw}}
#' @export
tte_truncate_weights <- function(
    data,
    weight_cols,
    lower = 0.01,
    upper = 0.99,
    suffix = "_trunc"
) {
  # Input validation

if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }
  if (!is.character(weight_cols) || length(weight_cols) == 0) {
    stop("weight_cols must be a non-empty character vector")
  }
  missing_cols <- setdiff(weight_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.numeric(lower) || !is.numeric(upper) || lower < 0 || upper > 1 ||
      lower >= upper) {
    stop("lower and upper must be numeric with 0 <= lower < upper <= 1")
  }

  # Truncate each weight column
  for (col in weight_cols) {
    bounds <- stats::quantile(data[[col]], c(lower, upper), na.rm = TRUE)
    new_col <- paste0(col, suffix)
    data[, (new_col) := pmin(pmax(get(col), bounds[1]), bounds[2])]
  }

  data
}

# -----------------------------------------------------------------------------
# tte_calculate_ipw: Inverse Probability of Treatment Weighting
# -----------------------------------------------------------------------------

#' Calculate inverse probability of treatment weights (IPW)
#'
#' Calculates stabilized inverse probability of treatment weights to adjust
#' for baseline confounding in target trial emulation. IPW reweights the sample
#' so that confounders are balanced across treatment groups, creating a
#' pseudo-population where treatment is independent of measured confounders.
#'
#' @param data A data.table with one row per trial at baseline (typically where
#'   `tstart == 0`).
#' @param exposure_var Character, name of the binary exposure/treatment column.
#' @param confounder_vars Character vector of confounder variable names for
#'   the propensity score model.
#' @param id_var Character, name of the trial identifier column
#'   (default: "trial_id").
#' @param stabilize Logical, whether to use stabilized weights (recommended,
#'   default: TRUE). Stabilized weights have the marginal probability in the
#'   numerator, reducing variance while maintaining valid inference.
#'
#' @return The input data.table with added columns:
#'   \describe{
#'     \item{ps}{Propensity score: P(exposure | confounders)}
#'     \item{ipw}{Inverse probability weight (stabilized if `stabilize = TRUE`)}
#'   }
#'
#' @details
#' IPW addresses confounding by creating a pseudo-population where treatment
#' assignment is independent of confounders. The propensity score P(A|L) is
#' estimated using logistic regression, where A is treatment and L represents
#' confounders.
#'
#' **Stabilized weights (recommended):**
#' \deqn{IPW = \frac{P(A)}{P(A|L)}}
#' for exposed, and
#' \deqn{IPW = \frac{1-P(A)}{1-P(A|L)}}
#' for unexposed.
#'
#' **Unstabilized weights:**
#' \deqn{IPW = \frac{1}{P(A|L)}}
#' for exposed, and
#' \deqn{IPW = \frac{1}{1-P(A|L)}}
#' for unexposed.
#'
#' Stabilized weights are generally preferred because they have lower variance
#' and the pseudo-population has the same expected size as the original sample.
#'
#' @examples
#' library(data.table)
#' # Create example baseline data
#' set.seed(42)
#' baseline <- data.table(
#'   trial_id = 1:1000,
#'   exposed = rbinom(1000, 1, 0.3),
#'   age_cat = sample(1:4, 1000, replace = TRUE),
#'   education = sample(1:3, 1000, replace = TRUE)
#' )
#' # Calculate IPW
#' result <- tte_calculate_ipw(
#'   data = baseline,
#'   exposure_var = "exposed",
#'   confounder_vars = c("age_cat", "education"),
#'   id_var = "trial_id"
#' )
#' summary(result$ipw)
#'
#' @family tte_weights
#' @seealso \code{\link{tte_calculate_ipcw}}, \code{\link{tte_combine_weights}}
#' @export
tte_calculate_ipw <- function(
    data,
    exposure_var,
    confounder_vars,
    id_var = "trial_id",
    stabilize = TRUE
) {
  # Declare variables for data.table NSE
  ps <- ipw <- NULL

  # Input validation
  if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }
  if (!is.character(exposure_var) || length(exposure_var) != 1) {
    stop("exposure_var must be a single character string")
  }
  if (!exposure_var %in% names(data)) {
    stop("exposure_var '", exposure_var, "' not found in data")
  }
  if (!is.character(confounder_vars) || length(confounder_vars) == 0) {
    stop("confounder_vars must be a non-empty character vector")
  }
  missing_confounders <- setdiff(confounder_vars, names(data))
  if (length(missing_confounders) > 0) {
    stop("Confounders not found in data: ",
         paste(missing_confounders, collapse = ", "))
  }
  if (!is.character(id_var) || length(id_var) != 1) {
    stop("id_var must be a single character string")
  }
  if (!id_var %in% names(data)) {
    stop("id_var '", id_var, "' not found in data")
  }

  # Build propensity score formula
  ps_formula <- stats::as.formula(
    paste(exposure_var, "~", paste(confounder_vars, collapse = " + "))
  )

  # Fit propensity score model
  ps_model <- stats::glm(ps_formula, data = data, family = stats::binomial)

  # Calculate propensity scores
  data[, ps := stats::predict(ps_model, data, type = "response")]

  # Calculate IPW
  if (stabilize) {
    # Stabilized weights: P(A) / P(A|L) for exposed, (1-P(A)) / (1-P(A|L)) for
    # unexposed
    p_exposed <- mean(data[[exposure_var]], na.rm = TRUE)
    data[,
      ipw := data.table::fifelse(
        get(exposure_var) == TRUE,
        p_exposed / ps,
        (1 - p_exposed) / (1 - ps)
      )
    ]
  } else {
    # Unstabilized weights: 1 / P(A|L) for exposed, 1 / (1-P(A|L)) for unexposed
    data[,
      ipw := data.table::fifelse(
        get(exposure_var) == TRUE,
        1 / ps,
        1 / (1 - ps)
      )
    ]
  }

  data
}

# -----------------------------------------------------------------------------
# tte_identify_censoring: Detect protocol deviation and loss to follow-up
# -----------------------------------------------------------------------------

#' Identify protocol deviation and loss to follow-up for per-protocol analysis
#'
#' Detects informative censoring events in longitudinal data for target trial
#' emulation. Protocol deviation occurs when treatment status changes from
#' baseline assignment. Loss to follow-up occurs when follow-up ends earlier
#' than expected (e.g., death, emigration) without administrative censoring.
#'
#' @param data A data.table in counting-process format (one row per person per
#'   time period).
#' @param exposure_var Character, name of the time-varying exposure column
#'   (current treatment status at each period).
#' @param baseline_exposure_var Character, name of the baseline treatment
#'   assignment column.
#' @param id_var Character, name of the trial identifier column
#'   (default: "trial_id").
#' @param tstart_var Character, name of the period start time column
#'   (default: "tstart").
#' @param tstop_var Character, name of the period end time column
#'   (default: "tstop").
#' @param follow_up_var Character, name of the expected follow-up duration
#'   column. If NULL, loss to follow-up is not calculated (default: NULL).
#' @param admin_censor_var Character, name of a column indicating the
#'   administrative censoring boundary (e.g., study end date). If NULL,
#'   administrative censoring is not distinguished from loss to follow-up
#'   (default: NULL).
#'
#' @return The input data.table with added columns:
#'   \describe{
#'     \item{protocol_deviated}{Logical, TRUE if current exposure differs from
#'       baseline assignment or is missing (NA)}
#'     \item{weeks_to_protocol_deviation}{Integer, tstop of first period with
#'       protocol deviation (NA if never deviated)}
#'     \item{weeks_to_loss}{Integer, max tstop if loss to follow-up occurred
#'       (NA otherwise). Only calculated if `follow_up_var` is provided.}
#'     \item{censored}{Logical, TRUE if protocol_deviated or lost to follow-up}
#'   }
#'
#' @details
#' In per-protocol analysis, participants are censored when they deviate from
#' their assigned treatment. This function identifies:
#'
#' **Protocol deviation:**
#' \itemize{
#'   \item Exposed at baseline who stop/switch treatment
#'   \item Unexposed at baseline who start treatment
#'   \item Missing exposure data (cannot confirm adherence)
#' }
#'
#' **Loss to follow-up:**
#' \itemize{
#'   \item Follow-up ended before expected duration
#'   \item No protocol deviation occurred
#'   \item Not due to administrative censoring (study end)
#' }
#'
#' These censoring events are typically informative (related to prognosis) and
#' require IPCW adjustment for valid per-protocol effect estimation.
#'
#' @examples
#' library(data.table)
#' # Create example counting-process data
#' dt <- data.table(
#'   trial_id = rep(1:3, each = 4),
#'   tstart = rep(0:3, 3),
#'   tstop = rep(1:4, 3),
#'   baseline_exposed = rep(c(TRUE, TRUE, FALSE), each = 4),
#'   current_exposed = c(TRUE, TRUE, FALSE, FALSE,  # Person 1 deviates at week 3
#'                       TRUE, TRUE, TRUE, TRUE,    # Person 2 adheres
#'                       FALSE, FALSE, TRUE, TRUE)  # Person 3 deviates at week 3
#' )
#' result <- tte_identify_censoring(
#'   data = dt,
#'   exposure_var = "current_exposed",
#'   baseline_exposure_var = "baseline_exposed"
#' )
#'
#' @family tte_weights
#' @seealso \code{\link{tte_calculate_ipcw}}
#' @export
tte_identify_censoring <- function(
    data,
    exposure_var,
    baseline_exposure_var,
    id_var = "trial_id",
    tstart_var = "tstart",
    tstop_var = "tstop",
    follow_up_var = NULL,
    admin_censor_var = NULL
) {
  # Declare variables for data.table NSE
  protocol_deviated <- weeks_to_protocol_deviation <- max_tstop <- NULL
  weeks_to_loss <- censored <- NULL

  # Input validation
  if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }
  required_cols <- c(exposure_var, baseline_exposure_var, id_var, tstop_var)
  if (!is.null(tstart_var)) required_cols <- c(required_cols, tstart_var)
  if (!is.null(follow_up_var)) required_cols <- c(required_cols, follow_up_var)
  if (!is.null(admin_censor_var)) {
    required_cols <- c(required_cols, admin_censor_var)
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }

  # Protocol deviation: current exposure differs from baseline assignment
  # NA exposure is treated as deviation (cannot confirm adherence)
  data[,
    protocol_deviated := data.table::fcase(
      get(baseline_exposure_var) == TRUE &
        (get(exposure_var) == FALSE | is.na(get(exposure_var))), TRUE,
      get(baseline_exposure_var) == FALSE &
        (get(exposure_var) == TRUE | is.na(get(exposure_var))), TRUE,
      default = FALSE
    )
  ]

  # Time to first protocol deviation
  data[,
    weeks_to_protocol_deviation := data.table::fifelse(
      any(protocol_deviated),
      min_with_infinite_as_na(get(tstop_var)[protocol_deviated]),
      NA_integer_
    ),
    by = c(id_var)
  ]

  # Loss to follow-up (if follow_up_var provided)
  if (!is.null(follow_up_var)) {
    data[, max_tstop := max(get(tstop_var)), by = c(id_var)]

    if (!is.null(admin_censor_var)) {
      # Exclude administrative censoring
      data[,
        weeks_to_loss := data.table::fifelse(
          max_tstop < get(follow_up_var) &
            is.na(weeks_to_protocol_deviation) &
            max_tstop < get(admin_censor_var),
          max_tstop,
          NA_integer_
        )
      ]
    } else {
      # Without admin_censor_var, any early ending is loss to follow-up
      data[,
        weeks_to_loss := data.table::fifelse(
          max_tstop < get(follow_up_var) &
            is.na(weeks_to_protocol_deviation),
          max_tstop,
          NA_integer_
        )
      ]
    }
    data[, max_tstop := NULL]
  }

  # Combined censoring indicator
  if (!is.null(follow_up_var)) {
    data[,
      censored := protocol_deviated |
        get(tstop_var) == weeks_to_loss
    ]
    data[is.na(censored), censored := FALSE]
  } else {
    data[, censored := protocol_deviated]
  }

  data
}

# -----------------------------------------------------------------------------
# tte_combine_weights: Combine IPW and IPCW
# -----------------------------------------------------------------------------

#' Combine IPW and IPCW weights for per-protocol analysis
#'
#' Creates combined inverse probability weights by multiplying IPW (baseline
#' confounding adjustment) and IPCW (censoring adjustment). The combined weight
#' is used for per-protocol effect estimation in target trial emulation.
#'
#' @param data A data.table containing both IPW and IPCW columns.
#' @param ipw_col Character, name of the IPW column (default: "ipw").
#' @param ipcw_col Character, name of the IPCW column (default: "ipcw").
#' @param output_col Character, name for the combined weight column
#'   (default: "weight_pp").
#'
#' @return The input data.table with an added column for combined weights.
#'
#' @details
#' The per-protocol weight combines two adjustments:
#' \itemize{
#'   \item **IPW**: Adjusts for baseline confounding (who gets treated)
#'   \item **IPCW**: Adjusts for time-varying censoring (who deviates/drops out)
#' }
#'
#' The combined weight is simply: weight_pp = IPW x IPCW
#'
#' This creates a pseudo-population where both treatment assignment and
#' censoring are independent of measured confounders, enabling unbiased
#' estimation of the per-protocol effect.
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   id = 1:100,
#'   ipw = runif(100, 0.8, 1.2),
#'   ipcw = runif(100, 0.9, 1.1)
#' )
#' result <- tte_combine_weights(dt)
#' # Creates weight_pp = ipw * ipcw
#'
#' @family tte_weights
#' @seealso \code{\link{tte_calculate_ipw}}, \code{\link{tte_calculate_ipcw}}
#' @export
tte_combine_weights <- function(
    data,
    ipw_col = "ipw",
    ipcw_col = "ipcw",
    output_col = "weight_pp"
) {
  # Input validation
  if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }
  if (!ipw_col %in% names(data)) {
    stop("ipw_col '", ipw_col, "' not found in data")
  }
  if (!ipcw_col %in% names(data)) {
    stop("ipcw_col '", ipcw_col, "' not found in data")
  }

  # Combine weights
  data[, (output_col) := get(ipw_col) * get(ipcw_col)]

  data
}

# -----------------------------------------------------------------------------
# tte_calculate_ipcw: Inverse Probability of Censoring Weighting
# -----------------------------------------------------------------------------

#' Calculate inverse probability of censoring weights (IPCW)
#'
#' Calculates time-varying stabilized inverse probability of censoring weights
#' for per-protocol analysis in target trial emulation. IPCW adjusts for
#' informative censoring due to protocol deviation or loss to follow-up.
#'
#' @param data A data.table in counting-process format (one row per person per
#'   time period). Should contain a censoring indicator column.
#' @param exposure_var Character, name of the baseline exposure column.
#' @param censoring_var Character, name of the binary censoring indicator
#'   column (1 = censored at end of this period, 0 = not censored).
#' @param confounder_vars Character vector of confounder variable names for
#'   the censoring model.
#' @param id_var Character, name of the trial identifier column
#'   (default: "trial_id").
#' @param tstart_var Character, name of the period start time column
#'   (default: "tstart").
#' @param tstop_var Character, name of the period end time column
#'   (default: "tstop").
#' @param separate_by_exposure Logical, whether to fit separate censoring
#'   models for each exposure stratum (default: TRUE). Recommended because
#'   censoring hazards often differ by treatment.
#' @param use_gam Logical, whether to use GAM with smooth time term instead
#'   of plain GLM (default: TRUE). GAM allows flexible, non-parametric time
#'   trends in the censoring hazard.
#'
#' @return The input data.table with added columns:
#'   \describe{
#'     \item{p_censor}{Predicted probability of censoring this period}
#'     \item{p_uncensored}{1 - p_censor}
#'     \item{cum_p_uncensored}{Cumulative product of p_uncensored over time}
#'     \item{marginal_p}{Marginal probability of remaining uncensored at each
#'       time point (within exposure stratum if `separate_by_exposure = TRUE`)}
#'     \item{cum_marginal}{Cumulative product of marginal_p over time}
#'     \item{ipcw}{Stabilized IPCW = cum_marginal / cum_p_uncensored}
#'   }
#'
#' @details
#' IPCW addresses informative censoring in per-protocol analysis. When people
#' deviate from assigned treatment or are lost to follow-up, this censoring
#' may be related to prognosis (sicker people may be more likely to change
#' treatment or die). IPCW upweights people who were likely to be censored
#' but weren't, creating a pseudo-population where censoring is independent
#' of measured confounders.
#'
#' **Algorithm:**
#' \enumerate{
#'   \item Fit a model for P(censored | confounders, time)
#'   \item Calculate conditional probability of remaining: p_uncensored = 1 - p_censor
#'   \item Cumulative conditional: cum_p_uncensored = cumulative product over time
#'   \item Marginal probability: average p_uncensored at each time (within exposure)
#'   \item Cumulative marginal: cumulative product of marginal probabilities
#'   \item Stabilized IPCW = cum_marginal / cum_p_uncensored
#' }
#'
#' Separate models by exposure are recommended because the hazard of censoring
#' often differs by treatment (e.g., treated patients may discontinue due to
#' side effects while untreated may start treatment if symptoms worsen).
#'
#' GAM with smooth time term is recommended because the censoring hazard may
#' vary non-linearly over time.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Assuming data is prepared with censoring indicators
#' result <- tte_calculate_ipcw(
#'   data = counting_process_data,
#'   exposure_var = "baseline_exposed",
#'   censoring_var = "censor_this_period",
#'   confounder_vars = c("age_cat", "education"),
#'   use_gam = TRUE
#' )
#' }
#'
#' @family tte_weights
#' @seealso \code{\link{tte_calculate_ipw}}, \code{\link{tte_identify_censoring}},
#'   \code{\link{tte_combine_weights}}
#' @export
tte_calculate_ipcw <- function(
    data,
    exposure_var,
    censoring_var,
    confounder_vars,
    id_var = "trial_id",
    tstart_var = "tstart",
    tstop_var = "tstop",
    separate_by_exposure = TRUE,
    use_gam = TRUE
) {
  # Declare variables for data.table NSE
  . <- .SD <- p_censor <- p_uncensored <- cum_p_uncensored <- marginal_p <- NULL
  cum_marginal <- ipcw <- NULL

  # Input validation
  if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }
  required_cols <- c(exposure_var, censoring_var, id_var, tstop_var,
                     confounder_vars)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }

  # Check for mgcv if using GAM
  if (use_gam && !requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required for use_gam = TRUE. ",
         "Install it with: install.packages('mgcv')")
  }

  # Build censoring model formula
  if (use_gam) {
    formula_str <- paste(
      censoring_var, "~ s(", tstop_var, ") +",
      paste(confounder_vars, collapse = " + ")
    )
  } else {
    formula_str <- paste(
      censoring_var, "~", tstop_var, "+",
      paste(confounder_vars, collapse = " + ")
    )
  }
  ipcw_formula <- stats::as.formula(formula_str)

  # Fit censoring models and predict
  if (separate_by_exposure) {
    # Fit separate models for exposed and unexposed
    if (use_gam) {
      fit_exp <- mgcv::gam(
        ipcw_formula,
        data = data[get(exposure_var) == TRUE],
        family = stats::binomial
      )
      data[
        get(exposure_var) == TRUE,
        p_censor := stats::predict(fit_exp, .SD, type = "response")
      ]

      fit_unexp <- mgcv::gam(
        ipcw_formula,
        data = data[get(exposure_var) == FALSE],
        family = stats::binomial
      )
      data[
        get(exposure_var) == FALSE,
        p_censor := stats::predict(fit_unexp, .SD, type = "response")
      ]
    } else {
      fit_exp <- stats::glm(
        ipcw_formula,
        data = data[get(exposure_var) == TRUE],
        family = stats::binomial
      )
      data[
        get(exposure_var) == TRUE,
        p_censor := stats::predict(fit_exp, .SD, type = "response")
      ]

      fit_unexp <- stats::glm(
        ipcw_formula,
        data = data[get(exposure_var) == FALSE],
        family = stats::binomial
      )
      data[
        get(exposure_var) == FALSE,
        p_censor := stats::predict(fit_unexp, .SD, type = "response")
      ]
    }
  } else {
    # Fit single model for all
    if (use_gam) {
      fit <- mgcv::gam(ipcw_formula, data = data, family = stats::binomial)
    } else {
      fit <- stats::glm(ipcw_formula, data = data, family = stats::binomial)
    }
    data[, p_censor := stats::predict(fit, data, type = "response")]
  }

  # Calculate time-varying stabilized IPCW
  # Step 1: Probability of remaining uncensored this period
  data[, p_uncensored := 1 - p_censor]
  data.table::setorderv(data, c(id_var, tstop_var))

  # Step 2: Cumulative probability of remaining (person's predicted survival)
  data[, cum_p_uncensored := cumprod(p_uncensored), by = c(id_var)]

  # Step 3: Marginal probability at each time point (within exposure stratum)
  if (separate_by_exposure) {
    marginal <- data[,
      .(marginal_p = mean(p_uncensored)),
      by = c(tstop_var, exposure_var)
    ]
    data <- merge(data, marginal, by = c(tstop_var, exposure_var), all.x = TRUE)
  } else {
    marginal <- data[,
      .(marginal_p = mean(p_uncensored)),
      by = c(tstop_var)
    ]
    data <- merge(data, marginal, by = c(tstop_var), all.x = TRUE)
  }
  data.table::setorderv(data, c(id_var, tstop_var))

  # Step 4: Cumulative marginal probability
  data[, cum_marginal := cumprod(marginal_p), by = c(id_var)]

  # Step 5: Stabilized IPCW
  data[, ipcw := cum_marginal / cum_p_uncensored]

  data
}
