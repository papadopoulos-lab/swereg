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

# -----------------------------------------------------------------------------
# tte_match_ratio: Sample comparison group at specified ratio
# -----------------------------------------------------------------------------

#' Match unexposed to exposed at a specified ratio
#'
#' Samples unexposed individuals to achieve a target ratio relative to exposed
#' individuals. This is commonly used in target trial emulation to increase
#' statistical efficiency while maintaining reasonable sample sizes.
#'
#' @param data A data.table containing the study population.
#' @param exposure_var Character, name of the binary exposure column.
#' @param eligible_var Character, name of the eligibility indicator column
#'   (default: NULL, meaning all rows are eligible).
#' @param ratio Numeric, target ratio of unexposed to exposed (default: 2,
#'   meaning 2 unexposed for every 1 exposed).
#' @param id_var Character, name of the ID column for reporting (default: NULL).
#' @param seed Integer, random seed for reproducibility (default: NULL).
#' @param mark_unsampled Character, how to mark unsampled unexposed individuals:
#'   "na" sets exposure to NA, "drop" removes rows, "flag" adds a column
#'   (default: "na").
#'
#' @return The input data.table, modified:
#'   \itemize{
#'     \item If `mark_unsampled = "na"`: unsampled unexposed have exposure set to NA
#'     \item If `mark_unsampled = "drop"`: unsampled unexposed rows are removed
#'     \item If `mark_unsampled = "flag"`: adds `sampled` column (TRUE/FALSE)
#'   }
#'
#' @details
#' In observational studies, there are often many more unexposed than exposed
#' individuals. Including all unexposed can be computationally expensive and
#' adds little statistical power beyond a certain ratio. Common choices are
#' 1:1, 2:1, or 4:1 matching.
#'
#' The function:
#' \enumerate{
#'   \item Counts exposed individuals among eligible rows
#'   \item Randomly samples `ratio * n_exposed` from eligible unexposed
#'   \item Marks or removes unsampled unexposed individuals
#' }
#'
#' If there are fewer unexposed than `ratio * n_exposed`, all unexposed are kept.
#'
#' @examples
#' library(data.table)
#' set.seed(42)
#' dt <- data.table(
#'   id = 1:1000,
#'   eligible = TRUE,
#'   exposed = c(rep(TRUE, 100), rep(FALSE, 900))
#' )
#' # 2:1 matching: keep 200 unexposed for 100 exposed
#' result <- tte_match_ratio(dt, "exposed", "eligible", ratio = 2, seed = 123)
#' table(result$exposed, useNA = "always")
#'
#' @family tte_design
#' @export
tte_match_ratio <- function(
    data,
    exposure_var,
    eligible_var = NULL,
    ratio = 2,
    id_var = NULL,
    seed = NULL,
    mark_unsampled = "na"
) {
  # Declare NSE variables
  sampled <- NULL

  # Input validation
  if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }
  if (!exposure_var %in% names(data)) {
    stop("exposure_var '", exposure_var, "' not found in data")
  }
  if (!is.null(eligible_var) && !eligible_var %in% names(data)) {
    stop("eligible_var '", eligible_var, "' not found in data")
  }
  if (!is.numeric(ratio) || ratio <= 0) {
    stop("ratio must be a positive number")
  }
  if (!mark_unsampled %in% c("na", "drop", "flag")) {
    stop("mark_unsampled must be 'na', 'drop', or 'flag'")
  }

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Determine eligible rows
  if (is.null(eligible_var)) {
    eligible_mask <- rep(TRUE, nrow(data))
  } else {
    eligible_mask <- data[[eligible_var]] == TRUE
  }

  # Count exposed among eligible
  exposed_mask <- eligible_mask & data[[exposure_var]] == TRUE
  n_exposed <- sum(exposed_mask, na.rm = TRUE)

  # Get indices of unexposed among eligible
  unexposed_idx <- which(eligible_mask & data[[exposure_var]] == FALSE)

  # Sample unexposed
 n_to_sample <- min(round(ratio * n_exposed), length(unexposed_idx))
  keep_idx <- sample(unexposed_idx, n_to_sample)
  drop_idx <- setdiff(unexposed_idx, keep_idx)

  # Mark unsampled
  if (mark_unsampled == "na") {
    data[drop_idx, (exposure_var) := NA]
  } else if (mark_unsampled == "drop") {
    data <- data[-drop_idx]
  } else if (mark_unsampled == "flag") {
    data[, sampled := TRUE]
    data[drop_idx, sampled := FALSE]
  }

  data
}

# -----------------------------------------------------------------------------
# tte_collapse_periods: Collapse fine-grained intervals to coarser periods
# -----------------------------------------------------------------------------

#' Collapse time intervals to coarser periods
#'
#' Aggregates fine-grained longitudinal data (e.g., weekly) into coarser time
#' periods (e.g., 4-week periods) for target trial emulation. This reduces
#' data size and can improve model stability.
#'
#' @param data A data.table in counting-process format with one row per
#'   fine-grained time unit (e.g., week).
#' @param id_var Character, name of the trial/person identifier column.
#' @param time_var Character, name of the time index column (e.g., week number
#'   within trial, starting at 0).
#' @param period_width Integer, number of time units per period (default: 4).
#' @param first_cols Character vector, columns to aggregate using first value
#'   (e.g., baseline characteristics, confounders).
#' @param last_cols Character vector, columns to aggregate using last value
#'   (e.g., time-varying exposure status).
#' @param max_cols Character vector, columns to aggregate using max
#'   (e.g., event indicators - any event in period = 1).
#' @param sum_cols Character vector, columns to aggregate using sum
#'   (e.g., counts, default: NULL).
#'
#' @return A data.table with one row per period per trial, containing:
#'   \describe{
#'     \item{tstart}{Period start time (period * period_width)}
#'     \item{tstop}{Period end time (period * period_width + period_width)}
#'     \item{Aggregated columns}{As specified by first_cols, last_cols, max_cols, sum_cols}
#'   }
#'
#' @details
#' This function implements a common preprocessing step in target trial
#' emulation where weekly registry data is collapsed to 4-week (monthly)
#' periods. This:
#' \itemize{
#'   \item Reduces dataset size by ~4x (for 4-week periods)
#'   \item Improves computational efficiency
#'   \item Can improve model stability with sparse events
#' }
#'
#' The aggregation rules are:
#' \itemize{
#'   \item \code{first_cols}: Take first value in period (for time-invariant
#'     or baseline values)
#'   \item \code{last_cols}: Take last value in period (for time-varying
#'     status at period end)
#'   \item \code{max_cols}: Take maximum (for event indicators: any event
#'     in period counts as event)
#'   \item \code{sum_cols}: Take sum (for counts)
#' }
#'
#' @examples
#' library(data.table)
#' # Weekly data for 2 trials, 8 weeks each
#' dt <- data.table(
#'   trial_id = rep(1:2, each = 8),
#'   week = rep(0:7, 2),
#'   exposed = c(rep(TRUE, 8), rep(FALSE, 8)),
#'   age = rep(c(55, 60), each = 8),
#'   event = c(0,0,0,1,0,0,0,0, 0,0,0,0,0,0,1,0)
#' )
#' # Collapse to 4-week periods
#' result <- tte_collapse_periods(
#'   dt,
#'   id_var = "trial_id",
#'   time_var = "week",
#'   period_width = 4,
#'   first_cols = c("age"),
#'   last_cols = c("exposed"),
#'   max_cols = c("event")
#' )
#'
#' @family tte_data_prep
#' @export
tte_collapse_periods <- function(
    data,
    id_var,
    time_var,
    period_width = 4L,
    first_cols = NULL,
    last_cols = NULL,
    max_cols = NULL,
    sum_cols = NULL
) {
  # Declare NSE variables
  . <- period <- tstart <- tstop <- NULL

  # Input validation
  if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }
  if (!id_var %in% names(data)) {
    stop("id_var '", id_var, "' not found in data")
  }
  if (!time_var %in% names(data)) {
    stop("time_var '", time_var, "' not found in data")
  }

  all_agg_cols <- c(first_cols, last_cols, max_cols, sum_cols)
  missing_cols <- setdiff(all_agg_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }

  # Create period variable
  data[, period := get(time_var) %/% period_width]

  # Aggregate each type
  result <- data[,
    .(tstart = period[1] * period_width,
      tstop = period[1] * period_width + period_width),
    by = c(id_var, "period")
  ]

  if (!is.null(first_cols) && length(first_cols) > 0) {
    first_agg <- data[,
      lapply(.SD, data.table::first),
      by = c(id_var, "period"),
      .SDcols = first_cols
    ]
    result <- merge(result, first_agg, by = c(id_var, "period"))
  }

  if (!is.null(last_cols) && length(last_cols) > 0) {
    last_agg <- data[,
      lapply(.SD, data.table::last),
      by = c(id_var, "period"),
      .SDcols = last_cols
    ]
    result <- merge(result, last_agg, by = c(id_var, "period"))
  }

  if (!is.null(max_cols) && length(max_cols) > 0) {
    max_agg <- data[,
      lapply(.SD, max, na.rm = TRUE),
      by = c(id_var, "period"),
      .SDcols = max_cols
    ]
    result <- merge(result, max_agg, by = c(id_var, "period"))
  }

  if (!is.null(sum_cols) && length(sum_cols) > 0) {
    sum_agg <- data[,
      lapply(.SD, sum, na.rm = TRUE),
      by = c(id_var, "period"),
      .SDcols = sum_cols
    ]
    result <- merge(result, sum_agg, by = c(id_var, "period"))
  }

  # Clean up
  result[, period := NULL]
  data[, period := NULL]

  result
}

# -----------------------------------------------------------------------------
# tte_time_to_event: Calculate time to first event
# -----------------------------------------------------------------------------

#' Calculate time to first event for each trial
#'
#' Computes the time (tstop) of the first period in which an event occurs
#' for each trial. This is a common preprocessing step for survival analysis
#' in target trial emulation.
#'
#' @param data A data.table in counting-process format.
#' @param id_var Character, name of the trial identifier column.
#' @param event_cols Character vector, names of event indicator columns
#'   (binary: 1 = event occurred, 0 = no event).
#' @param tstop_var Character, name of the period end time column
#'   (default: "tstop").
#' @param prefix Character, prefix for output columns (default: "weeks_to_").
#'
#' @return The input data.table with added columns \code{prefix + event_col} for
#'   each event column. Each contains the tstop of the first period with an
#'   event, or NA if no event occurred.
#'
#' @details
#' For each trial and each event type, this function finds the first period
#' where the event indicator equals 1, and returns the tstop (end time) of
#' that period. This represents "time to event" in discrete-time survival
#' analysis.
#'
#' The result is trial-level (constant within each trial) and is typically
#' used for:
#' \itemize{
#'   \item Determining censoring times
#'   \item Creating survival outcomes
#'   \item Defining analysis endpoints
#' }
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   trial_id = rep(1:3, each = 4),
#'   tstart = rep(c(0, 4, 8, 12), 3),
#'   tstop = rep(c(4, 8, 12, 16), 3),
#'   death = c(0,0,1,0, 0,0,0,0, 0,1,0,0),
#'   hosp = c(0,1,0,0, 0,0,0,1, 0,0,0,0)
#' )
#' result <- tte_time_to_event(dt, "trial_id", c("death", "hosp"))
#' # Trial 1: weeks_to_death = 12, weeks_to_hosp = 8
#' # Trial 2: weeks_to_death = NA, weeks_to_hosp = 16
#' # Trial 3: weeks_to_death = 8, weeks_to_hosp = NA
#'
#' @family tte_data_prep
#' @export
tte_time_to_event <- function(
    data,
    id_var,
    event_cols,
    tstop_var = "tstop",
    prefix = "weeks_to_"
) {
  # Input validation
  if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }
  if (!id_var %in% names(data)) {
    stop("id_var '", id_var, "' not found in data")
  }
  if (!tstop_var %in% names(data)) {
    stop("tstop_var '", tstop_var, "' not found in data")
  }
  missing_cols <- setdiff(event_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Event columns not found in data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Calculate time to event for each event column
  output_cols <- paste0(prefix, event_cols)

  data[,
    (output_cols) := lapply(.SD, function(x) {
      event_rows <- which(x == 1)
      if (length(event_rows) > 0) {
        min(get(tstop_var)[event_rows])
      } else {
        NA_integer_
      }
    }),
    by = c(id_var),
    .SDcols = event_cols
  ]

  data
}
