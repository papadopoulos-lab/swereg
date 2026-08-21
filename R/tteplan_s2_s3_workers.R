# --- s2_worker: Loop 2 IPCW-PP worker ----------------------------------------

#' Worker function for Loop 2: per-ETT IPCW-PP + save (internal)
#'
#' Loads an imputed enrollment file, runs `$s4_prepare_for_analysis()`, and
#' RETURNS the analysis-ready object. It writes nothing and knows no output
#' path: dispatched via `.batch_run_and_write(style = "return")`, batchit
#' serializes the returned `analysis` element to the declared output path and
#' commits it atomically.
#'
#' @param outcome Character, outcome variable name.
#' @param follow_up Integer, follow-up duration in weeks.
#' @param file_imp_path Path to the imputed enrollment .qs2 file.
#' @param n_threads Integer, number of data.table threads.
#' @param sep_by_tx Logical, estimate IPCW separately by treatment.
#' @param with_gam Logical, use GAM for IPCW estimation.
#' @param estimand Character, `"pp"` (default) or `"itt"`. ITT skips IPCW.
#' @return `list(analysis = <analysis-ready enrollment object>)`, matching the
#'   single declared output name `analysis`.
#' @noRd
.s2_worker <- function(
  outcome,
  follow_up,
  file_imp_path,
  n_threads,
  sep_by_tx,
  with_gam,
  estimand = "pp"
) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(file_imp_path, nthreads = 1L)
  enrollment$s4_prepare_for_analysis(
    outcome = outcome,
    follow_up = follow_up,
    estimand = estimand,
    estimate_ipcw_pp_separately_by_treatment = sep_by_tx,
    estimate_ipcw_pp_with_gam = with_gam
  )
  list(analysis = enrollment)
}


# --- s3_enrollment_worker: Loop 3a enrollment-level baseline worker -----------

#' Compute a single Table 1 panel from the baseline slice of a loaded
#' enrollment object. Bypasses the R6 method on the cached instance so it
#' works against pre-upgrade saved objects.
#' @noRd
.s3_enrollment_table1 <- function(
  enrollment,
  ipw_col = NULL,
  arm_labels = NULL,
  include_smd = TRUE,
  show_missing = TRUE
) {
  design <- enrollment$design
  if (!is.null(ipw_col) && !ipw_col %in% names(enrollment$data)) {
    return(NULL)
  }
  # The same entry-window read as `$table1()`. The two routes MUST agree.
  .tte_table1_core(
    data = enrollment$data,
    design = design,
    ipw_col = ipw_col,
    arm_labels = arm_labels,
    include_smd = include_smd,
    show_missing = show_missing
  )
}

#' Worker function for Loop 3a: per-enrollment baseline analysis in a subprocess.
#'
#' Loads an analysis file and raw file, computes table1 variants, and returns
#' the results. Dispatched via the generic batch runner (.batch_run()) in a
#' fresh R session for memory isolation.
#'
#' @param analysis_path Path to an analysis .qs2 file for this enrollment.
#' @param raw_path Path to the raw .qs2 file for this enrollment.
#' @param enrollment_id Character, enrollment identifier.
#' @param n_threads Integer, number of data.table threads.
#' @param arm_labels Optional named character vector with `comparator` and
#'   `intervention` keys, passed through to `$table1()`.
#' @return A named list with enrollment-level results.
#' @noRd
.s3_enrollment_worker <- function(
  analysis_path,
  raw_path,
  enrollment_id,
  n_threads,
  arm_labels = NULL
) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(analysis_path, nthreads = 1L)

  # Supplemental variant: Missing row forced for every variable, SMD column
  # included. Percentages over total N.
  supp_args <- list(
    arm_labels = arm_labels,
    include_smd = TRUE,
    show_missing = "always"
  )
  # Main variant: no Missing rows, SMD column included (used by the headline
  # "Table 1" sheet and by the "table1" CSV exhibit). Percentages over the
  # non-missing denominator so levels still sum to 100.
  main_args <- list(
    arm_labels = arm_labels,
    include_smd = TRUE,
    show_missing = "none"
  )

  safe <- function(fn_args, label) {
    tryCatch(
      do.call(.s3_enrollment_table1, fn_args),
      error = function(e) {
        warning(
          "table1 ",
          label,
          " failed for ",
          enrollment_id,
          ": ",
          conditionMessage(e)
        )
        NULL
      }
    )
  }

  table1_unweighted <- safe(
    c(list(enrollment = enrollment), supp_args),
    "unweighted"
  )
  table1_ipw_trunc <- safe(
    c(list(enrollment = enrollment, ipw_col = "ipw_trunc"), supp_args),
    "ipw_trunc"
  )
  table1_ipw <- safe(
    c(list(enrollment = enrollment, ipw_col = "ipw"), supp_args),
    "ipw"
  )
  table1_ipw_trunc_main <- safe(
    c(list(enrollment = enrollment, ipw_col = "ipw_trunc"), main_args),
    "ipw_trunc_main"
  )
  baseline_rows <- enrollment$data[
    get(enrollment$design$tstart_var) == 0
  ]
  n_baseline <- nrow(baseline_rows)
  # Per-arm analysis-set counts for the CONSORT "Analysis dataset" box.
  # Treatment is logical (intervention == TRUE), matching s2_ipw's
  # convention. If the split does not reconcile to the total (e.g. a
  # non-logical treatment var), fall back to NA so the box omits the split
  # rather than showing wrong arm counts.
  tv <- enrollment$design$treatment_var
  n_baseline_intervention <- sum(baseline_rows[[tv]] == TRUE, na.rm = TRUE)
  n_baseline_comparator <- sum(baseline_rows[[tv]] == FALSE, na.rm = TRUE)
  if ((n_baseline_intervention + n_baseline_comparator) != n_baseline) {
    n_baseline_intervention <- NA_integer_
    n_baseline_comparator <- NA_integer_
  }
  rm(enrollment, baseline_rows)
  gc()

  table1_raw <- NULL
  if (file.exists(raw_path)) {
    enrollment_raw <- swereg::qs2_read(raw_path, nthreads = 1L)
    table1_raw <- tryCatch(
      do.call(
        .s3_enrollment_table1,
        c(list(enrollment = enrollment_raw), supp_args)
      ),
      error = function(e) {
        warning(
          "table1 raw failed for ",
          enrollment_id,
          ": ",
          conditionMessage(e)
        )
        NULL
      }
    )
    rm(enrollment_raw)
    gc()
  }

  list(
    table1_raw = table1_raw,
    table1_unweighted = table1_unweighted,
    table1_ipw_trunc = table1_ipw_trunc,
    table1_ipw = table1_ipw,
    table1_ipw_trunc_main = table1_ipw_trunc_main,
    n_baseline = n_baseline,
    n_baseline_intervention = n_baseline_intervention,
    n_baseline_comparator = n_baseline_comparator,
    arm_labels = arm_labels,
    computed_at = Sys.time()
  )
}


# --- s3_ett_worker: Loop 3b per-ETT / per-analysis worker --------------------

#' Bootstrap replicates for every risk difference s3 computes.
#'
#' A fixed property of the stage, not an argument of a figure. A figure that
#' could lower it could lower the precision of a published interval. A figure
#' that could raise it could disagree with the results sheet beside it.
#' @noRd
.S3_RD_N_BOOT <- 500L

#' Random seed for every risk difference s3 computes.
#'
#' Fixed for the same reason as `.S3_RD_N_BOOT`, and recorded on every stored
#' result so a reader can reproduce the interval from the plan alone.
#' @noRd
.S3_RD_SEED <- 1L

#' Confidence level used when the study specification names none.
#'
#' The DEFAULT, not a constant. Unlike `.S3_RD_N_BOOT` and `.S3_RD_SEED`, the
#' confidence level is a scientific choice, so the study owns it. See
#' `.s3_conf_level()`.
#' @noRd
.S3_RD_CONF_LEVEL_DEFAULT <- 0.95


#' Read the study's confidence level for the risk-difference interval.
#'
#' The level is a STUDY property, read from
#' `spec$study$implementation$conf_level`. A study that wants 90 percent
#' intervals writes 90 percent once, in the specification, and every stored
#' result and every printed header then carries it.
#'
#' It is not a per-exhibit property. s3 computes the interval long before any
#' figure exists, so one study has one level. A figure that could restate the
#' level would print a label the numbers do not have.
#'
#' It is not a constant either. Fixing it at 0.95 would take a real capability
#' away from a study, and would take it away quietly.
#'
#' @param spec A parsed study specification, or `NULL`.
#' @return Numeric(1) strictly between 0 and 1. Returns
#'   `.S3_RD_CONF_LEVEL_DEFAULT` when the specification names no level.
#' @noRd
.s3_conf_level <- function(spec) {
  v <- spec$study$implementation$conf_level
  if (is.null(v)) {
    return(.S3_RD_CONF_LEVEL_DEFAULT)
  }
  v <- suppressWarnings(as.numeric(v))
  if (length(v) != 1L || is.na(v) || v <= 0 || v >= 1) {
    stop(
      "study$implementation$conf_level must be a single number strictly ",
      "between 0 and 1. It sets the risk-difference interval and the header ",
      "that states it."
    )
  }
  v
}


#' Split one risk-difference curve into the two results s3 stores.
#'
#' The row and the curve answer different questions, so they get different
#' slots. One shape cannot serve both. A results sheet reads the FIRST row of
#' whatever it is handed. Storing the 39-band curve where a one-row summary
#' belongs would report the first band under the header for the last one.
#'
#' The row is the end of follow-up. `.forest_rd_row()` takes the last band, and
#' this function adds the three fields that make the row self-describing:
#' `interval_status`, `n_boot` and `seed`. A reader of `plan$results_ett` can
#' then see why a bound is missing, and what produced the bound that is there,
#' without opening the curve.
#'
#' The curve is every band, with `surv_comparator` and `surv_intervention`
#' beside the risk difference. The risk difference is built from those two
#' columns. The old code threw them away, then read the analysis panel again
#' to recover them.
#'
#' It also carries `n_persons_at_risk_comparator` and
#' `n_persons_at_risk_intervention`, the head count of distinct people in each
#' arm and band. That count is what a numbers-at-risk row reports. It was the
#' last quantity a RENDERER had to open an analysis file for.
#'
#' The replicate matrix is DROPPED. `.tte_rd_curve()` attaches the whole
#' `n_boot` by `n_band` bootstrap matrix as the `rd_boot` attribute. Measured
#' on a 39-band curve at 500 replicates it is 156,216 bytes. Kept, it would add
#' 169 MB to a 540-ETT plan across two estimands. The stored percentiles
#' already summarise it.
#'
#' What stays is small. The row and the curve serialise to 2,335 bytes
#' together, which is 2.5 MB across that same plan.
#'
#' @param slot Character(1), the row slot name (`"rd_pp_trunc"` or
#'   `"rd_itt"`). The curve slot is the same name with `rd_curve_` in place of
#'   `rd_`.
#' @param curve The `$risk_difference()` return value, or the skip envelope
#'   `safe_call()` produces when it failed.
#' @param ett_id Character(1), the ETT the curve belongs to.
#' @param time_var Character(1), the band column name (`design$tstop_var`).
#' @return A named list of two elements, one per slot.
#' @noRd
.s3_rd_result <- function(slot, curve, ett_id, time_var) {
  curve_slot <- sub("^rd_", "rd_curve_", slot)
  usable <- data.table::is.data.table(curve) && nrow(curve) > 0L
  if (!usable) {
    # `curve` is the skip envelope here. It goes into BOTH slots. A slot left
    # absent reads as "this ETT was never asked", and that is the confusion
    # this whole phase exists to remove.
    return(stats::setNames(list(curve, curve), c(slot, curve_slot)))
  }
  i <- which.max(curve[[time_var]])
  row <- .forest_rd_row(ett_id, curve, time_var)
  data.table::set(
    row,
    j = "interval_status",
    value = as.character(curve$interval_status[i])
  )
  data.table::set(row, j = "n_boot", value = .S3_RD_N_BOOT)
  data.table::set(row, j = "seed", value = .S3_RD_SEED)
  data.table::setattr(curve, "rd_boot", NULL)
  data.table::setattr(curve, "seed", .S3_RD_SEED)
  stats::setNames(list(row, curve), c(slot, curve_slot))
}


#' Worker function for Loop 3b: runs ONE analysis on ONE ETT file.
#'
#' Loads an analysis file and calls a single method (rates or irr).
#' Dispatched via the generic batch runner (.batch_run()); each heavy call
#' gets its own subprocess so the OS reclaims all memory.
#'
#' @param analysis_path Path to the analysis .qs2 file.
#' @param method Character: "summary_and_rates", "rates", "irr",
#'   "risk_difference", "irr_by_subgroup", or "effect_modification_test".
#' @param weight_col Character, weight column name ("" for unweighted).
#' @param ett_id Character, ETT identifier (for logging).
#' @param n_threads Integer, number of data.table threads.
#' @param subgroup_var Optional column name for the stratified methods
#'   (`irr_by_subgroup`, `effect_modification_test`); `NULL` otherwise.
#' @param conf_level Numeric, the risk-difference interval level the study
#'   specification names. `.s3_conf_level()` resolves it in the parent, and
#'   every item carries it because batchit demands every formal on every item.
#'   Only `method = "risk_difference"` reads it.
#' @return The method result (data.table, list, etc.).
#' @noRd
.s3_ett_worker <- function(
  analysis_path,
  method,
  weight_col,
  ett_id,
  n_threads,
  subgroup_var = NULL,
  conf_level = .S3_RD_CONF_LEVEL_DEFAULT
) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(analysis_path, nthreads = 1L)

  safe_call <- function(expr_fn, label) {
    tryCatch(
      expr_fn(),
      error = function(e) {
        warning(label, " failed for ", ett_id, ": ", conditionMessage(e))
        list(skipped = TRUE, reason = conditionMessage(e))
      }
    )
  }

  # Always return a named list so the caller can merge with:
  #   for (k in names(res)) self$results_ett[[eid]][[k]] <- res[[k]]
  if (method == "summary_and_rates") {
    list(
      summary = enrollment$summary(),
      rates_pp_trunc = safe_call(
        \() enrollment$rates(weight_col = "analysis_weight_pp_trunc"),
        "rates_pp_trunc"
      ),
      rates_pp = safe_call(
        \() enrollment$rates(weight_col = "analysis_weight_pp"),
        "rates_pp"
      )
    )
  } else if (method == "irr") {
    # ITT weights on ipw_trunc (its only valid weight); name that slot irr_itt.
    # PP weights on analysis_weight_pp[_trunc] -> irr_pp[_trunc].
    slot <- if (identical(weight_col, "ipw_trunc")) {
      "irr_itt"
    } else {
      paste0("irr_", sub("^analysis_weight_", "", weight_col))
    }
    # The estimability decision is stored beside the ratio, exactly as
    # `nnt_direction` is stored beside the risk difference. A reader of
    # `plan$results_ett` reads the decision and applies no rule of its own.
    setNames(
      list(.s3_mark_irr_estimable(
        safe_call(\() enrollment$irr(weight_col = weight_col), slot)
      )),
      slot
    )
  } else if (method == "rates") {
    # ITT rates (weight ipw_trunc) -> rates_itt, for the ITT forest plot.
    slot <- if (identical(weight_col, "ipw_trunc")) {
      "rates_itt"
    } else {
      paste0("rates_", sub("^analysis_weight_", "", weight_col))
    }
    setNames(
      list(safe_call(\() enrollment$rates(weight_col = weight_col), slot)),
      slot
    )
  } else if (method == "risk_difference") {
    # The absolute scale. ITT weights on ipw_trunc -> rd_itt; PP weights on
    # analysis_weight_pp_trunc -> rd_pp_trunc. Nothing gates this branch: the
    # item builder emits it for every ETT, and the export path only formats
    # what it stores.
    slot <- if (identical(weight_col, "ipw_trunc")) {
      "rd_itt"
    } else {
      paste0("rd_", sub("^analysis_weight_", "", weight_col))
    }
    curve <- safe_call(
      \() {
        # Re-wrapped under the CURRENT class. A serialized R6 object keeps
        # the method bindings it was saved with. So an analysis file from an
        # earlier release carries no $risk_difference() at all. `own_data`
        # skips the defensive copy, which here is the whole panel.
        enr <- TTEEnrollment$new(
          enrollment$data,
          enrollment$design,
          data_level = "trial",
          own_data = TRUE
        )
        enr$risk_difference(
          weight_col = weight_col,
          n_boot = .S3_RD_N_BOOT,
          seed = .S3_RD_SEED,
          conf_level = conf_level
        )
      },
      slot
    )
    .s3_rd_result(slot, curve, ett_id, enrollment$design$tstop_var)
  } else if (method == "irr_by_subgroup") {
    # Stratified IRRs within subgroup_var; slot e.g. subgroup_rd_sex_pp / _itt.
    suffix <- if (identical(weight_col, "ipw_trunc")) "itt" else "pp"
    slot <- paste0("subgroup_", subgroup_var, "_", suffix)
    setNames(
      list(safe_call(
        \() enrollment$irr_by_subgroup(weight_col, subgroup_var),
        slot
      )),
      slot
    )
  } else if (method == "effect_modification_test") {
    # Interaction Wald test; slot e.g. emtest_rd_sex_pp / _itt.
    suffix <- if (identical(weight_col, "ipw_trunc")) "itt" else "pp"
    slot <- paste0("emtest_", subgroup_var, "_", suffix)
    setNames(
      list(safe_call(
        \() enrollment$effect_modification_test(weight_col, subgroup_var),
        slot
      )),
      slot
    )
  } else {
    stop("Unknown method: ", method)
  }
}
