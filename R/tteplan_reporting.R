# Reporting for a TTEPlan: the TARGET reporting checklist and the
# stored-results diagnostic. The console specification summary moved to
# `R/r6_tteplan_print.R` on 2026-09-22, beside the method that calls it.
#
# Each function here holds the whole body of the public method that carries
# its name, guards included. The method in `R/r6_tteplan_print.R`,
# `R/r6_tteplan_checklist.R` or `R/r6_tteplan_results.R` is a one-call
# delegate to it.

#' Print the TARGET reporting checklist
#'
#' The body of `TTEPlan$print_target_checklist()`.
#'
#' @param plan A `TTEPlan`.
#' @return `invisible(NULL)`.
#' @noRd
.plan_print_target_checklist <- function(plan) {
  # Local bindings (avoid R CMD check NSE notes)
  criterion <- n_person_trials <- NULL # nolint
  n_intervention <- n_comparator <- NULL # nolint

  spec <- plan$spec
  if (is.null(spec)) {
    stop("plan has no spec -- set plan$spec first", call. = FALSE)
  }

  bold <- function(x) paste0("\033[1m", x, "\033[0m")
  dim <- function(x) paste0("\033[2m", x, "\033[0m")
  red <- function(x) paste0("\033[31m", x, "\033[0m")
  cyan <- function(x) paste0("\033[36m", x, "\033[0m")

  # Header
  cat(strrep("\u2550", 59), "\n")
  cat("          TARGET CHECKLIST \u2014 Transparent Reporting of\n")
  cat("     Observational Studies Emulating a Target Trial (2025)\n")
  cat(strrep("\u2550", 59), "\n")
  cat("\n")
  cat("Reference: Cashin AG, Hansford HJ, Hern\u00e1n MA, et al. TARGET\n")
  cat("Statement. JAMA. 2025;334(12):1084-1093.\n")
  cat("doi:10.1001/jama.2025.13350\n")
  cat("\n")
  if (!is.null(spec$study$title)) {
    cat("Generated from TTEPlan:", spec$study$title, "\n")
  }
  cat("Date:", format(Sys.Date(), "%Y-%m-%d"), "\n")
  cat("\n")

  # Helper to print one item
  item <- function(num, sub, title, guidance, auto_content = NULL) {
    label <- if (!is.null(sub)) paste0(num, sub) else as.character(num)
    cat(bold(paste0("Item ", label, ". ")), title, "\n\n", sep = "")
    cat(dim(paste0("   Guidance: ", guidance)), "\n\n")
    if (!is.null(auto_content) && nchar(auto_content) > 0) {
      cat("   From spec:\n")
      lines <- strsplit(auto_content, "\n")[[1]]
      for (l in lines) {
        cat("   ", l, "\n")
      }
      cat("\n")
    }
    return(cat("   >> [FILL IN]\n\n"))
  }

  # --- ABSTRACT ---
  cat(strrep("\u2500", 59), "\n")
  cat(bold("ABSTRACT"), "\n")
  cat(strrep("\u2500", 59), "\n\n")

  item(
    "1",
    "a",
    "Identify that the study attempts to emulate a target trial.",
    "Readers should be able to identify from the abstract that the study used observational data to emulate a target trial."
  )

  item(
    "1",
    "b",
    "Report the data sources used for emulation.",
    "Knowledge of the data sources provides context for assessing robustness and generalizability.",
    if (!is.null(spec$study$title)) spec$study$title
  )

  item(
    "1",
    "c",
    "Key assumptions, methods, and findings.",
    "Summarize the key assumptions, statistical methods, and main findings."
  )

  # --- INTRODUCTION ---
  cat(strrep("\u2500", 59), "\n")
  cat(bold("INTRODUCTION"), "\n")
  cat(strrep("\u2500", 59), "\n\n")

  item(
    "2",
    NULL,
    "Scientific background and rationale.",
    "Describe the scientific background and rationale for the study."
  )

  item(
    "3",
    NULL,
    "Causal question.",
    "State the specific causal question the study aims to address.",
    spec$study$description
  )

  item(
    "4",
    NULL,
    "Rationale for target trial emulation approach.",
    "Explain why a target trial emulation was used instead of a randomized trial."
  )

  # --- METHODS ---
  cat(strrep("\u2500", 59), "\n")
  cat(bold("METHODS \u2014 TARGET TRIAL SPECIFICATION"), "\n")
  cat(strrep("\u2500", 59), "\n\n")

  # 6a: Eligibility
  elig_text <- NULL
  if (!is.null(spec$inclusion_criteria$isoyears)) {
    iso <- spec$inclusion_criteria$isoyears
    parts <- paste0("- ISO years: ", iso[1], "-", iso[2])
    for (ic in spec[["inclusion_criteria"]][["criteria"]] %||% list()) {
      parts <- c(
        parts,
        paste0(
          "- Inclusion: ",
          ic$name,
          " (variable: ",
          ic$implementation$source_variable_combined %||%
            ic$implementation$source_variable,
          ", window: ",
          .tte_inclusion_window_human(ic$implementation),
          .tte_checklist_rule(ic$implementation),
          ")"
        )
      )
    }
    if (!is.null(spec$exclusion_criteria)) {
      for (ec in spec$exclusion_criteria) {
        parts <- c(
          parts,
          paste0(
            "- Exclusion: ",
            ec$name,
            " (variable: ",
            ec$implementation$source_variable_combined %||%
              ec$implementation$source_variable,
            ", window: ",
            .format_window_human(ec$implementation),
            .tte_checklist_rule(ec$implementation),
            ")"
          )
        )
      }
    }
    elig_text <- paste(parts, collapse = "\n")
  }
  item(
    "6",
    "a",
    "Describe the eligibility criteria.",
    "The eligibility criteria indicate who would be eligible for the target trial, including any washout or run-in periods.",
    elig_text
  )

  # 6b: Treatment strategies
  treat_text <- NULL
  if (!is.null(spec$enrollments)) {
    parts <- character()
    for (enr in spec$enrollments) {
      tx <- enr$treatment
      parts <- c(
        parts,
        paste0(
          "Enrollment '",
          enr$id,
          "': ",
          tx$arms$intervention,
          " vs ",
          tx$arms$comparator,
          " (variable: ",
          tx$implementation$variable,
          ", comparator-to-intervention ratio: ",
          tx$implementation$comparator_to_intervention_ratio,
          ":1)"
        )
      )
    }
    treat_text <- paste(parts, collapse = "\n")
  }
  item(
    "6",
    "b",
    "Describe the treatment strategies being compared.",
    "Clearly describe each treatment strategy, including dose, route, frequency, and duration.",
    treat_text
  )

  # 6c: Assignment
  assign_parts <- character()
  for (enr in spec$enrollments) {
    ratio <- enr$treatment$implementation$comparator_to_intervention_ratio
    assign_parts <- c(
      assign_parts,
      sprintf(
        "In enrollment %s, the draw took %s times that trial's count of intervention individuals.",
        enr$id,
        format(ratio, trim = TRUE)
      )
    )
  }
  # The stratum of the draw, in words. `sample()` runs inside one
  # `trial_id` group, and `trial_id` is the week index divided by
  # `period_width`. The stratum is therefore the entry band, and the band
  # is the only stratum. A width of 1 makes the band one week, so the two
  # readings differ and the text has to say which one it describes.
  #
  # Do not write the two-word grouping expression here. Its literal text
  # is what `test-no_na_trial_id_in_aggregates.R` scans this file for,
  # and a comment is not a call site.
  pw <- as.integer(plan$period_width %||% 4L)
  pw_weeks <- paste0(pw, if (pw == 1L) " week" else " weeks")
  stratum_text <- if (pw > 1L) {
    paste0(
      "Each sequential trial was one entry band of ",
      pw_weeks,
      ". ",
      "The sampling was stratified by trial, and not by week. ",
      "The entry weeks of two individuals in one trial therefore differed by up to ",
      pw - 1L,
      if (pw == 2L) " week. " else " weeks. ",
      "The draw read no other variable. "
    )
  } else {
    paste0(
      "Each sequential trial was one entry week, so the sampling was ",
      "stratified by week. ",
      "The draw read no other variable. "
    )
  }
  # The draw is one sample per trial, sized from that trial's intervention
  # count. It pairs nothing, so no matched set exists to condition on.
  # `survey::svydesign(ids = ~person_id_var)` clusters the variance on
  # person, and `trial_id` enters the outcome model as a covariate: a
  # natural spline from 5 trials, linear below that. Both are what a
  # non-matched stratified sample needs, and neither is a matched-set
  # stratum.
  no_pairing_text <- paste0(
    "The draw took one sample per trial. ",
    "It attached no comparator individual to an intervention individual, ",
    "so it formed no matched set. ",
    "No later step conditions on one. ",
    "A person can be an intervention individual in one trial and a ",
    "comparator individual in another. "
  )
  # `assign_paragraph()` holds the sentences that items 6c and 7c share. Each
  # caller passes only the parts that differ, so the two paragraphs cannot
  # drift apart.
  assign_paragraph <- function(
    lead = "",
    alternative = "",
    intervention_line = "",
    computation_line = "",
    ipw_line = ""
  ) {
    return(paste0(
      lead,
      "Comparator individuals entered by incidence density sampling within each sequential trial. ",
      alternative,
      "The draw ran from a stated seed. ",
      stratum_text,
      intervention_line,
      paste(assign_parts, collapse = " "),
      " Where a trial held fewer comparator individuals than that, the draw took all of them. ",
      no_pairing_text,
      computation_line,
      ipw_line
    ))
  }

  assign_text <- assign_paragraph(
    intervention_line = "Every intervention individual entered its trial. ",
    ipw_line = "Inverse probability weighting then adjusted for confounding by the remaining measured covariates, taken at the recruiting week."
  )
  item(
    "6",
    "c",
    "Describe the assignment procedures.",
    "Describe how individuals were assigned to treatment strategies in the emulated trial.",
    assign_text
  )

  # 6d: Follow-up
  fu_text <- NULL
  if (!is.null(spec$follow_up)) {
    parts <- vapply(
      spec$follow_up,
      function(fu) {
        return(paste0(fu$label, " (", fu$weeks, " weeks)"))
      },
      character(1)
    )
    fu_text <- paste(parts, collapse = "\n")
  }
  item(
    "6",
    "d",
    "Describe the start and end of follow-up.",
    "Define when follow-up begins and the criteria for its end.",
    fu_text
  )

  # 6e: Outcomes
  out_text <- NULL
  if (!is.null(spec$outcomes)) {
    parts <- vapply(
      spec$outcomes,
      function(o) {
        # `variable` may be a multi-source list (e.g. an outcome
        # ascertained from ICD-10 OR a quality registry); collapse
        # so the result is always a length-1 string for vapply.
        return(paste0(
          o$name,
          " (variable: ",
          paste(unlist(o$implementation$variable), collapse = " + "),
          ")"
        ))
      },
      character(1)
    )
    out_text <- paste(parts, collapse = "\n")
  }
  item(
    "6",
    "e",
    "Describe the outcomes.",
    "Define the primary and secondary outcomes.",
    out_text
  )

  # 6f: Causal contrasts
  #
  # `.TTE_ESTIMANDS` (R/r6_tteplan_pipeline.R) is the set the pipeline builds.
  # s2 writes one analysis file per member, and s3 reads one weight column per
  # member. This item and the item 7a-h narrative below both read that
  # constant, so neither can name an estimand the pipeline does not build.
  #
  # `est_6f` is every estimand item 6f recognises. `est_note` says how this
  # pipeline gets each one, and `est_how` is the narrative sentence for each.
  #
  # The register is the design, not completed work. `tte_stage()` prints this
  # checklist after s1, before any estimate exists.
  est_6f <- c(
    pp = "per-protocol",
    itt = "intention-to-treat",
    at = "as-treated"
  )
  est_note <- c(
    pp = "IPW + IPCW-PP",
    itt = "baseline IPW",
    at = "needs time-varying IPW"
  )
  est_how <- c(
    pp = paste0(
      "The per-protocol estimand censors an individual at the time of ",
      "treatment switching. It weights by the inverse probability of ",
      "censoring, which adjusts for the potential informativeness of that ",
      "censoring (Hern\u00e1n and Robins, 2016; Danaei et al., 2013). "
    ),
    itt = paste0(
      "The intention-to-treat estimand keeps an individual in the assigned ",
      "arm for the whole of follow-up. It weights by the baseline inverse ",
      "probability of treatment alone. "
    ),
    at = ""
  )
  est_planned <- names(est_6f) %in% names(.TTE_ESTIMANDS)
  est_join <- function(x) {
    if (length(x) < 2L) {
      return(paste(x, collapse = ""))
    }
    return(paste0(
      paste(x[-length(x)], collapse = ", "),
      " and ",
      x[length(x)]
    ))
  }
  est_render <- paste0(est_6f, " (", est_note, ")")
  item(
    "6",
    "f",
    "Describe the causal contrasts (estimands).",
    "Specify the causal estimand (e.g., intention-to-treat, per-protocol).",
    paste0(
      "Supported: ",
      paste(est_render[est_planned], collapse = ", "),
      ". Not supported: ",
      paste(est_render[!est_planned], collapse = ", "),
      "."
    )
  )

  # 6g: Confounders
  conf_text <- NULL
  if (!is.null(spec$confounders)) {
    parts <- vapply(
      spec$confounders,
      function(c) {
        impl <- c$implementation
        if (isTRUE(impl$computed)) {
          return(paste0(
            c$name,
            " (computed from: ",
            impl$source_variable_combined %||% impl$source_variable,
            ", window: ",
            .format_window_human(impl),
            ")"
          ))
        } else {
          return(paste0(c$name, " (variable: ", impl$variable, ")"))
        }
      },
      character(1)
    )
    conf_text <- paste(parts, collapse = "\n")
  }
  item(
    "6",
    "g",
    "Describe assumptions and confounders.",
    "Assumptions for valid causal inference include no unmeasured confounding, positivity, consistency, and correct model specification.",
    conf_text
  )

  # 6h: Analysis plan
  item(
    "6",
    "h",
    "Describe the data analysis plan.",
    "Describe the statistical methods, including how weights were estimated, models fitted, and sensitivity analyses planned.",
    paste0(
      "Treatment weights were estimated using stabilized inverse probability weights derived from a logistic regression model ",
      "for the probability of treatment assignment conditional on measured baseline covariates, fitted on baseline rows only. ",
      "Per-protocol effects were estimated by censoring individuals at the time of protocol deviation (treatment switching or loss to follow-up) ",
      "and applying inverse probability of censoring weights to account for informative censoring. ",
      "The estimator follows Hern\u00e1n and Robins (2016) and Danaei et al. (2013). ",
      "Censoring probabilities were modelled by a complementary log-log generalized additive model (the pipeline default) with a person-time offset, ",
      "fitted separately for the intervention and comparator arms. ",
      "It included a smooth function of follow-up time, a smooth function of the trial index to adjust for calendar time, ",
      "and the most recently updated confounder values. With few distinct values, a time term took a simpler form, or was omitted when it had one value. ",
      "The numerator of the stabilized censoring weight came from a second model with the same time terms and no confounders. ",
      "The primary outcome model was a weighted Poisson regression (quasipoisson family) ",
      "with a natural cubic spline for follow-up time (3 degrees of freedom), a natural cubic spline of the trial index (3 degrees of freedom; a linear term with 2 to 4 trials, and none with one trial) to adjust for calendar time, ",
      "and a person-time offset, fitted via survey-weighted generalized linear models with person-level clustered standard errors. ",
      "Extreme weights were truncated at the 1st and 99th percentiles after each weighting step to reduce the influence of near-violations of the positivity assumption. ",
      "The absolute effect was the cause-specific risk difference, the difference between the arms in one minus the weighted discrete-time survival. ",
      "Death and end of observation censored follow-up, so the risk was not a cumulative incidence with death as a competing risk. ",
      "The ",
      format(100 * .s3_conf_level(spec)),
      "% confidence interval of the risk difference was the percentile interval of ",
      .S3_RD_N_BOOT,
      " bootstrap replicates that resampled persons, not person-trials. ",
      "The number needed to treat was the negative reciprocal of the risk difference, reported as the number needed to treat for benefit or for harm according to its sign."
    )
  )

  # 7a-7h: Emulation
  cat(strrep("\u2500", 59), "\n")
  cat(bold("METHODS \u2014 EMULATION"), "\n")
  cat(strrep("\u2500", 59), "\n\n")

  item(
    "7",
    "a-h",
    "Describe how each specification element was emulated.",
    "For each element (6a-6h), describe how it was emulated using the observational data, including any deviations from the target trial.",
    paste0(
      "Each element of the target trial specification (items 6a\u2013h) was emulated using the observational registry data as follows. ",
      # 7a: Eligibility
      "Eligibility (6a): Eligibility was assessed in every week of the person-week skeleton. ",
      "Consecutive weeks were then grouped into enrollment periods of ",
      pw_weeks,
      ", and each period defined one sequential trial. ",
      "A person could be eligible in some weeks of a period and not in others. ",
      "Individuals entered the pool of eligible person-trials if they met the inclusion criteria (calendar year range, age) and had not met any exclusion criterion ",
      "(e.g., no prior intervention within the specified washout window, no prior outcome event within the lookback window or over the lifetime, as defined in the specification). ",
      "Exclusion criteria were evaluated cumulatively, and the number of persons and person-trials remaining after each criterion was recorded for the participant flow diagram. ",
      # 7b: Treatment strategies
      "Treatment strategies (6b): Treatment status was determined from registry data in every week of the person-week skeleton. ",
      "The treatment variable and its values came from the study configuration. ",
      "Arm assignment within a period used only the weeks in which the person was eligible and on one of the two protocol arms. ",
      "A person entered the intervention arm if at least one of those weeks was on the intervention treatment. ",
      "A person entered the comparator arm if all of those weeks were on the comparator treatment. ",
      "A person with no such week was ineligible for that period's trial and entered neither arm. ",
      "Initiation occurring anywhere within the period was attributed to its start. ",
      "The enrollment period width, ",
      pw_weeks,
      ", determines the granularity of sequential trial entry. ",
      "Narrower periods reduce residual immortal time bias, at the cost of fewer eligible individuals per trial (Caniglia et al., 2023). ",
      "No grace period was implemented. ",
      "The period provides slack for the timing of initiation at enrollment only. ",
      "Deviation from the assigned strategy censored per-protocol follow-up at the first period off that strategy. ",
      # 7c: Assignment
      assign_paragraph(
        lead = "Assignment (6c): ",
        alternative = "The alternative keeps every eligible non-initiator and adjusts with inverse probability weighting alone (Danaei et al., 2013). ",
        computation_line = "The draw bounds the computation for a large registry dataset. ",
        ipw_line = "Inverse probability weighting on the covariates taken at the recruiting week then adjusted for confounding. "
      ),
      # 7d: Follow-up
      "Follow-up (6d): Follow-up began at the start of the enrollment period in which an individual met eligibility and intervention criteria ",
      "and ended at the earliest of the outcome event, protocol deviation (treatment switching), loss to follow-up, administrative censoring, or the pre-specified maximum follow-up duration. ",
      # 7e: Outcomes
      "Outcomes (6e): Outcome events were identified from registry data using the variables specified in the study configuration. ",
      "An event was recorded at the first time period in which the outcome indicator was observed. ",
      # 7f: Causal contrasts. Design register: this prints before s2 and s3
      # run, so it says what the design plans, not what was estimated.
      "Causal contrasts (6f): The design specifies ",
      est_join(est_6f[est_planned]),
      " estimands; ",
      est_join(est_6f[!est_planned]),
      " analyses are not planned. ",
      paste(est_how[est_planned], collapse = ""),
      # 7g: Confounders
      "Confounders (6g): Baseline confounders were measured at the start of each sequential trial. ",
      "For computed confounders (e.g., rolling-window indicators), values were derived from the specified source variable over the lookback window preceding trial entry. ",
      "Missing baseline confounder values were singly imputed at trial entry by the plan's impute_fn; the default is a single hot-deck draw from the observed distribution of that confounder. ",
      "Missing time-updated confounder values were carried forward from the last observed value within each person-trial, seeded from the entry value. ",
      "A person-trial with no observed value after entry kept its imputed entry value through follow-up. ",
      "The count of filled rows and person-trials was reported per enrollment by tteenrollment_fill_summary(). ",
      # 7h: Analysis
      "Analysis (6h): The analysis followed the two-stage weighting approach described in items 6c and 6f. ",
      "It combined baseline inverse probability of treatment weights with time-varying censoring weights for the per-protocol estimand ",
      "(Hern\u00e1n and Robins, 2016; Danaei et al., 2013)."
    )
  )

  # --- RESULTS ---
  cat(strrep("\u2500", 59), "\n")
  cat(bold("RESULTS"), "\n")
  cat(strrep("\u2500", 59), "\n\n")

  # Item 8: auto-populate from the stored attrition rows if available.
  # `.attrition_overall()` is the one aggregation rule, shared with the
  # CONSORT diagram and the attrition sheet. It reads the global rows, and
  # each of those already counts across every trial. A sum over both sets
  # counts every person-trial of that criterion twice.
  #
  # A PER-STEP LINE CARRIES THE TOTAL AND NO ARM COUNT.
  # `.s1_compute_attrition()` re-derives the arm at every cumulative
  # eligibility level, and the arm is `any()` over the weeks that are still
  # eligible. A criterion that makes weeks ineligible can therefore move a
  # person-trial from the intervention arm to the comparator arm, instead of
  # out of the cohort. The difference of two levels is then negative for one
  # arm. Item 8 goes into a paper, so it prints no such difference.
  #
  # Arm counts belong to a LEVEL. This prints them at three levels: the
  # eligible cohort, the comparator draw and the analysis dataset.
  # `R/consort.R` prints the same three.
  item8_text <- NULL
  {
    item8_all <- plan$get_attrition()
    item8_parts <- character()
    flow_complete <- TRUE
    for (enr_id in unique(item8_all$enrollment_id)) {
      ec <- .plan_cohort_counts(plan, enr_id)
      overall <- .attrition_overall(ec$attrition)
      # NULL means one criterion carries no global row, which is the shape
      # of an attrition table written before the global rows existed.
      # All or nothing across enrollments, which is the rule
      # `.attrition_overall()` uses across criteria. Item 8 then prints its
      # placeholder and no participant flow at all. A flow that omits one
      # enrollment reads exactly like a complete flow.
      if (is.null(overall)) {
        flow_complete <- FALSE
        break
      }

      # Compute column widths for right-justified alignment
      all_totals <- overall$n_person_trials
      all_intervention <- overall$n_intervention
      all_comparator <- overall$n_comparator
      deltas_total <- c(0, -diff(all_totals))

      fmt_num <- function(x, w) {
        return(formatC(format(x, big.mark = ","), width = w))
      }
      col_width <- function(vals, deltas) {
        return(max(nchar(format(c(vals, abs(deltas)), big.mark = ","))))
      }
      # The total column carries a per-step difference as well as a level, so
      # it is wide enough for both. No arm difference is printed, so the two
      # arm columns cover the levels alone.
      w_total <- col_width(all_totals, deltas_total)
      w_intervention <- col_width(all_intervention, 0)
      w_comparator <- col_width(all_comparator, 0)

      item8_parts <- c(
        item8_parts,
        paste0("Enrollment '", enr_id, "' participant flow:")
      )

      n_levels <- nrow(overall)
      for (j in seq_len(n_levels)) {
        tot <- all_totals[j]

        if (overall$criterion[j] == "before_exclusions") {
          item8_parts <- c(
            item8_parts,
            "  Before exclusions:",
            sprintf(
              "    \u21b3 %s person-trials",
              cyan(fmt_num(tot, w_total))
            )
          )
          next
        }
        item8_parts <- c(
          item8_parts,
          sprintf(
            "  Applying %s:",
            bold(as.character(overall$criterion[j]))
          ),
          sprintf(
            "    \u21b3 Excluding %s person-trials",
            red(fmt_num(all_totals[j - 1] - tot, w_total))
          )
        )
        # The last level IS the eligible cohort, and the block below prints
        # it with its arm split. A `Remaining` line here would repeat that
        # total on the line above it.
        if (j < n_levels) {
          item8_parts <- c(
            item8_parts,
            sprintf(
              "    \u21b3 Remaining %s person-trials",
              cyan(fmt_num(tot, w_total))
            )
          )
        }
      }

      # The eligible cohort: the last cumulative level, with its arm split.
      item8_parts <- c(
        item8_parts,
        "  Eligible cohort:",
        sprintf(
          "    \u21b3 %s person-trials (%s intervention person-trials, %s comparator person-trials)",
          cyan(fmt_num(all_totals[n_levels], w_total)),
          cyan(fmt_num(all_intervention[n_levels], w_intervention)),
          cyan(fmt_num(all_comparator[n_levels], w_comparator))
        )
      )
      if (!is.null(ec$matching)) {
        m <- ec$matching
        n_int <- sum(m$n_intervention_enrolled, na.rm = TRUE)
        n_cmp <- sum(m$n_comparator_enrolled, na.rm = TRUE)
        n_match_total <- n_int + n_cmp
        item8_parts <- c(
          item8_parts,
          "  After the comparator draw:",
          sprintf(
            "    \u21b3 %s person-trials (%s intervention person-trials, %s comparator person-trials)",
            cyan(fmt_num(n_match_total, w_total)),
            cyan(fmt_num(n_int, w_intervention)),
            cyan(fmt_num(n_cmp, w_comparator))
          )
        )
      }

      # The per-protocol analysis dataset, read through `$get_baselines()`.
      # `$s3_analyze()` stores it, so a plan that stopped after
      # `$s1_generate_enrollments_and_ipw()` holds no panel and Item 8 prints
      # no analysis line. `.baseline_count()` reports an absent panel as
      # `NA`, so the guard tests a true comparison and not a non-NULL value.
      baselines <- plan$get_baselines()
      n_baseline <- .baseline_count(baselines, enr_id, "n_baseline")
      if (isTRUE(n_baseline > 0)) {
        n_base_int <- .baseline_count(
          baselines,
          enr_id,
          "n_baseline_intervention"
        )
        n_base_cmp <- .baseline_count(
          baselines,
          enr_id,
          "n_baseline_comparator"
        )
        # The arm split when the stored panel carries both numbers, and the
        # total alone otherwise. The CONSORT analysis box uses the same rule.
        item8_parts <- c(
          item8_parts,
          "  Analysis dataset (per-protocol):",
          if (!is.na(n_base_int) && !is.na(n_base_cmp)) {
            sprintf(
              "    \u21b3 %s person-trials (%s intervention person-trials, %s comparator person-trials)",
              cyan(fmt_num(n_baseline, w_total)),
              cyan(fmt_num(n_base_int, w_intervention)),
              cyan(fmt_num(n_base_cmp, w_comparator))
            )
          } else {
            sprintf(
              "    \u21b3 %s person-trials",
              cyan(fmt_num(n_baseline, w_total))
            )
          }
        )
      }
    }
    if (flow_complete && length(item8_parts) > 0) {
      item8_text <- paste(item8_parts, collapse = "\n")
    }
  }
  if (is.null(item8_text)) {
    item8_text <- "Run $s1_generate_enrollments_and_ipw() first to populate attrition counts."
  }
  item(
    "8",
    NULL,
    "Participant selection (flow diagram).",
    "Provide a flow diagram or description of participant selection.",
    item8_text
  )

  item(
    "9",
    NULL,
    "Baseline data.",
    "Report baseline characteristics for each treatment group.",
    "Available via TTEEnrollment$table1(ipw_col)."
  )

  item(
    "10",
    NULL,
    "Follow-up summary.",
    "Report summary measures of follow-up time.",
    "Available via TTEEnrollment$summary(pretty = TRUE)."
  )

  item(
    "11",
    NULL,
    "Missing data.",
    "Report the amount of missing data and methods used to handle it.",
    paste0(
      "Missing entry value: single imputation by the plan's impute_fn; ",
      "the default performs one hot-deck draw via $s1_impute_confounders().\n",
      "Missing follow-up value: carried forward from the last observed value ",
      "via $s1b_fill_followup_confounders().\n",
      "Counts of filled rows and person-trials per enrollment: ",
      "tteenrollment_fill_summary()."
    )
  )

  item(
    "12",
    NULL,
    "Outcome frequencies.",
    "Report outcome event counts and rates.",
    "Available via TTEEnrollment$rates(weight_col)."
  )

  item(
    "13",
    NULL,
    "Effect estimates.",
    "Report estimated effects with confidence intervals.",
    paste0(
      "Relative effect: TTEEnrollment$irr(weight_col). ",
      "Absolute effect: TTEEnrollment$risk_difference(weight_col), the risk difference and the number needed to treat."
    )
  )

  item(
    "14",
    NULL,
    "Sensitivity analyses.",
    "Report results of any sensitivity analyses."
  )

  # --- DISCUSSION ---
  cat(strrep("\u2500", 59), "\n")
  cat(bold("DISCUSSION"), "\n")
  cat(strrep("\u2500", 59), "\n\n")

  item(
    "15",
    NULL,
    "Interpretation.",
    "Interpret results considering the study objectives, limitations, and context."
  )

  item(
    "16",
    NULL,
    "Limitations.",
    "Discuss limitations, including potential sources of bias and unmeasured confounding."
  )

  # --- OTHER ---
  cat(strrep("\u2500", 59), "\n")
  cat(bold("OTHER"), "\n")
  cat(strrep("\u2500", 59), "\n\n")

  for (num in 17:21) {
    titles <- c(
      "Ethics approval.",
      "Study registration.",
      "Data availability.",
      "Funding.",
      "Conflicts of interest."
    )
    item(
      as.character(num),
      NULL,
      titles[num - 16],
      "Report as per standard guidelines."
    )
  }

  return(invisible(NULL))
}


#' Print the stored-results diagnostic
#'
#' The body of `TTEPlan$results_summary()`. It reports on the CACHE and
#' never on a number.
#'
#' @param plan A `TTEPlan`.
#' @return The plan, invisibly.
#' @noRd
.plan_results_summary <- function(plan) {
  if (is.null(plan$results_ett) || length(plan$results_ett) == 0L) {
    cat("No ETT results stored. Run $s3_analyze() first.\n")
    return(invisible(plan))
  }

  rows <- lapply(names(plan$results_ett), function(ett_id) {
    r <- plan$results_ett[[ett_id]]
    n_events <- if (!is.null(r$summary)) r$summary$n_events else NA
    irr_status <- if (is.null(r$irr_pp_trunc)) {
      "NULL"
    } else if (isTRUE(r$irr_pp_trunc$skipped)) {
      paste0("SKIP: ", r$irr_pp_trunc$reason)
    } else {
      "OK"
    }
    rates_status <- if (is.null(r$rates_pp_trunc)) {
      "NULL"
    } else if (isTRUE(r$rates_pp_trunc$skipped)) {
      "SKIP"
    } else {
      "OK"
    }
    return(data.table::data.table(
      enrollment = r$enrollment_id,
      ett_id = ett_id,
      description = r$description,
      n_events = n_events,
      irr = irr_status,
      rates = rates_status
    ))
  })
  dt <- data.table::rbindlist(rows)
  print(dt, nrows = Inf)

  # Enrollment summary
  if (!is.null(plan$results_enrollment)) {
    cat(sprintf(
      "\nEnrollment results: %d/%d computed\n",
      length(plan$results_enrollment),
      length(unique(plan$ett$enrollment_id))
    ))
  }
  return(invisible(plan))
}


#' The rule clause the TARGET checklist appends to one criterion line
#'
#' The checklist is manuscript prose, so a washout of type `no_prior_value` or
#' `only_prior_value` states what it does. A criterion that is not a washout
#' adds nothing.
#'
#' @param impl An implementation list.
#' @return A single string, empty when the implementation is not a washout.
#' @noRd
.tte_checklist_rule <- function(impl) {
  rule <- .tte_washout_prose(impl)
  if (is.null(rule)) {
    return("")
  }
  return(paste0(", rule: ", rule))
}
