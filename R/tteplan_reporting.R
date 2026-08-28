# Reporting for a TTEPlan: the console specification summary, the TARGET
# reporting checklist and the stored-results diagnostic.
#
# Each function here holds the whole body of the public method that carries
# its name, guards included. The method in `R/r6_tteplan_print.R`,
# `R/r6_tteplan_checklist.R` or `R/r6_tteplan_results.R` is a one-call
# delegate to it.

#' Print the target trial specification summary
#'
#' The body of `TTEPlan$print_spec_summary()`.
#'
#' @param plan A `TTEPlan`.
#' @return `invisible(NULL)`.
#' @noRd
.plan_print_spec_summary <- function(plan) {
  spec <- plan$spec
  if (is.null(spec)) {
    stop("plan has no spec", call. = FALSE)
  }

  # ANSI color/style helpers
  bold <- function(x) paste0("\033[1m", x, "\033[0m")
  green <- function(x) paste0("\033[92m", x, "\033[0m")
  cyan <- function(x) paste0("\033[36m", x, "\033[0m")
  magenta <- function(x) paste0("\033[95m", x, "\033[0m")
  yellow <- function(x) paste0("\033[93m", x, "\033[0m")

  # Build code lookup if registry available
  cl <- .build_code_lookup(plan, colorize = TRUE)
  code_lookup <- cl$lookup
  fmt_var <- cl$fmt_var

  cat("=== Target Trial Specification ===\n")
  if (!is.null(code_lookup)) {
    cat("\n")
    cat("  Color   Meaning\n")
    cat(
      "  ",
      green("green"),
      "   Variable defined by a statistician (hardcoded in skeleton)\n",
      sep = ""
    )
    cat(
      "  ",
      cyan("cyan"),
      "    Variable auto-generated from ",
      magenta("registered codes"),
      "\n",
      sep = ""
    )
    cat(
      "  ",
      magenta("magenta"),
      " Registered diagnosis/medication codes (ICD-10, ATC, etc.)\n",
      sep = ""
    )
    cat(
      "  ",
      yellow("yellow"),
      "  Category levels / arm values\n",
      sep = ""
    )
    cat("\n")
  }
  # Helper: print a bold label padded to 17 chars
  lbl <- function(label) {
    padded <- formatC(label, width = -17, flag = "-")
    return(bold(padded))
  }

  impl <- spec$study$implementation
  cat(lbl("Title:"), spec$study$title, "\n", sep = "")
  if (!is.null(spec$study$design)) {
    cat(lbl("Design:"), spec$study$design, "\n", sep = "")
  }
  cat(lbl("PI:"), spec$study$principal_investigator, "\n", sep = "")
  if (!is.null(impl$date)) {
    cat(lbl("Date:"), impl$date, "\n", sep = "")
  }
  if (!is.null(impl$status)) {
    cat(lbl("Status:"), impl$status, "\n", sep = "")
  }
  cat(lbl("Version:"), impl$version, "\n", sep = "")
  # RegistryStudy + nested Skeletons + TTEPlan
  if (!is.null(plan$registry_study_created_at)) {
    cat(
      lbl("RegistryStudy:"),
      format(plan$registry_study_created_at, "%Y-%m-%d %H:%M:%S"),
      "\n",
      sep = ""
    )
  }

  # Skeletons (nested under RegistryStudy)
  n_skeletons <- length(plan$skeleton_files)
  n_expected <- plan$expected_skeleton_file_count
  skel_detail <- if (!is.null(n_expected) && n_skeletons != n_expected) {
    sprintf(
      "%d / %d expected \033[31m** WARNING: incomplete **\033[0m",
      n_skeletons,
      n_expected
    )
  } else if (!is.null(n_expected)) {
    sprintf("%d / %d expected", n_skeletons, n_expected)
  } else {
    sprintf("%d files", n_skeletons)
  }
  skel_label <- bold(formatC(
    " \u2514\u2500 Skeletons:",
    width = -17,
    flag = "-"
  ))
  if (!is.null(plan$skeleton_created_at)) {
    cat(
      skel_label,
      format(plan$skeleton_created_at, "%Y-%m-%d %H:%M:%S"),
      " (",
      skel_detail,
      ")\n",
      sep = ""
    )
  } else {
    cat(skel_label, "(", skel_detail, ")\n", sep = "")
  }

  if (!is.null(plan$created_at)) {
    cat(
      lbl("TTEPlan:"),
      format(plan$created_at, "%Y-%m-%d %H:%M:%S"),
      "\n",
      sep = ""
    )
  }
  if (!is.null(plan$expected_n_ids)) {
    cat(
      lbl("Individuals:"),
      format(plan$expected_n_ids, big.mark = ","),
      " (expected)\n",
      sep = ""
    )
  }
  if (!is.null(plan$global_max_isoyearweek)) {
    cat(
      lbl("Admin censoring:"),
      plan$global_max_isoyearweek,
      " (isoyear-isoweek)\n",
      sep = ""
    )
  }

  cat("\n")

  # Follow-up
  cat(bold("Follow-up:"), "\n")
  for (fu in spec$follow_up) {
    cat(sprintf("  - %s (%d weeks)\n", fu$label, fu$weeks))
  }
  cat("\n")

  # Inclusion criteria. The `criteria` container applies to every enrollment,
  # so it prints once here and never inside the enrollment loop below.
  cat(bold("Inclusion criteria (global):"), "\n")
  iso <- spec$inclusion_criteria$isoyears
  cat("  Isoyears: ", iso[1], "-", iso[2], "\n", sep = "")
  for (ic in spec[["inclusion_criteria"]][["criteria"]] %||% list()) {
    cat("  -", ic$name, "\n")
    cat(
      "    Variable:   ",
      fmt_var(
        ic$implementation$source_variable_combined %||%
          ic$implementation$source_variable
      ),
      "\n"
    )
    cat(
      "    Window:     ",
      .tte_inclusion_window_human(ic$implementation),
      "\n"
    )
  }
  cat("\n")

  # Exclusion criteria
  cat(bold("Exclusion criteria (global):"), "\n")
  for (ec in spec$exclusion_criteria) {
    cat("  -", ec$name, "\n")
    cat(
      "    Variable:   ",
      fmt_var(
        ec$implementation$source_variable_combined %||%
          ec$implementation$source_variable
      ),
      "\n"
    )
    cat("    Window:     ", .format_window_human(ec$implementation), "\n")
  }
  cat("\n")

  # Confounders
  cat(bold("Confounders:"), "\n")
  for (conf in spec$confounders) {
    cimpl <- conf$implementation
    cat("  -", conf$name, "\n")
    if (isTRUE(cimpl$computed)) {
      derived <- cimpl$variable %||%
        paste0(
          "rd_no_",
          cimpl$source_variable_combined %||% cimpl$source_variable,
          "_",
          .window_label(cimpl$window_weeks)
        )
      cat(
        "    Variable:   ",
        derived,
        "<-",
        fmt_var(cimpl$source_variable_combined %||% cimpl$source_variable),
        "\n"
      )
      cat("    Window:     ", .format_window_human(cimpl), "\n")
    } else {
      cat("    Variable:   ", fmt_var(cimpl$variable), "\n")
    }
    if (!is.null(conf$categories)) {
      cat(
        "    Categories: ",
        yellow(paste(conf$categories, collapse = ", ")),
        "\n"
      )
    }
  }
  cat("\n")

  # Outcomes
  cat(bold("Outcomes:"), "\n")
  for (out in spec$outcomes) {
    cat("  -", out$name, "\n")
    cat("    Variable:   ", fmt_var(out$implementation$variable), "\n")
  }
  cat("\n")

  # Enrollments
  cat(bold("Enrollments:"), "\n")
  for (enr in spec$enrollments) {
    cat(sprintf("  %s\n", bold(paste0(enr$id, ": ", enr$name))))

    # Treatment sub-block
    tx <- enr$treatment
    cat("    Treatment:\n")
    cat(sprintf(
      "      %-34s%s\n",
      "Variable:",
      fmt_var(tx$implementation$variable)
    ))
    cat(sprintf(
      "      %-34s%s <- %s\n",
      "Intervention:",
      tx$arms$intervention,
      yellow(tx$implementation$intervention_value)
    ))
    cat(sprintf(
      "      %-34s%s <- %s\n",
      "Comparator:",
      tx$arms$comparator,
      yellow(tx$implementation$comparator_value)
    ))
    # The spec's number sizes the comparator side of the draw, so it
    # prints on the left of the colon. The label names both sides, because
    # the bare digits read either way.
    cat(sprintf(
      "      %-34s%d:1\n",
      "Comparator-to-intervention ratio:",
      tx$implementation$comparator_to_intervention_ratio
    ))

    # Additional inclusion
    if (!is.null(enr$additional_inclusion)) {
      cat("    Additional inclusion:\n")
      for (ai in enr$additional_inclusion) {
        if (identical(ai$type, "age_range")) {
          cat(sprintf("      %-18s%d-%d\n", "Age range:", ai$min, ai$max))
        } else if (identical(ai$type, "has_event")) {
          cat("      -", ai$name, "\n")
          cat(
            "        Variable:    ",
            fmt_var(
              ai$implementation$source_variable_combined %||%
                ai$implementation$source_variable
            ),
            "\n"
          )
          cat(
            "        Window:      ",
            .format_window_human(ai$implementation),
            "\n"
          )
        } else {
          cat("      -", ai$name, "\n")
        }
      }
    }

    # Additional exclusion
    if (!is.null(enr$additional_exclusion)) {
      cat("    Additional exclusion:\n")
      for (ae in enr$additional_exclusion) {
        cat("      -", ae$name, "\n")
        cat(
          "        Variable:    ",
          fmt_var(
            ae$implementation$source_variable_combined %||%
              ae$implementation$source_variable
          ),
          "\n"
        )
        cat(
          "        Window:      ",
          .format_window_human(ae$implementation),
          "\n"
        )
      }
    }
  }

  cat("\n")

  return(invisible(NULL))
}


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
  assign_text <- paste0(
    "Comparator individuals entered by incidence density sampling within each sequential trial. ",
    "The draw ran from a stated seed. ",
    stratum_text,
    "Every intervention individual entered its trial. ",
    paste(assign_parts, collapse = " "),
    " Where a trial held fewer comparator individuals than that, the draw took all of them. ",
    no_pairing_text,
    "Inverse probability weighting then adjusted for confounding by the remaining measured covariates, taken at the recruiting week."
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
  item(
    "6",
    "f",
    "Describe the causal contrasts (estimands).",
    "Specify the causal estimand (e.g., intention-to-treat, per-protocol).",
    "Supported: Per-protocol (IPW + IPCW-PP). Not supported: ITT (pipeline censors at protocol deviation), As-treated (requires time-varying IPW)."
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
      "Censoring probabilities were modelled using a generalized additive model with a smooth function of follow-up time and sequential trial indicators, ",
      "conditional on baseline covariates, and fitted separately for the intervention and comparator arms. ",
      "Stabilization used marginal (population-average) censoring probabilities as the numerator. ",
      "The primary outcome model was a weighted Poisson regression (quasipoisson family) ",
      "with a natural cubic spline for follow-up time (3 degrees of freedom), sequential trial indicators to adjust for calendar time, ",
      "and a person-time offset, fitted via survey-weighted generalized linear models with person-level clustered standard errors. ",
      "Extreme weights were truncated at the 1st and 99th percentiles after each weighting step to reduce the influence of near-violations of the positivity assumption."
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
      "Assignment (6c): Comparator individuals entered by incidence density sampling within each sequential trial. ",
      "The alternative keeps every eligible non-initiator and adjusts with inverse probability weighting alone (Danaei et al., 2013). ",
      "The draw ran from a stated seed. ",
      stratum_text,
      paste(assign_parts, collapse = " "),
      " Where a trial held fewer comparator individuals than that, the draw took all of them. ",
      no_pairing_text,
      "The draw bounds the computation for a large registry dataset. ",
      "Inverse probability weighting on the covariates taken at the recruiting week then adjusted for confounding. ",
      # 7d: Follow-up
      "Follow-up (6d): Follow-up began at the start of the enrollment period in which an individual met eligibility and intervention criteria ",
      "and ended at the earliest of the outcome event, protocol deviation (treatment switching), loss to follow-up, administrative censoring, or the pre-specified maximum follow-up duration. ",
      # 7e: Outcomes
      "Outcomes (6e): Outcome events were identified from registry data using the variables specified in the study configuration. ",
      "An event was recorded at the first time period in which the outcome indicator was observed. ",
      # 7f: Causal contrasts
      "Causal contrasts (6f): The per-protocol effect was estimated by censoring individuals at the time of treatment switching ",
      "and applying inverse probability of censoring weights to adjust for the potential informativeness of this censoring. ",
      "Intention-to-treat and as-treated analyses were not conducted. ",
      # 7g: Confounders
      "Confounders (6g): Baseline confounders were measured at the start of each sequential trial. ",
      "For computed confounders (e.g., rolling-window indicators), values were derived from the specified source variable over the lookback window preceding trial entry. ",
      "Missing confounder values were imputed by sampling from the observed distribution of that confounder across person-trials. ",
      # 7h: Analysis
      "Analysis (6h): The analysis followed the two-stage weighting approach described in items 6c and 6h, ",
      "combining baseline inverse probability of treatment weights with time-varying inverse probability of censoring weights for the per-protocol estimand."
    )
  )

  # --- RESULTS ---
  cat(strrep("\u2500", 59), "\n")
  cat(bold("RESULTS"), "\n")
  cat(strrep("\u2500", 59), "\n\n")

  # Item 8: auto-populate from the stored attrition rows if available.
  # `$get_attrition()` returns every stored row, per-trial and global, so
  # this reads the same rows the raw table held.
  item8_text <- NULL
  {
    item8_all <- plan$get_attrition()
    item8_parts <- character()
    for (enr_id in unique(item8_all$enrollment_id)) {
      ec <- .plan_cohort_counts(plan, enr_id)
      if (!is.null(ec$attrition)) {
        att <- ec$attrition
        # Aggregate across trial_ids for overall counts
        overall <- att[,
          .(
            n_person_trials = sum(n_person_trials),
            n_intervention = sum(n_intervention),
            n_comparator = sum(n_comparator)
          ),
          by = criterion
        ]
        # Preserve criterion order from attrition (before_exclusions first)
        overall[,
          criterion := factor(criterion, levels = unique(criterion))
        ]
        data.table::setorder(overall, criterion)

        # Compute column widths for right-justified alignment
        all_totals <- overall$n_person_trials
        all_intervention <- overall$n_intervention
        all_comparator <- overall$n_comparator
        deltas_total <- c(0, -diff(all_totals))
        deltas_intervention <- c(0, -diff(all_intervention))
        deltas_comparator <- c(0, -diff(all_comparator))

        fmt_num <- function(x, w) {
          return(formatC(format(x, big.mark = ","), width = w))
        }
        col_width <- function(vals, deltas) {
          return(max(nchar(format(c(vals, abs(deltas)), big.mark = ","))))
        }
        w_total <- col_width(all_totals, deltas_total)
        w_intervention <- col_width(all_intervention, deltas_intervention)
        w_comparator <- col_width(all_comparator, deltas_comparator)

        item8_parts <- c(
          item8_parts,
          paste0("Enrollment '", enr_id, "' participant flow:")
        )

        for (j in seq_len(nrow(overall))) {
          tot <- all_totals[j]
          n_int <- all_intervention[j]
          n_cmp <- all_comparator[j]

          if (overall$criterion[j] == "before_exclusions") {
            item8_parts <- c(
              item8_parts,
              "  Before exclusions:",
              sprintf(
                "    \u21b3 %s person-trials",
                cyan(fmt_num(tot, w_total))
              )
            )
          } else {
            d_tot <- all_totals[j - 1] - tot
            d_intervention <- all_intervention[j - 1] - n_int
            d_comparator <- all_comparator[j - 1] - n_cmp
            item8_parts <- c(
              item8_parts,
              sprintf(
                "  Applying %s:",
                bold(as.character(overall$criterion[j]))
              ),
              sprintf(
                "    \u21b3 Excluding %s person-trials (%s intervention person-trials, %s comparator person-trials)",
                red(fmt_num(d_tot, w_total)),
                red(fmt_num(d_intervention, w_intervention)),
                red(fmt_num(d_comparator, w_comparator))
              ),
              sprintf(
                "    \u21b3 Remaining %s person-trials (%s intervention person-trials, %s comparator person-trials)",
                cyan(fmt_num(tot, w_total)),
                cyan(fmt_num(n_int, w_intervention)),
                cyan(fmt_num(n_cmp, w_comparator))
              )
            )
          }
        }
      }
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
    }
    if (length(item8_parts) > 0) {
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
    "Confounder imputation via $s1_impute_confounders() (sampling from observed)."
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
    "Available via TTEEnrollment$irr(weight_col)."
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
