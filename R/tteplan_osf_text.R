# =============================================================================
# tteplan_osf_text
# =============================================================================

#' The fields of the OSF Preregistration form that the spec answers
#'
#' The names are the OSF field headings. A name with a slash is a subheading of
#' one OSF field, as the cross-references of the text write it.
#' @noRd
.OSF_FIELDS <- c(
  "Study design",
  "Sample size",
  "Measured variables/Exposures",
  "Measured variables/Outcomes",
  "Measured variables/Baseline covariates",
  "Statistical models",
  "Transformations",
  "Inference criteria",
  "Data inclusion and exclusion",
  "Missing data",
  "Other planned analysis",
  "Code list"
)

#' The address of the swereg methods article
#' @noRd
.OSF_METHODS_URL <- "https://papadopoulos-lab.github.io/swereg/articles/tte-methods.html"

#' Write the OSF preregistration text that a spec determines
#'
#' Writes the text of each field of the OSF Preregistration form whose content
#' the study specification and swereg fix: the design, the sampling, the
#' treatment strategies, the outcomes, the covariates, the eligibility
#' criteria, the analysis, the minimum-events rule and the code list. Each fact
#' appears in one field.
#' The other fields point to it as `'Field/Subheading'`.
#'
#' The function reads the spec and the code registry only. It reads no
#' skeleton, rawbatch or registry data file, so it runs before s0. Every spec
#' entry appears in the text: no criterion, covariate or outcome is left out.
#'
#' The text uses the `name` of each criterion, covariate and outcome, the
#' `arms` and `description` of each treatment, and the `description` of each
#' outcome, as the spec writes them. Write those fields for a reader of the
#' registration. An age range and an ISO-year range are written from their
#' `min` and `max`, the values that run, and not from their `name`. The
#' categories of each covariate are data values, so they appear in the code
#' list only.
#'
#' Fields that the spec cannot answer are left to the author: the title, the
#' research question and the primary result, the foreknowledge of the data,
#' the data sources and the study population, and the ethics approval.
#'
#' @param spec_path Character(1), the path of the spec YAML. The function
#'   reads it with [tteplan_read_spec()].
#' @param code_registry The code registry that resolves each variable to its
#'   codes, as in [tteplan_excel_spec_summary()]: `NULL`, a [RegistryStudy],
#'   or a `data.table` with the columns of `RegistryStudy$summary_table()`.
#'   With `NULL`, every code cell of the code list reads `not resolved`.
#' @param period_width Integer, the width in weeks of the enrollment period.
#'   It has the default of [tteplan_from_spec_and_registrystudy()].
#' @param global_max_isoyearweek Character(1) or `NULL`, the administrative
#'   censoring boundary as an isoyearweek, for example `"2025-01"`. Follow-up
#'   includes that week. `NULL` writes "the administrative end of the data".
#' @param people Character(2), the singular and plural noun for a study
#'   participant, for example `c("woman", "women")`.
#' @return A named character vector. Each element is the Markdown text of one
#'   field, and the names are the OSF field headings: `Study design`,
#'   `Sample size`, `Measured variables/Exposures`,
#'   `Measured variables/Outcomes`, `Measured variables/Baseline covariates`,
#'   `Statistical models`, `Transformations`, `Inference criteria`,
#'   `Data inclusion and exclusion`, `Missing data`, `Other planned analysis`
#'   and `Code list`. `Inference criteria` states the minimum-events rule
#'   from `min_expected_events_per_arm_per_year`.
#'
#' @examples
#' spec_dir <- tempfile()
#' dir.create(spec_dir)
#' spec_path <- file.path(spec_dir, "spec_v001.yaml")
#' writeLines(
#'   c(
#'     "study:",
#'     "  title: Example study",
#'     "  implementation:",
#'     "    project_prefix: example",
#'     "    version: v001",
#'     "inclusion_criteria:",
#'     "  isoyears: [2010, 2020]",
#'     "exclusion_criteria:",
#'     "  - name: Prior oophorectomy",
#'     "    implementation:",
#'     "      source_variable: os_oophorectomy",
#'     "      window: lifetime_before_baseline",
#'     "confounders:",
#'     "  - name: Age",
#'     "    implementation:",
#'     "      variable: rd_age_continuous",
#'     "outcomes:",
#'     "  - name: Outcome",
#'     "    implementation:",
#'     "      variable: osd_outcome",
#'     "follow_up:",
#'     "  - {label: 1 year, weeks: 52}",
#'     "min_expected_events_per_arm_per_year: 5",
#'     "enrollments:",
#'     "  - id: '01'",
#'     "    name: A vs B",
#'     "    observed_var: {sentinel: row_presence}",
#'     "    intervention_tolerance_weeks: 0",
#'     "    comparator_tolerance_weeks: 0",
#'     "    additional_inclusion:",
#'     "      - {name: Age 50-59, type: age_range, min: 50, max: 59}",
#'     "    treatment:",
#'     "      arms: {intervention: A, comparator: B}",
#'     "      implementation:",
#'     "        variable: rd_tx",
#'     "        intervention_value: a",
#'     "        comparator_value: b",
#'     "        comparator_to_intervention_ratio: 2",
#'     "        seed: 1"
#'   ),
#'   spec_path
#' )
#' osf <- tteplan_osf_text(spec_path)
#' cat(osf[["Data inclusion and exclusion"]])
#' unlink(spec_dir, recursive = TRUE)
#'
#' @family tte_spec
#' @seealso [tteplan_excel_spec_summary()], `vignette("tte-methods")`
#' @export
tteplan_osf_text <- function(
  spec_path,
  code_registry = NULL,
  period_width = 4L,
  global_max_isoyearweek = NULL,
  people = c("person", "persons")
) {
  if (!is.character(people) || length(people) != 2L || anyNA(people)) {
    stop(
      "`people` must be two strings: the singular and the plural noun.",
      call. = FALSE
    )
  }
  spec <- tteplan_read_spec(spec_path)
  ctx <- list(
    spec = spec,
    codes = .osf_code_map(.tte_spec_only_code_registry(code_registry)),
    width = as.integer(period_width),
    admin = global_max_isoyearweek,
    one = people[[1L]],
    many = people[[2L]],
    comparison = .osf_comparison_numbers(spec)
  )
  out <- c(
    .osf_study_design(ctx),
    .osf_sample_size(ctx),
    .osf_exposures(ctx),
    .osf_outcomes(ctx),
    .osf_covariates(ctx),
    .osf_models(ctx),
    .osf_transformations(ctx),
    .osf_inference(ctx),
    .osf_eligibility(ctx),
    .osf_missing(ctx),
    .osf_other(ctx),
    .osf_code_list(ctx)
  )
  names(out) <- .OSF_FIELDS
  return(out)
}


#' Map each generated column of the code registry to its codes
#'
#' @param st `NULL`, or the summary table of a code registry.
#' @return A named character vector, or `NULL`.
#' @noRd
.osf_code_map <- function(st) {
  if (is.null(st) || nrow(st) == 0L) {
    return(NULL)
  }
  out <- character()
  for (i in seq_len(nrow(st))) {
    cols <- strsplit(st$generated_columns[i], ", ", fixed = TRUE)[[1L]]
    out[cols] <- st$codes[i]
  }
  return(out)
}


#' Number the enrollments 1, 2, ... in spec order
#'
#' The text names a comparison by its number, never by its enrollment id.
#' @noRd
.osf_comparison_numbers <- function(spec) {
  ids <- vapply(spec$enrollments, function(e) as.character(e$id), character(1))
  return(stats::setNames(seq_along(ids), ids))
}


#' The window of a criterion or covariate, as a phrase
#'
#' @param impl An `implementation` list.
#' @return A phrase such as "at any time before baseline", or `NULL` for an
#'   entry without a window.
#' @noRd
.osf_window <- function(impl) {
  w <- impl[["window"]]
  if (
    is.character(w) &&
      !w %in%
        c("lifetime_before_baseline", "lifetime_before_and_after_baseline")
  ) {
    w <- impl[["window_weeks"]]
  }
  w <- w %||% impl[["window_weeks"]]
  if (is.null(w)) {
    return(NULL)
  }
  if (identical(w, "lifetime_before_and_after_baseline")) {
    return("at any time before or after baseline")
  }
  if (
    identical(w, "lifetime_before_baseline") ||
      (is.numeric(w) && is.infinite(w))
  ) {
    return("at any time before baseline")
  }
  w <- as.numeric(w)
  years <- w / 52
  if (years >= 1 && years == round(years)) {
    return(
      if (years == 1) {
        "in the year before baseline"
      } else {
        paste0("in the ", years, " years before baseline")
      }
    )
  }
  return(paste0("in the ", w, " weeks before baseline"))
}


#' One criterion or covariate as a sentence: its name, then its window
#'
#' @param entry A spec entry with `name` and `implementation`.
#' @param window Logical, whether to state the window.
#' @noRd
.osf_entry <- function(entry, window = TRUE) {
  nm <- trimws(as.character(
    entry$name %||% .protocol_impl_variable(entry$implementation)
  ))
  wp <- if (window) .osf_window(entry$implementation) else NULL
  return(paste0(nm, if (!is.null(wp)) paste0(", ", wp), "."))
}


#' A Markdown numbered list
#' @noRd
.osf_numbered <- function(x) {
  return(paste0(seq_along(x), ". ", x, collapse = "\n"))
}


#' A Markdown bulleted list
#' @noRd
.osf_bullets <- function(x) {
  return(paste0("- ", x, collapse = "\n"))
}


#' Join paragraphs with blank lines, dropping empty ones
#' @noRd
.osf_paragraphs <- function(...) {
  x <- unlist(list(...))
  x <- x[!is.na(x) & nzchar(x)]
  return(paste(x, collapse = "\n\n"))
}


#' Join items as "a", "a and b", or "a, b and c"
#' @noRd
.osf_and <- function(x) {
  if (length(x) <= 1L) {
    return(paste(x, collapse = ""))
  }
  return(paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)]))
}


#' The last day of an ISO week, as "5 January 2025"
#'
#' @param isoyearweek Character(1), "YYYY-WW".
#' @noRd
.osf_isoweek_sunday <- function(isoyearweek) {
  y <- as.integer(substr(isoyearweek, 1L, 4L))
  wk <- as.integer(substr(isoyearweek, 6L, 7L))
  jan4 <- as.Date(sprintf("%d-01-04", y))
  monday1 <- jan4 - (as.integer(format(jan4, "%u")) - 1L)
  d <- monday1 + 7L * (wk - 1L) + 6L
  return(paste(
    as.integer(format(d, "%d")),
    month.name[as.integer(format(d, "%m"))],
    format(d, "%Y")
  ))
}


#' The arm labels of an enrollment
#' @noRd
.osf_arms <- function(enr) {
  arms <- enr$treatment$arms
  return(c(
    intervention = as.character(
      arms$intervention %||% enr$treatment$implementation$intervention_value
    ),
    comparator = as.character(
      arms$comparator %||% enr$treatment$implementation$comparator_value
    )
  ))
}


#' "comparison 1 (A versus B)"
#' @noRd
.osf_comparison_label <- function(ctx, eid) {
  for (enr in ctx$spec$enrollments) {
    if (identical(as.character(enr$id), as.character(eid))) {
      a <- .osf_arms(enr)
      return(paste0(
        "comparison ",
        ctx$comparison[[as.character(eid)]],
        " (",
        a[["intervention"]],
        " versus ",
        a[["comparator"]],
        ")"
      ))
    }
  }
  return(paste0("comparison ", eid))
}


#' @noRd
.osf_study_design <- function(ctx) {
  yrs <- ctx$spec$inclusion_criteria$isoyears
  horizons <- vapply(
    ctx$spec$follow_up,
    function(f) paste0(f$label, " (", f$weeks, " weeks)"),
    character(1)
  )
  admin <- if (is.null(ctx$admin)) {
    "the administrative end of the data"
  } else {
    paste0(
      "the administrative end of the data, at the end of ISO week ",
      ctx$admin,
      " (",
      .osf_isoweek_sunday(ctx$admin),
      ")"
    )
  }
  tol <- vapply(
    ctx$spec$enrollments,
    function(e) {
      i <- as.numeric(e$intervention_tolerance_weeks %||% 0)
      c <- as.numeric(e$comparator_tolerance_weeks %||% 0)
      if (i == 0 && c == 0) {
        return(NA_character_)
      }
      return(paste0(
        "In ",
        .osf_comparison_label(ctx, e$id),
        ", the tolerance is ",
        i,
        " weeks in the intervention arm and ",
        c,
        " weeks in the comparator arm."
      ))
    },
    character(1)
  )
  tol <- tol[!is.na(tol)]
  return(.osf_paragraphs(
    paste0(
      "The study emulates a sequence of target trials in register data. Calendar time from ISO year ",
      yrs[[1L]],
      " to ISO year ",
      yrs[[2L]],
      " is cut into consecutive enrollment periods of ",
      ctx$width,
      " weeks, and each enrollment period opens one trial. Eligibility is assessed in every week. A ",
      ctx$one,
      " can enter more than one trial."
    ),
    paste0(
      "Each ",
      ctx$one,
      " is assigned to a treatment strategy from the weeks of the enrollment period, as described under 'Measured variables/Exposures'."
    ),
    paste0(
      "Time zero is the first week after the enrollment period closes, in both arms. Only ",
      ctx$many,
      " who reach time zero under observation and free of every outcome listed under 'Measured variables/Outcomes' enter the trial."
    ),
    paste0(
      "Follow-up starts at time zero. It stops at the first of: the outcome; loss of observation, which is the first week after time zero without observation; ",
      admin,
      "; and the end of the analysis horizon. Death censors follow-up, and no competing-risk model is fitted. The horizons are ",
      .osf_and(horizons),
      "."
    ),
    paste0(
      "Under the per-protocol estimand, follow-up also stops at the first week that does not hold the assigned strategy, beyond the tolerance of the arm. A week with a missing treatment value counts as such a week. Under the intention-to-treat estimand, a deviation does not stop follow-up."
    ),
    if (length(tol) > 0L) {
      paste(tol, collapse = " ")
    } else {
      "The tolerance is 0 weeks in every arm, so the first such week is a deviation."
    },
    "The statistical analysis is described under 'Statistical models'."
  ))
}


#' @noRd
.osf_sample_size <- function(ctx) {
  ratios <- vapply(
    ctx$spec$enrollments,
    function(e) {
      return(as.numeric(
        e$treatment$implementation$comparator_to_intervention_ratio
      ))
    },
    numeric(1)
  )
  ratio_text <- if (length(unique(ratios)) == 1L) {
    paste0(
      ratios[[1L]],
      " for each enrolled ",
      ctx$one,
      " in the intervention arm"
    )
  } else {
    paste0(
      "a number for each enrolled ",
      ctx$one,
      " in the intervention arm that depends on the comparison (",
      paste0("comparison ", seq_along(ratios), ": ", ratios, collapse = "; "),
      ")"
    )
  }
  return(.osf_paragraphs(
    paste0(
      "In each comparison and each trial, every eligible ",
      ctx$one,
      " in the intervention arm is enrolled. Comparators are drawn at random from the eligible ",
      ctx$many,
      " in the comparator arm: ",
      ratio_text,
      ", or all of them if the trial holds fewer. The draw is made within each trial and creates no matched sets, so no analysis conditions on matched sets."
    ),
    paste0(
      "A ",
      ctx$one,
      " can enter several trials, so the number of person-trials can exceed the number of ",
      ctx$many,
      ". The number of person-trials in each comparison follows from this rule and the data."
    )
  ))
}


#' @noRd
.osf_exposures <- function(ctx) {
  items <- vapply(
    ctx$spec$enrollments,
    function(e) {
      a <- .osf_arms(e)
      d <- e$treatment$description
      return(paste0(
        a[["intervention"]],
        " versus ",
        a[["comparator"]],
        ".",
        if (!is.null(d) && nzchar(trimws(d))) paste0(" ", trimws(d))
      ))
    },
    character(1)
  )
  return(.osf_paragraphs(
    paste0(
      "The study has ",
      length(items),
      if (length(items) == 1L) " comparison" else " comparisons",
      ". Each compares an intervention strategy, named first, with a comparator strategy."
    ),
    .osf_numbered(items),
    paste0(
      "Assignment uses only the weeks of the enrollment period in which the ",
      ctx$one,
      " is eligible and holds one of the two compared strategies. A ",
      ctx$one,
      " enters the intervention arm when at least one of those weeks holds the intervention strategy, and the comparator arm when all of them hold the comparator strategy. A ",
      ctx$one,
      " with no such week does not enter that trial. Eligibility is assessed week by week, so a treatment started earlier in the enrollment period can make a later week ineligible."
    )
  ))
}


#' @noRd
.osf_outcomes <- function(ctx) {
  items <- vapply(
    ctx$spec$outcomes,
    function(o) {
      d <- o$description
      return(paste0(
        trimws(o$name),
        if (!is.null(d) && nzchar(trimws(d))) paste0(": ", trimws(d)) else "."
      ))
    },
    character(1)
  )
  return(.osf_paragraphs(
    .osf_numbered(items),
    "Each outcome counts its first occurrence at or after time zero. The codes are listed under 'Code list'."
  ))
}


#' @noRd
.osf_covariates <- function(ctx) {
  confs <- ctx$spec$confounders
  items <- vapply(
    confs,
    function(conf) {
      return(.osf_entry(conf, window = isTRUE(conf$implementation$computed)))
    },
    character(1)
  )
  sparse <- if (!is.null(ctx$spec$rare_condition_rule)) {
    "A rare condition can become an exclusion criterion instead of a covariate, by the rule under 'Data inclusion and exclusion'."
  }
  return(.osf_paragraphs(
    .osf_bullets(items),
    paste0(
      "Each covariate is measured at the recruiting week: the first week of the enrollment period in which the ",
      ctx$one,
      " is eligible and holds one of the two compared strategies."
    ),
    paste0(
      if (is.null(ctx$spec$rare_condition_rule)) {
        "Every covariate"
      } else {
        "Every covariate that the rare-condition rule keeps"
      },
      " enters both weight models. The treatment weights use its value at the recruiting week. The censoring weights use its value in each follow-up interval, together with the time since time zero and calendar time. Calendar time does not enter the treatment weights. The outcome model holds no confounder. It adjusts for the trial, which is calendar time at time zero, as described under 'Statistical models'."
    ),
    sparse
  ))
}


#' @noRd
.osf_models <- function(ctx) {
  ver <- as.character(utils::packageVersion("swereg"))
  return(.osf_paragraphs(
    "Each comparison is analysed separately, for each outcome and each horizon.",
    paste0(
      "The analysis uses the R package swereg, version ",
      ver,
      " at registration. The version used will be reported with the results. swereg carries out sequential target trial emulation as its methods article describes (",
      .OSF_METHODS_URL,
      "). Any change to the method between registration and analysis will be recorded in the deviations log."
    ),
    paste0(
      "The analysis estimates the ",
      .osf_and(unname(.TTE_ESTIMANDS)),
      " effects. The treatment weights are stabilized inverse probability of treatment weights, from a logistic regression of the assigned arm on the covariates. The per-protocol analysis adds stabilized inverse probability of censoring weights, from three models in each arm: loss of observation, deviation, and deviation at time zero. The intention-to-treat weight, and the per-protocol product of the treatment and censoring weights, are truncated at the 1st and 99th percentiles."
    ),
    paste0(
      "The outcome model is a weighted quasi-Poisson regression of the outcome on the treatment, the time since time zero and the trial, with a person-time offset. It holds no confounder, so the exponentiated treatment coefficient is the marginal incidence rate ratio. Standard errors are clustered on the ",
      ctx$one,
      ", because a ",
      ctx$one,
      " can enter several trials. The absolute effect is the risk difference at the end of each horizon, with a percentile interval from ",
      .S3_RD_N_BOOT,
      " bootstrap replicates that resample ",
      ctx$many,
      ". Each effect is reported with its ",
      100 * .s3_conf_level(ctx$spec),
      "% confidence interval."
    ),
    paste0(
      "Covariate balance is reported as standardized mean differences before and after weighting, together with the distribution of the weights."
    ),
    "The study-specific inputs are given elsewhere in this registration: the enrollment period and follow-up under 'Study design', the sampling under 'Sample size', the treatment strategies under 'Measured variables/Exposures' and the covariates under 'Measured variables/Baseline covariates'."
  ))
}


#' @noRd
.osf_transformations <- function(ctx) {
  return(.osf_paragraphs(
    "In each comparison, the treatment is coded 1 for the intervention strategy and 0 for the comparator strategy. Each outcome is a binary indicator.",
    paste0(
      "Follow-up is split into intervals of ",
      ctx$width,
      " weeks from time zero. The outcome model and the loss and deviation censoring models carry the person-time of each interval as an offset. The treatment model and the time-zero censoring model are logistic regressions on one row per person-trial."
    ),
    "Covariates enter the weight models as main effects, with no interaction and no spline. A numeric covariate enters as one linear term, and a binary covariate as one term. A categorical covariate enters as indicator variables, with its first level as the reference; the choice of reference does not change the fitted weights. The categories are listed under 'Code list'.",
    "The time since time zero, the trial and calendar time each enter as a spline, as a factor, or not at all, chosen by the number of distinct values, as section 1.8.9 of the methods article describes."
  ))
}


#' The inclusion criteria of one enrollment, as sentences
#' @noRd
.osf_enrollment_inclusion <- function(enr) {
  return(vapply(
    enr$additional_inclusion %||% list(),
    function(ai) {
      type <- ai$type %||% ""
      if (identical(type, "age_range")) {
        return(paste0("Age ", ai$min, " to ", ai$max, " years."))
      }
      if (identical(type, "isoyear_range")) {
        return(paste0("Trial entry in ISO years ", ai$min, " to ", ai$max, "."))
      }
      return(.osf_entry(ai))
    },
    character(1)
  ))
}


#' Criteria that every enrollment shares, and those of each enrollment
#'
#' @param ctx The context list.
#' @param f A function from an enrollment to its criteria sentences.
#' @return A list with `common` and `specific`, both character vectors.
#' @noRd
.osf_split_common <- function(ctx, f) {
  per <- lapply(ctx$spec$enrollments, f)
  common <- Reduce(intersect, per)
  specific <- unlist(lapply(seq_along(per), function(k) {
    own <- setdiff(per[[k]], common)
    if (length(own) == 0L) {
      return(character())
    }
    return(paste0(
      "In ",
      .osf_comparison_label(ctx, ctx$spec$enrollments[[k]]$id),
      " only: ",
      own
    ))
  }))
  return(list(
    common = common %||% character(),
    specific = specific %||% character()
  ))
}


#' @noRd
.osf_eligibility <- function(ctx) {
  spec <- ctx$spec
  yrs <- spec$inclusion_criteria$isoyears
  incl_enr <- .osf_split_common(ctx, .osf_enrollment_inclusion)
  incl <- c(
    paste0("Trial entry in ISO years ", yrs[[1L]], " to ", yrs[[2L]], "."),
    vapply(
      spec$inclusion_criteria$criteria %||% list(),
      .osf_entry,
      character(1)
    ),
    incl_enr$common,
    incl_enr$specific
  )
  excl_enr <- .osf_split_common(ctx, function(enr) {
    return(vapply(
      enr$additional_exclusion %||% list(),
      .osf_entry,
      character(1)
    ))
  })
  excl <- c(
    vapply(spec$exclusion_criteria %||% list(), .osf_entry, character(1)),
    excl_enr$common,
    excl_enr$specific
  )
  return(.osf_paragraphs(
    paste0(
      "Each criterion is assessed in each week of the enrollment period. A look-back window counts the calendar weeks before the week assessed, not that week itself. 'At any time before baseline' means from the start of the data."
    ),
    "Inclusion criteria:",
    .osf_numbered(incl),
    "Exclusion criteria:",
    .osf_numbered(excl),
    .osf_sparse_rule(ctx),
    paste0(
      "No ",
      ctx$one,
      " is excluded for an extreme value of a measured variable. Extreme weights are truncated, as described under 'Statistical models'."
    )
  ))
}


#' The minimum-events rule, as the paragraphs of the Inference criteria field
#'
#' The text states `min_expected_events_per_arm_per_year`, which
#' `tteplan_read_spec()` requires. With a threshold of 0, one sentence says
#' that the rule withholds no estimate.
#' @noRd
.osf_inference <- function(ctx) {
  value <- ctx$spec[["min_expected_events_per_arm_per_year"]]
  k <- format(value, trim = TRUE, scientific = FALSE)
  first <- paste0(
    "A prespecified reporting rule, the minimum-events rule, requires at least ",
    k,
    " expected events per arm in each year of follow-up"
  )
  null_def <- "The expected events of an arm in a year are computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms."
  if (value == 0) {
    return(paste0(first, ", so it withholds no estimate. ", null_def))
  }
  return(.osf_paragraphs(
    paste0(
      first,
      ". Follow-up is cut into years of ",
      .HORIZON_WINDOW_WEEKS,
      " weeks from time zero. The last year ends at the horizon, so it can be shorter. ",
      null_def,
      " The reportable horizon is the end of the last year before the first year that fails."
    ),
    paste0(
      "When any year up to the horizon fails the rule, the estimates of that estimand are withheld. Its incidence rate ratio, risk difference and number needed to treat are reported as NE, not estimable.",
      if (length(ctx$spec$subgroups %||% list()) > 0L) {
        " Its subgroup rate ratios and interaction tests are withheld too."
      },
      " Events, person-time and rates are still reported."
    ),
    "Which years pass depends on the observed events, through the pooled rate. The rule is a reporting rule and does not guarantee precision or coverage."
  ))
}


#' The rare-condition rule, as a paragraph, or NULL without a
#' rare_condition_rule
#' @noRd
.osf_sparse_rule <- function(ctx) {
  rule <- ctx$spec$rare_condition_rule
  if (is.null(rule)) {
    return(NULL)
  }
  drivers <- vapply(
    rule$decided_by,
    function(eid) .osf_comparison_label(ctx, eid),
    character(1)
  )
  return(paste0(
    "Rare conditions: each covariate below is adjusted for only if at least ",
    rule$min_n,
    " ",
    ctx$many,
    " hold the condition at baseline in the smaller arm of ",
    .osf_and(drivers),
    ". Otherwise ",
    ctx$many,
    " with the condition are excluded from every comparison. The rule is ",
    .TTE_SPARSE_BASIS,
    ".\n\n",
    .osf_bullets(.tte_sparse_candidate_names(list(spec = ctx$spec)))
  ))
}


#' @noRd
.osf_other <- function(ctx) {
  subs <- vapply(
    ctx$spec$subgroups %||% list(),
    function(g) trimws(as.character(g$name %||% g$implementation$variable)),
    character(1)
  )
  return(.osf_paragraphs(
    "The per-protocol analysis is repeated with untruncated weights, and the result is reported beside the truncated one.",
    if (length(subs) > 0L) {
      paste0(
        "Subgroup analyses estimate the rate ratio within each level of ",
        .osf_and(subs),
        ", with a Wald test of the interaction of each subgroup with treatment."
      )
    }
  ))
}


#' @noRd
.osf_missing <- function(ctx) {
  return(.osf_paragraphs(
    paste0(
      "A missing covariate value at the recruiting week is replaced by one value drawn at random from the observed values of that covariate (single hot-deck imputation). During follow-up, a missing covariate value is carried forward from the last observed value of the person-trial. A missing treatment value removes that week from the assignment in the enrollment period. During follow-up it is handled as 'Study design' describes. The rules are those of section 1.8.7 of the methods article (",
      .OSF_METHODS_URL,
      ")."
    ),
    if (length(ctx$spec$subgroups %||% list()) > 0L) {
      "A missing subgroup value removes the row from the analyses of that subgroup."
    },
    "The number of missing values of each baseline covariate will be reported before imputation."
  ))
}


#' The codes of one or more variables, from the code registry
#' @noRd
.osf_codes <- function(ctx, vars) {
  vars <- unlist(strsplit(as.character(unlist(vars)), "__", fixed = TRUE))
  if (is.null(ctx$codes)) {
    return("not resolved")
  }
  out <- vapply(
    vars,
    function(v) {
      return(
        if (v %in% names(ctx$codes)) ctx$codes[[v]] else "derived variable"
      )
    },
    character(1)
  )
  return(paste(unique(out), collapse = "; "))
}


#' Text on one line: line breaks removed
#' @noRd
.osf_one_line <- function(x) {
  return(trimws(gsub("[\r\n]+", " ", as.character(x))))
}


#' The code-list items of the enrollment criteria that read a variable
#'
#' An age or ISO-year range reads no code, so it has no item. A criterion that
#' every enrollment shares gets one item; any other names its comparisons.
#' @noRd
.osf_enrollment_code_rows <- function(ctx, row) {
  out <- character()
  n_enr <- length(ctx$spec$enrollments)
  for (use in c("additional_inclusion", "additional_exclusion")) {
    label <- if (use == "additional_inclusion") "inclusion" else "exclusion"
    entries <- list()
    owners <- list()
    for (enr in ctx$spec$enrollments) {
      for (e in enr[[use]] %||% list()) {
        if ((e$type %||% "") %in% c("age_range", "isoyear_range")) {
          next
        }
        key <- paste(
          use,
          .osf_entry(e),
          .protocol_impl_variable(e$implementation)
        )
        if (!key %in% names(entries)) {
          entries[[key]] <- e
          owners[[key]] <- character()
        }
        owners[[key]] <- c(
          owners[[key]],
          as.character(ctx$comparison[[as.character(enr$id)]])
        )
      }
    }
    for (key in names(entries)) {
      own <- unique(owners[[key]])
      use_label <- if (length(own) == n_enr) {
        label
      } else {
        paste0(label, " in comparison ", .osf_and(own))
      }
      out <- c(
        out,
        row(
          entries[[key]],
          use_label,
          .protocol_impl_variable(entries[[key]]$implementation)
        )
      )
    }
  }
  return(out)
}


#' @noRd
.osf_code_list <- function(ctx) {
  spec <- ctx$spec
  row <- function(entry, use, vars, window = TRUE, categories = NULL) {
    wp <- if (window) .osf_window(entry$implementation) else NULL
    codes <- .osf_codes(ctx, vars)
    cats <- unlist(categories)
    return(paste0(
      "- ",
      .osf_one_line(entry$name %||% ""),
      " (",
      use,
      "). ",
      if (identical(codes, "derived variable")) {
        "Derived variable, no codes."
      } else {
        paste0("Codes: ", .osf_one_line(codes), ".")
      },
      if (!is.null(wp)) paste0(" Window: ", wp, "."),
      if (length(cats) > 0L) {
        paste0(
          " Categories: ",
          .osf_one_line(paste(cats, collapse = ", ")),
          "."
        )
      }
    ))
  }
  rows <- c(
    lapply(spec$inclusion_criteria$criteria %||% list(), function(e) {
      return(row(e, "inclusion", .protocol_impl_variable(e$implementation)))
    }),
    lapply(spec$exclusion_criteria %||% list(), function(e) {
      return(row(e, "exclusion", .protocol_impl_variable(e$implementation)))
    }),
    .osf_enrollment_code_rows(ctx, row),
    lapply(spec$confounders %||% list(), function(e) {
      computed <- isTRUE(e$implementation$computed)
      vars <- if (computed) {
        e$implementation$source_variable %||%
          .protocol_impl_variable(e$implementation)
      } else {
        .protocol_impl_variable(e$implementation)
      }
      return(row(
        e,
        "covariate",
        vars,
        window = computed,
        categories = e$categories
      ))
    }),
    lapply(spec$outcomes, function(e) {
      return(row(e, "outcome", e$implementation$variable, window = FALSE))
    })
  )
  return(.osf_paragraphs(
    paste(unlist(rows), collapse = "\n"),
    "Each code matches every code that starts with it. A code that starts with ! is removed from the codes before it."
  ))
}
