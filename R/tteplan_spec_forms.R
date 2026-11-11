# =============================================================================
# The rule forms of a study specification
# =============================================================================
# `.TTE_SPEC_SCHEMA` names every key a specification may carry. It says
# nothing about what a rule built from those keys computes. This file holds
# that second list: every form a rule can take, the windows it accepts, and
# what the specification cannot express at all.
#
# Each form carries a YAML example and one worked case. The case is a small
# panel of eight ISO weeks with the value of each source column, and the
# eligibility column the form MUST write from it. Three things read the
# registry:
#
#   vignettes/tte-spec-schema.Rmd    renders every table of its section
#                                    "What each rule computes" from it, and
#                                    runs every case through the real reader
#                                    and compiler to print the result
#   test-tteplan-spec-forms.R        checks the registry against the types
#                                    the code dispatches on, parses every
#                                    example in every block it names, and
#                                    checks every case against its result
#   CLAUDE.md                        states the rule: a new form, window or
#                                    limit goes into this file first
#
# A form the code accepts and this file omits fails the test. So does a case
# whose stated result the code does not produce. A behaviour change therefore
# fails here until the registry, and so the vignette, describes it.

# The eight ISO weeks of every worked case. 2018 has 52 ISO weeks, so the
# panel crosses into 2019 with no week 53.
.TTE_SPEC_FORM_WEEKS <- c(
  "2018-49", "2018-50", "2018-51", "2018-52",
  "2019-01", "2019-02", "2019-03", "2019-04"
)

# The blocks a form can sit in. The name is the key the registry uses, and the
# value is how the vignette writes the path.
.TTE_SPEC_FORM_BLOCKS <- c(
  isoyears = "inclusion_criteria$isoyears",
  criteria = "inclusion_criteria$criteria",
  additional_inclusion = "enrollments[]$additional_inclusion",
  exclusion_criteria = "exclusion_criteria",
  additional_exclusion = "enrollments[]$additional_exclusion",
  confounders = "confounders"
)

# One element per form.
#
#   title       the form's name in the vignette
#   type        the rule type the code dispatches on, or NA for a form that
#               declares none
#   evaluator   the case of the batch evaluator `.tte_apply_eligibility_batch()`
#               that computes the column, or "vectorised" for a form that
#               writes its column without it
#   blocks      every block the example is accepted in, as written. The first
#               one is where the vignette runs the case
#   source      what the source column MUST hold
#   computes    what the column holds in a week
#   unobserved  how a week with NA, or a week with no row, enters the rule
#   column      the name pattern of the column the form writes
#   yaml        the example, as YAML text
#   written     the exact column name the example writes
#   case        `columns`, the source columns over the eight weeks, and
#               `expect`, the column the form MUST write
.TTE_SPEC_RULE_FORMS <- list(
  list(
    id = "calendar_years",
    title = "Calendar years",
    type = NA_character_,
    evaluator = "vectorised",
    blocks = "isoyears",
    source = "The skeleton's `isoyear` column.",
    computes = paste(
      "TRUE in a week whose ISO year lies from the first to the second year,",
      "both included."
    ),
    unobserved = "A week's own ISO year decides. No other week is read.",
    column = "`eligible_isoyears`",
    yaml = "isoyears: [2019, 2019]\n",
    written = "eligible_isoyears",
    case = list(
      columns = list(),
      expect = rep(c(FALSE, TRUE), each = 4L)
    )
  ),
  list(
    id = "has_event",
    title = "An event before baseline",
    type = "has_event",
    evaluator = "windowed",
    blocks = c("criteria", "additional_inclusion"),
    source = "A logical column. A list of columns is combined with OR.",
    computes = paste(
      "TRUE in a week when at least one earlier week inside the window is",
      "TRUE. The week itself is never in its own window."
    ),
    unobserved = paste(
      "A week with no row is not in the window. An NA week follows `any()`:",
      "the result is NA when the window holds an NA week and no TRUE week."
    ),
    column = "`eligible_has_<variable>_<window>`",
    yaml = paste0(
      "name: \"Recorded diagnosis before baseline\"\n",
      "type: has_event\n",
      "implementation:\n",
      "  source_variable: osd_f64\n",
      "  window: lifetime_before_baseline\n"
    ),
    written = "eligible_has_osd_f64_everbefore",
    case = list(
      columns = list(osd_f64 = c(FALSE, FALSE, TRUE, rep(FALSE, 5L))),
      expect = c(FALSE, FALSE, FALSE, rep(TRUE, 5L))
    )
  ),
  list(
    id = "age_range",
    title = "An age band",
    type = "age_range",
    evaluator = "vectorised",
    blocks = "additional_inclusion",
    source = "A numeric column, normally `rd_age_continuous`.",
    computes = paste(
      "TRUE in a week whose value is from `min` to `max`, both included. The",
      "value is compared as stored. On a continuous age, `max: 54` excludes",
      "an age of 54.5."
    ),
    unobserved = "A week's own value decides. No other week is read.",
    column = "`eligible_age`, one per enrollment",
    yaml = paste0(
      "name: \"Age 40-54\"\n",
      "type: age_range\n",
      "min: 40\n",
      "max: 54\n",
      "implementation:\n",
      "  variable: rd_age_continuous\n"
    ),
    written = "eligible_age",
    case = list(
      columns = list(
        rd_age_continuous = c(39.9, 40, 40.5, 50, 54, 54.1, 54.9, 55)
      ),
      expect = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
    )
  ),
  list(
    id = "isoyear_range",
    title = "A calendar range for one enrollment",
    type = "isoyear_range",
    evaluator = "vectorised",
    blocks = "additional_inclusion",
    source = "The skeleton's `isoyear` column.",
    computes = paste(
      "TRUE in a week whose ISO year lies from `min` to `max`, both",
      "included. The range MUST lie inside `inclusion_criteria$isoyears`."
    ),
    unobserved = "A week's own ISO year decides. No other week is read.",
    column = "`eligible_isoyears_<min>_<max>`",
    yaml = paste0(
      "name: \"Trial registration in 2019\"\n",
      "type: isoyear_range\n",
      "min: 2019\n",
      "max: 2019\n"
    ),
    written = "eligible_isoyears_2019_2019",
    case = list(
      columns = list(),
      expect = rep(c(FALSE, TRUE), each = 4L)
    )
  ),
  list(
    id = "no_prior_value",
    title = "No earlier week at a value",
    type = "no_prior_value",
    evaluator = "windowed_no_obs",
    blocks = c(
      "criteria",
      "additional_inclusion",
      "exclusion_criteria",
      "additional_exclusion"
    ),
    source = "One column, compared with `value`. Normally a character column.",
    computes = paste(
      "TRUE in a week when no earlier week inside the window holds `value`.",
      "The week itself is never in its own window."
    ),
    unobserved = paste(
      "A week with no row, and an NA week, hold no value, so neither breaks",
      "the rule."
    ),
    column = "`eligible_no_<variable>_<window>`",
    yaml = paste0(
      "name: \"No earlier treatment\"\n",
      "implementation:\n",
      "  type: no_prior_value\n",
      "  source_variable: rd_tx\n",
      "  value: treated\n",
      "  window: lifetime_before_baseline\n"
    ),
    written = "eligible_no_rd_tx_everbefore",
    case = list(
      columns = list(
        rd_tx = c(
          "control", NA, "control", "treated",
          "control", "control", "control", "control"
        )
      ),
      expect = rep(c(TRUE, FALSE), each = 4L)
    )
  ),
  list(
    id = "only_prior_value",
    title = "Every observed earlier week at a value",
    type = "only_prior_value",
    evaluator = "windowed_only_obs",
    blocks = c(
      "criteria",
      "additional_inclusion",
      "exclusion_criteria",
      "additional_exclusion"
    ),
    source = "One column, compared with `value`. Normally a character column.",
    computes = paste(
      "TRUE in a week when every earlier week inside the window that holds a",
      "value holds `value`. The week itself is never in its own window."
    ),
    unobserved = paste(
      "A week with no row, and an NA week, hold no value, so neither breaks",
      "the rule. A window with no observed week therefore passes: the rule",
      "does not require any week at `value`."
    ),
    column = "`eligible_only_<variable>_<window>`",
    yaml = paste0(
      "name: \"Treated in every week of the 2 weeks before baseline\"\n",
      "implementation:\n",
      "  type: only_prior_value\n",
      "  source_variable: rd_tx\n",
      "  value: treated\n",
      "  window: 2\n"
    ),
    written = "eligible_only_rd_tx_2wk",
    case = list(
      columns = list(
        rd_tx = c(
          "treated", "treated", "control", "treated",
          "treated", "treated", NA, "treated"
        )
      ),
      expect = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
    )
  ),
  list(
    id = "event_exclusion",
    title = "No event before baseline",
    type = NA_character_,
    evaluator = "windowed",
    blocks = c("exclusion_criteria", "additional_exclusion"),
    source = "A logical column. A list of columns is combined with OR.",
    computes = paste(
      "TRUE in a week when no earlier week inside the window is TRUE. The",
      "week itself is never in its own window."
    ),
    unobserved = paste(
      "A week with no row is not in the window. An NA week follows `any()`:",
      "the result is NA when the window holds an NA week and no TRUE week."
    ),
    column = "`eligible_no_<variable>_<window>`",
    yaml = paste0(
      "name: \"No cancer in the 2 weeks before baseline\"\n",
      "implementation:\n",
      "  source_variable: osd_c\n",
      "  window: 2\n"
    ),
    written = "eligible_no_osd_c_2wk",
    case = list(
      columns = list(osd_c = c(FALSE, FALSE, TRUE, rep(FALSE, 5L))),
      expect = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
    )
  ),
  list(
    id = "person_exclusion",
    title = "No event at any time",
    type = NA_character_,
    evaluator = "lifetime",
    blocks = c("exclusion_criteria", "additional_exclusion"),
    source = paste(
      "A logical column. It MUST NOT be an outcome variable, because this",
      "window reads the weeks after time zero."
    ),
    computes = paste(
      "FALSE in every week of a person when any of the person's weeks is",
      "TRUE, before or after baseline. It removes the whole person."
    ),
    unobserved = "An NA week is skipped (`any(na.rm = TRUE)`).",
    column = "`eligible_no_<variable>_lifetime_before_and_after_baseline`",
    yaml = paste0(
      "name: \"Record flagged in the delivery\"\n",
      "implementation:\n",
      "  source_variable: ri_flagged\n",
      "  window: lifetime_before_and_after_baseline\n"
    ),
    written = "eligible_no_ri_flagged_lifetime_before_and_after_baseline",
    case = list(
      columns = list(ri_flagged = c(rep(FALSE, 7L), TRUE)),
      expect = rep(FALSE, 8L)
    )
  ),
  list(
    id = "any_of_columns",
    title = "Any of several columns",
    type = NA_character_,
    evaluator = "windowed",
    blocks = c("exclusion_criteria", "additional_exclusion"),
    source = paste(
      "A list of logical columns. swereg ORs them into one column named",
      "after all of them, joined by `__`. The same list works in a",
      "`has_event` rule and in a computed confounder. Do not give a washout",
      "a list. `tteplan_read_spec()` accepts it, and the compiler then stops",
      "with a data.table error, because OR does not apply to a character",
      "column."
    ),
    computes = "As for the form that holds the list, on the combined column.",
    unobserved = "As for the form that holds the list.",
    column = "The form's pattern, with `<variable>` the joined name",
    yaml = paste0(
      "name: \"No cancer before baseline, in either register\"\n",
      "implementation:\n",
      "  source_variable: [osd_c, can_c]\n",
      "  window: lifetime_before_baseline\n"
    ),
    written = "eligible_no_osd_c__can_c_everbefore",
    case = list(
      columns = list(
        osd_c = rep(FALSE, 8L),
        can_c = c(FALSE, FALSE, TRUE, rep(FALSE, 5L))
      ),
      expect = rep(c(TRUE, FALSE), times = c(3L, 5L))
    )
  ),
  list(
    id = "computed_confounder",
    title = "A computed confounder",
    type = NA_character_,
    evaluator = "windowed",
    blocks = "confounders",
    source = "A logical column. A list of columns is combined with OR.",
    computes = paste(
      "TRUE in a week when NO earlier week inside the window is TRUE. The",
      "name says so: `rd_no_`. swereg names the column, and a `variable`",
      "key on a computed confounder is replaced. `computed: true` is what",
      "makes it computed. Every rule block accepts the same key, and no rule",
      "reads it."
    ),
    unobserved = paste(
      "A week with no row is not in the window. An NA week follows `any()`:",
      "the result is NA when the window holds an NA week and no TRUE week."
    ),
    column = "`rd_no_<variable>_<window>`",
    yaml = paste0(
      "name: \"Diabetes in the 2 weeks before baseline\"\n",
      "implementation:\n",
      "  computed: true\n",
      "  source_variable: osd_e11\n",
      "  window: 2\n"
    ),
    written = "rd_no_osd_e11_2wk",
    case = list(
      columns = list(osd_e11 = c(FALSE, FALSE, TRUE, rep(FALSE, 5L))),
      expect = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
    )
  )
)


# One element per way of writing a window.
#
#   written   the YAML value
#   weeks     the window in weeks, as `.convert_window()` returns it. NA for
#             the person-level window, which is not converted
#   label     the window part of the column name
#   accepted  where the value is accepted, or "refused"
#   meaning   what the window covers
.TTE_SPEC_WINDOW_FORMS <- list(
  list(
    written = "lifetime_before_baseline",
    weeks = Inf,
    label = "everbefore",
    accepted = "every rule with a window, and every computed confounder",
    meaning = "Every earlier week of the person."
  ),
  list(
    written = 12L,
    weeks = 12L,
    label = "12wk",
    accepted = "every rule with a window, and every computed confounder",
    meaning = paste(
      "The 12 ISO weeks before the week, counted on the calendar and not on",
      "the rows. A bare number is weeks."
    )
  ),
  list(
    written = "2 years",
    weeks = 104L,
    label = "104wk",
    accepted = "every rule with a window, and every computed confounder",
    meaning = paste(
      "A whole number of years, with `year` or `years`. One year is 52",
      "weeks, so the column name gives the weeks."
    )
  ),
  list(
    written = "lifetime_before_and_after_baseline",
    weeks = NA_real_,
    label = "lifetime_before_and_after_baseline",
    accepted = "`exclusion_criteria` and `additional_exclusion`, on a non-washout",
    meaning = "Every week of the person, after baseline included."
  ),
  list(
    written = 12.5,
    weeks = 12L,
    label = "12wk",
    accepted = "every rule with a window, with no message",
    meaning = paste(
      "A fraction is truncated to whole weeks: 12.5 reads as 12. Write a",
      "whole number."
    )
  ),
  list(
    written = "52 weeks",
    weeks = NA_real_,
    label = NA_character_,
    accepted = "refused",
    meaning = "A unit other than `year` is refused. Write the bare number 52."
  ),
  list(
    written = "6 months",
    weeks = NA_real_,
    label = NA_character_,
    accepted = "refused",
    meaning = "Months are refused. Write the weeks as a bare number."
  ),
  list(
    written = "1.5 years",
    weeks = NA_real_,
    label = NA_character_,
    accepted = "refused",
    meaning = "A fraction of a year is refused. Write the weeks, 78."
  )
)

# What the window rules leave out, whatever form holds them.
.TTE_SPEC_WINDOW_DEFAULTS <- c(
  paste(
    "An inclusion rule MAY leave out `window`. It then reads",
    "`lifetime_before_baseline`."
  ),
  paste(
    "An exclusion rule and a computed confounder MUST state `window`.",
    "`tteplan_read_spec()` stops without one."
  ),
  paste(
    "Every window ends at the week before the week it is evaluated in. No",
    "window ends earlier, and none starts at the week itself."
  )
)

# What no form expresses. Each item names the way to get it instead.
.TTE_SPEC_NOT_EXPRESSIBLE <- c(
  paste(
    "**A count or a duration.** No rule counts weeks, so \"at least 12 weeks",
    "of treatment\" has no form. `only_prior_value` over a window of 12",
    "weeks comes closest, with the limit that its row states. Anything",
    "else needs a column built in the skeleton."
  ),
  paste(
    "**AND across columns.** A list of columns is combined with OR only. Two",
    "rules give AND, because every eligibility column is combined with AND."
  ),
  paste(
    "**A threshold on a number.** Only `age_range` compares a number, and",
    "only against `min` and `max`. Any other threshold needs a logical",
    "column built in the skeleton."
  ),
  paste(
    "**A window that ends before the week before baseline**, such as a lag",
    "of 4 weeks. Every window ends at the week before."
  ),
  paste(
    "**Information after baseline**, except the person-level window, which",
    "removes the whole person. No inclusion rule reads a week after the",
    "week it is evaluated in."
  ),
  paste(
    "**A derived treatment variable.** `treatment$implementation$variable`",
    "MUST name a skeleton column, and it is read as it stands."
  ),
  paste(
    "**A derived outcome**, other than a list of outcome columns, which is",
    "combined with OR."
  )
)


#' The specification every worked case is built on
#'
#' One enrollment, ages 40 to 80, study years 2018 and 2019. It carries no
#' exclusion criterion and no computed confounder, so the form under test is
#' the only rule of its kind.
#'
#' @return A specification as an R list, before it is read.
#' @noRd
.tte_spec_form_base <- function() {
  return(list(
    study = list(
      title = "Rule form case",
      design = "Sequential target trial emulation",
      principal_investigator = "rule form case",
      implementation = list(project_prefix = "rulecase", version = "v001")
    ),
    inclusion_criteria = list(isoyears = c(2018L, 2019L)),
    confounders = list(list(
      name = "Age",
      implementation = list(variable = "rd_age_continuous")
    )),
    outcomes = list(list(
      name = "Outcome",
      implementation = list(variable = "osd_outcome")
    )),
    follow_up = list(list(label = "1 year", weeks = 52L)),
    min_expected_events_per_arm_per_year = 0,
    period_width = 4L,
    admin_censoring_isoyearweek = "2019-26",
    enrollments = list(list(
      id = "01",
      name = "Treated vs control",
      observed_var = list(sentinel = "row_presence"),
      intervention_tolerance_weeks = 0L,
      comparator_tolerance_weeks = 0L,
      additional_inclusion = list(list(
        name = "Age range",
        type = "age_range",
        min = 40,
        max = 80,
        implementation = list(variable = "rd_age_continuous")
      )),
      treatment = list(
        arms = list(intervention = "Treated", comparator = "Control"),
        implementation = list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_tx",
          intervention_value = "treated",
          comparator_value = "control",
          seed = 1L
        )
      )
    ))
  ))
}


#' Put one form's example into one block of the base specification
#'
#' @param form One element of `.TTE_SPEC_RULE_FORMS`.
#' @param block One name of `.TTE_SPEC_FORM_BLOCKS`.
#' @return A specification as an R list, before it is read.
#' @noRd
.tte_spec_form_spec <- function(form, block) {
  spec <- .tte_spec_form_base()
  entry <- yaml::yaml.load(form[["yaml"]])
  enr <- spec[["enrollments"]][[1L]]
  if (identical(block, "isoyears")) {
    spec[["inclusion_criteria"]][["isoyears"]] <- entry[["isoyears"]]
  } else if (identical(block, "criteria")) {
    spec[["inclusion_criteria"]][["criteria"]] <- list(entry)
  } else if (identical(block, "additional_inclusion")) {
    # An enrollment takes one age_range, so an age_range example replaces the
    # base band rather than sit beside it.
    keep <- if (identical(entry[["type"]], "age_range")) {
      list()
    } else {
      enr[["additional_inclusion"]]
    }
    enr[["additional_inclusion"]] <- c(keep, list(entry))
  } else if (identical(block, "exclusion_criteria")) {
    spec[["exclusion_criteria"]] <- list(entry)
  } else if (identical(block, "additional_exclusion")) {
    enr[["additional_exclusion"]] <- list(entry)
  } else if (identical(block, "confounders")) {
    spec[["confounders"]] <- c(spec[["confounders"]], list(entry))
  } else {
    stop("Unknown rule form block: ", block, call. = FALSE)
  }
  spec[["enrollments"]][[1L]] <- enr
  return(spec)
}


#' Run one form's worked case through the reader and the compiler
#'
#' The example goes into `block` of the base specification, and
#' `tteplan_read_spec()` reads it from a file. The case's source columns go
#' into a skeleton of one person over `.TTE_SPEC_FORM_WEEKS`. A rule then runs
#' through `tteplan_apply_exclusions()`, and a computed confounder through
#' `tteplan_apply_derived_confounders()`.
#'
#' @param form One element of `.TTE_SPEC_RULE_FORMS`.
#' @param block One of `form$blocks`. Defaults to the first.
#' @return A data.frame with `isoyearweek`, each source column of the case, and
#'   the column the form wrote, under the name `form$written`. Its attribute
#'   `eligible_cols` holds the eligibility columns the compiler combined, or
#'   `NULL` for a confounder.
#' @noRd
.tte_spec_form_run <- function(form, block = form[["blocks"]][[1L]]) {
  dir <- tempfile("tte_spec_form_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(.tte_spec_form_spec(form, block), path)
  spec <- suppressMessages(tteplan_read_spec(path))

  weeks <- .TTE_SPEC_FORM_WEEKS
  skeleton <- data.table::data.table(
    id = 1L,
    isoyear = as.integer(substr(weeks, 1L, 4L)),
    isoyearweek = weeks,
    rd_age_continuous = 50,
    rd_tx = "control",
    osd_outcome = FALSE
  )
  case_cols <- form[["case"]][["columns"]]
  for (nm in names(case_cols)) {
    data.table::set(skeleton, j = nm, value = case_cols[[nm]])
  }

  eligible_cols <- NULL
  if (identical(block, "confounders")) {
    skeleton <- tteplan_apply_derived_confounders(skeleton, spec)
  } else {
    skeleton <- tteplan_apply_exclusions(
      skeleton,
      spec,
      list(enrollment_id = "01")
    )
    eligible_cols <- attr(skeleton, "eligible_cols")
  }
  written <- form[["written"]]
  if (!written %in% names(skeleton)) {
    stop(
      "Rule form '", form[["id"]], "' in block '", block,
      "' wrote no column named ", written,
      call. = FALSE
    )
  }
  keep <- c("isoyearweek", names(case_cols), written)
  out <- as.data.frame(skeleton[, keep, with = FALSE])
  attr(out, "eligible_cols") <- eligible_cols
  return(out)
}


#' The summary table of every rule form, for the vignette
#'
#' @return A data.frame with one row per form.
#' @noRd
.tte_spec_form_table <- function() {
  code <- function(x) {
    return(paste0("`", x, "`"))
  }
  return(data.frame(
    form = vapply(.TTE_SPEC_RULE_FORMS, `[[`, character(1), "title"),
    type = vapply(
      .TTE_SPEC_RULE_FORMS,
      function(f) {
        return(if (is.na(f[["type"]])) "(none)" else code(f[["type"]]))
      },
      character(1)
    ),
    accepted_in = vapply(
      .TTE_SPEC_RULE_FORMS,
      function(f) {
        return(paste(
          code(.TTE_SPEC_FORM_BLOCKS[f[["blocks"]]]),
          collapse = ", "
        ))
      },
      character(1)
    ),
    column = vapply(.TTE_SPEC_RULE_FORMS, `[[`, character(1), "column"),
    stringsAsFactors = FALSE
  ))
}


#' The table of every way of writing a window, for the vignette
#'
#' @return A data.frame with one row per window form.
#' @noRd
.tte_spec_window_table <- function() {
  return(data.frame(
    written = vapply(
      .TTE_SPEC_WINDOW_FORMS,
      function(w) {
        return(paste0("`", format(w[["written"]]), "`"))
      },
      character(1)
    ),
    column_part = vapply(
      .TTE_SPEC_WINDOW_FORMS,
      function(w) {
        return(if (is.na(w[["label"]])) "" else paste0("`", w[["label"]], "`"))
      },
      character(1)
    ),
    accepted = vapply(.TTE_SPEC_WINDOW_FORMS, `[[`, character(1), "accepted"),
    meaning = vapply(.TTE_SPEC_WINDOW_FORMS, `[[`, character(1), "meaning"),
    stringsAsFactors = FALSE
  ))
}
