# =============================================================================
# tteplan_excel_spec_summary
# =============================================================================

#' The value of each workbook cell that a spec cannot supply
#'
#' A plan reads these values from the skeletons. The spec-only workbook has no
#' skeleton, so the cell says that.
#' @noRd
.TTE_SPEC_ONLY <- "not built (spec only)"

#' Write the specification workbook from a spec, without a plan
#'
#' Writes the Study Specification sheet and the Target trial protocol sheet
#' from the spec file and a code registry. The function reads no skeleton,
#' rawbatch or registry data file, so it runs before s0.
#'
#' Each cell holds the value that `TTEPlan$excel_spec_summary()` writes for a
#' plan at s0 from the same spec, code registry, `period_width` and
#' `protocol_ett_id`. The one exception is the `Admin censoring:` row. s0
#' reads that value from the first skeleton unless you supply it. Here the
#' row reads `not built (spec only)` unless you supply
#' `global_max_isoyearweek`.
#'
#' Only s1 decides the rare-condition rule. The workbook therefore states the
#' rule as not decided yet, and it has no Rare conditions sheet. The
#' function builds the ETT grid with the code of
#' [tteplan_from_spec_and_registrystudy()], so the protocol sheet documents
#' the same ETT for the same `protocol_ett_id`.
#'
#' @param spec_path Character(1), the path of the spec YAML. The function
#'   reads it with [tteplan_read_spec()].
#' @param path Character(1), the output `.xlsx` path. The function overwrites
#'   an existing file.
#' @param code_registry The code registry that resolves each variable to its
#'   codes. One of these:
#'   * `NULL`. Every variable is written as unresolved, as for a plan that
#'     has no code registry.
#'   * A [RegistryStudy]. The function uses `$summary_table()` and reads no
#'     data.
#'   * A `data.table` with the columns of `RegistryStudy$summary_table()`:
#'     `name`, `codes`, `label` and `generated_columns`.
#' @param period_width Integer, the width in weeks of the enrollment period.
#'   It has the default of [tteplan_from_spec_and_registrystudy()]. The
#'   Target trial protocol sheet states it.
#' @param global_max_isoyearweek Character(1) or `NULL`, the administrative
#'   censoring boundary as an isoyearweek, for example `"2023-52"`. `NULL`
#'   writes `not built (spec only)` in the `Admin censoring:` row.
#' @param protocol_ett_id Character(1) or `NULL`, the ETT that the Target
#'   trial protocol sheet documents. `NULL` takes the first ETT of the grid.
#'   An id that the grid does not hold gives a warning, and the sheet then
#'   documents the first ETT.
#' @return `path`, invisibly.
#'
#' @examples
#' spec_dir <- tempfile()
#' dir.create(spec_dir)
#' spec_path <- file.path(spec_dir, "spec_v001.yaml")
#' writeLines(
#'   c(
#'     "study:",
#'     "  title: Example study",
#'     "  principal_investigator: Example PI",
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
#' study <- RegistryStudy$new(data_rawbatch_dir = spec_dir)
#' study$register_codes(
#'   codes = list(oophorectomy = c("LAE20", "LAE21", "LAF10")),
#'   fn = swereg::add_operations,
#'   groups = list(os = c("inpatient", "outpatient"))
#' )
#' out <- tteplan_excel_spec_summary(
#'   spec_path = spec_path,
#'   path = file.path(spec_dir, "spec_v001.xlsx"),
#'   code_registry = study
#' )
#' openxlsx::getSheetNames(out)
#' unlink(spec_dir, recursive = TRUE)
#'
#' @family tte_spec
#' @seealso [tteplan_from_spec_and_registrystudy()], [TTEPlan]
#' @export
tteplan_excel_spec_summary <- function(
  spec_path,
  path,
  code_registry = NULL,
  period_width = 4L,
  global_max_isoyearweek = NULL,
  protocol_ett_id = NULL
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package 'openxlsx' is required. Install with: install.packages('openxlsx')",
      call. = FALSE
    )
  }
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be one file path.", call. = FALSE)
  }
  spec <- tteplan_read_spec(spec_path)

  plan <- TTEPlan$new(
    project_prefix = spec$study$implementation$project_prefix %||%
      NA_character_,
    skeleton_files = .TTE_SPEC_ONLY,
    global_max_isoyearweek = global_max_isoyearweek %||% .TTE_SPEC_ONLY
  )
  plan$period_width <- as.integer(period_width)
  plan$spec <- spec
  plan$spec_version <- spec$study$implementation$version
  plan$code_registry <- .tte_spec_only_code_registry(code_registry)
  .tteplan_add_spec_etts(plan, spec)

  protocol_ett_id <- .plan_protocol_ett_id(plan, protocol_ett_id)
  wb <- openxlsx::createWorkbook()
  .write_spec_summary(wb, plan)
  .write_protocol_table(wb, "Target trial protocol", plan, protocol_ett_id)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  message("Spec summary saved to: ", path)
  return(invisible(path))
}


#' Normalise the code registry argument to a summary table, or to NULL
#'
#' @param code_registry `NULL`, a [RegistryStudy], or a `data.table` with the
#'   columns of `RegistryStudy$summary_table()`.
#' @return A `data.table`, or `NULL`.
#' @noRd
.tte_spec_only_code_registry <- function(code_registry) {
  if (is.null(code_registry)) {
    return(NULL)
  }
  if (inherits(code_registry, "RegistryStudy")) {
    return(code_registry$summary_table())
  }
  needed <- c("name", "codes", "label", "generated_columns")
  if (!data.table::is.data.table(code_registry)) {
    stop(
      "`code_registry` must be NULL, a RegistryStudy, or a data.table with ",
      "the columns ",
      paste(needed, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  missing <- setdiff(needed, names(code_registry))
  if (length(missing) > 0L) {
    stop(
      "`code_registry` lacks the columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  return(code_registry)
}
