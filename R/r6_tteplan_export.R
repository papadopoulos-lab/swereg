# These TTEPlan methods write output. One produces the results workbook and
# one produces the ordered exhibit set. Both are one-call delegates. The
# bodies are plain functions in `R/tteplan_export.R`, together with the two
# exhibit producers that `$export()` dispatches to.

#' @include r6_tteplan.R
#' @description Export analysis results to an Excel workbook.
#'
#' Requires `self$results_enrollment` and `self$results_ett` to be populated
#' (run `$s3_analyze()` first).
#'
#' If the cached baseline tables were produced by an older version of
#' `swereg` (when Table 1 was a `tableone` object), they are automatically
#' refreshed in-process via `$recompute_baselines()` using the analysis
#' files in `output_dir`.
#'
#' The workbook carries no forest plot. The `PP results` and `ITT results`
#' sheets already report every emulated trial with counts, rates, ratios,
#' risk differences, intervals and numbers needed to treat. A forest image
#' repeated a subset of those numbers. `$export()` still draws one for a
#' manuscript.
#'
#' @param path File path for the output `.xlsx` file.
#' @param table1_enrollment Enrollment ID for Table 1 (main baseline table).
#'   Default: the enrollment with the most baseline observations.
#' @param protocol_ett_id Optional character(1) ETT id. The
#'   `Target trial protocol` sheet describes this one emulated trial. An id
#'   the plan does not hold raises a warning and falls back. When `NULL`
#'   (default), the sheet describes the first ETT of the Table 1
#'   enrollment, and otherwise the first ETT in the grid.
#' @param output_dir Optional directory holding the cached `.qs2` files.
#'   Used by the lazy `recompute_baselines()` refresh. Defaults to
#'   `self$output_dir`.
TTEPlan$set(
  "public",
  "export_tables",
  function(
    path = NULL,
    table1_enrollment = NULL,
    protocol_ett_id = NULL,
    output_dir = NULL
  ) {
    return(.plan_export_tables(
      self,
      path,
      table1_enrollment,
      protocol_ett_id,
      output_dir
    ))
  }
)

#' @description Produce an ORDERED set of exhibits (figures and/or tables)
#' from a manifest and write them to `dir` with two-digit order prefixes, so
#' the manifest order becomes the exhibit numbering. This is the single
#' programmatic entry point: a project declares its exhibit set once and
#' hands it over; other projects reuse the same driver with a different
#' manifest. Each spec's `type` routes it to a producer:
#' \describe{
#'   \item{figures}{`"survival"` (weighted survival curve for one ETT cell,
#'     one image per estimand), `"forest"` (forest plot over a named
#'     `exposures` set, one image per estimand), and `"consort"` (CONSORT
#'     flow diagram for an enrollment).}
#'   \item{tables}{`"table1"` (baseline characteristics for an enrollment,
#'     written as CSV).}
#' }
#' Full per-type fields are documented on the `.plan_export_figure()` and
#' `.plan_export_table()` producers in `R/tteplan_export.R`.
#'
#' Two `"forest"` and `"survival"` fields carry a decision worth stating
#' here, because both are silent when they go wrong.
#'
#' `"survival"` is drawn on the CUMULATIVE-FAILURE scale, which is one
#' minus survival. A y-axis window is therefore meaningless until it says
#' which scale it is measured on, so `ylim` requires a companion
#' `ylim_scale`, either `"survival"` or `"cumulative_failure"`. A
#' survival-scale window is translated onto the plotted scale:
#' `c(0.95, 1)` becomes `c(0, 0.05)` and shows the same band of the figure
#' it always did. An undeclared window is an error, not a guess. Left
#' undeclared and applied as given, a survival-scale window clips the whole
#' cumulative-failure curve out of view and produces a blank panel with no
#' error and no warning.
#'
#' `"forest"` takes `risk_difference = TRUE` to SHOW the signed
#' cause-specific risk difference per 10,000 people, with its interval.
#' The option computes nothing. `$s3_analyze()` computes the risk
#' difference for every ETT and stores it, so this switch only decides
#' whether the figure carries the two extra columns.
#'
#' The `n_boot`, `seed` and `conf_level` fields are inert and warn.
#' `$s3_analyze()` fixes `n_boot` and `seed`. It reads the confidence level
#' from `study$implementation$conf_level`, so a study sets its level once
#' and every result and header carries it. A figure that could restate the
#' level would print a label the numbers do not have.
#' @param manifest A non-empty list of exhibit specs. Every spec needs a
#'   `type`; other fields depend on the type. Optional `label` (filename
#'   stem) and `title`.
#' @param dir Output directory. Defaults to `self$dir_results`.
#' @return Character vector of all written paths (invisibly).
TTEPlan$set("public", "export", function(manifest, dir = NULL) {
  return(.plan_export(self, manifest, dir))
})
