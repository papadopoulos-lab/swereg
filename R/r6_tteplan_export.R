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
#' The second sheet, `Summary`, holds one row per ETT. It shows these values
#' from the `ITT results` and `PP results` sheets:
#' \itemize{
#'   \item the persons with an event in each arm, for each estimand;
#'   \item the incidence rate ratio and its interval, for each estimand;
#'   \item the per-protocol person-years of each arm, as a percentage of the
#'     intention-to-treat person-years;
#'   \item the per-protocol risk difference per 10,000 and its interval.
#' }
#' Each per-protocol value uses the truncated weights. The header is row 1
#' and carries an autofilter. A `Family` column shows when the spec sets an
#' enrollment `family`.
#'
#' A ratio, risk difference or number needed to treat that the minimum-events
#' rule withheld reads `NE` on every sheet. Its events and person-years stay.
#' The method stops, and names `$s3_analyze()`, when a stored result was
#' judged with a `min_expected_events_per_arm_per_year` other than the one in
#' the spec.
#'
#' The `Minimum events (Y or -)` and `Minimum events (#)` sheets follow the
#' Rare conditions sheet. Both hold one row per outcome and comparison and
#' one column per whole follow-up year, for the per-protocol estimand. The
#' first shows Y when the estimate passes the minimum-events rule and - when
#' it fails the rule. A failed estimate is withheld unless its comparison is
#' exempt, and an exempt comparison ends with ` (exempt)`. The second shows
#' the largest cut, of 1, 2, 3, 4, 5, 10 and 20, at which the estimate still
#' passes the rule. The intention-to-treat estimand follows the per-protocol
#' decision.
#' `$get_horizon_windows()` returns the year-by-year detail.
#'
#' The year columns run from 1 to the longest follow-up of all ETTs, in
#' whole years of 52 weeks. A follow-up that is not a whole number of years
#' gets no column. A year with an ETT of exactly that follow-up shows the
#' decision of that ETT. Another year shows the decision up to that year of
#' the shortest longer ETT. A year that no ETT of the row reaches is empty.
#'
#' The two Minimum events sheets are the only sheets that show a
#' feasibility-horizon ETT, one that the spec key
#' `feasibility_follow_up_weeks` adds. Its years get columns.
#' Every other sheet, `results.qs2` and `$get_horizon_windows()` leave it
#' out.
#'
#' The method also writes `feasibility.xlsx` to the directory of the
#' workbook. It holds the sheets of `$s3_feasibility()`, in the same order.
#' Its Minimum events sheets read the decisions that s3 stored.
#'
#' The workbook carries no forest plot. The `PP results` and `ITT results`
#' sheets already report every emulated trial with counts, rates, ratios,
#' risk differences and intervals. A forest image repeated a subset of those
#' numbers. `$export()` still draws one for a manuscript.
#'
#' The method also writes `results.qs2` to the directory of the workbook. A
#' manuscript reads that file. It is a named list of aggregate results:
#' \describe{
#'   \item{`estimates`}{`$get_estimates()`.}
#'   \item{`curves`}{`$get_curves()`.}
#'   \item{`subgroups`}{`$get_subgroups()`.}
#'   \item{`baselines`}{`$get_baselines()`.}
#'   \item{`attrition`}{`$get_attrition()`.}
#'   \item{`enrollments`}{One row per enrollment: `enrollment_id`,
#'     `enrollment_name`, `intervention_name`, `comparator_name`, and the
#'     per-protocol person-trials of each arm, `n_baseline_intervention` and
#'     `n_baseline_comparator`.}
#'   \item{`spec_version`}{The specification version of the plan.}
#'   \item{`swereg_version`}{The swereg version that wrote the file.}
#'   \item{`swereg_version_computing`}{A list with `s2` and `s3`, the swereg
#'     versions stamped on the stored results.}
#'   \item{`s1_run_id`, `s2_run_id`}{The s1 run id of the plan, and the
#'     distinct s2 run ids of its ETTs.}
#' }
#' The file holds no person-level data. No table has a person or person-trial
#' identifier, and no table has a row per person or per person-trial.
#'
#' @param path File path for the output `.xlsx` file.
#' @param table1_enrollment Enrollment ID for Table 1 (main baseline table).
#'   Default: the enrollment with the most baseline observations. An id
#'   that is not an analysed enrollment stops with an error that lists the
#'   analysed ids.
#' @param protocol_ett_id Optional character(1) ETT id. The
#'   `Target trial protocol` sheet describes this one emulated trial. An id
#'   the plan does not hold raises a warning and falls back. When `NULL`
#'   (default), the sheet describes the first ETT of the Table 1
#'   enrollment, and otherwise the first ETT in the grid.
#' @param output_dir Optional directory holding the cached `.qs2` files.
#'   Used by the lazy `recompute_baselines()` refresh. Defaults to
#'   `self$output_dir`.
#' @param power Numeric(1), at least 0.5 and below 1, default 0.8. The power
#'   of the minimum detectable effect (MDE) on the `PP results` and
#'   `ITT results` sheets. Each sheet reports the expected intervention events
#'   under the null and the protective and harmful MDE incidence rate ratio.
#'   The MDE is a naive Poisson calculation at the study confidence level.
#'   Under the null, each arm expects the pooled weighted rate of both arms
#'   times its weighted person-years. The calculation ignores the repeated
#'   contributions of one person. So it can differ from the detectable effect
#'   of the weighted estimator with person-clustered standard errors. It is not
#'   observed power (Hoenig and Heisey 2001).
TTEPlan$set(
  "public",
  "export_tables",
  function(
    path = NULL,
    table1_enrollment = NULL,
    protocol_ett_id = NULL,
    output_dir = NULL,
    power = 0.8
  ) {
    return(.plan_export_tables(
      self,
      path,
      table1_enrollment,
      protocol_ett_id,
      output_dir,
      power
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
#' `c(0.95, 1)` becomes `c(0, 0.05)` and shows the same region of the figure
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
#' The minimum-events rule shows on both figures. A `"survival"` curve stops
#' at the reportable horizon, and a grey band labelled "Not estimable" covers
#' the rest of the follow-up. A `"forest"` cell that the rule withheld reads
#' `NE`. The method stops, and names `$s3_analyze()`, when a stored result was
#' judged with a `min_expected_events_per_arm_per_year` other than the one in
#' the spec.
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
