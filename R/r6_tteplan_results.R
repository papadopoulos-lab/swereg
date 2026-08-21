# These TTEPlan methods read the results that Loop 3 stored. One of them
# recomputes the baseline tables in process.

#' @include r6_tteplan.R
#' @description Print a diagnostic summary of stored results.
#'
#' Shows one row per ETT with enrollment, event count, and whether
#' IRR/rates computed successfully.
#'
#' This method reads `self$results_ett` directly, and it is the one
#' DIAGNOSTIC exception to the rule that every consumer reads an accessor.
#' A tool that reports ABSENCE cannot read through an interface that hides
#' absence. The accessors report a missing slot and a skipped slot the same
#' way, as absent rows or as `NA`. They expose no skip envelope and no
#' failure reason. This method prints exactly three states. `"NULL"` names
#' a slot the plan does not hold. `"SKIP: <reason>"` names a worker that
#' failed. `"OK"` names a stored result.
#'
#' It reports on the CACHE and never on a number. A caller that wants the
#' numbers calls `$get_estimates()`.
TTEPlan$set("public", "results_summary", function() {
  .plan_results_summary(self)
})

#' @description Every stored effect estimate, as one flat table.
#'
#' One row per emulated trial, estimand and weighting.
#'
#' `estimand` and `weights` are two columns, not one. `estimand` reads
#' `"pp"` or `"itt"`. `weights` reads `"truncated"` or `"untruncated"` and
#' names the weighting choice inside per-protocol. Three combinations
#' occur: per-protocol truncated, per-protocol untruncated, and
#' intention-to-treat.
#'
#' Three rows per emulated trial is an UPPER BOUND, not a promise. A
#' combination gets a row when the plan holds at least one of its rates,
#' incidence rate ratio and risk-difference slots. A combination the plan
#' holds nothing for gets no row. So a complete 540-trial grid returns 1,620
#' rows, and a partial one returns fewer.
#'
#' The method computes nothing. It reads `plan$results_ett`, and it joins
#' the labels from `plan$ett` and `plan$spec`. A slot the plan does not
#' carry gives `NA` in that slot's columns. The method MUST NOT fill the
#' gap from a neighbouring slot.
#'
#' `irr_estimable` is READ, not decided. `$s3_analyze()` decides it beside
#' the ratio and stores it. A result stored before that column existed gives
#' `NA`, and the method MUST NOT apply the rule to fill the gap.
#'
#' Every number is a bare number. `irr_pvalue` is a probability, not
#' `"<0.001"`. `rd` is a proportion, not a rate per 10,000. The consumer
#' formats it.
#'
#' Five sibling methods return the other stored results in the same shape:
#' `$get_curves()`, `$get_baselines()`, `$get_attrition()`,
#' `$get_matching()` and `$get_subgroups()`. Each takes no argument, and
#' each computes nothing.
#'
#' The number needed to treat carries its interval. `nnt` is the point
#' estimate, and `nnt_lo` and `nnt_hi` are the bounds `$s3_analyze()`
#' stored. Both bounds are `NA` where `interval_status` reads
#' `"spans null"`, because the reciprocal of an interval that contains zero
#' is not an interval. A consumer MUST NOT invert `rd_lo` and `rd_hi`
#' itself, and MUST NOT print `nnt` alone where the bounds are missing.
#'
#' @return A data.table with 41 columns. The identifiers come first, then
#'   the weighted counts, then the incidence rate ratio, then the risk
#'   difference and the number needed to treat. `n_boot`, `seed` and
#'   `conf_level` record what produced the risk-difference interval.
TTEPlan$set("public", "get_estimates", function() {
  .acc_estimates(self)
})

#' @description Every stored survival curve, as one flat table.
#'
#' One row per emulated trial, estimand, weighting, arm and band.
#' `$s3_analyze()` stores one wide curve per estimand, with a survival
#' column for each arm. This method returns one row per arm instead.
#'
#' The table carries the numbers at risk beside survival.
#' `n_persons_at_risk` is an unweighted count of distinct people, per arm
#' per band. `$s3_analyze()` stores it and this method melts it. A risk
#' table reports people, so it cannot be derived from `surv`, which is a
#' weighted probability.
#'
#' A curve stored before that column existed gives `NA`. A consumer that
#' draws a risk table MUST check for missing values first. It MUST refuse to
#' draw. A row of missing counts looks like a drawn risk table.
#'
#' @return A data.table with columns `ett_id`, `estimand`, `weights`,
#'   `arm`, `band`, `surv` and `n_persons_at_risk`.
TTEPlan$set("public", "get_curves", function() {
  .acc_curves(self)
})

#' @description Every stored baseline panel, as one flat table.
#'
#' One row per enrollment, panel and table row. Three columns identify the
#' panel. `imputation` reads `"raw"` or `"imputed"`. `weighting` reads
#' `"none"`, `"ipw"` or `"ipw_trunc"`. `variant` reads `"main"` or
#' `"supplementary"`. Five combinations occur.
#'
#' The `"raw"` panel needs a separate pre-imputation file. The table holds
#' no `"raw"` rows when the plan holds no such panel. The method MUST NOT
#' present another panel under that name.
#'
#' `overall`, `comparator` and `intervention` are display strings, such as
#' `"12.3 (4.5)"` or `"120 (8.1%)"`. The producer stores them that way.
#' `smd_numeric` is the unrounded standardised mean difference.
#'
#' `variable` repeats on every row of its block. The stored panel prints
#' the name once and indents its levels under it, so `variable` is blank
#' there. A renderer that wants that indent MUST blank the repeat itself.
#'
#' @return A data.table. `n_baseline`, `n_baseline_intervention` and
#'   `n_baseline_comparator` repeat that enrollment's counts on every row.
TTEPlan$set("public", "get_baselines", function() {
  .acc_baselines(self)
})

#' @description The stored eligibility cascade, as one flat table.
#'
#' One row per enrollment and stored row, in pipeline order. Counts are
#' remaining-after-step.
#'
#' `$s1_generate_enrollments_and_ipw()` stores one row per trial and
#' criterion, plus ONE GLOBAL ROW per criterion. The global row carries the
#' true overall count of distinct people. This method returns EVERY STORED
#' ROW. `trial_id` is `NA` on a global row and the trial index on a
#' per-trial row, so the caller filters on that column.
#'
#' The method returns the stored rows and nothing else. It does not sum the
#' per-trial rows. It does not create a global row for a criterion that has
#' none. A criterion with per-trial rows and no global row therefore yields
#' per-trial rows and no global row.
#'
#' Collapsing to one row per criterion is a RENDERER's decision, and
#' `.attrition_overall()` makes it. That renderer reads the global rows and
#' nothing else. It returns NULL when one criterion carries no global row,
#' and the enrollment then gets no attrition sheet and no CONSORT diagram.
#' This method makes no such decision. It returns every stored row, and the
#' renderer needs the per-trial rows to see a criterion that has only
#' those.
#'
#' `step_order` is the position of the criterion in stored order, so every
#' row of one criterion carries the same value.
#'
#' The table holds the ELIGIBILITY CASCADE only. It holds no comparator-draw step
#' and no analysis step, because `$s1_generate_enrollments_and_ipw()` stores
#' neither as a step. `.build_cohort_flow()` builds those two rows and
#' derives the per-step change columns. Building a row is a renderer's job,
#' so this method calls that builder nowhere.
#'
#' The table carries no step KIND, because nothing stores one. The first
#' stored criterion is the cohort start and every later one is an exclusion.
#' A consumer labels them from `step_order`, and this method decides
#' nothing.
#'
#' @return A data.table with columns `enrollment_id`, `trial_id`,
#'   `step_order`, `step_name`, `n_persons`, `n_person_trials`,
#'   `n_arm_intervention` and `n_arm_comparator`.
TTEPlan$set("public", "get_attrition", function() {
  .acc_attrition(self)
})

#' @description The stored comparator-draw counts, as one flat table.
#'
#' One row per enrollment and trial.
#' `$s1_generate_enrollments_and_ipw()` stores it that way.
#' `n_intervention_total` and `n_comparator_total` count every person-trial
#' that was eligible for an arm. `n_intervention_enrolled` and
#' `n_comparator_enrolled` count the person-trials the draw took.
#'
#' This is a SIXTH method rather than four more columns on
#' `$get_attrition()`. The comparator-draw table has one row per enrollment and
#' trial. The attrition table has one row per enrollment, trial and
#' criterion. Joining them would repeat one comparator-draw count on every
#' criterion row, and report a grain that neither producer stored.
#'
#' The method computes nothing. It does not sum across trials, and it
#' derives no enrolment ratio. `.build_cohort_flow()` sums the enrolled
#' counts to build its comparator-draw step, and that sum is a renderer's.
#'
#' An enrollment that stored no comparator-draw table gets NO ROW.
#'
#' @return A data.table with columns `enrollment_id`, `trial_id`,
#'   `n_intervention_total`, `n_comparator_total`,
#'   `n_intervention_enrolled` and `n_comparator_enrolled`.
TTEPlan$set("public", "get_matching", function() {
  .acc_matching(self)
})

#' @description Every stored stratified estimate, as one flat table.
#'
#' One row per emulated trial, estimand, weighting, subgroup variable and
#' subgroup level. `subgroup_level` reads `"all"` on the whole-cohort row,
#' and the level label on every other row.
#'
#' `subgroup_var` is part of the KEY, not a label. One emulated trial MAY
#' carry several subgroup variables, and each one has its own `"all"` row.
#'
#' TWO p-values, and they answer different questions.
#' \itemize{
#'   \item `irr_pvalue` is the stratum's own p-value. Is this stratum's rate
#'     ratio distinguishable from the null?
#'   \item `em_pvalue` is the interaction test. Do the strata differ from
#'     each other?
#' }
#' A consumer that renders one where the other belongs reports a different
#' finding. The two never share a name.
#'
#' `em_pvalue`, `ratio_of_irrs`, `ratio_lo` and `ratio_hi` come from the
#' interaction test that `$s3_analyze()` stores. Each is one number for the
#' whole stratified result, so each repeats on every row of that result. A
#' renderer that wants them once shows them on the `"all"` row.
#'
#' `ratio_of_irrs` is the ratio of the two stratum rate ratios. It is `NA`
#' unless the subgroup variable has exactly two levels.
#'
#' The method reads the UNION of two stored families. `$s3_analyze()`
#' dispatches the stratified rate ratios and the interaction test as
#' separate work items, in separate subprocesses, so either can fail alone.
#' Four states occur.
#' \itemize{
#'   \item Both stored. Full rows.
#'   \item Stratified only. One row per stored level, with all four
#'     interaction columns `NA`.
#'   \item Interaction only. ONE row, with `subgroup_level` reading `"all"`
#'     and the four stratum columns `NA`. No stored table names the levels,
#'     so the method MUST NOT invent a stratum row.
#'   \item Neither stored. No rows, even when the specification names the
#'     variable.
#' }
#' A skipped stratified result reads as absent.
#'
#' Coverage. Study 002 runs no stratified analysis, so this method is
#' tested against a fixture. Other studies in the fleet do configure
#' subgroups, so treat the schema as production.
#'
#' @return A data.table with 13 columns: `ett_id`, `estimand`, `weights`,
#'   `subgroup_var`, `subgroup_level`, `irr`, `irr_lo`, `irr_hi`,
#'   `irr_pvalue`, `em_pvalue`, `ratio_of_irrs`, `ratio_lo` and `ratio_hi`.
TTEPlan$set("public", "get_subgroups", function() {
  .acc_subgroups(self)
})

#' @description Recompute baseline characteristic tables in-process.
#'
#' Reads each enrollment's smallest analysis file (and the raw file when
#' present) from disk and re-runs the new `swereg_table1` engine. Used to
#' refresh stale results after upgrading swereg, without re-running the
#' full `$s3_analyze()` pipeline.
#'
#' This is a PRODUCER, and the read is s3's. It calls
#' `.s3_enrollment_worker()`, the same worker `$s3_analyze()` calls, and it
#' stores what the worker returns. No renderer in the export path opens an
#' analysis file.
#'
#' `$export_tables()` calls this method on its own when a stored panel is
#' stale. Call it yourself when you want the refresh to be a visible step.
#' The lazy path costs minutes. Whether it runs at all depends on what a
#' cached plan happens to hold.
#'
#' @param output_dir Optional directory holding the `.qs2` files. Defaults
#'   to `self$output_dir`.
#' @param enrollment_ids Optional character vector. If NULL, refreshes
#'   every enrollment in `self$results_enrollment`.
#' @return `invisible(self)`.
TTEPlan$set(
  "public",
  "recompute_baselines",
  function(output_dir = NULL, enrollment_ids = NULL) {
    if (is.null(output_dir)) {
      output_dir <- self$output_dir
    }
    if (is.null(output_dir)) {
      stop("output_dir is not set. Pass it as an argument.")
    }
    if (
      is.null(self$results_enrollment) ||
        length(self$results_enrollment) == 0L
    ) {
      stop("No enrollment results to refresh.")
    }
    if (is.null(enrollment_ids)) {
      enrollment_ids <- names(self$results_enrollment)
    }
    ett <- self$ett
    for (eid in enrollment_ids) {
      enr_rows <- ett[ett$enrollment_id == eid]
      if (nrow(enr_rows) == 0L) {
        next
      }
      analysis_files <- file.path(output_dir, enr_rows$file_analysis)
      present <- file.exists(analysis_files)
      if (!any(present)) {
        warning("No analysis files found on disk for enrollment ", eid)
        next
      }
      analysis_files <- analysis_files[present]
      sizes <- file.size(analysis_files)
      smallest <- which.min(sizes)
      analysis_path <- analysis_files[smallest]
      raw_path <- file.path(output_dir, enr_rows$file_raw[1])
      new_result <- .s3_enrollment_worker(
        analysis_path = analysis_path,
        raw_path = raw_path,
        enrollment_id = eid,
        n_threads = data.table::getDTthreads(),
        arm_labels = .lookup_arm_labels(self$spec, eid)
      )
      # Preserve fields like n_baseline that came from the original run if
      # the worker returned NA (it shouldn't, but be defensive).
      prev <- self$results_enrollment[[eid]]
      if (!is.null(prev)) {
        for (k in setdiff(names(prev), names(new_result))) {
          new_result[[k]] <- prev[[k]]
        }
      }
      self$results_enrollment[[eid]] <- new_result
    }
    invisible(self)
  }
)
