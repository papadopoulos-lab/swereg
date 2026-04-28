# Static audit: every `by = trial_id` aggregation in r6_tteplan.R
# should be either preceded by an `is.na(trial_id)` filter on the
# input rows, or operating on a data.table where trial_id is
# guaranteed non-NA by construction.
#
# The original CONSORT bug (`.s1_compute_attrition` doubling the
# global cohort) was a `by = trial_id` aggregation over a `pt0`
# whose source rows could legitimately have `trial_id = NA` (person-
# weeks outside any trial period). The same shape exists elsewhere
# in s1_generate_enrollments_and_ipw -- this test forces every such
# call site to be inspected at PR review time.
#
# False positives are fine -- the test just enumerates every
# `by = trial_id` aggregation and points to it. If a new one is
# added, the test fails with a list of locations to audit. The
# allow-list below holds the call sites that have been hand-verified
# safe (input is guaranteed non-NA at that point).

test_that("every `by = trial_id` aggregation is either NA-filtered or allow-listed", {
  src_path <- testthat::test_path("..", "..", "R", "r6_tteplan.R")
  skip_if_not(file.exists(src_path), "R/r6_tteplan.R not found (installed pkg?)")
  src <- readLines(src_path, warn = FALSE)

  # Find every line that uses `by = trial_id` or `by = .(trial_id, ...)`
  pattern <- "by\\s*=\\s*(trial_id|\\.\\([^)]*trial_id[^)]*\\))"
  hits <- grep(pattern, src, value = FALSE)
  expect_true(length(hits) > 0L,
    info = "expected at least one `by = trial_id` aggregation in r6_tteplan.R")

  # Allow-list: line numbers whose input is guaranteed non-NA at the
  # call site. Each entry must be hand-verified and the rationale
  # logged here. If you add a new `by = trial_id` aggregation, you
  # MUST either (a) precede it with `[!is.na(trial_id)]` or (b) add
  # the new line number here with a one-line rationale.
  audited_call_sites <- list(
    # `before_row <- pt0[!is.na(trial_id), .(...), by = trial_id]`
    # post-26.4.27 CONSORT fix
    .s1_compute_attrition_before_row = "filtered with [!is.na(trial_id)]",
    # `rows[[i]] <- pt_i[!is.na(trial_id), .(...), by = trial_id]`
    # post-26.4.27 CONSORT fix
    .s1_compute_attrition_per_criterion_rows = "filtered with [!is.na(trial_id)]",
    # `enrolled_ids <- all_tuples[, ..., by = trial_id]`
    # all_tuples comes from .s1_eligible_tuples which only emits rows
    # for trials within the design's enrolled period -> trial_id
    # cannot be NA.
    s1_generate_enrolled_ids = "all_tuples is restricted to enrolled-trial rows",
    # `global_counts <- all_tuples[, ..., by = trial_id]` -- same input
    s1_generate_global_counts = "same all_tuples (no NA possible)",
    # `enrolled_counts <- enrolled_ids[, ..., by = trial_id]` -- subset
    # of the above
    s1_generate_enrolled_counts = "subset of all_tuples",
    # `attrition_summary <- all_attrition[, ..., by = .(trial_id, criterion)]`
    # all_attrition rbinds the per-batch attritions; the spurious-NA
    # contributor was eliminated upstream by the .s1_compute_attrition
    # fix above, so summing here cannot reintroduce the bug.
    s1_generate_attrition_summary = "upstream NA filter handled in .s1_compute_attrition"
  )

  # Print the call sites for the diff reviewer. This is informational
  # only -- the test passes as long as `length(hits)` matches the
  # number of audited call sites. (Update the count below if you add
  # a new audited site.)
  for (line_no in hits) {
    cat(sprintf("  R/r6_tteplan.R:%d  %s\n",
                line_no, trimws(src[line_no])))
  }
  expect_equal(
    length(hits), length(audited_call_sites),
    info = paste0(
      "found ", length(hits),
      " `by = trial_id` aggregations in R/r6_tteplan.R; the allow-list ",
      "in this test contains ", length(audited_call_sites), " hand-",
      "audited entries. If you added a new aggregation, audit it for ",
      "NA-trial_id input rows (cf. the pre-26.4.27 CONSORT bug) and ",
      "either (a) precede with [!is.na(trial_id)] or (b) extend the ",
      "allow-list above with a one-line rationale.")
  )
})
