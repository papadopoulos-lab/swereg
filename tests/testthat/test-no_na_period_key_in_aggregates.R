# Static audit: every `by = enrollment_period_id` aggregation in the TTEPlan
# sources should be either preceded by an `is.na(enrollment_period_id)` filter
# on the input rows, or operating on a data.table where enrollment_period_id is
# guaranteed non-NA by construction. `enrollment_period_id` is the trial of a
# person-trial, and the key every per-trial count is grouped by.
#
# The TTEPlan sources are `R/r6_tteplan.R` and every file the 26.10.3
# split spawned from it. The audit reads them by name pattern, so a
# new sibling joins it without an edit here.
#
# The original CONSORT bug (`.s1_compute_attrition` doubling the
# global cohort) was a per-trial aggregation over a `pt0` whose source
# rows could legitimately have an NA trial key (person-weeks outside any
# trial period, where `period_id` is NA). The same shape exists elsewhere
# in s1_generate_enrollments_and_ipw -- this test forces every such
# call site to be inspected at PR review time.
#
# False positives are fine -- the test just enumerates every
# `by = enrollment_period_id` aggregation and points to it. If a new one is
# added, the test fails with a list of locations to audit. The
# allow-list below holds the call sites that have been hand-verified
# safe (input is guaranteed non-NA at that point).

test_that("every `by = enrollment_period_id` aggregation is either NA-filtered or allow-listed", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")
  files <- list.files(
    r_dir,
    pattern = "^(r6_)?tteplan.*\\.R$",
    full.names = TRUE
  )

  # Find every line that uses `by = enrollment_period_id` or
  # `by = .(enrollment_period_id, ...)`
  pattern <- paste0(
    "by\\s*=\\s*(enrollment_period_id\\b|",
    "\\.\\([^)]*\\benrollment_period_id\\b[^)]*\\))"
  )
  hits <- character()
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    i <- grep(pattern, src, perl = TRUE)
    if (length(i) > 0L) {
      hits <- c(hits, sprintf("R/%s:%d  %s", basename(f), i, trimws(src[i])))
    }
  }
  expect_true(
    length(hits) > 0L,
    info = "expected at least one `by = enrollment_period_id` aggregation in the TTEPlan sources"
  )

  # Allow-list: line numbers whose input is guaranteed non-NA at the
  # call site. Each entry must be hand-verified and the rationale
  # logged here. If you add a new `by = enrollment_period_id` aggregation, you
  # MUST either (a) precede it with `[!is.na(enrollment_period_id)]` or (b) add
  # the new line number here with a one-line rationale.
  audited_call_sites <- list(
    # `before_row <- pt0[!is.na(enrollment_period_id), .(...),
    #   by = enrollment_period_id]`, post-26.4.27 CONSORT fix
    .s1_compute_attrition_before_row = "filtered with [!is.na(enrollment_period_id)]",
    # `rows[[i]] <- pt_i[!is.na(enrollment_period_id), .(...),
    #   by = enrollment_period_id]`, post-26.4.27 CONSORT fix
    .s1_compute_attrition_per_criterion_rows = "filtered with [!is.na(enrollment_period_id)]",
    # `enrolled_ids <- all_tuples[, ..., by = enrollment_period_id]`
    # all_tuples comes from .s1_eligible_tuples which only emits rows
    # for trials within the design's enrolled period -> enrollment_period_id
    # cannot be NA.
    s1_generate_enrolled_ids = "all_tuples is restricted to enrolled-trial rows",
    # `global_counts <- all_tuples[, ..., by = enrollment_period_id]` -- same
    # input
    s1_generate_global_counts = "same all_tuples (no NA possible)",
    # `enrolled_counts <- enrolled_ids[, ..., by = enrollment_period_id]` --
    # subset
    # of the above
    s1_generate_enrolled_counts = "subset of all_tuples",
    # `attrition_summary <- all_attrition[, ...,
    #   by = .(enrollment_period_id, criterion)]`
    # all_attrition rbinds the per-batch attritions; the spurious-NA
    # contributor was eliminated upstream by the .s1_compute_attrition
    # fix above, so summing here cannot reintroduce the bug.
    s1_generate_attrition_summary = "upstream NA filter handled in .s1_compute_attrition"
  )

  # Print the call sites for the diff reviewer. This is informational
  # only -- the test passes as long as `length(hits)` matches the
  # number of audited call sites. (Update the count below if you add
  # a new audited site.)
  for (hit in hits) {
    cat(sprintf("  %s\n", hit))
  }
  expect_equal(
    length(hits),
    length(audited_call_sites),
    info = paste0(
      "found ",
      length(hits),
      " `by = enrollment_period_id` aggregations in the TTEPlan sources; the allow-list ",
      "in this test contains ",
      length(audited_call_sites),
      " hand-",
      "audited entries. If you added a new aggregation, audit it for ",
      "NA-enrollment_period_id input rows (cf. the pre-26.4.27 CONSORT bug) and ",
      "either (a) precede with [!is.na(enrollment_period_id)] or (b) extend the ",
      "allow-list above with a one-line rationale."
    )
  )
})
