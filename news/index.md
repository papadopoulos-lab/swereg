# Changelog

## swereg 26.5.15

### Changed

- `status.txt`: each section (never-matched, rare) now leads with a
  bucket-count summary (one row per registry prefix, descending by size)
  before the per-bucket detail blocks. Per-bucket lists are never
  collapsed – every column name appears in full. Rare cutoff rendered as
  “1-9” (clearer than “\< 10”).

## swereg 26.5.14

### Changed

- `status.txt` rendered by `$compute_summary()` is restructured:
  - The `[ok]` count appears first (above the noise) so the headline
    number (“how many variables look healthy”) is immediately readable.
  - Never-matched and rare-variable sections are grouped by the column’s
    registry-type prefix (everything up to the first underscore:
    `dorsm`, `sv`, `os`, `osd`, `can`, `op`, `rx`, …), sorted by bucket
    size descending so the dominant problem shows first. Avoids the
    252-line flat alphabetical list that previously forced the reader to
    scroll past the entire never-matched dump to reach the actually-OK
    count.
  - All numbers comma-formatted (`8,852,776` not `8852776`).
  - Trailing pointer to `summary.qs2` for full per-column detail.

## swereg 26.5.13

### Changed

- Meta sidecar now carries weekly/annual splits and the date range of
  weekly data: `n_rows_weekly` / `n_rows_annual`, `n_persons_weekly` /
  `n_persons_annual`, `weekly_min_isoyearweek` /
  `weekly_max_isoyearweek`, `annual_min_isoyear` / `annual_max_isoyear`.
  Per-column `$counts` now emit `n_person_weeks_with` (TRUE rows where
  `is_isoyear == FALSE`) and `n_person_years_with` (TRUE rows where
  `is_isoyear == TRUE`) separately; previously the single field was
  misleadingly named since it was actually
  [`nrow()`](https://rdrr.io/r/base/nrow.html) (weekly + annual
  combined).
- `$compute_summary()` aggregates these splits; `status.txt` now prints
  the WEEKLY and ANNUAL time periods with their respective denominators.
- TSV audit-track gains an `n_person_years_with` column and header
  comments for both periods.

## swereg 26.5.12

### Breaking

- Removed the per-batch code-check warning machinery added in PR
  [\#4](https://github.com/papadopoulos-lab/swereg/issues/4) + 26.5.10.
  Specifically removed: `start_code_check_session()` /
  `end_code_check_session()` exports, `warn_unmatched_codes()` /
  `warn_empty_logical_cols()` exports, the internal
  `.swereg_codes_pre()` / `.swereg_codes_post()` hooks,
  `.code_check_snapshot()` / `.code_check_merge()` /
  `.code_check_emit()`, and the `code_check_state` field on the meta
  sidecar. Rationale: this information is now exposed more usefully via
  `RegistryStudy$compute_summary()` (see below), so having two parallel
  mechanisms reporting the same data was redundant.
- `RegistryStudy$save_skeleton()` no longer takes a `code_check_state`
  argument (it had no other callers). The meta sidecar shape changed:
  `code_check_state` is dropped, `n_persons` is added, and every entry
  in `applied_registry` now carries a `$counts` sub-field. Older meta
  sidecars (built before this version) keep working but their `$counts`
  field is missing, so `$compute_summary()` will report zero per-column
  counts for those batches. To fix, **delete the affected
  `skeleton_*.qs2` + `meta_*.qs2` files** (or call
  `study$delete_skeletons()`) and re-run `$process_skeletons()`.

### New

- `RegistryStudy$compute_summary()`: aggregates per-batch `meta_*.qs2`
  sidecars into a study-wide sanity report. Always writes `summary.qs2`
  (binary; programmatic reload) and `status.txt` (human-readable flag
  report) to `data_skeleton_dir`. On full runs (every expected batch
  present), also writes a git-tracked
  `summary_<UTC>_<git-sha>_<swereg-ver>.tsv` to the new
  `data_summaries_dir` candidate, with counts below `suppress_below`
  (default 5) masked as `"<N"` (Swedish registry data convention).
  Partial runs explicitly skip the TSV.
- New `data_summaries_dir` constructor parameter (optional). Defaults to
  NULL; when NULL, `$compute_summary()` skips the TSV even on full runs.
- `Skeleton$apply_code_entry()` now computes per-column `n_persons_with`
  and `n_person_weeks_with` for every column it adds, stored on the
  entry’s `applied_registry` record. These are the primitive that
  `$compute_summary()` rolls up.
- `kept` – `expand_codes()` / `expand_code_list()` (bracket / range
  expansion utilities from PR
  [\#4](https://github.com/papadopoulos-lab/swereg/issues/4)) remain.
  The expansion of code patterns happens inline in each `add_*()`
  function; the removed pieces were specifically the per-call *warning
  emission* machinery, not the *code expansion* machinery.

## swereg 26.5.11

### Fix

- `.swereg_dev_path()` (helper that hands the package root to callr
  workers so they can
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
  the same dev source as the parent) was returning
  `system.file(package="swereg")` directly, which for a dev-loaded
  package is the `inst/` subdirectory – not the package root that
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
  expects. Workers then loaded a broken namespace and
  `$process_skeletons()` failed on batch 1 with
  `attempt to apply non-function`. Now strips the trailing `/inst` so
  the worker receives the actual package root.

## swereg 26.5.10

### Breaking (on-disk)

- Skeleton schema bumped from version 4 to version 5 to introduce a
  `meta_%05d.qs2` sidecar next to every `skeleton_%05d.qs2`. Existing
  skeleton directories from earlier versions are not readable; run
  `study$delete_skeletons()` and re-run `$process_skeletons()` to
  regenerate. The sidecar is small (a few KB) and lets
  `$process_skeletons()` do an incremental-rebuild check by reading meta
  only – avoiding the full skeleton deserialise on the common no-change
  path. For a 2000-batch delivery with 500 MB skeletons this turns a ~1
  TB read into a few MB.

### New features

- `RegistryStudy$process_skeletons()` now emits a single consolidated
  code-check warning at the end of every run, covering every batch in
  scope. Sequential and parallel runs behave identically because the
  per-batch accumulators flow through the meta sidecars on disk rather
  than via in-memory state.

- `RegistryStudy$skeleton_pipeline_hashes()` now reads the small meta
  sidecars instead of deserialising every skeleton, with a transparent
  fallback to loading the skeleton when meta is missing. Significantly
  faster on large studies.

### Internal

- The code-check session machinery introduced in 26.5.9 is now fully
  internal: the previously-exported `start_code_check_session()`,
  `end_code_check_session()`, `expand_codes()`, `expand_code_list()`,
  `warn_unmatched_codes()`, `warn_empty_logical_cols()` are no longer
  exported. Users running through `$process_skeletons()` get cross-batch
  aggregation automatically; users running manual loops outside
  `RegistryStudy` get per-call warnings (the pre-26.5.9 behaviour). If
  you need cross-batch aggregation in a manual loop, open an issue and
  we’ll re-export.

## swereg 26.5.9

### New features

- [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
  [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
  [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
  [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
  [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
  [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md)
  and
  [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)
  now accept bracket / character-class / range patterns directly
  (e.g. `"I2[0-5]"`, `"FN[ABCDEGW][0-9][0-9]"`, `"!302[A-Z]"`). Bracket
  expansion runs unconditionally; the matchers themselves continue to
  use [`startsWith()`](https://rdrr.io/r/base/startsWith.html) on
  literal prefixes.

- The same `add_*` family now runs pre-call (per-literal source-data)
  and post-call (column-level) sanity checks automatically. Bad patterns
  surface at run-time instead of producing silent empty columns. Both
  checks can be disabled via `options(swereg.check_codes = FALSE)`.

- The pre-call check also runs a cheap, data-free *syntax* check on the
  expanded code list, firing in milliseconds at the first `add_*()` call
  rather than at hour 6 of a multi-hour batched pipeline. It warns when
  any expanded literal is empty or contains regex metacharacters
  (`^ $ * + ? . ( ) | \ [ ]`) that will not match under
  [`startsWith()`](https://rdrr.io/r/base/startsWith.html). Skipped
  automatically for `add_rx(source = "produkt")` because product names
  are exact-matched via `%chin%` and may legitimately contain those
  characters.

Contributed by [@alexengberg](https://github.com/alexengberg) (PR
[\#4](https://github.com/papadopoulos-lab/swereg/issues/4)).

## swereg 26.5.8

### Breaking

- File-naming format width bumped from 3 digits to 5 digits for batch /
  ETT identifiers. Rawbatch files become `00001_rawbatch_lmed.qs2` (was
  `001_rawbatch_lmed.qs2`); skeleton files become `skeleton_00001.qs2`
  (was `skeleton_001.qs2`); ETT identifiers become `ETT00001` (was
  `ETT001`). Existing on-disk files using the old 3-digit width will not
  be recognised by the new code – callers must rename them via shell or
  regenerate. Affects `RegistryStudy` (rawbatch + skeleton) and
  `TTEPlan` (ett_id, and the
  `file_analysis = "<prefix>_analysis_<ett_id>.qs2"` derived name).

## swereg 26.5.7

### Breaking-ish

- [`registrystudy_load()`](https://papadopoulos-lab.github.io/swereg/reference/registrystudy_load.md):
  parameter renamed `candidate_dir_rawbatch` -\> `candidate_dir_meta`.
  Callers must update if they passed by name. Behaviour is unchanged for
  callers that passed the same path used at
  `RegistryStudy$new(data_meta_dir = ...)` (which defaults to the
  rawbatch dir, so existing scripts continue to work positionally).

### New

- `RegistryStudy$new()` gains `data_meta_dir`: candidate paths for the
  directory holding `registrystudy.qs2`. Defaults to `data_rawbatch_dir`
  (full backward compatibility). Pass an explicit value – e.g. the
  parent of rawbatch – to keep the singleton control file out of the
  per-batch data directory. Exposed as a read-only active binding.
- `$meta_file` now resolves to
  `file.path(self$data_meta_dir, "registrystudy.qs2")`.

## swereg 26.5.6

### Performance

- [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
  is now ~8x faster and uses ~7.5x less memory at every cohort size
  tested (1k–25k IDs over an 11-year range; linear scaling). The win
  comes from sorting a single time spine once and replicating it per id,
  instead of
  [`expand.grid()`](https://rdrr.io/r/base/expand.grid.html)-ing the
  full cartesian product and then sorting the result. Output is
  identical to the previous implementation modulo a hidden `data.table`
  secondary index attribute. Contributed by
  [@gkaramanis](https://github.com/gkaramanis) (PR
  [\#3](https://github.com/papadopoulos-lab/swereg/issues/3)).

### Internal

- New `dev/bench/` scaffold for tracking performance regressions:
  `Rscript dev/bench/run_all.R` runs all benchmarks against the current
  source, `Rscript dev/bench/diff.R` compares the run to a checked-in
  `baseline.csv` and exits non-zero on \>20% time / \>50% memory
  regressions. Excluded from the package build via `.Rbuildignore`.
- New `tests/testthat/test-create_skeleton-parity.R` (31 tests) pinning
  [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
  output against an embedded reference oracle plus structural invariants
  and edge cases (empty / duplicate / NA ids, single-day range, ISO W53
  year, year boundary).

## swereg 26.4.28

### Breaking changes

- `RegistryStudy$process_skeletons()` now **fails fast** on any batch
  error instead of swallowing it into a
  [`warning()`](https://rdrr.io/r/base/warning.html) and pushing through
  the remaining batches. These pipelines run unattended for days; if
  batch 1 fails 10 minutes in (e.g. a systematic bug, missing column,
  unreadable rawbatch file), the user wants to SSH in within minutes and
  see the failure – not at the end of a 4-day run with the remaining
  batches all failing for the same root cause. The failing batch’s
  underlying error is surfaced via a
  [`stop()`](https://rdrr.io/r/base/stop.html) that includes (a) which
  batch number failed, (b) the original error message, and (c) a hint
  that successful batches are persisted on disk and the user can rerun
  with `batches = ...` to retry from the failed one. In the
  `n_workers > 1` (callr subprocess) path, in-flight workers are killed
  via `kill_tree()` before the
  [`stop()`](https://rdrr.io/r/base/stop.html) so they don’t keep
  burning compute on what is likely the same systematic failure. Callers
  who intentionally want the old “complete with warnings” behaviour can
  wrap the call in `tryCatch(error = function(e) ...)`.

### New features

- [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
  now supports `"!"`-prefixed exclusion patterns, restoring parity with
  the
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  family. Previously, `add_rx`’s matcher was a one-shot `Reduce(|, ...)`
  union with no per-pattern branching, so `"!"`-prefixed entries were
  silently treated as literal first characters of an ATC code (which
  never match) or literal product names (which would, accidentally,
  *include* a brand named with a leading `!`). Now:

  ``` R
  codes = list(rx_n05_atypical = c("N05A", "!N05AA", "!N05AB"))
  ```

  matches any antipsychotic except first-generation (N05AA / N05AB)
  classes – closing the spec-expressiveness gap that previously forced
  exhaustive enumeration of every desired sub-code. The veto is
  independent per named code (no leak across list entries) and applies
  whether `source = "atc"` (prefix match) or `source = "produkt"` (exact
  match).

### Documentation

- Expanded
  [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
  `@param codes` to spell out four nuances that the `add_diagnoses`
  family carried implicitly: vetoes are independent per named code (no
  leak across list entries); veto match style follows `source` (prefix
  for atc, exact for produkt – so `"!Sertralin"` does NOT mask
  `"Sertralin Sandoz"`); all-negative pattern sets produce empty
  columns; and the per- source-row veto interacts with the per-week
  aggregation such that a non-vetoed Rx still drives a week to TRUE even
  if a vetoed Rx overlaps in the same week.

- Fixed misleading pattern-syntax documentation on
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
  [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
  [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
  [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
  [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md),
  and
  [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md).
  The previous text described patterns as regex with auto-prepended `^`
  anchors and example strings like `"^F640"` / `"^8140"` /
  `"^80146002"`. The actual matcher is prefix-only via
  [`startsWith()`](https://rdrr.io/r/base/startsWith.html) – a literal
  `^` in a pattern is treated as an ordinary character and silently
  matches nothing. Updated each function’s `@param codes` to describe
  the real contract (prefix matching, no regex), and added a clear
  explanation of `"!"`-prefixed row-level vetoes (including the
  important detail that the veto operates on the raw source row, not on
  the `(id, isoyearweek)` bucket – a non-vetoed code in the same week
  still triggers TRUE).

- [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
  documentation now explicitly notes that it does NOT support `"!"`
  exclusion patterns (its matcher is a simple union).

## swereg 26.4.27

### Maintenance

- Renamed user-facing “exposure” terminology in the results workbook to
  match the project’s TTE vocabulary, where the assignment is the
  *treatment* and the active arm is the *intervention*. The Sensitivity
  sheet’s identifier column header `Exposure` is now `Intervention`, and
  the per-arm measurement columns `Events (exp)`, `PY (exp)`,
  `Rate/100k (exp)` are now `Events (int)`, `PY (int)`,
  `Rate/100k (int)` (paired with the unchanged `(cmp)` columns).
  Existing workbooks already on disk are not rewritten – re-run
  `plan$export_tables()` to regenerate.

### New features

- `TTEPlan$s3_analyze()` gains a `force` argument (default `FALSE`).
  When `TRUE`, cached `results_enrollment` and `results_ett` entries in
  the targeted scope are dropped before recomputation. Scope follows
  `enrollment_ids` / `ett_ids`: with both `NULL`, all cached results are
  cleared; otherwise only matching entries are dropped. Provides a
  supported way to recompute after a broken environment produced
  `skipped = TRUE` placeholders (e.g. missing `survey` package), without
  poking R6 internals from the calling script.

- `TTEPlan$s3_analyze()` gains an `n_workers` argument (default `1L`).
  Both the enrollment loop and the per-ETT loop now dispatch through
  [`parallel_pool()`](https://papadopoulos-lab.github.io/swereg/reference/parallel_pool.md)
  with the requested concurrency (previously hardcoded to `1L` at both
  call sites). Each subprocess loads its own analysis file, so peak RAM
  scales linearly with `n_workers`; CPU threads per worker are
  auto-partitioned as `floor(detectCores() / n_workers)`.

### Bug Fixes

- CONSORT cascade no longer double-counts the global cohort. In
  `.s1_compute_attrition()`, the per-trial summary tables (`before_row`
  and the per-criterion `rows[[i]]`) were grouped by `trial_id` over a
  `pt0` that included person-weeks outside any trial period. Those rows
  collapsed into a spurious `(trial_id = NA, criterion)` group whose
  `n_persons` later got summed together with the legitimate
  `before_global` / `global_rows[[i]]` row during per-batch aggregation
  (`by = .(trial_id, criterion)` at the centralized matching step),
  roughly doubling the global cohort number in the CONSORT diagram and
  attrition tables. Per-trial counts and arm counts were unaffected. Fix
  filters `pt0[!is.na(trial_id)]` (and `pt_i[!is.na(trial_id)]`) before
  computing per-trial summaries; the global row is still computed off
  the unfiltered `pt0` so it captures the full pre-exclusion uniqueN.
  Existing `enrollment_counts_*.qs2` files need to be regenerated by
  re-running s1 to pick up the corrected counts.

- `inst/worker_s2.R` now passes `sep_by_tx` (matching `.s2_worker()` and
  the plan-side item field) instead of the stale `sep_by_exp`.
  Previously `plan$s2_generate_analysis_files_and_ipcw_pp()` failed at
  the very first ETT with
  `unused argument (sep_by_exp = params$sep_by_exp)`.

### Maintenance

- Promoted runtime-required packages from `Suggests` to `Imports` so
  they install automatically: `survey`, `survival`, `mgcv`, `MASS`,
  `scales`, `glue`, `openxlsx`, `patchwork`, `DiagrammeR`,
  `DiagrammeRsvg`, `rsvg`. All are referenced unconditionally from R/
  (no [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) guards
  or alternative code paths). Previously, missing `survey` caused every
  IRR call in `s3_analyze` to be silently captured as
  `list(skipped = TRUE, reason = "there is no package called 'survey'")`,
  which then produced an empty forest plot in `export_tables` with no
  visible error. Missing `DiagrammeR`/`DiagrammeRsvg`/`rsvg` similarly
  skipped CONSORT sidecars with only a warning.

## swereg 26.4.25

### Maintenance

- Version bump.

## swereg 26.4.22

### New features

- **CONSORT reporting surfaces unique-person counts alongside
  person-trial counts.** Sequential TTE inflates the analytic
  denominator because one person enters many weekly trials; a cohort
  with 390k women can generate 22M person-weeks, and the 60x gap
  routinely confuses reviewers. Attrition bookkeeping now carries both
  numbers end-to-end:
  - `.s1_compute_attrition()` emits a global `trial_id = NA` row per
    criterion alongside the existing per-trial rows, with a true
    `uniqueN(person_id)` across the whole skeleton. Summing per-trial
    `n_persons` over-counted anyone entering more than one trial; the
    global row is the honest number. Per-trial rows are retained for
    diagnostic slicing.
  - `.build_consort_dot()` now renders one lumped red side-box with a
    bulleted list of every exclusion criterion (CONSORT-2010 convention)
    instead of a stacked red box per criterion, and each box reports
    `N persons / M person-trials`. Enrollment titles are split at ” (”
    onto two lines so long spec labels don’t blow out the box width.
  - `TTEEnrollment$rates()` gains an `n_persons` column per treatment
    arm, using `design$person_id_var`.
  - Results workbook: each enrollment’s combined-baseline sheet opens
    with a one-line “Cohort: N persons contributed M sequential trial
    enrollments” summary. A companion `Attrition_{id}` sheet carries the
    tabular form of the CONSORT numbers (criterion / n_persons /
    n_person_trials / excluded counts / n_intervention / n_comparator)
    so reviewers can cite exact figures without measuring pixels on the
    PNG sidecar.
- **`$register_codes()` auto-validates the `add_*` contract.** Every
  call to a registered `fn` is now wrapped with a pre/post-state check:
  row count must be preserved, structural columns (`id`, `isoyear`,
  `isoyearweek`, `is_isoyear`) must still be present, and every column
  name in the registration’s `codes` list must actually exist on the
  skeleton after the call. Contract violations error loudly with a
  pointer back to the offending `$register_codes(<label>)` entry. Custom
  `add_*` functions plugged into the pipeline (Norwegian registries,
  regional Swedish cohorts, payer claims, …) get this enforcement for
  free; built-ins already pass the checks, so no behaviour change for
  existing registrations.

### Documentation

- **New vignette `builtin-add-functions`** — end-to-end walkthrough of
  every `add_*` function swereg ships (`add_onetime`, `add_annual`,
  `add_diagnoses`, `add_operations`, `add_rx`, `add_cods`,
  `add_quality_registry`), with pattern syntax, collision policies, and
  a typical end-to-end ordering.
- **New vignette `custom-add-functions`** — how to write your own
  `add_*` function for registries swereg doesn’t ship support for
  (non-Swedish registries, in-house data, quality registries without
  dedicated built-ins). Covers the contract, reusing built-ins via
  `$register_codes(fn = swereg::add_diagnoses)`, `fn_args` for extra
  knobs like `diag_type = "main"`, a complete `add_vaccinations()`
  worked example run through a real `RegistryStudy`, a demonstration of
  the auto-wrap firing on a deliberately broken function, and a design
  cheat sheet of lessons from the built-ins.

### User experience

- **`RegistryStudy$skeleton_pipeline_hashes()` now reports progress.**
  After the last batch finishes, `$process_skeletons()` calls
  `$write_pipeline_snapshot()`, which in turn calls
  `$skeleton_pipeline_hashes()` to collect per-batch metadata. That
  function has to deserialize every `skeleton_*.qs2` file from disk (via
  [`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html)) to read
  a handful of hash fields, which with many/large batches can take tens
  of minutes. Previously this ran silently, making
  `$process_skeletons()` look hung after dispatching the last batch. It
  now prints an explanatory message up front and ticks a `progressr` bar
  per file.

## swereg 26.4.16

### Documentation

- Consolidated hand-rolled vignettes from 3-step (skeleton1-create,
  skeleton2-clean, skeleton3-analyze) into a 2-step manual workflow
  (skeleton-create, skeleton-analyze) that matches the actual
  architecture.
- Renamed pkgdown “Hand-rolled” section to “Manual workflow”.
- Updated skeleton-concept, skeleton-pipeline, and CLAUDE.md to reflect
  the 2-step model.

## swereg 26.4.20

### Bug fixes

- **[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md)
  now accepts `codes =` like all sibling `add_*` functions** (previously
  `cods =`). The old `cods =` name is kept as a deprecated alias with a
  warning. This mismatch caused every `add_cods` entry registered via
  `RegistryStudy$register_codes()` to fail silently with
  `"unused argument (codes = list(...))"` — the dispatcher always passes
  `codes = ...` but `add_cods` was the only one not expecting that name.

- **`RegistryStudy$process_skeletons()` progress bar now advances when
  batches fail.** Previously, the parallel branch’s error handler
  emitted a warning but never ticked the progressor, so a single failing
  batch left the bar frozen and — combined with `options("warn")`
  default of `0` buffering warnings until the function returns —
  produced the symptom “progress bar stuck at 0% indefinitely”. The bar
  now ticks on both success and failure, and failed-batch messages show
  up in the bar’s `(last: ...)` slot with a `FAILED` tag. Warnings are
  also emitted with `immediate. = TRUE` so failures surface in real time
  regardless of the session’s `warn` setting. The serial branch gained
  matching error handling (previously a single batch error aborted the
  whole run).

### BREAKING CHANGES

- **TTE vocabulary rename** (`exposure` / `exposed` / `unexposed` →
  `treatment` / `intervention` / `comparator`): the TTE system now uses
  PICO-aligned terminology. `treatment` is the umbrella concept (the
  variable naming the assignment); `intervention` and `comparator` are
  the two arm values. This matches how TTE papers are written up in the
  major medical journals (BMJ, NEJM) and Hernán’s canonical target-trial
  references. Migration:

  YAML spec:

  - `exposure:` → `treatment:`
  - `exposed_value:` → `intervention_value:`
  - `arms.exposed:` → `arms.intervention:`

  `TTEDesign$new()` arguments:

  - `exposure_var = ...` → `treatment_var = ...`
  - `time_exposure_var = ...` → `time_treatment_var = ...`

  Skeleton / trial-panel columns produced by the pipeline:

  - `baseline_exposed` → `baseline_intervention`
  - `rd_exposed` → `rd_intervention`
  - `n_exposed` / `n_unexposed` (in CONSORT / matching summaries) →
    `n_intervention` / `n_comparator` (and the `_total` / `_enrolled`
    variants)

  Logical semantics unchanged: `TRUE` = intervention arm, `FALSE` =
  comparator arm. Column names now describe what `TRUE` means, which is
  clearer than the prior “exposed” (ambiguous between an assignment and
  a time-varying status). User-chosen column names (e.g., a column named
  `"exposed"` in user data) are unaffected — the rename only touches
  package-provided API surface.

### New features

- **`RegistryStudy$register_derived_codes(codes, from, as)`**: registers
  a “derived” code entry that doesn’t read rawbatch data, but instead
  ORs together already-existing skeleton columns from earlier primary
  entries. For each `nm` in `codes`, writes
  `<as>_<nm> := <from[1]>_<nm> | <from[2]>_<nm> | ...`. Use case:
  building combined outcome columns like
  `osd_* = os_* | dorsu_* | dorsm_*` where the hospital half comes from
  `add_diagnoses` on OV+SV and the death half comes from `add_cods` on
  DORS – two functions that can’t share a single `combine_as` argument
  because they search different raw-data columns (`hdia`/`dia*` vs
  `ulorsak`/`morsak*`).

  Derived entries are full first-class citizens of the code registry:
  they get their own fingerprint, participate in the per-entry
  incremental sync via `Skeleton$sync_with_registry()`, and fold
  upstream primary fingerprints into their own so that editing an
  upstream primary’s `fn_args` / `groups` / `codes` (e.g. flipping
  `cod_type` from `"underlying"` to `"multiple"`) automatically cascades
  into a derived replay. Derived entries must be registered AFTER their
  upstream primaries (apply runs in registration order).

## swereg 26.4.19

### BREAKING CHANGES

- **New `Skeleton` R6 class**: per-batch skeleton files on disk are now
  serialized `Skeleton` R6 objects instead of bare `data.table`s. They
  carry their own phase provenance (framework hash, applied code
  registry fingerprints keyed by their minimal descriptor, and an
  ordered named list of applied phase-3 “randvars” steps). Legacy
  bare-`data.table` skeleton files are auto-wrapped on first load for
  backwards compatibility. Downstream code that reads skeletons via
  [`qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)
  now needs to unwrap via `sk$data`; the three swereg internal call
  sites (in `.s1_prepare_skeleton()`, `.s1b_worker()` cache reuse, and
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md))
  have been updated.

- **`RegistryStudy$process_skeletons()` signature changed**: drops the
  `process_fn` callback argument. Callers must pre-register their
  pipeline functions via `$register_framework(fn)` and
  `$register_randvars(name, fn)` before calling `$process_skeletons()`.
  The new three-phase orchestration re-runs each phase only when its
  relevant part of the pipeline has changed:

  - Phase 1 (framework): full rebuild on function body/formals hash
    change.
  - Phase 3 (randvars): ordered per-step divergence-point rewind-and-
    replay. Editing one step replays that step and everything downstream
    of it; upstream steps untouched.
  - Phase 2 (codes): per-entry fingerprint diff. Adding/removing one
    code entry touches only that entry’s columns on existing skeletons.
    For the typical “edit one ICD-10 code” workflow, this drops the
    re-run cost from a full pipeline rebuild to roughly one-Nth of phase
    2, where N is the number of registered code entries.

- **`RegistryStudy` schema bump (3 → 4)**: existing `registrystudy.qs2`
  files with an older schema error on load via `$check_version()` with
  an actionable message pointing at re-running the upstream generator
  script.

### New features

- **`Skeleton` R6 class** (`R/r6_skeleton.R`). Public methods:
  `initialize`, `check_version`, `pipeline_hash`, `apply_code_entry`,
  `drop_code_entry`, `sync_with_registry` (phase 2 incremental diff),
  `sync_randvars` (phase 3 divergence-point rewind-and-replay), `save`,
  `print`. `drop_code_entry` uses metadata prediction via the new
  file-level `.entry_columns(reg)` helper rather than a runtime column
  map.

- **New `RegistryStudy` methods**:

  - `$register_framework(fn)` and `$register_randvars(name, fn)` for
    phase 1 / phase 3 registration.
  - `$code_registry_fingerprints()` returning xxhash64 digests of each
    `code_registry` entry’s
    `(codes, label, groups, fn_args, combine_as)` tuple.
  - `$pipeline_hash()` returning a single xxhash64 over (framework hash,
    ordered randvars sequence hashes, code registry fingerprints).
    Answer to “what would a freshly-built skeleton look like?”
  - `$load_skeleton(batch_number)` / `$save_skeleton(sk)` as thin
    wrappers around `Skeleton$save()` that supply
    `self$data_skeleton_dir` automatically, mirroring the existing
    `$load_rawbatch()` / `$save_rawbatch()` pattern.
  - `$skeleton_pipeline_hashes()` returning a per-batch summary
    `data.table` with each skeleton’s current `pipeline_hash`. Useful
    for spotting batches out of sync with each other.
  - `$assert_skeletons_consistent()` as a pre-flight check for
    downstream consumers: errors on mixed-hash or partially-rebuilt
    state.
  - `$write_pipeline_snapshot()` writing a one-row TSV to
    `{data_pipeline_snapshot_dir}/{host_label}.tsv`. Git-trackable,
    concurrency-safe (each host writes only its own file), silently
    skipped when the snapshot candidate directory is not configured or
    not mounted on the current host.
  - `$adopt_runtime_state_from(other)` copies runtime fields (`n_ids`,
    `n_batches`, `batch_id_list`, `groups_saved`) from another
    `RegistryStudy` without touching config fields. Used by generator
    scripts to reload disk state without silently adopting stale
    `group_names` or `code_registry`.

- **New `RegistryStudy` public fields**: `framework_fn`, `randvars_fns`
  (named ordered list), `host_label`, `data_pipeline_snapshot_cp`
  (\[CandidatePath\]).

- **New active binding `data_pipeline_snapshot_dir`** resolving from
  `data_pipeline_snapshot_cp`, parallel to the existing
  `data_rawbatch_dir` / `data_skeleton_dir` / `data_raw_dir` accessors.

- **File-level helpers** in `r6_registrystudy.R`: `.hash_function(fn)`
  (xxhash64 over `list(body(fn), formals(fn))` – stable across R
  sessions because it excludes the function environment),
  `.fingerprint_entry(reg)`, `.entry_columns(reg)` (vectorized wrapper
  around the previously-private `.generated_columns_for_entry()` which
  has been lifted to file level), `.format_batch_range(batches)`, and
  `.process_one_batch(study, i, ...)` (the per-batch orchestration
  shared by `process_skeletons()`’s serial and callr-parallel branches).

### Memory footprint note

`$load_skeleton()` calls
`data.table::setalloccol(obj$data, n = getOption("datatable.alloccol", 4096L))`
on the loaded `Skeleton$data` to restore data.table over-allocation
slots that qs2 serialization drops. Memory overhead is a new data.table
HEADER (~8-16 bytes per slot, so ~32-64 KB total) – NOT a full copy of
the column data, which stays shared by reference. This is negligible
compared to the per-batch skeleton size (typically multi-GB). Without
the refresh, subsequent `:=` mutations inside helper functions would
silently reallocate and strand the R6 field pointing at the stale
old-address data.table. Studies that need more than 4096 column slots
can bump via `options(datatable.alloccol = 8192L)` at the top of their
generator script.

### Other changes

- 6 existing `process_skeletons` test cases were rewritten to use the
  new `$register_framework()` idiom.
- Added `tests/testthat/test-r6_skeleton.R` (74 tests),
  `tests/testthat/test-process_skeletons_incremental.R` (47 tests),
  `tests/testthat/test-entry_columns_parity.R` (10 tests), and 56 new
  unit tests in `test-registrystudy.R` for the new methods.
- Total test count: 800 tests passing.

## swereg 26.4.18

### BREAKING CHANGES

- **`TTEPlan` schema bump (1 → 2) and new constructor signature**:
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)
  now requires `candidate_dir_spec`, `candidate_dir_tteplan`,
  `candidate_dir_results`, and `spec_version`. The previous positional
  `spec` argument is removed — the spec YAML is now read from inside the
  resolved `candidate_dir_spec` using `filename_spec(spec_version)`.
  Existing `_plan.qs2` files error on load via `check_version()`;
  regenerate by re-running `s0_init.R` for each project.

- **`RegistryStudy` schema bump (2 → 3)**: private
  `.data_{rawbatch,skeleton,raw}_dir_candidates` and `_cache` fields are
  replaced by public `data_{rawbatch,skeleton,raw}_cp` fields of type
  `CandidatePath`. Existing `registry_study_meta.qs2` files error on
  load; regenerate by re-running `run_generic_create_datasets_v2.R`.

- **Stub-free on-disk filenames**. Project-scoped directories no longer
  need a `{project_id}_` prefix on files. The rename is:

  - `registry_study_meta.qs2` → `registrystudy.qs2`
  - `{project_id}_plan.qs2` → `tteplan.qs2`
  - `{project_id}_spec.xlsx` → `spec.xlsx`
  - `{project_id}_tables.xlsx` → `tables.xlsx`
  - `study_spec_vXXX.yaml` → `spec_vXXX.yaml` Use
    `dev/rename_r6_files.sh` in the downstream MHT repo for the on-disk
    migration.

### New features

- **`CandidatePath` R6 class** (`R/r6_candidate_path.R`). First-class
  representation of “a directory that lives at one of several candidate
  locations depending on host”. Owns its candidate list, its resolution
  cache, and its `$resolve()` / `$invalidate()` / `$is_resolved()` /
  `$print()` methods. Both `RegistryStudy` and `TTEPlan` now hold
  `CandidatePath` instances via public `*_cp` fields, so multi-host path
  resolution is structurally identical across classes — cannot drift.

- **`first_existing_path(candidates, label)`** (exported). Generic,
  study-agnostic “first existing path” picker lifted from the old
  `.resolve_path()`. Auto-creates the first candidate whose parent
  directory exists (unchanged from the old behavior).

- **`invalidate_candidate_paths(obj)`** (exported). Walks an R6 object’s
  public fields and calls `$invalidate()` on every `CandidatePath` it
  finds, recursing into embedded R6 objects. Called by
  `RegistryStudy$save_meta()` and `TTEPlan$save()` before serialization
  so on-disk files never carry the saving host’s cached resolved paths.

- **`tteplan_locate_and_load(candidate_dir_tteplan)`** (exported). Stage
  scripts (`s1.R`, `s2.R`, `s3.R`, `s4_export.R`) use this one-liner to
  load a `tteplan.qs2` from the first candidate directory that exists.

- **`registrystudy_load(candidate_dir_rawbatch)`** (exported). Paired
  with
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)
  so `s0_init.R` reads `registrystudy.qs2` from the first rawbatch
  directory that exists on the current host.

- **`TTEPlan` active bindings for owned paths**: `dir_tteplan`,
  `dir_spec`, `dir_results_base`, `dir_results` (appends
  `spec_version`), `tteplan`, `spec_path`, `spec_xlsx`, `tables_xlsx`,
  plus `data_skeleton` and `data_rawbatch` that delegate to the embedded
  `registrystudy` (no duplication). Stage methods default to these
  bindings, so `s1/s2/s3` no longer need an explicit `output_dir =`.

- **Host-portable `skeleton_files`** after load.
  [`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md)
  now refreshes `plan$skeleton_files` from
  `plan$registrystudy$skeleton_files` on every load, reapplying the
  `n_skeleton_files_limit` stored on the plan. A plan saved on one host
  and loaded on another immediately points at the current host’s
  skeleton files.

## swereg 26.4.17

### New features

- **Custom Table 1 engine**: `swereg` now ships its own Table 1 builder
  (`.swereg_table1()`), replacing the optional `tableone` dependency.
  `TTEEnrollment$table1()` returns a long-format `data.table` with class
  `swereg_table1` and supports new arguments:

  - `arm_labels` — display labels for the two exposure arms
  - `include_smd` — toggle SMD column (defaults TRUE)
  - `show_missing` — annotate variable names with `"(missing X.X%)"`
    instead of emitting a separate `Missing` row (defaults TRUE).
    Percentages for non-missing levels are computed against the
    non-missing denominator, so they sum to 100 within each column.
    Multi-level categorical SMDs follow the Yang & Dalton (2012)
    generalisation.

- **Forest plot for Table 3**: the workbook’s Table 3 sheet is now a
  forest plot rendered with `ggplot2`. Supplemental `Table S{n+1}` keeps
  the full tabular IRR for all ETTs (per-protocol truncated). The forest
  plot is delivered as a high-resolution PNG (300 dpi) embedded in the
  worksheet, plus a vector PDF sidecar saved next to the workbook.

- **CONSORT flowcharts**: `.write_consort()` now renders a Graphviz DOT
  flowchart via `DiagrammeR` + `DiagrammeRsvg` + `rsvg`, embeds the PNG
  into the worksheet, and saves PNG/PDF sidecars next to the workbook.
  Falls back to the legacy text-table layout when the optional packages
  are missing.

- **`featured_etts` argument** on `TTEPlan$export_tables()`: filters
  Tables 2 and 3 to a user-specified subset of ETT ids; the
  supplementary tabular IRR remains unfiltered. An “Exposure
  definitions” legend block is written above each main table, and the
  `_Exposed`/`_Unexposed` column suffixes are rewritten to spec-derived
  arm labels when all featured ETTs share a single enrollment.

- **`TTEPlan$reload_spec()`**: refreshes cosmetic spec fields (study
  title, enrollment names, exposure-arm labels, outcome names, ETT
  descriptions) from a YAML spec on disk WITHOUT re-running the upstream
  pipeline. Structural changes (confounders, exclusion criteria,
  follow-up windows, matching parameters, etc.) are detected and
  reported via a loud warning but NOT applied — cached results stay
  bound to the old definitions. The new fields `spec_reloaded_at` and
  `spec_reload_skipped_diffs` are surfaced on the Provenance sheet.

- **`TTEPlan$recompute_baselines()`**: re-runs the new Table 1 engine on
  cached enrollment files in-process, used to refresh stale baseline
  tables after upgrading swereg without re-running `$s3_analyze()`.
  `$export_tables()` calls this lazily when it detects pre-26.4.17
  cached Table 1 results.

### Workbook output changes

- Supplementary baseline panels are renamed:
  - `Raw` → `Unimputed and unweighted`
  - `Unweighted` → `Imputed and unweighted`
  - `IPW` → `Imputed and IPW`
  - `IPW Truncated` → `Imputed and IPW truncated`
- The main Table 1 sheet hides the SMD column and missingness
  annotations; supplementary panels include both.

### Breaking changes

- `TTEEnrollment$table1()` now returns a `data.table` (class
  `c("swereg_table1", "data.table", "data.frame")`) instead of a
  `tableone` S3 object. Code that introspected `TableOne` fields will
  need to read from the long-format columns instead.
- `tableone` is removed from `Suggests`. Cached `results_enrollment`
  lists produced by older versions are recognised and refreshed lazily
  on the next `$export_tables()` call (an `output_dir` is required for
  the refresh).
- `ggplot2` is now in `Imports` (was `Suggests`) since the forest plot
  is a mandatory part of `$export_tables()`.
- `DiagrammeR`, `DiagrammeRsvg`, and `rsvg` are added to `Suggests`.
  They are required for CONSORT flowcharts; without them
  `$export_tables()` silently falls back to the legacy text CONSORT.

## swereg 26.4.16

### Improvements

- [`setup_progress_handlers()`](https://papadopoulos-lab.github.io/swereg/reference/setup_progress_handlers.md):
  Pick `handler_progress()` format based on
  [`interactive()`](https://rdrr.io/r/base/interactive.html). In
  interactive sessions use `\r`-based single-line repaint
  (`clear = TRUE`, no trailing newline) so the bar updates in place like
  a normal terminal progress bar. In non-interactive sessions (RStudio
  background jobs, Rscript, CI) use a trailing `\n` with `clear = FALSE`
  so each step is a new line in the log.

## swereg 26.4.15

### Improvements

- `RegistryStudy$process_skeletons()`: Pass the current timestamp as the
  progress `message` (both sequential and parallel paths), matching the
  convention already used in
  [`parallel_pool()`](https://papadopoulos-lab.github.io/swereg/reference/parallel_pool.md).
  The `(last: :message)` suffix in the
  [`setup_progress_handlers()`](https://papadopoulos-lab.github.io/swereg/reference/setup_progress_handlers.md)
  format string now shows the clock time of the last completed batch
  (e.g. `(last: 14:35:22)`) so you can tell at a glance whether the job
  is making progress or frozen. Previously called `p()` with no message,
  so `(last: )` was always blank.

## swereg 26.4.14

### Bug Fixes

- [`setup_progress_handlers()`](https://papadopoulos-lab.github.io/swereg/reference/setup_progress_handlers.md):
  The real reason progress never showed up in RStudio background jobs –
  `progressr` silently suppresses reporting in non-interactive sessions
  unless you set `options("progressr.enable" = TRUE)`. Background jobs
  have `interactive() == FALSE`, so the global handler was being
  installed correctly but `progressor()` calls were emitting no output.
  Now forces the option on. Also restores `(last: :message)` in the
  format so you can tell the bar isn’t frozen by watching the item label
  advance.

## swereg 26.4.13

### Improvements

- [`setup_progress_handlers()`](https://papadopoulos-lab.github.io/swereg/reference/setup_progress_handlers.md):
  Drop the `handler_rstudio` / rstudioapi branch entirely. Use
  `handler_progress()` with
  `format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta\n"`
  and `clear = FALSE` in every context — same recipe as
  `cs9::set_progressr` and `plnr::Plan$run_all`. The trailing `\n` makes
  each update a new log line (instead of a `\r` repaint that job logs
  can’t render), and `clear = FALSE` keeps finished bars in the
  scrollback. Works in interactive R, RStudio’s foreground console,
  *and* RStudio background-job subprocesses without any detection logic
  or handler switching. `handler_rstudio` has been ineffective for the
  background-job case in this codebase.

## swereg 26.4.12

### Improvements

- [`setup_progress_handlers()`](https://papadopoulos-lab.github.io/swereg/reference/setup_progress_handlers.md):
  Collapse the RStudio-detection logic to a single
  `rstudioapi::isAvailable(child_ok = TRUE)` call. The `child_ok = TRUE`
  parameter handles both the foreground RStudio console and
  background-job subprocesses (via IPC to the parent session), so all
  the earlier `hasFun`/`exists`/`isJob` gymnastics were unnecessary.
  Also drops the now-redundant feature-test for
  `jobAdd`/`jobSetProgress`/`jobRemove` —
  [`progressr::handler_rstudio`](https://progressr.futureverse.org/reference/handler_rstudio.html)
  requires those to exist anyway.

## swereg 26.4.11

### Bug Fixes

- [`setup_progress_handlers()`](https://papadopoulos-lab.github.io/swereg/reference/setup_progress_handlers.md):
  Stop using `rstudioapi::hasFun("jobAdd")` to feature-test for the job
  wrappers. `hasFun()` (a) short-circuits to `FALSE` whenever
  `isAvailable()` is `FALSE`, and (b) looks the name up in the internal
  `rstudio` namespace where the function is actually called `addJob`
  (the
  [`rstudioapi::jobAdd()`](https://rstudio.github.io/rstudioapi/reference/jobAdd.html)
  wrapper forwards to `callFun("addJob", ...)`). So `hasFun("jobAdd")`
  returned `FALSE` even on systems where `jobAdd()` works fine, which
  caused the helper to fall through to the text handler every time. Now
  checks the `rstudioapi` wrapper namespace directly via
  `exists("jobAdd", envir = asNamespace("rstudioapi"), mode = "function")`.

## swereg 26.4.10

### Bug Fixes

- [`setup_progress_handlers()`](https://papadopoulos-lab.github.io/swereg/reference/setup_progress_handlers.md):
  Fix detection of RStudio background-job subprocesses. Previously
  relied on
  [`rstudioapi::isAvailable()`](https://rstudio.github.io/rstudioapi/reference/isAvailable.html),
  which returns FALSE inside a `jobRunScript()` subprocess (because
  `.Platform$GUI` is not “RStudio” there) — so scripts launched via
  *Source as Background Job* fell through to the text handler and no
  Jobs-pane progress bar appeared. Now also accepts
  [`rstudioapi::isJob()`](https://rstudio.github.io/rstudioapi/reference/isJob.html)
  as a valid context; in job subprocesses,
  [`rstudioapi::callFun()`](https://rstudio.github.io/rstudioapi/reference/callFun.html)
  auto-delegates `jobAdd`/`jobSetProgress`/`jobRemove` back to the
  parent RStudio session via IPC, so `handler_rstudio` works correctly.

## swereg 26.4.9

### New Features

- [`setup_progress_handlers()`](https://papadopoulos-lab.github.io/swereg/reference/setup_progress_handlers.md):
  Helper for run scripts. Feature-detects
  [`rstudioapi::jobAdd()`](https://rstudio.github.io/rstudioapi/reference/jobAdd.html)
  and installs
  [`progressr::handler_rstudio()`](https://progressr.futureverse.org/reference/handler_rstudio.html)
  when available, else falls back to `handler_progress()`. Fixes the “no
  good progress bar” problem when launching run scripts via RStudio’s
  *Source as Background Job* menu — the default text bar renders badly
  in job logs, the RStudio handler draws a proper Jobs-pane progress
  bar. Automatically covers every progressr-emitting method
  (`process_skeletons`, `s1_*`, `s2_*`, `s3_*`) with no per-method
  changes.

## swereg 26.4.8

### New Features

- `RegistryStudy$compute_population(by)`: Compute a population
  denominator table from saved skeleton files. Counts unique persons by
  `isoyear` and user-specified structural variables (e.g. sex, age,
  register tag). Handles both annual and weekly skeleton rows via
  `uniqueN(id)`. Produces a complete grid with all combinations (missing
  cells filled with zero). Saves result as `population.qs2` in the
  skeleton directory.

## swereg 26.4.7

### Improvements

- `s3_analyze()` now prints the output directory path and count of .qs2
  files before processing.
- `s3_analyze()` gains `ett_ids` parameter to run only specific ETTs
  (e.g. `ett_ids = "ETT01"`).
- Remove heterogeneity test (`het_test`) from `s3_analyze()` — a single
  call consumed 42GB RAM and 40+ minutes CPU on real data, making full
  runs infeasible.
- Remove `het_slot` parameter from
  [`tteenrollment_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_irr_combine.md)
  (no longer needed).

## swereg 26.4.6

### Bug Fixes

- Fix latent bug in
  [`parallel_pool()`](https://papadopoulos-lab.github.io/swereg/reference/parallel_pool.md):
  `Filter(Negate(is.null), results)` removed NULLs and shifted indices,
  breaking positional `item_map` indexing in `s3_analyze`. Workers that
  fail already raise errors before producing output, so NULL results
  cannot occur.

### Internal

- Deduplicate `.write_combined_rates()` and `.write_combined_irr()` into
  shared helper `.prepare_combine_data()`.
- Extract `.build_code_lookup()` helper for code_lookup + fmt_var
  construction, shared by `print_spec_summary()` and
  `.write_spec_summary()`.
- Call
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
  once at top of `s3_analyze()` instead of twice in the enrollment and
  ETT loops.

## swereg 26.4.5

### New Features

- **`$s3_analyze()`**: New Loop 3 method on TTEPlan that computes all
  analysis results (baseline characteristics, rates, IRR, heterogeneity
  tests) and stores them on the plan. Split into enrollment-level
  results (table1 variants) and ETT-level results (outcome-specific).
  Degenerate ETTs (GLM failure) are caught and stored with a skip reason
  instead of crashing. Progress bars via progressr.
- **`$results_summary()`**: Print diagnostic table showing event counts
  and IRR/rates status per ETT.
- **`$export_tables(path)`**: Export all results to a multi-sheet Excel
  workbook: enrollment overview, ETT overview, Table 1 (chosen
  enrollment), Tables 2-3 (combined rates/IRR), per-enrollment combined
  baselines (4-panel: Raw/Unweighted/IPW/IPW Truncated), CONSORT
  attrition sheets, supplemental rates/IRR.
- **`tteplan_load(path)`**: Load a TTEPlan from disk with the current
  class definition, ensuring new methods are available on old serialized
  objects.
- `$s1_generate_enrollments_and_ipw(resume = TRUE)` and
  `$s2_generate_analysis_files_and_ipcw_pp(resume = TRUE)` skip
  completed work based on file timestamps (must be \<24h old).

### Bug Fixes

- Fix `'from' must be of length 1` crash in `enroll()` when a skeleton
  file has no enrolled persons for the current enrollment. data.table
  evaluates `j` once on 0-row data even with `by`, giving by-variables
  length 0 instead of scalar. This also produced spurious `-Inf`
  warnings from `max(logical(0), na.rm = TRUE)` in Phase B. Fix:
  short-circuit `enroll()` with an empty panel when `entry_dt` has 0
  rows.
- IPCW GLM fallback: `fit_and_predict()` in `s6_ipcw_pp` now uses
  tryCatch; falls back to marginal censoring rate when the model fails
  (e.g. near-zero events).

## swereg 26.4.3

### Breaking Changes

- [`parallel_pool()`](https://papadopoulos-lab.github.io/swereg/reference/parallel_pool.md)
  rewritten to use `processx` + qs2 tempfiles instead of `future.callr`.
  Worker logic moved to standalone R scripts in `inst/` (`worker_s1a.R`,
  `worker_s1b.R`, `worker_s2.R`), launched via
  `processx::process$new()`. All data passes through qs2 files on disk
  instead of R’s IPC serialization, fixing the loop 1b bottleneck where
  `enrolled_ids` was serialized N times through pipe buffers.
  `enrolled_ids` is now written once to a shared tempfile. Dependencies
  `future`, `future.apply`, `future.callr` removed; `processx` added.

## swereg 26.4.2

### Breaking Changes

- [`parallel_pool()`](https://papadopoulos-lab.github.io/swereg/reference/parallel_pool.md)
  rewritten to use `future.callr` instead of persistent
  [`callr::r_session`](https://callr.r-lib.org/reference/r_session.html)
  workers. Each work item now runs in a fresh R subprocess, eliminating
  deadlocks caused by accumulated IPC socket state. New dependencies:
  `future`, `future.apply`, `future.callr`. The `processx` dependency is
  removed. `callr_kill_workers()` is removed (no longer needed).

### Internal

- Rename `.s2_worker()` (was `.s3_worker()`) to match
  `$s2_generate_analysis_files_and_ipcw_pp()`.

## swereg 26.3.30

### Improvements

- `callr_pool()` gains a `timeout_minutes` parameter (default: 30). If a
  work item runs longer than the timeout, its worker is killed and the
  item is retried once. If the retry also times out, `callr_pool()`
  calls [`stop()`](https://rdrr.io/r/base/stop.html). Disable with
  `timeout_minutes = NULL`.

### CRAN compliance

- Move `mgcv` from Imports to Suggests (only used conditionally via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)).
- Add `@importFrom` for `progressr` and
  [`utils::getFromNamespace`](https://rdrr.io/r/utils/getFromNamespace.html)
  to satisfy NAMESPACE checks.
- Replace `swereg:::` calls with
  [`getFromNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html)
  in callr worker sessions.
- Replace `assign(..., globalenv())` with a package-level environment
  (`.swereg_env`).
- Add `var <- NULL` declarations for all data.table NSE variables.
- Add `.vscode` to `.Rbuildignore`.

## swereg 26.3.23

### Improvements

- `callr_pool()` workers now self-terminate if the parent R session dies
  (e.g. OOM kill). Each worker spawns a lightweight shell watchdog that
  polls the parent PID every 5 seconds. Previously, orphaned workers ran
  indefinitely until manually cleaned up via `callr_kill_workers()`.

### Bug Fixes

- **Critical**: `.s1_eligible_tuples()` used `first(rd_exposed)` to
  classify exposure at each trial period, which only detected MHT
  initiation if it happened on the first week of a 4-week trial period.
  With `period_width = 4`, ~75% of exposed people start MHT mid-period
  and were silently dropped — their first trial period showed them as
  unexposed (week 1 was pre-initiation), and the next period excluded
  them for prior MHT. Fixed by using `any(rd_exposed, na.rm = TRUE)`
  instead. The existing `no_prior_exposure` exclusion correctly handles
  the new-user restriction. Verified: eligible exposed count on
  skeleton_001 went from 19 → 84, matching the old per-week pipeline.

- `.s1_compute_attrition()`: exposure classification now uses
  [`any()`](https://rdrr.io/r/base/any.html) per person-trial instead of
  checking the first eligible row. Aligns attrition reporting with the
  [`any()`](https://rdrr.io/r/base/any.html) fix in
  `.s1_eligible_tuples()` — previously the attrition flow underreported
  exposed counts by ~4x.

- [`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md):
  missing variables (confounders, outcomes, exclusion criteria,
  exposure) now [`stop()`](https://rdrr.io/r/base/stop.html) instead of
  [`warning()`](https://rdrr.io/r/base/warning.html). Previously, a
  misspelled or renamed variable would silently pass validation and
  break downstream (e.g. IPW model missing a confounder). Category
  mismatches (values in spec but not data) remain as warnings since they
  can occur in small batches.

## swereg 26.3.20

### Bug Fixes

- `.s1_compute_attrition()`: fix undercounting of person-trials for
  row-level eligibility criteria (e.g. `eligible_valid_exposure`). The
  old code checked only the first row per person-trial, missing cases
  where exposure onset occurred after the first week. The new approach
  filters to eligible rows first, then counts — matching the logic used
  by `.s1_eligible_tuples()`.

- `.s1_compute_attrition()`: fix negative exposed/comparator deltas in
  participant flow. The `before_exclusions` baseline now classifies
  exposure from the first row with non-NA exposure per person-trial,
  rather than the first overall row (which often has `rd_exposed = NA`).
  Total person-trial counts remain unfiltered.

### Performance

- TTE s1 pipeline: add
  [`data.table::setkey()`](https://rdrr.io/pkg/data.table/man/setkey.html)
  calls to eliminate redundant hash-based grouping. Skeleton reads in
  `.s1_prepare_skeleton()` and `.s1b_worker()` now set key on
  `(id, isoyearweek)` (metadata-only, no re-sort). `enroll()` Phase B
  collapse uses keyed grouping on `(pid, trial_id)`, and Phase D panel
  expansion uses keyed binary join instead of
  [`merge()`](https://rdrr.io/r/base/merge.html).

### Bug Fixes

- `callr_pool()` PID files now written to `/tmp` instead of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) so that orphaned
  workers from crashed R sessions can be discovered and cleaned up by
  new sessions.

- `callr_kill_workers()` simplified to orphan-only cleanup: kills
  workers whose parent R process is dead and removes stale PID files.
  Own-session cleanup is already handled by `callr_pool()`’s
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html) handler; this
  function is only needed after hard crashes (SIGKILL, OOM).

### Performance

- `callr_pool()` now uses persistent
  [`callr::r_session`](https://callr.r-lib.org/reference/r_session.html)
  workers instead of spawning a fresh
  [`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html) process
  per work item. The swereg namespace is loaded once per worker slot
  rather than once per item, eliminating redundant startup overhead when
  scaling to large numbers of items.

- Orphan protection: `callr_pool()` writes a PID file per invocation and
  cleans up orphaned worker sessions from previous crashed runs
  (e.g. OOM kills) on the next invocation.

### Bug Fixes

- Fixed 3 test failures in `test-tte_spec.R` caused by s1 pipeline
  changes: added missing `rd_exposed` column to `.s1_compute_attrition`
  test fixtures, added `n_exposed`/`n_unexposed` to mock attrition data,
  and updated matching output expectations.

### Performance

- `s1_generate_enrollments_and_ipw()` now caches prepared skeletons
  between s1a (scout) and s1b (enrollment) passes, eliminating redundant
  file reads and exclusion processing. Expected ~30-40% reduction in
  per-enrollment wall-clock time.

- `.s1b_worker()` now subsets the skeleton to enrolled persons before
  computing derived confounders, avoiding expensive rolling-window
  operations on non-enrolled persons.

- `TTEEnrollment$new()` accepts `own_data = TRUE` to skip the defensive
  [`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html)
  when the caller will not reuse the data. Used in `.s1b_worker()` where
  the skeleton is discarded immediately after.

- `enroll()` Phase B now aggregates confounders, time-exposure, and
  outcome columns in a single groupby pass instead of four separate
  passes with merges.

### Improvements

- “Valid exposure” (`eligible_valid_exposure`) is now the first
  exclusion criterion in the TTE attrition flow. Rows where `rd_exposed`
  is NA are explicitly accounted for rather than silently disappearing
  between the before-exclusions total and the first real criterion.

- TARGET Item 8 (participant flow) now shows a richer flow diagram with
  before-exclusion counts, per-step exposed/unexposed breakdown, delta
  (excluded) and remaining counts at each criterion, right-justified
  aligned columns, and color-coded output (red for exclusions, cyan for
  remaining). Post-matching line also reformatted with arrow indicator.
  “Before exclusions” line no longer shows a meaningless
  exposed/comparator breakdown.

- `enrollment_counts$attrition` now includes `n_exposed` and
  `n_unexposed` columns and a `"before_exclusions"` row.

### Bug Fixes

- Fixed `trial_id` missing error caused by `attr<-` breaking
  data.table’s internal self-reference. Replaced with
  [`data.table::setattr()`](https://rdrr.io/pkg/data.table/man/setattr.html)
  in `.s1_prepare_skeleton()` and
  [`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md)
  to preserve in-place modification semantics.

- Fixed callr worker stale-namespace bug: after
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
  in a subprocess, worker functions still referenced the old (installed)
  swereg namespace. Now rebinds the worker function’s environment to the
  freshly-loaded namespace.

### Improvements

- Reorganized `print_spec_summary()` header layout: renamed “Study
  created” → “RegistryStudy”, merged “Skeletons created” + “Skeleton
  files” into a single nested line with tree connector, renamed “Plan
  created” → “TTEPlan”, and reordered to follow data pipeline order.

- Rewrote TARGET checklist items 6c, 6h, and 7a-h in
  `print_target_checklist()` as academic prose suitable for copy-pasting
  into a methods section. Item 6c now dynamically reflects
  per-enrollment matching ratios from the spec.

### Breaking changes

- **`enrollment_counts` structure changed**: Each element of
  `TTEPlan$enrollment_counts` is now a list with `$attrition` and
  `$matching` sub-elements (was a single data.table). Code accessing
  `plan$enrollment_counts[["01"]]` directly as a data.table must update
  to `plan$enrollment_counts[["01"]]$matching`.

- **`person_trial_id` renamed to `enrollment_person_trial_id`**: The
  composite key column now has a 3-part name matching its 3-part format
  (`enrollment_id.person_id.trial_id`). All code referencing
  `person_trial_id` must be updated.

- **`process_fn` parameter removed from
  `$s1_generate_enrollments_and_ipw()`**: The two-pass spec-driven
  pipeline is now the only code path. `self$spec` is required (create
  plans with
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)).
  The legacy single-pass `.s1_worker()` has been deleted.

- **`.s2_worker()` renamed to `.s3_worker()`**: Internal Loop 2 IPCW-PP
  worker renamed to avoid confusion with the two-pass Loop 1 pipeline.

### New features

- **Two-pass enrollment pipeline**: `$s1_generate_enrollments_and_ipw()`
  now uses a two-pass pipeline that fixes cross-batch matching ratio
  imbalance:

  1.  **Pass 1a (scout)**: Lightweight parallel pass collecting eligible
      `(person_id, trial_id, exposed)` tuples from all batches.
  2.  **Centralized matching**: Combines all tuples and performs
      per-`trial_id` matching globally, ensuring the correct ratio
      across all batches.
  3.  **Pass 1b (full enrollment)**: Parallel pass using pre-matched IDs
      to enroll without per-batch matching.

- **`enrollment_counts` on TTEPlan**: New field storing per-trial
  matching counts (total vs enrolled, exposed vs unexposed) for TARGET
  Item 8 reporting.

- **`.assign_trial_ids()`**: New shared helper function that is the
  single source of truth for `isoyearweek -> trial_id` mapping. Used
  consistently by both scout (s1a) and enrollment (s1b/enroll) phases.

- **`enrolled_ids` parameter on `TTEEnrollment$new()`**: When provided,
  enrollment skips the matching phase and uses pre-decided IDs directly,
  enabling the two-pass pipeline.

- **Per-criterion attrition counts for TARGET Item 8**: The scout pass
  (s1a) now computes cumulative person and person-trial counts at each
  eligibility step. Stored in `plan$enrollment_counts[["01"]]$attrition`
  as a long-format data.table with columns `trial_id`, `criterion`,
  `n_persons`, `n_person_trials`. `$print_target_checklist()` Item 8
  auto-populates with these counts when available.

## swereg 26.3.21

### New features

- **`$heterogeneity_test()`**: New method on `TTEEnrollment` that tests
  for heterogeneity of treatment effects across trials via a Wald test
  on the `trial_id × exposure` interaction (Hernán 2008, Danaei 2013).

- **`$print_target_checklist()`**: New method on `TTEPlan` that
  generates a self-contained TARGET Statement (Cashin et al., JAMA 2025)
  21-item reporting checklist. Auto-populates items from the study spec
  and provides `[FILL IN]` placeholders for PI completion.

### Improvements

- **`$irr()` calendar-time adjustment**: Outcome model now includes
  `trial_id` as a covariate to adjust for calendar-time variation in
  outcome rates across enrollment bands (Caniglia 2023, Danaei 2013).
  Uses `ns(trial_id, df=3)` for ≥5 unique trial IDs, linear term for
  2-4, omitted for 1.

- **`$irr()` IPW-only guard**: `$irr()` now rejects IPW-only weight
  columns (`ipw`, `ipw_trunc`) after per-protocol censoring has been
  applied. The swereg pipeline applies per-protocol censoring in
  `$s4_prepare_for_analysis()`, so only per-protocol weights
  (`analysis_weight_pp_trunc`) are valid for the censored dataset.

### Documentation

- **Methodology vignette**: New
  [`vignette("tte-methodology")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methodology.md)
  maps the swereg TTE implementation to five reference papers (Hernán
  2008/2016, Danaei 2013, Caniglia 2023, Cashin 2025). Documents which
  methods are implemented, which are not, and design rationale.

- **Analysis types**:
  [`vignette("tte-nomenclature")`](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.md)
  now documents that swereg supports **per-protocol** analysis only. ITT
  analysis is not supported because the pipeline censors at protocol
  deviation. As-treated analysis requires time-varying IPW (not
  implemented).

- **`period_width` documentation**:
  [`vignette("tte-nomenclature")`](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.md)
  now explains the enrollment band width / residual immortal time bias
  trade-off, citing Caniglia (2023) and Hernán (2016).

- **Matching approach**:
  [`vignette("tte-nomenclature")`](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.md)
  now documents the per-band stratified matching design choice and
  alternatives from the literature.

- **`$s2_ipw()` documentation**: Clarified that IPW estimates the
  propensity score for baseline treatment assignment only, not
  time-varying treatment weights.

- **`$irr()` documentation**: Documented IRR ≈ HR for rare events,
  `ns(tstop)` for flexible baseline hazard, `quasipoisson` for
  overdispersion, and computational equivalence to pooled logistic
  regression.

- **IPCW stabilization**: Documented the simplified marginal
  stabilization approach and its relationship to Danaei (2013).

### Tests

- Added tests for `$rates()`, `$irr()`, `$km()`, `$irr()` with
  `trial_id`, IPW-only guard, and IPCW formula with `trial_id`.

## swereg 26.3.20

### Improvements

- **Band-based enrollment**: Added explicit `isoyearweek` ordering
  before band-level collapse to prevent silent misclassification when
  input data is not pre-sorted by time.
- **IPCW-PP**: Censoring model now includes `trial_id` to account for
  calendar-time variation in censoring patterns across enrollment bands.
- **`person_weeks`**: Now computed from actual source row counts during
  band collapse instead of hardcoded `period_width`. Partial-coverage
  bands (e.g., at data boundaries) now contribute accurate person-time.

### Breaking changes

- **`$irr()`**: Removed the constant (no time adjustment) Poisson model.
  Only the flexible model with natural splines
  (`splines::ns(tstop, df=3)`) is retained. Output columns renamed:
  `IRR_flex` → `IRR`, `IRR_flex_lower` → `IRR_lower`, `IRR_flex_upper` →
  `IRR_upper`, `IRR_flex_pvalue` → `IRR_pvalue`, `warn_flex` → `warn`.
  All `IRR_const*` and `warn_const` columns removed.
- **[`tteenrollment_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_irr_combine.md)**:
  Updated to match new `$irr()` output. Columns renamed:
  `IRR (flexible)` → `IRR`, `95% CI (flexible)` → `95% CI`,
  `p (flexible)` → `p`. Constant-model columns removed.
- **TTE ID semantics**: The composite person-per-trial identifier column
  is now called `person_trial_id` (was `trial_id`). The actual trial
  identifier (the enrollment band) is now exposed as `trial_id` in
  enrollment output. This fixes the semantics so `trial_id` means the
  trial and `person_trial_id` identifies a person’s participation in a
  trial.
- **TTEDesign default**: `id_var` default changed from `"trial_id"` to
  `"person_trial_id"`.
- **`s1_impute_confounders()`**: No longer hardcodes `trial_id`; uses
  `design$id_var` throughout.

### Code quality

- Rename private methods `prepare_outcome` and `ipcw_pp` to
  `s5_prepare_outcome` and `s6_ipcw_pp` to signal their execution order
  within `s4_prepare_for_analysis()`.
- Reorder `TTEEnrollment` public step methods to match their numeric
  sequence (s1 before s2).

### Breaking changes

- **Band-based enrollment**: `TTEEnrollment` enrollment now uses N-week
  bands (controlled by `period_width` in `TTEDesign`, default 4).
  Calendar time is grouped into bands based on `isoyearweek`, matching
  is done per-band (stratified), and data is collapsed to band level
  during enrollment. This eliminates the separate `$s1_collapse()` step
  entirely.

- **Step renumbering**: Public workflow methods on `TTEEnrollment` have
  been renumbered after removing `$s1_collapse()`:

  - `$s2_impute_confounders()` -\> `$s1_impute_confounders()`
  - `$s3_ipw()` -\> `$s2_ipw()`
  - `$s4_truncate_weights()` -\> `$s3_truncate_weights()`
  - `$s5_prepare_for_analysis()` -\> `$s4_prepare_for_analysis()`

- **`period_width` parameter**: Moved from
  `TTEPlan$s1_generate_enrollments_and_ipw()` to
  `TTEDesign$new(period_width = 4L)`. Now part of the design contract.

- **`isoyearweek` column required**: Band-based enrollment requires an
  `isoyearweek` column in person-week data.

- **Schema version bump**: `TTEDesign` and `TTEEnrollment` schema
  versions bumped to 2. Objects saved with version 1 will warn on load.

### New features

- **TTEPlan provenance timestamps**: TTEPlan now tracks `created_at`
  (stamped at construction), `registry_study_created_at` (from the
  source RegistryStudy), and `skeleton_created_at` (from the first
  skeleton file’s attribute). All three timestamps are shown in
  [`print()`](https://rdrr.io/r/base/print.html) and
  `print_spec_summary()` when available, making it easy to detect stale
  plans.

- **R6 schema versioning**: All R6 classes (`RegistryStudy`, `TTEPlan`,
  `TTEDesign`, `TTEEnrollment`) now carry a `.schema_version` private
  field, stamped at construction time. A new `$check_version()` public
  method compares the stored version against the current class
  definition and warns when stale.
  [`qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)
  automatically calls `$check_version()` on R6 objects after loading, so
  outdated serialized objects produce a clear warning instead of
  silently breaking.

- **Deprecation warnings for old `add_*` parameter names**:
  `add_diagnoses(diags=)`, `add_operations(ops=)`, `add_rx(rxs=)`,
  `add_icdo3s(icdo3s=)`, `add_snomed3s(snomed3s=)`, and
  `add_snomedo10s(snomedo10s=)` now emit a deprecation warning when the
  old parameter name is used. Use `codes=` instead.

### Breaking changes

- **RegistryStudy**: `register_codes()` now takes a declarative
  signature: `register_codes(codes, fn, groups, fn_args, combine_as)`.
  Each call declares codes, the function to apply them, which data
  groups to use, and optional prefix/combine behavior. The old per-type
  fields (`icd10_codes`, `rx_atc_codes`, `rx_produkt_codes`,
  `operation_codes`, `icdo3_codes`) and the old
  `register_codes(icd10_codes = ...)` signature are removed. The single
  `code_registry` list field replaces them.

- **`summary_table()`**: The `type` parameter is removed. The `type`
  column is replaced by `label`. Use `label` to filter.

- **[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)**,
  **[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md)**,
  **[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)**,
  **[`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md)**,
  **[`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md)**,
  **[`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)**:
  The codes parameter is renamed to `codes` (was `diags`, `ops`, `rxs`,
  `icdo3s`, `snomed3s`, `snomedo10s`). Old parameter names still work
  for backwards compatibility.

### Refactoring

- Moved
  [`qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)
  to its own file (`R/qs2.R`) and inlined the fallback logic directly.
  Removed pointless `.qs_save` wrapper (replaced with direct
  [`qs2::qs_save`](https://rdrr.io/pkg/qs2/man/qs_save.html) calls) and
  `.qs_read` internal helper.

### Breaking changes

- `skeleton_save()` no longer splits batches into sub-files. It saves
  one file per batch as `skeleton_NNN.qs2` (was `skeleton_NNN_SS.qs2`).
  The `ids_per_file` and `id_col` parameters have been removed.

- `RegistryStudy`: `batch_sizes` parameter (integer vector) replaced
  with `batch_size` (single integer, default 1000). The
  `ids_per_skeleton_file` parameter has been removed. All batches are
  now uniform size.

## swereg 26.3.21

### Breaking changes

- **RENAMED**: Standalone TTE functions renamed to signal which class
  they operate on:
  - `tte_rbind()` →
    [`tteenrollment_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rbind.md)
  - `tte_rates_combine()` →
    [`tteenrollment_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rates_combine.md)
  - `tte_irr_combine()` →
    [`tteenrollment_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_irr_combine.md)
  - `tte_impute_confounders()` →
    [`tteenrollment_impute_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_impute_confounders.md)
  - `tte_read_spec()` →
    [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
  - `tte_apply_exclusions()` →
    [`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md)
  - `tte_apply_derived_confounders()` →
    [`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md)
  - `tte_validate_spec()` →
    [`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md)
  - `tte_plan_from_spec_and_registrystudy()` →
    [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)
  - `tte_callr_pool()` → `callr_pool()`
- **RENAMED**: Eligibility helpers renamed from `tte_eligible_*` to
  `skeleton_eligible_*` to reflect that they operate on skeleton
  data.tables, not TTE classes:
  - `tte_eligible_isoyears()` →
    [`skeleton_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_isoyears.md)
  - `tte_eligible_age_range()` →
    [`skeleton_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_age_range.md)
  - `tte_eligible_no_events_in_window_excluding_wk0()` →
    [`skeleton_eligible_no_events_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_events_in_window_excluding_wk0.md)
  - `tte_eligible_no_observation_in_window_excluding_wk0()` →
    [`skeleton_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_observation_in_window_excluding_wk0.md)
  - `tte_eligible_no_events_lifetime_before_and_after_baseline()` →
    [`skeleton_eligible_no_events_lifetime_before_and_after_baseline()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_events_lifetime_before_and_after_baseline.md)
  - `tte_eligible_combine()` →
    [`skeleton_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_combine.md)

### File reorganization

- **RENAMED**: `R/tte_enrollment_r6.R` → `R/r6_tteenrollment.R`
- **RENAMED**: `R/tte_plan_r6.R` → `R/r6_tteplan.R`
- **RENAMED**: `R/registry_study_r6.R` → `R/r6_registry_study.R`
- **EXTRACTED**: `callr_pool()` to its own file `R/callr_pool.R`
- **MOVED**: Eligibility helpers to `R/skeleton_utils.R`
- **MOVED**:
  [`tteenrollment_impute_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_impute_confounders.md)
  to `R/r6_tteenrollment.R`

## swereg 26.3.20

### Breaking changes

- **RENAMED**: TTEEnrollment public workflow methods now have
  step-number prefixes to signal execution order:

  - `$collapse()` → `$s1_collapse()`
  - `$impute_confounders()` → `$s2_impute_confounders()`
  - `$ipw()` → `$s3_ipw()`
  - `$truncate()` → `$s4_truncate_weights()`
  - `$prepare_for_analysis()` → `$s5_prepare_for_analysis()`

- **RENAMED**: `$s4_truncate()` → `$s4_truncate_weights()` for clarity.

- **RENAMED**: TTEPlan orchestration methods now have step-number
  prefixes:

  - `$generate_enrollments_and_ipw()` →
    `$s1_generate_enrollments_and_ipw()`
  - `$generate_analysis_files_and_ipcw_pp()` →
    `$s2_generate_analysis_files_and_ipcw_pp()`

- **RENAMED**: Internal worker functions for consistent naming:

  - `.tte_process_skeleton()` → `.s1_worker()`
  - `.loop2_worker()` → `.s2_worker()`

- **REMOVED**: Constructor wrapper functions `tte_design()`,
  `tte_enrollment()`, and `tte_plan()`. Use `TTEDesign$new()`,
  `TTEEnrollment$new()`, and `TTEPlan$new()` directly. The
  auto-detection and data-copy logic from `tte_enrollment()` has been
  moved into `TTEEnrollment$new()`.

### Improvements

- **REFACTOR**: Inlined 5 of 6 private helper methods into their single
  callers on TTEEnrollment (`.calculate_ipw`, `.calculate_ipcw`,
  `.combine_weights_fn`, `.match_ratio`, `.collapse_periods`). Kept
  `.truncate_weights` as private (used in 2 places). Reduces indirection
  for stateless methods that don’t use `self`.

- **TESTS**: Rewrote `test-tte_weights.R` to test through public API
  (`$s1_collapse()`, `$s3_ipw()`, `$s4_truncate()`,
  `tte_enrollment(ratio=)`) instead of accessing inlined private
  methods.

## swereg 26.3.20

### Improvements

- **REFACTOR**: Inlined 6 weight/matching functions as private methods
  on TTEEnrollment (tte_truncate_weights, tte_calculate_ipw,
  tte_calculate_ipcw, tte_combine_weights, tte_match_ratio,
  tte_collapse_periods). Removed 2 orphaned functions
  (tte_identify_censoring, tte_time_to_event). Users access this
  functionality through R6 methods (\$collapse, \$ipw, \$truncate,
  etc.).

- **REFACTOR**: Consolidated TTE source files from 7 to 2 (+1 rename):

  - `tte_design.R` + `tte_enrollment.R` + `tte_weights.R` merged into
    `tte_enrollment_r6.R` (TTEDesign + TTEEnrollment + all
    weight/matching functions called by their methods)
  - `tte_plan.R` + `tte_spec.R` + `tte_eligibility.R` merged into
    `tte_plan_r6.R` (TTEPlan + spec functions + eligibility helpers)
  - `registry_study.R` renamed to `registry_study_r6.R`
  - Files containing R6 classes now have `_r6` suffix for
    discoverability

- **REORDER**: TTEEnrollment public methods now follow workflow
  execution order: collapse -\> ipw -\> impute_confounders -\> truncate
  -\> prepare_for_analysis -\> extract/summary/diagnostics -\> analysis
  output.

- **DOCS**: Added inline comments documenting data flow in
  `generate_enrollments_and_ipw()` (Loop 1), `.tte_process_skeleton()`,
  `private$enroll()`, `enrollment_spec()`, and `add_one_ett()`.

## swereg 26.3.18

### Improvements

- **MHT**: Added `rd_approach3b_{single,multiple}` exposure variables
  that collapse `estrogen_progesterone_bioidentical` and
  `estrogen_progesterone_synthetic` into a single
  `estrogen_progesterone` level. Derived by relabeling the finished
  approach3 columns, which is valid because switching between active MHT
  types never triggers “previous”.

- **MHT**:
  [`x2026_mht_add_lmed()`](https://papadopoulos-lab.github.io/swereg/reference/x2026_mht_add_lmed.md)
  now creates exposure variables
  (`rd_approach{1,2,3}_{single,multiple}`) internally via the new
  internal helper `x2026_mht_create_exposure_variables()`. This
  consolidates all MHT LMED logic in the package, eliminating the need
  for a separate step 14 in external workflow scripts.

- **MHT**: Removed 18 sensitivity columns (`*_sensitivity_60p`,
  `*_sensitivity_under60censorallat60`,
  `*_sensitivity_under60censorrefat65`) from
  `x2026_mht_create_exposure_variables()`. These had a logic issue where
  `local_or_none_mht` rows at age \>= 65 produced `NA` instead of
  `FALSE`. The `rd_age_continuous` column is no longer required as
  input.

## swereg 26.2.27

### Improvements

- **VALIDATION**: `tte_validate_spec()` now emits a
  [`warning()`](https://rdrr.io/r/base/warning.html) instead of
  [`stop()`](https://rdrr.io/r/base/stop.html) when spec variables or
  values are missing from the skeleton. This makes validation
  informational rather than blocking, useful when working with small
  data subsets where rare categories may be absent.

## swereg 26.2.22

### New features

- **EXPORTED**: `tte_callr_pool()` — generic
  [`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html) worker
  pool, generalized from the internal `.tte_callr_pool()`. New API
  accepts `items` (list of arg-lists), `worker_fn`, `item_labels`, and
  `collect` (FALSE to discard results when workers save directly).
  Eliminates boilerplate when scripts need their own parallel loops
  (e.g., Loop 2 IPCW-PP).

- **NEW**: `TTEPlan$generate_analysis_files_and_ipcw_pp()` — Loop 2
  method that runs per-ETT IPCW-PP calculation and saves analysis-ready
  files. Mirrors `$generate_enrollments_and_ipw()` (Loop 1). Parameters:
  `output_dir`, `estimate_ipcw_pp_separately_by_exposure`,
  `estimate_ipcw_pp_with_gam`, `n_workers`, `swereg_dev_path`.

### Improvements

- **MEMORY**: `tte_calculate_ipcw()` now uses
  `mgcv::bam(discrete = TRUE)` instead of
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) when
  `use_gam = TRUE`. `bam()` discretizes covariates to avoid forming the
  full model matrix, dramatically reducing peak memory for large
  datasets. Model objects are also explicitly freed
  ([`rm()`](https://rdrr.io/r/base/rm.html) +
  [`gc()`](https://rdrr.io/r/base/gc.html)) between exposed/unexposed
  fits.

- **MEMORY**: `$irr()` and `$km()` now subset to only the columns needed
  before creating
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html).
  Previously the full data.table (all columns) was copied into the
  survey object. Model objects and intermediate data are freed between
  fits.

## swereg 26.2.21

### Breaking changes

- **RENAMED**: `$prepare_for_analysis()` parameters
  `estimate_ipcw_separately_by_exposure` →
  `estimate_ipcw_pp_separately_by_exposure` and `estimate_ipcw_with_gam`
  → `estimate_ipcw_pp_with_gam` for consistency with the IPCW-PP method
  they control.

- **PRIVATE**: `$enroll()`, `$prepare_outcome()`, `$ipcw_pp()`, and
  `$combine_weights()` are now private methods on `TTEEnrollment`.

  - Enrollment: use `tte_enrollment(data, design, ratio = 2, seed = 4)`
    instead of
    `tte_enrollment(data, design)$enroll(ratio = 2, seed = 4)`.
  - Outcome prep + IPCW: use `$prepare_for_analysis()` (unchanged).
  - Weight combination: handled automatically by `$ipcw_pp()`
    (unchanged).
  - Tests can access private methods via
    `enrollment$.__enclos_env__$private$method_name()`.

## swereg 26.2.20

### Breaking changes

- **RENAMED**: `$prepare_analysis()` → `$prepare_for_analysis()` on
  `TTEEnrollment`. The new name better communicates that this method
  *prepares* the enrollment *for* analysis (it is not the analysis
  itself).

### Bug fixes

- **FIXED**: 3 remaining broken test calls (`tte_extract()`,
  `tte_summary()`, `tte_weights()`) migrated to R6 method syntax
  (`$extract()`, [`print()`](https://rdrr.io/r/base/print.html),
  `$combine_weights()`). Column assertion updated: `"weight_pp"` →
  `"analysis_weight_pp"`.

- **FIXED**: `$impute_confounders()` now appends `"impute"` to
  `steps_completed`, consistent with all other mutating methods.

- **FIXED**: `$ipcw_pp()` IPW column guard moved from after IPCW
  computation to before it (fail-fast).

### Documentation

- **FIXED**: Vignette truncation bounds corrected from “0.5th and 99.5th
  percentiles” to “1st and 99th percentiles” (matching code defaults
  `lower = 0.01, upper = 0.99`).

- **FIXED**: `TTEDesign` roxygen references to removed `tte_match()` /
  `tte_expand()` replaced with `$enroll()`.

- **FIXED**: `$weight_summary()` moved from “Mutating” to “Non-mutating”
  section in `TTEEnrollment` roxygen (it only prints, never modifies
  data).

## swereg 26.2.13

### New features

- **NEW**: `$prepare_for_analysis()` method on `TTEEnrollment` merges
  `$prepare_outcome()` + `$ipcw_pp()` into one step. Parameters:
  `outcome`, `follow_up`, `separate_by_exposure`, `use_gam`,
  `censoring_var`.

- **NEW**: `$enrollment_stage` active binding on `TTEEnrollment`.
  Derives lifecycle stage from existing state: `"pre_enrollment"` →
  `"enrolled"` → `"analysis_ready"`. Zero maintenance — reads
  `data_level` and `steps_completed`.

### Bug fixes

- **FIXED**: 24 broken test cases calling removed standalone functions
  (`tte_enroll()`, `tte_collapse()`, `tte_ipw()`, `tte_truncate()`,
  `tte_prepare_outcome()`) migrated to R6 method syntax. Error message
  patterns updated to match method names (e.g., `enroll()` not
  `tte_enroll()`).

## swereg 26.2.12

### Breaking changes

- **RENAMED**: `TTETrial` class → `TTEEnrollment`, `tte_trial()` →
  `tte_enrollment()`, `summary.TTETrial` → `summary.TTEEnrollment`. The
  class represents an enrollment (matching + panel expansion), not an
  individual emulated target trial (ETT). Aligns naming with the ETT
  grid concept in `TTEPlan`.

## swereg 26.2.11

### Breaking changes

- **REMOVED**: 19 standalone TTE functions moved to R6 methods on
  `TTETrial` (15 methods) and `TTEPlan` (4 methods). Pipe chaining
  (`trial |> tte_ipw()`) replaced with `$`-chaining (`trial$ipw()`).

  **TTETrial methods**: `$enroll()`, `$collapse()`, `$ipw()`,
  `$ipcw_pp()`, `$combine_weights()`, `$truncate()`,
  `$prepare_outcome()`, `$impute_confounders()`, `$weight_summary()`,
  `$extract()`, `$summary()`, `$table1()`, `$rates()`, `$irr()`,
  `$km()`.

  **TTEPlan methods**: `$add_one_ett()`, `$save()`,
  `$enrollment_spec()`, `$generate_enrollments_and_ipw()`.

- **RENAMED**: `TTEPlan$task()` → `TTEPlan$enrollment_spec()`. The
  method returns enrollment metadata (design, enrollment_id, age_range,
  n_threads), not a generic task. The `process_fn` callback parameter
  convention changes from `function(task, file_path)` to
  `function(enrollment_spec, file_path)`.

  Removed exports: `tte_enroll`, `tte_collapse`, `tte_ipw`,
  `tte_ipcw_pp`, `tte_weights`, `tte_truncate`, `tte_prepare_outcome`,
  `tte_extract`, `tte_summary`, `tte_weight_summary`, `tte_table1`,
  `tte_rates`, `tte_irr`, `tte_km`, `tte_plan_add_one_ett`,
  `tte_plan_save`, `tte_plan_task`, `tte_generate_enrollments_and_ipw`.

  Kept standalone: `tte_rbind()`, `tte_rates_combine()`,
  `tte_irr_combine()`, `tte_impute_confounders()` (thin wrapper for
  callback default).

- **CHANGED**: TTE classes (`TTEDesign`, `TTETrial`, `TTEPlan`) migrated
  from S7 to R6. Property access changes from `@` to `$` (e.g.,
  `trial@data` → `trial$data`, `design@id_var` → `design$id_var`). R6
  reference semantics eliminate copy-on-write overhead from
  `trial$data[, := ...]`, reducing peak RAM from ~3X to ~2X during the
  weight-calculation chain (Loop 2).

- **FIXED**: Three S7 `@` accessor bugs that silently produced no-ops:

  - `$ipcw_pp()`: dropping intermediate IPCW columns (`p_censor`, etc.)
  - `$collapse()`: creating `person_weeks` column
  - `$impute_confounders()`: deleting old confounder columns before
    merge All fixed automatically by R6 (in-place modification works).

- **CHANGED**: `$ipcw_pp()` now inlines weight combination and
  truncation (was calling `tte_combine_weights()` and
  `tte_truncate_weights()` via function parameters that created extra
  refcount). Keeps data.table refcount=1 throughout.

### File reorganization

- Split `tte_classes.R` and `tte_methods.R` into per-class files with
  methods inline: `tte_design.R`, `tte_trial.R`, `tte_plan.R`.
  `tte_generate.R` reduced to thin `tte_impute_confounders()` wrapper +
  `.tte_callr_pool()` helper.

- Added `S3method(summary, TTETrial)` → delegates to `$summary()`.

### Dependencies

- **ADDED**: R6 package to Imports (S7 retained for skeleton classes).

## swereg 26.2.10

### Bug fixes

- **FIXED**: `tte_ipw()`, `tte_ipcw_pp()`: in-place joins via S7 `@`
  accessor now use extract/modify/reassign pattern
  (`dt <- trial@data; dt[...]; trial@data <- dt`). The previous
  `trial@data[i, := ...]` silently modified a copy, leaving the S7
  object’s data unchanged.

### Performance

- **IMPROVED**: `tte_ipw()`, `tte_ipcw_pp()`, `tte_calculate_ipcw()`:
  replace [`merge()`](https://rdrr.io/r/base/merge.html) with in-place
  keyed joins (`data[i, := ...]`), reducing peak RAM from ~3x to ~2x
  panel size during the weight-calculation chain.

### Breaking changes

- **CHANGED**: `tte_ipcw_pp()` now also combines weights
  (`ipw * ipcw_pp` → `analysis_weight_pp`), truncates
  `analysis_weight_pp`, and drops intermediate IPCW columns (`p_censor`,
  `p_uncensored`, `cum_p_uncensored`, `marginal_p`, `cum_marginal`).
  Callers no longer need `tte_weights()` + `tte_truncate()` after
  `tte_ipcw_pp()`.

- **RENAMED**: `tte_generate_enrollments()` →
  `tte_generate_enrollments_and_ipw()`. Now computes IPW + truncation
  once on the full combined enrollment (after imputation), so the
  per-ETT Loop 2 no longer needs to call `tte_ipw()`. New `stabilize`
  parameter (default TRUE) controls IPW stabilization.

### New features

- **NEW**: `tte_plan_load()` reads a `.qs2` plan file and reconstructs
  the `TTEPlan` S7 object. Companion to `tte_plan_save()`.

- **CHANGED**: `tte_plan_save()` now persists `project_prefix` and
  `skeleton_files` alongside `ett` and `global_max_isoyearweek`, so
  `tte_plan_load()` can fully reconstruct the object.

- **NEW**: `skeleton_process()` gains `n_workers` parameter for parallel
  batch processing. When \> 1, uses
  [`callr::r()`](https://callr.r-lib.org/reference/r.html) +
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) to
  process batches concurrently while avoiding `fork()` + data.table
  OpenMP segfaults.

## swereg 26.2.9

### Improvements

- **CHANGED**: Migrate serialization from `qs` (archived) to `qs2`.
  `.qs_save`/`.qs_read` wrappers now call
  [`qs2::qs_save`](https://rdrr.io/pkg/qs2/man/qs_save.html)/[`qs2::qs_read`](https://rdrr.io/pkg/qs2/man/qs_read.html)
  (standard format, preserves S7 objects). All file extensions changed
  from `.qs` to `.qs2`. The `preset` parameter is no longer used.

- **IMPROVED**: `tte_rates()` now sets `swereg_type` and `exposure_var`
  attributes on its output; `tte_irr()` sets `swereg_type`.

- **RENAMED**: `tte_rates_table()` → `tte_rates_combine()`,
  `tte_irr_table()` → `tte_irr_combine()`. New API accepts
  `(results, slot, descriptions)` — extracts the rates/irr slot
  internally, removing the need for `lapply(results, [[, "table2")` at
  call sites. Exposure column is now read from the `exposure_var`
  attribute instead of guessing via
  [`setdiff()`](https://rdrr.io/r/base/sets.html).

### Breaking changes

- **CHANGED**: `tte_plan_add_one_ett()` now requires explicit
  `enrollment_id` parameter. Auto-assignment based on follow_up +
  age_group removed. Validation that design params match within an
  enrollment_id is preserved.

- **IMPROVED**: `print(plan)` now shows both enrollment grid and full
  ETT grid.

- **CHANGED**: `tte_plan_add_one_ett()` bundles `age_group`, `age_min`,
  `age_max`, `person_id_var` into an `argset` named list parameter.
  `time_exposure_var` and `eligible_var` no longer have defaults (must
  be explicit). `exposure_var` removed from interface (hardcoded to
  `"baseline_exposed"`).

- **RENAMED**: `file_id` column in the `ett` data.table →
  `enrollment_id`. This makes explicit that ETTs sharing the same
  follow_up + age_group are processed together as one “enrollment”
  (shared eligibility, matching, collapse, imputation).

- **RENAMED**: `tte_generate_trials()` → `tte_generate_enrollments()`.
  The function generates enrollments (one per follow_up × age_group),
  not individual trials.

- **RENAMED**: `tte_plan_task()` return list key `file_id` →
  `enrollment_id`.

- **UPDATED**: `print(plan)` now shows “Enrollments: N x M skeleton
  files” instead of “Tasks: N file_id(s) x M skeleton files”.

## swereg 26.2.8

### Breaking changes

- **CHANGED**: `tte_plan()` is now infrastructure-only — takes only
  `project_prefix`, `skeleton_files`, `global_max_isoyearweek`. Use
  `tte_plan_add_one_ett()` to add ETTs with per-ETT design parameters.

- **REMOVED**: TTEPlan plan-level properties `confounder_vars`,
  `person_id_var`, `exposure_var`, `time_exposure_var`, `eligible_var`.
  These are now per-ETT columns in the `ett` data.table.

- **REMOVED**: Internal `.tte_grid()` function. The ETT grid is now
  built incrementally via `tte_plan_add_one_ett()`.

- **ADDED**: `TTEPlan@project_prefix` property (needed for file naming
  in `tte_plan_add_one_ett()`).

### New features

- **NEW**: `tte_plan_add_one_ett()` — builder function that adds one ETT
  row to a plan. Stores design params (confounder_vars, person_id_var,
  exposure_var, time_exposure_var, eligible_var) per-ETT, allowing
  different ETTs to use different confounders. Validates that design
  params match within an enrollment_id (same follow_up + age_group).

- **RENAMED**: `TTEPlan@files` property → `TTEPlan@skeleton_files` for
  clarity.

## swereg 26.2.7

### Breaking changes

- **REFACTORED**: `tte_generate_enrollments()` (formerly
  `tte_generate_trials()`) now takes a `TTEPlan` object instead of
  separate parameters (`ett`, `files`, `confounder_vars`,
  `global_max_isoyearweek`). The `process_fn` callback signature changes
  from `function(file_path, design, file_id, age_range, n_threads)` to
  `function(task, file_path)` where `task` is a list with `design`,
  `enrollment_id`, `age_range`, and `n_threads`.

### New features

- **NEW**: `TTEPlan` S7 class bundles ETT grid, skeleton file paths,
  confounder definitions, and design column names into a single object
  for trial generation.
  - `tte_plan()`: Constructor function
  - `tte_plan_task(plan, i)`: Extract the i-th enrollment task as a list
    with `design`, `enrollment_id`, `age_range`, `n_threads`
  - `plan[[i]]`: Shorthand for `tte_plan_task(plan, i)`
  - `length(plan)`: Number of unique enrollment_id groups
  - Supports interactive testing:
    `task <- plan[[1]]; process_fn(task, plan@skeleton_files[1])`

## swereg 26.2.6

### Documentation

- **FIXED**: Add missing topics to pkgdown reference index (TTEDesign,
  TTETrial, x2026_mht_add_lmed)

## swereg 26.2.5

### Bug fixes

- **FIXED**: Set `eval = FALSE` in skeleton3-analyze vignette to prevent
  build errors from optional `qs` package dependency

## swereg 26.2.4

### Bug fixes

- **FIXED**: Remove `qs` from Suggests to fix GitHub Actions CI (package
  not available on CRAN)

## swereg 26.2.3

### Breaking changes

- **REPLACED**: `tte_match()` and `tte_expand()` merged into single
  `tte_enroll()` function:
  - Old workflow:
    `tte_trial(data, design) |> tte_match(ratio = 2, seed = 4) |> tte_expand(extra_cols = "isoyearweek")`
  - New workflow:
    `tte_trial(data, design) |> tte_enroll(ratio = 2, seed = 4, extra_cols = "isoyearweek")`
  - The two operations were tightly coupled and always used together
  - `tte_enroll()` combines sampling (matching) and panel expansion in
    one step
  - Records “enroll” in `steps_completed` (previously recorded “match”
    then “expand”)

### New features

- **NEW**: Trial eligibility helper functions for composable eligibility
  criteria:
  - `tte_eligible_isoyears()`: Check eligibility based on calendar years
  - `tte_eligible_age_range()`: Check eligibility based on age range
  - `tte_eligible_no_events_in_window_excluding_wk0()`: Check for no
    events in prior window (correctly excludes baseline week)
  - `tte_eligible_no_observation_in_window_excluding_wk0()`: Check for
    no specific value in prior window (for categorical variables)
  - `tte_eligible_combine()`: Combine multiple eligibility columns using
    AND logic
  - All functions modify data.tables by reference and return invisibly
    for method chaining

### Documentation

- **IMPROVED**: Clarified that eligibility checks should EXCLUDE the
  baseline week. Using `cumsum(x) == 0` is incorrect because it includes
  the current week. The new eligibility functions use
  [`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md)
  which correctly excludes the current row.

## swereg 26.1.31

### New features

- **NEW**: S7 object-oriented API for target trial emulation workflows:
  - `TTEDesign` class: Define column name mappings once and reuse across
    all TTE functions
  - `TTETrial` class: Fluent method chaining with workflow state
    tracking
  - `tte_design()` / `tte_trial()`: Constructor functions for the S7
    classes
  - `tte_match()`, `tte_expand()`, `tte_collapse()`, `tte_ipw()`: S7
    methods for data preparation
  - `tte_prepare_outcome()`, `tte_ipcw()`: Outcome-specific per-protocol
    analysis
  - `tte_weights()`, `tte_truncate()`: Weight combination and truncation
  - `tte_rbind()`: Combine batched trial objects
  - `tte_extract()`, `tte_summary()`: Access data and diagnostics
  - `tte_table1()`, `tte_rates()`, `tte_irr()`, `tte_km()`: Analysis and
    visualization

### Breaking changes

- **REMOVED**: Deprecated S7 methods replaced by
  `tte_prepare_outcome()`:
  - `tte_tte()`: Use `tte_prepare_outcome()` which computes
    `weeks_to_event` internally
  - `tte_set_outcome()`: Use `tte_prepare_outcome(outcome = "...")`
    instead
  - `tte_censoring()`: Use `tte_prepare_outcome()` which handles
    censoring internally

### Dependencies

- **ADDED**: S7 package to Imports for object-oriented class system

## swereg 26.1.30

### New features

- **NEW**: Target trial emulation weight functions for causal inference
  in observational studies:
  - `tte_calculate_ipw()`: Calculate stabilized inverse probability of
    treatment weights (IPW) for baseline confounding adjustment using
    propensity scores
  - `tte_calculate_ipcw()`: Calculate time-varying inverse probability
    of censoring weights (IPCW) for per-protocol analysis using GAM or
    GLM
  - `tte_identify_censoring()`: Identify protocol deviation and loss to
    follow-up for per-protocol analysis
  - `tte_combine_weights()`: Combine IPW and IPCW weights for
    per-protocol effect estimation
  - `tte_truncate_weights()`: Truncate extreme weights at specified
    quantiles to reduce variance
- **NEW**: Target trial emulation data preparation functions:
  - `tte_match_ratio()`: Sample comparison group at specified ratio
    (e.g., 2:1 unexposed to exposed)
  - `tte_collapse_periods()`: Collapse fine-grained time intervals
    (e.g., weekly) to coarser periods (e.g., 4-week)
  - `tte_time_to_event()`: Calculate time to first event for each
    trial/person

### Dependencies

- **ADDED**: mgcv package to Imports for flexible GAM-based censoring
  models in `tte_calculate_ipcw()`

## swereg 25.12.24

### API changes

- **SIMPLIFIED**: Removed `validate_source_column()` requirement from
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
  [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
  [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
  [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
  and
  [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md):
  - The `source` column is no longer required in diagnosis data
  - To track diagnoses by source (inpatient/outpatient/cancer), filter
    the dataset externally before calling
    [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  - See
    [`?add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
    for the recommended pattern

### New features

- **NEW**:
  [`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md)
  function for survival analysis:
  - Checks if any TRUE values exist in a preceding time window (excludes
    current row)
  - Useful for determining if an event occurred in a prior time period
  - Default window of 104 weeks (~2 years) with customizable size
  - Complements
    [`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)
    for comprehensive time-to-event analysis
- **ENHANCED**:
  [`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)
  function improvements:
  - Renamed parameter from `window` to `window_including_wk0` for
    clarity
  - Default window is now 104 (inclusive of current week)
  - Added `@family survival_analysis` tag and cross-reference to
    [`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md)

### Bug fixes

- **FIXED**: Added slider package to Imports in DESCRIPTION to fix R CMD
  check warning about undeclared import

### Data

- **BREAKING**: Replaced separate `fake_inpatient_diagnoses` and
  `fake_outpatient_diagnoses` with unified `fake_diagnoses` dataset:
  - New `SOURCE` column identifies data origin: “inpatient”,
    “outpatient”, or “cancer”
  - ~2000 inpatient records, ~2000 outpatient records, ~1000 cancer
    records
  - Cancer records always have populated `ICDO3` codes
  - Enables testing of source-based filtering and validation
- **ENHANCED**: Added ICD-O-3 and SNOMED-CT columns to fake diagnosis
  data:
  - `ICDO3`: ICD-O-3 morphology codes (always populated for cancer
    source)
  - `SNOMED3`: SNOMED-CT version 3 codes
  - `SNOMEDO10`: SNOMED-CT version 10 codes

### Validation

- **ENHANCED**: SOURCE column validation is now optional - filter
  externally if needed (see API changes above)

### Documentation

- **IMPROVED**: Examples for
  [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
  [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
  and
  [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)
  are now runnable using package fake data (previously wrapped in
  `\dontrun{}`)

## swereg 25.12.6

### New features

- **NEW**:
  [`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)
  function for survival analysis:
  - Calculates the number of steps (e.g., weeks) until the first TRUE
    value in a forward-looking window
  - Useful for time-to-event calculations in longitudinal registry data
  - Default window of 103 weeks (~2 years) with customizable size
  - Returns NA if no event occurs within the window

### Bug fixes

- **CRITICAL**: Fixed
  [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md)
  and
  [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)
  calling non-existent internal functions
  - Both functions now correctly call
    `add_diagnoses_or_operations_or_cods_or_icdo3_or_snomed()`
  - These functions would have caused runtime errors before this fix
- **FIXED**: Removed erroneous `icdo10` column references from
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md):
  - ICD-O only has editions 1, 2, and 3 (not 10)
  - ICD-O-3 codes should be handled via the dedicated
    [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md)
    function
- **FIXED**: Added `icd7*` and `icd9*` columns to diagnosis search in
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md):
  - Historical ICD-7 and ICD-9 columns are now properly searched when
    `diag_type = "both"`
  - Validation and helper function now consistent
- **FIXED**: Corrected error messages in
  [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
  [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
  and
  [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md):
  - Messages now correctly reference the appropriate data types instead
    of “operation data”

### Documentation

- **ENHANCED**:
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  documentation now clearly lists which diagnosis columns are searched:
  - When `diag_type = "both"`: `hdia`, `dia*`, `ekod*`, `icd7*`, `icd9*`
  - When `diag_type = "main"`: `hdia` only

## swereg 25.8.19

### CRAN Submission Preparation

- **CRAN READY**: Package prepared for CRAN submission with
  comprehensive compliance improvements:
  - Fixed DESCRIPTION file author field duplication issue
  - Updated .Rbuildignore to exclude all development files (docs/,
    .git/, .Rhistory, etc.)
  - Removed non-portable files ([@eaDir](https://github.com/eaDir)
    directories, .DS_Store files)
  - Added missing global variable declarations to prevent R CMD check
    warnings
  - Verified URL consistency between DESCRIPTION and package startup
    messages
- **OPTIMIZED**: Vignette structure significantly improved for CRAN
  submission:
  - Reduced total vignette content by 31% (626 lines removed)
  - Condensed cookbook-survival-analysis.Rmd (removed verbose
    descriptive statistics and redundant sections)
  - Simplified skeleton2-clean.Rmd (removed duplicated skeleton1_create
    workflow)
  - Streamlined skeleton3-analyze.Rmd (removed redundant data loading
    and best practices sections)
  - Fixed all vignette build errors by ensuring consistent data variable
    availability
  - All vignettes now compile successfully and use package synthetic
    data consistently
- **VALIDATED**: All examples are runnable using package fake data - no
  \dontrun sections without justification

### Code Quality Improvements

- **CONSISTENCY**: Fixed date_columns parameter usage throughout
  package:
  - Updated all vignettes to use lowercase date_columns parameters
    (e.g., “indatum” instead of “INDATUM”)
  - Added warning to make_lowercase_names() function when uppercase
    date_columns are provided
  - Enhanced documentation to clarify that date_columns should use
    lowercase names
  - Improved user experience with clear guidance and automatic handling
    of uppercase inputs
- **ELEGANCE**: Enhanced vignette code patterns for better readability:
  - Replaced verbose data() loading patterns with elegant pipe syntax
  - Updated all data loading to use swereg::fake\_\* \|\> copy() \|\>
    make_lowercase_names() pattern
  - Eliminated clumsy multi-step data preparation code throughout
    vignettes
  - Improved code flow and professional appearance of package examples
- **VERIFIED**: Package builds successfully with R CMD build and passes
  CRAN compliance checks
- **CONFIRMED**: inst/ directory contains only files referenced by
  package functions

## swereg 25.7.30

### New Features

- **NEW**:
  [`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
  helper function for rowdep → rowind transformations:
  - Simplifies the common pattern of creating row-independent variables
    from first occurrence of conditions
  - Automatically handles temp variable creation and cleanup
  - Uses
    [`first_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/first_non_na.md)
    for robust aggregation across all variable types
  - Includes comprehensive input validation and clear error messages
- **NEW**: “Understanding rowdep and rowind Variables” vignette:
  - Explains the fundamental distinction between row-dependent and
    row-independent variables
  - Demonstrates common transformation patterns with practical examples
  - Shows integration with the swereg workflow (skeleton1_create →
    skeleton2_clean → skeleton3_analyze)
  - Includes best practices for longitudinal registry data analysis

### Documentation

- **ENHANCED**: Helper functions now include `@family data_integration`
  tags for better organization
- **IMPROVED**: Function examples use existing fake datasets for
  consistency

## swereg 25.7.16

### New Swedish Date Parsing and Enhanced Data Cleaning

- **NEW**:
  [`parse_swedish_date()`](https://papadopoulos-lab.github.io/swereg/reference/parse_swedish_date.md)
  function for handling Swedish registry dates with varying precision:
  - Handles 4-character (YYYY), 6-character (YYYYMM), and 8-character
    (YYYYMMDD) formats
  - Automatically replaces “0000” with “0701” and “00” with “15” for
    missing date components
  - Supports custom defaults for missing date parts
  - Includes comprehensive error handling and vectorized processing
- **ENHANCED**:
  [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
  now supports automatic date cleaning:
  - New `date_column` parameter to specify which column contains dates
  - Automatically creates cleaned ‘date’ column using
    [`parse_swedish_date()`](https://papadopoulos-lab.github.io/swereg/reference/parse_swedish_date.md)
  - Works with both default and data.table methods
  - Maintains backward compatibility with existing code
- **ENHANCED**: All `add_*` functions now require cleaned date columns:
  - [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
    [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
    [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
    [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md)
    expect ‘date’ column
  - Clear error messages guide users to use
    `make_lowercase_names(data, date_column = "...")`
  - Improved validation ensures data preprocessing consistency
- **ENHANCED**:
  [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
  now includes `personyears` column:
  - Annual rows (is_isoyear==TRUE) have personyears = 1
  - Weekly rows (is_isoyear==FALSE) have personyears = 1/52.25
  - Facilitates person-time calculations for survival analysis
- **IMPROVED**: Survival analysis cookbook vignette updated:
  - Uses weekly data instead of yearly data for more precise analyses
  - Age calculation based on isoyearweeksun instead of isoyear
  - Includes person-time in descriptive statistics
  - Demonstrates proper use of new date cleaning workflow

### Enhanced error handling and validation

- **ENHANCED**: Comprehensive input validation for all `add_*`
  functions:
  - [`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md):
    Validates skeleton structure, ID column exists, checks for ID
    matches
  - [`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md):
    Validates isoyear parameter, checks skeleton year coverage
  - [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md):
    Validates diagnosis patterns, checks for diagnosis code columns
  - [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md):
    Validates operation patterns, checks for operation code columns  
  - [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md):
    Validates prescription data structure, checks source columns
  - [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md):
    Validates death data structure, checks cause of death columns
- **IMPROVED**: User-friendly error messages with specific guidance:
  - Clear indication when
    [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
    is forgotten
  - Helpful suggestions for column naming issues
  - Informative ID mismatch diagnostics with sample values
- **NEW**: Internal validation helper functions for consistent error
  handling
- **ADDED**: Input validation for pattern lists, data structures, and
  parameter ranges

### New cookbook documentation

- **NEW**: Comprehensive survival analysis cookbook
  (`cookbook-survival-analysis.Rmd`):
  - Complete workflow from raw data to Cox proportional hazards model
  - Time-varying covariates (annual income) with heart attack outcome
  - Handles common challenges: missing data, multiple events, competing
    risks
  - Performance tips for large datasets
  - Practical solutions for real-world registry analysis
- **ENHANCED**: Updated `_pkgdown.yml` with new “Cookbooks” section
- **ADDED**: `survival` package to Suggests dependencies

### Bug fixes

- **FIXED**: Improved ID matching warnings and error messages across all
  functions
- **CORRECTED**: Better handling of missing data in time-varying
  covariate analysis
- **ENHANCED**: More robust parameter validation prevents common user
  errors

## swereg 25.7.16

### Major documentation restructuring

- **RESTRUCTURED**: Complete vignette reorganization for clear learning
  progression:
  - NEW “Skeleton concept” vignette: Conceptual foundation explaining
    the skeleton approach without technical implementation
  - “Building the data skeleton (skeleton1_create)”: Pure data
    integration focus - raw data to time-structured skeleton
  - “Cleaning and deriving variables (skeleton2_clean)”: Pure data
    cleaning and variable derivation focus
  - “Production analysis workflows (skeleton3_analyze)”:
    Memory-efficient processing and final analysis datasets
- **IMPROVED**: Clear separation of concerns with focused,
  single-purpose tutorials
- **ENHANCED**: Systematic learning progression from concept to
  implementation to production
- **UPDATED**: \_pkgdown.yml structure with logical vignette grouping
- **PRESERVED**: All existing technical content while improving
  organization

### Content improvements

- **NEW**: Comprehensive conceptual introduction based on presentation
  content
- **IMPROVED**: Each vignette builds systematically on the previous one
- **ENHANCED**: Better explanation of three types of data integration
  (one-time, annual, event-based)
- **CLARIFIED**: Production workflow patterns with memory-efficient
  batching strategies
- **STANDARDIZED**: Consistent academic tone and sentence case
  throughout

## swereg 25.7.15

### Documentation and presentation improvements

- **STANDARDIZED**: Changed all titles and headings to normal sentence
  case throughout:
  - Vignette titles: “Basic Workflow” → “Basic workflow”, “Complete
    Workflow” → “Complete workflow”, etc.
  - README.md section headings: “Core Functions” → “Core functions”,
    “Data Integration” → “Data integration”, etc.
  - NEWS.md section headings: “Vignette Restructuring” → “Vignette
    restructuring”, etc.
  - CLAUDE.md section headings: “Project Overview” → “Project overview”,
    “Development Commands” → “Development commands”, etc.
- **IMPROVED**: Consistent normal sentence case for better readability
  and less formal appearance
- **SIMPLIFIED**: Removed subtitle text after colons in vignette titles
  for cleaner presentation
- **ENHANCED**: Improved Core Concept section in basic workflow vignette
  with clear explanation of three data types:
  - One-time data (demographics): Added to all rows for each person
  - Annual data (income, family status): Added to all rows for specific
    year
  - Event-based data (diagnoses, prescriptions, deaths): Added to rows
    where events occurred
- **CLARIFIED**: Step 1 documentation now properly explains all skeleton
  columns including `isoyearweeksun`
- **VERIFIED**: All vignettes compile successfully with improved content

### Major documentation and vignette reorganization

- **RESTRUCTURED**: Complete vignette reorganization with improved
  naming and content flow:
  - `swereg.Rmd` → `basic-workflow.Rmd`: Focused introduction to
    skeleton1_create
  - `advanced-workflow.Rmd` → `complete-workflow.Rmd`: Two-stage
    workflow (skeleton1_create + skeleton2_clean)
  - `memory-efficient-batching.Rmd`: Maintained as comprehensive
    three-stage workflow guide
- **IMPROVED**: Eliminated content redundancy between vignettes for
  clearer learning progression
- **ENHANCED**: Updated \_pkgdown.yml configuration to reflect new
  vignette structure

### Function documentation improvements

- **ENHANCED**: Comprehensive documentation improvements for all
  exported functions:
  - Added [@family](https://github.com/family) tags for logical grouping
    (data_integration, skeleton_creation, data_preprocessing)
  - Added [@seealso](https://github.com/seealso) sections with
    cross-references to related functions and vignettes
  - Replaced placeholder examples with runnable code using synthetic
    data
  - Improved parameter documentation with detailed descriptions and
    expected formats
  - Enhanced return value documentation with explicit side effects
    description
- **STANDARDIZED**: Consistent academic tone throughout all
  documentation

### Professional presentation updates

- **IMPROVED**: Removed informal elements and adopted academic tone
  across all documentation
- **UPDATED**: Changed terminology from “fake data” to “synthetic data”
  throughout
- **ENHANCED**: More professional language in README.md and vignettes
- **STANDARDIZED**: Consistent formal tone appropriate for scientific
  software

### Technical improvements

- **VERIFIED**: All vignettes compile successfully with updated content
- **TESTED**: Package passes R CMD check with all documentation
  improvements
- **UPDATED**: CLAUDE.md reflects new vignette structure and
  documentation standards

## swereg 25.7.1

### Vignette restructuring

- **RESTRUCTURED**: Reorganized vignettes for clearer learning
  progression:
  - `swereg.Rmd`: Clean skeleton1_create tutorial using full datasets
    (removed subset filtering)
  - `advanced-workflow.Rmd`: Focused skeleton1→skeleton2 workflow
    (removed batching and skeleton3 content)
  - `memory-efficient-batching.Rmd`: NEW comprehensive batching vignette
    with complete skeleton1→skeleton2→skeleton3 workflow for large-scale
    studies
- **IMPROVED**: GitHub Actions workflow optimization with dependency
  caching and binary packages for faster CI/CD

### Batching vignette fixes

- **FIXED**: Updated memory-efficient-batching vignette with
  production-ready improvements:
  - Replace [`split()`](https://rdrr.io/r/base/split.html) with
    [`csutil::easy_split`](https://rdrr.io/pkg/csutil/man/easy_split.html)
    for better batch handling
  - Replace `saveRDS/readRDS` with `qs::qsave/qread` for 2-10x faster
    file I/O
  - Fix skeleton3_analyze to properly aggregate weekly→yearly data using
    [`swereg::max_with_infinite_as_na`](https://papadopoulos-lab.github.io/swereg/reference/max_with_infinite_as_na.md)
  - Remove incorrect `is_isoyear == TRUE` filter in skeleton3_analyze
  - Fix analysis results to avoid NaN outputs in treatment rate
    calculations
  - Add explanations for weekly→yearly data aggregation and qs package
    performance benefits

### New features

- **NEW**: Added `isoyearweeksun` variable to
  [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
  function - provides Date representing the Sunday (last day) of each
  ISO week/year for easier date calculations
- **NEW**: Updated package logo
- **IMPROVED**: Updated all vignettes to not assume swereg is loaded -
  all functions use `swereg::` prefix and
  [`data()`](https://rdrr.io/r/utils/data.html) calls use
  `package="swereg"` argument
- **IMPROVED**: Updated function documentation to clarify that pattern
  matching functions (`add_diagnoses`, `add_cods`, `add_rx`)
  automatically add “^” prefix - users should NOT include “^” in their
  patterns
- **NEW**: Added comprehensive fake Swedish registry datasets for
  development and vignettes:
  - `fake_person_ids`: 1000 synthetic personal identifiers
  - `fake_demographics`: Demographics data matching SCB format
  - `fake_annual_family`: Annual family status data
  - `fake_inpatient_diagnoses` and `fake_outpatient_diagnoses`: NPR
    diagnosis data with ICD-10 codes
  - `fake_prescriptions`: LMED prescription data with ATC codes and
    hormone therapy focus
  - `fake_cod`: Cause of death data
- **NEW**: Added two comprehensive vignettes:
  - `swereg.Rmd`: Basic skeleton1_create workflow tutorial
  - `advanced-workflow.Rmd`: Complete 3-phase workflow (skeleton1 →
    skeleton2 → skeleton3)
- **NEW**: Replaced magrittr pipe (%\>%) with base pipe (\|\>)
  throughout codebase
- **NEW**: Added memory-efficient batched processing examples for large
  registry studies

### Bug fixes

- **CRITICAL**: Fixed incorrect variable names in `fake_cod` dataset -
  changed from non-Swedish
  `underlying_cod/contributory_cod1/contributory_cod2` to correct
  Swedish registry names `ulorsak/morsak1/morsak2`
- **VERIFIED**: Confirmed all fake datasets use correct Swedish registry
  variable name conventions
- **VERIFIED**: All ICD-10 and ATC codes in fake datasets are properly
  formatted and realistic

### Documentation improvements

- **BREAKING**: Fixed incorrect function descriptions that were copied
  from another package
- **NEW**: Added comprehensive roxygen2 documentation for all exported
  functions:
  - [`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md):
    Documents merging one-time/baseline data to skeleton
  - [`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md):
    Documents merging annual data for specific ISO years
  - [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md):
    Documents cause of death analysis with ICD-10 codes
  - [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md):
    Documents diagnosis analysis with main/secondary diagnoses
  - [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md):
    Documents surgical operation analysis including gender-affirming
    procedures
  - [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md):
    Documents prescription drug analysis with ATC/product codes
  - [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md):
    Documents longitudinal skeleton creation with detailed return
    structure
  - [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md):
    Documents generic function with S3 methods
  - [`x2023_mht_add_lmed()`](https://papadopoulos-lab.github.io/swereg/reference/x2023_mht_add_lmed.md):
    Documents specialized MHT study function
- **NEW**: Added documentation for all helper functions:
  - [`min_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/min_with_infinite_as_na.md),
    [`max_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/max_with_infinite_as_na.md)
  - [`as_logical_min_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/as_logical_min_with_infinite_as_na.md),
    [`as_logical_max_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/as_logical_max_with_infinite_as_na.md)
  - [`first_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/first_non_na.md),
    [`last_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/last_non_na.md)
- **NEW**: Added `@param` descriptions for all function parameters
- **NEW**: Added `@return` descriptions explaining function outputs
- **NEW**: Added `@examples` with practical usage demonstrations
- **NEW**: Added `@details` and `@note` sections for complex functions
- **IMPROVED**: Used proper roxygen2 practices including `@rdname` for
  S3 methods and `@seealso` cross-references

### Package structure

- All exported functions now have complete, accurate documentation
  suitable for CRAN submission
- Documentation focuses on Swedish registry data analysis workflows
- Examples use `\dontrun{}` appropriately for functions requiring
  external data
