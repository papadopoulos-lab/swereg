# These are the three TTEPlan pipeline methods. Loop 1 builds the trial
# panels and the treatment weights, Loop 2 the censoring weights, and Loop 3
# the estimates.

#' @include r6_tteplan.R
#' @description Loop 1: Create trial panels from skeleton files and compute IPW.
#'
#' Uses a two-pass pipeline to fix a cross-batch comparator-ratio imbalance.
#' Requires `self$spec` to be set (e.g., via
#' [tteplan_from_spec_and_registrystudy()]).
#'
#' \enumerate{
#'   \item **Pass 1a (scout)**: Lightweight parallel pass that reads each
#'     skeleton file, applies exclusions and treatment, and returns eligible
#'     `(person_id, trial_id, intervention)` tuples. No confounders or enrollment.
#'   \item **Centralized comparator draw**: Combines all tuples from all batches,
#'     then per `trial_id` keeps all intervention and samples
#'     `ratio * n_intervention` comparator globally. Stores counts on
#'     `self$enrollment_counts` for TARGET Item 8 reporting.
#'   \item **Pass 1b (full enrollment)**: Parallel pass that re-reads each
#'     skeleton file with full processing (exclusions + confounders +
#'     treatment), then enrolls using the pre-drawn IDs (skipping the
#'     per-batch draw). Produces panel-expanded TTEEnrollment objects.
#' }
#'
#' @param output_dir Optional directory override for output files. If
#'   `NULL` (default), uses `self$dir_tteplan`.
#' @param impute_fn Imputation callback or NULL (default:
#'   [tteenrollment_impute_confounders]). swereg calls it with the panel and
#'   with the `.tte_entry__` snapshot names, not with the plain confounder
#'   names. It MUST impute only the columns it is given.
#' @param stabilize Logical, stabilize IPW (default: TRUE).
#' @param n_workers Integer, concurrent subprocesses. Default
#'   [default_n_workers]`("s1")` (1 unless `SWEREG_N_WORKERS_S1` is set).
#' @param swereg_dev_path Path to local swereg dev copy, or NULL.
TTEPlan$set(
  "public",
  "s1_generate_enrollments_and_ipw",
  function(
    output_dir = NULL,
    impute_fn = tteenrollment_impute_confounders,
    stabilize = TRUE,
    n_workers = default_n_workers("s1"),
    swereg_dev_path = NULL
  ) {
    # Validate FIRST, before any self$ mutation or filesystem work. A bad
    # count used to error only after self$output_dir had already been
    # overwritten, leaving the plan half-changed.
    n_workers <- .validate_n_workers(
      n_workers,
      "s1_generate_enrollments_and_ipw()"
    )
    if (is.null(output_dir)) {
      output_dir <- self$dir_tteplan
    }
    # All-subprocess s1 dispatcher. The main R process holds only paths,
    # status flags, and progressors -- never a data.table. Four sub-steps
    # (s1a..s1d) communicate via files in
    #   {study$data_meta_dir}/s1_work/{project_prefix}/
    # which is removed on success. See "s1 work directory + path
    # constructors" above for the file-naming contract.
    #
    # Sub-step    Mode                                 Target
    # --------    ----                                 ------
    # s1a         parallel x skeleton                  .s1a_worker_multi()
    # s1b         single x enrollment                  .s1b_worker()
    # s1c         parallel x (enrollment x skeleton)   .s1c_worker()
    # s1d         single x enrollment                  .s1d_worker()
    # All four sub-steps dispatch through .batch_run_and_write(), which
    # commits each item's declared output paths atomically -- all of them,
    # or none. s1b/s1c use style = "return" (the worker returns the objects,
    # batchit serializes them). s1a and s1d use style = "staged_writer" (the
    # worker writes each output itself via .batch_where_to_write_output()):
    #   * s1a because one item writes 2 x n_enrollments files streamed
    #     inside a loop, and holding them all to return at the end would put
    #     every (tuples, attrition) chunk in RAM at once; and
    #   * s1d because its two outputs are two STATES of one by-reference
    #     object and cannot be returned together -- see the s1d dispatch
    #     below.
    # In both staged_writer cases the parent declares every path and the
    # worker names outputs only, so a parent/worker drift is a loud child
    # failure rather than a file written where nothing will read it.
    if (is.null(self$ett) || nrow(self$ett) == 0) {
      stop("plan has no ETTs. Use $add_one_ett() to add ETTs first.")
    }
    if (is.null(self$spec)) {
      stop(
        "plan has no spec. ",
        "Create the plan with tteplan_from_spec_and_registrystudy()."
      )
    }
    # Declared-output paths must be ABSOLUTE -- batchit's atomic commit
    # rejects a relative `outputs` entry. Create the directory BEFORE
    # normalizing: normalizePath(mustWork = FALSE) returns an absolute path
    # for a path that exists, but returns a non-existent relative path
    # UNCHANGED, so normalizing too early fails silently. Same precedent as
    # `outpaths` in R/r6_registrystudy.R. `output_dir` itself is left alone:
    # it is persisted to self$output_dir just below and s3_analyze falls back
    # to that field, so normalizing it would change what a saved plan reports
    # across a save/load.
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    out_abs <- normalizePath(output_dir, mustWork = FALSE)
    if (!grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", out_abs)) {
      stop(
        "s1_generate_enrollments_and_ipw(): output_dir did not resolve to an ",
        "absolute path (declared outputs must be absolute): ",
        out_abs,
        call. = FALSE
      )
    }

    self$output_dir <- output_dir
    spec <- self$spec

    ett <- self$ett
    files <- self$skeleton_files
    skel_basenames <- basename(files)
    n_threads <- .threads_per_worker(n_workers)

    # Per-enrollment summary (one row per enrollment_id).
    ett_loop1 <- ett[,
      .(
        max_follow_up = max(follow_up),
        age_grp = age_group[1],
        file_raw = file_raw[1],
        file_imp = file_imp[1]
      ),
      by = enrollment_id
    ]
    n_enr <- nrow(ett_loop1)

    cat(sprintf(
      "Creating enrollment files: %d enrollment(s) x %d skeleton files\n",
      n_enr,
      length(files)
    ))

    # Pre-build enrollment_spec objects once (used by all sub-steps).
    all_es <- lapply(seq_len(n_enr), function(i) {
      es <- self$enrollment_spec(i)
      es$n_threads <- n_threads
      es
    })
    enrollment_ids <- ett_loop1$enrollment_id

    work_dir <- .s1_work_dir(self, ensure_exists = FALSE)
    # The work directory is transient dataflow between the four sub-steps,
    # cleared at the start of every run and removed on success. Nothing here
    # persists across runs (Phase 5': s1 has no resume).
    if (dir.exists(work_dir)) {
      unlink(work_dir, recursive = TRUE, force = TRUE)
      if (dir.exists(work_dir)) {
        stop(
          "Could not clear the s1 work directory: ",
          work_dir,
          "\nRemove it by hand and re-run.",
          call. = FALSE
        )
      }
    }
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
    cat(sprintf("Work directory: %s\n", work_dir))

    # Restore enrollment_counts from sidecar files on disk (idempotent).
    if (is.null(self$enrollment_counts)) {
      self$enrollment_counts <- list()
    }
    .restore_enrollment_counts(self, output_dir, enrollment_ids)

    # The four sub-steps below each create their progressor right before
    # they run so the handler's "active" bar matches the current phase.

    # ====================================================================
    # s1a -- per skeleton (parallel)
    # ====================================================================
    cat(sprintf(
      "\n[s1a] Eligibility + attrition + tuples + caches (per skeleton, parallel x %d):\n",
      n_workers
    ))
    cat(sprintf(
      "      reading %d canonical skeleton(s) ONCE each across %d enrollments\n",
      length(files),
      n_enr
    ))
    p_s1a <- progressr::progressor(steps = length(files))
    s1a_items <- lapply(seq_along(files), function(j) {
      list(
        file_path = files[j],
        enrollment_specs = all_es,
        spec = spec
      )
    })
    # Stable ids: the skeleton each scout reads, prefixed with the sub-step
    # so a failure among all four Loop 1 dispatches says which one died.
    names(s1a_items) <- paste0("s1a_", skel_basenames)
    # Every file this item will write, declared here and NOWHERE ELSE: 2 x
    # n_enrollments per skeleton. `work_dir` is absolute (.s1_work_dir()),
    # which batchit's atomic commit requires. The worker never sees
    # `work_dir` -- it asks for these names back through
    # .batch_where_to_write_output().
    s1a_outputs <- lapply(skel_basenames, function(bn) {
      .s1a_outputs_for_skeleton(work_dir, enrollment_ids, bn)
    })
    names(s1a_outputs) <- names(s1a_items)
    if (length(s1a_items) > 0L) {
      .batch_run_and_write(
        target = .batch_target("swereg", ".s1a_worker_multi"),
        items = s1a_items,
        outputs = s1a_outputs,
        style = "staged_writer",
        n_workers = n_workers,
        dev_path = swereg_dev_path,
        p = p_s1a,
        label = "s1a"
      )
    }
    rm(s1a_items, s1a_outputs)

    # ====================================================================
    # s1b -- per enrollment (single subworker each, run sequentially)
    # ====================================================================
    cat(sprintf(
      "\n[s1b] Draw comparators (per enrollment, single subworker x %d)\n",
      n_enr
    ))
    p_s1b <- progressr::progressor(steps = n_enr)
    for (i in seq_len(n_enr)) {
      eid <- enrollment_ids[i]
      counts_path <- .enrollment_counts_path(
        output_dir,
        self$project_prefix,
        eid
      )
      id <- sprintf("s1b_%s", eid)
      s1b_items <- list(list(
        enrollment_spec = all_es[[i]],
        spec = spec,
        work_dir = work_dir,
        skel_basenames = skel_basenames
      ))
      names(s1b_items) <- id
      # The two objects the worker's return value commits to. They live in
      # DIFFERENT directories -- enrolled_ids in work_dir (transient input
      # to s1c), counts in output_dir (the sidecar the master reads back
      # below) -- which batchit's atomic commit handles as one set. The
      # declared counts path is built from `out_abs`, not `output_dir`,
      # because batchit rejects a relative declared output; it names the
      # same file the read-back below opens via `counts_path`.
      s1b_outputs <- list(c(
        enrolled_ids = .s1b_enrolled_ids_path(work_dir, eid),
        counts = .enrollment_counts_path(out_abs, self$project_prefix, eid)
      ))
      names(s1b_outputs) <- id
      .batch_run_and_write(
        target = .batch_target("swereg", ".s1b_worker"),
        items = s1b_items,
        outputs = s1b_outputs,
        style = "return",
        n_workers = 1L,
        dev_path = swereg_dev_path,
        p = p_s1b,
        label = "s1b"
      )
      # Surface the comparator-draw and attrition counts to the plan object.
      if (file.exists(counts_path)) {
        self$enrollment_counts[[eid]] <- qs2_read(counts_path)
      }
    }

    # ====================================================================
    # s1c -- per (enrollment, skeleton) (parallel)
    # ====================================================================
    cat(sprintf(
      "\n[s1c] Build panels (per enrollment x per skeleton, parallel x %d)\n",
      n_workers
    ))
    s1c_steps <- n_enr * length(files)
    s1c_items <- list()
    s1c_outputs <- list()
    for (i in seq_len(n_enr)) {
      eid <- enrollment_ids[i]
      es <- all_es[[i]]
      for (j in seq_along(files)) {
        # Named at construction: the id ("s1c_<enrollment>__<skeleton>") is
        # what a failure among 39k panel builds reports, so it must say
        # exactly which (enrollment, skeleton) pair died, and which stage.
        id <- sprintf("s1c_%s__%s", eid, skel_basenames[j])
        s1c_items[[id]] <- list(
          enrollment_spec = es,
          file_path = files[j],
          spec = spec,
          work_dir = work_dir
        )
        # The panel chunk the worker's return value commits to. `work_dir` is
        # absolute (.s1_work_dir()), which batchit's atomic commit requires.
        s1c_outputs[[id]] <- c(
          panel = .s1c_panel_path(work_dir, eid, skel_basenames[j])
        )
      }
    }
    p_s1c <- progressr::progressor(steps = s1c_steps)
    if (length(s1c_items) > 0L) {
      .batch_run_and_write(
        target = .batch_target("swereg", ".s1c_worker"),
        items = s1c_items,
        outputs = s1c_outputs,
        style = "return",
        n_workers = n_workers,
        dev_path = swereg_dev_path,
        p = p_s1c,
        label = "s1c"
      )
    }
    rm(s1c_items, s1c_outputs)

    # ====================================================================
    # s1d -- per enrollment (single subworker each, run sequentially)
    # ====================================================================
    cat(sprintf(
      "\n[s1d] Combine + impute + IPW + save (per enrollment, single subworker x %d)\n",
      n_enr
    ))
    p_s1d <- progressr::progressor(steps = n_enr)
    for (i in seq_len(n_enr)) {
      eid <- enrollment_ids[i]
      id <- sprintf("s1d_%s", eid)
      s1d_items <- list(list(
        enrollment_spec = all_es[[i]],
        spec = spec,
        work_dir = work_dir,
        skel_basenames = skel_basenames,
        impute_fn = impute_fn,
        stabilize = stabilize
      ))
      names(s1d_items) <- id
      # Declared-output commit, `staged_writer` style. The worker writes
      # each of its two outputs to .batch_where_to_write_output("raw" /
      # "imp") -- staging paths in the final directories -- and batchit
      # renames BOTH into place only once the item has returned. The two
      # writes are separated by imputation, IPW estimation and weight
      # truncation on a multi-GB panel, i.e. minutes; before this, a crash
      # in that window left `file_raw` committed with `file_imp` absent, and
      # nothing downstream could tell.
      #
      # `style = "return"` WOULD BE INCORRECT HERE, not merely slower. DO
      # NOT "simplify" this. TTEEnrollment is R6 wrapping a data.table, and
      # `$s2_ipw()` mutates that data.table BY REFERENCE
      # (R/r6_tteenrollment.R). So a returned `list(raw = trial, imp =
      # trial)` would be two references to the SAME post-mutation object,
      # and `file_raw` would silently contain the imputed, IPW'd panel
      # instead of the raw one. `$clone(deep = TRUE)` does not rescue it
      # either: TTEEnrollment defines no `deep_clone` private method, so R6
      # copies the binding, not the data.table.
      s1d_outputs <- list(c(
        raw = file.path(out_abs, ett_loop1$file_raw[i]),
        imp = file.path(out_abs, ett_loop1$file_imp[i])
      ))
      names(s1d_outputs) <- id
      .batch_run_and_write(
        target = .batch_target("swereg", ".s1d_worker"),
        items = s1d_items,
        outputs = s1d_outputs,
        style = "staged_writer",
        n_workers = 1L,
        dev_path = swereg_dev_path,
        p = p_s1d,
        label = "s1d"
      )
    }

    # All sub-steps complete -- remove the work directory.
    unlink(work_dir, recursive = TRUE, force = TRUE)
    cat(sprintf("\nRemoved work directory: %s\n", work_dir))
    invisible(self)
  }
)

#' @description Loop 2: Per-ETT IPCW-PP calculation and analysis file generation.
#' For each ETT, loads the imputed enrollment file, calls
#' `$s4_prepare_for_analysis()` (outcome + IPCW-PP + weight combination +
#' truncation), and saves the analysis-ready file.
#' @param output_dir Optional directory override containing imp files and
#'   where analysis files are saved. If `NULL` (default), uses
#'   `self$dir_tteplan`.
#' @param estimate_ipcw_pp_separately_by_treatment Logical, estimate IPCW-PP
#'   separately by treatment group (default: TRUE).
#' @param estimate_ipcw_pp_with_gam Logical, use GAM for IPCW-PP estimation
#'   (default: TRUE).
#' @param n_workers Integer, concurrent subprocesses (default: 1L).
#' @param swereg_dev_path Path to local swereg dev copy, or NULL.
TTEPlan$set(
  "public",
  "s2_generate_analysis_files_and_ipcw_pp",
  function(
    output_dir = NULL,
    estimate_ipcw_pp_separately_by_treatment = TRUE,
    estimate_ipcw_pp_with_gam = TRUE,
    n_workers = 1L,
    swereg_dev_path = NULL
  ) {
    # Validate FIRST, before any filesystem work.
    n_workers <- .validate_n_workers(
      n_workers,
      "s2_generate_analysis_files_and_ipcw_pp()"
    )
    if (is.null(output_dir)) {
      output_dir <- self$dir_tteplan
    }
    if (is.null(self$ett) || nrow(self$ett) == 0) {
      stop("plan has no ETTs. Use $add_one_ett() to add ETTs first.")
    }

    ett <- self$ett
    n_threads <- .threads_per_worker(n_workers)

    # Declared-output paths must be ABSOLUTE -- see the same block in
    # s1_generate_enrollments_and_ipw(). Create the directory BEFORE
    # normalizing, because normalizePath(mustWork = FALSE) leaves a
    # non-existent relative path relative. `output_dir` itself is untouched.
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    out_abs <- normalizePath(output_dir, mustWork = FALSE)
    if (!grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", out_abs)) {
      stop(
        "s2_generate_analysis_files_and_ipcw_pp(): output_dir did not resolve ",
        "to an absolute path (declared outputs must be absolute): ",
        out_abs,
        call. = FALSE
      )
    }

    sep_by_tx <- estimate_ipcw_pp_separately_by_treatment
    with_gam <- estimate_ipcw_pp_with_gam

    # Each ETT yields two analysis files off the same file_imp: per-protocol
    # (file_analysis, with IPCW) and intention-to-treat (file_analysis_itt,
    # no switch censoring, no IPCW). Old grids without the file_analysis_itt
    # column fall back to deriving the path from file_analysis.
    itt_path <- function(i) {
      if (
        "file_analysis_itt" %in%
          names(ett) &&
          !is.na(ett$file_analysis_itt[i])
      ) {
        ett$file_analysis_itt[i]
      } else {
        sub(
          "_analysis_",
          "_analysis_itt_",
          ett$file_analysis[i],
          fixed = TRUE
        )
      }
    }
    items <- list()
    outputs <- list()
    for (i in seq_len(nrow(ett))) {
      base <- list(
        outcome = ett$outcome_var[i],
        follow_up = ett$follow_up[i],
        file_imp_path = file.path(output_dir, ett$file_imp[i]),
        n_threads = n_threads,
        sep_by_tx = sep_by_tx,
        with_gam = with_gam
      )
      items[[length(items) + 1L]] <- c(base, list(estimand = "pp"))
      outputs[[length(outputs) + 1L]] <- c(
        analysis = file.path(out_abs, ett$file_analysis[i])
      )
      items[[length(items) + 1L]] <- c(base, list(estimand = "itt"))
      outputs[[length(outputs) + 1L]] <- c(
        analysis = file.path(out_abs, itt_path(i))
      )
    }
    # Stable ids: the analysis file each item commits, `s2_`-prefixed so a
    # batchit failure message names the stage as well as the file. Unique by
    # construction (PP and ITT commit different files).
    ids <- paste0(
      "s2_",
      vapply(outputs, function(o) basename(o[["analysis"]]), character(1))
    )
    names(items) <- ids
    names(outputs) <- ids

    cat(sprintf(
      "Loop 2: Building per-ETT analysis files - PP (IPCW) + ITT (%d file(s), %d worker(s), %d threads each)\n",
      length(items),
      n_workers,
      n_threads
    ))

    p <- progressr::progressor(steps = length(items))
    .batch_run_and_write(
      target = .batch_target("swereg", ".s2_worker"),
      items = items,
      outputs = outputs,
      style = "return",
      n_workers = n_workers,
      dev_path = swereg_dev_path,
      p = p
    )
  }
)

#' @description Loop 3: Compute all analysis results and store on the plan.
#'
#' For each enrollment: loads one analysis file and the raw file, computes
#' baseline characteristics (raw, unweighted, IPW, IPW truncated).
#' For each ETT: loads the analysis file, computes rates, IRR, and
#' heterogeneity test with both truncated and untruncated weights.
#'
#' Every ETT also gets the ABSOLUTE scale, and nothing switches it off.
#' Two estimand and weight combinations carry it: per-protocol on
#' `analysis_weight_pp_trunc`, stored under `rd_pp_trunc`, and
#' intention-to-treat on `ipw_trunc`, stored under `rd_itt`. Each stores
#' one summary row at the end of follow-up, with `rd`, `rd_lo`, `rd_hi`,
#' `nnt`, `nnt_lo`, `nnt_hi`, `nnt_direction` and `interval_status`. Each
#' also stores the full band-by-band curve under `rd_curve_pp_trunc` or
#' `rd_curve_itt`, with `surv_comparator` and `surv_intervention` beside the
#' risk difference.
#'
#' The curve also carries `n_persons_at_risk_comparator` and
#' `n_persons_at_risk_intervention`. Each is a head count of distinct people
#' in that arm and band. It is the count a numbers-at-risk row reports. The
#' figure reads it rather than opening the analysis file again.
#'
#' The bootstrap runs at 500 replicates with seed 1. Both are fixed here.
#' The confidence level is a STUDY property, read from
#' `spec$study$implementation$conf_level` and defaulting to 0.95. All three
#' are recorded on every stored row. The export path formats those numbers
#' and never recomputes them.
#'
#' Cost. Each risk difference is its own work item, so it is its own worker
#' process with its own read of the analysis file. That is two more reads
#' per ETT, or 1,080 more reads on a 540-ETT grid.
#'
#' Results are stored in `self$results_enrollment` and `self$results_ett`.
#' Every targeted result is recomputed on each call (no skip cache). Use
#' `plan$save()` to persist.
#'
#' @param enrollment_ids Character vector of enrollment IDs to analyze, or
#'   `NULL` (default) for all.
#' @param ett_ids Character vector of ETT IDs to analyze, or
#'   `NULL` (default) for all.
#' @param output_dir Optional directory override. If `NULL` (default),
#'   uses `self$dir_tteplan` (falls back to the legacy `self$output_dir`
#'   for plans created before the CandidatePath migration).
#' @param swereg_dev_path Path to local swereg dev copy, or NULL.
#' @param n_workers Integer >= 1 (default `1L`). Number of concurrent
#'   worker subprocesses for both the enrollment loop and the per-ETT
#'   loop. Each worker reads its own analysis file fresh, so peak RAM
#'   scales linearly with `n_workers`; on machines with multi-GB
#'   analysis files, set this conservatively. CPU threads per worker
#'   are auto-partitioned as `floor(detectCores() / n_workers)`.
TTEPlan$set(
  "public",
  "s3_analyze",
  function(
    enrollment_ids = NULL,
    ett_ids = NULL,
    output_dir = NULL,
    swereg_dev_path = NULL,
    n_workers = default_n_workers("s3")
  ) {
    # This checked >= 1 but never whole-ness, then as.integer()'d anyway -- so
    # s3_analyze(2.5) silently became 2 workers before parallel_pool() ever
    # saw the value.
    n_workers <- .validate_n_workers(n_workers, "s3_analyze()")
    if (is.null(output_dir)) {
      output_dir <- tryCatch(self$dir_tteplan, error = function(e) NULL)
    }
    if (is.null(output_dir)) {
      output_dir <- self$output_dir # legacy fallback
    }
    if (is.null(output_dir)) {
      stop(
        "output_dir is not set. Pass it as an argument, ",
        "configure dir_tteplan_cp, or run $s1_generate_enrollments_and_ipw() first."
      )
    }
    ett <- self$ett
    # The batch runner is thread-agnostic (each target calls setDTthreads()
    # itself), so the per-worker thread count is decided HERE, not injected by
    # the pool. Same value parallel_pool used to overwrite item n_threads
    # with, so runtime threading is unchanged. NB the ETT items below used to
    # say `n_threads = n_cores` and RELY on that overwrite -- carried
    # verbatim to .batch_run, that would have oversubscribed every worker.
    n_threads <- .threads_per_worker(n_workers)

    # Resolve enrollment IDs
    all_enrollment_ids <- unique(ett$enrollment_id)
    if (!is.null(enrollment_ids)) {
      bad <- setdiff(enrollment_ids, all_enrollment_ids)
      if (length(bad) > 0L) {
        stop("Unknown enrollment_ids: ", paste(bad, collapse = ", "))
      }
      all_enrollment_ids <- enrollment_ids
    }
    # When ett_ids is given, auto-narrow enrollments to only those needed
    if (!is.null(ett_ids)) {
      bad_ett <- setdiff(ett_ids, ett$ett_id)
      if (length(bad_ett) > 0L) {
        stop("Unknown ett_ids: ", paste(bad_ett, collapse = ", "))
      }
      ett_enrollment_ids <- unique(
        ett$enrollment_id[ett$ett_id %in% ett_ids]
      )
      all_enrollment_ids <- intersect(all_enrollment_ids, ett_enrollment_ids)
    }

    if (is.null(self$results_enrollment)) {
      self$results_enrollment <- list()
    }
    if (is.null(self$results_ett)) {
      self$results_ett <- list()
    }

    # Recompute everything in the targeted scope on every call: drop any
    # previously stored results for it, so the stores are pure output
    # containers, never a skip cache (Phase 5': the TTE stages hold no
    # staleness opinion; see PROJECT.md).
    if (is.null(enrollment_ids) && is.null(ett_ids)) {
      self$results_enrollment <- list()
      self$results_ett <- list()
    } else {
      for (eid in all_enrollment_ids) {
        self$results_enrollment[[eid]] <- NULL
      }
      # Drop exactly the ETTs that will be recomputed below (== ett_subset):
      # the ETTs under the targeted enrollments, further narrowed by ett_ids
      # if given. Using the raw `ett_ids` here would clear an ETT whose
      # enrollment is outside `all_enrollment_ids` -- dropped but never
      # recomputed, silently losing that result.
      drop_ett_ids <- ett$ett_id[ett$enrollment_id %in% all_enrollment_ids]
      if (!is.null(ett_ids)) {
        drop_ett_ids <- intersect(drop_ett_ids, ett_ids)
      }
      for (eid in drop_ett_ids) {
        self$results_ett[[eid]] <- NULL
      }
    }

    # --- Enrollment loop: baseline characteristics (subprocess-isolated) ---
    # Every targeted enrollment is recomputed (the scope was cleared above).
    enr_todo <- all_enrollment_ids

    # --- Build all work items for both loops ---
    # Enrollment items
    enr_items <- list()
    if (length(enr_todo) > 0L) {
      enr_items <- lapply(enr_todo, function(eid) {
        enr_rows <- ett[ett$enrollment_id == eid]
        analysis_files <- file.path(output_dir, enr_rows$file_analysis)
        sizes <- file.size(analysis_files)
        smallest <- which.min(sizes)
        list(
          analysis_path = analysis_files[smallest],
          raw_path = file.path(output_dir, enr_rows$file_raw[1]),
          enrollment_id = eid,
          n_threads = n_threads,
          arm_labels = .lookup_arm_labels(self$spec, eid)
        )
      })
      # Name the items by enrollment id so .batch_run uses those as stable ids:
      # a worker failure then reports the actual enrollment, not "item 1".
      names(enr_items) <- enr_todo
    }

    # ETT items
    ett_subset <- ett[ett$enrollment_id %in% all_enrollment_ids]
    if (!is.null(ett_ids)) {
      ett_subset <- ett_subset[ett_subset$ett_id %in% ett_ids]
    }
    ett_todo <- ett_subset
    n_ett <- nrow(ett_todo)

    # The study's confidence level, resolved ONCE and carried on every item.
    # It is a study property, not a per-figure one: s3 computes the interval
    # long before any figure exists.
    rd_conf_level <- .s3_conf_level(self$spec)

    all_items <- list()
    item_map <- list()
    if (n_ett > 0L) {
      for (i in seq_len(n_ett)) {
        apath <- file.path(output_dir, ett_todo$file_analysis[i])
        eid <- ett_todo$ett_id[i]
        # subgroup_var = NULL is EXPLICIT: the contract demands every formal,
        # including optional ones -- an optional arg silently absent is the
        # arm_labels bug's shape, and .batch_run rejects it.
        base <- list(
          analysis_path = apath,
          ett_id = eid,
          n_threads = n_threads,
          subgroup_var = NULL,
          conf_level = rd_conf_level
        )
        idx <- length(all_items)
        all_items[[idx + 1L]] <- c(
          base,
          list(
            method = "summary_and_rates",
            weight_col = ""
          )
        )
        item_map[[idx + 1L]] <- list(ett_i = i, slot = "summary_and_rates")

        all_items[[idx + 2L]] <- c(
          base,
          list(
            method = "irr",
            weight_col = "analysis_weight_pp_trunc"
          )
        )
        item_map[[idx + 2L]] <- list(ett_i = i, slot = "irr_pp_trunc")

        all_items[[idx + 3L]] <- c(
          base,
          list(
            method = "irr",
            weight_col = "analysis_weight_pp"
          )
        )
        item_map[[idx + 3L]] <- list(ett_i = i, slot = "irr_pp")

        # Intention-to-treat: read the ITT analysis file and weight on the
        # baseline IPW (ipw_trunc). Old grids without file_analysis_itt fall
        # back to deriving the path from the PP analysis path.
        itt_apath <- if (
          "file_analysis_itt" %in%
            names(ett_todo) &&
            !is.na(ett_todo$file_analysis_itt[i])
        ) {
          file.path(output_dir, ett_todo$file_analysis_itt[i])
        } else {
          sub("_analysis_", "_analysis_itt_", apath, fixed = TRUE)
        }
        all_items[[idx + 4L]] <- list(
          analysis_path = itt_apath,
          ett_id = eid,
          n_threads = n_threads,
          method = "irr",
          weight_col = "ipw_trunc",
          subgroup_var = NULL,
          conf_level = rd_conf_level
        )
        item_map[[idx + 4L]] <- list(ett_i = i, slot = "irr_itt")

        all_items[[idx + 5L]] <- list(
          analysis_path = itt_apath,
          ett_id = eid,
          n_threads = n_threads,
          method = "rates",
          weight_col = "ipw_trunc",
          subgroup_var = NULL,
          conf_level = rd_conf_level
        )
        item_map[[idx + 5L]] <- list(ett_i = i, slot = "rates_itt")

        # The absolute scale, for EVERY ETT and with nothing to switch it
        # off. Two estimand/weight combinations carry it: per-protocol on the
        # truncated weight, and intention-to-treat on the baseline IPW.
        # Per-protocol on the untruncated weight carries rates and the
        # incidence rate ratio only.
        #
        # It used to be computed in the export path, behind a figure option.
        # A production script that did not set the option drew every figure
        # without it, with no error and no warning. A quantity a figure can
        # switch off is a quantity a script can forget to ask for. So this
        # stage computes it. The export path only formats it.
        all_items[[idx + 6L]] <- c(
          base,
          list(
            method = "risk_difference",
            weight_col = "analysis_weight_pp_trunc"
          )
        )
        item_map[[idx + 6L]] <- list(ett_i = i, slot = "rd_pp_trunc")

        all_items[[idx + 7L]] <- list(
          analysis_path = itt_apath,
          ett_id = eid,
          n_threads = n_threads,
          method = "risk_difference",
          weight_col = "ipw_trunc",
          subgroup_var = NULL,
          conf_level = rd_conf_level
        )
        item_map[[idx + 7L]] <- list(ett_i = i, slot = "rd_itt")

        # Effect modification: for each subgroup variable, stratified IRRs
        # (irr_by_subgroup) and the interaction test (effect_modification_test)
        # for BOTH estimands -- PP (analysis_weight_pp_trunc) and ITT
        # (ipw_trunc). Old grids without subgroup_vars contribute nothing.
        sg_vars <- if (
          "subgroup_vars" %in%
            names(ett_todo) &&
            !is.null(ett_todo$subgroup_vars[[i]])
        ) {
          ett_todo$subgroup_vars[[i]]
        } else {
          character(0)
        }
        for (sv in sg_vars) {
          arms <- list(
            list(path = apath, weight = "analysis_weight_pp_trunc"),
            list(path = itt_apath, weight = "ipw_trunc")
          )
          for (arm in arms) {
            k <- length(all_items)
            all_items[[k + 1L]] <- list(
              analysis_path = arm$path,
              ett_id = eid,
              n_threads = n_threads,
              method = "irr_by_subgroup",
              weight_col = arm$weight,
              subgroup_var = sv,
              conf_level = rd_conf_level
            )
            item_map[[k + 1L]] <- list(ett_i = i, slot = "subgroup")
            all_items[[k + 2L]] <- list(
              analysis_path = arm$path,
              ett_id = eid,
              n_threads = n_threads,
              method = "effect_modification_test",
              weight_col = arm$weight,
              subgroup_var = sv,
              conf_level = rd_conf_level
            )
            item_map[[k + 2L]] <- list(ett_i = i, slot = "emtest")
          }
        }
      }
      # Stable ids: one per (ETT, analysis call), so a worker failure names
      # the exact analysis ("e01_f32_104w_45__irr__analysis_weight_pp"), not
      # "item 371". Unique by construction: weight_col separates the PP IRRs
      # from each other and from ITT; subgroup_var separates the stratified
      # calls. .batch_run stops on any collision rather than papering over it.
      names(all_items) <- vapply(
        all_items,
        function(it) {
          paste(
            c(
              it$ett_id,
              it$method,
              if (nzchar(it$weight_col)) it$weight_col,
              it$subgroup_var
            ),
            collapse = "__"
          )
        },
        character(1)
      )
    }

    # Total steps across both loops
    total_steps <- length(enr_items) + length(all_items)
    message("Output dir: ", output_dir)
    n_files <- length(list.files(output_dir, pattern = "\\.qs2$"))
    message(sprintf("  %d .qs2 files found", n_files))
    # The call count is REPORTED, not asserted. It was the literal "5"
    # while the builder emitted five items per ETT. A grid with a subgroup
    # variable takes four more items per variable, so the literal was
    # already wrong there.
    cat(sprintf(
      "Analyzing: %d enrollment(s) + %d ETTs (%d analysis calls, PP + ITT)\n",
      length(enr_items),
      n_ett,
      length(all_items)
    ))

    p <- progressr::progressor(steps = total_steps)

    # --- Enrollment loop ---
    if (length(enr_items) > 0L) {
      # Both s3 loops go through the ONE generic runner. The generic worker
      # do.call()s the target with EVERY named formal, which is what makes
      # the arm_labels class-of-bug (an optional formal silently dropped by a
      # hand-written dispatch script) structurally impossible here.
      enr_results <- .batch_run(
        target = .batch_target("swereg", ".s3_enrollment_worker"),
        items = enr_items,
        n_workers = n_workers,
        dev_path = swereg_dev_path,
        p = p
      )

      for (i in seq_along(enr_todo)) {
        self$results_enrollment[[enr_todo[i]]] <- enr_results[[i]]
      }
      rm(enr_results)
    }

    # --- ETT loop ---
    if (length(all_items) > 0L) {
      all_results <- .batch_run(
        target = .batch_target("swereg", ".s3_ett_worker"),
        items = all_items,
        n_workers = n_workers,
        dev_path = swereg_dev_path,
        p = p
      )

      # Assemble per-ETT results from the flat list
      for (j in seq_along(all_results)) {
        m <- item_map[[j]]
        eid <- ett_todo$ett_id[m$ett_i]
        if (is.null(self$results_ett[[eid]])) {
          self$results_ett[[eid]] <- list(
            enrollment_id = ett_todo$enrollment_id[m$ett_i],
            description = ett_todo$description[m$ett_i],
            computed_at = Sys.time()
          )
        }
        for (k in names(all_results[[j]])) {
          self$results_ett[[eid]][[k]] <- all_results[[j]][[k]]
        }
      }
      rm(all_results)
    }

    invisible(self)
  }
)
