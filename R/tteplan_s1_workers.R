# --- s1c: Panel build worker (formerly .s1b_worker) ------------------------

#' Per-(enrollment, skeleton) panel build worker for sub-step s1c.
#'
#' Reads the s1a cache, restricts to enrolled persons (from s1b), derives
#' confounders, and expands to the trial-week panel via [TTEEnrollment$new()].
#' Dispatched via .batch_run_and_write(style = "return") in a fresh R session:
#' the worker RETURNS the panel and writes nothing itself, and batchit commits
#' the returned `panel` element to the declared output path atomically.
#'
#' CONTRACT CHANGE (s1a staged_writer phase): the production path now passes
#' `require_cache = TRUE`, so a missing s1a cache is a LOUD ERROR here instead
#' of a silent recompute. The recompute fallback produces a different column
#' set and runs roughly 10x slower, and it used to hide a parent/worker path
#' drift completely -- no error, no warning, changed production output. A
#' standalone partial s1c run, or an external `swereg:::.s1c_worker()` caller
#' that has not run s1a first, will now fail where it previously recomputed.
#' Call `.s1c_worker_impl()` directly with `require_cache = FALSE` (the
#' default) if you want the old recomputing behaviour.
#'
#' @param enrollment_spec Enrollment spec list.
#' @param file_path Path to a skeleton `.qs2` file. Read only on the
#'   `require_cache = FALSE` recompute fallback, which this worker never
#'   takes; kept so `.s1c_worker_impl()` has it for dev callers.
#' @param spec Parsed study spec.
#' @param work_dir Per-project s1 work directory ([.s1_work_dir()]). An INPUT
#'   here, not an output-path source: the worker reads the s1a cache and the
#'   s1b enrolled-ids file from it.
#' @return `list(panel = <TTEEnrollment>)`. Writes nothing.
#' @noRd
.s1c_worker <- function(enrollment_spec, file_path, spec, work_dir) {
  eid <- enrollment_spec$enrollment_id
  skel_basename <- basename(file_path)
  cache_path <- .s1a_cache_path(work_dir, eid, skel_basename)
  enrolled_ids_path <- .s1b_enrolled_ids_path(work_dir, eid)

  enrolled_ids <- qs2_read(enrolled_ids_path, nthreads = 1L)
  enrollment <- .s1c_worker_impl(
    enrollment_spec,
    file_path,
    spec,
    enrolled_ids,
    cache_path,
    require_cache = TRUE
  )
  return(list(panel = enrollment))
}

# Core panel-build logic, kept separate from .s1c_worker() so dev/verify
# scripts and tests can drive it directly with in-memory enrolled_ids
# instead of having to materialise a work_dir.
#
# `require_cache` is the guard against a SILENT production-output change. s1a
# declares its cache path in the parent and writes it by name in the worker;
# .s1c_worker() recomputes that same path here to read it back. If those two
# ever drift by one character the old code just took the `else` branch --
# ~10x slower, a different column set, no error and no warning. With
# require_cache = TRUE the branch decision is made ONCE (`use_cache` below,
# so there is no TOCTOU gap between the check and the read) and a missing
# cache stops the item.
#
# It stays FALSE by default: dev/verify scripts and direct callers that never
# ran s1a legitimately want the recompute.
#' @noRd
.s1c_worker_impl <- function(
  enrollment_spec,
  file_path,
  spec,
  enrolled_ids,
  cache_path = NULL,
  require_cache = FALSE
) {
  id <- isoyearweek <- NULL
  # Subset to enrolled persons before expensive confounder computation
  pid <- enrollment_spec$design$person_id_var
  enrolled_persons <- unique(enrolled_ids[[pid]])

  # Decide ONCE. Reusing `use_cache` for the branch below is what closes the
  # TOCTOU gap a bare `stop()` in the wrapper would leave open.
  use_cache <- !is.null(cache_path) && file.exists(cache_path)
  if (isTRUE(require_cache) && !use_cache) {
    stop(
      ".s1c_worker_impl(): the s1a skeleton cache is required but absent: ",
      if (is.null(cache_path)) "<NULL>" else cache_path,
      "\nThis means s1a never committed the cache this (enrollment, skeleton) ",
      "pair needs, or the path s1c derives no longer matches the one s1a ",
      "declared. Recomputing instead would succeed silently with a DIFFERENT ",
      "column set at ~10x the cost, so it is refused on the production path.",
      call. = FALSE
    )
  }

  if (use_cache) {
    # Reuse cached skeleton from s1a (already has exclusions + treatment applied)
    data.table::setDTthreads(enrollment_spec$n_threads)
    # qs2_read() has already restored the data.table over-allocation that
    # qs2 does not keep. See the "data.table over-allocation" section of
    # `?qs2_read`.
    skeleton <- qs2_read(cache_path, nthreads = 1L)
    data.table::setkey(skeleton, id, isoyearweek)
    # Binary-search join on the existing (id, isoyearweek) key beats
    # `%in%` for selecting enrolled persons from a 17 M-row panel; same
    # fix as in `private$enroll`. Saves ~2 s + a hash allocation per
    # stage-1b worker call.
    skeleton <- skeleton[
      .(unique(enrolled_persons)),
      on = pid,
      nomatch = NULL
    ]
    # Mark that we've already filtered to enrolled persons so
    # private$enroll() in Phase B doesn't redo the same filter (which
    # otherwise allocates another 2.85 GB copy of the panel as an
    # identity transformation).
    data.table::setattr(skeleton, ".tte_filtered_to_enrolled", TRUE)
    skeleton <- tteplan_apply_derived_confounders(skeleton, spec)
  } else {
    skeleton <- .s1_prepare_skeleton(
      enrollment_spec,
      file_path,
      spec,
      derive_confounders = FALSE
    )
    skeleton <- skeleton[
      .(unique(enrolled_persons)),
      on = pid,
      nomatch = NULL
    ]
    data.table::setattr(skeleton, ".tte_filtered_to_enrolled", TRUE)
    skeleton <- tteplan_apply_derived_confounders(skeleton, spec)
  }
  enrollment <- TTEEnrollment$new(
    data = skeleton,
    design = enrollment_spec$design,
    enrolled_ids = enrolled_ids,
    seed = enrollment_spec$seed,
    extra_cols = "isoyearweek",
    own_data = TRUE
  )
  rm(skeleton)

  # Prefix enrollment_person_trial_id with enrollment_id
  id_var <- enrollment$design$id_var
  if (nrow(enrollment$data) > 0L && id_var %in% names(enrollment$data)) {
    enrollment$data[,
      (id_var) := stringi::stri_c(
        enrollment_spec$enrollment_id,
        ".",
        get(id_var)
      )
    ]
  }
  return(enrollment)
}


# --- s1b: Draw worker (single subprocess per enrollment) -------------------

#' Draw sub-step: pool per-skeleton scout outputs for one enrollment, then
#' draw comparators at the comparator-to-intervention ratio.
#'
#' Reads the 2,194-ish `s1a_pre_*` chunks for this enrollment, rbindlists
#' tuples + attrition, samples comparators per `trial_id`, and RETURNS the two
#' declared outputs:
#'   - `enrolled_ids` (post-draw enrolled IDs for s1c)
#'   - `counts`       (`matching` + `attrition` sidecar the master reads back)
#'
#' Runs in a fresh R session via .batch_run_and_write() with `n_workers = 1L`
#' and `style = "return"`: batchit commits both objects to their declared
#' paths. The worker itself writes nothing, and the master never holds the
#' rbinded tuples in RAM.
#'
#' @param enrollment_spec Enrollment spec list (includes seed, comparator_to_intervention_ratio,
#'   design$person_id_var, enrollment_id).
#' @param spec Parsed study spec (not currently used; reserved for future
#'   per-spec comparator-draw rules).
#' @param work_dir Per-project s1 work directory.
#' @param skel_basenames Character vector of skeleton basenames (used to
#'   construct `s1a_pre_*` paths).
#' @return `list(enrolled_ids = , counts = )`, matching the declared output
#'   names at the call site.
#' @noRd
.s1b_worker <- function(
  enrollment_spec,
  spec,
  work_dir,
  skel_basenames
) {
  intervention <- trial_id <- criterion <- n_persons <- n_person_trials <-
    n_intervention <- n_comparator <- NULL

  eid <- enrollment_spec$enrollment_id
  data.table::setDTthreads(enrollment_spec$n_threads)

  pre_paths <- vapply(
    skel_basenames,
    function(bn) {
      return(.s1a_pre_path(work_dir, eid, bn))
    },
    character(1)
  )
  missing_pre <- !file.exists(pre_paths)
  if (any(missing_pre)) {
    stop(sprintf(
      "s1b: %d/%d pre files missing for enrollment '%s'. First missing: %s",
      sum(missing_pre),
      length(pre_paths),
      eid,
      pre_paths[which(missing_pre)[1L]]
    ), call. = FALSE)
  }

  tuples_chunks <- vector("list", length(pre_paths))
  attr_chunks <- vector("list", length(pre_paths))
  for (j in seq_along(pre_paths)) {
    pre <- qs2_read(pre_paths[j], nthreads = 1L)
    tuples_chunks[[j]] <- pre$tuples
    attr_chunks[[j]] <- pre$attrition
    rm(pre)
  }
  all_tuples <- data.table::rbindlist(tuples_chunks, use.names = TRUE)
  all_attrition <- data.table::rbindlist(attr_chunks, use.names = TRUE)
  rm(tuples_chunks, attr_chunks)

  set.seed(enrollment_spec$seed)
  x_ratio <- enrollment_spec$comparator_to_intervention_ratio

  enrolled_ids <- all_tuples[,
    {
      int_rows <- .SD[intervention == TRUE]
      cmp_rows <- .SD[intervention == FALSE]
      n_to_sample <- min(
        round(x_ratio * nrow(int_rows)),
        nrow(cmp_rows)
      )
      sampled <- if (n_to_sample > 0) {
        cmp_rows[sample(.N, n_to_sample)]
      } else {
        cmp_rows[0]
      }
      data.table::rbindlist(list(int_rows, sampled))
    },
    by = trial_id
  ]

  global_counts <- all_tuples[,
    .(
      n_intervention_total = sum(intervention == TRUE),
      n_comparator_total = sum(intervention == FALSE)
    ),
    by = trial_id
  ]
  enrolled_counts <- enrolled_ids[,
    .(
      n_intervention_enrolled = sum(intervention == TRUE),
      n_comparator_enrolled = sum(intervention == FALSE)
    ),
    by = trial_id
  ]
  matching_counts <- merge(
    global_counts,
    enrolled_counts,
    by = "trial_id",
    all.x = TRUE
  )

  attrition_summary <- all_attrition[,
    .(
      n_persons = sum(n_persons),
      n_person_trials = sum(n_person_trials),
      n_intervention = sum(n_intervention),
      n_comparator = sum(n_comparator)
    ),
    by = .(trial_id, criterion)
  ]

  counts <- list(attrition = attrition_summary, matching = matching_counts)

  return(list(enrolled_ids = enrolled_ids, counts = counts))
}


# --- s1d: Post worker (single subprocess per enrollment) -------------------

#' Post sub-step: pool per-skeleton panel chunks for one enrollment, impute,
#' compute IPW, truncate, and save the final `file_raw` + `file_imp`.
#'
#' Runs in a fresh R session via `.batch_run_and_write()` with
#' `style = "staged_writer"` and `n_workers = 1L`. The master never holds the
#' rbinded panel in RAM, so multi-GB enrollments don't push the parent process
#' over the OOM line.
#'
#' It is handed NO output paths. Both destinations are resolved with
#' `.batch_where_to_write_output("raw" / "imp")`, which only answers inside an
#' active `staged_writer` run -- so this worker cannot be called directly
#' in-process. That indirection is what makes the pair ALL-OR-NONE: the two
#' writes land on attempt-scoped staging files, and batchit renames both into
#' place only after the item returns. Minutes of imputation + IPW + weight
#' truncation sit between them, and a failure anywhere in that window now
#' leaves both final paths untouched -- absent if they were absent,
#' byte-identical to their previous contents if they existed.
#'
#' `qs2_write_atomic()` is kept for both writes: its `.tmp` litter matches
#' batchit's attempt-scoped failure sweep.
#'
#' @param enrollment_spec Enrollment spec list.
#' @param spec Parsed study spec (not currently used; reserved).
#' @param work_dir Per-project s1 work directory.
#' @param skel_basenames Character vector of skeleton basenames.
#' @param impute_fn Imputation callback or NULL. It receives the
#'   `.tte_entry__` snapshot names, and not the plain confounder names.
#' @param stabilize Logical, stabilize IPW.
#' @return Invisible NULL.
#' @noRd
.s1d_worker <- function(
  enrollment_spec,
  spec,
  work_dir,
  skel_basenames,
  impute_fn = NULL,
  stabilize = TRUE
) {
  eid <- enrollment_spec$enrollment_id
  data.table::setDTthreads(enrollment_spec$n_threads)

  panel_paths <- vapply(
    skel_basenames,
    function(bn) {
      return(.s1c_panel_path(work_dir, eid, bn))
    },
    character(1)
  )
  missing_panels <- !file.exists(panel_paths)
  if (any(missing_panels)) {
    stop(sprintf(
      "s1d: %d/%d panel files missing for enrollment '%s'. First missing: %s",
      sum(missing_panels),
      length(panel_paths),
      eid,
      panel_paths[which(missing_panels)[1L]]
    ), call. = FALSE)
  }

  panels <- vector("list", length(panel_paths))
  for (j in seq_along(panel_paths)) {
    panels[[j]] <- qs2_read(panel_paths[j], nthreads = 1L)
  }
  trial <- tteenrollment_rbind(panels)
  rm(panels)

  qs2_write_atomic(trial, .batch_where_to_write_output("raw"), nthreads = 1L)

  if (!is.null(impute_fn)) {
    # Imputation is name-list driven, so it MUST be handed the entry-window
    # snapshot names. `$s2_ipw()` fits on those columns, and handing it the
    # plain confounder names would leave every one of them unimputed. It also
    # MUST NOT overwrite the follow-up value under the plain name.
    trial <- impute_fn(
      trial,
      .tte_entry_col(enrollment_spec$design$confounder_vars)
    )
  }
  trial$s2_ipw(stabilize = stabilize)
  trial$s3_truncate_weights(weight_cols = "ipw")

  qs2_write_atomic(trial, .batch_where_to_write_output("imp"), nthreads = 1L)
  return(invisible(NULL))
}
