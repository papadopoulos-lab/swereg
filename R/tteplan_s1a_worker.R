# --- internal: finalize one enrollment's scout on a prepared skeleton ------
#
# Called by .s1a_worker_multi() (one canonical read shared across all
# enrollments). Its single-enrollment predecessor .s1a_worker() -- one
# canonical read PER enrollment -- was deleted in Phase 3: no production
# call site had selected it since the multi-enrollment scout landed. The
# caller is responsible for handing in a skeleton that has already had
# exclusions + treatment applied (.s1_prepare_loaded()).
.s1a_finalize_on_skeleton <- function(
  skeleton,
  enrollment_spec,
  spec,
  cache_path
) {
  enrollment_person_trial_id <- trial_id <- NULL
  pid <- enrollment_spec$design$person_id_var

  .assign_trial_ids(skeleton, enrollment_spec$design$period_width)
  data.table::setorderv(skeleton, c(pid, "trial_id", "isoyearweek"))

  eligible_cols <- attr(skeleton, "eligible_cols")
  attrition <- .s1_compute_attrition(skeleton, eligible_cols, pid)

  tuples <- .s1_eligible_tuples(skeleton, enrollment_spec$design)

  # Landmark qualification. `.s1b_worker()` draws comparators from the pooled
  # tuples and never sees a person-week again, so the drop MUST happen here,
  # while the weekly source data is still in hand. Every tuple that reaches
  # `enrolled_ids <- all_tuples[...]` is therefore already observed, eligible
  # and event-free at its landmark, and the draw refills the ratio from
  # qualified comparators alone.
  #
  # The four cascade rows stack onto the exclusion cascade: same columns, same
  # units, so `.s1b_worker()` sums them across skeletons unchanged and CONSORT
  # reads one continuous table.
  qualified <- .tte_qualify_bands(
    bands = tuples,
    data = skeleton,
    design = enrollment_spec$design,
    person_id_col = pid,
    arm_col = "intervention"
  )
  tuples <- qualified$bands
  if (!is.null(qualified$attrition)) {
    attrition <- data.table::rbindlist(
      list(attrition, qualified$attrition),
      use.names = TRUE
    )
  }

  tuples[,
    enrollment_person_trial_id := stringi::stri_c(
      enrollment_spec$enrollment_id,
      ".",
      get(pid),
      ".",
      trial_id
    )
  ]

  # Cache prepared skeleton for s1b reuse, projected to only the columns
  # s1b actually consumes (Lever 1 -- ~10x smaller cache, ~10x faster s1b
  # cache read).
  if (!is.null(cache_path)) {
    cache_cols <- .tte_s1_cache_columns(skeleton, enrollment_spec, spec)
    qs2_write_atomic(
      skeleton[, ..cache_cols],
      cache_path,
      nthreads = 1L
    )
  }

  return(list(tuples = tuples, attrition = attrition))
}

# --- internal: union of canonical columns needed across ALL enrollments ----
#
# Walks every enrollment_spec + the global spec to collect every column
# that s1a + s1b for any enrollment will read. Used by .s1a_worker_multi
# to project the canonical immediately after qs2 deserialisation, so the
# working skeleton is ~50-100 cols instead of ~1025 (the bulk being
# registry diagnosis/medication flags that no enrollment touches).
.tte_canonical_needed_cols <- function(spec, enrollment_specs, all_cols) {
  needed <- c("id", "isoyearweek", "isoyear")
  add_source <- function(impl) {
    if (!is.null(impl$source_variable)) {
      needed <<- c(needed, impl$source_variable)
    }
    if (
      !is.null(impl$source_variable_combined) &&
        impl$source_variable_combined %in% all_cols
    ) {
      needed <<- c(needed, impl$source_variable_combined)
    }
    if (!is.null(impl$variable)) {
      return(needed <<- c(needed, impl$variable))
    }
  }
  for (es in enrollment_specs) {
    if (!is.null(es$treatment_impl$variable)) {
      needed <- c(needed, es$treatment_impl$variable)
    }
    # The observation column, when the design names one. This projection runs
    # BEFORE .tte_s1_cache_columns(), so a column missing here never reaches
    # the cache allow-list at all.
    needed <- c(needed, .tte_observed_column(es$design$observed_var))
  }
  for (enr in spec$enrollments) {
    for (ae in enr$additional_inclusion %||% list()) {
      add_source(ae$implementation)
    }
    for (ec in enr$additional_exclusion %||% list()) {
      add_source(ec$implementation)
    }
  }
  for (ec in spec$exclusion_criteria %||% list()) {
    add_source(ec$implementation)
  }
  for (conf in spec$confounders %||% list()) {
    add_source(conf$implementation)
  }
  for (out in spec$outcomes %||% list()) {
    add_source(out$implementation)
  }
  return(intersect(unique(needed), all_cols))
}

# --- internal: multi-enrollment scout worker (sub-step s1a) -----------------
#
# Reads the canonical skeleton ONCE, projects it to the union of columns
# any enrollment needs (dropping the ~95% of registry-flag columns no
# enrollment touches), then applies each enrollment's exclusions +
# treatment + scout in place against that small projection. Between
# enrollments we drop any columns that prepare_loaded() / finalize() added
# to reveal the projected canonical. No data.table::copy() needed.
#
# Per-(enrollment, skeleton) outputs are streamed to disk inside the loop --
# 2 x length(enrollment_specs) files per item:
#
#   cache_{eid}   -> s1a_cache_enr{eid}_{basename}  (projected skeleton cache,
#                    written one frame down by .s1a_finalize_on_skeleton)
#   pre_{eid}     -> s1a_pre_enr{eid}_{basename}    (tuples + attrition)
#
# Dispatched via .batch_run_and_write(style = "staged_writer"): the PARENT
# declares all 2N paths (.s1a_outputs_for_skeleton()) and this worker BUILDS
# NO PATH of its own -- it resolves each destination by NAME through
# .batch_where_to_write_output(), which answers only inside a staged_writer
# item and errors on a name the parent did not declare. That turns a
# parent/worker name drift into a loud child failure instead of a cache file
# written where s1c will never look for it (which s1c would otherwise absorb
# by silently recomputing; see .s1c_worker_impl()'s `require_cache`).
#
# Consequence: `.s1a_worker_multi()` is NOT callable outside a staged_writer
# dispatch. It takes no `work_dir` -- there is nowhere for it to decide to
# write. The atomic commit also means a crashed item leaves none of its 2N
# files behind, where the old streamed writes left a partial set.
#
# The worker returns nothing through the result envelope, so the master never
# holds 19 (tuples, attrition) chunks in RAM after the pool completes.
.s1a_worker_multi <- function(file_path, enrollment_specs, spec) {
  id <- isoyearweek <- NULL # nolint
  n_threads <- enrollment_specs[[1L]]$n_threads %||% 1L
  data.table::setDTthreads(n_threads)
  skel_basename <- basename(file_path)

  canonical <- .s1_load_skeleton(file_path, n_threads)
  # Drop unneeded columns in place via `:= NULL` instead of copying the
  # needed subset out to a new data.table. With ~970 columns to drop out
  # of ~1025, the in-place drop is essentially free (each `:= NULL` just
  # removes the column reference); a `[, ..needed]` projection would
  # allocate a fresh data.table and copy every kept column's values.
  needed <- .tte_canonical_needed_cols(
    spec,
    enrollment_specs,
    names(canonical)
  )
  drop_cols <- setdiff(names(canonical), needed)
  if (length(drop_cols) > 0L) {
    canonical[, (drop_cols) := NULL]
  }
  data.table::setkey(canonical, id, isoyearweek)
  pristine_cols <- copy(names(canonical))

  for (k in seq_along(enrollment_specs)) {
    es <- enrollment_specs[[k]]
    eid <- es$enrollment_id
    canonical <- .s1_prepare_loaded(
      canonical,
      es,
      spec,
      derive_confounders = FALSE
    )
    one <- .s1a_finalize_on_skeleton(
      canonical,
      es,
      spec,
      cache_path = .batch_where_to_write_output(.s1a_cache_name(eid))
    )
    qs2_write_atomic(
      one,
      .batch_where_to_write_output(.s1a_pre_name(eid)),
      nthreads = 1L
    )
    rm(one)
    added_cols <- setdiff(names(canonical), pristine_cols)
    if (length(added_cols) > 0L) {
      canonical[, (added_cols) := NULL]
    }
    data.table::setattr(canonical, "eligible_cols", NULL)
  }
  return(invisible(NULL))
}

# --- internal: enumerate columns s1b will actually read from the cache -----
#
# The cache must contain:
#   - id, isoyearweek, trial_id          (keying + grouping)
#   - rd_intervention, baseline_intervention (treatment, computed in s1a)
#   - design$confounder_vars             (Phase B `first()` aggregation)
#   - design$treatment_var               (Phase B treatment override)
#   - design$outcome_vars                (Phase B `max()` aggregation)
#   - the observation column, when the design names one. This allow-list is
#     named, so a column absent from it is dropped before s1b and s1c ever
#     see it. Drop the observation column here and every later landmark step
#     reads an unobserved person as an ineligible one. Nothing errors and
#     nothing warns.
#   - all eligible_* columns             (comparator draw + attrition)
#   - source variables for any `computed = TRUE` confounder, because
#     tteplan_apply_derived_confounders() runs against the cached
#     skeleton in s1b and reads those raw sources (the OR'd
#     `*_combined` column is materialised at apply time).
.tte_s1_cache_columns <- function(skeleton, enrollment_spec, spec) {
  design <- enrollment_spec$design
  needed <- c(
    "id",
    "isoyearweek",
    "trial_id",
    "rd_intervention",
    "baseline_intervention",
    design$confounder_vars,
    design$treatment_var,
    design$outcome_vars,
    .tte_observed_column(design$observed_var),
    attr(skeleton, "eligible_cols")
  )
  for (conf in spec$confounders %||% list()) {
    impl <- conf$implementation
    if (isTRUE(impl$computed)) {
      needed <- c(needed, impl$source_variable)
    }
  }
  return(unique(intersect(needed, names(skeleton))))
}
