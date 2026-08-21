# --- Shared preparation helpers (used by s1a and s1b workers) ----------------

#' Read skeleton, apply exclusions, optionally derive confounders, set treatment.
#' Used by `.s1b_worker()` (full, with confounders); the scout path
#' (`.s1a_worker_multi()`) reads the canonical once and calls
#' `.s1_prepare_loaded()` directly.
#' @noRd
.s1_prepare_skeleton <- function(
  enrollment_spec,
  file_path,
  spec,
  derive_confounders = TRUE
) {
  data.table::setDTthreads(enrollment_spec$n_threads)
  skeleton <- .s1_load_skeleton(file_path, enrollment_spec$n_threads)
  .s1_prepare_loaded(
    skeleton,
    enrollment_spec,
    spec,
    derive_confounders = derive_confounders
  )
}

# --- internal: read + key + alloccol a canonical skeleton ------------------
#
# Split out of .s1_prepare_skeleton so .s1a_worker_multi() can read the
# canonical ONCE and reuse it across multiple enrollment_specs (Lever 2:
# reduces canonical reads from 19x per skeleton to 1x).
.s1_load_skeleton <- function(file_path, n_threads) {
  id <- isoyearweek <- NULL
  obj <- qs2_read(file_path, nthreads = 1L)
  # Under the Skeleton R6 migration, skeleton_*.qs2 files hold a
  # Skeleton R6 object wrapping the data.table. Legacy bare-data.table
  # files are still supported for backwards compat.
  skeleton <- if (inherits(obj, "Skeleton")) obj$data else obj
  rm(obj)
  # qs2 round-tripping drops data.table over-allocation; restore it so
  # subsequent `:=` mutations don't reallocate at a new address.
  skeleton <- data.table::setalloccol(
    skeleton,
    n = getOption("datatable.alloccol", 4096L)
  )
  # Skeleton is already sorted by (id, isoyearweek) from create_skeleton();
  # qs2 preserves row order so setkey is an O(n) verification, not a full sort.
  data.table::setkey(skeleton, id, isoyearweek)
  skeleton
}

# --- internal: apply exclusions + treatment to a pre-loaded skeleton -------
#
# Mutates skeleton in place. Caller is responsible for having called
# .s1_load_skeleton() (which sets the key + over-alloc). When called from
# .s1a_worker_multi() against a copy of the canonical, the previous
# enrollment's eligible_* columns don't leak in because the caller passes a
# fresh data.table::copy().
.s1_prepare_loaded <- function(
  skeleton,
  enrollment_spec,
  spec,
  derive_confounders = TRUE
) {
  baseline_intervention <- rd_intervention <- eligible_valid_treatment <- NULL
  # Combine exclusion grouped specs + computed-confounder grouped specs into
  # a SINGLE `dt[, c(...) := list(...), by = id]` call.
  built_excl <- .tte_build_exclusion_specs(skeleton, spec, enrollment_spec)
  conf_specs <- if (derive_confounders) {
    .tte_build_confounder_specs(skeleton, spec)
  } else {
    list()
  }
  skeleton <- .tte_apply_eligibility_batch(
    skeleton,
    c(built_excl$grouped_specs, conf_specs),
    id_col = "id"
  )
  skeleton <- skeleton_eligible_combine(skeleton, built_excl$eligible_cols)
  data.table::setattr(skeleton, "eligible_cols", built_excl$eligible_cols)
  x_tx <- enrollment_spec$treatment_impl
  skeleton[,
    c(
      "rd_intervention",
      "baseline_intervention",
      "eligible_valid_treatment"
    ) := {
      rd <- data.table::fcase(
        get(x_tx$variable) == x_tx$intervention_value , TRUE  ,
        get(x_tx$variable) == x_tx$comparator_value   , FALSE ,
        default = NA
      )
      list(rd, rd, !is.na(rd))
    }
  ]

  eligible_cols <- attr(skeleton, "eligible_cols")
  data.table::setattr(
    skeleton,
    "eligible_cols",
    c("eligible_valid_treatment", eligible_cols)
  )
  skeleton_eligible_combine(skeleton, attr(skeleton, "eligible_cols"))

  skeleton
}


#' Get all eligible (person_id, trial_id, intervention, recruit_week_index)
#' tuples from a skeleton.
#' Used by `.s1a_finalize_on_skeleton()` for scouting and available for direct
#' use. Caller should pre-sort by (pid, trial_id, isoyearweek) for efficiency.
#'
#' `recruit_week_index` names the week that recruited each person into each
#' band. It travels the whole scout chain: these tuples reach `.s1b_worker()`,
#' the comparator draw keeps it, and it lands in `enrolled_ids` on disk. The
#' s1c enrollment then reads it back on `entry_dt`.
#' @noRd
.s1_eligible_tuples <- function(skeleton, design) {
  if (!"trial_id" %in% names(skeleton)) {
    .assign_trial_ids(skeleton, design$period_width)
  }
  # `.band_baseline_treatment()` is the single source of truth for the
  # (person, band) -> baseline treatment mapping, and `enroll()` Phase C
  # calls the same function. It drops the weeks that are not eligible or
  # not in an arm, then uses any() and not first() over the weeks that are
  # left: treatment can start at any week within a trial period, not just
  # the first. first() silently drops ~75% of intervention people whose
  # treatment initiation falls mid-period. The no_prior_intervention exclusion
  # criterion handles the new-user restriction (one-time initiation)
  # separately.
  #
  # No setorderv() before the group-by: the scout path has already
  # sorted the skeleton by (pid, trial_id, isoyearweek), logical-vector
  # subsetting preserves order, and any() is order-independent regardless.
  # Dropping the re-sort avoids a 17M-row radix sort per scout worker.
  .band_baseline_treatment(
    data = skeleton,
    person_id_col = design$person_id_var,
    treatment_col = "rd_intervention",
    eligible_col = design$eligible_var,
    out_col = "intervention"
  )
}


# --- Attrition helper -------------------------------------------------------

#' Compute cumulative attrition counts per eligibility criterion.
#'
#' Returns a long-format data.table with rows per (trial_id, criterion)
#' AND a global row (`trial_id = NA`) per criterion. The global row
#' carries true overall `uniqueN(person_id)` — summing the per-trial
#' `n_persons` across trial_ids over-counts because one person who
#' enters N trials contributes N times to that sum. Downstream CONSORT
#' consumers must prefer the NA-trial_id rows for person headcounts.
#' Per-trial rows are retained for diagnostic slicing.
#'
#' Each row includes a "before_exclusions" entry plus one per cumulative
#' eligibility level, with intervention/comparator breakdowns (always in
#' person-trial units) for TARGET Item 8 reporting.
#'
#' @param skeleton data.table with trial_id and eligible_* columns assigned.
#' @param eligible_cols Character vector of eligible_* column names in
#'   application order.
#' @param pid Character, person ID column name.
#' @param treatment_var Character, name of the treatment column (default
#'   `"rd_intervention"`).
#' @return data.table with columns: trial_id, criterion, n_persons,
#'   n_person_trials, n_intervention, n_comparator. Rows with `trial_id = NA`
#'   carry true overall uniqueN of persons.
#' @noRd
.s1_compute_attrition <- function(
  skeleton,
  eligible_cols,
  pid,
  treatment_var = "rd_intervention"
) {
  .tte_pid <- .tte_tx <- .tte_tx_any <- trial_id <- . <- criterion <- NULL
  if (is.null(eligible_cols) || length(eligible_cols) == 0L) {
    stop("eligible_cols must be a non-empty character vector")
  }

  # Subset to needed columns for efficiency
  .cols <- c(pid, "trial_id", eligible_cols, treatment_var)
  sk <- skeleton[, .cols, with = FALSE]

  # Alias pid and treatment columns to fixed names for j-expressions
  data.table::setnames(sk, c(pid, treatment_var), c(".tte_pid", ".tte_tx"))

  # Classify each (person, trial) as any()-exposed so that a row in `pt0`
  # corresponds to one person-trial with a single boolean treatment flag.
  # Treatment uses any(): a person-trial is "intervention" if ANY week within
  # the trial period has .tte_tx == TRUE. This matches .s1_eligible_tuples().
  pt0 <- sk[,
    .(
      .tte_tx_any = any(.tte_tx == TRUE, na.rm = TRUE)
    ),
    by = c(".tte_pid", "trial_id")
  ]
  # Per-trial summary: drop rows where trial_id is NA (person-weeks that
  # fall outside any trial period). Without this filter, those rows
  # collapse into a `(trial_id = NA, criterion)` group whose `n_persons`
  # later gets summed together with the genuine `before_global` row in
  # the per-batch aggregation step (line ~1641, `by = .(trial_id,
  # criterion)`), inflating the reported global cohort by ~2x in CONSORT.
  before_row <- pt0[
    !is.na(trial_id),
    .(
      n_persons = data.table::uniqueN(.tte_pid),
      n_person_trials = .N,
      n_intervention = sum(.tte_tx_any, na.rm = TRUE),
      n_comparator = sum(!.tte_tx_any, na.rm = TRUE)
    ),
    by = trial_id
  ]
  before_row[, criterion := "before_exclusions"]
  # Global (across-trials) row: true uniqueN of persons, not a sum of
  # per-trial uniqueNs. CONSORT reporting reads this row; without it, the
  # person column of the attrition table double-counts everyone who
  # enters more than one sequential trial.
  before_global <- pt0[, .(
    trial_id = NA_integer_,
    n_persons = data.table::uniqueN(.tte_pid),
    n_person_trials = .N,
    n_intervention = sum(.tte_tx_any, na.rm = TRUE),
    n_comparator = sum(!.tte_tx_any, na.rm = TRUE)
  )]
  before_global[, criterion := "before_exclusions"]

  # For each cumulative criterion level, filter the full skeleton to rows where
  # ALL criteria 1..i pass, then classify treatment per person-trial using
  # any() --a person-trial is "intervention" if ANY eligible week within the
  # trial period has .tte_tx == TRUE. This matches .s1_eligible_tuples().
  rows <- vector("list", length(eligible_cols))
  global_rows <- vector("list", length(eligible_cols))
  cumulative_mask <- rep(TRUE, nrow(sk))

  for (i in seq_along(eligible_cols)) {
    # `sk[[col]]` is already logical; the explicit `== TRUE` is a no-op
    # except for cycling NA values, which `&` propagates either way.
    cumulative_mask <- cumulative_mask & sk[[eligible_cols[i]]]
    # Fused `[i, j, by=]` skips the intermediate `filtered` data.table
    # (a ~220 MB allocation on a 17 M-row panel) and lets data.table
    # do the filter + group-by in a single internal pass.
    pt_i <- sk[
      cumulative_mask,
      .(.tte_tx_any = any(.tte_tx == TRUE, na.rm = TRUE)),
      by = c(".tte_pid", "trial_id")
    ]
    # Same filter as `before_row` above: drop the spurious `trial_id = NA`
    # group so it doesn't collide with `global_rows[[i]]` during the
    # per-batch aggregation summing.
    rows[[i]] <- pt_i[
      !is.na(trial_id),
      .(
        n_persons = data.table::uniqueN(.tte_pid),
        n_person_trials = .N,
        n_intervention = sum(.tte_tx_any, na.rm = TRUE),
        n_comparator = sum(!.tte_tx_any, na.rm = TRUE)
      ),
      by = trial_id
    ][, criterion := eligible_cols[i]]
    # Global (trial_id = NA) companion row: true uniqueN of persons
    # across all trials after this cumulative criterion.
    global_rows[[i]] <- pt_i[,
      .(
        trial_id = NA_integer_,
        n_persons = data.table::uniqueN(.tte_pid),
        n_person_trials = .N,
        n_intervention = sum(.tte_tx_any, na.rm = TRUE),
        n_comparator = sum(!.tte_tx_any, na.rm = TRUE)
      )
    ][, criterion := eligible_cols[i]]
  }

  # sk is a local copy (column subset), no need to restore names

  data.table::rbindlist(
    c(list(before_row, before_global), rows, global_rows),
    use.names = TRUE
  )
}
