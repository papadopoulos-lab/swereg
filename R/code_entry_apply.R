# Applying one code registry entry to a skeleton, and predicting the columns it writes.

# Predict the character vector of column names that a single code within a
# registry entry will contribute to a skeleton. Mirrors the prefixing logic
# inside .apply_code_entry_impl() below.
#
# Primary entries: for each (group_prefix, code_name) pair in
# (groups x code_name), the column is `prefix_code_name` (or just
# `code_name` when the prefix is empty). When `combine_as` is set, one
# additional column is produced with `combine_as_code_name`.
#
# Derived entries (kind = "derived"): one column per code_name of the form
# `<as>_<code_name>`, built by OR-ing `<from[i]>_<code_name>` across the
# upstream source prefixes. No group machinery.
#
# This is the single source of truth for column-name prediction. It is
# used by:
#   - RegistryStudy$summary_table() (via .entry_columns())
#   - Skeleton$drop_code_entry() via .entry_columns() on stored descriptors,
#     to know which columns to remove when a registry entry is dropped
.generated_columns_for_entry <- function(reg, code_name) {
  kind <- reg$kind %||% "primary"
  if (identical(kind, "derived")) {
    return(paste0(reg$as, "_", code_name))
  }
  cols <- character()
  for (i in seq_along(reg$groups)) {
    prefix <- names(reg$groups)[i]
    if (!is.null(prefix) && nzchar(prefix)) {
      cols <- c(cols, paste0(prefix, "_", code_name))
    } else {
      cols <- c(cols, code_name)
    }
  }
  if (!is.null(reg$combine_as)) {
    cols <- c(cols, paste0(reg$combine_as, "_", code_name))
  }
  return(cols)
}

# Vectorized wrapper: predict the full character vector of column names a
# registry entry contributes, across ALL its code names. Used by the
# Skeleton R6 class at drop time. The prediction MUST stay in sync with
# the behavior of every built-in `fn` (add_diagnoses, add_rx, add_operations,
# add_cancer_without_morphology, add_quality_registry); the parity tests in
# tests/testthat/test-entry_columns_parity.R enforce this invariant.
.entry_columns <- function(reg) {
  return(unlist(
    lapply(
      names(reg$codes),
      function(code_name) .generated_columns_for_entry(reg, code_name)
    ),
    use.names = FALSE
  ) %||%
    character())
}

# Apply ONE registry entry to a skeleton, mutating it in place.
#
# This is the per-entry body extracted from apply_codes_to_skeleton() so the
# Skeleton R6 class can call it one entry at a time during incremental code
# registry sync. apply_codes_to_skeleton() itself becomes a thin loop around
# this helper for the "apply everything at once" path; behavior is unchanged.
#
# Derived entries (kind = "derived") bypass batch_data entirely -- they
# OR together already-existing skeleton columns under new names. All
# source columns referenced by `reg$from` must already exist on the
# skeleton; they're produced by PRIMARY entries registered earlier in
# registration order. sync_with_registry() walks the registry in order,
# which keeps this invariant during incremental apply.
#
# The column-naming logic here MUST match .entry_columns() above or the
# Skeleton's drop_code_entry() will leak orphan columns. The parity tests
# guard this invariant.
.apply_code_entry_impl <- function(skeleton, batch_data, reg, id_col) {
  kind <- reg$kind %||% "primary"
  if (identical(kind, "derived")) {
    for (nm in names(reg$codes)) {
      src_cols <- paste0(reg$from, "_", nm)
      missing_cols <- setdiff(src_cols, names(skeleton))
      if (length(missing_cols)) {
        stop(
          "register_derived_codes: source columns missing from skeleton: ",
          paste(missing_cols, collapse = ", "),
          ". Primary entries producing these columns must be registered ",
          "BEFORE the derived entry.",
          call. = FALSE
        )
      }
      out_col <- paste0(reg$as, "_", nm)
      skeleton[,
        (out_col) := Reduce("|", lapply(src_cols, function(c) get(c)))
      ]
    }
    return(invisible(skeleton))
  }

  # Per-group calls
  for (i in seq_along(reg$groups)) {
    group_names <- reg$groups[[i]]
    prefix <- names(reg$groups)[i]

    # Get data: rbindlist if multiple groups
    data_list <- Filter(
      function(x) !is.null(x) && nrow(x) > 0,
      lapply(group_names, function(g) batch_data[[g]])
    )
    if (length(data_list) == 0) {
      next
    }
    data <- data.table::rbindlist(
      data_list,
      use.names = TRUE,
      fill = TRUE
    )

    # Prefix code names
    if (!is.null(prefix) && nzchar(prefix)) {
      prefixed_codes <- stats::setNames(
        reg$codes,
        paste0(prefix, "_", names(reg$codes))
      )
    } else {
      prefixed_codes <- reg$codes
    }

    # Call fn, wrapped with the add_* contract validator so misbehaving
    # user-registered fns fail loudly instead of silently corrupting the
    # skeleton. Input-data mutation is not checked here (harmless in the
    # batched pipeline; several built-ins deliberately mutate the input
    # as scratch space).
    snap <- skeleton_snapshot(skeleton)
    do.call(
      reg$fn,
      c(
        list(skeleton, data, id_name = id_col, codes = prefixed_codes),
        reg$fn_args
      )
    )
    validate_skeleton_after_add(
      skeleton,
      snap,
      expected_new_cols = names(prefixed_codes),
      context = sprintf("$register_codes(%s)", reg$label %||% "<anon>")
    )
  }

  # Combined (combine_as)
  if (!is.null(reg$combine_as)) {
    all_groups <- unique(unlist(reg$groups))
    data_list <- Filter(
      function(x) !is.null(x) && nrow(x) > 0,
      lapply(all_groups, function(g) batch_data[[g]])
    )
    if (length(data_list) > 0) {
      combined_data <- data.table::rbindlist(
        data_list,
        use.names = TRUE,
        fill = TRUE
      )
      combined_codes <- stats::setNames(
        reg$codes,
        paste0(reg$combine_as, "_", names(reg$codes))
      )
      snap <- skeleton_snapshot(skeleton)
      do.call(
        reg$fn,
        c(
          list(
            skeleton,
            combined_data,
            id_name = id_col,
            codes = combined_codes
          ),
          reg$fn_args
        )
      )
      validate_skeleton_after_add(
        skeleton,
        snap,
        expected_new_cols = names(combined_codes),
        context = sprintf(
          "$register_codes(%s, combine_as = %s)",
          reg$label %||% "<anon>",
          reg$combine_as
        )
      )
    }
  }

  return(invisible(skeleton))
}


# Per-column counts emitted by Skeleton$apply_code_entry() for every column
# a registry entry contributes to the skeleton. Stored on the entry's
# applied_registry record (under $counts) so the meta sidecar serialises
# them automatically and $compute_summary() can sum across batches without
# touching the heavy skeleton data files.
#
# Returns a named list keyed by column name; each value is a small list:
#   $n_persons_with       distinct ids where the column was TRUE in this batch
#   $n_person_weeks_with  rows where the column was TRUE in this batch
#
# Only logical columns get counts; non-logical columns (factor / numeric /
# character demographics added by randvars or framework) are silently
# skipped because $compute_summary() reports cohort presence, not value
# distributions -- those belong in $compute_population() or downstream
# Table 1 logic.
#
# `dt` is the skeleton data.table; the row-key column is hardcoded as "id"
# per the skeleton convention. (The study's `id_col` refers to the rawbatch
# join key, e.g. "lopnr" -- not the skeleton's own row-key.)
.compute_entry_column_counts <- function(dt, cols) {
  if (length(cols) == 0L) {
    return(list())
  }
  ids_all <- dt[["id"]]
  has_isoyear_flag <- "is_isoyear" %in% names(dt)
  is_weekly <- if (has_isoyear_flag) !dt$is_isoyear else rep(FALSE, nrow(dt))
  is_annual <- if (has_isoyear_flag) dt$is_isoyear else rep(FALSE, nrow(dt))

  out <- vector("list", length(cols))
  names(out) <- cols
  for (col in cols) {
    v <- dt[[col]]
    if (!is.logical(v)) {
      next
    }
    v_na <- !is.na(v) & v
    n_persons <- if (is.null(ids_all) || !any(v_na)) {
      0L
    } else {
      data.table::uniqueN(ids_all[v_na])
    }
    out[[col]] <- list(
      n_persons_with = as.integer(n_persons),
      n_person_weeks_with = as.integer(sum(v_na & is_weekly)),
      n_person_years_with = as.integer(sum(v_na & is_annual))
    )
  }
  return(out[!vapply(out, is.null, logical(1))])
}
