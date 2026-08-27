# RegistryStudy methods that report what a study holds and what it produced:
# the code registry description, the study summary, and the population tables.

#' @include r6_registrystudy.R
#' @description Print human-readable description of all registered codes.
RegistryStudy$set("public", "describe_codes", function() {
  if (length(self$code_registry) == 0) {
    cat("No codes registered.\n")
    return(invisible(self))
  }

  for (reg in self$code_registry) {
    codes <- reg$codes
    cat(sprintf(
      "\n=== %s (%d entries) ===\n",
      reg$label,
      length(codes)
    ))

    if (identical(reg$kind %||% "primary", "derived")) {
      cat(sprintf(
        "  Kind: derived (%s_* = %s)\n",
        reg$as,
        paste(paste0(reg$from, "_*"), collapse = " | ")
      ))
    } else {
      # Describe groups
      group_descs <- vapply(
        seq_along(reg$groups),
        function(i) {
          prefix <- names(reg$groups)[i]
          grps <- reg$groups[[i]]
          if (is.null(prefix) || !nzchar(prefix)) {
            return(paste(grps, collapse = " + "))
          } else {
            return(paste0(prefix, " (", paste(grps, collapse = " + "), ")"))
          }
        },
        character(1)
      )
      cat(sprintf("  Groups: %s\n", paste(group_descs, collapse = ", ")))

      if (!is.null(reg$combine_as)) {
        cat(sprintf("  Combined as: %s_*\n", reg$combine_as))
      }

      # Extra fn_args
      if (length(reg$fn_args) > 0) {
        args_str <- paste(
          names(reg$fn_args),
          vapply(reg$fn_args, deparse, character(1)),
          sep = " = ",
          collapse = ", "
        )
        cat(sprintf("  Extra args: %s\n", args_str))
      }
    }

    cat("\n")
    for (nm in names(codes)) {
      code_val <- codes[[nm]]
      code_str <- if (isTRUE(code_val)) {
        "event flag"
      } else if (is.call(code_val) || is.name(code_val)) {
        deparse(code_val)
      } else {
        paste(code_val, collapse = ", ")
      }
      cat(sprintf(
        "  %s: %s\n",
        nm,
        code_str
      ))
      gen_cols <- .generated_columns_for_entry(reg, nm)
      cat(sprintf(
        "    -> columns: %s\n",
        paste(gen_cols, collapse = ", ")
      ))
    }
  }
  return(invisible(self))
})

#' @description Return a data.table summarizing all registered codes.
#' @return data.table with columns: name, codes, label, generated_columns.
RegistryStudy$set("public", "summary_table", function() {
  rows <- list()

  for (reg in self$code_registry) {
    for (nm in names(reg$codes)) {
      gen_cols <- .generated_columns_for_entry(reg, nm)
      code_val <- reg$codes[[nm]]
      code_str <- if (isTRUE(code_val)) {
        "event flag"
      } else if (is.call(code_val) || is.name(code_val)) {
        deparse(code_val)
      } else {
        paste(code_val, collapse = ", ")
      }
      rows[[length(rows) + 1L]] <- list(
        name = nm,
        codes = code_str,
        label = reg$label,
        generated_columns = paste(gen_cols, collapse = ", ")
      )
    }
  }

  if (length(rows) == 0) {
    return(data.table::data.table(
      name = character(0),
      codes = character(0),
      label = character(0),
      generated_columns = character(0)
    ))
  }
  return(data.table::rbindlist(rows))
})

#' @description Read a pre-computed population table for one of the
#'   `by` specs declared at construction time via
#'   `population_by_specs`.
#'
#'   Population tables are computed automatically at the end of
#'   `$process_skeletons()` from the per-batch aggregations stored
#'   in each meta sidecar, then written as
#'   `population_<spec>.qs2` in the skeleton directory. This
#'   getter just reads that file.
#'
#' @param by Character vector of column names. Must match (in any
#'   order) one of the entries in `self$population_by_specs`.
#' @return The population `data.table` with columns: `isoyear`, the
#'   `by` columns, and `n` (unique-person count). Errors if the
#'   spec was not declared or the file does not exist yet.
RegistryStudy$set("public", "population", function(by) {
  key <- .population_spec_key(by)
  declared <- vapply(
    self$population_by_specs %||% list(),
    .population_spec_key,
    character(1)
  )
  if (!(key %in% declared)) {
    stop(
      "by = c(",
      paste(shQuote(by), collapse = ", "),
      ") is not in this study's $population_by_specs. ",
      "Add it to the RegistryStudy constructor and re-run ",
      "$process_skeletons().",
      call. = FALSE
    )
  }
  path <- file.path(
    self$data_skeleton_dir,
    sprintf("population_%s.qs2", .population_spec_filename_key(by))
  )
  if (!file.exists(path)) {
    stop(
      "Population file for spec ",
      key,
      " not found at ",
      path,
      ". Run $process_skeletons() to generate it.",
      call. = FALSE
    )
  }
  return(qs2::qs_read(path))
})

#' @description Print method for RegistryStudy.
#' @param ... Ignored.
RegistryStudy$set("public", "print", function(...) {
  cat("<RegistryStudy>\n")

  # Created timestamp
  if (!is.null(self$created_at)) {
    cat("  Created:", format(self$created_at, "%Y-%m-%d %H:%M:%S"), "\n")
  }

  cat("  IDs:", format(self$n_ids, big.mark = ","), "total\n")

  # Code registry
  if (length(self$code_registry) > 0) {
    parts <- vapply(
      self$code_registry,
      function(reg) {
        return(sprintf("%d %s", length(reg$codes), reg$label))
      },
      character(1)
    )
    cat("  Code registry:", paste(parts, collapse = ", "), "\n")
    # Count generated columns
    n_cols <- sum(vapply(
      self$code_registry,
      function(reg) {
        n_codes <- length(reg$codes)
        n_groups <- length(reg$groups)
        n_combine <- if (!is.null(reg$combine_as)) 1L else 0L
        return(n_codes * (n_groups + n_combine))
      },
      integer(1)
    ))
    cat("  Generated columns:", n_cols, "\n")
  }

  # Pipeline status: batches → rawbatch → skeleton (grouped together)
  cat(
    "  Batches:",
    self$n_batches,
    "(",
    format(self$batch_size, big.mark = ","),
    "IDs each)\n"
  )

  # Rawbatch info
  if (length(self$groups_saved) > 0) {
    rb_files <- list.files(
      self$data_rawbatch_dir,
      pattern = "\\d+_rawbatch_.*\\.qs2$",
      full.names = TRUE
    )
    rb_size <- sum(file.size(rb_files), na.rm = TRUE)
    cat(
      "  Rawbatch groups saved:",
      paste(self$groups_saved, collapse = ", "),
      "(",
      length(rb_files),
      "files,",
      .format_bytes(rb_size),
      ")\n"
    )
  } else {
    cat("  Rawbatch groups saved: (none)\n")
  }

  # Skeleton info
  n_observed <- length(self$skeleton_files)
  n_expected <- self$expected_skeleton_file_count
  if (n_observed > 0) {
    sk_size <- sum(file.size(self$skeleton_files), na.rm = TRUE)
    cat(
      "  Skeleton files:",
      n_observed,
      "/",
      n_expected,
      "expected",
      "(",
      .format_bytes(sk_size),
      ")\n"
    )
  } else {
    cat("  Skeleton files: 0 /", n_expected, "expected\n")
  }

  # Dirs -- show all candidates, mark resolved one with >
  .print_dir_candidates <- function(label, cp) {
    cat("  ", label, ":\n", sep = "")
    resolved <- tryCatch(cp$resolve(), error = function(e) NULL)
    for (p in cp$candidates) {
      prefix <- if (!is.null(resolved) && identical(p, resolved)) {
        "  > "
      } else {
        "    "
      }
      cat(prefix, p, "\n", sep = "")
    }
    return(invisible(NULL))
  }

  .print_dir_candidates("Rawbatch", self$data_rawbatch_cp)
  if (
    !identical(
      self$data_skeleton_cp$candidates,
      self$data_rawbatch_cp$candidates
    )
  ) {
    .print_dir_candidates("Skeleton", self$data_skeleton_cp)
  }
  if (!is.null(self$data_raw_cp)) {
    .print_dir_candidates("Data raw", self$data_raw_cp)
  }

  return(invisible(self))
})

# Aggregate per-batch counts from `meta_NNNNN.qs2` sidecars into a
# study-wide sanity summary and write it to disk. Three artefacts:
#   * `summary.qs2` in `data_skeleton_dir` (always)
#   * `status.txt`  in `data_meta_dir`     (always)
#   * audit-track TSV in `data_summaries_dir` (only on full runs)
# Reads only the meta sidecars; never touches the heavy skeletons.
RegistryStudy$set(
  "private",
  ".compute_summary",
  function(
    suppress_below = 5L,
    write_tsv = TRUE,
    write_status_txt = TRUE
  ) {
    n_expected <- as.integer(self$n_batches %||% 0L)
    candidates <- if (n_expected > 0L) {
      seq_len(n_expected)
    } else {
      as.integer(sub(
        "^.*meta_(\\d+)\\.qs2$",
        "\\1",
        list.files(self$data_skeleton_dir, pattern = "^meta_\\d+\\.qs2$")
      ))
    }
    meta_paths <- file.path(
      self$data_skeleton_dir,
      sprintf("meta_%05d.qs2", candidates)
    )
    meta_paths <- meta_paths[file.exists(meta_paths)]
    n_present <- length(meta_paths)
    if (n_expected == 0L) {
      n_expected <- n_present
    }
    is_complete <- (n_present == n_expected && n_expected > 0L)

    n_persons_total <- 0L
    n_person_weeks_total <- 0L
    n_person_years_total <- 0L
    weekly_min <- character(0)
    weekly_max <- character(0)
    annual_min <- integer(0)
    annual_max <- integer(0)
    col_n_persons <- list()
    col_n_weeks <- list()
    col_n_years <- list()
    col_label <- list()
    col_entry_fp <- list()
    missing_counts_batches <- integer(0)

    for (i in seq_along(meta_paths)) {
      m <- qs2::qs_read(meta_paths[i])
      n_persons_total <- n_persons_total + (m$n_persons %||% 0L)
      n_person_weeks_total <- n_person_weeks_total + (m$n_rows_weekly %||% 0L)
      n_person_years_total <- n_person_years_total + (m$n_rows_annual %||% 0L)
      if (!is.na(m$weekly_min_isoyearweek %||% NA)) {
        weekly_min <- c(weekly_min, m$weekly_min_isoyearweek)
      }
      if (!is.na(m$weekly_max_isoyearweek %||% NA)) {
        weekly_max <- c(weekly_max, m$weekly_max_isoyearweek)
      }
      if (!is.na(m$annual_min_isoyear %||% NA)) {
        annual_min <- c(annual_min, m$annual_min_isoyear)
      }
      if (!is.na(m$annual_max_isoyear %||% NA)) {
        annual_max <- c(annual_max, m$annual_max_isoyear)
      }
      for (fp in names(m$applied_registry %||% list())) {
        entry <- m$applied_registry[[fp]]
        counts <- entry$counts
        if (is.null(counts)) {
          missing_counts_batches <- c(missing_counts_batches, i)
          next
        }
        for (col in names(counts)) {
          c <- counts[[col]]
          col_n_persons[[col]] <- (col_n_persons[[col]] %||% 0L) +
            as.integer(c$n_persons_with %||% 0L)
          col_n_weeks[[col]] <- (col_n_weeks[[col]] %||% 0L) +
            as.integer(c$n_person_weeks_with %||% 0L)
          col_n_years[[col]] <- (col_n_years[[col]] %||% 0L) +
            as.integer(c$n_person_years_with %||% 0L)
          col_label[[col]] <- entry$label %||% NA_character_
          col_entry_fp[[col]] <- fp
        }
      }
    }

    cols <- sort(names(col_n_persons))
    columns_dt <- data.table::data.table(
      column_name = cols,
      entry_label = unlist(col_label[cols]) %||% character(0),
      entry_fingerprint = unlist(col_entry_fp[cols]) %||% character(0),
      n_persons_with = vapply(
        cols,
        function(k) col_n_persons[[k]],
        integer(1)
      ),
      n_person_weeks_with = vapply(
        cols,
        function(k) col_n_weeks[[k]],
        integer(1)
      ),
      n_person_years_with = vapply(
        cols,
        function(k) col_n_years[[k]],
        integer(1)
      )
    )

    summary <- list(
      meta = list(
        built_at = Sys.time(),
        swereg_version = as.character(utils::packageVersion("swereg")),
        n_batches_present = n_present,
        n_batches_expected = n_expected,
        is_complete = is_complete,
        missing_counts_batches = unique(missing_counts_batches)
      ),
      registry_wide = list(
        n_persons_total = n_persons_total,
        n_person_weeks_total = n_person_weeks_total,
        n_person_years_total = n_person_years_total,
        weekly_period_min = if (length(weekly_min) == 0L) {
          NA_character_
        } else {
          min(weekly_min)
        },
        weekly_period_max = if (length(weekly_max) == 0L) {
          NA_character_
        } else {
          max(weekly_max)
        },
        annual_period_min = if (length(annual_min) == 0L) {
          NA_integer_
        } else {
          min(annual_min)
        },
        annual_period_max = if (length(annual_max) == 0L) {
          NA_integer_
        } else {
          max(annual_max)
        }
      ),
      columns = columns_dt
    )

    if (isTRUE(write_status_txt)) {
      txt_path <- file.path(self$data_meta_dir, "status.txt")
      .write_status_txt(summary, txt_path)
      cat(sprintf("Status report written: %s\n", txt_path))
    }

    tsv_written <- FALSE
    if (isTRUE(write_tsv)) {
      if (!is_complete) {
        cat(sprintf(
          "TSV skipped: partial run (%d / %d batches present).\n",
          n_present,
          n_expected
        ))
      } else if (is.null(self$data_summaries_cp)) {
        cat(
          "TSV skipped: data_summaries_dir not configured on RegistryStudy.\n"
        )
      } else {
        dir_summaries <- self$data_summaries_cp$resolve()
        ts <- format(Sys.time(), "%Y-%m-%dT%H-%MZ", tz = "UTC")
        sha <- .swereg_git_short_sha(dir_summaries) %||% "NA"
        tsv_name <- sprintf(
          "summary_%s_%s_swereg-%s.tsv",
          ts,
          sha,
          summary$meta$swereg_version
        )
        tsv_path <- file.path(dir_summaries, tsv_name)
        .write_summary_tsv(summary, tsv_path, suppress_below)
        cat(sprintf("Summary TSV written (audit-track): %s\n", tsv_path))
        tsv_written <- TRUE
      }
    }
    summary$meta$tsv_written <- tsv_written

    qs2_path <- file.path(self$data_skeleton_dir, "summary.qs2")
    qs2_write_atomic(summary, qs2_path)
    cat(sprintf("Summary (qs2) written: %s\n", qs2_path))

    return(invisible(summary))
  }
)

# Build a population table for ONE registered by-spec by walking
# every meta_NNNNN.qs2 sidecar, pulling the per-batch aggregation
# already cached there, summing across batches, and completing the
# grid (CJ of observed values, NA -> 0). Writes
# `population_<safe_key>.qs2` in `data_skeleton_dir`. No skeleton
# I/O; runs in milliseconds even on hundreds of batches.
RegistryStudy$set("private", ".compute_population_for_spec", function(spec) {
  key <- .population_spec_key(spec)
  file_key <- .population_spec_filename_key(spec)
  group_cols <- c("isoyear", spec)

  candidates <- if (!is.null(self$n_batches) && self$n_batches > 0L) {
    seq_len(self$n_batches)
  } else {
    as.integer(sub(
      "^.*meta_(\\d+)\\.qs2$",
      "\\1",
      list.files(self$data_skeleton_dir, pattern = "^meta_\\d+\\.qs2$")
    ))
  }
  meta_paths <- file.path(
    self$data_skeleton_dir,
    sprintf("meta_%05d.qs2", candidates)
  )
  meta_paths <- meta_paths[file.exists(meta_paths)]
  if (length(meta_paths) == 0L) {
    stop(
      "No meta sidecars found in ",
      self$data_skeleton_dir,
      "; cannot compute population for spec ",
      key,
      ". Run $process_skeletons() first.",
      call. = FALSE
    )
  }

  pop_list <- vector("list", length(meta_paths))
  for (i in seq_along(meta_paths)) {
    m <- qs2::qs_read(meta_paths[i])
    agg <- m$population_aggregations[[key]]
    if (is.null(agg)) {
      stop(
        "Meta file ",
        basename(meta_paths[i]),
        " is missing population aggregation for spec ",
        key,
        ". Re-run $process_skeletons() to refresh.",
        call. = FALSE
      )
    }
    pop_list[[i]] <- agg
  }
  population <- data.table::rbindlist(pop_list)
  population <- population[, .(n = sum(n)), by = group_cols]

  unique_vals <- lapply(
    group_cols,
    function(col) sort(unique(population[[col]]))
  )
  names(unique_vals) <- group_cols
  complete_grid <- do.call(data.table::CJ, unique_vals)
  population <- population[complete_grid, on = group_cols]
  population[is.na(n), n := 0L]

  data.table::setorderv(population, group_cols)

  out_path <- file.path(
    self$data_skeleton_dir,
    sprintf("population_%s.qs2", file_key)
  )
  qs2_write_atomic(population, out_path)
  cat(sprintf(
    "Population table saved: %s (%d rows)\n",
    out_path,
    nrow(population)
  ))
  return(invisible(population))
})
