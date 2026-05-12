# Helpers for RegistryStudy$compute_summary()
#
# These build the human-readable status.txt report and the git-tracked
# TSV from the in-memory summary object returned by compute_summary().
# Kept internal; users invoke them indirectly via $compute_summary().

# Try to capture a short git SHA of the repo containing `dir` so the
# git-tracked TSV filename pins to the project's git state. Returns NULL
# if `git` is unavailable, the dir is not in a repo, or anything else
# goes wrong -- callers fall back to a placeholder.
.swereg_git_short_sha <- function(dir) {
  if (Sys.which("git") == "") return(NULL)
  out <- tryCatch(
    suppressWarnings(system2(
      "git",
      args = c("-C", shQuote(dir), "rev-parse", "--short", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) NULL
  )
  if (is.null(out) || length(out) == 0L || !nzchar(out[[1L]])) return(NULL)
  out[[1L]]
}


# Write the plain-text status report. Always created next to summary.qs2.
# Lists totals, partial-run state, variables that never matched, and
# variables with very small counts (n_persons_with < 10) so the user can
# eyeball the run without parsing the qs2.
# Pull the registry-type prefix off a column name (everything up to the
# first underscore). Generic: works for any column produced by add_*()
# since they all carry a swereg-side prefix like `ov_`, `sv_`, `osd_`,
# `dorsm_`, `can_`, `op_`, `rx_`. Columns without underscores bucket as
# the whole name ("(unprefixed)" if empty).
.bucket_prefix <- function(col_name) {
  pfx <- sub("_.*$", "", col_name)
  ifelse(pfx == col_name & !grepl("_", col_name, fixed = TRUE),
         "(unprefixed)", pfx)
}

# Group a sorted column-name vector by registry prefix, returning a list
# of c("<prefix>", <n>, <col1>, <col2>, ...) blocks ordered by descending
# count -- largest bucket first so the eye finds the dominant problem.
.bucket_lines <- function(cols_dt, n_persons_col = NULL, indent = "    ") {
  if (nrow(cols_dt) == 0L) return(character(0))
  buckets <- split(cols_dt, .bucket_prefix(cols_dt$column_name))
  buckets <- buckets[order(-vapply(buckets, nrow, integer(1)))]
  out <- character(0)
  for (pfx in names(buckets)) {
    b <- buckets[[pfx]]
    out <- c(out, sprintf("  %s (%d):", pfx, nrow(b)))
    if (is.null(n_persons_col)) {
      for (cn in b$column_name) out <- c(out, paste0(indent, cn))
    } else {
      ord <- order(b[[n_persons_col]])
      for (i in ord) {
        out <- c(out,
          sprintf("%s%-58s n_persons=%s  weeks=%s  years=%s",
                  indent,
                  b$column_name[i],
                  formatC(b$n_persons_with[i],      big.mark = ",", format = "d"),
                  formatC(b$n_person_weeks_with[i], big.mark = ",", format = "d"),
                  formatC(b$n_person_years_with[i], big.mark = ",", format = "d")))
      }
    }
  }
  out
}

.write_status_txt <- function(summary, path) {
  cols <- summary$columns
  meta <- summary$meta
  rw   <- summary$registry_wide
  comma <- function(x) formatC(x, big.mark = ",", format = "d")

  lines <- character(0)
  lines <- c(lines, "swereg compute_summary")
  lines <- c(lines, paste(rep("=", 22), collapse = ""))
  lines <- c(lines, sprintf("Built:           %s", format(meta$built_at, "%Y-%m-%dT%H:%M:%SZ")))
  lines <- c(lines, sprintf("swereg version:  %s", meta$swereg_version))
  status_word <- if (meta$is_complete) "FULL" else "PARTIAL"
  lines <- c(lines, sprintf("Skeletons:       %d / %d (%s)",
                            meta$n_batches_present, meta$n_batches_expected,
                            status_word))
  if (!meta$is_complete) {
    lines <- c(lines,
      "TSV audit-track skipped: full run required (n_present == n_expected)."
    )
  }
  if (length(meta$missing_counts_batches) > 0L) {
    lines <- c(lines,
      sprintf("Note: %d batches lack per-column counts and were skipped in totals.",
              length(meta$missing_counts_batches)))
  }
  lines <- c(lines, "")

  # --- Weekly data section ---
  lines <- c(lines, sprintf("Time period with WEEKLY data: %s to %s",
                            rw$weekly_period_min %||% "NA",
                            rw$weekly_period_max %||% "NA"))
  lines <- c(lines, sprintf("  n_persons (any weekly row):      %s", comma(rw$n_persons_total)))
  lines <- c(lines, sprintf("  n_person_weeks:                  %s", comma(rw$n_person_weeks_total)))
  lines <- c(lines, "")

  # --- Annual data section ---
  ay_min <- rw$annual_period_min; ay_max <- rw$annual_period_max
  lines <- c(lines, sprintf("Time period with ANNUAL data: %s to %s",
                            if (is.na(ay_min)) "NA" else as.character(ay_min),
                            if (is.na(ay_max)) "NA" else as.character(ay_max)))
  lines <- c(lines, sprintf("  n_person_years:                  %s", comma(rw$n_person_years_total)))
  lines <- c(lines, "")

  if (nrow(cols) == 0L) {
    lines <- c(lines, "(no per-column counts available)")
    writeLines(lines, path)
    return(invisible(path))
  }

  never <- cols[n_persons_with == 0L]
  rare  <- cols[n_persons_with > 0L & n_persons_with < 10L]
  okn   <- nrow(cols) - nrow(never) - nrow(rare)

  # ok-count first so it's the easy number to read.
  lines <- c(lines,
    sprintf("[ok] Variables with n_persons_with >= 10:  %s of %s total",
            comma(okn), comma(nrow(cols))))
  lines <- c(lines, "")

  # Never-matched: bucket-by-prefix summary, then per-bucket list.
  lines <- c(lines,
    sprintf("[!] Variables that NEVER matched (n_persons_with == 0): %s",
            comma(nrow(never))))
  if (nrow(never) > 0L) {
    lines <- c(lines, .bucket_lines(never, n_persons_col = NULL))
    lines <- c(lines, "")
  }

  # Rare: bucketed too, with the per-column count details.
  lines <- c(lines,
    sprintf("[!] Variables matched but VERY RARE (n_persons_with < 10): %s",
            comma(nrow(rare))))
  if (nrow(rare) > 0L) {
    lines <- c(lines, .bucket_lines(rare, n_persons_col = "n_persons_with"))
    lines <- c(lines, "")
  }

  lines <- c(lines,
    "For full per-column counts (including suppressed cells), load summary.qs2")
  lines <- c(lines, sprintf("  qs2::qs_read(\"<data_skeleton_dir>/summary.qs2\")"))

  writeLines(lines, path)
  invisible(path)
}


# Write the audit-track TSV. Counts strictly less than `suppress_below`
# are masked to "<N" (Swedish registry convention). The qs2 keeps exact
# values; only the git-tracked TSV is suppressed.
.write_summary_tsv <- function(summary, path, suppress_below = 5L) {
  cols <- data.table::copy(summary$columns)

  # Header block as comment lines (TSV reader ignores via skip)
  meta <- summary$meta
  rw   <- summary$registry_wide
  header <- c(
    sprintf("# swereg compute_summary"),
    sprintf("# built_at\t%s", format(meta$built_at, "%Y-%m-%dT%H:%M:%SZ")),
    sprintf("# swereg_version\t%s", meta$swereg_version),
    sprintf("# n_batches\t%d / %d (complete=%s)",
            meta$n_batches_present, meta$n_batches_expected,
            tolower(as.character(meta$is_complete))),
    sprintf("# weekly_period\t%s to %s",
            rw$weekly_period_min %||% "NA", rw$weekly_period_max %||% "NA"),
    sprintf("# annual_period\t%s to %s",
            if (is.na(rw$annual_period_min)) "NA" else as.character(rw$annual_period_min),
            if (is.na(rw$annual_period_max)) "NA" else as.character(rw$annual_period_max)),
    sprintf("# n_persons_total\t%d",      rw$n_persons_total),
    sprintf("# n_person_weeks_total\t%d", rw$n_person_weeks_total),
    sprintf("# n_person_years_total\t%d", rw$n_person_years_total),
    sprintf("# suppress_below\t%d", suppress_below)
  )

  writeLines(header, path)

  if (nrow(cols) == 0L) {
    # Header only -- no per-column rows to emit.
    return(invisible(path))
  }

  suppress <- function(x, k) {
    out <- as.character(x)
    out[x < k & x > 0L] <- sprintf("<%d", k)
    out[x == 0L]        <- "0"
    out
  }
  body <- data.table::data.table(
    column_name         = cols$column_name,
    entry_label         = cols$entry_label,
    entry_fingerprint   = cols$entry_fingerprint,
    n_persons_with      = suppress(cols$n_persons_with,      suppress_below),
    n_person_weeks_with = suppress(cols$n_person_weeks_with, suppress_below),
    n_person_years_with = suppress(cols$n_person_years_with, suppress_below)
  )
  data.table::fwrite(
    body, file = path, append = TRUE,
    sep = "\t", quote = FALSE, col.names = TRUE
  )
  invisible(path)
}
