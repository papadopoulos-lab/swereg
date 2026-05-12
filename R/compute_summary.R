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
.write_status_txt <- function(summary, path) {
  cols <- summary$columns
  meta <- summary$meta

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
      sprintf("Note: %d batches lack per-column counts (older swereg version) and were skipped in totals.",
              length(meta$missing_counts_batches)))
  }
  lines <- c(lines, sprintf("n_persons_total:      %d", summary$registry_wide$n_persons_total))
  lines <- c(lines, sprintf("n_person_weeks_total: %d", summary$registry_wide$n_person_weeks_total))
  lines <- c(lines, "")

  if (nrow(cols) == 0L) {
    lines <- c(lines, "(no per-column counts available)")
  } else {
    never <- cols[n_persons_with == 0L]
    rare  <- cols[n_persons_with > 0L & n_persons_with < 10L]
    okn   <- nrow(cols) - nrow(never) - nrow(rare)

    lines <- c(lines,
      sprintf("[!] Variables that NEVER matched (n_persons_with == 0): %d", nrow(never)))
    if (nrow(never) > 0L) {
      for (cn in never$column_name) lines <- c(lines, paste0("    ", cn))
      lines <- c(lines, "")
    }

    lines <- c(lines,
      sprintf("[!] Variables matched but VERY RARE (n_persons_with < 10): %d", nrow(rare)))
    if (nrow(rare) > 0L) {
      ord <- order(rare$n_persons_with)
      for (i in ord) {
        lines <- c(lines,
          sprintf("    %-60s n_persons = %d", rare$column_name[i],
                  rare$n_persons_with[i]))
      }
      lines <- c(lines, "")
    }

    lines <- c(lines,
      sprintf("[ok] All other variables (n_persons_with >= 10): %d", okn))
  }

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
  header <- c(
    sprintf("# swereg compute_summary"),
    sprintf("# built_at\t%s", format(meta$built_at, "%Y-%m-%dT%H:%M:%SZ")),
    sprintf("# swereg_version\t%s", meta$swereg_version),
    sprintf("# n_batches\t%d / %d (complete=%s)",
            meta$n_batches_present, meta$n_batches_expected,
            tolower(as.character(meta$is_complete))),
    sprintf("# n_persons_total\t%d",      summary$registry_wide$n_persons_total),
    sprintf("# n_person_weeks_total\t%d", summary$registry_wide$n_person_weeks_total),
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
    n_person_weeks_with = suppress(cols$n_person_weeks_with, suppress_below)
  )
  data.table::fwrite(
    body, file = path, append = TRUE,
    sep = "\t", quote = FALSE, col.names = TRUE
  )
  invisible(path)
}
