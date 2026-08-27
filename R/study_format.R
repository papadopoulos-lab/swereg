# Rendering helpers for RegistryStudy console output.

# Format byte counts for display
.format_bytes <- function(bytes) {
  if (bytes >= 1e9) {
    return(sprintf("%.1f GB", bytes / 1e9))
  } else if (bytes >= 1e6) {
    return(sprintf("%.1f MB", bytes / 1e6))
  } else if (bytes >= 1e3) {
    return(sprintf("%.1f KB", bytes / 1e3))
  } else {
    return(paste(bytes, "B"))
  }
}

# Collapse sequential integer runs into a compact range string.
# c(1, 2, 3, 5, 6, 7, 10) -> "1-3, 5-7, 10"
# Empty input returns "(none)".
.format_batch_range <- function(batches) {
  if (length(batches) == 0L) {
    return("(none)")
  }
  x <- sort(unique(as.integer(batches)))
  diffs <- c(Inf, diff(x))
  starts <- x[diffs != 1L]
  ends <- c(x[which(diffs != 1L)[-1] - 1L], x[length(x)])
  parts <- ifelse(
    starts == ends,
    as.character(starts),
    paste0(starts, "-", ends)
  )
  return(paste(parts, collapse = ", "))
}
