# Compare dev/bench/results/latest.csv to dev/bench/baseline.csv.
# Exit 0 on no regressions; non-zero if any benchmark crosses the threshold.
# Pass --accept to overwrite baseline.csv with latest.csv (no diff).

args <- commandArgs(trailingOnly = TRUE)

threshold_time <- 0.20  # 20% slower
threshold_mem  <- 0.50  # 50% more memory
accept <- FALSE
for (a in args) {
  if (a == "--accept") accept <- TRUE
  else if (grepl("^--threshold-time=", a)) threshold_time <- as.numeric(sub("^--threshold-time=", "", a))
  else if (grepl("^--threshold-mem=",  a)) threshold_mem  <- as.numeric(sub("^--threshold-mem=",  "", a))
}

latest_path   <- "dev/bench/results/latest.csv"
baseline_path <- "dev/bench/baseline.csv"

if (!file.exists(latest_path)) {
  stop("No latest results found. Run: Rscript dev/bench/run_all.R")
}

if (accept) {
  file.copy(latest_path, baseline_path, overwrite = TRUE)
  cat("Updated dev/bench/baseline.csv from latest.csv\n")
  quit(status = 0L)
}

if (!file.exists(baseline_path)) {
  cat("No baseline.csv yet. To create one from the current run:\n")
  cat("  Rscript dev/bench/diff.R --accept\n")
  quit(status = 0L)
}

latest   <- read.csv(latest_path,   stringsAsFactors = FALSE)
baseline <- read.csv(baseline_path, stringsAsFactors = FALSE)

m <- merge(
  baseline[, c("name", "median_seconds", "mem_alloc_mb")],
  latest[,   c("name", "median_seconds", "mem_alloc_mb")],
  by = "name", suffixes = c("_base", "_now"), all = TRUE
)
m$time_delta_pct <- (m$median_seconds_now - m$median_seconds_base) / m$median_seconds_base
m$mem_delta_pct  <- (m$mem_alloc_mb_now   - m$mem_alloc_mb_base)   / m$mem_alloc_mb_base

# Flag rows.
m$status <- ifelse(
  is.na(m$median_seconds_base), "NEW",
  ifelse(is.na(m$median_seconds_now), "GONE",
    ifelse(m$time_delta_pct > threshold_time | m$mem_delta_pct > threshold_mem, "REGRESSION", "ok")
  )
)

# Pretty-print.
cat(sprintf(
  "Comparing latest vs baseline (regress if time > +%.0f%% or mem > +%.0f%%):\n\n",
  threshold_time * 100, threshold_mem * 100
))
fmt_pct <- function(x) ifelse(is.na(x), "      -", sprintf("%+6.1f%%", x * 100))
out <- data.frame(
  name       = m$name,
  time_base  = sprintf("%6.3fs", m$median_seconds_base),
  time_now   = sprintf("%6.3fs", m$median_seconds_now),
  time_delta = fmt_pct(m$time_delta_pct),
  mem_base   = sprintf("%7.1f MB", m$mem_alloc_mb_base),
  mem_now    = sprintf("%7.1f MB", m$mem_alloc_mb_now),
  mem_delta  = fmt_pct(m$mem_delta_pct),
  status     = m$status,
  stringsAsFactors = FALSE
)
print(out, row.names = FALSE)

regressions <- sum(m$status == "REGRESSION", na.rm = TRUE)
if (regressions > 0L) {
  cat(sprintf("\n%d regression(s) detected.\n", regressions))
  quit(status = 1L)
}
cat("\nNo regressions.\n")
