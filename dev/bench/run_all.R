# Run every dev/bench/bench_*.R against the current source tree, write
# results/latest.csv (and an archived copy keyed by git SHA + UTC timestamp).

suppressPackageStartupMessages({
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools is required to load swereg from source.")
  }
  devtools::load_all(".", quiet = TRUE)
})

source("dev/bench/lib.R")
bench_reset()

bench_files <- list.files("dev/bench", pattern = "^bench_.*\\.R$", full.names = TRUE)
for (f in bench_files) {
  source(f, local = TRUE)
}

# Each bench_*.R defines one or more benchmark_<name>() functions.
# Discover and call them.
bench_fns <- ls(pattern = "^benchmark_")
for (fn in bench_fns) {
  do.call(fn, list())
}

results <- bench_results()
if (is.null(results) || nrow(results) == 0L) {
  stop("No benchmarks ran. Did you define any benchmark_*() functions?")
}

dir.create("dev/bench/results", showWarnings = FALSE, recursive = TRUE)

# Archived copy keyed by git SHA + UTC timestamp.
sha <- tryCatch(
  system("git rev-parse --short HEAD", intern = TRUE),
  error = function(e) "nogit"
)
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
archive_path <- file.path("dev/bench/results", sprintf("%s_%s.csv", sha, ts))
write.csv(results, archive_path, row.names = FALSE)

# Latest pointer (used by diff.R).
write.csv(results, "dev/bench/results/latest.csv", row.names = FALSE)

cat("\nWrote:\n  dev/bench/results/latest.csv\n  ", archive_path, "\n", sep = "")
