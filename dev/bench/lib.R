# Shared helpers for the dev/bench/ scaffold.
# Sourced by run_all.R; exposes: bench_run(), bench_results(), bench_reset().

suppressPackageStartupMessages({
  library(bench)
})

.bench_env <- new.env(parent = emptyenv())
.bench_env$rows <- list()

bench_reset <- function() {
  .bench_env$rows <- list()
  invisible(NULL)
}

# Run a zero-arg thunk `iterations` times via bench::mark and append a row
# to the in-memory results buffer. `name` is the row identifier (and the
# join key against baseline.csv), so it must be stable across runs.
# The thunk closes over its definition env, which avoids the parent.frame()
# scoping ambiguity that bench::mark's internal evaluator would otherwise hit.
bench_run <- function(name, thunk, iterations = 3L) {
  stopifnot(is.function(thunk), length(formals(thunk)) == 0L)
  b <- bench::mark(
    thunk(),
    iterations = iterations,
    check      = FALSE,
    filter_gc  = FALSE,
    memory     = TRUE
  )
  row <- data.frame(
    name           = name,
    median_seconds = as.numeric(b$median[[1]]),
    mem_alloc_mb   = as.numeric(b$mem_alloc[[1]]) / 1024^2,
    n_gc           = as.integer(b$n_gc[[1]]),
    iterations     = as.integer(iterations),
    stringsAsFactors = FALSE
  )
  .bench_env$rows[[length(.bench_env$rows) + 1L]] <- row
  cat(sprintf(
    "  %-50s %7.3fs   %8.1f MB\n",
    name, row$median_seconds, row$mem_alloc_mb
  ))
  invisible(row)
}

bench_results <- function() {
  do.call(rbind, .bench_env$rows)
}
