# Benchmarks for create_skeleton().
# Naming convention: "<function>/<scenario>" so rows in baseline.csv
# read like a category tree.

benchmark_create_skeleton <- function() {
  cat("create_skeleton:\n")

  # Scaling sweep over N IDs, fixed ~11-year date range.
  date_min <- "2010-01-01"
  date_max <- "2020-12-31"
  for (N in c(1000L, 5000L, 10000L, 25000L)) {
    local({
      ids_local <- seq_len(N)
      dmin <- date_min
      dmax <- date_max
      bench_run(
        name  = sprintf("create_skeleton/N=%d_yrs=11", N),
        thunk = function() create_skeleton(ids_local, dmin, dmax)
      )
    })
  }

  # Long date range, modest N: stresses the time-spine path.
  local({
    ids_local <- seq_len(1000L)
    bench_run(
      name  = "create_skeleton/N=1000_yrs=30",
      thunk = function() create_skeleton(ids_local, "1990-01-01", "2020-12-31")
    )
  })

  # Single-day range: degenerate edge case.
  local({
    ids_local <- seq_len(10000L)
    bench_run(
      name  = "create_skeleton/N=10000_yrs=0",
      thunk = function() create_skeleton(ids_local, "2020-06-15", "2020-06-15")
    )
  })
}
