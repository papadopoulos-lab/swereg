# Performance benchmarks

Hand-run microbenchmarks for swereg's hot-path functions, with a checked-in
baseline so regressions are visible at review time.

## Usage

```bash
# 1. Run all benchmarks against the current source tree.
Rscript dev/bench/run_all.R

# 2. Compare the latest run to the checked-in baseline.
Rscript dev/bench/diff.R

# 3. If the new numbers are intentionally different (e.g. you sped something
#    up), update the baseline and commit it.
Rscript dev/bench/diff.R --accept
```

## Layout

| file                          | role                                                                 |
|-------------------------------|----------------------------------------------------------------------|
| `lib.R`                       | shared helpers (load pkg, run + record a benchmark)                  |
| `bench_<function>.R`          | one file per hot function; defines a `benchmark()` function          |
| `run_all.R`                   | sources all `bench_*.R`, runs them, writes `results/latest.csv`      |
| `diff.R`                      | compares `results/latest.csv` to `baseline.csv`; prints delta table  |
| `baseline.csv`                | checked-in reference numbers; updated only by deliberate `--accept`  |
| `results/<sha>_<utc>.csv`     | every run is also archived (gitignored) for local history            |

## Adding a new benchmark

1. Create `dev/bench/bench_my_function.R`.
2. Define `benchmark <- function() bench_run(name = "my_function/case", { ... })`
   where the `{ ... }` block is the code to time. `bench_run` is exported by
   `lib.R`.
3. Run `Rscript dev/bench/run_all.R` and commit the updated `baseline.csv`.

## Workflow conventions

- **Don't** auto-update `baseline.csv` on every run. Updates are reviewed.
- **Do** re-run on the same machine when comparing — these are wall-clock
  numbers, sensitive to load. Treat ~10% noise as the floor on a quiet box.
- **Do** keep individual benchmarks short (target: each finishes in <10 s).
  Long benchmarks discourage running them.
- The `diff.R` regression threshold is **20% slower** or **50% more memory**
  by default. Override with `--threshold-time=0.30 --threshold-mem=0.75`.
