# Run process_fn on each skeleton file via a pool of callr::r_bg() workers

Launches up to n_workers concurrent subprocesses. Each subprocess loads
data.table + swereg in a fresh R session (clean OpenMP state), reads one
skeleton file, applies process_fn, and returns the TTETrial object.

## Usage

``` r
.tte_callr_pool(
  files,
  process_fn,
  design,
  file_id,
  age_range,
  n_threads,
  n_workers,
  swereg_dev_path
)
```

## Value

list of TTETrial objects (one per file, failures excluded with warning)
