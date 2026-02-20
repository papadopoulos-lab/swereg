# Run process_fn on each skeleton file via a pool of callr::r_bg() workers

Launches up to n_workers concurrent subprocesses. Each subprocess loads
data.table + swereg in a fresh R session (clean OpenMP state), reads one
skeleton file, applies process_fn, and returns the TTEEnrollment object.

## Usage

``` r
.tte_callr_pool(
  files,
  process_fn,
  enrollment_spec,
  n_workers,
  swereg_dev_path,
  p
)
```

## Arguments

- files:

  character vector of skeleton file paths

- process_fn:

  callback with signature \`function(enrollment_spec, file_path)\`

- enrollment_spec:

  list from \`\$enrollment_spec()\` with design, enrollment_id,
  age_range, n_threads

- n_workers:

  integer number of concurrent subprocesses

- swereg_dev_path:

  path to local swereg dev copy, or NULL

- p:

  progressor function from \[progressr::progressor()\]

## Value

list of TTEEnrollment objects (one per file, failures excluded with
warning)
