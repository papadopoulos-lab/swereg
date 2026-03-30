# Run a function on each work item via a pool of persistent callr sessions

Creates \`n_workers\` persistent \[callr::r_session\] workers, loads the
swereg namespace once per worker, then dispatches items across workers
as they become idle. This avoids the per-item startup cost of
\[callr::r_bg()\].

## Usage

``` r
callr_pool(
  items,
  worker_fn,
  n_workers,
  swereg_dev_path = NULL,
  p = NULL,
  item_labels = NULL,
  collect = TRUE,
  timeout_minutes = 30
)
```

## Arguments

- items:

  List of argument lists, one per work item. Each element is passed to
  \`worker_fn\` via \[do.call()\].

- worker_fn:

  Function to call in each subprocess. Its signature must match the
  names in each element of \`items\`.

- n_workers:

  Integer number of concurrent subprocesses.

- swereg_dev_path:

  Path to local swereg dev copy (for \`devtools::load_all()\`), or
  \`NULL\` to use installed swereg.

- p:

  Progressor function from \[progressr::progressor()\], or \`NULL\`.

- item_labels:

  Character vector of labels for error messages (same length as
  \`items\`). Defaults to \`"1"\`, \`"2"\`, etc.

- collect:

  If \`TRUE\` (default), collect and return worker results. If
  \`FALSE\`, discard results (useful when workers save output directly).

- timeout_minutes:

  Numeric. Maximum minutes a single work item may run before the worker
  is killed and respawned with the same item. A timed-out item is
  retried once; if it times out again, \`callr_pool()\` calls
  \[stop()\]. Set to \`NULL\` to disable. Default: 30.

## Value

If \`collect = TRUE\`, a list of results (failures excluded with
warning). If \`collect = FALSE\`, \`invisible(NULL)\`.

## Details

On entry, kills any orphaned workers from previous crashed runs
(detected via PID files in \`/tmp\`).
