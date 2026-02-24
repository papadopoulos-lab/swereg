# Run a function on each work item via a pool of callr::r_bg() workers

Launches up to \`n_workers\` concurrent subprocesses. Each subprocess
loads data.table + swereg in a fresh R session (clean OpenMP state),
then calls \`do.call(worker_fn, items\[\[i\]\])\`.

## Usage

``` r
tte_callr_pool(
  items,
  worker_fn,
  n_workers,
  swereg_dev_path = NULL,
  p = NULL,
  item_labels = NULL,
  collect = TRUE
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

## Value

If \`collect = TRUE\`, a list of results (failures excluded with
warning). If \`collect = FALSE\`, \`invisible(NULL)\`.
