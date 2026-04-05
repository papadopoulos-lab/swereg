# Run a function on each work item in parallel via processx

Dispatches items across \`n_workers\` concurrent R subprocesses. Each
item's arguments are saved to a qs2 tempfile and passed to a standalone
worker R script (in \`inst/\`) via command-line arguments. Results are
read back from qs2 output tempfiles. This avoids R's IPC serialization
overhead entirely.

## Usage

``` r
parallel_pool(
  items,
  worker_script,
  n_workers,
  swereg_dev_path = NULL,
  p = NULL,
  collect = TRUE,
  ...
)
```

## Arguments

- items:

  List of argument lists, one per work item. Each element is saved to a
  qs2 tempfile and read by the worker script.

- worker_script:

  Basename of the worker R script under \`inst/\` (e.g.
  \`"worker_s1a.R"\`).

- n_workers:

  Integer number of concurrent subprocesses.

- swereg_dev_path:

  Path to local swereg dev copy (for \`devtools::load_all()\`), or
  \`NULL\` to use installed swereg.

- p:

  Progressor function from \[progressr::progressor()\], or \`NULL\`.
  Called once per completed work item in the main process.

- collect:

  If \`TRUE\` (default), collect and return worker results from output
  tempfiles. If \`FALSE\`, discard (useful when workers save output
  directly to their final location).

- ...:

  Ignored (absorbs unused arguments from callers).

## Value

If \`collect = TRUE\`, a list of results. If \`collect = FALSE\`,
\`invisible(NULL)\`.
