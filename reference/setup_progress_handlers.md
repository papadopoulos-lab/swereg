# Install a progressr handler for interactive R and for job logs

Sets `progressr::handlers(global = TRUE)` and installs a handler chosen
by [`interactive()`](https://rdrr.io/r/base/interactive.html). An
interactive session gets a repainting progress bar; a job log gets one
plain line per interval.

## Usage

``` r
setup_progress_handlers()
```

## Value

Invisibly returns `NULL`.

## Details

- **Interactive sessions** (normal R console, RStudio foreground
  console):
  [`progressr::handler_progress()`](https://progressr.futureverse.org/reference/handler_progress.html)
  with `clear = TRUE`. The bar repaints in place and disappears when the
  run finishes.

- **Non-interactive sessions** (Rscript, Slurm, CI, RStudio background
  jobs spawned via *Source as Background Job* /
  [`rstudioapi::jobRunScript()`](https://rstudio.github.io/rstudioapi/reference/jobRunScript.html)):
  one plain line on stderr, at most one per interval, plus one line when
  the progression finishes. The handler writes no carriage return, so
  `grep` and `tail` stay usable on the log file.

A non-interactive line looks like this:

    2026-09-09T14:03:12 123/2194 (5.6%) elapsed 00:12:34 last: batch_00123

Set the interval in seconds with
`options(swereg.progress_interval_s = ...)`. The default is 600 seconds.

Also forces `options("progressr.enable" = TRUE)` so progressr emits
signals in non-interactive sessions. Without this, every `progressor()`
emission is silently dropped in a background job and no progress ever
appears.

## Examples

``` r
if (FALSE) { # \dontrun{
swereg::setup_progress_handlers()
study$process_skeletons(skeleton_create, n_workers = 4L)
} # }
```
