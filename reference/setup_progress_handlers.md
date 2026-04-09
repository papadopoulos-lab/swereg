# Install a progressr handler that works in interactive R and RStudio jobs

Sets \`progressr::handlers(global = TRUE)\` and installs
\[progressr::handler_progress()\] with a format chosen based on
\`interactive()\`:

## Usage

``` r
setup_progress_handlers()
```

## Value

Invisibly returns \`NULL\`.

## Details

\* \*\*Interactive sessions\*\* (normal R console, RStudio foreground
console): single-line carriage-return repaint with \`clear = TRUE\`.
Same behavior as a normal terminal progress bar – updates in place,
disappears when the run finishes. \* \*\*Non-interactive sessions\*\*
(RStudio background jobs spawned via \*Source as Background Job\* /
\`rstudioapi::jobRunScript()\`, Rscript, CI): append a trailing newline
with \`clear = FALSE\`. Each step becomes a new line in the log and
finished bars stay in the scrollback. Carriage returns (which job logs
do not honor) are never emitted.

Also forces \`options("progressr.enable" = TRUE)\` so progressr emits
signals in non-interactive sessions – without this, every
\`progressor()\` emission is silently dropped in a jobRunScript
subprocess and no bar ever appears.

## Examples

``` r
if (FALSE) { # \dontrun{
swereg::setup_progress_handlers()
study$process_skeletons(skeleton_create, n_workers = 4L)
} # }
```
