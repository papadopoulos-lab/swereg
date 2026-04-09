# Install a progressr handler that works everywhere, including RStudio jobs

Sets \`progressr::handlers(global = TRUE)\` and installs
\[progressr::handler_progress()\] with a format that renders correctly
in every context: interactive R consoles, RStudio's foreground console,
and RStudio background-job subprocesses spawned via \*Source as
Background Job\* / \`rstudioapi::jobRunScript()\`. The two critical
details that make this work in job logs are:

## Usage

``` r
setup_progress_handlers()
```

## Value

Invisibly returns \`NULL\`.

## Details

\* \*\*Trailing newline in the format string\*\* – each update prints as
a new line instead of a carriage-return repaint. Job logs do not honor
the carriage return, so without a trailing newline every update
overwrites nothing and you end up with a wall of mangled partial bars.
\* \*\*\`clear = FALSE\`\*\* – keeps finished bars in the log scrollback
instead of erasing them, so you can scroll back and see the full run
history.

This is the same recipe used in \`cs9::set_progressr\` and inside
\`plnr::Plan\$run_all\*\`, which have been battle-tested across both
interactive and background-job contexts.

Intended to be called once at the top of a run script, replacing the
hand-rolled \`handlers(global = TRUE) +
handlers(handler_progress(...))\` boilerplate. Safe to call multiple
times.

## Examples

``` r
if (FALSE) { # \dontrun{
swereg::setup_progress_handlers()
study$process_skeletons(skeleton_create, n_workers = 4L)
} # }
```
