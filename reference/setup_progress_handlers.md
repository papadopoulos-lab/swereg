# Choose a progressr handler that works in the current launch context

Sets \`progressr::handlers(global = TRUE)\` and selects a sensible
handler based on whether the current R session can drive the RStudio
Jobs pane:

## Usage

``` r
setup_progress_handlers(
  format = ":bar :percent :elapsed / :eta (last: :message)"
)
```

## Arguments

- format:

  Format string passed to \[progressr::handler_progress()\] in the
  text-fallback branch. Ignored when the RStudio handler is chosen.

## Value

Invisibly returns \`NULL\`.

## Details

\* If \`rstudioapi\` is available and exposes \`jobAdd()\`,
\`jobSetProgress()\`, and \`jobRemove()\`, the "rstudio" handler is
installed (\[progressr::handler_rstudio()\]). Progress is drawn as a
virtual RStudio Jobs-pane bar via \`rstudioapi::jobAdd()\` /
\`rstudioapi::jobSetProgress()\`. This works both in the interactive
RStudio console and inside a background job launched via
\`rstudioapi::jobRunScript()\` / RStudio's "Source as Background Job"
menu, where the default text bar renders poorly in the job log. \*
Otherwise (plain R, \`Rscript\`, non-RStudio front-ends, RStudio Server
without the jobs API), falls back to \`progressr::handler_progress()\`
with the given \`format\`.

Intended to be called once at the top of a run script, replacing the
hand-rolled

“\`r progressr::handlers(global = TRUE)
progressr::handlers(progressr::handler_progress(format = "...")) “\`

boilerplate. Safe to call multiple times. Because the choice is made via
feature detection on \`rstudioapi\`, no changes are needed to swereg's
long-running methods (\`RegistryStudy\$process_skeletons\`,
\`TTEPlan\$s1_generate_enrollments_and_ipw\`,
\`TTEPlan\$s2_generate_analysis_files_and_ipcw_pp\`,
\`TTEPlan\$s3_analyze\`) — they all already emit
\`progressr::progressor()\` signals and automatically render through
whichever handler is active.

## Examples

``` r
if (FALSE) { # \dontrun{
swereg::setup_progress_handlers()
study$process_skeletons(skeleton_create, n_workers = 4L)
} # }
```
