# Resolve the default number of parallel workers

Used as the default for \`n_workers\` across the heavy pipeline steps
(\[RegistryStudy\]'s \`save_rawbatch()\` / \`process_skeletons()\`, and
\[TTEPlan\]'s \`s1_generate_enrollments_and_ipw()\` / \`s3_analyze()\`).
Resolves, in order:

1.  \`getOption("swereg.n_workers")\`

2.  the \`SWEREG_N_WORKERS\` environment variable

3.  fallback \`max(1L, parallel::detectCores() - 2L)\`

Set the per-host value once (e.g. \`SWEREG_N_WORKERS\` in the host's
\`Renviron\`) and every step picks it up; pass \`n_workers\` explicitly
at the call site to override for a single run.

## Usage

``` r
default_n_workers()
```

## Value

Integer worker count (\>= 1).

## Details

Note: \`s2_generate_analysis_files_and_ipcw_pp()\` deliberately does NOT
use this — it stays single-worker (\`n_workers = 1L\`) for per-ETT
memory isolation, and should keep that default.
