# Resolve the default number of parallel workers for a pipeline stage

Used as the default for `n_workers` across the heavy pipeline steps.
Each step passes its own `stage` tag so worker counts can be tuned
independently — a memory-heavy stage never inherits a box-wide setting
meant for a light one. Resolution, in order:

1.  `getOption("swereg.n_workers.<stage>")`

2.  the `SWEREG_N_WORKERS_<STAGE>` environment variable (stage
    upper-cased)

3.  fallback `1L` (serial)

The default is **1 worker everywhere**; parallelism is opt-in, per
stage. For example, to run the enrollment/IPW loop with 3 workers but
keep the analysis loop serial, set `SWEREG_N_WORKERS_S1=3` and leave
`SWEREG_N_WORKERS_S3` unset. Pass `n_workers` explicitly at the call
site to override for a single run.

## Usage

``` r
default_n_workers(stage = NULL)
```

## Arguments

- stage:

  Optional character stage tag (e.g. `"s1"`, `"s3"`, `"skeleton"`,
  `"rawbatch"`). When `NULL`, no per-stage override is consulted and the
  function returns `1L`.

## Value

Integer worker count (\>= 1).

## Details

Stage tags currently used by the pipeline:

- `"rawbatch"`:

  [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)'s
  `save_rawbatch()`

- `"skeleton"`:

  [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)'s
  `process_skeletons()`

- `"s1"`:

  [TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)'s
  `s1_generate_enrollments_and_ipw()` (~6 GB/worker)

- `"s3"`:

  [TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)'s
  `s3_analyze()` — peaks ~20 GB/worker on large "vs none" panels, so
  keep at 1 unless you have the RAM headroom

Note: `s2_generate_analysis_files_and_ipcw_pp()` does not call this
function. Its `n_workers` argument defaults to `1L`. It accepts a larger
whole number, and s2 then runs that many concurrent subprocesses.

swereg reads no box-wide worker count. A single global knob could
silently leak a high worker count into a heavy stage (3 x ~20 GB s3
workers -\> OOM). When the deprecated `SWEREG_N_WORKERS` is set, the
function warns once per R session and ignores it. It reads no
`getOption("swereg.n_workers")`.
