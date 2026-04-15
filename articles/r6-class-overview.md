# R6 class overview: who does what

## R6 class overview

swereg uses R6 classes for every piece of state that lives across
function calls: the skeleton pipeline, the per-batch skeleton files
themselves, the trial schema, the per-enrollment data container, and the
ETT grid builder. This vignette is a top-down tour of all six classes
and a walk through where each one fits in the data flow.

It is deliberately high-level. For the framework / randvars / codes
mechanics see
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md);
for the full TTE workflow see
[`vignette("tte-workflow")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md);
for the `?ClassName` page of each class see
[`?CandidatePath`](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md),
[`?RegistryStudy`](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md),
[`?Skeleton`](https://papadopoulos-lab.github.io/swereg/reference/Skeleton.md),
[`?TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`?TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md),
[`?TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md).

### The six classes at a glance

| Class               | What it owns                                                                                                                                       | Created by                                                                                                                                                                       | Persisted?                                                                           |
|---------------------|----------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------|
| **`CandidatePath`** | An ordered list of filesystem paths where a directory might live on different hosts, plus a cached resolution.                                     | `$new(candidates, label)`                                                                                                                                                        | No – cache cleared on save so objects are portable.                                  |
| **`RegistryStudy`** | The full skeleton pipeline: directories, batch config, the three-phase pipeline (framework / randvars / codes), and the per-batch processing loop. | `RegistryStudy$new(...)` in the runner script.                                                                                                                                   | Yes, as `registrystudy.qs2`.                                                         |
| **`Skeleton`**      | One batch’s person-week `data.table` plus its phase-provenance (framework hash, applied randvars, applied code entries).                           | Implicitly by `RegistryStudy$process_skeletons()`; accessed via `study$load_skeleton(i)`.                                                                                        | Yes, one `skeleton_NNN.qs2` per batch.                                               |
| **`TTEDesign`**     | The column name schema for a trial (id, exposure, outcome, confounder, time).                                                                      | `TTEDesign$new(...)` from the spec during [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md). | Yes, embedded in `TTEPlan` / `TTEEnrollment`.                                        |
| **`TTEEnrollment`** | One sequence of sequential trials: the data + design + lifecycle state. Methods mutate in place and return `invisible(self)`.                      | `TTEEnrollment$new(data, design, ratio = ...)` during Loop 1.                                                                                                                    | Yes, as `file_raw` and `file_imp` (after Loop 1) and `file_analysis` (after Loop 2). |
| **`TTEPlan`**       | The ETT grid (one row per outcome × follow-up × age group), references back to the `RegistryStudy`, and the two loops that produce analysis files. | `tteplan_from_spec_and_registrystudy(spec, study)`.                                                                                                                              | Yes, as `tteplan.qs2`.                                                               |

### The dependency picture

                           +-------------------+
                           |   CandidatePath   |
                           +---------+---------+
                                     |
                    (used as fields on both R6 owners below)
                                     |
                       +-------------+-------------+
                       |                           |
              +--------v--------+         +--------v--------+
              |  RegistryStudy  |         |     TTEPlan     |
              +--------+--------+         +--------+--------+
                       | produces                  | references
                       v                           v
                 +-----+------+          +---------+---------+
                 |  Skeleton  |          |   TTEEnrollment   |
                 +------------+          +---------+---------+
                                                   |
                                                   | `$data`
                                                   v
                                         +---------+---------+
                                         |  trial panel      |
                                         |  (data.table)     |
                                         +-------------------+

Two things to notice:

1.  `CandidatePath` is a leaf: other classes *hold* instances of it, but
    `CandidatePath` doesn’t hold anything from swereg. That’s deliberate
    – it’s the single type for “multi-host directory” knowledge, so its
    resolution behavior cannot drift between users.

2.  `TTEPlan` holds a reference back to the `RegistryStudy` it was built
    against. That’s how Loop 1 workers know where the skeleton files
    live (via `study$load_skeleton(i)`) and how the plan can assert that
    all skeletons are pipeline-consistent before running anything
    expensive.

### The data flow, in seven steps

Data flows through five different shapes in a full swereg pipeline. Each
shape has a name, an owner, and a position in the workflow.

#### 1. Raw registry files (bytes on disk)

The original `.txt` / `.sas7bdat` / `.csv` files from SCB, SOS,
Riksstroke, etc. swereg does not own these – they sit in
`study$data_raw_dir`, which is a `CandidatePath`-resolved location. The
runner script reads them with
[`swereg::fread_raw()`](https://papadopoulos-lab.github.io/swereg/reference/fread_raw.md)
and hands them to `RegistryStudy$save_rawbatch()`.

#### 2. Rawbatches (per-group-per-batch qs2 files)

A **rawbatch** is the intermediate form between raw registry files and
skeletons. Instead of holding one giant `inpatient.txt` in memory,
`$save_rawbatch("inpatient", sv)` splits the inpatient data by
person-batch (the person IDs assigned to batch 1, batch 2, etc.) and
writes one `.qs2` file per `(batch, group)` pair:

    /data/.../2026/rawbatch/
      001_rawbatch_inpatient.qs2
      001_rawbatch_outpatient.qs2
      001_rawbatch_lmed.qs2
      ...
      150_rawbatch_other.qs2

Rawbatches are **not an R6 class**. They’re plain data.tables serialized
with qs2, accessed via `study$load_rawbatch(batch_number)` which returns
a named list of data.tables keyed by group name. The reason they exist
as a separate step is memory: peak RAM during skeleton processing equals
`max(group_size)` instead of `sum(all_groups)`, because each group is
loaded and processed separately.

`RegistryStudy` owns the rawbatch directory via its `data_rawbatch_cp`
field (a `CandidatePath`) and exposes `$save_rawbatch()`,
`$load_rawbatch()`, and `$delete_rawbatches()` methods.

#### 3. Skeletons (per-batch Skeleton R6 objects)

A **skeleton** is the person-week time grid for one batch, with derived
columns. This is where most of the interesting state lives. Each
`skeleton_NNN.qs2` file is a serialized `Skeleton` R6 object carrying:

- `data`: the underlying `data.table`.
- `framework_fn_hash`: the xxhash64 of the phase-1 function that built
  the base grid.
- `randvars_state`: a named ordered list of phase-3 steps that have been
  applied, with their hashes and the columns each one added.
- `applied_registry`: a fingerprint-keyed map of phase-2 code entries
  that have been applied, with the metadata needed to recompute which
  columns each entry wrote.

`Skeleton` objects are rarely constructed directly. The normal lifecycle
is:

``` r
# Load (from disk or auto-wrap from legacy bare data.table format)
sk <- study$load_skeleton(batch_number = 1L)

# Mutate via the methods on Skeleton itself or via RegistryStudy's
# sync helpers -- $sync_randvars() and $sync_with_registry().
# These are called implicitly by $process_skeletons().

# Save back to disk
study$save_skeleton(sk)
```

`RegistryStudy$process_skeletons()` orchestrates the three phases across
all batches, reading and writing `Skeleton` objects one per batch. For
the full story of how phase invalidation works see
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md).

#### 4. Person-week panels (data.table inside a TTEEnrollment)

At the start of Loop 1, the plan loads the right subset of skeletons and
hands a filtered person-week data.table to a fresh `TTEEnrollment`
object:

``` r
enrollment <- TTEEnrollment$new(
  data    = filtered_person_week_dt,
  design  = tte_design,
  ratio   = 2
)
enrollment$data_level  # "person_week"
```

At this point `$data` is still one row per person per ISO week. The
`TTEDesign` object supplies the column name schema – what’s the exposure
column called, what’s the outcome column called, what are the
confounders – without redefining it per enrollment.

#### 5. Trial panels (data.table after `$enroll()`)

Passing `ratio` to `TTEEnrollment$new()` triggers `$enroll()`
automatically. This is the step that turns person-week rows into
**counting-process trial rows**: one row per person per trial per
follow-up period, with `tstart` / `tstop` columns in the Andersen-Gill
style. Per-band stratified matching samples unexposed individuals at
`matching_ratio:1` within each enrollment band.

``` r
enrollment$data_level  # "trial"
# now has: trial_id, tstart, tstop, exposed, ...
```

The remaining Loop 1 steps work on this trial panel:

``` r
enrollment$s1_collapse()             # drop empty rows
enrollment$s2_impute_confounders()   # multiple imputation
enrollment$s3_ipw()                  # stabilized logistic IPW
enrollment$s4_truncate_weights()     # winsorize at 1/99 percentiles
```

After `$s4`, the enrollment’s `$enrollment_stage` transitions from
`"pre_enrollment"` to `"enrolled"`, and Loop 1 serializes it as
`file_imp` (one per `enrollment_id`).

#### 6. Analysis panels (one per ETT)

Loop 2 reloads each `file_imp`, prepares an outcome column, fits
IPCW-PP, combines weights, and saves the result as `file_analysis`:

``` r
enrollment <- swereg::qs2_read(x_file_imp)
enrollment$s5_prepare_for_analysis(
  outcome_var      = x_outcome,
  follow_up_weeks  = x_follow_up_weeks
)
# enrollment$enrollment_stage -> "analysis_ready"
# analysis_weight_pp = ipw * ipcw_pp (truncated)
```

One `file_analysis` per row of `plan$ett`. This is where estimation
happens.

#### 7. Estimates (numbers)

``` r
enrollment <- swereg::qs2_read(x_file_analysis)
enrollment$irr(weight_col = "analysis_weight_pp_trunc")
enrollment$rates()
enrollment$km()
```

These are wrappers around
[`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html) /
[`survey::svykm`](https://rdrr.io/pkg/survey/man/svykm.html) that apply
the weight column correctly, include the trial-as-covariate term, and
return effect estimates with person-level clustered standard errors.

### Class responsibilities in detail

#### `CandidatePath` – one thing only

`CandidatePath` owns an ordered list of filesystem paths and caches the
first one that exists on the current host. The cache is host-specific
and deliberately not persisted across save/load –
\[invalidate_candidate_paths()\] walks an R6 object tree before
serialization and clears every `CandidatePath` cache it finds, so a
`registrystudy.qs2` written on a Linux host reads correctly on a Windows
host by re-walking its candidate lists from scratch.

Construction takes the candidates and an optional label used in error
messages:

``` r
cp <- CandidatePath$new(
  candidates = c(
    "//argos/Bronze/Embla_data/_MHT/2026/rawbatch/",
    "/data/argos/Bronze/Embla_data/_MHT/2026/rawbatch/",
    "C:/Users/me/argos/Bronze/Embla_data/_MHT/2026/rawbatch/"
  ),
  label = "data_rawbatch_dir"
)
cp$resolve()     # first existing, cached
cp$is_resolved() # TRUE if cache still valid
cp$invalidate()  # clear cache
```

End users rarely construct `CandidatePath` directly. You pass a
character vector to `RegistryStudy$new(data_rawbatch_dir = ...)` and the
constructor wraps it internally. The active bindings
`study$data_rawbatch_dir`, `study$data_skeleton_dir`, etc. call
`$resolve()` on the backing `CandidatePath` so plain character access
“just works”.

#### `RegistryStudy` – the orchestrator

`RegistryStudy` is the top-level state-holder for the skeleton pipeline.
Its responsibilities split into four buckets:

1.  **Portable directories** via `CandidatePath` active bindings.
2.  **Batch configuration** (`set_ids()`, `batch_size`, the
    `batch_id_list`).
3.  **The declarative pipeline** (`register_framework()`,
    `register_randvars()`, `register_codes()`,
    `register_derived_codes()`).
4.  **The batch processing loop** (`process_skeletons()`) plus helpers
    (`load_rawbatch`, `save_rawbatch`, `load_skeleton`, `save_skeleton`,
    `pipeline_hash`, `assert_skeletons_consistent`,
    `write_pipeline_snapshot`).

`RegistryStudy$process_skeletons()` is the method that ties everything
together: for each batch, it loads the rawbatch data, runs the three
phases with incremental invalidation, and saves a new `Skeleton` object
to `data_skeleton_dir`. Parallelism is via a callr worker pool when
`n_workers > 1`.

#### `Skeleton` – per-batch provenance + data

`Skeleton` is the on-disk unit produced by
`RegistryStudy$process_skeletons()`. One file per batch. The class is
deliberately simple: a `data` field holding a data.table and three
provenance fields tracking what’s been applied. The methods
(`sync_with_registry`, `sync_randvars`, `apply_code_entry`,
`drop_code_entry`, `pipeline_hash`) are invoked by `RegistryStudy`
during processing and can also be called manually for inspection or
debugging.

The reason this isn’t a plain `data.table` is that the provenance is
load-bearing: without it you can’t answer “is this file in sync with the
current pipeline?” without a full rebuild. With it,
`sk$pipeline_hash() == study$pipeline_hash()` is a cheap check that
`RegistryStudy$assert_skeletons_consistent()` runs across every
persisted batch before Loop 1 consumes them.

#### `TTEDesign` – the trial schema

`TTEDesign` holds column names. That’s it. What’s the person ID column
called? What’s the exposure column? What are the confounder columns?
What are the time variables (`time_exposure_var`, `time_outcome_var`)?
What’s the eligibility variable?

Keeping this as a separate class is a discipline choice: enrollment data
gets passed through many methods (`$s1_collapse`, `$s2_ipw`,
`$s3_ipcw_pp`, `$rates`, `$irr`, `$km`) and every one of them needs the
same schema. Threading the design through as a single R6 field on each
`TTEEnrollment` means no one has to pass column names around as
arguments, and renaming a column is a one-line edit on the shared
`TTEDesign`.

`TTEDesign` is constructed inside
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)
from the spec YAML, not by end users. A plan stores one `TTEDesign` per
distinct enrollment; `TTEEnrollment` instances reference the same
`TTEDesign` via their `$design` field.

#### `TTEEnrollment` – the workflow container

`TTEEnrollment` is where most of the TTE complexity lives. It holds:

- `$data`: the data.table currently attached (person-week or trial or
  analysis-ready, depending on lifecycle stage).
- `$design`: a reference to the `TTEDesign` describing the schema.
- `$data_level`: `"person_week"` or `"trial"` – tracks the major shape
  transition during enrollment.
- `$enrollment_stage`: `"pre_enrollment"` / `"enrolled"` /
  `"analysis_ready"` – tracks Loop 1 / Loop 2 progress via an active
  binding that derives from internal state.

Its public methods fall into numbered stages so the intended ordering is
obvious:

``` r
# Loop 1
$s1_collapse()            # drop empty rows
$s2_impute_confounders()  # multiple imputation
$s3_ipw()                 # baseline stabilized IPW
$s4_truncate_weights()    # winsorize

# Loop 2
$s5_prepare_for_analysis()  # outcome + per-protocol censoring +
                            # IPCW-PP + weight combination
```

Estimation methods at the end:

``` r
$rates(weight_col = ..., by = ...)
$irr(weight_col = ..., formula = ...)
$km(weight_col = ...)
$heterogeneity_test()
```

Methods mutate in place and return `invisible(self)` for `$`-chaining.
Missing stages error clearly: if you try `$s3_ipw()` before
`$s2_impute_confounders()`, you get a message telling you so.

#### `TTEPlan` – the ETT grid + two loops

`TTEPlan` is the top-level container for a target trial emulation
project. It owns:

- `$spec`: the parsed spec YAML.
- `$registrystudy`: the `RegistryStudy` the skeletons came from.
- `$ett`: the ETT grid (one row per outcome × follow-up combination ×
  enrollment_id, with `file_raw`, `file_imp`, and `file_analysis` path
  columns).
- Output directories via `CandidatePath` (`dir_tteplan`, `dir_results`).

Its two big methods drive the parallel workflow:

``` r
plan$s1_generate_enrollments_and_ipw(n_workers = 4L)
# Loop 1: enroll + collapse + impute + IPW + truncate + save
#         One callr subprocess per enrollment_id.

plan$s2_generate_analysis_files_and_ipcw_pp()
# Loop 2: per-ETT outcome prep + IPCW-PP + weight combine + save
#         Sequential in the main process.
```

Plus helpers: `$print_target_checklist()` for reporting, `$save(dir)` /
`TTEPlan$load(dir)` for persistence, `$length()` / `$[[` S3 methods for
ETT grid access.

### Where the classes interact

Here’s the full pipeline from raw files to ETT estimates, with class
ownership highlighted at each step.

           [raw registry files]
                  |
                  v
    study$save_rawbatch(group, data)         // RegistryStudy owns
                  |
                  v
           [001_rawbatch_inpatient.qs2, ...]
                  |
                  v
    study$process_skeletons()                // RegistryStudy orchestrates
                  |                          // runs framework -> randvars -> codes
                  v                          // produces one Skeleton per batch
           [skeleton_001.qs2, ...]           // each file is a Skeleton R6
                  |
                  v
    tteplan_from_spec_and_registrystudy(     // reads spec + study, builds TTEPlan
      spec, study                            // creates one TTEDesign per enrollment
    )                                        //
                  |
                  v
           [TTEPlan with ett grid]
                  |
                  v
    plan$s1_generate_enrollments_and_ipw()   // Loop 1 in parallel
                  |                          // creates one TTEEnrollment per
                  v                          // enrollment_id, runs s1-s4
           [file_imp per enrollment_id]
                  |
                  v
    plan$s2_generate_analysis_files_and_    // Loop 2 sequential
    ipcw_pp()                                // reloads TTEEnrollment, runs s5
                  |
                  v
           [file_analysis per ETT]
                  |
                  v
    enrollment$irr() / $rates() / $km()      // TTEEnrollment estimation methods
                  |
                  v
           [estimates]

Every box in that diagram is either a class method call or a persisted
file. Rawbatches and skeletons are the only persisted forms between raw
data and the TTE workflow. `file_imp` and `file_analysis` are persisted
`TTEEnrollment` objects. The rest is in-memory state during a pipeline
run.

### When to construct each class by hand

End users of swereg *construct*:

- `CandidatePath` – essentially never; pass character vectors to
  `RegistryStudy$new()` instead and let it wrap internally.
- `RegistryStudy` – once per data-generation project, at the top of the
  runner script.

End users of swereg **never construct**:

- `Skeleton` – produced by `process_skeletons()`; accessed via
  `study$load_skeleton(i)`.
- `TTEDesign` – constructed by
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)
  from the spec YAML.
- `TTEEnrollment` – constructed by the Loop 1 / Loop 2 workers.
- `TTEPlan` – constructed by
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md).

This is intentional. The classes are plumbing; the user-facing surface
is the runner script, the spec YAML, and the per-ETT analysis loop. If
you find yourself constructing a `TTEEnrollment` by hand, you’re
probably debugging something or writing a test – both legitimate use
cases but not the main workflow.

### Summary

- **Six R6 classes**, each with one job: `CandidatePath` (directory
  resolution), `RegistryStudy` (pipeline orchestrator), `Skeleton`
  (per-batch data + provenance), `TTEDesign` (trial schema),
  `TTEEnrollment` (workflow container), `TTEPlan` (ETT grid + two
  loops).
- **Rawbatches are not a class**: they’re per-batch per-group qs2 files
  managed by `RegistryStudy$save_rawbatch()` / `$load_rawbatch()`,
  existing so peak RAM is bounded by the largest group rather than the
  sum.
- **Data flow**: raw files → rawbatches → skeletons → person-week panel
  → trial panel → `file_imp` → `file_analysis` → estimates. At most
  steps, one class owns the transformation; at the edges (raw files,
  persisted qs2), files are plain data.tables.
- **End users construct `RegistryStudy` once**. Everything else is
  constructed by swereg internals.

For next steps:

- [`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md)
  – how `RegistryStudy` and `Skeleton` collaborate during
  `$process_skeletons()`.
- [`vignette("tte-workflow")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md)
  – how `TTEDesign`, `TTEEnrollment`, and `TTEPlan` collaborate during
  Loop 1 and Loop 2.
- [`vignette("tte-methodology")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methodology.md)
  – mapping to reference papers.
- [`?CandidatePath`](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md),
  [`?RegistryStudy`](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md),
  [`?Skeleton`](https://papadopoulos-lab.github.io/swereg/reference/Skeleton.md),
  [`?TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
  [`?TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md),
  [`?TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)
  – full method reference.
