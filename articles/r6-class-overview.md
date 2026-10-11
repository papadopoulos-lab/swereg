# R6 class overview: who does what

## R6 class overview

swereg uses R6 classes for every piece of state that lives across
function calls. That state includes the skeleton pipeline, the per-batch
skeleton files, the trial schema, the per-enrollment data container and
the ETT grid builder. This vignette describes all six classes and where
each one fits in the data flow.

It stays at a high level. For the framework, trim, codes and randvars
mechanics see
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md).
For the full TTE workflow see
[`vignette("tte-workflow")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md).
For the reference page of each class see
[`?CandidatePath`](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md),
[`?RegistryStudy`](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md),
[`?Skeleton`](https://papadopoulos-lab.github.io/swereg/reference/Skeleton.md),
[`?TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
[`?TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md)
and
[`?TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md).

### The six classes at a glance

| Class               | What it owns                                                                                                                                             | Created by                                                                                                                                                                       | Persisted?                                                                           |
|---------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------|
| **`CandidatePath`** | An ordered list of filesystem paths where a directory might live on different hosts, plus a cached resolution.                                           | `$new(candidates, label)`                                                                                                                                                        | No. swereg clears the cache on save, so objects are portable.                        |
| **`RegistryStudy`** | The full skeleton pipeline: directories, batch config, the four-phase pipeline (framework / trim / codes / randvars), and the per-batch processing loop. | `RegistryStudy$new(...)` in the runner script.                                                                                                                                   | Yes, as `registrystudy.qs2`.                                                         |
| **`Skeleton`**      | One batch’s person-week `data.table` plus its phase-provenance (framework hash, trim identity, phase order, applied randvars, applied code entries).     | Implicitly by `RegistryStudy$process_skeletons()`; accessed via `study$load_skeleton(i)`.                                                                                        | Yes, one `skeleton_NNN.qs2` per batch.                                               |
| **`TTEDesign`**     | The column name schema for a trial (id, treatment, outcome, confounder, time).                                                                           | `TTEDesign$new(...)` from the spec during [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md). | Yes, embedded in `TTEPlan` / `TTEEnrollment`.                                        |
| **`TTEEnrollment`** | One sequence of sequential trials: the data, the design and the lifecycle state.                                                                         | `TTEEnrollment$new(data, design, ratio = ...)` during Loop 1.                                                                                                                    | Yes, as `file_raw` and `file_imp` (after Loop 1) and `file_analysis` (after Loop 2). |
| **`TTEPlan`**       | The ETT grid (one row per outcome × follow-up × age group), references back to the `RegistryStudy`, and the two loops that produce analysis files.       | `tteplan_from_spec_and_registrystudy(spec, study)`.                                                                                                                              | Yes, as `tteplan.qs2`.                                                               |

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

The picture shows two properties:

1.  `CandidatePath` is a leaf: other classes *hold* instances of it, but
    `CandidatePath` holds nothing from swereg. It is the single type for
    “multi-host directory” knowledge, so its resolution behavior cannot
    drift between users.

2.  `TTEPlan` holds a reference back to the `RegistryStudy` it was built
    against. That reference tells a Loop 1 worker where the skeleton
    files live, through `study$load_skeleton(i)`. It also lets the plan
    assert every skeleton is pipeline-consistent before any expensive
    work starts.

### The data flow, in seven steps

Data flows through seven shapes in a full swereg pipeline. Each shape
has a name, an owner, and a position in the workflow.

#### 1. Raw registry files (bytes on disk)

The original `.txt` / `.sas7bdat` / `.csv` files from SCB, SOS,
Riksstroke, etc. swereg does not own these. They are in
`study$data_raw_dir`, which is a `CandidatePath`-resolved location. The
runner script reads them with
[`swereg::fread_raw()`](https://papadopoulos-lab.github.io/swereg/reference/fread_raw.md)
and hands them to `RegistryStudy$save_rawbatch()`.

#### 2. Rawbatches (per-group-per-batch qs2 files)

A **rawbatch** is the intermediate form between raw registry files and
skeletons. It keeps one large `inpatient.txt` out of memory.
`$save_rawbatch("inpatient", sv)` splits the inpatient data by
person-batch, which is the set of person IDs assigned to batch 1, batch
2, and so on. It then writes one `.qs2` file per `(batch, group)` pair:

    /data/.../2026/rawbatch/
      001_rawbatch_inpatient.qs2
      001_rawbatch_outpatient.qs2
      001_rawbatch_lmed.qs2
      ...
      150_rawbatch_other.qs2

Rawbatches are not an R6 class. They are plain data.tables serialized
with qs2. `study$load_rawbatch(batch_number)` reads them and returns a
named list of data.tables keyed by group name. They exist as a separate
step for memory. Each group is loaded and processed separately, so peak
RAM during skeleton processing equals `max(group_size)` and not
`sum(all_groups)`.

`RegistryStudy` owns the rawbatch directory via its `data_rawbatch_cp`
field (a `CandidatePath`) and exposes `$save_rawbatch()`,
`$load_rawbatch()`, and `$delete_rawbatches()` methods.

#### 3. Skeletons (per-batch Skeleton R6 objects)

A **skeleton** is the person-week time grid for one batch, with derived
columns. Most of the pipeline state is in the skeleton. Each
`skeleton_NNN.qs2` file is a serialized `Skeleton` R6 object carrying:

- `data`: the underlying `data.table`.
- `framework_fn_hash`: the xxhash64 of the phase-1 function that built
  the base grid.
- `trim_fn_hash`: the identity of the phase-1b trim that deleted rows
  from that grid. A sentinel when the study registers no trim, and
  `NULL` when the skeleton file carries no trim identity.
- `phase_order`: the order the phases ran in. When the current pipeline
  runs the phases in a different order, swereg rebuilds the file and
  does not reuse it.
- `applied_registry`: a fingerprint-keyed map of the applied phase-2
  code entries, with the metadata needed to recompute which columns each
  entry wrote.
- `randvars_state`: a named ordered list of the applied phase-3 steps,
  with their hashes and the columns each one added.

`Skeleton` objects are rarely constructed directly. The normal lifecycle
is:

``` r
# Load from disk
sk <- study$load_skeleton(batch_number = 1L)

# Mutate via the methods on Skeleton itself or via RegistryStudy's
# sync helpers -- $sync_randvars() and $sync_with_registry().
# These are called implicitly by $process_skeletons().

# Save back to disk
study$save_skeleton(sk)
```

`RegistryStudy$process_skeletons()` orchestrates the four phases across
all batches, reading and writing `Skeleton` objects one per batch.
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md)
describes how phase invalidation works.

#### 4. Person-week panels (data.table inside a TTEEnrollment)

In Loop 1 sub-step s1d, each worker reads one skeleton and hands a
filtered person-week data.table for one enrollment to a fresh
`TTEEnrollment` object. Sub-step s1e then binds the panels of all
skeletons of that enrollment into one:

``` r
enrollment <- TTEEnrollment$new(
  data    = filtered_person_week_dt,
  design  = tte_design,
  ratio   = 2
)
enrollment$data_level  # "person_week"
```

At this point `$data` is still one row per person per ISO week. The
`TTEDesign` object supplies the column name schema, and no enrollment
redefines it. The schema names the treatment column, the outcome column
and the confounder columns.

#### 5. Trial panels (data.table after `$enroll()`)

Passing `ratio` to `TTEEnrollment$new()` triggers `$enroll()`
automatically. This step turns person-week rows into **counting-process
trial rows**. Each trial row is one person, one trial and one follow-up
period, and it carries `tstart` / `tstop` columns in the Andersen-Gill
style. The comparator draw takes comparator individuals by incidence
density sampling within each enrollment period. It takes
`comparator_to_intervention_ratio` times that period’s count of
intervention individuals, or every remaining comparator when the
enrollment period holds fewer. The draw is seeded and stratified by the
enrollment period. It reads no other variable, and it forms no matched
set.

``` r
enrollment$data_level  # "trial"
# now has: enrollment_period_id, period_id, tstart, tstop, intervention, ...
```

The remaining Loop 1 steps work on this trial panel:

``` r
# impute_fn at trial entry; the default draws one hot-deck value
enrollment$s1_impute_confounders(confounder_vars = entry_confounder_cols)
enrollment$s1b_fill_followup_confounders()  # carry forward through follow-up
enrollment$s2_ipw()                         # stabilized logistic IPW
enrollment$s3_truncate_weights()            # winsorize at 1/99 percentiles
```

`$enrollment_stage` reads `"enrolled"` as soon as `$data_level` becomes
`"trial"`, which the comparator draw in `$new(ratio = )` does. It does
not track the numbered steps. Loop 1 serializes the result as
`file_imp`, one per `enrollment_id`.

#### 6. Analysis panels (one per ETT)

Loop 2 reloads each `file_imp`, prepares an outcome column, fits
IPCW-PP, combines weights, and saves the result as `file_analysis`:

``` r
enrollment <- swereg::qs2_read(x_file_imp)
enrollment$s4_prepare_for_analysis(
  outcome   = x_outcome,
  follow_up = x_follow_up_weeks
)
# enrollment$enrollment_stage -> "analysis_ready"
# analysis_weight_pp = ipw * ipcw_pp (not truncated)
# analysis_weight_pp_trunc = analysis_weight_pp truncated at 1/99 percentiles
```

Loop 2 writes one `file_analysis` per row of `plan$ett`. Estimation
reads these files.

#### 7. Estimates (numbers)

``` r
enrollment <- swereg::qs2_read(x_file_analysis)
enrollment$irr(weight_col = "analysis_weight_pp_trunc")
enrollment$rates(weight_col = "analysis_weight_pp_trunc")
enrollment$survival_curve(weight_col = "analysis_weight_pp_trunc")
```

`$irr()`/`$rates()` wrap
[`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html) and
`$survival_curve()` computes a weighted discrete-time survival curve
from the panel. All three apply the weight column. `$irr()` also
includes the trial-as-covariate term and returns effect estimates with
person-level clustered standard errors.

### Class responsibilities in detail

#### `CandidatePath`: portable filesystem paths

`CandidatePath` owns an ordered list of filesystem paths and caches the
first one that exists on the current host. The cache is host-specific,
and swereg does not persist it across save and load.
\[invalidate_candidate_paths()\] walks an R6 object tree before
serialization and clears every `CandidatePath` cache it finds. A
`registrystudy.qs2` written on a Linux host therefore reads correctly on
a Windows host, by re-walking its candidate lists from scratch.

Construction takes the candidates and an optional label used in error
messages:

``` r
cp <- CandidatePath$new(
  candidates = c(
    "//shared-drive/registry/2026/rawbatch/",
    "/mnt/shared/registry/2026/rawbatch/",
    "C:/shared/registry/2026/rawbatch/"
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
`$resolve()` on the backing `CandidatePath`, so plain character access
returns the resolved path.

#### `RegistryStudy`: the skeleton pipeline

`RegistryStudy` holds the top-level state of the skeleton pipeline. Its
responsibilities fall into four groups:

1.  **Portable directories** via `CandidatePath` active bindings.
2.  **Batch configuration** (`set_ids()`, `batch_size`, the
    `batch_id_list`).
3.  **The declarative pipeline** (`register_framework()`,
    `register_trim()`, `register_codes()`, `register_derived_codes()`,
    `register_randvars()`). Only the trim may delete skeleton rows. A
    code entry or a randvars step that changes the row count stops the
    run.
4.  **The batch processing loop** (`process_skeletons()`) plus helpers
    (`load_rawbatch`, `save_rawbatch`, `load_skeleton`, `save_skeleton`,
    `pipeline_hash`, `assert_skeletons_consistent`).

`RegistryStudy$process_skeletons()` runs the pipeline. For each batch it
loads the rawbatch data, runs the four phases with incremental
invalidation, and saves a new `Skeleton` object to `data_skeleton_dir`.
When `n_workers > 1`, a batch-runner worker pool processes the batches
in parallel.

#### `Skeleton`: data and provenance of one batch

`Skeleton` is the on-disk unit produced by
`RegistryStudy$process_skeletons()`, one file per batch. The class holds
a `data` field with a data.table and five provenance fields that record
what produced it. The methods (`sync_with_registry`, `sync_randvars`,
`apply_code_entry`, `drop_code_entry`, `refresh_code_entry_counts`,
`pipeline_hash`) are invoked by `RegistryStudy` during processing and
can also be called manually for inspection or debugging.

The skeleton is an R6 object and not a plain `data.table`, because
swereg needs the provenance. Without it, only a full rebuild answers “is
this file in sync with the current pipeline?”. With it,
`sk$pipeline_hash() == study$pipeline_hash()` is a cheap check that
`RegistryStudy$assert_skeletons_consistent()` runs across every
persisted batch before Loop 1 consumes them.

#### `TTEDesign`: the trial schema

`TTEDesign` holds column names and nothing else. It names the person ID
column, the treatment column, the confounder columns, the time variables
(`time_treatment_var`, `time_outcome_var`) and the eligibility variable.

Enrollment data passes through many methods (`$s1_impute_confounders`,
`$s2_ipw`, `$s4_prepare_for_analysis`, `$rates`, `$irr`,
`$survival_curve`), and every one of them needs the same schema. The
design travels as a single R6 field on each `TTEEnrollment`. Nobody has
to pass column names around as arguments, and a column rename is a
one-line edit on the shared `TTEDesign`.

`TTEDesign` is constructed inside
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)
from the spec YAML, not by end users. A plan stores one `TTEDesign` per
distinct enrollment; `TTEEnrollment` instances reference the same
`TTEDesign` via their `$design` field.

#### `TTEEnrollment`: the workflow container

`TTEEnrollment` holds most of the TTE workflow state:

- `$data`: the data.table currently attached (person-week or trial or
  analysis-ready, depending on lifecycle stage).
- `$design`: a reference to the `TTEDesign` describing the schema.
- `$data_level`: `"person_week"` or `"trial"`. It tracks the major shape
  transition during enrollment.
- `$enrollment_stage`: `"pre_enrollment"` / `"enrolled"` /
  `"analysis_ready"`. It is a read-only active binding. It reads
  `"pre_enrollment"` while `$data_level` is `"person_week"`,
  `"analysis_ready"` once `$steps_completed` holds `"prepare_outcome"`,
  and `"enrolled"` otherwise.

Its public methods carry stage numbers that give the intended order:

``` r
# Loop 1
$s1_impute_confounders()          # impute_fn; default hot-deck draw
$s1b_fill_followup_confounders()  # carry forward through follow-up
$s2_ipw()                         # baseline stabilized IPW
$s3_truncate_weights()            # winsorize

# Loop 2
$s4_prepare_for_analysis()  # outcome + per-protocol censoring +
                            # IPCW-PP + weight combination
```

Methods mutate in place and return `invisible(self)` for `$`-chaining. A
skipped stage does not error. `$s2_ipw()` before
`$s1_impute_confounders()` fits the propensity model on the
person-trials with no missing confounder, and leaves `ipw` as `NA` on
the rest. `$s3_truncate_weights()` before `$s2_ipw()` warns
`No weight columns to truncate` and changes nothing. Run the stages in
their numbered order.

Estimation methods at the end:

``` r
$rates(weight_col = ...)
$irr(weight_col = ...)
$survival_curve(weight_col = ...)
$heterogeneity_test(weight_col = ...)
```

Each returns an estimate object, a `data.table` or a list, and mutates
nothing. `$survival_curve()` also saves a plot when `save_path` is
given.

#### `TTEPlan`: the ETT grid and two loops

`TTEPlan` is the top-level container for a target trial emulation
project. It owns:

- `$spec`: the parsed spec YAML.
- `$registrystudy`: the `RegistryStudy` the skeletons came from.
- `$ett`: the ETT grid (one row per outcome × follow-up combination ×
  enrollment_id, with `file_raw`, `file_imp`, and `file_analysis` path
  columns).
- Output directories via `CandidatePath` (`dir_tteplan`, `dir_results`).

Two methods run the workflow:

``` r
plan$s1_generate_enrollments_and_ipw(n_workers = 4L)
# Loop 1: enroll + collapse + impute + IPW + truncate + save
#         Five sub-steps. Each item runs in a fresh worker subprocess:
#         s1a  one item per skeleton,               n_workers at a time
#         s1b  one item, only when the spec has a sparse_rule
#         s1c  one item per enrollment,             one at a time
#         s1d  one item per enrollment x skeleton,  n_workers at a time
#         s1e  one item per enrollment,             one at a time

plan$s2_generate_analysis_files_and_ipcw_pp(n_workers = 4L)
# Loop 2: per-ETT outcome prep + IPCW-PP + weight combine + save
#         One worker subprocess per ETT and estimand (pp, itt),
#         n_workers at a time (default 1L).
```

In Loop 1, `n_workers` sets how many s1a and s1d items run at the same
time. It also sets the threads of each worker: the core count divided by
`n_workers`, rounded down, and at least 1. The default is
`default_n_workers("s1")`. That is the option `swereg.n_workers.s1`,
then the environment variable `SWEREG_N_WORKERS_S1`, then 1.

Plus helpers: `$print_target_checklist()` for reporting, `$save(dir)` /
`TTEPlan$load(dir)` for persistence, `$length()` / `$[[` S3 methods for
ETT grid access.

### Where the classes interact

The diagram below shows the pipeline from raw files to ETT estimates,
and the class that owns each step.

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
                  |                          // framework -> trim -> codes -> randvars
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
    plan$s1_generate_enrollments_and_ipw()   // Loop 1: s1a and s1d run
                  |                          // n_workers at a time. s1d builds
                  |                          // one TTEEnrollment per enrollment
                  |                          // x skeleton. s1e binds them per
                  |                          // enrollment_id and runs s1, s1b,
                  v                          // s2 and s3
           [file_imp per enrollment_id]
                  |
                  v
    plan$s2_generate_analysis_files_and_    // Loop 2: one subprocess per ETT
    ipcw_pp()                                // and estimand, n_workers at a time;
                  |                          // reloads TTEEnrollment, runs s4,
                  |                          // which runs s5, then s6 for pp
                  |
                  v
           [file_analysis per ETT]
                  |
                  v
    enrollment$irr(weight_col = ...)         // TTEEnrollment estimation
    enrollment$rates(weight_col = ...)       // methods, one weight column
    enrollment$survival_curve(weight_col = ...)
                  |
                  v
           [estimates]

Every step in the diagram above is either a class method call or a
persisted file. Rawbatches and skeletons are the only persisted forms
between raw data and the TTE workflow. `file_imp` and `file_analysis`
are persisted `TTEEnrollment` objects. The rest is in-memory state
during a pipeline run.

### When to construct each class by hand

End users of swereg *construct*:

- `CandidatePath`: almost never. Pass character vectors to
  `RegistryStudy$new()`, and it wraps them internally.
- `RegistryStudy`: once per data-generation project, at the top of the
  runner script.

End users of swereg **never construct**:

- `Skeleton`: produced by `process_skeletons()`, and read with
  `study$load_skeleton(i)`.
- `TTEDesign`: constructed by
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)
  from the spec YAML.
- `TTEEnrollment`: constructed by the Loop 1 / Loop 2 workers.
- `TTEPlan`: constructed by
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md).

The classes are internal machinery. The user works with the runner
script, the spec YAML and the per-ETT analysis loop. You construct a
`TTEEnrollment` by hand when you debug something or write a test.
Neither is the main workflow.

### Summary

- **Six R6 classes**, each with one job.
  - `CandidatePath`: directory resolution.
  - `RegistryStudy`: pipeline orchestrator.
  - `Skeleton`: per-batch data and provenance.
  - `TTEDesign`: trial schema.
  - `TTEEnrollment`: workflow container.
  - `TTEPlan`: ETT grid and two loops.
- **Rawbatches are not a class**. They are per-batch per-group qs2 files
  managed by `RegistryStudy$save_rawbatch()` and `$load_rawbatch()`.
  They bound peak RAM by the largest group, and not by the sum of all
  groups.
- **Data flow**: raw files → rawbatches → skeletons → person-week panel
  → trial panel → `file_imp` → `file_analysis` → estimates. At most
  steps, one class owns the transformation. At the edges (raw files,
  persisted qs2), files are plain data.tables.
- **End users construct `RegistryStudy` once**. Everything else is
  constructed by swereg internals.

For next steps:

- [`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md):
  how `RegistryStudy` and `Skeleton` work together during
  `$process_skeletons()`.
- [`vignette("tte-workflow")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md):
  how `TTEDesign`, `TTEEnrollment`, and `TTEPlan` work together during
  Loop 1 and Loop 2.
- [`vignette("tte-methodology")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methodology.md):
  mapping to reference papers.
- [`?CandidatePath`](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md),
  [`?RegistryStudy`](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md),
  [`?Skeleton`](https://papadopoulos-lab.github.io/swereg/reference/Skeleton.md),
  [`?TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md),
  [`?TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md),
  [`?TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md):
  full method reference.
