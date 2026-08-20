# swereg

**swereg** builds longitudinal person-week skeletons from Swedish
healthcare registry data and runs target trial emulations on top of
them. Its centerpiece is an incrementally-rebuildable four-phase
skeleton pipeline.

## What swereg does

- **Builds person-week skeletons** from NPR (inpatient/outpatient),
  LMED (prescriptions), DORS (cause of death), LISA (demographics),
  cancer registry, and quality registries. Skeleton rows are one per
  person per ISO week, with derived columns for diagnoses,
  medications, operations, and time-varying confounders.
- **Orchestrates a declarative four-phase pipeline** (framework /
  trim / codes / randvars) where each phase has its own
  fingerprint-based invalidation strategy. An edit to one phase-2
  code entry drops that entry's columns on all persisted skeletons
  and re-applies only that entry. An edit to one phase-3 randvars
  step triggers a divergence-point rewind-and-replay of that step
  and everything downstream of it. An edit to the framework or to
  the trim rebuilds from scratch. Phase 1b, the trim, is the one
  phase that MAY delete skeleton rows.
- **Runs target trial emulations** (Hernan & Robins 2016 / Danaei
  2013 style) via a YAML spec file + an R6 `TTEPlan` that builds an
  ETT grid, runs a parallel Loop 1 (enrollment + baseline IPW) and a
  sequential Loop 2 (per-protocol censoring + IPCW-PP), and produces
  per-ETT analysis files ready for weighted Poisson IRR estimation.
- **Survives multi-host deployments** by storing directory
  candidates in `CandidatePath` fields that resolve lazily per host,
  so the same `registrystudy.qs2` file works across Linux and
  Windows mounts of the same shared data drive without edits.

## The six R6 classes

| Class | Role |
|---|---|
| [`CandidatePath`](reference/CandidatePath.html) | Ordered list of filesystem paths with host-specific caching. Used by the other classes to own their multi-host directory knowledge. |
| [`RegistryStudy`](reference/RegistryStudy.html) | Top-level pipeline orchestrator: portable directories, batch splitting, declarative four-phase pipeline (framework / trim / codes / randvars), per-batch processing. |
| [`Skeleton`](reference/Skeleton.html) | One persisted batch's person-week data.table plus its phase provenance (framework hash, trim hash, phase order, applied randvars, applied code registry fingerprints). |
| [`TTEDesign`](reference/TTEDesign.html) | The column name schema for a trial (ID, treatment, outcome, confounders, time variables). |
| [`TTEEnrollment`](reference/TTEEnrollment.html) | One sequence of sequential trials with data + design + lifecycle state. Mutating methods return `invisible(self)` for `$`-chaining. |
| [`TTEPlan`](reference/TTEPlan.html) | The ETT grid (one row per outcome × follow-up × enrollment_id) plus the two loops that produce analysis files. |

For a top-down tour with a data-flow diagram see
`vignette("r6-class-overview")`.

## Installation

```r
# Recommended: pak (fast, lightweight)
# install.packages("pak")
pak::pak("papadopoulos-lab/swereg")

# Alternative: devtools
# install.packages("devtools")
devtools::install_github("papadopoulos-lab/swereg")
```

### Optional: CONSORT flowcharts

`TTEPlan$export_tables()` renders CONSORT participant-flow diagrams
via Graphviz. To enable the flowchart path (otherwise the workbook
falls back to a text-table CONSORT), install three optional R
packages and one system library:

```r
install.packages(c("DiagrammeR", "DiagrammeRsvg", "rsvg"))
```

```bash
sudo apt-get install librsvg2-dev   # Debian/Ubuntu
sudo dnf install librsvg2-devel     # Fedora/RHEL
brew install librsvg                # macOS
```

## Small example: a hand-rolled skeleton

If you just want to play with the core concept without setting up a
full production pipeline, the low-level functions work on their own.
This is the right entry point for learning, for prototyping, and for
small ad-hoc analyses.

```r
library(data.table)
data("fake_person_ids", package = "swereg")
data("fake_demographics", package = "swereg")
data("fake_diagnoses", package = "swereg")

# 1. Build a person-week time grid for 100 people
skeleton <- swereg::create_skeleton(
  ids      = fake_person_ids[1:100],
  date_min = "2020-01-01",
  date_max = "2022-12-31"
)

# 2. Normalize column names + parse dates
swereg::make_lowercase_names(fake_demographics, date_columns = "fodelseman")
swereg::make_lowercase_names(fake_diagnoses,    date_columns = "indatum")

# 3. Attach demographics (one row per person) and diagnoses
swereg::add_onetime(
  skeleton,
  fake_demographics[lopnr %in% fake_person_ids[1:100]],
  id_name = "lopnr"
)
swereg::add_diagnoses(
  skeleton,
  fake_diagnoses[lopnr %in% fake_person_ids[1:100]],
  id_name = "lopnr",
  codes = list(
    diabetes   = c("E10", "E11"),
    depression = c("F32", "F33")
  )
)

head(skeleton)
```

For the full manual walkthrough see `vignette("skeleton-create")`.

## Production example: four-phase `RegistryStudy` pipeline

Real projects use `RegistryStudy` as the top-level orchestrator.
The pipeline is *declared* on the study via four families of
registration calls before `$process_skeletons()` runs. On the
first run it builds everything; on subsequent runs it only re-runs
the phases that changed.

```r
study <- swereg::RegistryStudy$new(
  data_rawbatch_dir = c(
    "/mnt/shared/registry/2026/rawbatch/",
    "C:/shared/registry/2026/rawbatch/"
  ),
  data_skeleton_dir = c(
    "/mnt/shared/registry/2026/skeleton/",
    "C:/shared/registry/2026/skeleton/"
  ),
  group_names = c("lmed", "inpatient", "outpatient", "cancer", "dors")
)

# Phase 1 -- framework: build the base time grid + structural censoring
study$register_framework(my_framework_fn)

# Phase 1b -- trim: the one phase that MAY delete skeleton rows.
# A study registers at most one. It runs on a fresh base, before the
# code registry, and returns a data.table. The skeleton it receives
# holds framework columns only.
study$register_trim(function(skeleton, batch_data, config) {
  skeleton[isoyear >= 2010]
})

# Phase 2 -- codes: declarative primary + derived registrations
study$register_codes(
  codes      = ICD10_CODES,
  fn         = swereg::add_diagnoses,
  groups     = list(ov = "outpatient", sv = "inpatient"),
  combine_as = "os"
)
study$register_codes(
  codes   = ICD10_CODES,
  fn      = swereg::add_cods,
  fn_args = list(cod_type = "underlying"),
  groups  = list(dorsu = "dors")
)
study$register_codes(
  codes   = ICD10_CODES,
  fn      = swereg::add_cods,
  fn_args = list(cod_type = "multiple"),
  groups  = list(dorsm = "dors")
)
# Build osd_<nm> := os_<nm> | dorsu_<nm> | dorsm_<nm>
# (the combined outcome column the TTE spec consumes)
study$register_derived_codes(
  codes = ICD10_CODES,
  from  = c("os", "dorsu", "dorsm"),
  as    = "osd"
)

# Phase 3 -- randvars: ordered user-supplied steps
# (registration order = execution order). They run after the code
# registry, so a step MAY read a code column. A step that changes the
# row count stops the run.
study$register_randvars("demographics", my_demographics_fn)
study$register_randvars("exposure",     my_exposure_fn)

# Every registration is in. First run: build everything.
# Subsequent runs: incremental.
study$process_skeletons(n_workers = 4L)

# Per-batch provenance verification
sk <- study$load_skeleton(1L)
sk$pipeline_hash() == study$pipeline_hash()   # FALSE => definitely stale
study$assert_skeletons_consistent()           # errors on mixed state
```

Hash equality is necessary for a synced skeleton. It is not
sufficient. Unequal hashes mean the skeleton is definitely stale.
Equal hashes mean only that nothing changed among the inputs both
hashes cover. The rawbatch data sits outside both hashes, and so
does whatever a registered function calls or reads from its
environment.

The `register_derived_codes()` call is how swereg expresses "the
combined `osd_*` column should OR together outputs from the OV/SV
`add_diagnoses` registration and both flavors of `add_cods` on DORS".
This can't be done with `combine_as` alone because that runs the
same `fn` on rbind data -- `add_diagnoses` searches `hdia`/`dia*` and
`add_cods` searches `ulorsak`/`morsak*`, so they need different
registrations. Derived fingerprints cascade upstream primary edits
(e.g. flipping `cod_type = "underlying"` to `"both"`) so the
`osd_*` columns re-compute correctly without the user touching the
derived entry.

See `vignette("skeleton-pipeline")` for the full mechanics, including
the divergence-point rewind-and-replay semantics for phase 3 and the
per-entry fingerprint diff for phase 2.

## Target trial emulation

Given a built skeleton pipeline and a study spec YAML, swereg can
run the full causal-inference workflow:

```r
plan <- swereg::tteplan_from_spec_and_registrystudy(
  spec  = "my_study/spec.yaml",
  study = study
)

# Loop 1: per-enrollment_id in parallel worker subprocesses
#   enroll + collapse + impute + IPW + truncate -> file_imp
plan$s1_generate_enrollments_and_ipw(n_workers = 4L)

# Loop 2: per-ETT sequential in the main process
#   outcome prep + per-protocol censoring + IPCW-PP
#   + analysis_weight_pp = ipw * ipcw_pp -> file_analysis
plan$s2_generate_analysis_files_and_ipcw_pp()

# Per-ETT estimation
for (i in seq_len(nrow(plan$ett))) {
  enrollment <- swereg::qs2_read(plan$ett$file_analysis[i])
  enrollment$irr(weight_col = "analysis_weight_pp_trunc")
  enrollment$rates()
  enrollment$km()
}
```

The spec YAML captures every clinical and methodological decision
in a file that medical collaborators can review: inclusion /
exclusion criteria with rationales and time windows, confounders
(including rolling-window computed ones), outcomes, follow-up
durations, and per-enrollment treatment definitions with matching
ratios.

For the full TTE walkthrough (epi rationale + R6 mechanics + spec
anatomy + the two loops + estimation) see
`vignette("tte-workflow")`.

## Learning path

swereg's vignettes are meant to be read in roughly this order. The
grouping matches the pkgdown Articles menu on the package website.

**Concept** -- build a mental model before touching anything:

1. `vignette("skeleton-concept")` -- the person-week time grid and
   why it beats ad-hoc wide-format builds.
2. `vignette("r6-class-overview")` -- top-down tour of the six R6
   classes (`CandidatePath`, `RegistryStudy`, `Skeleton`,
   `TTEDesign`, `TTEEnrollment`, `TTEPlan`) with a data-flow
   diagram showing who owns which step.
3. `vignette("rowdep-rowind-concept")` -- `rd_` (row-dependent)
   vs `ri_` (row-independent) variable prefix conventions and
   `rd_` -> `ri_` transformation patterns.

**Pipeline** -- the two production workflows:

4. `vignette("skeleton-pipeline")` -- the four-phase skeleton
   pipeline (framework / trim / codes / randvars), `Skeleton` R6 class,
   incremental invalidation, derived codes, `pipeline_hash`
   provenance, reload anti-pattern fix.
5. `vignette("tte-workflow")` -- target trial emulation from spec
   YAML to per-ETT estimates, epi rationale and R6 mechanics on
   the same page.

**TTE reference** -- dive-deep material for TTE users after
reading the workflow:

6. `vignette("tte-nomenclature")` -- one-page glossary of TTE
   terms used throughout swereg.
7. `vignette("tte-methodology")` -- mapping to reference papers
   (Hernan 2008/2016, Danaei 2013, Caniglia 2023, TARGET 2025).

**Manual workflow** -- the low-level entry point for small ad-hoc
analyses that don't need the four-phase pipeline:

8. `vignette("skeleton-create")` -- build a skeleton by hand
   with `create_skeleton()` + `add_diagnoses()` + derived variables.
9. `vignette("skeleton-analyze")` -- collapse to person-years and
   run analyses on the finished skeleton.

**Cookbooks** -- worked end-to-end examples:

11. `vignette("cookbook-survival-analysis")` -- survival analysis
    walkthrough.

## Core API quick reference

### Four-phase pipeline (production)

The list is in execution order.

- `RegistryStudy$new(...)` -- constructor taking candidate
  directory lists + batch config.
- `$register_framework(fn)` -- phase 1: base time grid + censoring.
- `$register_trim(fn)` -- phase 1b: the one phase that MAY delete
  skeleton rows. A study registers at most one.
- `$register_codes(codes, fn, groups, fn_args, combine_as)` --
  phase 2: primary code registrations (`add_diagnoses`,
  `add_cods`, `add_operations`, `add_rx`,
  `add_cancer_without_morphology`, `add_quality_registry`).
- `$register_derived_codes(codes, from, as)` -- phase 2: derived
  registrations that OR together upstream primary columns.
- `$register_randvars(name, fn)` -- phase 3: ordered derived
  variables (demographics, exposure classification, exclusions).
  A step MAY read a phase-2 code column.
- `$process_skeletons(batches, n_workers)` -- run all four phases
  with incremental invalidation.
- `$load_skeleton(i)` / `$save_skeleton(sk)` / `$pipeline_hash()` /
  `$assert_skeletons_consistent()` -- skeleton I/O + provenance.
- `$adopt_runtime_state_from(other)` -- safe reload pattern (never
  overwrite `study <- qs2_read(...)` the naive way).

### Low-level data integration (hand-rolled)

- `create_skeleton()` -- build the person-week time grid.
- `make_lowercase_names()` -- normalize column names + parse
  Swedish dates (4/6/8-character precision).
- `add_onetime()` -- baseline/demographic data (one row per person).
- `add_annual()` -- annual data for specific years.
- `add_diagnoses()` -- NPR diagnosis code matching.
- `add_operations()` -- NPR surgical procedure codes.
- `add_rx()` -- LMED prescription data with ATC / product codes.
- `add_cods()` -- DORS cause of death (`cod_type = "underlying"` /
  `"multiple"` / `"both"`).
- `add_cancer_without_morphology()` -- cancer registry topography codes
  (site only), searching both `icdo10` and `icdo3`.
- `add_quality_registry()` -- Riksstroke and similar quality
  registries with structured-field filters.

### Target trial emulation

- `tteplan_from_spec_and_registrystudy(spec, study)` -- build a
  `TTEPlan` from a spec YAML and a built `RegistryStudy`.
- `tteplan_read_spec(path)` -- parse + validate a spec YAML.
- `TTEPlan$s1_generate_enrollments_and_ipw()` -- Loop 1 (enroll +
  IPW).
- `TTEPlan$s2_generate_analysis_files_and_ipcw_pp()` -- Loop 2
  (outcome + IPCW-PP).
- `TTEEnrollment$rates()` / `$irr()` / `$km()` /
  `$heterogeneity_test()` -- per-ETT estimation.
- `TTEPlan$print_target_checklist()` -- pre-populated TARGET 2025
  reporting checklist.

## Multi-host directory resolution

Swedish registry projects typically run on several hosts (Linux
workstations, Windows laptops, shared compute servers) with the
same data mounted at different paths on each. swereg handles this
via `CandidatePath`: every directory field on `RegistryStudy` (and
`TTEPlan`) is a priority list of candidate paths, and the first
existing one on the current host wins.

```r
study <- swereg::RegistryStudy$new(
  data_rawbatch_dir = c(
    "//shared-drive/registry/2026/rawbatch/",
    "/mnt/shared/registry/2026/rawbatch/",
    "C:/shared/registry/2026/rawbatch/"
  ),
  # ...
)
```

When the study object is serialized via `$save_meta()`, all
`CandidatePath` caches are cleared, so the resulting
`registrystudy.qs2` loads correctly on any host.

## Parallel workers

The heavy pipeline stages run parallel worker subprocesses, but **parallelism is opt-in per
stage** — the default is **1 worker (serial)** everywhere. Each stage resolves its worker count
via `swereg::default_n_workers("<stage>")`, in order:

1. `getOption("swereg.n_workers.<stage>")`
2. the `SWEREG_N_WORKERS_<STAGE>` environment variable (stage upper-cased)
3. fallback `1L`

Passing `n_workers` explicitly at the call site overrides all of the above. Because worker
counts are per-host and memory-bound, the idiomatic place to set them is each host's
`~/.Renviron` (just like the per-host paths above) — so a memory-heavy stage never inherits a
setting meant for a light one.

| Stage tag | Set by | Env var | Notes |
|-----------|--------|---------|-------|
| `rawbatch` | `RegistryStudy$save_rawbatch()` | `SWEREG_N_WORKERS_RAWBATCH` | generic pipeline; I/O-bound, usually left at 1 |
| `skeleton` | `RegistryStudy$process_skeletons()` | `SWEREG_N_WORKERS_SKELETON` | generic pipeline; framework + trim + codes + randvars |
| `s1` | `TTEPlan$s1_generate_enrollments_and_ipw()` | `SWEREG_N_WORKERS_S1` | TTE; ~6 GB/worker |
| `s2` | `TTEPlan$s2_generate_analysis_files_and_ipcw_pp()` | — | TTE; always `1L` (per-ETT memory isolation) |
| `s3` | `TTEPlan$s3_analyze()` | `SWEREG_N_WORKERS_S3` | TTE; ~20 GB/worker on large panels — keep low |

```bash
# ~/.Renviron on a big box: parallelise the light stages, keep the heavy one serial
SWEREG_N_WORKERS_S1=3
SWEREG_N_WORKERS_SKELETON=3
```

The former box-wide `SWEREG_N_WORKERS` (and `getOption("swereg.n_workers")`) is **deprecated
and ignored** — a single global knob could leak a high count into a heavy stage (3 × ~20 GB s3
workers → OOM). It emits a one-time warning if still set. Full details:
`?swereg::default_n_workers`.

## Known issues / follow-ups

Robustness edge cases flagged by review (2026-07-09) of the `survival_curve()` / `export()` /
forest work. None break a well-formed run (fully role-tagged outcomes, non-NA `age_group`,
exposure-grouped forest, both treatment arms), but they should be hardened:

- **survival `export()`** selects the ETT via `age_group == spec$age_group`, so an NA/omitted
  age group finds 0 rows and stops (`NA == NA` is not TRUE). Use NA-aware matching.
- **survival estimator** documents "deaths censored" but does not enforce it — it assumes the
  trial data is already death-censored upstream; post-death `event=0` rows would bias survival up.
- **survival treatment validation** is weak: the no-plot path skips checks; the plot path lets
  logical `NA` and single-arm data through. Require non-missing treatment + both arms.
- **role default label** renders `"(NA)"` when only *some* outcomes have `role:`. Append role
  conditionally (or store missing as `""`); consider validating exactly one primary.
- **forest `group_by="outcome"`** row-labels with `{enrollment_name}`, discarding the validated
  `names(exposures)`; and orders groups by `unique(ett$outcome_name)` (plan order), not manifest
  order. Carry the exposure label per `ett_id` and order by the manifest/spec.
- **`.ff_irr_ci`** blanks *every* `irr < 0.01`, not just true no-event cells — a real 0.004
  becomes indistinguishable from not-estimated. Test the no-intervention-events condition.
- **`export()`** manifest validation is thin (vector `type`, missing files, empty `estimands`
  fall through to cryptic errors); validate shape + check files before `qs2_read()`.
- **`reload_spec()`** only writes `!is.na(new_role)`, so a role removed from the spec persists;
  overwrite to `NA_character_` when absent.

## Documentation

Full reference, vignettes, and release notes:
https://papadopoulos-lab.github.io/swereg/

## Citation

If you use **swereg** in your research, please cite the repository:

```
Papadopoulos Lab (2026). swereg: longitudinal skeletons and target
trial emulation for Swedish registry data. R package.
https://github.com/papadopoulos-lab/swereg
```
