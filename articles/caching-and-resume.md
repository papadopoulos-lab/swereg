# Caching and resume: what invalidates what

## Caching and resume: what invalidates what

A full run of this pipeline is measured in **days**, so the skeleton
side of it avoids redoing work it has already done. That is three caches
and one commit record, each with different rules about what makes it
stale.

Getting those rules wrong is not loud. A cache that should have been
discarded and wasn’t produces a run that completes normally and reports
nothing unusual — it is simply built partly from data you have since
replaced. This vignette is the map: what is cached, what invalidates it,
and what none of it protects you from.

**The TTE stages (s1–s3) hold no caching or resume opinion of their
own** — that was deliberately removed (Phase 5′; see `PROJECT.md`).
`s1`’s work directory is transient dataflow between its four internal
sub-steps, cleared at the start of every call and removed again on
success; `s3_analyze()` recomputes every targeted result on every call.
A killed s1 restarts from zero, and a mid-run s3 crash means a full
recompute — accepted costs, not bugs. This vignette is therefore only
about the skeleton pipeline below s1, where the caching that remains
lives.

Read
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md)
first for how skeletons are built, and
[`vignette("tte-workflow")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md)
for what s1–s4 do.

### The layers

| layer                                    | what it avoids                                                           | invalidated by                                                                        |
|------------------------------------------|--------------------------------------------------------------------------|---------------------------------------------------------------------------------------|
| **1. Rawbatch staging**                  | re-reading tens of GB of raw registry files                              | nothing automatic — guarded on existence                                              |
| **2. Skeleton phases**                   | rebuilding 2,000+ skeletons for a one-line change                        | the `body`/`formals` hash of the changed function                                     |
| **3. Meta sidecars + derived artifacts** | re-reading whole skeletons to answer provenance and population questions | rewritten with their batch                                                            |
| **—. Skeleton manifest**                 | *(not a cache — a commit record)*                                        | cleared before every `$process_skeletons()`, recommitted only if the result validates |

------------------------------------------------------------------------

### Layer 1 — rawbatch staging

`$save_rawbatch()` writes the raw registries into per-batch `.qs2`
files. This is guarded by existence, not by a hash: once the rawbatches
and `registrystudy.qs2` exist, a reclean skips staging entirely and does
**not** re-read the raw delivery.

That is a deliberate trade. Re-staging is expensive and the raw delivery
is a fixed artifact, so the guard is almost always right. But note the
consequence: **if the raw files change, nothing here notices.** You must
delete the rawbatches (or the meta file) by hand.

``` r
study$delete_rawbatches()   # force a re-stage
study$delete_meta_file()
```

### Layer 2 — skeleton phases

`$process_skeletons()` does not rebuild skeletons; it **replays the
phases that changed**. Each batch’s skeleton carries its own provenance,
and the decision is per phase:

- **Phase 1 (framework).** If `framework_fn`’s hash differs from the one
  stored on the skeleton — or no skeleton exists — the base is rebuilt
  from scratch, and phases 2 and 3 are reset with it.
- **Phase 3 (randvars).** `$sync_randvars()` walks the registered
  functions in order and finds the **divergence point**: the first
  position where the name or hash differs from what the skeleton
  recorded. Everything from there on is dropped and replayed; everything
  before it is kept. If nothing diverges, the batch is untouched.

**The rewind is not a time machine.** `$sync_randvars()` drops only the
columns each step *recorded itself as adding*. It cannot restore a
column an earlier step overwrote, and it cannot bring back rows a step
filtered out. If a randvars step mutates or filters rather than adds,
replaying from a divergence point does not return the skeleton to its
pre-step state — rebuild from phase 1 instead.

The hash is `digest(list(body(fn), formals(fn)))`. Two consequences
follow, and both matter in practice:

**Comments are free.** Documentation, changelog entries, and
reformatting of comments do not change
[`body()`](https://rdrr.io/r/base/body.html), so they do not trigger a
replay. You can fix a comment in a randvars file while a regeneration is
running.

**Code identity is not data identity.** The hash fingerprints the
*registered functions*. It says nothing about called helpers, package
versions, globals, or the raw inputs. **Change the raw data with
unchanged code and every hash is identical** — layer 2 will conclude it
has nothing to replay, and it will be wrong.

The skeleton manifest below is downstream of this layer: it records
exactly which generation layer 2 actually produced, so a rebuild cannot
go unnoticed by anyone reading the manifest. It does **not** rescue the
case above — if layer 2 never rewrites the batch, there is no new
generation to notice. For changed raw data you must invalidate layer 1
by hand.

### The skeleton manifest (not a cache)

A manifest is `$process_skeletons()`’s **commit record**. It attests
that, at commit time, every batch’s provenance sidecar was readable, the
set was complete and internally uniform, and it matched the study’s
pipeline fingerprint. It does **not** prove the skeleton bytes match
their sidecars, that the raw inputs were current, or that any content is
correct.

It lives in the `skeleton_manifest` field of `RegistryStudy` and is
written into the ordinary `registrystudy.qs2` by the ordinary
`$save_meta()`. No extra file.

``` r
study <- registrystudy_load("/path/to/meta/dir")
str(study$skeleton_manifest)
#> List of 7
#>  $ manifest_version: int 1
#>  $ committed_at    : POSIXct "2026-07-17 04:51:12"
#>  $ swereg_version  : chr "26.7.19"
#>  $ n_batches       : int 2194
#>  $ batches         : int [1:2194] 1 2 3 4 5 ...
#>  $ pipeline_hash   : chr "9e079c0876398c90"
#>  $ identity        : chr "73180d2cd7417e91"
```

`pipeline_hash` **fingerprints the registered pipeline code**.
`identity` is a **generation token** derived from per-batch provenance
metadata — it digests the ordered per-batch
`(batch, pipeline_hash, built_at)` triples. Neither is a content hash of
the skeleton data.

`built_at` is what makes `identity` a *generation* token rather than a
code one: when a batch and its provenance are rewritten, its `built_at`
changes, so `identity` moves even though the code did not. Conversely an
incremental run that skips every already-current batch rewrites nothing,
so `identity` is unchanged.

#### The commit protocol

The manifest is **cleared before a single batch is touched**, and only
recommitted at the end if the finished dataset earns it:

1.  **Provenance readable** for every batch.
2.  **Exactly one distinct `pipeline_hash`** — more means an interrupted
    replay left a mix of old and new skeletons.
3.  **That hash equals the study’s current `$pipeline_hash()`.**
    Internal agreement is not currency: a dataset can be perfectly
    self-consistent and simply out of date.
4.  **Batch IDs are exactly `seq_len(expected)`.** A count cannot
    distinguish 1..N from 2..N+1 — and a *first* build interrupted at
    batch 272 leaves 272 mutually-consistent skeletons that a hash-only
    check waves straight through, after which s1 analyses 12% of the
    cohort and looks fine doing it.

Clearing first is what makes a kill safe: a manifest that survived an
interrupted run would vouch for skeletons it no longer describes, which
is worse than none.

`batches` controls only what a run **processes**, never what is
validated — validation always covers the whole directory, because that
is what s1 reads. A subset run (`batches = 1:10`) therefore still
commits if the resulting whole dataset validates. What a full run
(`batches = NULL`) changes is the **failure** behaviour: it raises,
rather than letting a caller’s file-count gate report success over a
dataset nothing will accept.

### What none of this protects you from

Stated plainly, because a green run is seductive.

- **Concurrency.** Exactly one `$process_skeletons()` may run against a
  skeleton directory at a time. **Nothing enforces this.** Two writers
  interleave as clear(A), clear(B), commit(A),
  B-replaces-skeletons-and-dies — leaving A’s manifest vouching for B’s
  skeletons. Atomic writes cannot fix a logical race. **Serialise your
  regenerations.**
- **Identity is not content.** Nothing here digests skeleton bytes.
  Editing, replacing, restoring from backup, or corrupting a skeleton
  without correspondingly regenerated provenance may go undetected. The
  stale-sidecar case below is one instance of this general limit.
- **A skeleton replaced under its own sidecar.** Validation reads
  sidecars, not skeletons — opening all of them would mean reading
  gigabytes every run. Both writes are atomic, but they are separate
  writes, so a crash between them can leave a new skeleton with a stale
  sidecar.
- **Untracked dependencies.** A function-body fingerprint does not cover
  called helpers, package versions, globals, environment variables,
  external files, time, or randomness.
- **Raw data that changes without forcing a rewrite.** Layer 1 is
  guarded on existence and layer 2 on code identity, so unchanged code
  over changed raw inputs invalidates nothing. Delete the rawbatches by
  hand.
- **Closure-captured state, anywhere.** Functions are hashed by body and
  formals, so two closures differing only in captured values key
  identically. This applies to `framework_fn` and the randvars steps.
- **Phase-2 code registry entries** are fingerprinted by their declared
  codes and arguments, not by the implementation of the functions that
  apply them.
- **Adversarial integrity.** These are short cache identifiers, not
  cryptographic authenticity guarantees.
- **Scientific correctness.** All of this checks identity and
  completeness. It has nothing to say about whether the contents are
  right.

### Recovery: what to actually do

| symptom                                   | do this                                                                                                                                                                                   |
|-------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Regeneration was killed                   | Run a **full** `$process_skeletons()` (`batches = NULL`) to completion. The cleared manifest is the protocol working, not damage.                                                         |
| Raw delivery changed, code did not        | Nothing will notice on its own. `$delete_rawbatches()`, `$delete_skeletons()`, `$delete_meta_file()`, then re-run the full generator.                                                     |
| “Is my skeleton dir mixed?”               | `study$skeleton_pipeline_hashes()` — one distinct `pipeline_hash` and no `NA`s is what you want. Compare it to `study$pipeline_hash()` to see whether it is *current* as well as uniform. |
| s1’s work directory survived a killed run | Nothing to do by hand — the next `$s1_generate_enrollments_and_ipw()` call clears it automatically at start.                                                                              |

### A worked example

From a real production run (2026-07-17), showing layers 1–3 and the
manifest commit.

A regeneration had been killed the previous afternoon, leaving the
skeleton directory **mixed**: 272 batches rebuilt with new code, 1,922
still on the old.

                        before                      after
    skeleton state      272  @ 9e079c08 (new)       2194 @ 9e079c08
                        1922 @ b4b325e5 (old)       1 distinct hash, 0 unreadable
    manifest            NULL                        identity 73180d2cd7417e91

The rerun:

- **Layer 1** skipped raw staging entirely (rawbatches + meta existed).

- **Layer 2** reached 272/2194 in **56 seconds** — the meta-sidecar fast
  path found those batches’ pipeline already current and skipped them
  without ever loading the skeleton. The killed run’s work was salvaged.
  It then rebuilt the remaining 1,922 in 7h 35m.

- **Layer 3** validated and committed:

      Skeleton manifest committed: 2194 batches, pipeline 9e079c0876398c90,
                                   identity 73180d2cd7417e91

Had the same spec been run against a different skeleton generation
before Phase 5′, s1’s (now-removed) cache key would have moved and a
fresh work directory been used. Today s1 rebuilds its transient work
directory from scratch on every call regardless.
