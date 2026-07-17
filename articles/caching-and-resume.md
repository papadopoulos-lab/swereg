# Caching and resume: what invalidates what

## Caching and resume: what invalidates what

A full run of this pipeline is measured in **days**, so almost every
stage avoids redoing work it has already done. That is five caches and
one commit record, each with different rules about what makes it stale —
and the rules get markedly weaker as you move downstream.

Getting those rules wrong is not loud. A cache that should have been
discarded and wasn’t produces a run that completes normally and reports
nothing unusual — it is simply built partly from data you have since
replaced. This vignette is the map: what is cached, what invalidates it,
and what none of it protects you from.

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
| **4. s1 work directory**                 | ~14 hours of enrollment + IPW                                            | spec, skeleton identity, batch selection, s1 parameters, swereg version               |
| **5. s2 analysis files**                 | ~2.5 hours                                                               | **wall-clock only** — see below                                                       |
| **6. s3 results**                        | ~10–15 hours                                                             | **presence only** — skipped unless `force = TRUE`                                     |

Layers 2 and 4 are the ones people trip over. The manifest exists to
connect them: without it, layer 4 cannot tell whether layer 2’s *output*
changed underneath it.

**Layers 5 and 6 are much weaker than layer 4, and you should know that
before you rely on them.** s1’s key is derived from its inputs; s2’s is
a timestamp and s3’s is mere existence. They are convenience caches for
a human iterating, not correctness mechanisms. The production stage
scripts bypass both (`resume = FALSE` for s2, `force = TRUE` for s3) —
keep it that way unless you are deliberately iterating.

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

Note precisely what the manifest does and does not fix here. It ties
layer 4 to the *generations layer 2 actually produced*, so a rebuild
cannot be missed. It does **not** rescue this case: if layer 2 never
rewrites the batch, there is no new generation to notice. For changed
raw data you must invalidate layer 1 by hand.

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
so `identity` is unchanged — and that stability is what makes
`resume = TRUE` usable at all.

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

### Layer 4 — the s1 work directory

s1 writes sentinels and cached chunks into:

    {data_meta_dir}/s1_work/{project_prefix}/{cache_key}/

`cache_key` is a full 64-bit digest of **everything s1 explicitly
tracks**. It is not everything s1’s output depends on — the known-limits
section below lists what is deliberately outside it:

| input                              | why it must be in the key                                                                                                                                                                             |
|------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| the parsed spec                    | different exclusions, different people                                                                                                                                                                |
| manifest `identity`                | different skeleton generation                                                                                                                                                                         |
| the **ordered** selected batch IDs | `n_skeleton_files` caps what s1 reads while leaving the directory unchanged, so a capped run and a full run would otherwise share a key. Order matters because matching consumes chunks in plan order |
| `impute_fn`, `stabilize`           | change the imputed values or the weights                                                                                                                                                              |
| `output_dir`                       | s1d skips on its sentinel, so resuming with a new destination would skip s1d, delete the work dir, and report success having written nothing there                                                    |
| swereg version                     | s1’s own implementation is an input to its output                                                                                                                                                     |

s1 reads the manifest **from disk**, re-reading `registrystudy.qs2`
rather than trusting `plan$registrystudy` — which is a copy frozen when
s0 saved the plan and would happily describe skeletons that have since
been rebuilt.

`resume = FALSE` additionally clears its own keyed directory before
starting, rather than trusting every sub-step to overwrite every
artifact it finds: a leftover sentinel beside a half-rewritten cache is
exactly what a later `resume = TRUE` mistakes for completed work.

#### When s1 refuses to resume

    resume = TRUE needs a committed skeleton manifest, and there is none.

No manifest means one of:

- **The skeletons predate manifests.** Run `$process_skeletons()` once.
- **A regeneration was interrupted or failed.** The manifest is cleared
  up-front and only recommitted on success — this is the protocol
  working. Re-run the regeneration to completion.
- **The skeletons are bare `data.table`s**, not `Skeleton` objects. s1
  accepts these, but they carry no provenance and never can, so they are
  limited to `resume = FALSE`.

`resume = FALSE` bypasses **this particular requirement** — it reads no
cache, so there is nothing to match against the skeletons. It does not
make s1 unconditional: every other validation still applies.

### Cleaning up

s1 removes its own work directory on success. A directory left behind is
from a run that was killed or superseded — and since the key covers
skeleton state, every regeneration orphans the previous cache.

``` r
tteplan_s1_cache_delete(plan)                   # dry run: reports, deletes nothing
tteplan_s1_cache_delete(plan, dry_run = FALSE)  # actually delete
```

Deliberately explicit rather than automatic at startup: only the run
that created a work directory knows whether it is finished with it,
deleting another run’s work would break a concurrent s1, and a killed
run’s cache is the only evidence of what it did. Deleting a cache is
never a correctness risk — it is an accelerator, not an artifact, so the
worst case is that s1 redoes work.

### What none of this protects you from

Stated plainly, because a green run is seductive.

- **Concurrency.** Exactly one `$process_skeletons()` may run against a
  skeleton directory at a time, and no s1 may read it while one does.
  **Nothing enforces this.** Two writers interleave as clear(A),
  clear(B), commit(A), B-replaces-skeletons-and-dies — leaving A’s
  manifest vouching for B’s skeletons. Atomic writes cannot fix a
  logical race. Two s1 runs sharing a cache key likewise share a work
  directory. **Serialise your stages.**
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
- **Editing swereg without bumping the version.** `swereg_dev_path`
  loads a development checkout whose code need not match any released
  version. Bump the version for any change to s1 semantics.
- **Closure-captured state, anywhere.** Functions are hashed by body and
  formals, so two closures differing only in captured values key
  identically. This applies to `framework_fn` and the randvars steps
  exactly as much as to a custom `impute_fn`.
- **Phase-2 code registry entries** are fingerprinted by their declared
  codes and arguments, not by the implementation of the functions that
  apply them.
- **s2 and s3 have much weaker invalidation than s1** — a 24-hour
  timestamp and bare existence respectively. Do not read this vignette’s
  care over layer 4 as applying to them.
- **Adversarial integrity.** These are short cache identifiers, not
  cryptographic authenticity guarantees.
- **Scientific correctness.** All of this checks identity and
  completeness. It has nothing to say about whether the contents are
  right.

### Recovery: what to actually do

| symptom                            | do this                                                                                                                                                                                                                      |
|------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| s1 refuses: no committed manifest  | Stop any concurrent job, then run a **full** `$process_skeletons()` (`batches = NULL`) to completion. Not a subset — a subset that fails to validate is deliberately non-fatal and will leave you exactly where you started. |
| Regeneration was killed            | Same. The cleared manifest is the protocol working, not damage.                                                                                                                                                              |
| Raw delivery changed, code did not | Nothing will notice on its own. `$delete_rawbatches()`, `$delete_skeletons()`, `$delete_meta_file()`, then re-run the full generator.                                                                                        |
| Legacy bare `data.table` skeletons | They cannot earn provenance. Use `resume = FALSE`.                                                                                                                                                                           |
| “Is my skeleton dir mixed?”        | `study$skeleton_pipeline_hashes()` — one distinct `pipeline_hash` and no `NA`s is what you want. Compare it to `study$pipeline_hash()` to see whether it is *current* as well as uniform.                                    |
| Orphaned s1 caches eating disk     | `tteplan_s1_cache_delete(plan)` to look, `dry_run = FALSE` to remove.                                                                                                                                                        |

### A worked example

From a real production run (2026-07-17), showing all four layers.

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

- **Layer 4** — s1, started afterwards, reported:

      Skeletons: 2194 of 2194 batches, pipeline 9e079c0876398c90,
                 identity 73180d2cd7417e91
      Cache key: 7d94b5f14005b414

Had the same spec been run against a different skeleton generation, the
key would have moved and a fresh work directory been used. Before
`identity` entered the key, it would have been **identical** — and the
stale cache reused in silence.
