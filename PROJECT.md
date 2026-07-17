# PROJECT: one dispatcher + a real boundary contract (toward batchit)

Running design + learning doc. Previous project (ITT alongside PP +
effect modification, issue \#6) is **complete** — see `PROJECT.md` at
`e48a54d`.

One goal, stated plainly: **swereg dispatches work to subprocesses
through three hand-rolled engines and no enforced contract. Replace them
with one dispatcher and a contract that is validated at both ends and
tested.** A separate package (`batchit`) is the *eventual* packaging of
that contract, not the way to get it.

## STATUS: Phase 0 (design) — in progress. No code written.

Nothing below is implemented. Phases 1-3 need a package reinstall and
must wait for the running 002 pipeline to clear (s1 started 2026-07-17
05:06, ~14h).

------------------------------------------------------------------------

## The diagnosis (why this project exists)

On 2026-07-16/17 an engine review — plus four adversarial `codex` rounds
at `model_reasoning_effort=high` — turned up six defects. **Every one is
a contract failure at the parent/child process boundary. Not one is a
scheduling bug.**

| defect                                       | the contract it breaks |
|----------------------------------------------|------------------------|
| `arm_labels` silently dropped                | item -\> function      |
| `call_mirai(h)$value` is dead                | result                 |
| stdout/stderr pipes never drained            | I/O                    |
| `n_workers = 0` busy-spins forever           | input                  |
| invalid `dev_path` runs stale installed code | environment            |
| torn final file + mtime-trusting resume      | output                 |

Six for six. That is the whole argument for this project. The engines
are fine; the boundary they cross is unspecified, so each crossing
invents its own rules and each set of rules is wrong somewhere.

The 100-line `tests/testthat/test-worker_arg_parity.R` is the tell: it
is a regex parser that reads worker *source code* to check
`<arg> = params$<field>` against a function’s formals — and its third
test asserts `parsed$arg == parsed$field`, i.e. the mapping must always
be the identity. So 127 lines of dispatch scripts plus 100 lines of
regex are a hand-written, regex-verified implementation of
[`do.call()`](https://rdrr.io/r/base/do.call.html) — and it still missed
`arm_labels`, because it guards worker-\>formals while the live bug is
builder-\>worker.

## The two workload shapes (measured, not asserted)

The three engines are not three needs. Git says: everything was `callr`;
on 2026-05-15 `8d6b3e0` (“All-subprocess s1 architecture (OOM fix +
clean dispatcher)”) deliberately migrated the *hardest* stage off
`callr` onto `parallel_pool`, removing 1
[`callr::r_bg`](https://callr.r-lib.org/reference/r_bg.html) and adding
6
[`parallel_pool()`](https://papadopoulos-lab.github.io/swereg/reference/parallel_pool.md)
calls; the `callr` block in `process_skeletons()` is simply **the one
file that sweep missed**. `mirai` arrived the next day for a genuinely
different shape.

There are exactly **two** shapes, and they are real:

|               | shape A                    | shape B                                    |
|---------------|----------------------------|--------------------------------------------|
| stages        | skeleton, s1a-s1d, s2, s3  | `save_rawbatch()`                          |
| parent’s role | holds nothing              | **is the producer**                        |
| the item is   | ~11 KB of paths + config   | the data slice itself                      |
| distinction   | items already exist        | items generated lazily, under backpressure |
| engine today  | `parallel_pool` (processx) | `mirai` bounded queue                      |

Measured, live, on the box: shape A’s items are **10,905 bytes** each
(2,194 tempfiles, 26 MB total) — paths and config; the worker opens its
own data. Shape B cannot use `parallel_pool`: it materializes **every**
item to a tempfile before launching any worker
(`R/parallel_pool.R:80-85`), which on an I/O-bound bulk write means 2x
the I/O and the whole dataset on disk twice. `mirai`’s
`max_inflight = 2L * n_workers` is the bounded producer-consumer that
shape B needs.

**Not adopting crew/future/batchtools/targets.** Worker reuse — their
headline feature — is the one thing shape A must *not* have: s3 peaks
~20 GB/worker and R does not return that to the OS, so
fresh-process-per-item **is** the memory strategy. And the measured
upside is small: R startup + swereg namespace load is ~0.5s (bare R
0.42s), so s1c = 39,492 items x 0.5s / 6 workers ~= 55 min of a ~10h
stage ~= **9%**. The hypothesis that reuse was worth 30-50% was tested
and died.

## The contract

Write this down before any code. It is the deliverable; the package is
packaging.

**Target** is a *descriptor*, never a function, name, or closure:

``` r
.batch_target(package, symbol, version = NULL)   # + signature/hash where feasible
```

Package-name-plus-symbol is insufficient on its own: development code,
installed code and cache identity can differ. `R/r6_tteplan.R:5929`
already admits cache identity records only `packageVersion("swereg")`
while dev workers may load unversioned edited code. A contract that does
not fix this preserves the stale-code/resume problem under a new name.

**Item** contract:

- a named list; **every** name is a formal of the target
- **every formal is named — including optional ones.** Not just the
  required ones: `arm_labels` is an *optional* formal, so a rule that
  only demanded required formals would have missed it exactly as the
  regex test did. See Decisions.
- no positional arguments, no duplicate/blank names
- targets containing `...` are **prohibited** — arbitrary dots are
  incompatible with reliable typo detection
- **executor configuration is never mixed into the arguments.** Today
  `R/parallel_pool.R:80` injects `swereg_dev_path` and `n_threads` into
  every item even when they are not target formals. Envelope instead:
  `list(meta = list(target, threads, dev_path, ...), args = item)`

**Validation happens at both ends.** Parent validation is early UX;
child validation is correctness, because the child may load a different
package version, or the parent may be dev-loaded while the child falls
back to installed code, or an input may be replayed independently.
Parent validation must check **every** item, not `items[[1]]` — item
schemas are already legitimately heterogeneous (some s3 items carry
`subgroup_var`, others rely on the default).

**Result envelope**: protocol version, item id, status,
value-or-structured-error, target identity, captured warnings/log
metadata.

### What the contract does NOT promise

Stated explicitly, because the tempting version of this promise is
unimplementable:

- **Atomicity is scoped to the envelope.** The executor can atomically
  commit its own return envelope. It **cannot** transactionally commit a
  target that writes a raw file, an imputed file, a counts sidecar and a
  sentinel. Such targets own their own commit protocol (sentinel), or
  return a **commit plan the parent executes**.
- **Atomic rename is not durability.**
  [`file.rename()`](https://rdrr.io/r/base/files.html) is atomic on
  POSIX and server-side atomic on SMB/CIFS; it is not an fsync.
  `R/qs2.R:44`’s `path.tmp<PID>` is also not collision-proof, and
  replacement semantics vary by filesystem. The prose there currently
  overstates the guarantee.
- **Semantic failure is the consumer’s call.** Some swereg targets catch
  analysis errors and return `list(skipped = TRUE, ...)` as a
  *successful* result. “Loud failure” requires swereg to decide which
  domain conditions are failures; a generic executor cannot infer it.
- **Worker-count and thread policy are the consumer’s.**
  `SWEREG_N_WORKERS_<STAGE>` (`R/default_n_workers.R:41`) encodes
  stage-specific RAM policy and is baked into the production image’s
  `$R_HOME/etc/Renviron`. The executor takes a validated integer; it
  does not choose one, and it must not silently set data.table/BLAS
  threads.

## The six defects, verified

Fix order is deliberate: correctness before genericity.

1.  **`arm_labels` silently dropped — affects delivered output.** The
    builder (`R/r6_tteplan.R:2109`) computes
    `arm_labels = .lookup_arm_labels(self$spec, eid)` into the item; the
    target accepts it (`R/r6_tteplan.R:7185`, default `NULL`);
    `inst/worker_s3_enrollment.R:7` **never forwards it**.
    `s3_analyze()` (`R/r6_tteplan.R:2005`) calls `parallel_pool`
    **unconditionally** — there is no `n_workers == 1` serial branch —
    so this happens on every s3 run, while `recompute_baselines()`
    (`R/r6_tteplan.R:2443`) calls the same target directly *with*
    `arm_labels`. Two methods that build the same Table 1 disagree; the
    pipeline runs the broken one. Impact is labelling, not numbers:
    Table 1’s comparator/intervention headers default instead of coming
    from the spec. **`recompute_baselines()` is the repair path** — no
    s3 rerun needed.

2.  **`call_mirai(h)$value` is dead.** mirai moved the result to `$data`
    in 0.2.0 (2022-03-28); there is no `$value` alias. So at
    `R/r6_registrystudy.R:1645` `v` is always `NULL`,
    `inherits(NULL, "errorValue")` is `FALSE`, and **every**
    `save_rawbatch()` worker error is reported as success. The happy
    path works, which is exactly why it looks proven. Production speed
    does not validate the failure path. Use `collect_mirai()` /
    `is_error_value()`, pin a minimum version. Related:
    `save_rawbatch()` calls `daemons(n)` on mirai’s **default profile**,
    destroying any daemon config the user had — use a unique compute
    profile.

3.  **Pipe deadlock.** `stdout = "|"` / `stderr = "|"`
    (`R/parallel_pool.R:103`) are only read after the child exits
    (`:116-117`). A chatty child fills the 64 KB pipe buffer, blocks on
    write, never exits, and stays `is_alive() == TRUE` forever. **This
    looks exactly like a hung worker.** Redirect each worker to a
    bounded log file — simpler than draining, and it gives diagnostics
    for free.

4.  **`n_workers` is never validated.** `n_workers = 0` gives
    `floor(n_cores / 0)` -\> `n_threads = Inf` (`R/parallel_pool.R:61`)
    and the dispatch loop busy-spins at 100% CPU forever, because the
    `Sys.sleep(0.1)` sits inside `if (length(active) > 0L)`. Validate a
    finite, length-one, positive integer before any division or
    destructive state clearing; handle `detectCores()` returning NA.

5.  **Invalid `dev_path` silently runs stale code.**
    `R/parallel_pool.R:40` — a non-NULL but nonexistent `dev_path` makes
    `is_dev` FALSE and falls back to the *installed* package instead of
    erroring. Check the source tree’s DESCRIPTION name matches the
    target package.

6.  **Non-atomic final writes + mtime-trusting resume.** Several workers
    [`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html)
    straight to final paths (e.g. `R/r6_tteplan.R:7132`) while resume
    trusts existence/mtime -\> a killed worker leaves a torn file a
    later run **skips**. Adding a timeout makes this *more* likely, so
    atomicity comes first.

Also: **`mirai` and `devtools` are both undeclared.** `Suggests:` is not
empty (11 packages); mirai is simply missing from it, and
`inst/worker_bootstrap.R:10` uses
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html).
mirai belongs in **Suggests** — parallelism is opt-in, default is
serial, absence already errors clearly. Note `pkgload` would merely
change *which* dev dependency is undeclared; only removing source-tree
dev execution removes the dependency.

And `R/parallel_pool.R:75`
[`on.exit()`](https://rdrr.io/r/base/on.exit.html)-unlinks every input
tempfile on any exit including error, so the replay command
`.check_worker_error()` prints names a file that is deleted as the error
unwinds. Retention policy must keep the **failed** input — and only that
one, in a caller-chosen secure directory with restrictive permissions,
because these items can carry registry data.

------------------------------------------------------------------------

## Plan

### Phase 0 — design (no reinstall needed). THIS DOC.

Write the contract and the defect list down. Done when this file is
agreed.

### Phase 1 — correctness (needs reinstall; after 002 clears)

Fix the six defects *against* the contract, in the order above. Each
lands with the test that proves it:

| \#  | test                                                                                  |
|-----|---------------------------------------------------------------------------------------|
| 1   | fixture target with an explicit formal; omitting it fails **before dispatch**         |
| 2   | a worker error **halts** `save_rawbatch()`                                            |
| 3   | child writes 5 MB to both streams; parent completes under a deadline                  |
| 4   | `0` / `NA` / `-1` / vector all error immediately                                      |
| 5   | a non-NULL nonexistent `dev_path` errors rather than falling back                     |
| 6   | kill between temp creation and rename; result absent or previous-complete, never torn |

Then run `recompute_baselines()` on 002/003/006 to repair Table 1
without an s3 rerun.

### Phase 2 — one runner, inside swereg

New `R/batch.R`, package-neutral from line one, zero R6/domain imports:

``` r
.batch_target(package, symbol, version = NULL)
.batch_run(target, items, workers, ...)             # items already exist
.batch_stream(target, ids, producer, workers, ...)  # producer(id) -> item, lazily, bounded
```

`ids`, not a bare count: failures, retries, progress, retained inputs
and output records all need stable identities. Plus
`inst/batch_worker.R` (~10 lines): read envelope -\> resolve target -\>
re-validate -\> `do.call` -\> write result envelope.

Two frontends over **one** engine. They share target resolution,
validation, result envelopes, lifecycle and failure semantics; they may
differ in internal transport. Do not expose “mirai vs processx” as two
public conceptual models.

The executor owns a **private, matched** IPC codec derived from the
atomic-write logic.
[`qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)/[`qs2_write_atomic()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_write_atomic.md)
stay in swereg as persistence for scientific files. In particular
[`qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)’s
`check_version()` duck-type (`R/qs2.R:22`) stays swereg behaviour — a
generic executor duck-typing arbitrary environments for a
`check_version` member is hidden swereg policy masquerading as an
extension point.

### Phase 3 — route everything through it, then lock the door

1.  s1a/s1b/s1c/s1d/s2/s3/s3_enrollment -\> `.batch_run`
2.  `save_rawbatch` -\> `.batch_stream`
3.  `process_skeletons` -\> `.batch_run` with `.process_one_batch` as
    target — **but first** write the study snapshot **once** and pass
    `{snapshot_path, batch_idx}`. `registrystudy.qs2` is 5.7 MB; `callr`
    currently serializes it only for *launched* batches (~6 in flight ~=
    34 MB), whereas a naive `parallel_pool` translation would serialize
    it for all 2,194 items up front ~= **12.5 GB**. A 350x regression.
4.  Delete: `R/parallel_pool.R`, the
    [`callr::r_bg`](https://callr.r-lib.org/reference/r_bg.html) block,
    all 8 `inst/worker_*.R` dispatchers, `inst/worker_bootstrap.R`,
    `tests/testthat/test-worker_arg_parity.R`.
5.  **The step that makes this real** — assert no alternative dispatch
    path exists:

``` r
test_that("only R/batch.R dispatches subprocesses", {
  others <- setdiff(list.files("R", full.names = TRUE), "R/batch.R")
  hits <- grep("processx::|callr::|mirai::", unlist(lapply(others, readLines)), value = TRUE)
  expect_equal(hits, character(0))
})
```

This is the enforcement. A package boundary is *not* access control —
`:::` exists, and swereg could always call processx directly. What
enforces a contract is making one dispatcher unavoidable, validating at
both ends, and testing that no bypass exists. That costs three lines and
works today, inside one package.

### Phase 4 — extract `batchit` (mechanical)

`batchit` is **free on CRAN** (`batchtools`, `batch`, `BatchJobs`,
`batchmix` are taken). Extract only when **`tte` has a real call site
exercising the same contract** — a hypothetical second consumer is not
enough. Then it is `git mv R/batch.R` + `inst/batch_worker.R`, leaving a
thin swereg adapter (target selection, progress labels, dev-vs-install
policy). No cycle: swereg Imports batchit; the child loads the *named
consumer package* at runtime — plugin loading, not a static dependency.

**Manifest for that day** (reviewed): SPLIT `parallel_pool`
(scheduling/lifecycle/ IPC/validation move; target selection + progress
labels + dev policy stay in a swereg adapter) and the mirai block
(bounded scheduling moves; `payload_for_batch()`, group bookkeeping,
filenames stay). DELETE the 8 workers, the bootstrap, the callr block,
the parity test. **STAY in swereg**: `default_n_workers.R` (deployment
policy), `progress_handlers.R` (UI/session policy), both `qs2.R`
functions, `r6_candidate_path.R` + `path_resolution.R` (where files live
is not how work is batched), `validation_helpers.R` (none of its 432
lines is batching), everything domain. Real generic core: **~250-400
lines**, not the ~600-700 first estimated.

------------------------------------------------------------------------

## Decisions (settled 2026-07-17, Richard)

- **Every formal must be named, including optional ones.** This is what
  makes the contract catch `arm_labels` rather than merely document it:
  `arm_labels` *is* an optional formal (`arm_labels = NULL`), so a rule
  that only requires *required* formals would have missed it exactly as
  the regex test did. The bug’s shape is “an optional arg silently
  absent, target takes its default” — which is indistinguishable from a
  deliberate default unless every formal is explicit. **Migration cost,
  accepted:** every builder must pass `subgroup_var = NULL`,
  `arm_labels = NULL` etc. explicitly, and adding an optional formal to
  a target becomes a breaking change until every builder is updated.
  That cost is the feature.

- **Target identity = package + symbol + body hash.**
  `digest(list(body(fn), formals(fn)))` — the mechanism already exists
  in this package: `.hash_function()` in `R/r6_registrystudy.R` uses
  exactly this for skeleton phase replay, and it is empirically verified
  that comments do not move the hash while a one-token code change does.
  This closes the hole `R/r6_tteplan.R:5929` already admits, where cache
  identity records only `packageVersion("swereg")` while
  `swereg_dev_path` workers execute arbitrary edited code under an
  unchanged version string. Accepted consequence: any real edit to a
  target invalidates its cache. That is correct, and it is what already
  happens for skeleton phases.

- **Retention: metadata only, never payload. Replay by regeneration.**
  Keep the target descriptor, item id, field *names*, error and logs —
  never the argument *values*. This is why `batch_stream()` takes `ids`
  rather than a bare count: with a stable id, replay does not need
  retention at all — **re-run the producer for that id and regenerate
  the item.** The builders are pure functions of (spec, skeleton list,
  index), so items are reproducible on demand. One policy covers both
  shapes, and no registry payload ever lands in a failure directory —
  which matters because shape B items *are* patient data, and `/tmp` on
  a shared box is not an acceptable home for them.

- **Single-writer: not enforced, and not in this contract.** One
  operator; none of the six defects is a concurrency bug; and a
  run-level lock is the wrong layer for an item-level executor. If it
  ever bites, it belongs in `bin/tte.sh` — which already writes
  `current_stage.txt` — not here. Noted rather than built, deliberately:
  this is a single-operator serial pipeline, not a distributed system.
  (Real but unrealised scenario: bench and uppsala both mount the same
  Argos share, and `claude rc` is a second entry point.)

- **[`.libPaths()`](https://rdrr.io/r/base/libPaths.html) propagation is
  forced, not chosen.** `--vanilla` does not reproduce the parent’s
  runtime library path, and it *cannot* travel in the payload — the
  child needs `qs2` to read the payload in the first place. It must be
  set as `R_LIBS` in `processx::process$new(env = )`, before startup.

- **Timeout:** per-item configurable, generous default. Not a hardcoded
  value. Log capture must be bounded — draining without a bound trades
  deadlock for unbounded RAM/disk.

## Open

- **Is `inst/worker_s1a.R` dead?** No production call site selects it
  (s1 uses `worker_s1a_multi.R`); it survives only in the parity test’s
  map. Confirm and delete rather than port. (Verification, not a
  decision.)

## Understanding checklist (for Richard)

### 1. The problem

- Why all six defects are the *same* bug: an unspecified boundary.
- Why the parity test passes while `arm_labels` is dropped (it guards
  the wrong half: worker-\>formals, not builder-\>worker).

### 2. The solution

- Why the contract, not the package, is the deliverable.
- Why validation must happen in the child too, not just the parent.
- Why “atomic output” can only ever mean “atomic envelope”.

### 3. Broader context

- Why two shapes justify two transports but one contract.
- Why `batchit` waits for `tte` to have a real call site.
