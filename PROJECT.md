# PROJECT: one dispatcher + a real boundary contract (toward `batchit`)

Running design + learning doc. Previous project (ITT alongside PP + effect
modification, issue #6) is **complete** — see `PROJECT.md` at `e48a54d`.

One goal, stated plainly: **swereg dispatches work to subprocesses through three
hand-rolled engines and no enforced contract. Replace them with one dispatcher and
a contract that is validated at both ends and tested.** A separate package
(`batchit`) was always the intended *packaging* of that contract, but building it
first was never the way to *get* the contract: the contract was built and hardened
inside swereg first (Phases 1-3), then extracted. **As built (2026-07-18):**
`batchit` now exists (`papadopoulos-lab/batchit`) and IS that packaging — swereg
`Imports` it and is a thin adapter over it (`R/batch_adapter.R`).

## STATUS: Phases 0-3 complete (Phase 3 signed off by codex, round 3, 2026-07-18). **Phase 4 EXECUTED 2026-07-18** — the dispatcher was shrunk and extracted into `batchit` by explicit maintainer direction, ahead of the recorded wait-for-`tte` precondition. The final adversarial gate reached round 2 with a single remaining blocker — this doc still carried pre-extraction present-tense claims contradicting the as-built split — now recast as historical here (see the Phase 4 section below; a later commit records the final sign-off).

- **Phase 3 (route everything through it, delete the engines): DONE.** Every
  parallel work dispatch in the package now crosses the ONE batch contract and
  the three legacy engines are deleted. (Scoped honestly: the two deliberate
  serial branches run the same targets in-process -- no process boundary, no
  dispatcher -- and the one non-batch child process left is the `git
  rev-parse` metadata shell-out in `compute_summary.R`, which is not work
  dispatch.) Commits fc4b5d0 (s3 ETT loop; the two builder
  landmines — per-worker `n_threads` the pool used to overwrite, explicit
  `subgroup_var = NULL` — fixed and pinned red/green by
  `test-s3_item_contract.R`), e3dd95e (s2), 2a46b30 (s1a–s1d + delete the dead
  `.s1a_worker()`), 162c058 (`save_rawbatch` → `.batch_stream`, the missing
  shape-B production proof; serial stays in-process so mirai remains
  opt-in), d44082a (`process_skeletons` → `.batch_run` via the ONE-snapshot
  target `.process_one_batch_snapshot` — the 12.5 GB trap is pinned
  structurally), 9135e16 (delete `parallel_pool`/`worker_bootstrap`/the callr
  Import; port the surviving I/O guarantees; widen the no-direct-qs_save guard
  to all of R/), b95c6df (the lockdown test: parse-based, R/ + inst/, bans any
  engine MENTION outside `R/batch.R`, non-vacuous and proven red).
  Production-boundary proofs drive REAL subprocesses at every migrated caller:
  `test-batch_s3_production.R` (both s3 loops asserted),
  `test-batch_rawbatch_production.R`, `test-batch_skeletons_production.R`.
  Retrospective inputs honoured: `save_rawbatch` proven for shape B, thread
  policy inventoried before the pool died, stable ids at every caller, workers
  deleted in the same commit as their migration.
  codex (`model_reasoning_effort=high`) returned **DONE — YES** on round 3;
  blocker arc 5 → 1 → 0, every one legitimate: R1 caught a non-recursive
  lockdown scan, a host-dependent thread test (both 1 on a 1-core box),
  wrapper-phrase-only failure assertions, two undeclared progress
  regressions (per-item completion ids, drain-side counts), and doc
  overclaims (worker count, execution order, serial scope); R2 caught one
  residual overclaim in the same NEWS sentence. Fixed in fe5e517 + ded113f.
- **Phase 2 (the one runner): DONE — signed off, adversarially, over EIGHT rounds.**
  `R/batch.R` (`.batch_target`, `.batch_run` shape A, `.batch_stream` shape B, the
  private codec, `.batch_execute`, total input+result inspectors), `inst/batch_worker.R`
  (the ONE generic worker with a pre-load structural gate), and the s3_analyze
  **enrollment-loop** migration are in. codex (`model_reasoning_effort=high`)
  returned **DONE — YES** on round 8, after seven rounds each of which found a real
  blocker (7 → 4 → 5 → 2 → 2 → 1 → 1 → 0). Every fix landed with a test demonstrated
  to fail without it, and the production boundary is tested through the REAL worker
  (`test-batch_s3_production.R`), not helpers. The arc of blockers, all fixed:
  - R1 (7): both-end envelope validation; captured+surfaced warnings;
    production-boundary FAILURE proof (not just success); metadata-only fail-closed
    retention with a real (deserialising) test; validate-before-empty-return;
    unified failure lifecycle (stream retention/timeout, overflow, profile guard);
    stable enrollment-id item ids.
  - R2 (4): full result-schema + target identity (pkg+symbol+hash) in the inspector;
    connection-independent profile detection; honest retention contract; scalar
    timeout/collect validation.
  - R3 (5): inspector total on malformed error/warnings + strict id (id-before-status,
    worker echoes real id); profile NA/aliased-default reject + fail-closed;
    retention fails closed on chmod failure; `items` must be a list; worker temp cleanup.
  - R4 (2): `mirai::daemons_set()` ownership (floor → 2.3.0); fully total inspector
    (whole-body tryCatch) + duplicate-name rejection.
  - R5 (2): safe `conditionMessage` (a hostile condition can't escape); nested
    duplicate-name checks (target + child meta/outer).
  - R6 (1): the worker validates envelope STRUCTURE before loading any code.
  - R7 (1): exact `[[` extraction everywhere (no `$` partial-matching steering loads).
  - R8: **DONE — YES.**
- **Phase 0 (contract + decisions): DONE.**
- **Phase 1 (correctness): DONE — 20 defects, adversarially signed off.** The
  arbiter (codex, `model_reasoning_effort=high`) returned **DONE — YES** on the
  sixth review, after five earlier rounds each of which found a real blocker.
  Every defect was **reproduced first**, then fixed, then covered by a test
  **demonstrated to fail without the fix**. Suite: **1578 pass, 0 fail, 0 error.**
  New tests: `test-parallel_pool_io.R`, `test-qs2_write_atomic.R`,
  `test-mirai_error_contract.R`, `test-resume_fresh.R`,
  `test-worker_count_validation.R`, `test-n_workers_entry_points.R`, plus the
  mirror check in `test-worker_arg_parity.R`.
- Phase 1's sign-off carry-forward — *make the generic runner the only real
  process boundary, then test failures through that production boundary; helper
  tests and dependency demonstrations must not substitute for proving the
  actual caller -> dispatcher -> worker -> cleanup -> completion path* — was
  honoured in Phases 2 and 3 (it drove Phase 2's final review rounds and Phase
  3's per-caller production tests). Three of Phase 1's six rounds' findings
  were my own tests passing for the wrong reason; that lesson is why every
  migrated caller has a boundary test driving real subprocesses.
- **Phase 4 (`batchit` extraction): EXECUTED 2026-07-18.** The recorded rule said
  wait — extract ONLY once a second consumer (`tte`) had a real call site, and not
  before. The maintainer overrode that rule explicitly (2026-07-18, "complete
  phase 4", target `papadopoulos-lab/batchit`): the extraction proceeded with no
  `tte` package, no second consumer, and no production rerun after Phase 3. The
  seam risk the rule guarded against (runner-vs-consumer loading from different
  trees) was mitigated by synthetic-consumer seam tests in `batchit`'s suite
  rather than by a real second consumer. See the Phase 4 section below for the
  as-built shrink-then-extract narrative and the dated exception notes.

**Two review rounds have said NO, and both were right on every count.**

Round 1 caught: `.log_tail()` reading the whole file then tailing (a defect **I
introduced**, under a comment claiming "bounded on purpose"); s2 resume taking
`max(mtime)` so one 1h-old file validated a 100h-old one (**a defect nobody had
found**); `.compute` needing `mirai >= 0.8.0`; a profile test that proved a fact
about **mirai** rather than **swereg**, so production could revert and stay
green; and two acceptance criteria written into this doc and then skipped.

Round 5 caught two more, and both were the *same shape* as the round-4 fixes —
"a rejected count changes nothing" was still violable. A whole double above
`.Machine$integer.max` passed validation and became `NA` on coercion, then flowed
past the check into the manifest destruction; and the mirai daemon's `tempfile()`
inline cleaned up only on rename failure, not on a serialization error, so a
failed `qs_save()` left a partial (sensitive) slice on the shared disk. Both
fixed. Round 5 also showed my two new *behavioural* state tests passed for the
**wrong reason** — a fresh study errors on missing `framework_fn` before reaching
the manifest code — so they were rebuilt to register a framework and genuinely
reach the destructive path, and joined by a **structural** guard (the first body
expression of each entry must literally be the `.validate_n_workers()`
assignment) that no reordering can slip past.

Round 4 caught three more: `save_rawbatch()` still skipping validation on its
already-saved early-return; `s1`/`process_skeletons` mutating `self` *before*
validating (so a rejected call left the object half-changed); and the mirai
daemon still hand-inlining the **unsafe PID temp name** I had just fixed in
`qs2_write_atomic()`. It also found my round-4 ordering-guard helper was itself
broken — `.first_line_matching()` returned the first hit of the first *pattern*,
not the minimum across all — so the guard could pass while the bug was live. All
fixed; validation is now the first statement of every entry, backed by
**behavioural** tests (a rejected call leaves the manifest and `output_dir`
untouched) as well as the repaired ordering guards, and the anti-drift check now
**derives** the entry-point list from the R6 formals instead of hard-coding it.

Round 3 caught a defect **I introduced in round 3's own fix**
(`results[[idx]] <- NULL` deletes the element, corrupting `collect = TRUE` when a
worker returns `NULL`) and two entry points still validating too late
(`process_skeletons()` invalidated the manifest *before* checking `n_workers`;
`default_n_workers()` silently truncated a bad config). It also caught my
`NULL`-result test using the **benign** completion order, so it passed against
the bug — the third "test that looked load-bearing" in as many rounds. All fixed;
the `NULL` test now uses the corrupting order and is proven to fail without it.

Round 2 caught something worse — **tests that only looked load-bearing**:

| test | why it guarded nothing |
|---|---|
| "huge log reports a bounded tail" | the buggy `readLines(whole_file)` **also** emitted a small message. The defect was the unbounded *read*, and an output-size assertion cannot see it. Now proved by **time**: 120 MB, must return in <1s. |
| "log reclaimed immediately" | the pre-existing `on.exit(unlink(log_paths))` made before/after counts equal either way. Now observes **during** the run, via the progressor. |
| "per-file resume" | tests the helper, not that s2 calls it. |
| `expect_false(grepl("<old message>", err))` in `test-s3_n_workers.R` | my reworded error made these **pass vacuously forever**. Pattern now lives in one constant. |

Plus five more live defects: `detectCores()` guarded in `parallel_pool()` but not
in the four other paths that divide it; `s3_analyze(2.5)`/`save_rawbatch(2.5)`
converting **before** validating, so the guard never saw the bad value; an
existing-but-wrong `dev_path` still loading the wrong package; a future-dated
file being fresh **forever** (negative age passes `age <= 24`); and the per-item
`unlink()` destroying a log **before** the output contract was checked.

The lesson this project keeps re-learning: **a claim is not a check.** "Bounded"
in a comment; a test that exercises a dependency instead of your own code; an
assertion that a *symptom* is absent rather than that the *cause* is fixed —
all three are assertions wearing verification's clothes.
- The 002 run was **killed at 09h04m into s1** (2026-07-17 14:11) rather than
  waiting it out: it would have finished carrying every defect below, and
  Phases 2-3 rewrite the dispatcher underneath it anyway.
- **`recompute_baselines()` repairs `arm_labels` in an existing plan** — no s3
  rerun needed.

---

## The diagnosis (why this project exists)

On 2026-07-16/17 an engine review — plus four adversarial `codex` rounds at
`model_reasoning_effort=high` — turned up six defects. **Every one is a contract
failure at the parent/child process boundary. Not one is a scheduling bug.**
All six are now **reproduced, fixed, and covered by a test demonstrated to fail
without the fix.**

| defect | the contract it breaks | proof it was real |
|---|---|---|
| `arm_labels` silently dropped | item -> function | test fails without the fix, naming `arm_labels` |
| `daemons(n)` on the default profile | resource ownership | caller holding 2 daemons left holding **0** |
| stdout/stderr pipes never drained | I/O | 1 KB: 0.7s. **100 KB: never returned** |
| `n_workers = 0` busy-spins forever | input | timed out (exit 124), no error, 100% CPU |
| invalid `dev_path` runs stale installed code | environment | fell through to `system.file()`, empty path |
| 10 writes bypassing the atomic writer | output | guard fails on any reverted site |

> **Retraction (2026-07-17).** A seventh defect was claimed and is **false**:
> "`call_mirai(h)$value` is dead, so every `save_rawbatch()` worker error is
> silently swallowed as success" — billed at the time as the most severe finding
> of the whole review. The review asserted mirai removed `$value` in 0.2.0; that
> was checked against mirai's *documentation*, which mentions only `$data`, and
> the absence was read as removal. Run against mirai 2.7.1, both bindings exist,
> are `identical()`, and the original guard fires on failure and stays quiet on
> success. The result contract was never broken.
>
> It reached two pushed commits before one `Rscript` refuted it. The rule it cost
> us: **a doc's silence is not a verification** — and every defect in the table
> above is now backed by a run, not a reading, which is why each has a "proof"
> column. What survived the retraction is real but smaller: mirai was genuinely
> undeclared, `save_rawbatch()` genuinely hijacked the default daemon profile
> (now defect #2), and the failure path genuinely had no test — now
> `tests/testthat/test-mirai_error_contract.R`.

Six for six. That is the whole argument for this project. The engines are fine;
the boundary they cross is unspecified, so each crossing invents its own rules and
each set of rules is wrong somewhere.

The 100-line `tests/testthat/test-worker_arg_parity.R` is the tell: it is a regex
parser that reads worker *source code* to check `<arg> = params$<field>` against a
function's formals — and its third test asserts `parsed$arg == parsed$field`, i.e.
the mapping must always be the identity. So 127 lines of dispatch scripts plus 100
lines of regex are a hand-written, regex-verified implementation of `do.call()` —
and it still missed `arm_labels`, because it guards worker->formals while the live
bug is builder->worker.

## The two workload shapes (measured, not asserted)

The three engines are not three needs. Git says: everything was `callr`; on
2026-05-15 `8d6b3e0` ("All-subprocess s1 architecture (OOM fix + clean
dispatcher)") deliberately migrated the *hardest* stage off `callr` onto
`parallel_pool`, removing 1 `callr::r_bg` and adding 6 `parallel_pool()` calls; the
`callr` block in `process_skeletons()` is simply **the one file that sweep missed**.
`mirai` arrived the next day for a genuinely different shape.

There are exactly **two** shapes, and they are real:

| | shape A | shape B |
|---|---|---|
| stages | skeleton, s1a-s1d, s2, s3 | `save_rawbatch()` |
| parent's role | holds nothing | **is the producer** |
| the item is | ~11 KB of paths + config | the data slice itself |
| distinction | items already exist | items generated lazily, under backpressure |
| engine (pre-refactor) | `parallel_pool` (processx) | `mirai` bounded queue |

Measured, live, on the box: shape A's items are **10,905 bytes** each (2,194
tempfiles, 26 MB total) — paths and config; the worker opens its own data. Shape B
cannot use `parallel_pool`: it materializes **every** item to a tempfile before
launching any worker (`R/parallel_pool.R:80-85`), which on an I/O-bound bulk write
means 2x the I/O and the whole dataset on disk twice. `mirai`'s
`max_inflight = 2L * n_workers` is the bounded producer-consumer that shape B needs.

**Not adopting crew/future/batchtools/targets.** Worker reuse — their headline
feature — is the one thing shape A must *not* have: s3 peaks ~20 GB/worker and R
does not return that to the OS, so fresh-process-per-item **is** the memory
strategy. And the measured upside is small: R startup + swereg namespace load is
~0.5s (bare R 0.42s), so s1c = 39,492 items x 0.5s / 6 workers ~= 55 min of a ~10h
stage ~= **9%**. The hypothesis that reuse was worth 30-50% was tested and died.

## The contract

Write this down before any code. It is the deliverable; the package is packaging.

**Target** is a *descriptor*, never a function, name, or closure:

```r
.batch_target(package, symbol, version = NULL)   # + signature/hash where feasible
```

Package-name-plus-symbol is insufficient on its own: development code, installed
code and cache identity can differ. `R/r6_tteplan.R:5929` already admits cache
identity records only `packageVersion("swereg")` while dev workers may load
unversioned edited code. A contract that does not fix this preserves the
stale-code/resume problem under a new name.

**Item** contract:

- a named list; **every** name is a formal of the target
- **every formal is named — including optional ones.** Not just the required ones:
  `arm_labels` is an *optional* formal, so a rule that only demanded required
  formals would have missed it exactly as the regex test did. See Decisions.
- no positional arguments, no duplicate/blank names
- targets containing `...` are **prohibited** — arbitrary dots are incompatible
  with reliable typo detection
- **executor configuration is never mixed into the arguments.** Today
  `R/parallel_pool.R:80` injects `swereg_dev_path` and `n_threads` into every item
  even when they are not target formals. Envelope instead:
  `list(meta = list(target, threads, dev_path, ...), args = item)`

**Validation happens at both ends.** Parent validation is early UX; child
validation is correctness, because the child may load a different package version,
or the parent may be dev-loaded while the child falls back to installed code, or an
input may be replayed independently. Parent validation must check **every** item,
not `items[[1]]` — item schemas are already legitimately heterogeneous (some s3
items carry `subgroup_var`, others rely on the default).

**Result envelope**: protocol version, item id, status, value-or-structured-error,
target identity, captured warnings/log metadata.

### What the contract does NOT promise

Stated explicitly, because the tempting version of this promise is unimplementable:

- **Atomicity is scoped to the envelope.** The executor can atomically commit its
  own return envelope. It **cannot** transactionally commit a target that writes a
  raw file, an imputed file, a counts sidecar and a sentinel. Such targets own their
  own commit protocol (sentinel), or return a **commit plan the parent executes**.
- **Atomic rename is not durability.** `file.rename()` is atomic on POSIX and
  server-side atomic on SMB/CIFS; it is not an fsync. `R/qs2.R:44`'s
  `path.tmp<PID>` is also not collision-proof, and replacement semantics vary by
  filesystem. The prose there overstated the guarantee at the time; fixed in Phase 1
  — `qs2_write_atomic()`'s docs now state it is not durability and not a lock.
- **Semantic failure is the consumer's call.** Some swereg targets catch analysis
  errors and return `list(skipped = TRUE, ...)` as a *successful* result. "Loud
  failure" requires swereg to decide which domain conditions are failures; a
  generic executor cannot infer it.
- **Worker-count and thread policy are the consumer's.** `SWEREG_N_WORKERS_<STAGE>`
  (`R/default_n_workers.R:41`) encodes stage-specific RAM policy and is baked into
  the production image's `$R_HOME/etc/Renviron`. The executor takes a validated
  integer; it does not choose one, and it must not silently set data.table/BLAS
  threads.

## The six defects, verified

Fix order is deliberate: correctness before genericity. "Verified" here means
**run**, not reasoned about — see the retracted sixth above for why that
distinction earned its own paragraph.

1. **`arm_labels` silently dropped — affects delivered output.** The builder
   (`R/r6_tteplan.R:2109`) computes `arm_labels = .lookup_arm_labels(self$spec, eid)`
   into the item; the target accepts it (`R/r6_tteplan.R:7185`, default `NULL`);
   `inst/worker_s3_enrollment.R:7` **never forwards it**. `s3_analyze()`
   (`R/r6_tteplan.R:2005`) calls `parallel_pool` **unconditionally** — there is no
   `n_workers == 1` serial branch — so this happens on every s3 run, while
   `recompute_baselines()` (`R/r6_tteplan.R:2443`) calls the same target directly
   *with* `arm_labels`. Two methods that build the same Table 1 disagree; the
   pipeline runs the broken one. Impact is labelling, not numbers: Table 1's
   comparator/intervention headers default instead of coming from the spec.
   **`recompute_baselines()` is the repair path** — no s3 rerun needed.

2. **`save_rawbatch()` hijacks mirai's default daemon profile.** It calls
   `daemons(n)` on the default profile and later `daemons(0)`, which resets and
   destroys any daemon configuration the caller already had. mirai's own guidance
   to package authors is to leave daemon settings to users, or claim a unique
   compute profile for dedicated internal resources. (This is what survives of
   the retracted `$value` finding: the *result* contract was fine, but the
   *daemon-ownership* contract is genuinely broken.)

3. **Pipe deadlock.** `stdout = "|"` / `stderr = "|"` (`R/parallel_pool.R:103`) are
   only read after the child exits (`:116-117`). A chatty child fills the 64 KB pipe
   buffer, blocks on write, never exits, and stays `is_alive() == TRUE` forever.
   **This looks exactly like a hung worker.** Redirect each worker to a bounded log
   file — simpler than draining, and it gives diagnostics for free.

4. **`n_workers` is never validated.** `n_workers = 0` gives
   `floor(n_cores / 0)` -> `n_threads = Inf` (`R/parallel_pool.R:61`) and the
   dispatch loop busy-spins at 100% CPU forever, because the `Sys.sleep(0.1)` sits
   inside `if (length(active) > 0L)`. Validate a finite, length-one, positive
   integer before any division or destructive state clearing; handle
   `detectCores()` returning NA.

5. **Invalid `dev_path` silently runs stale code.** `R/parallel_pool.R:40` — a
   non-NULL but nonexistent `dev_path` makes `is_dev` FALSE and falls back to the
   *installed* package instead of erroring. Check the source tree's DESCRIPTION
   name matches the target package.

6. **Non-atomic final writes + mtime-trusting resume.** Several workers
   `qs2::qs_save()` straight to final paths (e.g. `R/r6_tteplan.R:7132`) while
   resume trusts existence/mtime -> a killed worker leaves a torn file a later run
   **skips**. Adding a timeout makes this *more* likely, so atomicity comes first.

Also: **`mirai` and `devtools` are both undeclared.** `Suggests:` is not empty (11
packages); mirai is simply missing from it, and `inst/worker_bootstrap.R:10` uses
`devtools::load_all()`. mirai belongs in **Suggests** — parallelism is opt-in,
default is serial, absence already errors clearly. Note `pkgload` would merely
change *which* dev dependency is undeclared; only removing source-tree dev
execution removes the dependency.

And `R/parallel_pool.R:75` `on.exit()`-unlinks every input tempfile on any exit
including error, so the replay command `.check_worker_error()` prints names a file
that is deleted as the error unwinds. Retention policy must keep the **failed**
input — and only that one, in a caller-chosen secure directory with restrictive
permissions, because these items can carry registry data.

---

## Plan

### Phase 0 — design (no reinstall needed). THIS DOC.

Write the contract and the defect list down. Done when this file is agreed.

### Phase 1 — correctness. Resubmitted after review round 1.

| # | fix | test | file |
|---|---|---|---|
| 1 | worker forwards `arm_labels` | worker must forward **every** formal its target accepts | `test-worker_arg_parity.R` |
| 2 | named compute profile `swereg_rawbatch` | **AST of `save_rawbatch()` itself**: every `mirai::daemons`/`mirai::mirai` call carries `.compute` | `test-mirai_error_contract.R` |
| 3 | stdout/stderr -> per-item log file; tail reads only the last 64 KB **from the end**; successful logs reclaimed per item | 512 KB per stream completes; a 4 MB failing log reports a useful *and* bounded tail; no logs left after a run | `test-parallel_pool_io.R` |
| 4 | validate `n_workers`; guard `detectCores()` returning `NA` | `0`/`-1`/`NA`/vector/`"2"`/`NULL` all error | `test-parallel_pool_io.R` |
| 5 | a missing `dev_path` errors instead of falling back | nonexistent dev path errors, naming `swereg_dev_path` | `test-parallel_pool_io.R` |
| 6 | 10 sites routed through `qs2_write_atomic()`; unique temp name | no `qs2::qs_save()` to a final path; real `kill -9` mid-write | `test-qs2_write_atomic.R` |
| 7 | s2 resume ages **each** file, not `max(mtime)` | a 1h-old file must not validate a 100h-old one | `test-resume_fresh.R` |
| 8 | `mirai (>= 0.8.0)` pinned; `devtools` declared | — (DESCRIPTION) | — |
| 9 | one guarded `.safe_n_cores()`/`.threads_per_worker()`; all 8 sites routed | mocked `detectCores() == NA`; **no direct `parallel::detectCores()` anywhere** | `test-worker_count_validation.R` |
| 10 | `.validate_n_workers()` at every entry: validate **then** convert | `s3_analyze(2.5)` / `save_rawbatch(2.5)` rejected, not truncated | `test-s3_n_workers.R`, `test-worker_count_validation.R` |
| 11 | `dev_path` must be a tree whose DESCRIPTION says `swereg` | a tree naming another package is rejected | `test-parallel_pool_io.R` |
| 12 | freshness requires a **non-negative** age | a future-dated file is not fresh forever | `test-resume_fresh.R` |
| 13 | output validated **before** its log is reclaimed | a worker exiting 0 with no output still reports its log | `test-parallel_pool_io.R` |

| 14 | `collect = TRUE` preserves a `NULL` result (`results[idx] <- list(x)`, not `[[<-`) | out-of-order completion with a mid-list `NULL`: length and positions hold | `test-parallel_pool_io.R` |
| 15 | validate `n_workers` at **all six** entries, before any destructive step; `default_n_workers()` rejects bad config instead of repairing it | ordering guard on each method body; config `2.5`/`0`/`"abc"` rejected | `test-n_workers_entry_points.R`, `test-worker_count_validation.R` |

`.pp_log_tail()` was also lifted out of `parallel_pool()`'s closure so it can be
unit-tested at all, and hardened against non-text worker output: an embedded NUL
made `rawToChar()` error, the `tryCatch` swallowed it, and the caller reported
"(no output captured)" for a worker that had said exactly what was wrong. Its
boundedness is now pinned by a **source guard** (no `readLines()` in the
function) as well as the portability-fragile timing test. Two roxygen blocks
split by the `.pp_log_tail()`/`.resume_fresh()` extractions were repaired, and
`parallel_pool()`'s docstring no longer overstates its transport ("avoids
serialization entirely" → qs2-vs-RDS file transport).

Also: `mirai` declared in `Suggests` (it was in no field at all), and
`qs2_write_atomic()`'s temp file moved from `paste0(path, ".tmp", Sys.getpid())`
to a `tempfile()` in the destination directory — PIDs are unique only among live
processes **on one host**, and this data lives on a share two hosts mount at once.
Its docs now state what it does *not* promise (it is not durability; it is not a
lock).

**Every fix landed only with a test demonstrated to fail without it**, and every
defect was **reproduced before being fixed**. That is not ceremony: every false
finding this project produced came from reasoning about code instead of running
it, and one of them (the retracted `$value` claim) reached two pushed commits.

Not fixed here, deliberately — and stated narrowly, because the first version of
this paragraph overclaimed. Atomic writes establish exactly one thing: **a
process interrupted from now on cannot create a torn final path.** They do *not*
establish that a file:

- came from the current inputs, spec, target body, or package version;
- was not left torn by a run predating this fix;
- survived a machine or mount failure (rename is not `fsync`);
- belongs to this run at all.

So **resume is sound only when inputs and code are unchanged**, and that is now
what the docs say. Per-file ageing (#7) removes the worst of the heuristic's
dishonesty, but mtime remains a proxy. Completion records tied to input/target
identity are Phase 2.

Also deferred, unchanged: per-item timeouts (the pipe, atomicity and validation
work they depend on is now done), eager item materialization, the
`process_skeletons()` one-snapshot payload, and `parallel_pool()`'s export /
swereg-naming / metadata injection.

Then run `recompute_baselines()` on 002/003/006 to repair Table 1 without an s3 rerun.

### Phase 2 — one runner, inside swereg

New `R/batch.R`, package-neutral from line one, zero R6/domain imports:

```r
.batch_target(package, symbol, version = NULL)
.batch_run(target, items, workers, ...)             # items already exist
.batch_stream(target, ids, producer, workers, ...)  # producer(id) -> item, lazily, bounded
```

`ids`, not a bare count: failures, retries, progress, retained inputs and output
records all need stable identities. Plus `inst/batch_worker.R` (~10 lines): read
envelope -> resolve target -> re-validate -> `do.call` -> write result envelope.

Two frontends over **one** engine. They share target resolution, validation, result
envelopes, lifecycle and failure semantics; they may differ in internal transport.
Do not expose "mirai vs processx" as two public conceptual models.

The executor owns a **private, matched** IPC codec derived from the atomic-write
logic. `qs2_read()`/`qs2_write_atomic()` stay in swereg as persistence for
scientific files. In particular `qs2_read()`'s `check_version()` duck-type
(`R/qs2.R:22`) stays swereg behaviour — a generic executor duck-typing arbitrary
environments for a `check_version` member is hidden swereg policy masquerading as
an extension point.

### Phase 3 — route everything through it, then lock the door. **DONE (as-built below).**

Executed 2026-07-18, all in one session. Actual commit order: inventory ->
s3 ETT -> s2 -> s1a-s1d -> save_rawbatch -> process_skeletons -> deletion ->
lockdown. (The retrospective had said save_rawbatch FIRST; in execution the
mechanical shape-A swaps landed before it. The ordering risk that rule guarded
against -- stopping early with the shape-B proof still missing -- never
materialised because the whole phase landed in one sitting, but the record
should say what happened, not what was planned.) Each worker was deleted in
the same commit as its migration, with a production-boundary test at every
migrated caller.

0. **Migration inventory before any edit** — one row per call site: target,
   exact formals, item ids, `collect`, thread handling, resume interaction.
   This is what surfaced the two s3 landmines *before* the swap: the ETT items
   said `n_threads = n_cores` and RELIED on `parallel_pool()` overwriting it
   (carried verbatim to the thread-agnostic runner = every worker
   oversubscribed), and ordinary ETT items omitted the optional `subgroup_var`
   (the every-formal rule rejects that). Both fixed and pinned proven-red by
   `test-s3_item_contract.R`.
1. s3 ETT loop, then s2, then s1a/s1b/s1c/s1d -> `.batch_run`
   (`worker_s3_enrollment.R` was already orphaned by Phase 2). Stable,
   meaningful ids everywhere: skeleton basename, enrollment id, analysis-file
   basename, `enrollment__skeleton` for the 39,492-item s1c stage — a failure
   names the exact unit of work, not "Worker 17384". `inst/worker_s1a.R` and
   its target `.s1a_worker()` were confirmed dead and deleted, not ported.
2. `save_rawbatch` -> `.batch_stream` (the shape-B production proof Phase 2
   lacked), on the dedicated `swereg_rawbatch` profile, via the new
   `.rawbatch_write_worker` target — the ONE rawbatch write path in both
   modes. **Serial stays in-process, deliberately:** `n_workers = 1` (the
   default) calls the same target directly — no process boundary means no
   dispatcher and no mirai requirement, so mirai stays a Suggests that only
   parallelism needs. The daemon-side hand-inlined atomic-write copy (which
   had already drifted once) is gone; the target uses the real
   `qs2_write_atomic()`.
3. `process_skeletons` -> `.batch_run` via the new `.process_one_batch_snapshot`
   target (NOT `.process_one_batch` directly — its formals take the study
   object). The study snapshot is written **once** per run and each item
   carries only its path plus small scalars. `registrystudy.qs2` is 5.7 MB;
   `callr` serialized it only for *launched* batches (~6 in flight ~= 34 MB),
   whereas a naive translation to the eager-materialising runner would
   serialize it for all 2,194 items up front ~= **12.5 GB**. A 350x
   regression, pinned structurally by `test-batch_skeletons_production.R`:
   one snapshot shared by all items, present at dispatch, cleaned on unwind,
   every item < 50 KB.
4. Deleted: `R/parallel_pool.R`, the `callr::r_bg` block, all eight
   `inst/worker_*.R` dispatchers plus `inst/worker_bootstrap.R`, the `callr`
   Import, `export(parallel_pool)`, `tests/testthat/test-worker_arg_parity.R`
   and `test-parallel_pool_io.R` — with the surviving guarantees ported:
   `.pp_log_tail()` moved into `R/batch.R` (unit tests follow in
   `test-batch_log_tail.R`), and chatty-worker-no-deadlock + per-item log
   reclaim re-proven against `.batch_run`. The no-direct-`qs_save` guard
   widened from `r6_tteplan.R` to ALL of `R/` except `qs2.R` and `batch.R`.
5. **The step that makes this real** — `test-batch_lockdown.R`. NOT the
   three-line grep first sketched here (it would match the historical
   comments that legitimately mention the dead engines, miss `inst/`, and
   miss qualified *references* like `processx::process$new`): a parse-based
   AST walk over `R/` AND `inst/` that bans any `processx::`/`callr::`/
   `mirai::` **mention** outside `R/batch.R`, plus the parallel-package
   process spawners by name. `system()`/`system2()` stay legal (metadata
   shell-outs — the git SHA in the summary filename — are not work dispatch),
   as does `parallel::detectCores()` (a count, routed through
   `.safe_n_cores()`). Non-vacuous both ways: the walker must FIND the engine
   calls inside `batch.R`, and the test was proven red against a planted
   `callr::r_bg`. Companion assertions: no `inst/worker_*.R` can reappear,
   `callr` cannot silently return to DESCRIPTION, `parallel_pool` is gone
   from the namespace — not merely unexported.

This is the enforcement. A package boundary is *not* access control — `:::` exists,
and swereg could always call processx directly. What enforces a contract is making
one dispatcher unavoidable, validating at both ends, and testing that no bypass
exists.

### Phase 4 — extract `batchit` (NOT mechanical; consolidate first, shrink second, extract third)

`batchit` is **free on CRAN** (`batchtools`, `batch`, `BatchJobs`, `batchmix` are
taken). Extract only when **`tte` has a real call site exercising the same
contract** — a hypothetical second consumer is not enough. Then it is largely
`git mv R/batch.R` + `inst/batch_worker.R`, leaving a thin swereg adapter (target
selection, progress labels, dev-vs-install policy). No cycle: swereg Imports
batchit; the child loads the *named consumer package* at runtime — plugin loading,
not a static dependency.

> **Exception (2026-07-18, maintainer-directed).** The "extract only when `tte`
> has a real call site" rule above was overridden by explicit maintainer direction
> to complete Phase 4 now. The extraction proceeded with no `tte` and no second
> consumer in existence; the runner-vs-consumer seam was proven with a synthetic
> throwaway consumer package in `batchit`'s test suite instead of a real second
> consumer. Recorded here so the rule is not read as silently violated.

**Not fully mechanical — one seam had to be split at extraction time.** This was
the plan, written before extraction; the reasoning is kept because it explains
*why* the seam exists. Phase 2 deliberately fused it, since runner and consumer
were both `swereg` at the time: `.batch_worker_script()` looked for the worker
script *in the consumer's dev tree*, and `.batch_stream`'s dev branch `load_all`ed
only the consumer. Once `batchit` became the runner, the worker script had to come
from the **installed runner** while `dev_path` stayed the **consumer's** tree, and
both the processx worker and the mirai daemon had to load *both* packages.
`meta$runner_package` already carried the distinction; the loading logic was what
changed.

**As built (2026-07-18):** that seam IS split. `batchit`'s
`.batch_worker_script()` always resolves `inst/batch_worker.R` from the **runner's**
own `system.file()` — never the consumer tree — and both the worker and the mirai
daemons load BOTH the runner and the consumer. `meta$runner_package` is **required**
(non-empty, validated pre-load in the worker and in `.batch_check_envelope`,
hardened in `batchit` `235174f` after this reviewer's round-1 blocker). swereg is a
thin adapter (`R/batch_adapter.R`) whose three wrappers forward to `batchit::`.

**Sequencing — planned vs as-built.** The recorded plan (retrospective,
2026-07-18) was **consolidate -> run production -> SHRINK -> extract**: do not
simplify the signed-off dispatcher while migrating callers (Phase 3 honoured
this), and only once Phase 3 had survived a real production run should `batch.R`
be shrunk, *before* any extraction. **As-built (2026-07-18): the production-run
step was skipped by the same maintainer direction that ordered Phase 4 now** — no
full MHT production rerun happened after Phase 3. The shrink then ran and
extraction followed (see the as-built note after the candidate list). The agreed
shrink candidates, from the joint retrospective (all now executed):

- **Retention** (`.batch_retain_failure` + `keep_failed_dir`): the record adds
  little over the surfaced error and replay is by regeneration. The
  never-persist-argument-VALUES rule is MHT governance and stays wherever the
  code lands; the fail-closed chmod machinery is swereg/MHT policy, not
  generic-runner material — move it to the swereg adapter at extraction, or
  drop the feature if production never uses it.
- **mirai profile ownership**: generate a unique private profile name per
  invocation and tear it down, instead of caller-selectable `compute` +
  `daemons_set()` ownership proof — deletes the collision policy, the
  fail-closed predicate, and several tests, while keeping the one rule that
  matters (never touch the default profile).
- **`inst/batch_worker.R`** from 119 to ~30-50 lines: read once, validate only
  the fields that decide what to load, load, execute, write. A load failure is
  already a supported failure channel via nonzero exit + the per-item log.
- **Prune the review archaeology** from `batch.R`'s comments (keep the "why",
  move the history here); a shared envelope constructor for the two frontends.

**As-built (26.7.24, commits 0c7e132 + nonce fix 1d6e41c):** all four candidates
executed — failure retention dropped entirely; the mirai profile became a private
per-invocation name under a high-entropy session nonce (the plain per-invocation
counter of the first cut was an interim-review blocker, fixed in 1d6e41c);
`inst/batch_worker.R` slimmed (119 → ~72 lines); and both frontends now build the
wire envelope through one shared `.batch_input_envelope()` constructor. Extraction
(af9f30d + 6e775ec) followed the shrink, with the production-run precondition
skipped as noted above.

An honest complete implementation is ~450-650 lines + a 30-50-line worker (the
~250-400 first written here was optimistic; the current ~805 code lines
overshoot the honest figure, not just the optimistic one).

**And the standing rule: if `tte` never materialises a real call site,
`batchit` is never created.** A package is not built to validate an
architecture.

> **Superseded 2026-07-18 (maintainer-directed).** This standing rule was
> overridden: `batchit` was created on explicit maintainer direction with no `tte`
> and no second consumer in existence. The rule's text stays as the record of what
> was in force; the seam it protected was instead validated by a synthetic
> throwaway consumer package in `batchit`'s tests rather than a real second
> consumer.

**Manifest for that day** (updated post-Phase 3): SPLIT `batch.R` (scheduling/
lifecycle/IPC/validation move; target selection + progress labels + dev policy
stay in a swereg adapter). The old engines and workers are already deleted.
**STAY in swereg**: `default_n_workers.R` (deployment policy),
`progress_handlers.R` (UI/session policy), both `qs2.R` functions,
`r6_candidate_path.R` + `path_resolution.R` (where files live is not how work is
batched), `validation_helpers.R` (none of its 432 lines is batching), the
`.rawbatch_write_worker` / `.process_one_batch_snapshot` targets (consumer
code), everything domain.

---

## Decisions (settled 2026-07-17, Richard)

- [x] **Every formal must be named, including optional ones.** This is what makes
      the contract catch `arm_labels` rather than merely document it: `arm_labels`
      *is* an optional formal (`arm_labels = NULL`), so a rule that only requires
      *required* formals would have missed it exactly as the regex test did. The
      bug's shape is "an optional arg silently absent, target takes its default" —
      which is indistinguishable from a deliberate default unless every formal is
      explicit. **Migration cost, accepted:** every builder must pass
      `subgroup_var = NULL`, `arm_labels = NULL` etc. explicitly, and adding an
      optional formal to a target becomes a breaking change until every builder is
      updated. That cost is the feature.

- [x] **Target identity = package + symbol + body hash.** `digest(list(body(fn),
      formals(fn)))` — the mechanism already exists in this package:
      `.hash_function()` in `R/r6_registrystudy.R` uses exactly this for skeleton
      phase replay, and it is empirically verified that comments do not move the
      hash while a one-token code change does. This closes the hole
      `R/r6_tteplan.R:5929` already admits, where cache identity records only
      `packageVersion("swereg")` while `swereg_dev_path` workers execute arbitrary
      edited code under an unchanged version string. Accepted consequence: any real
      edit to a target invalidates its cache. That is correct, and it is what
      already happens for skeleton phases.

- [x] **Retention: the runner never persists the argument VALUES. Replay by
      regeneration.** Keep the target descriptor, item id and field *names* — never
      the argument *values*. This is why `batch_stream()` takes `ids` rather than a
      bare count: with a stable id, replay does not need retention at all — **re-run
      the producer for that id and regenerate the item.** The builders are pure
      functions of (spec, skeleton list, index), so items are reproducible on
      demand. Shape B items *are* patient data, and `/tmp` on a shared box is not an
      acceptable home for them.
      **The honest boundary of this guarantee** (found in review round 2): the
      persisted `error` field is the target's OWN message, passed through verbatim,
      and a target that writes a value into its `stop()` exposes that value itself —
      the runner can't tell a data value from a diagnostic in free text. So the
      worker's stdout/stderr tail (which reliably prints whole slices) is **never**
      persisted, and the failure dir is `0700`/`0600`; that permission is the
      safeguard for whatever a target chooses to emit. The runner's own promise is
      narrower and true: it never writes the argument *values* to the retained
      failure record. (Shape A does serialise the input envelope — args included —
      to an OS tempfile for transport, but that is transient: `on.exit`-unlinked
      when the pool returns, never copied into the failure directory. The failure
      record holds only names + id + target + the target's error message.)
      Retention also **fails closed**: if the directory/file cannot actually be
      chmod'd to 0700/0600 (e.g. a mount that ignores it), no record is written.

- [x] **Single-writer: not enforced, and not in this contract.** One operator; none
      of the six defects is a concurrency bug; and a run-level lock is the wrong
      layer for an item-level executor. If it ever bites, it belongs in
      `bin/tte.sh` — which already writes `current_stage.txt` — not here. Noted
      rather than built, deliberately: this is a single-operator serial pipeline,
      not a distributed system. (Real but unrealised scenario: bench and uppsala
      both mount the same Argos share, and `claude rc` is a second entry point.)

- [x] **`.libPaths()` propagation is forced, not chosen.** `--vanilla` does not
      reproduce the parent's runtime library path, and it *cannot* travel in the
      payload — the child needs `qs2` to read the payload in the first place. It
      must be set as `R_LIBS` in `processx::process$new(env = )`, before startup.

- [x] **Timeout:** per-item configurable, **generous default** (`.BATCH_DEFAULT_TIMEOUT`,
      6 h — a hang-catcher, `Inf` disables), not a hardcoded short value.
      Log capture is bounded **in RAM**: workers write stdout/stderr to a per-item
      file (not a pipe — the pipe buffer is what deadlocked a chatty worker), and
      only the last 64 KB is ever read back (`.pp_log_tail`), so a huge log never
      OOMs the parent. The on-disk file is transient (unlinked per item) and its
      size is bounded in practice by the timeout; there is no fs-level byte cap
      (a truly pathological infinite-printer is caught by the timeout, not a cap).

## Open

- [x] **Is `inst/worker_s1a.R` dead?** CONFIRMED and deleted (Phase 3, 2a46b30):
      no production call site selected it since the multi-enrollment scout
      landed; its target `.s1a_worker()` went with it (all shared helpers keep
      their other callers).

## Understanding checklist (for Richard)

### 1. The problem
- [ ] Why all six defects are the *same* bug: an unspecified boundary.
- [ ] Why the parity test passes while `arm_labels` is dropped (it guards the
      wrong half: worker->formals, not builder->worker).

### 2. The solution
- [ ] Why the contract, not the package, is the deliverable.
- [ ] Why validation must happen in the child too, not just the parent.
- [ ] Why "atomic output" can only ever mean "atomic envelope".

### 3. Broader context
- [ ] Why two shapes justify two transports but one contract.
- [ ] Why the rule had `batchit` wait for `tte` to have a real call site — and
      why the maintainer overrode it (2026-07-18) to extract without one.
