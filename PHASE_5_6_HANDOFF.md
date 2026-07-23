# Phase 5′ / 6′ handoff (batchit / swereg)

Self-contained brief for the next session. Written 2026-07-20 after Gate 0 passed.
**Authoritative design doc: `~/swereg/PROJECT.md`** (STATUS line + the Phase 5′/6′ sections
around lines 685–753). Read it first. Also read memory `[[swereg-split-batchit]]` and
`[[swereg-contract-bugs]]`. This file summarizes and gives the execution plan; if it and
PROJECT.md ever disagree, PROJECT.md wins.

## Where we are (all committed)

- **Gate 0 PASSED** (2026-07-20): full 002 pipeline (spec v010) ran end-to-end on uppsala through
  the new batchit dispatcher, every stage rc=0. Durations s1 13.8 h, s2 4.5 h @2, s3 15.5 h @2, ~35 h
  wall. Regression-checked vs v009 (proxy): differences all explained by v010's two new exclusions;
  no dispatcher artefact.
- **Phases 0–4 DONE.** `batchit` 26.7.19 + `swereg` 26.8.0 installed; both CIs green; swereg suite
  1565/0/0. swereg HEAD `80fc959` (clean). batchit is `papadopoulos-lab/batchit` (HEAD `235174f`).
- **Doctrine (settled, maintainer-directed):** NO caching in batchit or the TTE stages. Caching stays
  ONLY in swereg generic (rawbatch skip-if-exists + skeleton phase-replay). The v3 provenance contract
  is SHELVED as a PROJECT.md appendix (the designed fallback if production later shows pain).
- **MHT repo (`~/structural-mht-registry-data`, HEAD `657b424`, pushed):** worker counts hardcoded
  per project (skeleton=6, s1=6, s2=3, s3=3), env vars gone; bench retired → uppsala-only (dropped
  `/data/argos` candidates and the `~/papadopoulos/swereg` dev-path branching from every stage script).
  **Consequence for Phase 5′/6′: the pipeline now ALWAYS loads the INSTALLED swereg.** To test a swereg
  change against the pipeline, `pak::pak("papadopoulos-lab/swereg")` to reinstall — do NOT re-add a
  dev-path. Reinstalling swereg/batchit while a stage runs is unsafe; nothing is running now.

## Phase 5′ — DELETE the TTE resume/cache heuristics (do this FIRST)

Deletion, not construction. Simpler than 6′, and it removes code 6′ would otherwise have to preserve.
Strip the three per-stage cache/resume heuristics; leave rawbatch + skeleton caching untouched.

- **swereg** (`R/r6_tteplan.R`, `R/r6_registrystudy.R`):
  - s1: the `resume=` path in `s1_generate_enrollments_and_ipw()` — the s1a/s1b/s1c/s1d sentinels
    (`.s1a_done_path` … `.s1d_done_path`), the `work_dir` cache clear-on-`resume=FALSE`, the "committed
    skeleton manifest" guard. Remove the `resume` parameter entirely.
  - s2: the `resume=` path in `s2_generate_analysis_files_and_ipcw_pp()` — `.resume_fresh()` + the 24 h
    window + skip-if-`file_analysis`-exists. Remove the `resume` parameter.
  - s3: the skip-if-cached logic in `s3_analyze()` (cache keyed on presence) + the `force=` parameter.
    Decide: either always recompute, or keep an explicit recompute with no cache short-circuit.
- **MHT repo** (`002/003/006` `s1.R`/`s2.R`/`s3.R`): remove the now-dead `resume=`/`force=` arguments;
  update the CLAUDE.md notes that describe the s1_work `rm -rf` trap and the resume caveats (those
  hazards disappear with the heuristics).
- **Accepted costs** (maintainer-accepted, state them in the PR): a killed s1 restarts from zero (no
  14 h resume); a mid-run s3 crash = full recompute.

## Phase 6′ — batchit output-commit + `batch_fn` (BUILD, after 5′)

Absorb the non-caching conveniences into batchit; still NO staleness (no fingerprints/plans/sidecars).

- `batch_task(work, outputs, style = return | staged_writer | external_writer)` — child-side atomic
  writes (final paths leave the target's formals), **item-level all-or-none commit**, existence
  verification. `staged_writer` = target streams into a batchit-supplied temp dir, batchit renames on
  success. See PROJECT.md ~747–753 and the table at ~1031 (s1a streaming outputs → `staged_writer`).
- `batch_fn` — dispatch self-contained function VALUES, with a `codetools::findGlobals()`
  self-containedness guardrail at BOTH ends. Justified now that caching is gone (by-value dispatch has
  no parent/child cache-identity skew). For ad-hoc analyses, not production stages.
- Then migrate the swereg TTE stages that currently hand-roll atomic writes onto `batch_task`.

## Execution model (maintainer-directed — follow exactly)

**opus orchestrates, sonnet executes, codex judges.**
- opus (this main session): write self-contained briefs (exact files, contract rules, the proven-red
  test discipline), dispatch sonnet subagents for the actual coding, VERIFY by running the tests and
  reading the diff — never by trusting the subagent's report — and hand completion judgement to codex.
  opus does the `git push` itself (a subagent's push can be denied non-deterministically).
- sonnet subagents: do the edits/coding per brief.
- codex: adversarial gate. On uppsala the codex CLI is installed + logged in — **code-review ONLY,
  never paste registry/health data** (it goes to OpenAI). Run `codex exec` in the BACKGROUND with
  `--skip-git-repo-check -s read-only -c model_reasoning_effort=high`, redirect `< /dev/null`, read the
  log with `head` (the model/effort header is at the top). Budget 10–20 min. Gate each phase.

## Non-negotiable constraints (from `~/.claude/CLAUDE.md` — carry forward)

- **Proven-red:** every deletion/fix ships with a test shown to FAIL without it — revert, run, confirm
  it fails for the stated reason, restore. A green test proves nothing until seen red.
- **Test the REAL boundary:** drive the actual caller → worker/subprocess path, not a helper that
  bypasses production wiring.
- **Deletions get the FULL suite**, not just touched-file tests; and the sweep must cover NON-CODE
  config (`_pkgdown.yml`, CI workflows, README, `.Rbuildignore`).
- **CI is a separate gate** from a green local run — installed-package vs `load_all` asymmetries only
  surface there. Watch `gh run watch` after pushing swereg/batchit.
- **srcref trap:** `digest(body(fn))` includes srcref under `keep.source=TRUE` (interactive) but not
  under `Rscript`; `utils::removeSource(fn)` before any code-identity hash, or installed-vs-dev disagree.
- **swereg test suite:** from `~/swereg`,
  `NOT_CRAN=true Rscript --vanilla -e 'suppressMessages(pkgload::load_all(".")); testthat::test_dir("tests/testthat", reporter="summary")'`
  — grep the output for `SKIP` and confirm only the expected opt-in/`TrialEmulation`-absent skips.

## First moves for the next session

1. Confirm the box is idle (`pgrep -f 'exec/R|Rscript'`) — a stray R was seen 2026-07-20; don't start
   swereg work while a pipeline stage runs.
2. Read `~/swereg/PROJECT.md` (STATUS + ~685–753) and the two memory notes.
3. Start **Phase 5′**: grep swereg for the resume/force/sentinel machinery
   (`grep -nE 'resume|force|_done_path|\.resume_fresh|work_dir' R/r6_tteplan.R R/r6_registrystudy.R`),
   enumerate every deletion site, write the proven-red tests, dispatch sonnet to delete, run the FULL
   suite, then codex-gate. Push, then `gh run watch`.
4. Then **Phase 6′** (build) as above.
