# PROJECT: ITT alongside PP + effect modification (issue \#6)

Running design + learning doc. Two goals bundled this session:

1.  **ITT alongside PP** — every run produces both an intention-to-treat
    and a per-protocol analysis, funneled side by side through all
    results.
2.  **Issue \#6 — effect modification** — stratified IRRs within
    subgroup levels plus an interaction (difference) test.

They are orthogonal at the method level and meet only at the **results
funnel**.

------------------------------------------------------------------------

## The core problem (why ITT isn’t just a reweight)

Per-protocol censoring in this pipeline is **destructive, not a
weighting choice**. `s5_prepare_outcome` (`R/r6_tteenrollment.R:1809`)
deletes every person-trial row after the cut:

``` r
data <- data[get(design$tstop_var) <= censor_week | is.na(censor_week)]
```

`censor_week` includes `weeks_to_protocol_deviation`, so rows after a
person switches arms are gone. ITT needs exactly those rows. Therefore
ITT cannot be recovered from the PP analysis dataset — it must be built
as its own dataset.

**Branch point:** `file_imp` (`R/r6_tteplan.R:4971`) holds the trial
panel with full uncensored follow-up + imputed confounders +
`ipw`/`ipw_trunc`, saved *before* any censoring. Both estimands fork
from it. ITT is the cheaper branch: no IPCW, no GAM, weight =
`ipw_trunc`.

------------------------------------------------------------------------

## The five reasons follow-up ends (and how each estimand treats them)

`s5_prepare_outcome` cuts at the earliest of five reasons:

| Reason        | What it is                                    | Variable                      | PP (today)           | ITT (locked)                          |
|---------------|-----------------------------------------------|-------------------------------|----------------------|---------------------------------------|
| **Event**     | the outcome happens                           | `weeks_to_event`              | cut, counts as event | same                                  |
| **Switch**    | deviate from assigned arm                     | `weeks_to_protocol_deviation` | cut + IPCW-corrected | **ignored entirely**                  |
| **Loss**      | records run out early (emigration, data ends) | `weeks_to_loss`               | cut + IPCW-corrected | cut, treated as independent (no IPCW) |
| **Admin end** | study calendar cutoff (db lock)               | `weeks_to_admin_end`          | cut, no reweight     | same                                  |
| **Horizon**   | chosen max follow-up                          | `effective_follow_up`         | cut, no reweight     | same                                  |

- PP final weight = `ipw × ipcw_pp` (`analysis_weight_pp[_trunc]`).
- ITT final weight = `ipw_trunc` (baseline IPW only).

### Locked decision (2026-06-16)

**ITT = drop Switch, IPW-only, Loss treated as independent censoring.**
Rationale: (1) matches the Hernán/Robins definition — emulated-trial ITT
uses baseline IPTW, IPCW only enters for non-adherence (PP); (2) keeps
the ITT-vs-PP contrast clean — the only difference is “do we censor at
switching,” so any gap is attributable to adherence; (3) cheap, reuses
`ipw_trunc` from `file_imp`.

**Implementation subtlety:** `weeks_to_loss` is currently defined
relative to the PP planned stop (it includes the deviation term via
`.first_planned_stop`). ITT mode must recompute that block with the
Switch term dropped *everywhere*, not just from the final `pmin`.

------------------------------------------------------------------------

## Docs that currently say “ITT not supported” (must flip when ITT lands)

- `vignettes/tte-methodology.Rmd:53,80` — “ITT not possible after this
  step.”
- `vignettes/tte-nomenclature.Rmd:106` — table row “ITT — Not
  implemented.”
- `vignettes/tte-methods.Rmd` — per-protocol manuscript doc; needs a
  parallel ITT estimand section.
- Roxygen on the method that carries the `estimand` switch.

The five-reasons table above is the artifact to drop into
`tte-methodology.Rmd`. Docs land **with** the code, never ahead of it.

------------------------------------------------------------------------

## Validation plan (synthetic data with known truth)

Correctness needs a DGP with known truth (the `.simulate_*` functions in
the test harness), **not** the static `fake_*` package data (no ground
truth). TrialEmulation 0.0.4.11 is installed and supports
`estimand_type` in `{ITT, PP, As-Treated}`.

**Already exists (PP only):** - `test-tte_simulation_correctness.R` —
plants true PP log-IRR (`.true_pp_log_irr`, force A=0 vs A=1 each
period); asserts estimate within 0.10 + CI covers truth. -
`test-tte_vs_trialemulation.R` — same data through
`TrialEmulation::initiators(estimand_type="PP")`; asserts point
estimates within 0.20 + CIs overlap. (Caveat baked in: swereg = Poisson
IRR, TrialEmulation = logistic OR; agree only for rare events.)

**Status (2026-06-16):** TDD spec laid down. `helper-tte_itt.R` (DGP +
both truth fns + OR-\>IRR conversion). `test-tte_itt_correctness.R`:
DGP-separation GREEN, swereg-ITT-recovers-truth RED (pins
`estimand="itt"`). `test-tte_itt_vs_trialemulation.R`:
TE-recovers-both-truths GREEN (peer validated), swereg-vs-TE point+width
RED. Remaining: Monte Carlo coverage (write after Phase 1 so tolerances
tune against real output). Then Phase 1 turns the REDs green.

**Two gaps to close:**

1.  **No ITT validation.**
    - `.true_itt_log_irr()` — `do(A_0=1)` vs `do(A_0=0)` with *natural
      switching thereafter* (forces baseline only); standardized over
      L0.
    - DGP tuned so deviation is real -\> true ITT attenuates vs true PP
      (proves ITT != PP).
    - swereg ITT `$irr()` (IPW-only) recovers ITT truth.
    - `TrialEmulation::initiators(estimand_type="ITT")` agrees on same
      data.
2.  **CI width never directly compared** (only coverage + overlap
    today).
    - **Monte Carlo coverage study** (the rigorous “width is right”
      test): refit over M sims, confirm 95% CI covers truth ~95%. Run on
      ITT (cheap, no IPCW); lean on cross-package check for PP
      (expensive).
    - Cross-package comparison happens on a **common IRR scale**:
      convert TE’s logistic log-OR to a log-IRR via
      `tte_log_or_to_log_irr()` (Zhang & Yu 1998:
      `RR = OR/(1 - p0 + p0*OR)`, p0 = reference-arm per-period risk).
      This removes the OR-vs-IRR scale gap so we compare tightly instead
      of leaning on a wide tolerance.
    - Width guard `|width_swereg - width_TE|` / SE on the (converted)
      log scale, within tolerance.
3.  **TrialEmulation must be validated as a peer, NOT trusted as an
    oracle.** “swereg agrees with TE” is circular if TE is wrong. So
    assert TE *itself* recovers the planted truth on our synthetic data,
    for both estimands.
    - Assert TE point estimate within scale-aware tolerance (~0.10-0.12)
      of planted truth (PP and ITT). Do NOT assert TE’s OR-CI contains
      the IRR truth – that is flaky (see below).
    - **Empirical finding (N=8000, lor=-0.7, persist=8, verified
      2026-06-16):** TE recovers both truths. After OR-\>IRR conversion:
      - **PP** gap -0.022 -\> **-0.006**: the discrepancy was *pure
        OR-vs-IRR scale*; conversion nearly zeroes it. Assert tight
        (~0.05).
      - **ITT** gap -0.072 -\> **-0.061**: conversion removes only
        ~0.01; a ~0.06 residual survives. This is **NOT scale** – it is
        conditional-vs-marginal (OR non-collapsibility): TE adjusts for
        L0 by *conditioning* in the outcome model; truth/swereg target
        the *marginal* effect via IPW. Document a slightly wider ITT
        tolerance (~0.10) for the TE comparison; the tight anchor stays
        swereg-ITT (marginal) vs the marginal truth.
      - TODO once swereg-ITT exists: confirm the ~0.06 TE residual *is*
        exactly the conditional-vs-marginal offset (swereg-ITT should
        recover truth within ~0.10-0.15, tighter than TE’s converted
        estimate).

## Estimands: marginal (swereg) vs conditional (TrialEmulation)

Both packages adjust for the baseline confounder `L0` (a per-person
baseline covariate that drives BOTH treatment and outcome -\> the thing
the adjustment machinery must defeat). They do it differently, yielding
two valid estimands:

- **swereg = marginal.** Removes confounding by IPTW (reweight so
  treatment is independent of `L0`), then fits a covariate-free Poisson
  MSM `event ~ treatment + ns(tstop) + offset`. Coefficient =
  population-average effect, standardised over the whole `L0`
  distribution. This is the marginal structural model (Robins) and the
  natural TTE target (“treat everyone vs none”); it is also exactly what
  our planted truth is (`do(A_0=1)` vs `do(A_0=0)` standardised over
  `L0`).
- **TrialEmulation = conditional.** Puts `L0` *in* the pooled-logistic
  outcome model. Coefficient = effect holding `L0` fixed
  (within-stratum).

**OR non-collapsibility** is why they differ numerically: the odds ratio
(and hazard ratio) marginal != conditional *even with no confounding*
(the odds transform is nonlinear, so collapsing strata shrinks the OR
toward null). The **rate ratio (IRR) is collapsible**: marginal =
conditional. Demonstrated (randomised A, L NOT a confounder): OR
marginal 2.18 vs conditional 2.48; RR identical 1.413 both ways.

**Why TE is conditional** (architectural, not a deep preference): (1) it
uses a *logistic* outcome model – the only reason a gap exists at all;
(2) it reserves its weighting machinery (IPCW) for the hard time-varying
switching and handles *baseline* confounders by the simpler
regression-adjustment route -\> conditional by construction; (3)
adjustment is often more efficient (tighter CIs) than IPTW; (4) in its
rare-outcome target setting OR~=IRR so the gap is usually negligible.
Configurable -\> could be made marginal via baseline IPTW +
g-computation.

**What’s correct:** neither is wrong; they answer different questions.
For the TTE/policy contrast the marginal effect is the standard target,
so swereg’s choice matches both the literature and our marginal truth.
Consequence for validation: converting TE’s OR via
`tte_log_or_to_log_irr` removes the OR-vs-RR **scale** difference ONLY –
it does NOT turn a conditional OR into a marginal IRR, so a residual
conditional-vs-marginal gap remains (~0.05 for ITT here). The
cross-package check is therefore a bounded consistency check, NOT an
identity; the clean correctness anchor is within-package: swereg vs the
known marginal first-event truth.

Documented in
[`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md)
(“Marginal versus conditional estimands”) + `$irr()` roxygen.

## Codex independent review (2026-06-16) — all findings addressed

Ran the work past Codex (GPT-5.x) for an adversarial second opinion.
Findings and resolutions:

| \#  | Severity | Finding                                                                                                                                                           | Resolution                                                                                                                                                                   |
|-----|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1   | SEVERE   | `s5` [`stop()`](https://rdrr.io/r/base/stop.html) on NULL `time_treatment_var` fired before the ITT bypass -\> ITT broke for studies with no switch variable      | Gated the whole deviation block (stop + computation) under PP; ITT sets `weeks_to_protocol_deviation := NA` and needs no switch var. Regression test added.                  |
| 2   | MODERATE | Truth functions summed **recurrent** events over `N x T`; swereg uses **first-event** censoring                                                                   | Rewrote both truth fns as first-event incidence rates (censor at first event, person-time-at-risk denominator).                                                              |
| 3   | MODERATE | Truth = pooled rate ratio vs MSM with `ns(tstop)` could diverge                                                                                                   | First-event person-time denominator + multi-seed test confirm they agree (gaps ~0.05).                                                                                       |
| 4   | MINOR    | Independent-loss assumption never exercised (complete panels)                                                                                                     | Added `tte_apply_independent_loss()` + test: ITT recovers truth under independent loss.                                                                                      |
| 5   | MINOR    | Zhang-Yu converts a CONDITIONAL OR with a marginal p0 -\> not a clean marginal IRR                                                                                | Softened claims in `tte-methods.Rmd`, NEWS, PROJECT: conversion fixes SCALE only, cross-package check is a bounded consistency check; clean anchor is swereg vs known truth. |
| 6   | CORRECT  | NA-before-pmin mechanism is right                                                                                                                                 | No action.                                                                                                                                                                   |
| 7   | CORRECT  | ITT dropping `censor_this_period` (loss-only) rows is defensible                                                                                                  | No action (covered by \#4).                                                                                                                                                  |
| 8   | MINOR    | [`tteenrollment_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rbind.md) dropped the `estimand` tag -\> combined ITT wrongly blocked | rbind now preserves estimand; errors on mixed estimands.                                                                                                                     |
| 9   | MINOR    | Single deterministic seed, loose tolerances                                                                                                                       | Added multi-seed recovery + rough CI-calibration test.                                                                                                                       |

All addressed; full TTE suite (ITT + PP + cross-package + classes +
weights + spec) green, no regressions.

## Build plan

### Phase 1 — ITT/PP estimand spine (structural)

- `s5_prepare_outcome(estimand=)`: ITT mode sets
  `weeks_to_protocol_deviation := NA` so Switch drops out of every
  pmin + `censor_this_period`.
- `s4_prepare_for_analysis(estimand=)`: ITT skips `s6_ipcw_pp`; tags
  `self$estimand`; ITT weight = `ipw_trunc`.
- Relaxed `irr()` guard: fires only when
  `!identical(self$estimand, "itt")`.
- Tests GREEN: ITT recovers planted ITT truth; ITT vs TE agree on
  point + CI width (common scale). No PP regression (existing suites
  pass).
- Roxygen (`s4` `@param estimand`, `$irr()` marginal-estimand note) +
  `tte-methods` vignette + NEWS 26.6.16 + version bump.
- **Plan layer**: ETT grid gains `file_analysis_itt`; `.s2_worker`
  builds both files from shared `file_imp`.
- **Results funnel** (`R/r6_tteplan.R:1929`): add `irr_itt` slot; widen
  tables / forest plots / rates / table1 to show ITT + PP side by side.
- **Monte Carlo coverage** test for ITT (tune tolerances against real
  output now that swereg-ITT exists).
- **Vignette flips**: `tte-methodology.Rmd:53,80`,
  `tte-nomenclature.Rmd:106` (“ITT not implemented” -\> implemented) +
  five-reasons table.
- **ITT full-follow-up structural test**: row counts show ITT keeps
  post-switch rows PP drops.

### Phase 2 — issue \#6 effect modification (rides on the spine)

- Refactor `irr()` -\> `.fit_irr(data_subset, weight_col)`.
- `irr_by_subgroup()` + `effect_modification_test()`.
- YAML `subgroups:` parse + validate (must be a confounder); thread
  through design.
- Tests (simulation correctness) + docs + version bump.
- Runs for **both** estimands.

------------------------------------------------------------------------

## Understanding checklist (for Richard)

### 1. The problem

- Why ITT can’t be a reweight of the PP dataset (destructive censoring).
- What `file_imp` is and why it’s the right branch point.

### 2. The solution

- The five reasons follow-up ends; which one ITT changes.
- Why ITT = IPW-only and why Loss is treated as independent.
- The `weeks_to_loss` recompute subtlety.

### 3. Broader context

- How both estimands funnel side by side into the results.
- How issue \#6 (effect modification) reuses the same funnel for both.
