# swereg 26.10.3

## New features

* **`tteplan_export_slurm()` writes a Slurm job chain from a `TTEPlan`.** It
  writes one job script per stage, plus a driver that chains them with
  `--dependency=afterok`, and it never calls `sbatch`.

## Bug fixes

* **The prevalent-user warning now counts only an exclusion that targets the
  treatment variable.** The guard tested the exclusion type before, so one
  `no_prior_intervention` exclusion on any unrelated variable silenced it. An
  exclusion names its target columns in `source_variable`, whatever its type.
  A multi-source exclusion also matches through its derived
  `source_variable_combined` column.
  `options(swereg.warn_prevalent_user = FALSE)` still silences the warning.

## Package check

* **`R CMD check --as-cran` reports 0 errors, 0 warnings and 0 notes.**
  Version 26.10.2 reported two notes.

* **Each undefined global is now declared in the function that reads it.**
  Six internal functions and the two `text_col` closures carry a
  `<sym> <- NULL` line. The old `utils::globalVariables("i.irr_estimable_itt")`
  call is gone. `R/imports.R` keeps two symbols package-wide instead. The check
  reports `.` as an undefined function, which no local binding satisfies. A
  local `..cache_cols <- NULL` would make data.table warn on every call.

* **`survival` moves from Imports to Suggests.** Nothing in `R/` calls it.
  `vignette("cookbook-survival-analysis")` does.

* **`mgcv` and `survey` stay in Imports, and `R/imports.R` gains
  `ignore_unused_imports()`.** Every real call sits inside a `TTEEnrollment`
  method. The scan behind the unused-Imports note does not read R6 method
  bodies.

## Website

* **The pkgdown template package is renamed to `pptemplate`.** `DESCRIPTION`
  `Config/Needs/website` and `_pkgdown.yml` `template: package:` both name
  `papadopoulos-lab/pptemplate` now. The house style itself is unchanged.

## Continuous integration

* **A caller of the shared `papadopoulos-lab/pptemplate` workflow replaces
  `.github/workflows/check-and-pkgdown.yml`.** The shared workflow runs
  `loc-limit`, then `R-CMD-check`, then `pkgdown`. The caller's `loc-allowlist`
  names the three files in `R/` already over 1000 code lines. `rhub.yaml` is
  unchanged.

* **`R/forest_plot.R` splits into four files by role, and leaves the
  `loc-allowlist`.** The renderers go to `R/forest_render.R`, the data assembly
  to `R/forest_data.R`, and the cell and header text to `R/forest_format.R`.
  Each of the four holds under 1000 code lines.

* **`R/r6_registrystudy.R` splits into eleven files by role, and leaves the
  `loc-allowlist`.** Twenty-four helper functions and two constants move to
  eight role-named files. Sixteen `RegistryStudy` methods reach the class
  through `$set()`, from `R/r6_registrystudy_persistence.R` and
  `R/r6_registrystudy_report.R`. `DESCRIPTION` gains the `Collate:` field that
  `@include` generates, so `r6_registrystudy.R` loads first. The largest of the
  eleven holds 675 code lines.

* **`R/r6_tteenrollment.R` splits into fourteen files by role, and leaves the
  `loc-allowlist`.** Forty helper functions and three constants move to ten
  role-named files. `TTEDesign` moves whole to `R/r6_ttedesign.R`. Fifteen
  `TTEEnrollment` methods reach the class through `$set()`, from
  `R/r6_tteenrollment_weighting.R` and `R/r6_tteenrollment_estimation.R`. The
  largest of the fourteen holds 787 code lines.

* **`R/r6_tteplan.R` splits into twenty-nine files by role, and the
  `loc-allowlist` is now empty.** Eighty-eight helper functions and five
  constants move to twenty-two role-named files. Twenty-two `TTEPlan` methods
  reach the class through `$set()`, from six `r6_tteplan_*.R` files. Four tests
  that read the old file by path now name the file holding the code they audit.
  The caller workflow drops the `loc-allowlist` key. The largest of the
  twenty-nine holds 695 code lines.

## Internal

* **The three `RegistryStudy` code-identity methods now delegate to plain
  functions in `R/code_identity.R`.** `$code_registry_fingerprints()`,
  `$randvars_hashes()` and `$pipeline_hash()` keep their formals, their
  documentation and every hash value.
  `tests/testthat/test-code_identity.R` tests the three functions behind them
  directly.

* **The nine `TTEEnrollment` estimation members now delegate to plain
  functions in `R/tte_estimation.R`.** The eight public methods keep their
  formals, their documentation and every estimate. The private `.fit_irr()`
  leaves the class and becomes `.tte_fit_irr()`, which takes the design as an
  argument. `$table1()` and the plan's `.s3_enrollment_table1()` share one
  computation, `.tte_table1_core()`, so the two routes cannot drift apart.

* **The eight `TTEPlan` reporting and export members now delegate to plain
  functions in `R/tteplan_reporting.R` and `R/tteplan_export.R`.** The six
  public methods keep their formals, their documentation and every artefact
  they write. The private `.export_figure()` and `.export_table()` producers
  leave the class and become `.plan_export_figure()` and
  `.plan_export_table()`, which take the plan as their first argument.

## Documentation

* **Two false documentation claims are gone, and the TTE/ETT rule is written
  down.** `vignette("tte-workflow")` described `pipeline_hash()` as a
  three-phase summary. It now names the five inputs the hash folds in, and says
  why the trim carries a field of its own. `README.md` drops its "Known issues"
  tracker and returns to a landing page under 500 words. TTE names the method
  and ETT names one grid cell: the rule sits in `vignette("tte-nomenclature")`
  and in the `$add_one_ett()` roxygen. No identifier changes.

* **The 26.9.0 `observed_var` entry lists three row deletions, and that list
  predates the caller's trim of 2026-08-20.** The entry names first
  immigration, emigration and death. The trim of 2026-08-20 also deletes every
  person-week before birth. The list is one example of a trim, not the full set
  of deletions a trim may make.

# swereg 26.10.2

**Breaking.** Two things MUST happen before the next run:

1. Move any row deletion into a trim registered with `$register_trim()`.
2. Regenerate `registrystudy.qs2`. A study saved at schema version 5 stops on
   load and names the generator to re-run.

`$process_skeletons()` then rebuilds every stored skeleton once, because the
phase order changed.

## The code registry runs before randvars

The per-batch pipeline is now `framework -> trim -> codes -> randvars`. A
randvars step MAY read a code registry column. That was impossible before, and
it is the capability this release unlocks.

Every randvars step's hash now folds in the code registry fingerprints. So one
code entry edit replays the whole randvars sequence against the new columns.

`Skeleton` carries a new `phase_order` field, and
`.REGISTRY_STUDY_SCHEMA_VERSION` is now 6. A skeleton written under the old
order reads `NULL` in that field. The rebuild is the only correct answer: no
rewind can add a value the old order never wrote.

## A registered code function's body now reaches its fingerprint

`$code_registry_fingerprints()` hashed an entry's `codes`, `label`, `groups`,
`fn_args` and `combine_as`, and never its `fn`. So an edit to a registered code
function changed the column it wrote, moved no fingerprint, re-applied nothing
and replayed no randvars step. The skeleton kept the old column and reported
nothing.

The fingerprint now folds in that function's body and formals. Every
fingerprint in every study moves once, so every code entry re-applies and every
randvars step replays. This release already rebuilds every skeleton, so that
costs nothing extra today.

Two inputs still sit outside the hashes: the rawbatch data, and whatever a
registered function calls or reads from its environment. A hash covers a
function's own body and formals, and follows no call into a helper.

## A function's hash no longer depends on how it was parsed

`.hash_function()` read `body(fn)` with the srcref attached. R keeps a srcref
when `keep.source` is `TRUE`, which is the interactive default, and drops it
under `Rscript`, which is the default there. The same function therefore hashed
two ways.

Register a framework function in RStudio, then run the pipeline under
`Rscript`, and every batch rebuilt. `utils::removeSource()` now runs before the
hash, so the two sessions agree.

## Row deletion belongs to a registered trim

`$register_trim(fn)` registers at most one trim function, of signature
`(skeleton, batch_data, config)`. The function returns a `data.table`. The trim
runs on a fresh base, after the framework and before the code registry, so every
later phase sees the rows it leaves behind.

**It is the one place in the pipeline that may delete skeleton rows.** A code
entry or a randvars step that changes the row count now stops the run, and the
error names the registration to edit.

To migrate, take the row filter out of the randvars step or `add_*` function
that carries it. Register that same predicate with `$register_trim()`. Only the
filter moves; the rest of the step stays where it is.

A trim edit rebuilds the base of every batch, because a deletion cannot be
rewound. A first trim and a removed trim do the same. `Skeleton` carries the
trim's identity in a new `trim_fn_hash` field.

`Skeleton$refresh_code_entry_counts()` recomputes every applied code entry's
per-column counts from the final data, and `$save_skeleton()` calls it before it
writes either file. The counts therefore describe the skeleton that gets
written.

## Documentation

* `vignette("skeleton-pipeline")` is now titled "The skeleton pipeline". It
  takes the four phases in execution order and gains a phase-1b section. Two
  false claims are gone: that the code registry runs after randvars, and that a
  randvars step cannot read a code column.
* `?RegistryStudy` and `?Skeleton` were regenerated. `$register_trim()`,
  `$randvars_hashes()` and `Skeleton$refresh_code_entry_counts()` reach a help
  page for the first time.
* `vignette("caching-and-resume")` takes all four phases in the replay decision.
  `vignette("r6-class-overview")` names the trim.

# swereg 26.10.1

## Documentation

* **The TTE vignettes describe current behaviour only.** Three passages
  narrated a superseded implementation instead of the one that ships.
  `vignette("tte-timing")` states that `enroll()` reads the weekly
  `time_treatment_var` sequence, and that neither follow-up end is rounded to
  a band boundary. `vignette("tte-methods")` and `vignette("tte-nomenclature")`
  state what the band width trades, with no reference to an earlier design.
* **`vignette("tte-methods")` section 1.1 carries a worked example of the
  eligible-week filter.** A table follows one person through a four-week band,
  and shows why a lifetime washout on both arms assigns that person to the
  treatment that person started first. It also states the one case a shared exposure
  column cannot record: both treatments covered in the same week. Behaviour is
  unchanged.
* **`vignette("tte-methods")` section 4.2 "Provenance notes" is removed.** It
  restated the 26.7.3 event-priority change and the `admin_censor_var` error.
  This file records both under swereg 26.7.3, in more detail. Section 4.3 is
  renumbered to 4.2.

# swereg 26.10.0

**Breaking. Every spec MUST be edited.** The enrollment key
`treatment.implementation.matching_ratio` is now
`treatment.implementation.comparator_to_intervention_ratio`. The number is
unchanged: the draw takes that many times a trial's count of intervention
individuals. `tteplan_read_spec()` stops on a spec that still carries
`matching_ratio`, and the message names the new key. swereg does not accept the
old key.

To migrate a spec, rename the key in every enrollment. A spec version is the
record of a completed run. Copy a released version to a new version. Do not
edit a released version.

## The generated methods text and the CONSORT figure call the comparator draw incidence density sampling

The generated manuscript methods text, the protocol table and the CONSORT
diagram called the comparator draw "matching". swereg runs no matching. The
draw is incidence density sampling, and every generated artefact now says so.

The draw takes one sample per sequential trial. Its size is
`comparator_to_intervention_ratio` times that trial's count of intervention
individuals, capped at the comparators the trial holds. One sequential trial is
one entry band of `period_width` weeks, which defaults to 4. The sampling is
stratified by that band, and the draw reads no other variable.

The draw attaches no comparator individual to an intervention individual, so no
matched set exists, and nothing in the analysis conditions on one.
`survey::svydesign()` clusters the variance on person. Where more than one
sequential trial contributes, the trial enters the outcome model as a
covariate, never as a stratum. A person can be an intervention individual in
one trial and a comparator individual in another.

Confounding adjustment is unchanged. It is by inverse probability weighting on
the covariates taken at the recruiting week.

* **TARGET item 6c and item 7c name incidence density sampling.** Each
  paragraph states the trial-level draw size, the cap, and the entry band
  width. Each also states that the draw forms no matched set.
* **TARGET item 7a prints the period width instead of the literal
  `period_width`.** Two generated sentences leaked the variable name into
  manuscript prose.
* **The protocol table gains five assignment rows.** `Comparator draw:` names
  the scheme. `Comparator draw size:` names the trial-level count and its cap.
  `Comparator draw stratum:` names the entry band and its width.
  `Comparator pairing:` records that none exists. `Confounding adjustment:`
  names the weighting step and the recruiting week.
* **The protocol table reads `Comparator-to-intervention ratio:` and
  `Comparator draw seed:`.** They read `Matching ratio:` and `Matching seed:`.
* **Every generated ratio prints comparators first, as `2:1`.** The protocol
  table, the console spec summary, the Excel spec sheet and TARGET item 6b
  printed `1:2`, against `2:1` in the vignettes. The label now names both
  sides, so the bare digits cannot read either way. The illustrative ratio in
  `vignette("tte-nomenclature")` reads `5:1`.
* **The CONSORT node reads `Enrolled after the comparator draw`, then the
  scheme and the stratum on its own line.** It read `Enrolled after matching`.
* **The cohort-flow step is `enrolled_after_comparator_draw`.** It was
  `enrolled_after_matching`. Its change label is `not drawn (comparator
  draw)`, which was `not selected (matching)`.
* **Six vignettes stop calling swereg's own draw matching:** `tte-timing`,
  `tte-methods`, `tte-workflow`, `tte-nomenclature`, `tte-methodology` and
  `r6-class-overview`. Each now names the scheme and the absence of a matched
  set.
* **`vignette("tte-nomenclature")` keeps the literature alternatives.**
  Propensity score matching and the no-matching IPW design are real
  alternatives in other people's designs, so those two entries keep the word.

No estimate moves. The draw, the seed and the ratio are one operation on one
number.

## `$get_matching()` keeps its name

`$get_matching()`, `plan$enrollment_counts[[eid]]$matching` and the `matching`
counts slot keep their names. They are a stored schema and a public accessor.
A rename there breaks a saved plan and every consumer of one. Their
documentation now calls the operation the comparator draw.

## The release-version test tracks the version instead of pinning it

`tests/testthat/test-landmark-migration.R` compared `DESCRIPTION` and the
newest `NEWS.md` heading against the literal `26.9.0`, so it turned red at the
first bump after 26.9.0. Release 26.9.1 carries it red. Its `skip_if_not()`
fires under `R CMD check`, so CI reported green. The block now asserts that the
two agree, and names neither.

# swereg 26.9.1

## The `observed_var` example uses the statin enrollment from `vignette("tte-workflow")`

This release changes documentation only, so no estimate moves. The example
documents the same keys as before.

# swereg 26.9.0

Time zero moved to the landmark. **Every intention-to-treat estimate and every
per-protocol estimate moves.** Re-run every analysis an earlier release
produced, and re-create every derived file.

The estimand changed with it. A 26.9.0 estimate does not correct a 26.8.21
estimate. The two answer different questions, so a reader MUST NOT compare
them as one number before and after a fix. `vignette("tte-timing")` states the
new estimand, the interval convention and the limits of both.

## An outcome lands in the week it happened, not at the stop of its band

**An outcome inside a partial terminal band was recorded as loss to follow-up
and left the numerator.** Re-run every per-protocol and intention-to-treat
estimate. The production skeleton deletes every person-week after death and
keeps the death week. A death is therefore a record end inside a partial band,
and that is the case this repairs.

* **`weeks_to_event` is exact to the week.** `enroll()` reads the outcome week
  from the weekly rows and writes it as `weeks_to_event_<outcome>`, one column
  per outcome. It was the stop of the band the outcome fell in.
* **The event indicator holds again.** `s5_prepare_outcome()` sets
  `event` from `tstop == weeks_to_event`. A row clipped to week 10 compared
  itself against band stop 12 and read 0.
* **The boundaries now share one resolution.** `weeks_to_protocol_deviation`,
  `weeks_to_record_end`, `weeks_to_loss` and the administrative end were
  already exact. An event that ties with a record end wins the tie.
* **A person-trial stops at its event week.** The terminal row clips there, so
  its `person_weeks` no longer runs to the end of the band.
* A panel built outside `enroll()` carries no weekly boundary. It keeps the
  band-collapsed read and the numbers it had.

## A survival risk set spans the time point instead of stopping at it

Every survival curve and every risk difference moves on a panel that holds a
clipped terminal row. Re-run any figure or table an earlier release produced
from `$survival_curve()` or `$risk_difference()`.

* **A row is at risk at every time it spans.** The risk set at time `t` holds
  every row with `tstart < t <= tstop`. It held only the rows that stopped at
  `t` before. That was harmless while every stop sat on the band grid.
  `$s4_prepare_for_analysis()` clips the terminal row at the exact censoring
  week, so a stop now falls between two band boundaries.
* **The event still lands at the stop of its own row.** The risk set spans and
  the event does not. That asymmetry is the half-open convention the panel is
  built on.
* **The risk set stays a weighted COUNT of the person-trials at risk.** It is
  not a sum of person-time. `$rates()` owns the person-time quantity, and
  neither it nor the `offset(log(person_weeks))` of a Poisson model changes.
* **The bootstrap resamples the same risk sets as the point estimate.**
  `$risk_difference()` builds one denominator per person-trial and band from
  the spanning rows. The point estimate and every replicate read it.
* **Survival carries forward where an arm holds nobody at risk.** The hazard is
  undefined there and reads `NA`. The reported survival is the latest exact
  value. An arm that runs out of follow-up first no longer turns the rest of
  its curve missing.
* Both methods report one row per arm and reporting time. An arm that holds no
  row of its own at a time now gets a row there.
* A panel with no `tstart` column keeps the numbers it had. Without a start
  column a row states no interval, so it is read as covering the band that ends
  at its own stop.

## The censoring weight is complementary log-log, offset by person-time, and lagged

Every per-protocol estimate moves. A weight multiplies every row, so re-run
any per-protocol analysis an earlier release produced.

* **The censoring model is complementary log-log with an
  `offset(log(person_weeks))`.** It was logistic with no offset. For one
  linear predictor, the probability of staying uncensored over four weeks is
  now the one-week probability to the fourth power. A clipped terminal band no
  longer carries a whole band's censoring risk.
* **The cumulative product is lagged.** The weight on band `k` is the
  probability of remaining uncensored through the START of band `k`, so the
  product stops at band `k - 1`. The first row of every person-trial then
  weighs exactly 1. The censored band stays in the risk set in this release.
  The inclusive product swereg inherited counted that band's own censoring
  probability inside its own weight.
* **The numerator is a second fitted model.** It carries the same band-start
  and trial-index terms as the denominator, and it drops the confounders. It
  was the empirical mean of the denominator predictions within a band and arm.
* **The model reads the band start and no longer the band stop.** The weight
  applies through that start, so the start is the time the model conditions
  on.
* **A stratum that cannot be estimated stops the run.** Earlier releases
  substituted the arm's marginal censoring rate when a stratum held no
  censored row, held no uncensored row, or held fewer than 10 rows. A stratum
  with no censored row is now exact: every uncensoring probability is 1, so
  every weight is 1. A stratum with no uncensored row stops, and so does a
  model that cannot be fit.
* **`estimate_ipcw_pp_with_gam = FALSE` fits a spline of the band start.** It
  fits `splines::ns(tstart, df = 3)` at four or more distinct starts, a factor
  at two or three, and no time term at one. It fitted a linear term before.
* A zero-width row stays out of the offset, because `log(0)` is `-Inf`. It
  holds no person-time, so nothing can censor it, and its uncensoring
  probability is 1.

`vignette("tte-methods")` section 1.5 states the model and the weight.

## An enrollment declares how observation is encoded

An enrollment now states how the data records that a person was under
observation in a week. swereg could not tell an unobserved person from one who
was out of arm or ineligible before. A trimmed skeleton hid the gap: an absent
week reads exactly like an observed comparator week.

`observed_var` is a flat key on the enrollment. It takes one of two forms.

* `observed_var: {column: rd_observed}` names a real logical person-week
  column.
* `observed_var: {sentinel: row_presence}` asserts that the caller already
  deleted every unobserved person-week. A row then exists if and only if the
  person was observed that week.

Use the sentinel when the skeleton already deletes every person-week the
person was not under observation. The production skeleton is one example. It
deletes every person-week up to and including first immigration, every
person-week on or after emigration, and every person-week after death. It
keeps the death week itself. A real `observed` column there would hold `TRUE`
on every retained row. It could not represent an absent week. Row presence as
a silent proxy stays forbidden. The sentinel is what makes the assumption
explicit and testable.

Two more flat keys carry the arm tolerances: `intervention_tolerance_weeks`
and `comparator_tolerance_weeks`. Each MUST be a whole number of at least 0.
Each defaults to 0, which is what every earlier release did.

```yaml
enrollments:
  - id: "01"
    name: "Statin initiation vs none, age 40-75"
    observed_var:
      sentinel: row_presence
    intervention_tolerance_weeks: 0
    comparator_tolerance_weeks: 0
```

**Every enrollment MUST declare `observed_var`, and this breaks every
existing spec.** `tteplan_read_spec()` stops on an enrollment that omits it.
There is no exemption for an older spec. A spec that cannot say who was under
observation carries the immortal-time defect silently. It looks exactly like a
spec that can.

To migrate a spec, copy it to a new version and add the key to every
enrollment. Never edit a released spec version. That version is the record of
what produced a run.

`tteplan_read_spec()` rejects a declaration that gives both `column` and
`sentinel`, a declaration that gives neither, and a sentinel name swereg does
not know. It reads no data, so it cannot check a named column.
`tteplan_validate_spec()` runs that check against the skeleton: the column
MUST exist and it MUST be logical.

The three fields travel the whole chain.
`tteplan_from_spec_and_registrystudy()` writes them into the ETT grid,
`$enrollment_spec()` reads them back, and [TTEDesign] carries them. The s1
column allow-lists keep a named observation column, so the s1b and s1c workers
can read it.

`$reload_spec()` calls all three fields structural. Each one changes who is
enrolled and when they are censored, so a cached run cannot take a new value
without a re-run.

Already-expanded trial data MAY leave `observed_var` unset. One row there is
one trial and not one week of observation, so there is nothing for the field
to encode.

## Enrollment qualifies every person-band at its landmark

The candidate table handed to comparator sampling now holds only person-bands
that are under observation and event-free at the landmark. Two defects went
with the old behaviour, and each one changed a published number.

A woman with an outcome INSIDE her entry band enrolled into the intervention
arm when she started treatment later in that band. The event sat at
`tstart = 0`, and the initiation came after it. That is immortal-time
attribution, and removing it is the reason this step exists.

A woman with entry-window rows but no row at the landmark reached the
candidate table. Panel expansion dropped her later. The pipeline counted her
as enrolled first. She took a comparator slot, she changed the requested
comparator count, and she moved the seeded draw.

### The landmark, and the two statements

The landmark of a person-band is the week that closes its entry band. Band `b`
covers week indices `b * period_width` to `(b + 1) * period_width - 1`. Its
landmark sits at week index `(b + 1) * period_width`, the first week of band
`b + 1`. Week indices are positions in `cstime::dates_by_isoyearweek`, the
scale `trial_id` already reads.

A person-band qualifies when both statements hold.

1. The person is under observation at the landmark.
2. No outcome occurrence stops at or before the landmark.

A week is a half-open interval, so an occurrence in week `w` stops at `w + 1`.
Statement 2 therefore covers every week of the entry band, and every week
before it.

`observed_var` decides statement 1. The `row_presence` sentinel reads the row
being there as the observation. A named column MUST hold `TRUE` on that row.

Statement 2 reads EVERY column in `design$outcome_vars`, and not the one
outcome a later step analyses. One enrollment serves several outcomes.
`$enrollment_spec()` collects every ETT that shares an `enrollment_id`, and
the s2 worker fans out over them. One enrolled set has to be event-free for
all of them.

### Eligibility stays a baseline property

`eligible` is assessed on the entry band, by `.band_baseline_treatment()`. It
is NOT read again at the landmark, and reading it there would empty the
intervention arm.

swereg wants a new-user or washout exclusion on the treatment variable.
`tteplan_read_spec()` warns when an enrollment declares none. That exclusion
sets `eligible` to `FALSE` from the week after initiation. An initiator starts
inside her entry band, and her landmark always falls after that week. She is
therefore ineligible at her own landmark by construction.

Measured on the `ttm_skeleton()` fixture in `test-s1a_declared_outputs.R`: 21
of 21 intervention person-bands were ineligible at the landmark. Of the 361
comparator bands that reached a landmark, 0 were ineligible there.

Sequential-trial designs assess eligibility at the start of a trial's
eligibility window. They assess survival and event-freedom through the grace
window (Danaei et al. 2013, Caniglia et al. 2023). swereg follows that split.

### `recruit_week_index` names the week that recruited each person

`.band_baseline_treatment()` now also returns `recruit_week_index`. It names
the week that recruited the person into that band. It is the earliest week the
classifier reads, which is the earliest week that is both eligible and in an
arm.

* For an initiator that is her initiation week.
* For a comparator that is her first eligible comparator week.

The rule is symmetric across the arms. No rule keyed to initiation can be.

This is where eligibility is assessed, and it is true there by construction.
Eligibility is part of what makes a week survive the classifier's mask. No
eligibility criterion is left to re-assess at the landmark.

Both enrollment paths carry it, because both call
`.band_baseline_treatment()`. In the s1a scout it travels into the tuples,
through the comparator draw, and into `enrolled_ids` on disk. `enroll()` puts
it on the entry rows of the direct path and the pre-matched path.

Week indices are positions in `cstime::dates_by_isoyearweek`, minus one. That
is the scale `trial_id` reads, so `recruit_week_index` and the landmark
`(trial_id + 1) * period_width` compare directly.

`min()` is order-independent, exactly as `any()` is, so this adds no sort. It
reads `isoyearweek` as a string. Every week in `cstime::dates_by_isoyearweek`
matches `YYYY-WW` with a zero-padded week, measured across all 10,436 of them,
so the strings sort chronologically.

`recruit_week_index` reports WHEN the person qualified. The entry-window
snapshot below reads her covariates at that instant.

### Where it runs, and why the position matters

Qualification runs after the arm classification and before the comparator
draw. Both enrollment paths apply it.

* `.s1a_finalize_on_skeleton()`, the production scout. `.s1b_worker()` never
  reads a person-week, so the drop happens while the weekly data is in hand.
* `enroll()` Phase C, the direct `TTEEnrollment$new(..., ratio = )` path.

The position carries two properties. Attrition reports both arms, because each
band already carries one. Sampling refills the ratio from qualified
comparators, so an unqualified woman no longer shrinks the matched set.

### The cascade says why

Three criteria join the CONSORT attrition table: `landmark_candidates`,
`landmark_observed` and `landmark_event_free`. Each count is cumulative, and
each row splits into the intervention and comparator arms. They carry the
columns `.s1_compute_attrition()` already writes, so the two tables stack and
`.s1b_worker()` sums them unchanged.

`TTEEnrollment$landmark_attrition` is a new field carrying the same table for
the direct path. It stays `NULL` in pre-matched mode, where the scout already
qualified the ids.

### Two consequences to expect

**The last band of a skeleton enrolls nobody.** No week follows it, so it has
no landmark. A trial whose landmark falls past the end of the data has no
follow-up to contribute.

**Qualification runs only when the design declares `observed_var`.** A design
that declares none cannot say whether an absent week is an unobserved week or
a week outside the study. `tteplan_read_spec()` makes the declaration
mandatory, so every spec-driven enrollment qualifies. A [TTEDesign] built by
hand without `observed_var` does not.

This step removes the person-bands that cannot qualify. The next section
re-bases follow-up on the landmark, which is what removes the immortal time
those bands carried.

## Time zero is the landmark, and baseline covariates are read at the recruiting week

**Every ITT and per-protocol estimate moves.** The trial panel now opens one
band AFTER the entry band, so the entry band contributes no follow-up. It used
to open at the entry band, and a woman who initiated in week 3 of a four-week
band carried three immortal weeks. Landmark qualification makes the whole entry
band immortal: she must reach the landmark, event-free and under observation,
to enroll at all.

### What the panel looks like now

`entry_band_id` stays on the panel and names the trial. `trial_id` names the
follow-up band, and the first row of every person-trial holds
`trial_id == entry_band_id + 1` and `tstart == 0`.

Each confounder reaches the panel twice.

* `.tte_entry__<v>` holds the value at the recruiting week. That is the
  earliest week of the entry band that is both eligible and in an arm.
* `<v>` holds the time-updated value of the follow-up band, exactly as before.

The first week of the entry window is the wrong instant to read. A woman need
not be eligible there, and she need not be in an arm there.

`.tte_entry__` is a reserved prefix. [TTEDesign] stops on a confounder name
that takes it.

### What reads which

`$s2_ipw()` and `$table1()` fit and tabulate `.tte_entry__<v>`, under the plain
name inside a local table. Time zero moved, so the `tstart == 0` row now holds
the landmark-band value, and reading it there would adjust for the wrong
instant. The plan's Table 1 worker takes the same route, and the two MUST
agree.

`$s6_ipcw_pp()` still reads the time-updated `<v>`, because censoring depends
on what is true during follow-up.

**`$s6_ipcw_pp()` now stops when a time-updated confounder is missing on the
rows it fits.** It names the confounder, the rows and the person-trials. An
`NA` there used to make `stats::predict()` return `NA`, and `cumprod()` carried
that `NA` through the rest of the person-trial into the survey fit. swereg MUST
NOT substitute the `.tte_entry__` value: that value describes the recruiting
week.

The old call hid the gap rather than filling it.
`$s1_impute_confounders()` update-joins the baseline value onto every row of a
person-trial, so an imputed confounder came out NA-free and flat across
follow-up.

**`impute_fn` now receives the `.tte_entry__` names.** Imputation is name-list
driven, so the old call left the snapshot unimputed and imputed a column no
adjustment step reads. It MUST impute only the columns it is given, and it MUST
NOT overwrite the follow-up value.

### Two consequences to expect

**A person-trial whose follow-up band is absent from the data contributes no
panel row.** The old expansion gave it the entry band. That row was immortal
time, and it was the only row such a trial had.

**A panel with no `recruit_week_index` keeps the old read.** A caller who
builds `enrolled_ids` by hand, outside the plan chain, gets no snapshot.
`$s2_ipw()` then falls back to the follow-up column. Every spec-driven
enrollment carries the column, on the direct path and the pre-matched path.

## Protocol deviation is read from the weekly assessments

**Every per-protocol estimate moves.** Deviation used to be decided from the
band-collapsed treatment value, which is the LAST week of the band. A woman's
verdict followed where her weeks fell against the calendar grid, and not what
she did. Two women who behaved the same way could get opposite verdicts.

`enroll()` now reads the weekly sequence itself. It writes one exact boundary
per person-trial into `weeks_to_protocol_deviation`, and
`$s4_prepare_for_analysis()` reads that column instead of recomputing.

### Five patterns, and what each one used to give

Each row is the weekly assessment of one intervention woman across one band of
four weeks, under a tolerance of 0.

| weekly pattern | collapsed value | old stop | new stop |
|---|---|---|---|
| `{T,T,F,F}` | `FALSE` | end of week 4 | end of week 3 |
| `{T,F,T,T}` | `TRUE` | n/a | end of week 2 |
| `{T,F,F,T}` | `TRUE` | n/a | end of week 2 |
| `{T,NA,T,T}` | `TRUE` | n/a | end of week 2 |
| `{T,T,T,NA}` | `NA` | end of week 4 | end of week 4 |

Three of the five hid the switch completely. The last row is the one the old
read already got right.

### The rule

Discordance is arm-specific. An assessment is discordant when
`time_treatment_var` does not hold the assigned arm of that person-trial. `NA`
is discordant in both arms.

A tolerance is the number of CONSECUTIVE discordant assessments an arm allows,
and a concordant assessment resets the run. For tolerance `k`, follow-up stops
at the right edge of the `(k + 1)`th consecutive discordant week. A run that
starts at week `u0` stops at `(u0 + k + 1) - L`, where `L` is the landmark
week. A tolerance of 0 stops at the first discordant week.

`intervention_tolerance_weeks` and `comparator_tolerance_weeks` carry the two
values, and each one applies to its own arm.

A run that starts before the landmark counts only its weeks at or after it.

### Loss of observation is never tolerated

An internal gap in the weekly sequence stops follow-up at the first absent
week. The person may return in a later week. She is still censored at the gap.
No tolerance applies, because loss of observation is not discordance.

Under the `row_presence` sentinel an absent week is an absent row. A band that
loses one of its four weeks still reaches the panel, so the band-level read
could not see the gap at all.

A record that simply ends carries no internal gap. `weeks_to_loss` already
reports that case, and it reads the panel.

### The boundary can fall inside a band

The panel is still one row per person-trial-band. Nothing is expanded weekly.

Every band before the boundary is complete follow-up, and the band that
reaches it carries the censoring. `$s4_prepare_for_analysis()` clips that band
at the boundary and keeps it. The next section covers the clipping.

### Two limits to expect

**The boundary needs `observed_var`.** A design that declares none cannot say
whether an absent week is an unobserved week or a week outside the study. It
keeps the band-collapsed read. Every spec-driven enrollment declares the field.

**A trial panel handed in directly keeps the band-collapsed read.** One row
there is one band, so there is no weekly sequence left to read.

## Person-time is the exposure contributed, and the censoring row is kept

**Every rate, every incidence rate ratio and every Poisson offset moves.** The
denominator used to be the number of source weeks the band collapsed, and
`$s4_prepare_for_analysis()` then deleted the whole censoring row. A woman who
deviated in week 2 of a four-week terminal band was billed for four weeks, and
then lost all four.

`s5_prepare_outcome()` now clips the terminal row at the exact boundary, and
sets `person_weeks` to `tstop - tstart`. The row stays in the analysis data.
It carries the exposure before the boundary and nothing after it.

### What the boundary is

Follow-up stops at the earliest of five events.

1. The first outcome event.
2. The first protocol deviation.
3. The first observed loss.
4. The administrative end.
5. The requested follow-up end.

Priority runs in three levels. An outcome event beats everything. A protocol
deviation and an observed loss come next. An administrative or requested end
comes last.

An event that stops in the deviation band wins that band. The row counts as an
event and not as a censoring, and the deviation does not clip it. That rule is
unchanged since 26.7.3. The row stops at the event week itself, which can fall
inside the band.

### A record that ends inside a band bills only the weeks present

`weeks_to_loss` reports a record that stops before the planned end. It used to
read `.max_tstop`, which is the stop of the LAST BAND. swereg credited a record
that ended inside a band with weeks the person was never observed for.

`enroll()` now writes the exact week into `weeks_to_record_end`, read from the
weekly sequence before the collapse. A woman observed for 10 follow-up weeks
under a four-week band bills 10 weeks, and no longer 12.

The meaning of `weeks_to_loss` does not change. Only its resolution moves from
the band to the week. A panel handed in directly carries no
`weeks_to_record_end`, so it keeps the band-level read.

A record that reaches the end of the panel sets `weeks_to_record_end` to `NA`.
That woman completed the follow-up the panel holds, so this boundary never
censors her.

The boundary needs `observed_var`, exactly as the deviation boundary does.

### The administrative and requested ends are exact

Neither is rounded to a band boundary. A six-week requested follow-up stops at
week six, and a woman in a four-week band keeps the two weeks that used to
disappear.

`weeks_to_admin_end` also moves by one week. `difftime()` counts the whole
weeks BETWEEN the baseline week and the administrative week. The person is
under study to the end of the administrative week itself, so the stop is one
week after the count.

The warning about short trials changed with it. Only a trial that enters at or
after the administrative week is dropped now.

### Why the censoring row is safe to keep

The stated reason for deleting it was the deviated regime. A censoring row
observes that regime, and a downstream outcome regression then attributes its
outcome to the baseline treatment.

Clipping removes that risk at the source. The retained row stops at the exact
censoring week, so it holds no time under the deviated regime. It also carries
no event, because an event in the same band wins the band and clears
`censor_this_period`.

`$s6_ipcw_pp()` still fits on the censoring rows, exactly as before. It ran
before the deletion, so its inputs do not change.

### No retained row has zero duration

A row that opens at or after the boundary contributes nothing, so
`s5_prepare_outcome()` drops it. Every retained row has `tstop > tstart`, and
`log(person_weeks)` is finite in every offset model.

## Every stop is exclusive, and the convention is now stated once

`[tstart, tstop)` is the interval, and `tstop - tstart` is the duration. The
rule held throughout the code and appeared in no document.

`TTEDesign` now carries an interval convention section, and `TTEEnrollment`
and `TTEPlan` inherit it. `tests/testthat/test-interval-convention.R` pins the
five boundaries: `weeks_to_event`, `weeks_to_protocol_deviation`,
`weeks_to_loss`, `weeks_to_admin_end` and `weeks_to_record_end`. Each fixture
places its boundary where the exclusive and inclusive readings disagree.

No behaviour changes. The audit of the five boundaries, and of every `+ 1L`
and `- 1L` in `R/r6_tteenrollment.R` and `R/r6_tteplan.R`, found no further
defect.

## Three TTE schema versions moved to 3

`.TTE_DESIGN_SCHEMA_VERSION`, `.TTE_ENROLLMENT_SCHEMA_VERSION` and
`.TTE_PLAN_SCHEMA_VERSION` are now `3L`.

**An object saved by an earlier release no longer loads.**
`TTEDesign$check_version()`, `TTEEnrollment$check_version()` and
`TTEPlan$check_version()` each stop with a migration message. `qs2_read()`
calls the check for every R6 object it reads, and `tteplan_load()` calls it
too. Re-run the project's `s0_init.R` to build a new plan.

A version-2 object MUST NOT be reinterpreted. Its `tstart == 0` rows are entry
band rows, and a 26.9.0 reader would take them for landmark rows. The two
earlier classes warned and continued, which is what made that silent
reinterpretation possible.

# swereg 26.8.21

## A band with no eligible in-arm week enters neither arm

Enrollment reads only the weeks of a band in which the person is eligible and
the treatment column holds `TRUE` or `FALSE`. It drops every other week of the
band first. It then applies three rules.

1. The person is an initiator when at least one week it reads holds `TRUE`.
2. The person is a comparator when every week it reads holds `FALSE`.
3. The person-band is ineligible when it reads no week at all.

Rule 3 changes who enters a trial, so it can change an estimate. The s1a scout
path classified such a band as a comparator before. That path called
`any(x, na.rm = TRUE)`, and `any()` reports `FALSE` after it removes every
value. The band now yields no row at all.

`.band_baseline_treatment()` is the one function that decides the rule. The
s1a scout path and the direct `enroll()` path both call it, so the two paths
cannot disagree. The direct path read `first()` off the earliest eligible week
before, so its classification also changes.

* A person who starts the intervention in week 3 of a four-week band was a
  comparator. That person is now an initiator.
* A band whose first eligible week is out of arm entered neither arm. swereg
  drops that week now, and classifies the band from the weeks that remain.
* A band whose eligible weeks are all out of arm entered neither arm, and it
  still enters neither arm. `first()` returned `NA` there, which matched
  neither arm.

The drop comes first, so an out-of-arm week neither creates nor prevents a
comparator classification. A band of `FALSE`, `NA`, `FALSE`, `FALSE` is a
comparator band. A band of `NA`, `TRUE`, `FALSE`, `FALSE` is an intervention
band.

Time zero stays the start of the entry band. swereg attributes initiation
anywhere in the entry band to that week. The band therefore carries residual
within-band immortal time of at most `period_width - 1` weeks.

## `.attrition_overall()` returns NULL when a criterion carries no global row

That renderer reads the global attrition rows and nothing else. Every criterion
MUST carry a global row. The renderer returns NULL when one criterion does not.

A per-trial fallback stood there and is removed. The fallback summed the
per-trial rows, which counts a person once per sequential trial she enters. It
reported that sum under a unique-persons heading, where the real unit is
person-trials. Under a mixed input the sum sat one row away from a `uniqueN`
count, so the CONSORT delta between the two rows could be negative.

An affected enrollment loses three outputs.

1. The workbook holds no `Attrition_<id>` sheet. The export writes no CONSORT
   PNG and no CONSORT PDF for that enrollment.
2. The provenance table of contents names no such sheet. One condition gates
   the sheet and its table-of-contents row now: the return value of
   `.write_attrition_sheet()`.
3. The combined-baseline sheet (`Table S<n>`) prints no cohort summary
   sentence. That sentence reported per-trial sums under a unique-persons
   label.

An attrition table written before the global rows existed reaches this state. A
missing number is safer than a wrong one.

## Documentation, examples and shipped data

* The vignettes state the mixed-band classification rule precisely.
  `vignette("tte-methods")`, `vignette("tte-workflow")` and
  `vignette("tte-nomenclature")` say which weeks of a band the rule reads, and
  which it drops. `TTEEnrollment` gains a Baseline treatment section that
  states the same rule, and `$print_target_checklist()` states it too.
* The grace-period wording agrees across every statement. swereg implements no
  grace period. `period_width` gives within-band slack for the timing of
  initiation at enrollment, and nothing else. Deviation after the entry band
  censors per-protocol follow-up. `vignette("tte-nomenclature")` called
  `period_width` a grace period before, and the checklist that
  `$print_target_checklist()` prints made the same claim.
* `vignette("tte-nomenclature")` records that band boundaries are anchored to a
  fixed calendar origin, and not to the first week of a study.
* `example/` and `dev/` no longer name a collaborator or a private file path.
  The file server addresses are example addresses now, and one example script
  is renamed. The maintainer's own name stays in `example/`.
* The documentation, the tests and the fixtures use unrelated codes in place of
  the study-specific ones.
* `dev/generate_fake_data.R` calls `set.seed()` in each generator that draws
  random numbers, and each of those generators takes its own `seed` argument.
  All six datasets under `data/` are regenerated from that script. Five change
  in value. `fake_person_ids` does not change, and every row count stays the
  same.

# swereg 26.8.20

## A risk-difference result carries its statistical decisions as data

`$risk_difference()` returns two more columns. `nnt` is the signed number
needed to treat, which is `-1/rd`. `nnt_direction` is the benefit-or-harm
decision, and it reads `"benefit"`, `"harm"` or `NA`.

`.tte_nnt_from_rd()` makes that decision once, beside `rd`, from the same
numbers. Every formatter now reads the stored column. The cell that renders the
number needed to treat tested the sign of its own input before. Each formatter
was therefore its own decision-maker, and nothing forced two of them to agree
about one band.

The cell builder takes the direction as a required argument. It has no default.
A caller that cannot supply a direction gets an error, because a silent fall
back to the sign is the defect this repairs.

The value stays signed throughout. `abs()` appears nowhere in the
risk-difference or number-needed-to-treat arithmetic. A magnitude that lost its
sign cannot separate benefit from harm.

## The decision travels with the cached risk-difference row

The forest export path caches one risk-difference row per emulated trial onto
`plan$results_ett`. That row now carries `nnt` and `nnt_direction`, copied off
the curve. The figure reads the stored direction from the row. It no longer
rebuilds one from `rd`.

The internal number-needed-to-treat helper reports no direction at all. It
returns the magnitude and the interval only. One function decides the
direction, one column stores it, and every reader reads that column.

A result cached before these columns existed renders no benefit-or-harm label.
It does not error, and it does not fall back to the sign of `rd`. The two
decision columns are exempt from the required-column check for that reason.
Every other column of the contract stays required. The `PP results` and
`ITT results` sheets print no benefit-or-harm label, so a legacy result loses
nothing there.

The sheets gain no column. The cache stores the decision; the sheets render
what they rendered before.

## `interval_status` names a third state, `"spans null"`

The column read `"ok"` or `"zero-event arm"`. It now reads `"ok"`,
`"spans null"` or `"zero-event arm"`.

`"spans null"` means the bootstrap interval is estimable and contains the null.
The number needed to treat has no interval there, because `x -> -1/x` is
undefined across zero. Such a band read `"ok"` before, and the reason reached
the reader only as an empty cell on a figure.

`"ok"` therefore says more than it did. It now means the interval is estimable
AND strictly excludes the null. Code that tested `interval_status == "ok"` to
find a band with an interval MUST test `interval_status != "zero-event arm"`
instead.

`"zero-event arm"` wins where a band could take two states. An `NA` bound
cannot be judged against the null, and the zero-event arm is why the bounds are
`NA`.

## The risk-difference bootstrap multiplies its replicates in batches

`$risk_difference()` now multiplies 50 bootstrap replicates against the arm
matrices at once. It multiplied one replicate at a time before. Each product is
now one level-3 BLAS call in place of one call per replicate, and the
arithmetic is memory-bandwidth bound. Measured at 500 replicates on a
national-registry panel, the batched form runs 3.1 times faster.

The numbers do not move. The estimator draws one multiplicity vector per
replicate, in replicate order, by the same call it always made. So the random
number stream is unchanged, and the same seed returns the same survival, risk
difference, interval and count columns. A test pins every one of those columns
to values measured on the old code, cell for cell. It pins the stored replicate
matrix too.

The batch size is fixed at 50 and is not an argument. Sizes of 50, 100, 250 and
500 are within 1 percent of each other on speed. A reachable size would let a
performance setting move a published confidence interval.

## `$s3_analyze()` computes the absolute scale for every emulated trial

The risk difference was computed in the export path, behind the `"forest"`
figure option `risk_difference`. A script that did not set the option drew
every figure without the quantity. There was no error and no warning.

`$s3_analyze()` now computes it, for every ETT and with nothing to switch it
off. Two estimand and weight combinations carry it. Per-protocol on
`analysis_weight_pp_trunc` stores under `rd_pp_trunc`. Intention-to-treat on
`ipw_trunc` stores under `rd_itt`. Per-protocol on the untruncated weight
carries rates and the incidence rate ratio only, as before.

Each slot holds one summary row at the end of follow-up. The row carries `rd`,
`rd_lo`, `rd_hi`, `nnt`, `nnt_direction` and `interval_status`.

## The survival curve is stored beside the risk difference

`rd_curve_pp_trunc` and `rd_curve_itt` hold the whole band-by-band curve, with
`surv_comparator` and `surv_intervention` next to the risk difference. The
risk difference is `S_comparator(t) - S_intervention(t)`, so those two columns
were computed to produce it and then thrown away. The export path read the
analysis panel off disk to rebuild them.

The curve is WIDE: one row per band, one column per arm. Measured on a 39-band
curve, the stored row and curve together serialise to 2,335 bytes, which is
2.5 MB across 540 ETTs and two estimands. The `n_boot` by `n_band` replicate
matrix is dropped, because the stored percentiles already summarise it. It
measures 156,216 bytes per curve, so keeping it would add 169 MB to that same
plan.

## The bootstrap settings belong to s3, and a figure cannot change them

`$s3_analyze()` runs the bootstrap at 500 replicates with seed 1. Both are
fixed there and recorded on every stored row and curve. A reader of
`plan$results_ett` can reproduce an interval from the plan alone.

The `"forest"` figure options `n_boot`, `seed` and `conf_level` no longer reach
the estimator. A manifest that sets one now warns and names where the value
belongs. Remove them from the manifest. A figure that could lower the replicate
count could lower the precision of a published interval.

## The confidence level is a study property: `study$implementation$conf_level`

`$s3_analyze()` reads the risk-difference confidence level from
`spec$study$implementation$conf_level`, and defaults to `0.95`. A study that
wants 90 percent intervals writes `conf_level: 0.90` once. Every stored row,
every stored curve and every printed header then carries 90 percent.

The level MUST be a single number strictly between 0 and 1. `$s3_analyze()`
refuses anything else before it dispatches a single worker, and the message
names the field.

A per-exhibit `conf_level` in a forest figure is ignored and warns. s3 computes
the interval long before any figure exists, so one study has one level. A
figure that could restate the level would print a label the numbers do not
have.

## `risk_difference = TRUE` is a display switch and computes nothing

The `"forest"` option still decides whether the figure carries the two extra
columns. It no longer decides whether the quantity exists.

The forest figure reads `plan$results_ett` and opens no file. The survival
figure still reads its analysis panel, because its numbers-at-risk table needs
`n_persons_at_risk`, a distinct-person count that no survival curve carries.

The `PP results` and `ITT results` sheets are unchanged in layout. They read
the same slot they always read. An export that ran `$s3_analyze()` now finds
the four risk-difference columns populated, in whatever order the export steps
ran.

## Five accessors return every stored result as one flat table

`TTEPlan` gains `$get_estimates()`, `$get_curves()`, `$get_baselines()`,
`$get_attrition()` and `$get_subgroups()`. Each takes no argument. Each returns
everything the plan stores. The caller filters.

A reader needed three things to reach one number: the slot name, the estimand
that slot stands for, and the column names inside it. The five methods carry all
three as columns.

`$get_estimates()` returns one row per emulated trial, estimand and weighting.
`$get_curves()` melts the wide stored curve into one row per arm and band.

`estimand` and `weights` are two columns, not one. `estimand` reads `"pp"` or
`"itt"`. `weights` reads `"truncated"` or `"untruncated"`, and names the
weighting choice inside per-protocol. Three combinations occur.

No consumer changed. The workbook, the figures and the CONSORT diagrams still
read the result slots directly.

## An accessor computes nothing and creates no row

An accessor MUST NOT compute a result. It MUST NOT apply a rule or a threshold.
It MUST NOT read an analysis panel. It MUST NOT create a row that no slot backs.

A missing slot yields absent rows. The rule keeps staleness visible. A plan saved
before a stage ran holds fewer slots than a plan saved after it. An accessor that
recovered the gap would report a full table over a partial plan.

Three rows per emulated trial is an upper bound, not a promise. `$get_estimates()`
gives a combination a row when the plan holds at least one of its three slots. A
combination the plan holds nothing for gets no row.

`$get_baselines()` shows the same rule. The `"raw"` baseline panel needs a
separate pre-imputation file, and that file is optional. The table then holds no
`"raw"` rows, and no other panel appears under that name.

Numbers come back bare. `irr_pvalue` is a probability, not `"<0.001"`. `rd` is a
proportion, not a rate per 10,000. The consumer formats it.

Baseline cell values are the one exception. `overall`, `comparator` and
`intervention` are display strings, because the producer stores them that way.

## `$s3_analyze()` stores `irr_estimable` beside the incidence rate ratio

An arm with no event gives a ratio of exactly 0, which is finite. Printed bare it
reads as a point estimate of no risk, known perfectly. It is neither.

`.tte_irr_estimable()` is now the one place the package answers that question.
`$s3_analyze()` calls it and stores the answer as the `irr_estimable` column, as
it stores `nnt_direction` beside the risk difference. The results sheet calls the
same function, so the sheet and the stored column cannot disagree.

`$get_estimates()` READS that column. A result stored before the column existed
reports `NA`, and the accessor does not apply the rule to fill the gap.

## `$get_attrition()` returns the stored global rows only

`$s1_generate_enrollments_and_ipw()` stores one attrition row per trial and
criterion, plus one global row per criterion. The global row carries the true
overall count of distinct people. `$get_attrition()` returns the global rows.

A criterion with no global row gets no row. `$get_attrition()` never rebuilds one
by summing the per-trial rows. That sum counts a person once per sequential trial
she enters, so it over-counts `n_persons` and reports a wrong number with no
signal.

An attrition file written before the global row existed therefore returns fewer
criteria than it holds. The CONSORT renderer keeps the old fallback, where an
inflated number beats no number at all, and `.attrition_overall()` is unchanged.

The table holds the eligibility cascade only. It holds no matching step, no
analysis step and no per-step change columns.
`$s1_generate_enrollments_and_ipw()` stores neither of those two steps.
`.build_cohort_flow()` builds them and derives the change columns. Building a row
is a renderer's job, so `.build_cohort_flow()` stays the one place it happens.

The table carries no step kind either, because nothing stores one. The first
stored criterion is the cohort start and every later one is an exclusion, so a
consumer labels them from `step_order`.

A consumer that needs the full participant flow passes the plan to the CONSORT
renderer, as it did before.

## `$get_curves()` carries the numbers at risk beside survival

`n_persons_at_risk` is an unweighted count of distinct people, per arm per band.
`$s3_analyze()` stores it and `$get_curves()` melts it, so a consumer can draw a
risk table without opening an analysis file.

The count is READ, not derived. A risk table reports people. Survival is a
weighted probability, so no head count follows from it.

A curve stored before that column existed gives `NA`. A consumer that draws a
risk table MUST check for missing values and refuse to draw. A row of missing
counts looks like a drawn risk table.

## `$get_subgroups()` returns both p-values, and they never share a name

`irr_pvalue` is the stratum's own p-value. It answers whether that stratum's rate
ratio is distinguishable from the null. `em_pvalue` is the interaction test. It
answers whether the strata differ from each other.

The two are not interchangeable. A consumer that renders one where the other
belongs reports a different finding, so the table gives them different names.

`em_pvalue`, `ratio_of_irrs`, `ratio_lo` and `ratio_hi` all come from the stored
interaction test. That is the same result the Effect modification exhibit reads,
so the exhibit and the accessor report one number rather than two.

`$irr_by_subgroup()` runs the interaction test a second time and attaches its own
`em_pvalue` and `ratio_of_irrs` attributes to the stratified table.
`$get_subgroups()` does not read those attributes. One of the two calls can fail
while the other succeeds.

## `$get_subgroups()` reads the union of two stored families

`$s3_analyze()` dispatches the stratified rate ratios and the interaction test
as separate work items, in separate subprocesses. Either can fail alone.

`$get_subgroups()` returns a row for every subgroup variable and estimand that
EITHER family stores. It keys neither family off the other. Four states occur.

1. Both stored. Full rows.
2. Stratified only. One row per stored level, with all four interaction columns
   `NA`.
3. Interaction only. One row, with `subgroup_level` reading `"all"` and the four
   stratum columns `NA`.
4. Neither stored. No rows, even when the specification names the variable.

A skipped stratified result reads as absent, so state 3 covers a failed
stratified worker beside a stored interaction test.

State 3 used to return nothing. The Effect modification exhibit reported the
interaction p-value there, and the accessor could not.

In state 3 no stored table names the strata, so the accessor names none. `"all"`
is the level `$irr_by_subgroup()` gives its whole-cohort row, and it is the level
the Effect modification exhibit prints in the same state.

State 4 is where the accessor and the exhibit differ, and the difference is
deliberate. The exhibit reads the specification and prints one empty row for a
variable it stored nothing for. The accessor reads stored results and returns no
row.

## `subgroup_var` is part of the `$get_subgroups()` key

One emulated trial MAY carry several subgroup variables, and each one has its own
`"all"` row. The grain is therefore the emulated trial, the estimand, the
weighting, the subgroup variable and the subgroup level.

## A spec reload no longer overwrites a cached description

`$reload_spec()` refreshed `plan$ett$description`, then wrote the new label onto
every cached result in `plan$results_ett`. That write is gone.

A description is input-derived, and `plan$ett` owns it. The accessors join the
current label from there. A stale result carried a fresh label before, which hid
the age of the result.

A study that needs the label a result was computed under MUST store it under its
own immutable name.

## `$export_tables()` reads the accessors, and the workbook does not move

Every sheet writer and every figure builder in the export path now calls
`$get_estimates()`, `$get_baselines()`, `$get_subgroups()` or
`$get_attrition()`. None reads a slot of `plan$results_ett` or
`plan$results_enrollment`. A slot rename can no longer reach a cell.

The output is unchanged. A test builds two stored-result generations across
four project shapes and runs one export for each. It compares every sheet cell
by cell, and every image on content, inventory and pixel size, against a stored
snapshot. All eight exports match.

`$export()` moved with it. The forest exhibit builds its risk-difference
lookup from `$get_estimates()`, and the `table1` exhibit composes its CSV from
`$get_baselines()`.

Three consumers still read a stored list, and each says why in its own
comments. `$results_summary()` and `$print_target_checklist()` report on the
CACHE, and they separate an absent slot from a skipped one, which an accessor
reports the same way. The CONSORT diagram and the attrition sheet read
`plan$enrollment_counts` for a second reason. `.build_cohort_flow()` needs the
matching block and the per-trial rows, and `$get_attrition()` returns the
global eligibility rows only.

## A baseline panel is composed from `$get_baselines()`, not read whole

The `Table 1`, `Table S1`-`Table SN` and `table1` CSV exhibits build their
display panel from the accessor rows. Two rendering conventions are the
renderer's, and it restores both. The variable name prints once per block. The
`SMD` column is the display string that `.t1_fmt_smd()` builds from the stored
`smd_numeric`.

The arm column headers now come from the CURRENT specification, through
`.lookup_arm_labels()`. Every other sheet already read the specification for
those labels, so the workbook is consistent for the first time. A specification
edited between `$s3_analyze()` and `$export_tables()` changes those two headers
and no number. A specification that names no arms heads the two columns
`FALSE` and `TRUE`, which are the two values of the treatment variable.

## `.ff_irr_ci()` reads the stored estimability decision

The forest cell tested `irr < 0.01` and blanked the cell itself. It now reads
`irr_estimable`, which `$s3_analyze()` decides through `.tte_irr_estimable()`
and stores beside the ratio. The plottability guard behind the forest panel
reads the same column.

A ratio stored before that column existed carries `NA`. The one shared rule
then answers, so a legacy result renders what it always did.

The `>100` display cap is a SEPARATE decision and it stays in the formatter.
`.tte_irr_estimable()` answers "may this ratio be reported at all", and the cap
answers "does this ratio fit on the axis". A ratio of 150 is estimable, and it
prints as `>100`. The two share a number at the lower edge and are not the same
question, so `.FOREST_IRR_PANEL_RANGE` names the drawing window and says so.

## `.prepare_combine_data()` no longer stops on one absent description

The helper read `description` off every stored result before it subset the
list. One emulated trial whose copy was absent, or was not one string, stopped
the whole call, including the trials the caller had asked for.

The description now comes from `plan$ett`, which holds one row per emulated
trial and is an input. A trial the grid does not name falls back to its own
identifier.

## `$get_attrition()` returns every stored row, and `$get_matching()` is new

`$get_attrition()` returned the global rows only. It now returns every stored
attrition row, per-trial and global, and it carries `trial_id`. That column is
`NA` on a global row and the trial index on a per-trial row, so the caller
filters on it.

The method still computes nothing. It does not sum the per-trial rows, it
creates no global row for a criterion that has none, and it fills no value
down. `step_order` is the criterion's position in stored order, so every row of
one criterion carries the same value.

Summing is a RENDERER's decision and `.attrition_overall()` still makes it.
That sum counts a person once per sequential trial she enters, so it
over-counts `n_persons`. For a CONSORT diagram an inflated number beats no
number. Returning a stored row is not summing it, so the accessor can supply
the rows without making the decision.

`$get_matching()` is a sixth accessor. It returns the stored matching counts,
one row per enrollment and trial. It is a separate method rather than four more
columns on `$get_attrition()`. The matching table has one row per enrollment
and trial, and the attrition table has one row per enrollment, trial and
criterion. Joining them would repeat one matching count on every criterion
row.

## The participant flow reads the accessors

`.write_attrition_sheet()`, `.format_enrollment_summary()`,
`$print_target_checklist()` and the CONSORT renderer read
`plan$enrollment_counts` before. All four now read `$get_attrition()` and
`$get_matching()`.

No consumer reads `plan$results_ett`, `plan$results_enrollment` or
`plan$enrollment_counts` any more. Three classes of reader remain, and each
states its reason in its own comments. The producers write those lists. Three
readers of the KEYS answer "did this stage run". Two console reporters separate
an absent slot from a skipped one.

The numbers do not move. The Attrition sheets and the CONSORT diagrams are
unchanged, cell for cell and pixel for pixel.

## `$export_tables()` works without a RegistryStudy

`.write_provenance()` called `format(NA, "%Y-%m-%d %H:%M:%S")` when a plan
carried no `registry_study_created_at`. `format.default()` read the format
string as its `trim` argument and stopped with `invalid 'trim' argument`, so
the whole export failed. An absent timestamp now prints as an empty cell.

## An accessor reports what is stored, including that nothing is

Four states stored a fact that the accessors could not report, so a cell that
used to carry a number went blank. Each is a supported state, and each is
fixed.

`$get_baselines()` carried the three enrollment counts on the rows of a stored
panel. An enrollment that stored counts and NO panel now gets ONE row instead,
with every panel column `NA`. `.baseline_panel_is_stale()` calls such a result
CURRENT, so it reaches the sheets, and the `Enrollments` sheet and the CONSORT
analysis step report its size again.

`$get_estimates()` gains `irr_stored` and `irr_interval_stored`. Every numeric
column reports an absent slot and a stored `NA` the same way, as `NA`, and two
consumers must separate the two. The `PP results` and `ITT results` sheets
report the arm counts of a combination whose rate ratio failed. They report
nothing for a combination that has no rate-ratio slot.

`$get_estimates()` returns ONE row for an emulated trial that stored a result
and no estimate slot, with `estimand` and `weights` both `NA`. A stored summary
is a stored slot, and the `ETTs` sheet reports its event count again.

`$get_baselines()` gains `comparator_label` and `intervention_label`, the two
arm names the STORED panel heads its columns with. The `Table 1` and
`Table S1`-`Table SN` sheets read them from there. They no longer re-read the
specification, which would head yesterday's numbers with today's labels.

No consumer that asks for a panel or an estimand sees either new row. Each one
filters on a key column, and `NA == "imputed"` is `NA`, which selects nothing.

## Absence is a stored SHAPE, never a missing value

`$get_estimates()` gains `rates_stored` and `rd_stored`. `$get_baselines()`
gains `smd_stored`. `$get_subgroups()` gains `strata_stored`. Each reports
whether the plan HOLDS a usable table, and none reports what is inside it.

A consumer that read a missing value as a missing table dropped a row that the
raw-slot reader kept. Four consumers did:

* The `PP results`, `ITT results` and `Weight truncation (PP)` sheets dropped
  an emulated trial whose rates table held `NA` numbers, and with it the trial
  identifiers and the rate ratio beside them.
* The `Effect modification` sheet dropped a stored subgroup level whose
  per-protocol estimates were `NA`. That discarded the intention-to-treat
  result for the same level. A real finding disappeared because a different
  estimand could not be computed.
* The `Table 1` and `Table S1`-`Table SN` sheets dropped the `SMD` column of a
  panel whose every standardised mean difference was `NA`.
* The forest figure of `$export()` dropped a stored risk-difference row whose
  values were `NA`.

All four report the stored shape now. A row with missing numbers renders blank
cells, which is what it always did.

`rates_stored` is TRUE when the stored table passes four checks. It is a
data.table. It carries the three measurement columns. It carries its
`treatment_var` attribute as a column. It holds exactly one row per arm. Those
are the four checks the raw-slot reader made.

## `$results_summary()` is the one diagnostic exception

`$results_summary()` reads `plan$results_ett` directly, and it is named as a
diagnostic rather than as a consumer. A tool that reports ABSENCE cannot read
through an interface that hides absence. The accessors report a missing slot
and a skipped slot the same way, and they expose no skip envelope and no
failure reason. This method prints exactly those three states: `"NULL"`,
`"SKIP: <reason>"` and `"OK"`.

It reports on the cache and never on a number. A caller that wants the numbers
calls `$get_estimates()`.

## `.build_forest_df()` returns `irr_estimable` in place of `warn`

The `warn` column recorded whether the Poisson fit warned. No renderer read it
and no test asserted on it. `irr_estimable` takes its place, and the two
formatters that print a ratio read that column.

## The supplement workbook carries no forest plot

`$export_tables()` wrote three forest sheets: `PP forest plot`, `ITT forest
plot` and `ITT vs PP forest`. Each sheet held a title, a treatment legend and
one image. All three are gone, and so are their six sidecar files.

The `PP results` and `ITT results` sheets report every emulated trial. Each row
carries the counts, the rates, the ratio, the risk difference, the interval and
the number needed to treat. The images repeated a subset of those numbers.

`$export()` still draws a forest figure for a manuscript. That route is
unchanged.

Every surviving sheet keeps every cell. The Love plot and the CONSORT sidecars
keep their content and their pixel dimensions. The table of contents loses the
three rows and renumbers.

## `$export_tables()` names the trial its protocol sheet documents

The `Target trial protocol` sheet documents ONE emulated trial. `featured_etts`
chose it, as a side effect of a figure argument. A list that chooses which
trials a paper figure shows MUST NOT decide what a supplement sheet describes.

`protocol_ett_id` replaces that pick and does one thing. Pass the ETT id the
sheet MUST document. An id the plan does not hold raises a warning and falls
back.

The fallback is unchanged. Without `protocol_ett_id`, the sheet describes the
first trial of the Table 1 enrollment, and otherwise the first trial in the
grid.

`$export_tables()` no longer accepts `featured_etts`, `forest_label_format`,
`forest_desc_header` or `forest_role_headers`. All four served the removed
images. A call that passes one now fails with an "unused argument" error.
`$export()` keeps its own `label_format`, `desc_header` and `role_headers` for
the manuscript figure.

## The CONSORT fallback counted every affected criterion twice

`.attrition_overall()` collapses the stored attrition table to one row per
criterion. That table holds per-trial rows, and since the global-row change it
also holds one global row per criterion. The two sets describe the same people.

The fallback summed both sets. A criterion with a global row and per-trial rows
therefore contributed both, so the diagram counted it twice. The fallback now
reads the per-trial rows and nothing else.

The fallback runs when at least one criterion has no global row. An attrition
file written before the global row existed has that shape.

Measured on the test fixture, the starting cohort read 5,700 persons and 48,000
person-trials. It now reads 3,100 persons and 24,000 person-trials.

`tables_consort_*` and the `Attrition_*` sheets both read this function, so
both move for an affected enrollment. An enrollment whose every criterion
carries a global row is untouched.

The fallback number is still not a count of persons, and that is by design. It
sums one unique-person count per trial, so it counts a person once per
sequential trial she enters. The CONSORT box prints it under a "persons"
heading, and the unit is really person-trials. For a legacy file an inflated
number beats no number, but the heading overstates what the number is. This
release records that, and does not repair it.

## The number needed to treat is stored with its interval

`$risk_difference()` gains two columns, `nnt_lo` and `nnt_hi`. `$s3_analyze()`
copies them onto the stored row, and `$get_estimates()` returns them.

One site computes them. `.tte_nntb()` maps a risk-difference interval onto the
reciprocal scale, and `.tte_rd_curve()` calls it. No consumer inverts `rd_lo`
and `rd_hi` again.

Before this release the accessor carried the point estimate alone. A figure that
wanted the interval had to invert the bounds itself, which would have put a
second estimator in the reader.

## A spans-null band has no reciprocal interval, and both bounds say so

`x -> -1/x` is undefined across zero. An interval that contains the null
therefore has no reciprocal interval, and `nnt_lo` and `nnt_hi` are `NA` on
exactly those bands. `interval_status` reads `"spans null"` there, so the
missing value has a stated reason.

The point estimate stays. It is a valid descriptive quantity.

A formatter that prints an interval MUST NOT print the point estimate alone
there. `.tte_nntb_cell()` renders an empty cell, and `.ff_rd_ci()` renders
`not estimable` for the matching risk-difference case. Both conventions are
honest. Neither prints a finite-looking interval.

## The risk-difference curve carries the numbers at risk

`$risk_difference()` gains `n_persons_at_risk_comparator` and
`n_persons_at_risk_intervention`. Each is `uniqueN()` over the person
identifier, per arm per band.

It is the same count `$survival_curve()` returns as `n_persons_at_risk`, taken
on the same panel. It is neither the row count, which counts person-trials, nor
`sum(w)`, which is the weighted risk set and the denominator of the hazard.

## No consumer opens an analysis file to render

The survival figure read one analysis file per estimand. It read it for the
numbers-at-risk row alone, because the stored curve carried no head count.

`$s3_analyze()` now stores that count, so the figure reads `$get_curves()` and
the read is gone. No renderer in `$export_tables()` or `$export()` opens an
analysis file. s3 computes and s4 formats.

A plan whose stored curve predates the count gets an error that names the
repair, which is to re-run `$s3_analyze()`. The figure MUST NOT draw a risk
table of missing values.

**One analysis-file read remains in `$export_tables()`, and it belongs to a
PRODUCER.** A baseline panel that an earlier release wrote is stale, and
`$export_tables()` then calls `$recompute_baselines()`. That method calls
`.s3_enrollment_worker()`, which is s3's own worker. The worker opens the
analysis file, computes a Table 1 panel and stores it. That is s3 running late,
not s4 computing a result.

The two are different operations and this release separates them in the tests.
One test pins that neither entry point reads a panel to render. A second test
pins that a stale panel reaches `.s3_enrollment_worker()`, and that the worker
is the only thing that opens a panel.

**Call `$recompute_baselines()` yourself when you want the refresh to be a
step you can see.** The lazy path costs minutes, and whether it runs depends on
what a cached plan happens to hold.

## A figure may look different on two machines, and the numbers do not

`ggplot2::ggsave()` chooses the program that turns a plot into pixels. It uses
`ragg` when `ragg` is installed, and cairo when it is not. swereg names `ragg`
in neither `Imports` nor `Suggests`, so the choice follows the machine.

Two machines therefore MAY write PNG files that differ. Text weight is the
visible part. Every number on the figure is the same, because the same data
reaches the same renderer either way.

**Install `ragg` when you need one appearance across a group of machines.** Do
that in the analysis project, not in swereg. swereg names no device, so a user
who prefers cairo keeps cairo.

The tests follow the same rule. They pin the data that reaches each renderer,
the file inventory an export writes, and the pixel size of every image. They
compare no pixels. A pixel comparison reports which program drew the figure,
which is not a property of the result.

# swereg 26.8.19

## The headline Table 1 carries a standardised mean difference

`$export_tables()` and the `"table1"` exhibit now write an `SMD` column on the
main baseline panel. The supplementary panels already carried one. The headline
panel did not. A reader of the primary table had to open a second sheet to
judge covariate balance.

The main panel keeps the rest of its layout. It emits no Missing row. Its
percentages divide by the non-missing denominator, so the levels of a variable
sum to 100. Only the SMD column is new.

`.baseline_panel_is_stale()` now reads every panel a cached result holds, not
the first one it finds. A cache whose main panel lacks `smd_numeric` is stale,
even when the supplementary panel carries one. That state is real: an earlier
release refreshed the supplementary panels and left the main panel alone. The
old predicate stopped at `table1_ipw_trunc` and reported the result as current.
`$export_tables()` then wrote the old table, with no error and no warning.

A cached plan needs no manual step. `$export_tables()` marks the panel stale
and calls `$recompute_baselines()`, which reads the analysis files from
`output_dir`.

Absence is still not staleness. A panel the worker never produced is `NULL`,
and the check skips it. `table1_raw` is `NULL` when no raw file sits on disk.
`table1_ipw_trunc_main` is `NULL` when the enrollment has no `ipw_trunc`
column.

The exported CSV carries the formatted `SMD` string and not `smd_numeric`.
`smd_numeric` stays a programmatic contract for the Love plot and for balance
checks, so `.export_table()` strips it before it writes the file.

## A zero-event arm gets no confidence interval

`$risk_difference()` now returns `NA` for `rd_lo` and `rd_hi` at any horizon
where either arm has no positive-weight event.

An ordinary empirical bootstrap cannot produce an event the sample does not
hold. Every replicate draws from the same event-free set, so every replicate
gives that arm a failure risk of exactly zero. The interval then carries only
the other arm's sampling variation, and treats this arm's risk as known with
certainty. That is anti-conservative. More replicates do not repair it, because
the degeneracy is in the resampling scheme and not in the sample size.

One production figure showed the defect. A row with zero weighted events in the
intervention arm left its incidence rate ratio blank, correctly. It still
printed a risk difference with a full interval, and a number needed to treat
beside it.

The point estimate stays. It remains a valid descriptive quantity, and a new
`interval_status` column says why nothing accompanies it. That column reads
`"ok"` where the interval is estimable and `"zero-event arm"` where it is not.
A reader can therefore separate an interval that spans the null from an
interval that does not exist.

The condition is evaluated per horizon and per arm, on the events up to and
including that band. An arm can have no event by week 52 and several by
week 156. The week-156 interval is then estimable, and only the week-52 one is
suppressed.

The number needed to treat suppresses itself from there. `.tte_nntb()` already
returns `NA` when the interval does not strictly exclude the null, so a missing
interval renders an empty cell.

The suppression reads the WEIGHTS. An event whose weight is zero contributes
nothing to either bootstrap sum, so it is not an event this estimator can
resample. The unweighted `n_persons_with_event_*` columns still report that
person, and the two columns answer different questions.

## The number needed to treat carries its interval

`.tte_nntb_cell()` gains `nntb_lo` and `nntb_hi`. Supply both and the cell reads
`NNTB 2,000 (1,250 to 5,000)`. Supply neither and it renders the point estimate
alone, as before.

The bounds already existed. `.tte_nntb()` has always returned them, so nothing
new is computed here.

Both bounds take the point estimate's thousands separator and its 0 decimal
places, because a fractional number needed to treat is not a quantity. The
separator between them is ` to `, the one the risk-difference column already
uses.

The printed bounds ascend on both signs, and the two branches reach that
differently. `.tte_nntb()` guarantees `nntb_lo < nntb_hi`, so the benefit branch
prints the bounds in the order it holds them. The harm branch negates each
bound, which reverses the order, so it prints the negated high bound first. The
negation is explicit and never `abs()`.

A row whose bounds are missing renders EMPTY, even when the point estimate is
finite. A point estimate printed without its interval invites a reader to treat
it as precise, and a zero-event arm is exactly where it is not.

The labels stay `NNTB` and `NNTH` in full. They are the Cochrane and GRADE
terms, and the abbreviated forms are not recognised notation.

**`.forest_rd_map()` in `R/forest_plot.R` still calls the one-argument form, so
the figure column shows the point estimate alone.** Passing the bounds there is
a separate change. The cell grows from about 10 characters to about 27, so the
column's relative width MUST be revisited at the same time.

## The survival figure carries numbers at risk

`.render_survival_curve()` now composes two panels: the curve on top and a
numbers-at-risk table beneath it. `$survival_curve(save_path = ...)` and
`$export()` therefore write a figure a reader can interpret without going back
to the data.

The table is populated from `n_persons_at_risk`, the count of DISTINCT PEOPLE.
It is never populated from `at_risk`, which is the weighted risk set `sum(w)`
and is the hazard denominator. The two differ on every real panel, because the
weights are not 1 and because one woman holds several sequential trials. A risk
table reports people, so the panel title says "Numbers at risk (persons)".

Both panels are given the same x breaks and the same x limits, from one scale
built once. A risk table whose columns do not sit under the curve's ticks is
worse than no risk table, so that sharing is checked directly: a test compares
the two built panels' x range and x breaks.

The x axis is now drawn once, under the table at the foot of the composition,
and the curve panel's own x axis is blank.

A panel holds more bands than the table has room for, so the labelled times are
thinned. The rule counts backwards from the last band in steps of one fixed
stride. Every gap between adjacent labelled times is therefore the same width,
and the last band is always labelled. Counting forwards and then adding the
last band instead leaves one short final gap, and six-digit counts printed in
that gap overlap. On a 156-week panel the final gap was 12 weeks against a
20-week stride, and two counts rendered as one number. A test pins the
property: every gap MUST be equal, and the last band MUST be kept.

The returned object is a `patchwork`. It still inherits `ggplot`, and the curve
is the composition's own plot, so `ggplot2::layer_data()` and
`ggplot2::get_labs()` applied to the return value describe the curve exactly as
before. `patchwork` was already a dependency; nothing new was added.

Whether the table is legible at publication size is a human judgement and has
no test.

## Survival curves can plot cumulative failure

`$survival_curve()` gains a `scale` argument. `"survival"` is the default and
is unchanged: it plots `surv`, starting at 100%. `"cumulative_failure"` plots
`1 - surv`, starting at 0, which is readable for a rare outcome where the
survival curve sits pinned against the top of the panel.

Deaths are censored, not modelled as a competing risk, so `1 - surv` is
cause-specific failure under independent censoring and NOT a competing-risk
cumulative incidence function. The y label says "Weighted cause-specific
cumulative failure" for that reason, and a test pins the wording.

The plot is now built by a pure internal renderer, `.render_survival_curve()`,
which returns a `ggplot` and writes nothing. `$survival_curve()` delegates to
it. Both scales therefore share one code path and cannot drift apart. The
origin row is transformed with the rest of the curve, so a cumulative-failure
curve starts at 0 rather than at 1.

`$export()` now writes the survival figure on the cumulative-failure scale.
Nothing else about the figure changed.

## Table 1 keeps the unrounded standardised difference

`.swereg_table1()` formatted the standardised mean difference to three decimals
and discarded the double it formatted. Anything downstream that needed the
number had to parse the string back, which silently rounds: an SMD of 0.8911
returned as 0.891.

The double is now kept as a real column, `smd_numeric`, alongside the `SMD`
display string. It is a column and not a row-parallel attribute on purpose. Row
subsetting, reordering, `rbindlist()` and a `qs2` round trip are all free to
drop an attribute; none of them can separate a column from its own row.

`smd_numeric` is a programmatic contract, not a display column, so it is
removed before a panel is written to a worksheet or a CSV. No rendered table
gains a column.

## `$export_tables()` refreshes a baseline cache that predates `smd_numeric`

The staleness test asked only whether a cached panel carried the
`swereg_table1` class. A panel written between the `swereg_table1` change and
this one carries that class and has no `smd_numeric`, so it was declared
current and never recomputed. A panel is now stale when it lacks either.

## New sheet: Love plot

`$export_tables()` writes a "Love plot" sheet for the Table 1 enrollment,
showing the absolute standardised difference for every covariate twice:
unweighted, and under the IPW-truncated analysis weights. The conventional 0.1
balance threshold is drawn as a dashed reference line. PNG (300 dpi) and vector
PDF sidecars are written next to the workbook, as the forest plot sheets do.

The weighted series is `table1_ipw_trunc`, not `table1_ipw`. The truncated
weights are the analysis weights; the untruncated panel is a robustness
variant and is not plotted.

## The person id is always present, and numbers at risk count persons

`TTEDesign$new()` defaulted `person_id_var` to `NULL`. Nothing in the pipeline
relied on that default: `create_skeleton()` names the person identifier `id`,
and `TTEPlan` already passes `"id"` whenever an argset does not override it. The
default is now `"id"` too, so a `TTEDesign` built by hand matches one the plan
builds, and a person id is always available.

Two guards existed only to cover the `NULL` case, one in `$summary()` and one in
`$rates()`. Both are removed. `$rates()` still emits `n_persons`; it just stops
being conditional and can no longer report `NA` for a design that omitted the
argument.

`$survival_curve()` gains a column, `n_persons_at_risk`. It is an unweighted
count of distinct people per arm-band, taken over `design$person_id_var`.

Three different numbers live in one arm-band cell of a sequential target trial
emulation, and only the third is what a risk table under a survival panel
reports:

* rows, which are person-trials, because one person is enrolled into many
  sequential trials
* `at_risk`, the weighted risk set `sum(w)`, which is the hazard denominator
* `n_persons_at_risk`, the head count

On a large national-registry panel the gap is real: person-trials outnumber
persons, roughly one trial per person on average, with a minority of women
holding several. Reporting rows where persons are meant overstates the sample.

`at_risk` is unchanged and still drives the hazard, so no estimate moves. The
existing column set is unchanged; this adds one.

## New method: `$risk_difference()`

`TTEEnrollment` gains `$risk_difference(weight_col, n_boot, seed, conf_level)`.
It returns, at each band, the signed cause-specific risk difference with a
percentile bootstrap interval, alongside the two arm survivals it was formed
from and per-arm event counts.

The sign convention is fixed and is not a display choice:

```
RD(t) = Risk_intervention(t) - Risk_comparator(t)
      = [1 - S_intervention(t)] - [1 - S_comparator(t)]
      = S_comparator(t) - S_intervention(t)
```

The stored `rd` is signed. A protective intervention gives a negative risk
difference, and that minus sign is the result. `abs()` appears nowhere in the
arithmetic. An absolute risk difference has a correct-looking magnitude at every
band, so no assertion on the point estimate alone can see a stray one. Two
independent tests can, and both ship with the method: the mirror test (relabel
the arms, require the sign to flip), and the person-trial/person layout
equivalence test, whose bootstrap replicates straddle zero even where the point
estimate does not, so `abs()` breaks 83 of its 600 bit-identical replicate
comparisons. Pinning the convention in two unrelated places is stronger than
pinning it in one.

The two survivals are the ones `$survival_curve()` already builds, from the same
weighted discrete-time hazard, so the figure and the risk difference are the
same numbers. Deaths are censored rather than modelled as a competing risk, so
this is a cause-specific risk difference under independent censoring.

### The bootstrap resamples people

A woman contributes several sequential trials. Those trials share her baseline
covariates and can carry the same outcome event, so they are not exchangeable:
the resampling unit is the person, and a drawn person brings all of her rows.
Resampling person-trials, or rows, would treat one woman's repeated entries as
independent observations.

**One multiplicity vector is drawn per replicate and applied to both arms.**
Persons cross arms: a woman can be a comparator in an early trial and an
initiator in a later one, and on a large national-registry panel a few percent
of women appear in both. Drawing a separate resample per arm leaves
the point estimate unbiased and biases the interval, because it discards the
covariance between the two arms' survival estimates. No point estimate can show
that, so the shared vector is pinned by a test that reads back the vector each
arm was actually multiplied by.

A replicate that draws no person for an arm, or that empties a band, yields `NA`
for that band and onwards; the percentile step drops those rather than erroring
or substituting a value. The replicate matrix the interval was read off is
attached as the `rd_boot` attribute.

### Event counts are distinct people

`n_persons_with_event_comparator` and `n_persons_with_event_intervention` count
distinct PEOPLE who had the outcome at or before that band, in that arm. They
are deliberately neither row counts nor person-trial counts. One woman can carry
the event in two of her sequential trials; she is one person who had the
outcome, and the columns are named so that cannot be misread.

`$summary()` and `$rates()` report the event ROW count instead, which equals the
number of event-bearing person-trials, and on real analysis panels the two
numbers differ.

### Performance

The weighted hazard is `sum(w * event) / sum(w)` over the rows at risk, and both
sums decompose additively over persons. The panel is therefore aggregated once
into two dense matrices per arm, and a replicate is a single matrix product
against the multiplicity vector rather than a second pass over the panel.

At the shape of a large national-registry panel (person-trials outnumbering
persons, 13 bands, a few million rows) a replicate costs about 0.05 s, so 500 of
them cost about half a minute after the one-off aggregation. The point estimate
is bit-identical to `$survival_curve()` on the same data, which is the property
that makes the aggregation an optimisation rather than a second estimator.

The matrix row is the person-trial rather than the person, because the bootstrap
index is taken over the person-trial table. That is a representation of the
person-indexed sum, not an approximation of it: a person's multiplicity is
carried by every trial she owns, so the two forms collapse onto each other. A
test applies one person multiplicity vector to both forms across 300 draws,
degenerate draws included, and requires the risk differences to be bit-identical.

No new dependency: these are base dense matrices, 46 MB each at that shape.

The method computes; nothing writes it to a workbook or a figure yet.

## Number needed to treat for benefit

`.tte_nntb()` inverts the signed cause-specific risk difference:

```
nntb    = -1 / rd
nntb_lo = -1 / rd_lo
nntb_hi = -1 / rd_hi
```

The minus sign is load-bearing. `$risk_difference()` returns
`Risk_intervention - Risk_comparator`, so a protective intervention gives a
negative risk difference, and negating the reciprocal makes a benefit read as a
positive number of women. The value stays signed: a harmful intervention
returns a negative number and keeps it, and `abs()` appears nowhere in the
arithmetic. It is named `nntb` and never "NNT", because a reader who meets a
column headed "NNT" assumes the number is positive and means benefit.

The interval must strictly exclude the null. The map `x -> -1/x` is monotone
increasing on each side of zero and undefined across it, so an interval that
contains zero has no reciprocal interval to report. A bound of exactly zero
touches the null and is therefore not exclusion of it: all three values are
`NA` there, on both sides. Loosening either comparison to `>=` or `<=` is a
one-character change that reports a finite number needed to treat for an
interval compatible with no effect.

That `NA` means undefined, not absent, and it does make the displayed value
depend on the interval: a band whose interval crosses zero shows nothing. It is
a property of the reciprocal transform, not a decision to hide a
non-significant result.

Deaths are censored rather than modelled as a competing risk, so this inverts a
cause-specific risk difference under independent censoring.

`.tte_nntb_cell()` renders one cell, and the SIGN chooses the label. A positive
value renders `NNTB 250`, the number needed to treat for benefit. A negative
value renders `NNTH 400`, the number needed to harm. Those are opposite clinical
statements, and the label is the only thing that separates them.

The magnitude comes from negating the stored value, never from `abs()`. The
harm branch negates explicitly, so a reader of the source sees which branch they
are in. The cell carries 0 decimal places and a thousands separator, because a
fractional number of people is not a quantity.

Every row gets a cell. The function took an `outcome_role` argument and rendered
a number for the primary outcome only. That guard is gone, so a secondary
outcome now shows its own number needed to treat.

## The forest plot shows the risk difference

`.render_combined_forest_plot()` gains an optional `rd_lookup`, keyed by
`ett_id`. When it is supplied, two text columns join the figure. The first is
the signed cause-specific risk difference per 10,000 people, with its interval.
The second is the number needed to treat it inverts to.

The value is signed and stays signed. A protective intervention reads `-4.88`
per 10,000 and a harmful one reads `+4.88`; the sign is written explicitly on
the point estimate and on both bounds, so a harm can never be read as an
unsigned magnitude. `abs()` appears nowhere in the formatting.

A row the lookup does not carry renders an empty cell, not `"NA"` and not a
zero. An empty cell says nothing was computed; either of those says something
was.

The two columns are left out of the layout entirely when nothing populated
them, rather than reserving width for a quantity nobody computed. An existing
forest figure is therefore unchanged in layout.

## The risk-difference header states the level the interval was computed at

`conf_level` is configurable on a `"forest"` exhibit spec, so a hard-coded
`95% CI` header would put a 90 percent interval under a 95 percent label. The
number would be right and only the label would lie, which is the version of
this defect that survives into a manuscript: a wrong number gets questioned, a
mislabelled one gets believed.

The header is now built from the level, so `conf_level = 0.9` heads the column
`90% CI`. An integer percentage prints with no decimal point and a non-integer
one keeps only the digits it needs, so `0.975` heads it `97.5% CI`.

One value, one source, and it is checked rather than trusted. `.forest_rd_row()`
copies the level off the curve's own `conf_level` attribute, set by the
computation that produced the bounds, so the level travels with the numbers it
belongs to. `.render_combined_forest_plot()` errors if the level it was told to
print disagrees with the level the lookup carries, or if one lookup mixes two
levels. A figure cannot be rendered with a header that contradicts its own
interval.

The neighbouring IRR header keeps a fixed `95% CI`, and correctly so. `$irr()`
accepts no confidence level, and `.fit_irr()` computes its bounds with a
hard-coded 1.96 multiplier. So 95 percent is the only level it can be.

## The two event counts say which is which

The per-arm columns beside the risk difference are `sum(event * weight)` over
event ROWS. One woman who carries the outcome in two of her sequential trials is
counted twice, and each time by her weight. The risk difference counts distinct
PEOPLE, unweighted. Two figures that read as the same quantity and are not is
worse than one, so the arm headers say "weighted events / PY".

The distinct-person counts are reported on the `PP results` and `ITT results`
sheets, under headers that name persons. See "The results sheets carry the risk
difference and the person counts" below.

## `$export()` computes the risk difference for a forest, on request

The risk difference is not in the cached per-ETT results, so it cannot be read
back the way the rates and the IRR are. A `"forest"` exhibit spec takes
`risk_difference = TRUE`, and the export path then loads each featured ETT's
analysis panel and computes it.

That costs minutes per ETT, which is why it is opt-in and why `n_boot`
(default 500), `seed` (default 1) and `conf_level` (default 0.95) are exposed
on the spec: run a smoke pass at a handful of replicates before spending the
full one. Without `risk_difference = TRUE` nothing is loaded and nothing is
computed.

The row takes the LAST band of the curve, the risk difference at the end of
follow-up. The person counts are already cumulative through that band.

## A survival-figure `ylim` must declare its own scale

`$export()` draws the survival figure on the cumulative-failure scale. It was
still passing `spec$ylim` through untouched, so a survival-scale window such as
`c(0.95, 1)` would clip the entire failure-scale curve out of view through
`coord_cartesian()`: a blank panel, with no error and no warning. No spec in
this package sets `ylim` today, so nothing was broken in practice, but the
documentation advises setting an explicit range for a publication figure, which
is exactly when it would bite.

`ylim` now requires a companion `ylim_scale`, either `"survival"` or
`"cumulative_failure"`. A survival-scale window is translated onto the plotted
scale, so `c(0.95, 1)` becomes `c(0, 0.05)` and shows the same band of the
figure it always did. An undeclared window is an error, not a guess.

The scale is declared rather than assumed because neither pure convention is
safe on its own: the mirror mistake, a failure-scale window read as a
survival-scale one, blanks the panel just as quietly.

## New sheet: Target trial protocol

`$export_tables()` writes a "Target trial protocol" sheet in the Dickerman
Table S1 layout: `Protocol component`, `Target trial specification`,
`Target trial emulation`, over the seven components (eligibility criteria,
treatment strategies, assignment procedure, outcome, follow-up period, causal
contrast, analysis plan).

The two content columns have different provenance, and that is the point. The
specification column is the study team's own protocol prose, authored in the
spec under a new optional `target_trial:` key, appended to the clinical fields
the spec already carries (criterion names, arm labels, outcome names,
follow-up labels). The emulation column is **rendered** from the nested
`implementation:` blocks the spec already carries. It is never read from YAML
prose, and there is deliberately no `emulation:` key for it to read: a
hand-written emulation column can drift away from the pipeline it claims to
describe, and a rendered one cannot.

A spec materialises many ETTs, and one protocol table cannot stand for all of
them. The sheet names in its title row the single ETT it documents: the
enrollment, the outcome and the follow-up horizon. It documents the first
featured ETT when `featured_etts` is supplied, otherwise an ETT of the Table 1
enrollment.

`target_trial:` is optional. A spec without it still renders the sheet, with
the derived clinical text alone in the specification column. Two components,
causal contrast and analysis plan, have no clinical section anywhere in a spec,
so for those the `target_trial:` entry is the whole cell.

`tteplan_read_spec()` already tolerated unknown top-level sections, so no
existing spec needs changing to keep parsing.

The eligibility row renders global inclusion criterion objects as well as
global exclusion criteria. No spec written so far has one: `inclusion_criteria`
holds only the `isoyears` pair, so that loop renders an empty set today. It is
there so a spec that later adds one is not silently dropped from the table.

## The forest plot reads left to right, and states its horizon

`.render_combined_forest_plot()` composes its text columns in one fixed order:

1. the row description
2. intervention weighted events / PY
3. comparator weighted events / PY
4. risk difference
5. number needed to treat
6. IRR
7. the forest panel

A reader meets what each arm contributed, then the absolute difference between
them. The number needed to treat says how many people that difference is. The
ratio follows, and then the panel that draws the ratio.

A test pins the composed ORDER, not merely that the columns exist. Two adjacent
columns swapped still draws, and every label is still on the figure.

The "People with event" column is gone from the figure. Those counts now go to
the `PP results` and `ITT results` sheets, where a reader can sort and copy
them. See "The results sheets carry the risk difference and the person counts"
below.

The number-needed-to-treat column is composed only when a risk difference
populated it, and a row whose interval spans the null renders an empty cell.

Three headers now name a time, and the wording separates a PERIOD measure from
a POINT measure:

```
IRR over 156 wks              Risk difference per 10,000    Number needed to treat
(95% CI)                      at 156 wks (95% CI)           at 156 wks
```

The horizon is derived from the rows, never written as a literal.
`.forest_horizon()` reads `follow_up` off the forest rows, and the rows MUST
carry one value. Rows that mix horizons raise an error, because one header
cannot state two. This is the contract `.forest_rd_conf_level()` already keeps
for the confidence level. A caller that puts two follow-up horizons on one
forest MUST now split it into two figures.

The two `weighted events / PY` headers take no time reference. `PY` already
names the exposure measure, and five repetitions of the horizon would be noise.

`.render_itt_vs_pp_forest()` is a different renderer and is unchanged.

## The results sheets carry the risk difference and the person counts

`PP results` and `ITT results` gain four columns after the measurement block:

| Column | What it is |
|---|---|
| `Persons with event (int)` | Distinct people in the intervention arm who had the outcome |
| `Persons with event (cmp)` | Distinct people in the comparator arm who had the outcome |
| `Risk difference per 10,000` | The signed risk difference at the horizon |
| `Risk difference 95% CI` | Its interval, at the level the bounds were computed at |

The counts are unweighted head counts of people. `Events (int)` and
`Events (cmp)` beside them are weighted sums over event ROWS. The two differ on
every real row: the weights are not 1, and one woman holds several sequential
trials.

The risk difference keeps its sign, and Excel prints a `+` on a positive value.
The interval header states the level the bounds carry, so a 90 percent interval
heads its column `Risk difference 90% CI`. Two levels under one header raise an
error.

The four columns appear only when a risk difference was computed. Computing one
costs minutes per ETT, so most exports have none, and four empty columns would
claim a quantity nobody computed. A sheet with no risk difference keeps its 14
columns unchanged.

`$export()` caches each computed risk-difference row onto
`plan$results_ett[[ett_id]]`, under `rd_pp_trunc` or `rd_itt`. The sheets read
that cache. So a workbook carries these four columns after a `"forest"` exhibit
ran with `risk_difference = TRUE` on the same plan object, and not otherwise.

# swereg 26.8.11

## `add_rx()` no longer aborts on an ISO year outside the converter's range

`cstime` supports ISO years of roughly 1900 to 2200. A supplied
`start_isoyearweek` or `stop_isoyearweek` outside that range, such as
`"0001-01"` or `"9999-01"`, made the calendar check return `NA` rather than
`TRUE` or `FALSE`. The `NA` propagated into the row-drop test and the call
failed with `missing value where TRUE/FALSE needed`.

An unparseable year is malformed input. It is now dropped with the same warning
as `"2019-99"` and `"2019-53"`, and the surrounding valid rows are kept. One bad
year no longer destroys the batch it arrived in.

# swereg 26.8.10

## `add_rx()` rejects a week 53 the ISO year does not have

Whether an ISO year has a week 53 depends on the year: 2020 has one, 2019 does
not. The 26.8.9 well-formedness check was syntactic, so it accepted
`"2019-53"`, which then silently marked the 2019 annual row.

The check is now calendar-aware. It reads the last week of an ISO year off the
package's own converter, using the fact that 28 December always falls in the
final ISO week of its own ISO year, so the check cannot disagree with the
conversion it guards. `"2020-53"` remains valid.

## `add_rx()` no longer reads `fddd` when `fddd` defines nothing

`round(fddd)` was evaluated before the branch that uses it, so a caller who
supplied `stop_isoyearweek` or `stop_date` — meaning `fddd` defines no
endpoint — still needed `fddd` to be numeric. A factor `fddd` errored with
`'round' not meaningful for factors` even though the value was never used.
`fddd` is now read only when the stop endpoint is actually resolved from it.

## Note for callers upgrading from 26.8.8 or earlier

Retiring the rule that preserved caller-supplied ISO week columns (26.8.9)
removes a capability, and callers relying on it should know what it was.

**Lost:** supplied weekly endpoints outside the skeleton's weeks used to express
"match only if these exact skeleton rows exist". A supplied
`start_isoyearweek` of `"2019-51"`, on a skeleton whose weekly period starts in
2020, matched nothing and left every row FALSE. It now remaps onto the
`"2019-**"` annual row and marks it.

**Workaround:** filter or clip such rows before calling `add_rx()`. Drop the
prescriptions whose endpoints fall outside the skeleton's weekly period, or clip
their endpoints to that period, and the result is what the old behaviour gave.
There is no argument that restores it, and this is intentional: the remap is the
same rule every derived endpoint follows.

# swereg 26.8.9

## `add_rx()` resolves, validates and remaps every interval by one rule

`add_rx()` accepts four optional columns: `start_date`, `stop_date`,
`start_isoyearweek` and `stop_isoyearweek`. Versions 26.8.7 and 26.8.8 branched
on **which of them the caller supplied**, so the sixteen combinations each
needed their own reasoning and several were never specified. Validation could
be skipped by supplying the right column, and a column that defined nothing
could change the answer.

There is now one path. Each endpoint is resolved once, from its own provenance:

* start: supplied `start_isoyearweek`, else the ISO week of the supplied
  `start_date`, else the ISO week of `edatum`.
* stop: supplied `stop_isoyearweek`, else the ISO week of the supplied
  `stop_date`, else the ISO week of `edatum + round(fddd) - 1`.

The resolved pair is then validated on a single expression that every
combination reaches, and only then remapped onto the annual rows. Three
behaviours change as a result.

**A column that defines no endpoint can no longer change the result.** The
duration filter now applies if and only if the stop endpoint is actually
resolved from `fddd`. Previously, adding a `stop_date` column to a prescription
whose interval was already fully given as ISO weeks flipped the output from no
coverage to two weeks.

**An invalid interval is dropped whatever the caller supplied.** A row is
dropped, with one warning naming the count, when either endpoint is missing,
when either endpoint is not a well-formed ISO week, when the start week is later
than the stop week, or when both endpoints came from dates and the start date is
later than the stop date. The last of these catches an interval inverted by days
but contained in one ISO week, which compares equal as week strings.

**A malformed ISO week is rejected.** A supplied `start_isoyearweek` of
`"2019-99"` used to be injected into the interval ranking as a synthetic
boundary and silently marked ten skeleton rows. It is now dropped with a
warning. Well-formed means `"YYYY-WW"` with week 01 to 53, or the annual
`"YYYY-**"` form.

An endpoint that is well formed but outside the skeleton's weeks is **kept**.
`"2020-53"` is a real week that a skeleton ending in `"2020-52"` does not carry,
and the interval still covers every week before it. The same applies to a
derived interval running past `date_max`.

## `add_rx()` remaps caller-supplied ISO endpoints too (behaviour change)

Previously a caller-supplied `start_isoyearweek` or `stop_isoyearweek` was
preserved exactly and never remapped onto the annual rows. **A supplied endpoint
that falls before the weekly period now marks the annual row of its ISO year,
where it previously matched nothing.**

On a skeleton whose weekly period starts in 2020, a supplied
`start_isoyearweek` of `"2019-51"` now marks the `"2019-**"` row. That is the
same rule every derived endpoint follows, and it is what repair 12 in 26.8.7
exists to express: a prescription covering week 51 of 2019 **is** covered by the
2019 annual row. A caller who wants the annual row can still write `"2019-**"`
directly, and that continues to work.

## Corrected warning after the ISO week conversion

The post-conversion filter counted **tagged matches**, so one prescription
matching two requested codes reported `2 prescription rows dropped`. It now
counts source rows. Its text also named only `start_isoyearweek` while a missing
stop endpoint fired it just as readily, and claimed a value could be "unknown to
the skeleton", which it never could.

# swereg 26.8.8

## `add_rx()` no longer writes into the prescription table (behaviour change)

`add_rx()` used to write `start_date`, `stop_date`, `start_isoyearweek` and
`stop_isoyearweek` back into the caller's `lmed` by reference, and to skip
recomputing any of them on a later call if the column was already present. It
now computes all four on a local working copy. **`lmed` is read, never
written.**

This is a behaviour change, not only a bug fix. Any caller that relied on the
helper columns appearing on its own `lmed` after the call must compute them
itself. Nothing inside swereg did.

The write-back became unsafe in 26.8.7, because the ISO week columns now depend
on the skeleton via the annual remap. Three consequences, all fixed here:

* **Reusing one `lmed` across two skeletons.** The second call reused the first
  skeleton's remap. A 2019 prescription that should have marked five weekly rows
  on a skeleton whose weekly spine covers 2019 instead marked one annual row and
  no weekly rows.
* **Schema depending on unrelated bad rows.** With `fddd = c(0, 7)` the caller's
  table gained one helper column; with `fddd = c(1, 7)` it gained four. Adding
  one invalid row changed the schema seen by every valid row.
* **A cached `stop_date` bypassing the duration policy.** After one call wrote
  `stop_date` back, a later call treated that column as caller-supplied and
  skipped the duration filter entirely, even if `fddd` had changed to `0`.

## `add_rx()` validates the coverage interval as dates, before the annual remap

The annual remap collapses every pre-weekly date of one ISO year onto a single
`"YYYY-**"` string. An inverted interval whose endpoints share an ISO year
therefore came out of the remap as an equal pair and read as one week of valid
coverage. With `edatum = 2018-05-10` and a caller-supplied
`stop_date = 2018-05-01`, 26.8.7 marked the 2018 annual row TRUE and warned
about nothing; 26.8.6 dropped the row.

`add_rx()` now checks the interval as dates, before the ISO week conversion,
whenever both ISO week columns are derived. A row with a missing `start_date` or
`stop_date`, or with `stop_date` before `start_date`, is dropped with a warning
naming the count. The post-conversion filter stays as a backstop for the
remaining case, a caller-supplied ISO week column, and its warning text now
describes that case instead of blaming `fddd`.

Which drop applied on which path depended on the caller's column set. That
turned out to be the wrong shape and was replaced in 26.8.9, which validates
every interval on one rule; read that entry rather than this one for the
current behaviour.

## Note on the annual/weekly string ordering

The 26.8.7 notes justified the ordering of `"YYYY-**"` against `"YYYY-WW"` from
ASCII byte values. That reasoning was wrong: R's `<`, `min()` and `sort()` use
locale collation, not raw bytes. The conclusion is unchanged and was checked
empirically — annual strings sort below the weekly strings of the same year
under both `C` and `en_US.UTF-8`.

# swereg 26.8.7

## `add_rx()` reaches the annual skeleton rows

`create_skeleton()` builds two spines: an annual one (`"<year>-**"`,
`is_isoyear == TRUE`) covering every ISO year before the weekly period, and the
weekly one covering the study period. The `add_diagnoses()` /
`add_operations()` / `add_quality_registry()` family already remapped events
that fall before the weekly spine onto the annual rows. `add_rx()` did not, so
a prescription whose coverage started before the weekly period matched no
skeleton row at all and was silently lost.

`add_rx()` now applies the same remap, to **both** ends of the coverage
interval:

* A prescription entirely before the weekly period sets the annual rows of the
  ISO years it spans.
* A prescription that starts before the weekly period and ends inside it sets
  the annual rows of the pre-weekly portion **and** the weekly rows it covers.
  Weekly resolution is not lost.
* A prescription entirely inside the weekly period is unchanged.

Annual strings sort below every weekly string (`*` is `0x2A`, `0` is `0x30`),
so the integer ranking used by `foverlaps()` keeps a boundary-spanning interval
contiguous.

If the skeleton has no weekly rows at all, the remap is skipped rather than
taking `min()` of an empty set.

## `add_rx()` treatment periods are one day shorter, and non-positive durations are dropped

Two fixes to the interval derived from `edatum` and `fddd`:

* **Inclusive end.** `foverlaps(type = "any")` matches inclusively at both
  endpoints, so the old `stop_date <- edatum + round(fddd)` covered
  `round(fddd) + 1` days. It is now `edatum + round(fddd) - 1`. A 7-day
  prescription starting on a Monday no longer reaches into the following week.
* **Non-positive and non-finite durations.** Rows with `round(fddd)` missing,
  non-finite or `<= 0` are dropped **before** the ISO week conversion, with one
  warning naming the number of rows dropped. Filtering afterwards could not
  express this: once collapsed to weeks, `fddd = 0` and `fddd = -1` both look
  like a valid single-week interval and survived the existing inverted-interval
  filter. Negative durations occur in the register.

Both changes apply only when `add_rx()` derives the interval itself. A caller
who supplies `stop_date`, `start_isoyearweek` or `stop_isoyearweek` keeps full
control: the duration filter does not run on those rows, and no remap is applied
to a supplied ISO week column. Other row drops are still possible — see 26.8.8,
which adds date-level validation of the interval and describes exactly which
drops apply on which path.

**Callers should expect fewer TRUE weeks per prescription and more TRUE annual
rows than before.**

# swereg 26.8.6

## Study-specific code moved out of swereg

The study-specific functions are gone from swereg. They now live in a separate
downstream package, which both consumer repositories already call.

Removed:

* The two study `add_lmed()` entry points, their internal helpers, and the two
  files that held them.
* The study approach workbook under `inst/`. The downstream package now ships
  it.

Callers of either function MUST switch to the downstream package. swereg had no
internal call sites, so nothing else in the package changes behaviour.

`glue` and `readxl` leave `Imports`. The two removed files were their only
users.

# swereg 26.8.5

## `qs2_read()` reads standard qs2 format only

`qs2_read()` now calls `qs2::qs_read()` directly. It previously tried
`qs2::qd_read()` (qdata format) first and fell back to the standard reader when
the error message matched `"qs2 format"`. The qdata attempt is gone.

Two consequences, both visible to callers:

* **Files in qdata format are no longer readable.** A file written with
  `qs2::qd_save()` now raises `qdata format detected, use qs2::qd_read`. swereg
  has never written qdata files itself, so this affects only a caller who
  produced one by hand and read it back through `qs2_read()`. Read such a file
  with `qs2::qd_read()` instead.
* **Error messages change for every failing input, not only qdata ones.** The
  removed `tryCatch()` wrapper meant that a corrupt or missing file surfaced
  the error raised by `qs2::qd_read()`. The error now comes from
  `qs2::qs_read()`. A corrupt file reports `Unknown file format detected`; a
  missing file reports `Failed to open for reading`. Code that matches on these
  strings needs updating.

The `check_version()` hook is unchanged: `qs2_read()` still calls
`obj$check_version()` when the object read is an environment that has one.

# swereg 26.8.4

## `qs2_write_atomic()` now delegates to batchit

The atomic-write engine -- temp file in the destination directory, then
`file.rename()` into place -- was implemented twice, once here and once in
`batchit`. Two copies of the same guarantee drift: one gets a fix, the other
does not, and they stop agreeing about what "atomic" means.

`swereg::qs2_write_atomic()` is now a one-line delegation to
`batchit::write_qs2_atomically()`. The exported name, the arguments and the
documented contract are unchanged, so no calling code needs to change.

One visible difference: the rename-failure error is raised by batchit, so its
prefix reads `write_qs2_atomically()` instead of `qs2_write_atomic()`. Code
that matches on the message text (rather than on the `could not rename` part)
should be updated.

## The batchit lockdown now bans dispatch, not the string `batchit::`

The test guard used to allow `batchit::` only in `R/batch_adapter.R`. That was
the wrong rule: it banned a name, when what needs confining is *dispatch* --
target selection must stay behind the mockable `.batch_*` wrappers. A plain
utility like `write_qs2_atomically()` has nothing to dispatch and nothing to
mock, so confining it bought nothing and blocked this delegation.

The guard now classifies each batchit symbol:

* **Dispatch** symbols (`run`, `run_and_collect`, `package_function`,
  `run_and_write_files_atomically`,
  `stream_from_parent_and_write_files_atomically`, `where_to_write_output`)
  remain adapter-only.
* **Primitives** (`write_qs2_atomically`) may be called from anywhere in
  swereg.
* Any **unclassified** batchit symbol fails the guard, so a new batchit export
  cannot be used here without review.

Three hardenings came with the reclassification: the adapter exemption now
compares the repo-relative path `R/batch_adapter.R` rather than the basename
(a file at `R/subdir/batch_adapter.R` used to be silently exempt), every
`batchit:::` reach into batchit internals is rejected outright, and the
classifier is itself tested against synthetic fixtures for each branch.

# swereg 26.8.3

## The TTE stages now commit their outputs all-or-none

Every parallel stage of `tteplan` — s1a, s1b, s1c, s1d and s2 — used to hand each
worker the final path of the file it was to produce, and the worker wrote there
itself. A worker that died partway through left whatever it had already written
sitting at the real filename, indistinguishable from a complete result. The next
run then read it and carried on.

The stages now declare their output paths in the parent and let `batchit` commit
them. A worker either returns its results (s1b, s1c, s2) or writes to a staging
path it is handed at run time (s1a, s1d); in both cases the final filenames come
into existence only after the item finishes cleanly.

**The defect this fixes, concretely.** s1d produces a matched pair — the raw
enrollment file and its imputed counterpart — with minutes of imputation, IPW
estimation and weight truncation between the two writes. A crash, an OOM kill or
a cancelled run inside that window left a new `file_raw` next to a stale or
missing `file_imp`. Nothing downstream could tell: s2 read the pair and produced
estimates from two different states of the same enrollment. Both files are now
committed together or not at all, so the previous pair survives a failed run
intact.

**What this means for you.**

* An interrupted `s1_generate_enrollments_and_ipw()` or
  `s2_generate_analysis_files_and_ipcw_pp()` no longer leaves a partial file
  that a later run mistakes for a finished one. Re-run after a crash and the
  result is the same as if the crash had not happened.
* A missing s1a cache is now a loud error naming the file, instead of a silent
  recomputation that made the stage look slow for no visible reason.
* File contents, names and locations are unchanged, as is the qs2 codec. Objects
  written before this release read back exactly as before; no re-run is needed.

Internal: the workers no longer take output-path arguments at all, and
`.s1b_attrition_path()` (which named a file nothing read) is gone. New contract
tests cover each stage's declared outputs and s1d's atomic pair.

# swereg 26.8.2

* **Migrated the dispatch adapter to batchit's new public API (batchit >= 26.7.20, "naming v2").** `R/batch_adapter.R` now calls `batchit::package_function()` / `run()` / `run_and_collect()` / `stream_from_parent_and_write_files_atomically()` instead of the removed `batch_target()`/`batch_run()`/`batch_stream()`. The internal `.batch_target`/`.batch_run`/`.batch_stream` wrapper names are unchanged, so the s1/s2/s3 call sites and test mocks are untouched; `.batch_run` routes on `collect` (`TRUE` → `run_and_collect`, `FALSE` → `run`), preserving the old per-site behaviour. Added a `.batch_where_to_write_output()` wrapper so `staged_writer` targets resolve their output path without naming `batchit::` outside the adapter.
* **`save_rawbatch()`'s parallel path migrated to the atomic declared-output commit contract.** The `n_workers > 1` path now dispatches through `stream_from_parent_and_write_files_atomically()` with `style = "staged_writer"`: `.rawbatch_write_worker()` writes its slice to `where_to_write_output("rawbatch")` (keeping `nthreads = 1` per daemon) and batchit commits it atomically. The serial (`n_workers == 1`) path inlines `qs2_write_atomic()` directly. Final rawbatch file contents are unchanged.

# swereg 26.8.1

* **Phase 5′ — removed the TTE stage resume/cache heuristics.** `s1_generate_enrollments_and_ipw()` and `s2_generate_analysis_files_and_ipcw_pp()` lose their `resume` argument; `s3_analyze()` loses `force` and now recomputes every targeted result on each call. The s1 work directory is transient dataflow, cleared at the start of every run and removed on success — no cross-run cache. Removed `tteplan_s1_cache_delete()` and the internal `.s1_cache_key`/`.skeleton_manifest_on_disk`/`.assert_skeleton_selection`/`.resume_fresh`/sentinel machinery. **Rawbatch skip-if-exists and skeleton phase-replay caching are unchanged.** Costs (accepted): a killed s1 restarts from zero; a mid-run s3 crash means full recompute.

# swereg 26.8.0

## Internal — the batch dispatcher is now `batchit`; swereg is a thin adapter

Phase 4 step 3 (PROJECT.md): the one subprocess dispatcher — signed off through
Phases 0–3 and shrunk in Phase 4 step 1 — has been **extracted into its own
package, `batchit`**. This was done by explicit maintainer direction (2026-07-18,
target `papadopoulos-lab/batchit`), *ahead of* the recorded precondition that a
second consumer (`tte`) first have a real call site: there is no `tte` package and
no second consumer today. The seam that precondition guarded against — runner and
consumer loading from different trees once they are separate packages — was
instead proven with a synthetic throwaway consumer package in `batchit`'s own test
suite, not by a real second consumer. swereg `Imports: batchit` and drives it as a
plugin: the child loads the *named consumer package* at runtime, so there is no
dependency cycle.

**What moved to `batchit`:** the whole engine — the `batch_target` descriptor
(package + symbol + body/formals identity hash), both-ends item validation, the
result envelope (protocol, id, status, value-or-error, target identity, captured
warnings), the two transports (a fresh process per item via processx; a lazy
producer under bounded backpressure via mirai), and the one generic worker
script (now `batchit`'s `inst/batch_worker.R`, the runner-vs-consumer seam).
`R/batch.R`, `R/batch_selftest.R` and `inst/batch_worker.R` are gone from swereg,
along with the migrated contract tests (`test-batch_run`, `test-batch_stream`,
`test-batch_log_tail` — they live in `batchit` now).

**What stays in swereg (policy, not engine):** the dispatch call sites in
`R/r6_tteplan.R` / `R/r6_registrystudy.R` (which targets, progress labels, stable
ids), the thread policy (`.threads_per_worker`), the dev-path selection
(`.swereg_dev_path`), the consumer-code targets themselves
(`.rawbatch_write_worker`, `.process_one_batch_snapshot`, the s1/s2/s3 workers),
and the production-boundary proofs (`test-batch_s3_production`,
`test-batch_rawbatch_production`, `test-batch_skeletons_production`,
`test-s3_item_contract`). `R/batch_adapter.R` holds three `@noRd` wrappers
(`.batch_target` / `.batch_run` / `.batch_stream`) that forward to `batchit` so
every call site and test keeps the internal names and `local_mocked_bindings`
keeps working.

**Dev-source discriminator changed.** `.dev_source_root()` used
`inst/batch_worker.R` as swereg's source-tree marker — a file now shipped by
`batchit`, not swereg. Left alone, `.swereg_dev_path()` would return `NULL` under
`load_all()` and the workers would silently load the stale INSTALLED swereg. The
marker is now swereg's own source: a `DESCRIPTION` naming `swereg`, no
`Meta/package.rds`, and an `R/` directory holding `.R` sources (an installed
package's `R/` holds `swereg.rdb`/`.rdx` bytecode instead).

**Lockdown tightened.** With zero engine code left in swereg, the AST ban on
processx/callr/mirai mentions now covers all of `R/` and `inst/` with no
allowlist; and the lock additionally asserts that `inst/batch_worker.R` and
`R/batch.R` do not exist, that `DESCRIPTION` names no `processx`/`callr` as a
hard dependency (`Imports`/`Depends`) and no `callr` at all (`Suggests`
included), and that `batchit::` is mentioned nowhere in `R/` outside
`R/batch_adapter.R`. `processx` is permitted in `Suggests` because a test
harness — not the package code — spawns a process directly.

**Dependencies.** `batchit` added to `Imports` (+ `Remotes:
papadopoulos-lab/batchit` until it is on CRAN); `processx` moved `Imports` →
`Suggests` (swereg no longer dispatches — the transport is `batchit`'s — but a
`qs2_write_atomic()` test spawns a real process directly to prove atomicity, so
R CMD check `--as-cran` needs it declared as a test harness); `devtools` dropped
from `Suggests` (its only swereg use was the dispatcher's dev-load, now
`batchit`'s worker's). `mirai` stays in `Suggests` (the mirai error-contract and
rawbatch production tests use it directly). No user-facing change: the dispatcher
was internal throughout.

# swereg 26.7.25

## Internal — high-entropy session nonce in the private mirai profile name

Corrects the 26.7.24 "private mirai profile per invocation" claim. The
session-local counter alone produced `.batch_stream_<n>`, which is unique only
among calls through the runner's own closure — but mirai's compute-profile
registry is session-*wide*, and `daemons()` resets an existing profile of the
same name. So if any other caller/package already held `.batch_stream_1`, the
first invocation would reset it and its `on.exit` would then destroy it:
"private" and "collision-free" were not true by construction.

`.batch_stream_profile()` now namespaces the counter under a high-entropy,
session-specific nonce: `.batch_stream_<nonce>_<counter>`. The nonce is derived
once (lazily, then cached) from `basename(tempfile())`, which embeds the pid +
random hex **without touching R's RNG stream** — so library code cannot disturb
a caller's `set.seed()`/reproducibility. A collision now requires another party
to have claimed a name under the runner's reserved `.batch_stream_<nonce>_`
prefix in the same session; the never-equals-`"default"` guarantee (defect #2)
still holds by construction.

# swereg 26.7.24

## Internal — pre-extraction shrink of the batch dispatcher

Phase 4 step 1 (PROJECT.md): shrink the signed-off dispatcher *before* any
`batchit` extraction. (The recorded sequencing put a real production run ahead of
this shrink; that precondition was skipped by explicit maintainer direction — no
full production rerun happened after Phase 3.) Four independent
simplifications, no change to the dispatch contract the callers rely on:

* **Failure retention dropped entirely.** Removed `keep_failed_dir` from
  `.batch_run()`/`.batch_stream()`, the `.batch_retain_failure()` /
  `.batch_id_slug()` functions and their fail-closed chmod machinery, and the
  retention calls in the failure paths. Rationale: no production caller ever
  passed `keep_failed_dir`; replay is by regeneration (stable ids + pure
  producers), not by persisted records; and the fail-closed `0700`/`0600`
  handling of a target's own error text is sensitive-data policy, not generic
  dispatcher material (the joint retrospective recommended adapter-or-drop, and
  it is unused, so drop). The "runner never persists argument VALUES" guarantee
  now holds by construction — the runner persists nothing.

* **Private mirai profile per invocation.** `.batch_stream()` no longer takes a
  caller-selectable `compute` profile and no longer proves ownership via
  `mirai::daemons_set()`. Instead a package-local, session-local counter
  (`.batch_stream_profile()`) generates a fresh `.batch_stream_<n>` name each
  call. That name can never be `"default"`, so the never-touch-the-default-
  profile guarantee (defect #2) holds by construction, deleting the collision
  policy, the fail-closed ownership predicate and their tests.
  `save_rawbatch()` no longer passes `compute = "swereg_rawbatch"` — the runner
  owns profile allocation.

* **`inst/batch_worker.R` slimmed** from 119 to ~72 lines (45 code). The
  envelope is read once (the second read and the id-extraction preamble are
  gone) and the 32-line `tryCatch` fallback error-envelope writer is removed.
  New failure contract: any failure at or before `.batch_execute()` writes
  nothing and exits non-zero — the parent's exit-code channel plus the per-item
  log tail is the diagnostic path. The signed-off security properties are
  unchanged: exact `[[` extraction throughout, and the package-independent
  pre-load structural check of the fields that decide what code loads. Target-
  level failures still return a structured error envelope (exit 0) via the
  unchanged, total `.batch_execute()`.

* **Shared input-envelope constructor + comment prune.** Both frontends now build
  the wire envelope through one `.batch_input_envelope()`, so the schema cannot
  drift between them. Review-archaeology comments (which round found what) in
  `R/batch.R` were cut to the live constraint they document; every constraint
  comment stays.

# swereg 26.7.23

## Bug fixes

* **The dev-path probe no longer mistakes an installed package for a dev source
  tree under R CMD check.** `.swereg_dev_path()` decided "installed vs source"
  with `any(startsWith(system.file(), .libPaths()))`, which is fragile: under
  R CMD check the package is loaded from a check library whose realpath-
  normalized form is not a string-prefix of `system.file()`'s recorded path, so
  the probe took the INSTALLED `.Rcheck/swereg` dir for a dev tree and handed it
  to the batch dispatcher as `dev_path`. The dispatcher then sought
  `inst/batch_worker.R` (absent — install promotes `inst/*` to the package root)
  and asked mirai daemons to `devtools::load_all()` an installed layout (which
  cannot resolve `.batch_execute`) — the four `test-batch_*_production` /
  `test-process_skeletons_loud_errors` failures that appeared only on CI, never
  under `load_all()`. Two guard layers, both structural rather than heuristic:
  - `.swereg_dev_path()` now discriminates via markers R itself writes: an
    installed package carries `Meta/package.rds` and has no `inst/` subdir; a
    source tree is the converse. Split into a unit-testable `.dev_source_root()`.
  - `.batch_validate_dev_path()` (the runner-level guard) now REJECTS an
    installed-package dir loudly (`Meta/package.rds` present), rather than
    limping on to a deeper "worker script not found" — defect #5 (a wrong
    `dev_path` must error, never silently run different code).

# swereg 26.7.22

## Breaking changes

* **`parallel_pool()` is removed** (it was exported). Every parallel pipeline
  stage now dispatches through the internal generic batch runner (the two
  explicit serial branches -- `save_rawbatch` and `process_skeletons` at
  `n_workers = 1` -- call the same targets in-process; every other path
  launches batch-runner subprocesses even with one worker); there is no
  supported external entry to the old pool. `callr` is no longer a
  dependency.

## New features / internal

* **Phase 3 of the dispatcher unification: every parallel work dispatch in the
  package now crosses the ONE batch contract, and the legacy engines are
  deleted.** What "one dispatcher" claimed in 26.7.21 is now true at every
  process boundary the package creates for pipeline work (the two deliberate
  serial branches -- `save_rawbatch` / `process_skeletons` -- run the same
  targets in-process; the only other child process is a `git rev-parse`
  metadata shell-out):

  - `s3_analyze()`'s **ETT loop** joins the enrollment loop on `.batch_run`
    (one method no longer carries two dispatch contracts). Two builder fixes
    the mechanical swap would have missed: items now carry the per-worker
    thread share themselves (they used to say `n_cores` and rely on
    `parallel_pool()` overwriting it — carried verbatim that would have
    oversubscribed every worker), and the optional `subgroup_var` formal is
    explicit on every item (the every-formal rule).
  - **s2** and **s1a–s1d** migrate mechanically; all items get stable,
    meaningful ids (skeleton basename, enrollment id, analysis-file basename,
    `enrollment__skeleton` for the 39k-item s1c stage) so a failure names the
    exact unit of work that died.
  - **`save_rawbatch()`** — the shape-B production proof Phase 2 lacked —
    drops its hand-rolled mirai block for `.batch_stream` on the dedicated
    `swereg_rawbatch` profile, with the new `.rawbatch_write_worker` target as
    the one rawbatch write path in both modes: serial (`n_workers = 1`, the
    default) calls it in-process — no process boundary, no mirai requirement —
    and parallel dispatches it. The daemon-side hand-inlined copy of the
    atomic write (which had already drifted once) is gone; the target uses the
    real `qs2_write_atomic()`.
  - **`process_skeletons()`** drops its `callr::r_bg` engine for `.batch_run`
    with the new `.process_one_batch_snapshot` target: the study is written to
    ONE snapshot file per run and each item carries only its path plus small
    scalars — a naive migration putting the ~5.7 MB study into each of 2,194
    eagerly-materialised item envelopes would have serialized ~12.5 GB before
    the first worker launched. Pinned structurally (one snapshot, items
    < 50 KB, cleaned on unwind) by `test-batch_skeletons_production.R`.
  - **Deleted:** `R/parallel_pool.R`, `inst/worker_bootstrap.R`, all eight
    `inst/worker_*.R` dispatch scripts (plus the dead `.s1a_worker()`),
    `test-worker_arg_parity.R` and `test-parallel_pool_io.R` (guarantees
    ported: chatty-worker no-deadlock and per-item log reclaim re-proven
    against `.batch_run`; `.pp_log_tail()` moved into `R/batch.R` with its
    unit tests). The no-direct-`qs_save` guard now covers ALL of `R/` (the
    two engines whose deliberate raw writes forced its narrow scope are gone).
  - **The lockdown** (`test-batch_lockdown.R`): a parse-based sweep of `R/`
    and `inst/` bans any processx/callr/mirai *mention* outside `R/batch.R`,
    proven non-vacuous against `batch.R` itself and proven red against a
    planted engine call. Old workers cannot reappear; `callr` cannot return
    to `DESCRIPTION` unnoticed.

  Production-boundary proofs run REAL subprocesses end to end:
  `test-batch_s3_production.R` (s1→s2→s3 with both s3 loops asserted),
  `test-batch_rawbatch_production.R` (daemons write + round-trip real slices;
  a blocked rename surfaces naming the batch), and
  `test-batch_skeletons_production.R` (skeletons built from the snapshot).

# swereg 26.7.21

## New features

* **One generic subprocess dispatcher (`R/batch.R`) with a contract validated at
  both ends (Phase 2 of the dispatcher unification, toward `batchit`).** swereg
  dispatched work to subprocesses through three hand-rolled engines
  (`parallel_pool`/processx, `callr`, a `mirai` bounded queue) with no enforced
  parent/child contract; Phase 1 fixed 20 boundary defects but left the engines.
  This release adds the single runner they will collapse into:

  - `.batch_target(package, symbol)` — a target is a **descriptor** (package +
    symbol + a hash of the function's body and formals via `.hash_function()`),
    never a closure. The child re-resolves it and refuses to run if its hash
    differs from what the parent dispatched (closing the stale-dev-code hole).
  - `.batch_run(target, items, n_workers, ...)` — **shape A**: items already
    exist, one fresh subprocess per item (the memory strategy for ~20 GB/worker
    stages). Evolved from the hardened `parallel_pool()`.
  - `.batch_stream(target, ids, producer, n_workers, ...)` — **shape B**: the
    parent is the producer, items are big data slices generated lazily under
    `mirai` backpressure and passed in-memory. Ownership of the compute profile is
    checked with `daemons_set()` (never hijacks a profile the caller configured;
    never the default profile).
  - `inst/batch_worker.R` — the **one** generic worker, replacing the eight
    hand-written `inst/worker_*.R` dispatch scripts and the 100-line regex parser
    that verified them.

  The contract: every formal named explicitly (including optional ones — this is
  what makes the `arm_labels`-dropped class of bug impossible, not merely
  documented); validation at **both** ends, with *total* input and result
  inspectors (a hostile/corrupt envelope becomes a structured failure, never a
  crash) and exact `[[` extraction (no `$` partial-matching steering which code a
  worker loads); a private matched IPC codec; target warnings captured and
  re-surfaced in the parent; **fail-closed** metadata-only failure retention
  (never argument values); a per-item timeout; and a pre-load structural gate in
  the worker so a malformed envelope cannot steer loading.

  `s3_analyze()`'s enrollment loop is migrated onto `.batch_run` as the
  production-boundary proof: `test-batch_s3_production.R` drives the real
  `s3_analyze -> .batch_run -> generic worker -> .s3_enrollment_worker` path end
  to end and asserts both a real baseline result with `arm_labels` forwarded AND
  that a corrupt analysis file makes `s3_analyze()` raise. The remaining call
  sites (`save_rawbatch`, the s1/s2/s3-ETT/skeleton loops), deleting the old
  engines, and extracting `batchit` are Phase 3+.

  `mirai` floor raised to `>= 2.3.0` (for `daemons_set()`). Adversarially reviewed
  by codex (`model_reasoning_effort=high`) over eight rounds; every fix landed with
  a test demonstrated to fail without it, and the production boundary is tested
  through the real worker, not helpers.

## Bug fixes

* **`s3_analyze()` silently discarded `arm_labels`, so every Table 1 it built
  used default arm headers instead of the spec's.** The item builder in
  `s3_analyze()` computed `arm_labels = .lookup_arm_labels(self$spec, eid)` into
  every enrollment item, and `.s3_enrollment_worker()` accepted it
  (`arm_labels = NULL`) -- but `inst/worker_s3_enrollment.R` never forwarded it
  from `params`, so the target always took its default. `s3_analyze()` calls
  `parallel_pool()` unconditionally (there is no `n_workers == 1` serial branch),
  so this affected **every** run at every worker count, while
  `recompute_baselines()` -- which calls the same target directly, with
  `arm_labels` -- produced the correct labels. Two methods that build the same
  table disagreed, and the pipeline ran the wrong one.

  Impact is labelling, not estimates: Table 1's comparator/intervention column
  headers. No numbers move. **`recompute_baselines()` repairs an existing plan**
  without re-running s3.

  Nothing caught this because the guard was pointed at the wrong half.
  `test-worker_arg_parity.R` checked that a worker passes no argument the target
  *rejects* -- which fails loudly, at the first item, with `unused argument`. It
  never checked that a worker forwards every argument the target *accepts* --
  whose failure is silent, because the target simply takes its default. The
  mirror check is now in place and is demonstrated to fail without the fix. A
  formal a worker never forwards is unreachable from production: either it is a
  dropped field or it is dead code, and both now error.

* **A chatty worker could deadlock `parallel_pool()` forever, looking exactly
  like a hung stage.** `stdout` and `stderr` were pipes (`stdout = "|"`) that
  were only read *after* the child exited. A pipe has a fixed OS buffer (64 KB
  on Linux), so a child that out-wrote it blocked in `write()`, never exited,
  and stayed `is_alive() == TRUE` -- the dispatch loop then span on it until
  killed. Reproduced before the fix: 1 KB per stream finished in 0.7s, **100 KB
  never returned**. Workers now write to a per-item log file, and a failure
  reports a genuinely bounded tail of it: at most the last 64 KB are read from
  the end of the file, so a worker that dies after emitting a multi-GB log
  cannot OOM the *parent* while its error is being reported. Each successful
  item's log is reclaimed as soon as it finishes rather than at pool exit --
  s1c dispatches 39,492 items over ~10h, and deferring cleanup would sit on
  ~39k files for the whole stage.

* **`detectCores()` returning `NA` no longer poisons thread counts.** It is
  documented to return `NA` when it cannot determine the core count; unguarded,
  that `NA` propagated into `floor(NA / n_workers)` and surfaced much later
  inside a worker's `setDTthreads()`, a long way from the cause -- or went
  straight into qs2's `nthreads`. All eight call sites (s1, s2, `s3_analyze()`,
  `process_skeletons()`, serial `save_rawbatch()`, `TTEPlan$save()`,
  `Skeleton$save()`, `parallel_pool()`) now route through one guarded
  `.safe_n_cores()` / `.threads_per_worker()`, and a test fails if any future
  code calls `parallel::detectCores()` directly.

* **Worker-count validation now runs at the very top of every entry point,
  before any early return or object mutation.** Fractional counts used to be
  silently truncated (`as.integer(2.5)` = 2) because callers converted before
  checking; and the check, where it existed, ran too late -- `save_rawbatch()`'s
  "group already saved" return, `s1`'s `self$output_dir` overwrite, and
  `process_skeletons()`'s manifest invalidation all preceded it, so an invalid
  count could report success, or leave the object half-modified, or destroy the
  committed skeleton manifest and only then error. All six entries
  (`parallel_pool()`, `s1`, `s2`, `s3_analyze()`, `save_rawbatch()`,
  `process_skeletons()`) now call a shared `.validate_n_workers()` as their first
  statement, and it names its caller in the error. `default_n_workers()` likewise
  validates a configured value rather than repairing it. The validator also
  rejects a whole number above `.Machine$integer.max`: it would otherwise pass
  every other check and become `NA` on `as.integer()`, and that `NA` then flowed
  past validation into callers that clear state before their own `n_workers <= 1`
  test -- so a "rejected" count could still destroy the committed manifest.

* **`resume` could skip a future-dated file forever.** The freshness test was
  `age <= 24h`; a file dated in the future has a *negative* age, so it satisfied
  that on every run, indefinitely. Freshness now requires a finite, non-negative
  age. Two hosts mount the same share here, so clock skew makes this reachable
  rather than theoretical.

* **A `swereg_dev_path` that exists but is the wrong package is now rejected.**
  The path was only checked for existence, then its `inst/` scripts were executed
  and the tree `load_all()`ed into every worker -- so a mistargeted or renamed
  directory loaded the wrong package and mixed it with the installed `swereg`.
  The tree's `DESCRIPTION` must now name `swereg`.

* **`parallel_pool(n_workers = 0)` span at 100% CPU forever instead of
  erroring.** `floor(n_cores / 0)` gave `n_threads = Inf`, and the dispatch loop
  never launched anything (`length(active) < 0` is never true) while its
  `Sys.sleep(0.1)` sat inside `if (length(active) > 0L)` -- an infinite *busy*
  loop, silent and invisible. `n_workers` is now validated as a single finite
  whole number >= 1 before anything is divided by, launched, or written.

* **A `swereg_dev_path` that did not exist silently ran the installed package
  instead.** `is_dev` simply went `FALSE` and execution fell through to
  `system.file()`, so a typo'd dev path ran *different code than you asked for*
  and reported success. Asking for a dev tree that is not there is now an error;
  pass `swereg_dev_path = NULL` to use the installed package deliberately.

* **Ten writes went straight to their final path, bypassing
  `qs2_write_atomic()`.** Panels, counts, `file_raw`, `file_imp`,
  `file_analysis` and `TTEPlan$save()` all used `qs2::qs_save()` directly, while
  resume logic trusts those files' *existence* -- so a killed worker could leave
  a truncated `.qs2` that a later run then skipped as "already done". All ten now
  route through `qs2_write_atomic()`, and a test fails if any future write
  bypasses it again.

  Scope, stated precisely rather than broadly: this means **an interrupted
  process can no longer create a torn final path**. It does *not* make resume
  safe in general. Resume still trusts existence and mtime, which cannot tell
  you that a file came from the current inputs, spec, target body or package
  version, nor that it was not left by a pre-existing run. Resume is only sound
  when inputs and code are unchanged; completion records tied to input/target
  identity are future work.

* **`s2` resume could reuse arbitrarily old analysis files.** The age check took
  `max(mtime)` across all existing outputs and, if that single newest file was
  under 24h, skipped **every** existing file -- so one 1-hour-old output caused a
  100-hour-old output to be reused, while the log claimed "analysis files <24h
  old" about files that were nothing of the sort. Each file is now aged on its
  own, via a new internal `.resume_fresh()`, and the message reports skipped and
  redone counts separately. A file whose mtime cannot be read is no longer
  treated as fresh.

* **`qs2_write_atomic()` was weaker than its own documentation.** Its temp file
  was `paste0(path, ".tmp", Sys.getpid())`, but PIDs are unique only among live
  processes *on a single host* -- and this package's data lives on a CIFS share
  that more than one host mounts at once, so two machines could pick the same
  temp path for the same target. It now uses `tempfile()` in the destination
  directory (same directory is required: `file.rename()` is not atomic across
  filesystems), and cleans up the partial file on an R-level failure -- though
  not after a `SIGKILL`, which `on.exit()` cannot survive: a hard-killed worker
  leaves its temp file behind, and only the *destination* is guaranteed
  absent-or-complete. Its docs now state
  what it does **not** promise: it is not durability (rename is not `fsync`) and
  it is not a lock (concurrent writers each produce a complete file; the last
  rename wins). `save_rawbatch()`'s mirai daemon hand-inlines the same
  temp+rename (the daemon may not have swereg loaded to call the function), and
  was still carrying the old PID-based name; it now inlines the
  `tempfile()`-based form too, and removes its partial temp on **any** R-level
  failure (previously only on a rename failure, so a `qs_save()` that errored
  mid-serialization left a partial -- potentially sensitive -- rawbatch slice in
  the shared directory).

* **`save_rawbatch()` destroyed the caller's mirai daemon configuration.** It
  called `daemons(n)` / `daemons(0)` on mirai's **default** compute profile,
  which resets whatever the caller had already set up -- verified: a caller
  holding 2 daemons was left holding 0. It now claims a named profile
  (`swereg_rawbatch`), per mirai's guidance to package authors, and a caller's
  default daemons survive untouched.

* **`mirai` was undeclared.** `save_rawbatch(n_workers > 1)` calls
  `mirai::daemons()` and friends, but mirai appeared nowhere in `DESCRIPTION` --
  an `R CMD check` warning, and the reason the parallel path died on a machine
  that had never installed it. Now in `Suggests`, which matches the contract:
  worker counts default to serial, parallelism is explicitly opt-in, and absence
  already produces a clear runtime error.

  `drain_one()` now reads `call_mirai(h)$data` and tests it with
  `is_error_value()` -- mirai's documented API -- rather than reading the
  undocumented sibling binding `$value` and hand-checking classes. This is
  hygiene, **not** a bug fix: both bindings exist and are `identical()`, and the
  original guard was verified to fire correctly on failure and stay quiet on
  success. New `test-mirai_error_contract.R` pins that behaviour, because it is
  the failure path and no happy-path production run ever exercises it.

# swereg 26.7.19

## Bug fixes

* **s1 resume caches are now keyed on the inputs s1's output depends on, not the spec alone.**
  `{data_meta_dir}/s1_work/{project_prefix}/{cache_key}/` previously used a hash of the parsed
  spec and nothing else. Since s1's output depends on the spec *and* the skeletons it reads,
  regenerating skeletons under an unchanged spec produced an identical key, and `resume = TRUE`
  reused s1a/s1b/s1d sentinels computed from the **previous** skeletons -- silently, with nothing
  downstream able to notice. Production-reachable: projects that pass `resume = TRUE` would hit
  this after any reclean.

  The key now covers: the spec; the committed skeleton manifest identity; the **ordered batch IDs
  this run actually selects** (`n_skeleton_files` caps what s1 reads while leaving the directory
  unchanged, so a capped run and a full run previously shared a key -- the capped run's
  enrollment-level sentinels could be reused by the full run, making matched comparators
  silently represent only the subset); `impute_fn`; `stabilize`; `output_dir` (s1d skips on its
  sentinel, so resuming with a different destination would skip s1d, delete the work dir, and
  report success having written nothing there); and the swereg version. The digest is now the
  full 64 bits rather than a 48-bit prefix.

  `.spec_cache_key()` is replaced by `.s1_cache_key()`, and `.s1_work_dir()`'s `spec_hash`
  argument is renamed `cache_key` -- it is no longer a hash of the spec. Both internal.

  Two known limits, stated rather than papered over. The swereg version is a weak proxy for s1's
  implementation, because `swereg_dev_path` loads a dev checkout whose code need not match any
  released version -- bump the version for any change to s1 semantics. And `impute_fn` is hashed by
  body and formals only, so two closures with identical bodies but different captured values key
  identically; that covers the package's own defaults but not a custom callback carrying state.

* **`resume = FALSE` now clears its own keyed work directory before starting.** It previously
  overwrote artefacts in place, so a process killed mid-overwrite could leave an old sentinel
  beside a partially-rewritten cache -- which a later `resume = TRUE` would read as completed
  work. Only this key's directory is touched, and a failure to remove it now raises rather than
  proceeding. Note a cache key identifies a *configuration*, not a run: a concurrent s1 with the
  same key shares the directory, which the single-writer invariant below covers and this code does
  not.

## New behaviour

* **`$process_skeletons()` now commits a skeleton manifest, and `$s1_generate_enrollments_and_ipw()`
  requires one to resume.** The manifest is a new `skeleton_manifest` field on `RegistryStudy`,
  written into the existing `registrystudy.qs2` by the existing `$save_meta()` (which is already
  atomic via `qs2_write_atomic()`) -- no new file, no new machinery.

  It is cleared *before* any batch is touched and re-committed only if the finished dataset
  validates, so an interrupted or failed run leaves no manifest rather than a stale one vouching
  for skeletons it no longer describes. Committing requires all four of: every batch's provenance readable
  (its meta sidecar, or the skeleton itself where none exists -- note this does NOT open every
  skeleton, which would mean reading GBs per run, so a skeleton replaced while its old sidecar
  survives is not detected; both writes are atomic but they are separate writes);
  exactly one distinct `pipeline_hash`; that hash equal to the study's **current**
  `$pipeline_hash()` (internal agreement is not currency -- a uniformly obsolete dataset would
  otherwise pass); and batch IDs exactly `seq_len(expected)` (a count cannot tell 1..N from
  2..N+1, and a *first* build interrupted at batch 272 leaves 272 mutually-consistent skeletons
  that a hash-only check waves through, after which s1 would analyse 12% of the cohort and look
  fine doing it).

  `batches` controls only what this run *processes*, never what is validated -- validation always
  looks at the whole directory, because that is what s1 will read. So a **subset** run
  (`batches = 1:10`) still commits if the resulting whole dataset validates, and simply leaves no
  manifest if it does not. The difference is that a **full** run (`batches = NULL`) which fails to
  validate *raises*: otherwise a caller's file-count gate reports success over a dataset nothing
  will accept.

  The manifest records `manifest_version`, `committed_at`, `swereg_version`, the exact ordered
  `batches`, the `pipeline_hash`, and an `identity` digest over the ordered per-batch
  `(batch, pipeline_hash, built_at)` triples. `identity` is what makes this a *data* identity
  rather than a *code* one: `pipeline_hash` is derived from function hashes, so rebuilding from
  changed raw data with unchanged code leaves it identical, whereas `built_at` moves on every save.

  s1 re-reads `registrystudy.qs2` from disk to get it -- `plan$registrystudy` is a copy frozen
  when s0 saved the plan, and would describe whatever the skeletons were then. One small read,
  replacing a per-batch sidecar scan (~40 s for 2,200 batches over SMB), so s1 startup is faster
  as well as safer.

  **Migration:** existing skeleton datasets have no manifest, so `resume = TRUE` will refuse until
  `$process_skeletons()` has completed once under this version. `resume = FALSE` is unaffected --
  it reads no cache, so there is nothing to match. Skeletons that are bare `data.table`s rather
  than `Skeleton` objects (a supported s1 input) carry no provenance and can never have a
  manifest; they too are limited to `resume = FALSE`.

  **SINGLE-WRITER INVARIANT (not enforced).** Exactly one `$process_skeletons()` may run against a
  skeleton directory at a time, and no s1 may read it while one does. Two concurrent writers can
  interleave as clear(A), clear(B), commit(A), B-replaces-skeletons-and-dies, leaving A's manifest
  vouching for B's skeletons -- a logical race that atomic writes cannot fix. Likewise s1 can read
  a valid manifest immediately before a regeneration starts. Serialise the stages.

* **New `tteplan_s1_cache_delete(plan, dry_run = TRUE)`** deletes a project's s1 resume caches.
  s1 removes its own work directory on success, so leftovers are from killed or superseded runs;
  now that the key covers skeleton state, every regeneration orphans one. Deliberately explicit
  rather than automatic at s1 startup: only the run that created a work directory knows whether it
  is finished with it, deleting another run's work would break a concurrent s1, and a killed run's
  cache is the only evidence of what it did. Deleting a cache is never a correctness risk -- it is
  an accelerator, not an artefact, so the worst case is that s1 redoes work.

# swereg 26.7.18

## Breaking changes

* **Removed the pipeline-snapshot feature.** `RegistryStudy$write_pipeline_snapshot()`,
  the `data_pipeline_snapshot_dir` constructor argument, the
  `data_pipeline_snapshot_cp` / `data_pipeline_snapshot_dir` fields, and the
  now-orphaned `host_label` field are all gone. It wrote a one-row per-host TSV of
  `pipeline_hash` / `framework_fn_hash` / `all_batches_consistent` into the caller's
  git repo — write-only provenance that nothing read back, and whose "is this host
  consistent?" claim went stale silently the moment a caller stopped passing
  `data_pipeline_snapshot_dir` (the default), because the writer no-ops on NULL.
  `$compute_summary()` / `data_summaries_dir` already answer the same question more
  usefully, by diffing actual per-variable output rather than a hash of the code.

  Migration: drop `data_pipeline_snapshot_dir = ...` from any `RegistryStudy$new()`
  call; it is now an unused argument. Nothing else changes.

  **The schema version stays at 5** — deliberately. The removed fields were optional,
  NULL by default, and carried no data anyone reads back, so no `registrystudy.qs2` on
  disk becomes invalid. Bumping would have forced every host to regenerate its meta
  file (and, per the downstream convention that a missing `registrystudy.qs2` re-stages
  rawbatch, potentially a great deal more) for a purely cosmetic removal.

* **`DiagrammeR`, `DiagrammeRsvg` and `rsvg` moved from `Imports` to `Suggests`.**
  `R/consort.R` is their only consumer and it already guarded them with
  `requireNamespace()` and degraded to a warning when absent — so they were declared
  mandatory while the code treated them as optional. The declaration was wrong.

  This matters on Linux: `DiagrammeRsvg` needs `V8`, which needs a system
  `libnode-dev`, so on a box without it *every* swereg entry point that loads the
  package — `devtools::load_all()`, `roxygen2`, `R CMD check`, the test suite — failed,
  purely to render a CONSORT diagram. Windows/macOS are unaffected either way (CRAN
  ships self-contained binaries).

  Migration: if you render CONSORT diagrams on Linux, install the optional stack
  explicitly — `pak::pak(c("DiagrammeR", "DiagrammeRsvg", "rsvg"))` plus
  `apt install libnode-dev`. Without it, `$s4_export()` still produces every other
  output and emits one warning naming exactly what to install.

## Bug fixes

* `qs2_write_atomic()` is now actually exported. It was added in 26.7.17 with an
  `@export` tag, but NAMESPACE was never regenerated, so `swereg::qs2_write_atomic()`
  errored with "not an exported object". Internal callers use it unqualified and were
  unaffected — atomic writes themselves worked as documented.

## Changes

* **Atomic qs2 writes.** `RegistryStudy` and `Skeleton` now serialize skeletons,
  rawbatch slices, the `registrystudy.qs2` meta, summaries and population tables
  to a temp file and `file.rename()` into place (new `qs2_write_atomic()`; the
  rawbatch mirai worker inlines the same temp+rename). An interrupted worker
  (SIGKILL, crash, dropped CIFS mount) can no longer leave a truncated `.qs2` at
  the final path, so a `process_skeletons()` resume never halts on "unexpected
  end of file". file.rename() verified atomic on SMB/CIFS.

# swereg 26.7.16

## Changes

* **ITT-vs-PP overlay: ITT is the upper point in each dodged pair** (was
  per-protocol). ITT now reads "first" both ways — left-hand text column and
  top marker on each row. Colours unchanged (ITT blue, PP red).

# swereg 26.7.15

## Changes

* **The "PP vs ITT forest" exhibit is now "ITT vs PP forest" (ITT first).** The
  head-to-head sheet, its numeric table, and the two-colour overlay now put
  intention-to-treat **first / on the left**; per-protocol moves to the right.
  Colours are unchanged — **PP stays red, ITT stays blue** — so after the swap
  the left column is blue (ITT) and the right is red (PP). The sheet name,
  title, TOC entry, and sidecar filename (`…_forest_plot_itt_vs_pp`) follow, as
  do the internal writers/renderer (`.build_itt_vs_pp_df`,
  `.write_itt_vs_pp_forest`, `.render_itt_vs_pp_overlay`). Callers that only use
  `$export_tables()` need no change; the old `…_pp_vs_itt` sidecar filename is
  retired.

# swereg 26.7.14

## Improvements

* **Role sub-headers now reach every forest renderer.** The `role_headers`
  grouping added in 26.7.13 (for `$export()` forest figures) is now also
  honoured by `$export_tables()` via a new `forest_role_headers` argument. The
  "PP forest plot" / "ITT forest plot" sheets *and* the two-series "PP vs ITT
  forest" overlay all thread the same exposure → role → outcome hierarchy, so
  every forest in the workbook matches the standalone publication figures.
  Backward compatible: `forest_role_headers` defaults to NULL.

# swereg 26.7.13

## Improvements

* **Forest plots can group outcome rows by role.** `$export()` forest specs
  gain a `role_headers` field — a named map from an `outcome_role` value to a
  sub-header label (e.g.
  `role_headers = list(primary = "Primary outcome", secondary = "Secondary outcomes")`).
  When supplied (and the forest groups by exposure, i.e. outcomes are the rows),
  a bold-italic sub-header is threaded into each exposure block whenever the
  role changes, and the outcome rows indent beneath it, giving an
  exposure → role → outcome hierarchy. Pair it with an explicit
  `label_format = "{outcome_name}"` so the role names the group instead of
  riding along in each row label as `(primary)`/`(secondary)`. Backward
  compatible: `role_headers` defaults to NULL, leaving the two-tier
  exposure/outcome layout (and every existing forest) byte-identical.

# swereg 26.7.11

## Improvements

* **s1 progress bars now self-identify the sub-stage.** `parallel_pool()` gains a
  `label` argument that is prefixed to the per-item progressor message, and
  `TTEPlan$s1_generate_enrollments_and_ipw()` passes `"s1a"`/`"s1b"`/`"s1c"`/`"s1d"`
  to its four calls. The live bar's `(last: ...)` slot now reads e.g.
  `(last: s1c 09:33:18)`, so a `tail` of a job log mid-run tells you which of the
  four s1 sub-stages is active (s1a scout, s1b match, s1c panels, s1d IPW/save)
  instead of a bare `N/M` bar. Backward compatible: `label` defaults to `NULL`
  (timestamp only), so other callers (s2/s3/skeleton) are unchanged.

# swereg 26.7.9

## Features

* **`TTEPlan$export(manifest)`** — one declarative, manifest-driven entry point
  for study exhibits (figures and tables). Pass an ordered list of specs; each is
  produced in order and written with a two-digit prefix, so the manifest order
  becomes the exhibit numbering. Each spec's `type` routes it to a private
  producer:
  - **figures** (`.export_figure`): `"survival"` (weighted survival curve for one
    ETT cell, one image per estimand — PP from `file_analysis` +
    `analysis_weight_pp_trunc`, ITT from `file_analysis_itt` + `ipw_trunc`;
    loaded analysis objects are re-wrapped under the current class so they pick
    up `survival_curve()`) and `"forest"` (forest plot over a named `exposures`
    set).
  - **tables** (`.export_table`): `"table1"` (baseline characteristics for an
    enrollment, written as CSV).

  Projects declare their exhibit set once; the same driver serves every project
  with a different manifest. The existing `$export_tables()` full-bundle writer
  is unchanged.

  Forest exhibits also support: `spec$group_by` = `"exposure"` (default; exposure
  is the group header, outcomes are the rows) or `"outcome"` (outcome is the group
  header, exposures are the rows); and an outcome **role** shown from metadata. Add
  `role: primary` / `role: secondary` to a spec outcome and it surfaces via the
  new `{outcome_role}` label field (and by default on the row label when grouping
  by exposure) -- the outcome `name` stays clean. Forest inputs are validated
  (fully-named `exposures`, all ETT ids known).

* **`TTEEnrollment$survival_curve(weight_col, save_path, title)`** — weighted
  discrete-time survival curve computed on the person-week panel. Per arm and
  period it forms the weighted hazard `h(t) = sum(w * event) / sum(w)` from the
  (optionally time-varying) `weight_col` and returns `S(t) = prod(1 - h(t))`.
  Because it works on the full panel rather than one row per subject, it accepts
  **time-varying** weights: pass a baseline IPW column for the ITT/IPW curve, or
  a per-protocol weight (e.g. `"analysis_weight_pp_trunc"`) for the **PP** curve
  — which the old `$km()` could not do. It is a descriptive weighted curve (not
  the MSM-standardised survival estimator). Deaths are censored (no competing-risk
  adjustment), so `surv` is cause-specific event-free survival under independent
  censoring and `1 - surv` is not a real-world cumulative incidence; label it
  accordingly. Inputs are validated (weight must be finite/non-negative; `event`
  must be a non-missing 0/1 indicator).

## Breaking changes

* **`$km()` has been removed and replaced by `$survival_curve()`.** The old
  method used `survey::svykm` on one row per subject (baseline IPW only); the
  panel estimator generalises it and additionally supports per-protocol
  (time-varying) weights. Call `$survival_curve(weight_col = ...)` instead; the
  return type is a `data.table` of `(treatment, tstop, hazard, surv)` rather than
  an `svykm` object.

# swereg 26.7.4

## Documentation

* **`vignette("tte-methods")` restructured so the SAP reads like a SAP.**
  Section 2 (statistical analysis plan) is now implementation-agnostic:
  function names, argument names, option names, and test-file paths have been
  moved out of the SAP prose into a new Section 4 ("Implementation mapping"),
  which tabulates SAP step -> code (function/argument/option), records
  estimator-behaviour provenance (the 26.7.3 event-priority change, the
  `admin_censor_var` error), and maps each validation layer to its test file
  and opt-in environment variable.
* **Validation evidence is now hard numbers, not prose claims.** Section 3
  renders 17 tables and 6 figures from a committed results artifact
  (`vignettes/tte-validation-evidence.rds`) containing truth, estimate, 95%
  CI, log-scale bias, per-replicate draws, and coverage counts for every
  validation cell: the cross-package triangle (truth vs swereg vs
  TrialEmulation, single fixed-seed datasets plus a 20-replicate version
  whose mean bias is plotted with Monte Carlo error bars, for both
  truncated and untruncated swereg weights — the contrast attributes the
  s3 per-protocol residual to weight truncation under informative loss),
  the stress matrix (rare outcomes,
  null effect, informative attrition, depletion of susceptibles,
  truncation-attenuation dose response, treatment-confounder feedback,
  determinism), the plan-layer factorial plus an 8-seed Monte Carlo
  (per-replicate and summarised), the M=200 ITT coverage study (with a
  caterpillar figure of all 600 replicate CIs), and a boundary-of-validity
  section (3.8): a truncation-tradeoff grid varying loss informativeness,
  selection direction, and effect direction, two cells in which selection
  is driven by an unmeasured prognostic factor (dropout on U and a
  healthy-adherer adherence mechanism), a feedback cell where all
  weighting/conditioning approaches fail, and bias/spread/RMSE figures
  grounding the recommendation of truncated-primary plus untruncated
  sensitivity. The section also documents
  the validation design itself: how counterfactual truth is simulated, the
  data-generating processes as equations, per-scenario nuisance parameters,
  and realized descriptives of every analysed dataset. Numbers quoted in
  the prose are computed inline from the same artifact, so they cannot
  drift from the tables.
* **New `dev/generate_validation_evidence.R`** regenerates the artifact by
  rerunning every validation cell through the same DGP/truth/fit helpers the
  testthat suite sources (`tests/testthat/helper-tte_*.R`); rerun it after
  any estimator change and commit the refreshed artifact.
* `scen_fit_te()` (test helper) now also returns the converted CI bounds
  (`lo`, `hi`), used by the artifact generator; existing tests are unaffected.

# swereg 26.7.3

## Bug Fixes

* **Per-protocol events colliding with protocol deviation are no longer
  dropped.** When a person-trial's first outcome event fell in the same
  enrollment band as its first protocol deviation, `s5_prepare_outcome()`
  flagged the row as both `event = 1` and `censor_this_period = 1`, and the
  post-IPCW censoring-row drop in `s4_prepare_for_analysis()` silently deleted
  the event. The discrete-time convention is event-priority: the outcome is
  measured over the interval before within-interval censoring applies, so the
  band now counts as an event (and the censoring model no longer sees it as a
  censoring). In a switching-heavy simulation (`persist_coef = 2`, 76% of
  trials switching) the old behaviour lost 9.6% of legitimate per-protocol
  events; after the fix the pipeline's event count matches the ground-truth
  event-priority count exactly. Low-switching analyses shift much less. ITT is
  unaffected (it never censors at deviation, and end-of-data loss cannot
  collide with an event by construction). Regression test:
  `test-pp_event_deviation_collision.R`.
* **`admin_censor_var` now fails loudly instead of being silently ignored.**
  `TTEDesign` accepted and stored `admin_censor_var`, but no code ever used
  it -- callers requesting per-person administrative censoring got none.
  `s5_prepare_outcome()` now stops with a clear message directing to
  `admin_censor_isoyearweek`.
* **Robust treatment-coefficient lookup in `$irr()`.** The Poisson MSM
  extracted the treatment coefficient as `<treatment_var>TRUE`, which fails
  cryptically ("subscript out of bounds") when the treatment column is numeric
  0/1 rather than logical/factor. The lookup now falls back to the bare
  variable name and otherwise stops with the available coefficient names.

## New Features

* **Prevalent-user guard in `tteplan_read_spec()`.** The enrollment engine has
  no built-in new-user rule: without a washout exclusion on the treatment
  variable, prevalent users enrol as "intervention" at every eligible band and
  discontinuers re-enter as comparators -- a prevalent-user design, almost
  never the intended estimand (and the exact failure mode documented in the
  downstream project's v006 spec changelog). Reading a spec where an enrollment has
  neither an exclusion of `type: "no_prior_intervention"` nor any exclusion
  referencing its treatment variable now warns. Both washout styles satisfy
  the check: a finite look-back window in weeks (the Danaei 2013 convention,
  which allows re-qualification after time off treatment) or a lifetime
  look-back (never-user design). Suppress with
  `options(swereg.warn_prevalent_user = FALSE)`. Tests:
  `test-spec_newuser_warning.R`.
* **Known-truth stress-test infrastructure.** The synthetic-truth validation
  triangle (planted truth vs swereg vs TrialEmulation) is extended from the
  original three-scenario matrix to a permanent adversarial battery:
  - `test-tte_stress_matrix.R` (enrollment layer): null effect, harmful
    effects under depletion of susceptibles, rare outcomes (~0.25%/band),
    near-positivity violation with a truncation-severity sweep, heavy
    informative attrition (~73% of person-periods lost), time-varying
    confounding with treatment-confounder feedback (time-updated vs frozen
    censoring covariates), and bit-identical determinism. Heavy cells are
    opt-in via `SWEREG_RUN_STRESS=true`.
  - `test-tteplan_truth_matrix.R` (plan layer): the full production path --
    YAML spec, skeleton files (including an ISO week-53 year), sequential
    eligibility, per-band matched enrollment, IPW, PP/ITT analysis files,
    weighted `svyglm` -- recovers a planted IRR of 2.0, with factorial
    coverage of baseline confounding and independent/informative loss to
    follow-up plus a discontinuation (PP vs ITT separation) scenario. Heavy
    cells opt-in via `SWEREG_RUN_PLAN_MATRIX=true`.

## Documentation

* **`vignette("tte-methods")` rewritten as manuscript + statistical analysis
  plan text, matched to the verified implementation.** The manuscript section
  now covers both estimands (ITT and per-protocol). The former supplemental
  section is upgraded to a full SAP: it documents the estimators exactly as
  implemented -- including the marginal (not covariate-conditional) IPCW
  stabilization numerator, the inclusive cumulative-product IPCW convention
  (with rationale: censoring-band rows are removed, so a row at band k exists
  iff uncensored through k), the actual outcome-model formula with its
  follow-up-time and trial-index splines, the event-priority convention,
  hot-deck single imputation, weight-truncation defaults and the
  positivity/truncation bias tradeoff (with simulated attenuation magnitudes),
  identifying assumptions per estimand, interpretation of the IRR as a
  person-time-weighted average under non-proportional hazards, and known
  limitations (no grace periods/cloning, no as-treated, variance conditional
  on estimated weights). A validation-evidence section summarises the
  cross-package matrix, the stress matrix, the full-pipeline truth recovery,
  and the Monte-Carlo coverage calibration.
* **`vignette("tte-methodology")` corrections.** The paper-mapping table no
  longer describes `period_width` as a grace period (a true Hernán-2016 grace
  period requires cloning, which is not implemented; the band width only
  gives within-band slack at enrollment). The follow-up table documents the
  event-priority collision rule.

## New Features

* **Publication workbook: real numbers + per-estimand restructure.**
  `TTEPlan$export_tables()` reorganises the results sheets around estimand and
  writes every measurement as a **real number formatted in Excel** -- so cells
  sort, sum, and no longer trip the "number stored as text" warning:
  - The headline results sheets are now **PP forest plot**, **ITT forest
    plot**, **PP results**, **ITT results**, and **PP vs ITT forest** -- the
    last a numeric `PP IRR` / `ITT IRR` head-to-head table above a two-colour
    overlay forest (red per-protocol, blue intention-to-treat, dodged per
    outcome). The truncated-vs-untruncated weight comparison (formerly "Full
    results") moves to a supplementary **Weight truncation (PP)** sheet.
  - events (`0.0`), PY (`#,##0`), rate (`0.0`) and p-value
    (`[<0.001]"<0.001";0.000`, so tiny p reads `<0.001` not a misleading
    `0.000`) are bare numerics carrying Excel number formats. IRR + 95% CI
    stay human-formatted display strings -- they are inherently composite,
    like Table 1's "n (%)" cells, which also remain display strings. CONSORT
    attrition counts gain a thousands separator.
  - Forest-plot **figures are unchanged**: their text panels are rendered from
    the same numerics through the plot's own formatters (`.ff_num`,
    `.ff_irr_ci`), fully decoupled from the worksheet cells.
  - Forest-plot sidecar files follow one consistent scheme:
    `{root}_forest_plot_{pp,itt,pp_vs_itt}.{png,pdf}`.

  New internal writers `.write_results_single()`, `.write_pp_vs_itt_forest()`
  and renderer `.render_pp_vs_itt_overlay()` back the new sheets. This is an
  `s4` re-export change only -- no pipeline (`s1`/`s2`/`s3`) rerun needed.

# swereg 26.6.20

## Bug Fixes

* **Locale-proof spec reading.** `tteplan_read_spec()` previously called
  `yaml::read_yaml()`, which reads via `readLines()` and, under a non-UTF-8
  session locale (e.g. `LC_CTYPE=C`, common in headless/cron/CI runs),
  **silently truncated** the YAML at the first non-ASCII byte — dropping
  trailing sections with only a `readLines()` "invalid input" warning. A spec
  whose sole non-ASCII characters were em-dashes in a comment lost enrollments
  16–18, yielding a 15/18-enrollment grid with no error. The reader now loads
  the file as raw bytes (`readBin`, binary and locale-independent), strips an
  optional UTF-8 BOM, asserts the content is valid UTF-8 (`validUTF8()` — a
  clear "re-save as UTF-8" error otherwise), and parses that. Bypassing
  `readLines()` makes the read behave identically under any locale.

# swereg 26.6.16

## New Features

* **Intention-to-treat estimand (enrollment level).**
  `TTEEnrollment$s4_prepare_for_analysis()` gains an `estimand` argument
  (`"pp"`, default, or `"itt"`). With `estimand = "itt"`, follow-up is no
  longer censored at treatment switching and IPCW is skipped — baseline IPW is
  the valid weight, so analyse with `$irr(weight_col = "ipw_trunc")`. The
  `$irr()` guard against IPW-only weights is relaxed for ITT-tagged datasets.
  Per-protocol behaviour is unchanged. The production pipeline
  (`s2_generate_analysis_files_and_ipcw_pp()`) now builds both a per-protocol
  and an intention-to-treat analysis file per ETT off the shared imputed file,
  and `s3_analyze()` computes an intention-to-treat IRR (`irr_itt`) and rates
  (`rates_itt`) per ETT alongside the per-protocol results. The exported
  workbook reports per-protocol and intention-to-treat **side by side** (a new
  "PP vs ITT" sheet) and adds a **separate ITT forest plot**. The TTE
  methodology and nomenclature vignettes now document both estimands (including
  a five-reasons censoring table) instead of stating ITT is unsupported.

* **Effect modification (issue #6) — analysis methods.**
  `TTEEnrollment$irr_by_subgroup(weight_col, subgroup_var)` returns stratified
  IRRs (an `"all"` row plus one row per subgroup level) with the
  effect-modification p-value and (binary) ratio of stratum IRRs attached as
  attributes. `$effect_modification_test(weight_col, subgroup_var)` fits a
  single `treatment * factor(subgroup_var)` model and runs the interaction Wald
  test — the correct test for whether stratum IRRs differ — returning
  `ratio_of_irrs = exp(beta)` for a binary subgroup. The `irr()` estimation core
  was factored into a shared internal helper; `irr()` output is unchanged.
  Strata with no events or one treatment arm degrade to NA with a warning. A
  top-level YAML `subgroups:` spec block is now parsed (`tteplan_read_spec`),
  validated (`tteplan_validate_spec` requires each subgroup variable to exist in
  the skeleton **and** be a confounder), and threaded into
  `TTEDesign$subgroup_vars`. `s3_analyze()` now runs the stratified IRRs and the
  interaction test automatically per ETT for each subgroup variable and BOTH
  estimands (stored as `subgroup_<var>_pp` / `subgroup_<var>_itt` and
  `emtest_<var>_pp` / `emtest_<var>_itt`), and the exported workbook gains an
  "Effect modification" sheet showing the stratified IRRs (PP and ITT side by
  side) and the interaction test per ETT × subgroup. `vignette("tte-workflow")`
  gains an effect-modification example. This completes issue #6.

## Bug Fixes

* ITT no longer requires a treatment-switch variable: `s5_prepare_outcome()`
  previously stopped when `time_treatment_var` was `NULL` before the ITT branch
  could bypass protocol-deviation handling, breaking ITT for studies with no
  observed switching variable.
* `tteenrollment_rbind()` now preserves the `estimand` tag (and errors on
  mixing estimands), so a combined ITT object keeps its tag and `$irr()` does
  not wrongly reject its valid IPW-only weight.
* Fixed mis-positioned roxygen so `tteplan_apply_exclusions()` and
  `tteplan_apply_derived_confounders()` are exported and documented again (their
  documentation had drifted onto the internal `.tte_build_*` helpers, which are
  now correctly internal). Restores the pkgdown reference index.

## Validation

* Added a simulation harness with a known true ITT effect (deliberately
  attenuated vs the true PP effect) that confirms `$irr()` recovers the
  marginal ITT truth. The simulated truths are computed as **first-event
  incidence rates** (censoring each person at their first event, with a
  person-time-at-risk denominator) to match swereg's estimand exactly, rather
  than as recurrent-event rates over a fixed denominator. Cross-checks against
  `TrialEmulation` convert its odds ratio toward the rate-ratio scale (Zhang &
  Yu 1998) and compare point estimates and CI widths; the conversion removes
  the scale difference only (not conditional-vs-marginal non-collapsibility),
  so the cross-package check is a bounded consistency check and the primary
  anchor is agreement with the known truth. `TrialEmulation` is itself
  validated against the known truth, not trusted as an oracle. Regression tests
  cover ITT without a switch variable, ITT retaining post-switch follow-up,
  ITT under independent loss to follow-up, and multi-seed CI calibration.
* Added a swereg-vs-`TrialEmulation` **validation matrix**
  (`test-tte_validation_matrix.R`): three escalating scenarios (no confounding /
  no loss; confounding + independent loss; confounding + informative loss), each
  checked across the full triangle (known truth, swereg, TrialEmulation) for
  **both** estimands, comparing point estimate **and** CI width. It shows
  swereg and TrialEmulation are identical with no confounder, both recover truth
  under confounding + independent loss, and -- the key case -- under *informative*
  loss the ITT estimand is biased in *both* packages (they agree with each other
  but miss the truth), while per-protocol IPCW stays close. `TrialEmulation` is
  in `Suggests` and installed in the check workflow, so this runs in CI.
  Replaces the narrower ITT-only cross-package test.

## Documentation

* `vignette("tte-methods")` gains a "Marginal versus conditional estimands"
  section explaining why swereg's IPW-based marginal IRR and `TrialEmulation`'s
  covariate-adjusted conditional OR differ (OR non-collapsibility) and how the
  validation reconciles them.
* `vignette("tte-methods")`'s validation section now documents the three-scenario
  swereg-vs-`TrialEmulation` matrix and the Monte Carlo coverage results, and
  points to the executable tests (`test-tte_validation_matrix.R`,
  `test-tte_coverage.R`) as the enforced contract -- replacing the prior generic
  paragraph.

# swereg 26.6.8

## Breaking changes

* **Cancer / ICD-O matchers consolidated.** `add_icdo3s()`, `add_snomed3s()`
  and `add_snomedo10s()` are **removed**. Cancer ascertainment now goes through
  the new `add_cancer_without_morphology()`.
  - `add_cancer_without_morphology()` matches cancer by **topography** (tumour
    site, C-codes), searching BOTH `icdo10` (ICD-O/2 topography, complete back
    to register start) and `icdo3` (ICD-O/3 topography, ~2000 onward). It
    supersedes `add_icdo3s()`, which searched only the partial `icdo3` column.
  - Rationale: ICD-O *topography* codes ARE the ICD-10 neoplasm site codes
    (e.g. `C50` = breast). `icdo10` is Socialstyrelsen's confusingly-named
    column for ICD-O/2 topography -- **not** "ICD-O edition 10". A prior removal
    of `icdo10` from the diagnosis matcher silently dropped cancer-register
    ascertainment in callers that relied on it; this restores it under a clear
    name.
  - Morphology/histology (`snomed3`/`snomedo10`) matching is dropped for now;
    it will return via a future `add_cancer_with_morphology()` when needed.
  - **Migration:** replace
    `add_icdo3s(skeleton, data, id, icdo3s = <codes>)` with
    `add_cancer_without_morphology(skeleton, data, id, codes = <codes>)`.

## Internal

* Private dispatcher renamed
  `add_diagnoses_or_operations_or_cods_or_icdo3_or_snomed` ->
  `add_diagnoses_or_operations_or_cods_or_cancer`. `add_diagnoses`,
  `add_operations` and `add_cods` are otherwise behaviorally unchanged.

# swereg 26.6.3

## Per-stage worker counts; box-wide `SWEREG_N_WORKERS` retired

* `default_n_workers()` now takes a `stage` tag and **defaults to 1 worker
  everywhere**. Parallelism is opt-in, per stage, via
  `SWEREG_N_WORKERS_<STAGE>` (e.g. `SWEREG_N_WORKERS_S1=3`) or the equivalent
  `options(swereg.n_workers.<stage> = )`. Each pipeline step passes its own
  tag: `save_rawbatch()` -> `"rawbatch"`, `process_skeletons()` ->
  `"skeleton"`, `s1_generate_enrollments_and_ipw()` -> `"s1"`, `s3_analyze()`
  -> `"s3"`. `s2_generate_analysis_files_and_ipcw_pp()` stays hardcoded `1L`.
* The former box-wide `SWEREG_N_WORKERS` env var (and
  `getOption("swereg.n_workers")`) are **retired** — a single global knob could
  silently leak a high worker count into a memory-heavy stage. Concretely, a
  host-wide `SWEREG_N_WORKERS=3` forced 3 x ~20 GB s3 IRR workers and OOM-killed
  the analysis loop on the biggest "vs none" panels (17M rows). The default is
  now safe (1) and heavy stages can never inherit a setting meant for a light
  one. A one-time warning fires if the deprecated `SWEREG_N_WORKERS` is still
  set.

# swereg 26.5.31

## qs2 single-threaded in `parallel_pool()` workers (fix TBB segfault)

* `parallel_pool()` workers (stages s1/s2/s3) now call qs2 with `nthreads = 1L`
  instead of `floor(detectCores() / n_workers)`. Multithreaded qs2 uses
  RcppParallel/TBB, whose scheduler segfaults nondeterministically while
  building its worker-thread pool (`generic_scheduler::allocate_task`, fault
  address `0xff…f7`) on some hosts — the cause of the s1 worker crashes that
  drifted across batches (1160/1398/1652/1792). Process-level parallelism via
  `parallel_pool` already provides throughput, so per-worker qs2 threading was
  redundant. qs2 stays multithreaded in the main process and the
  lower-exposure regen path (`skeleton$save`, `load_rawbatch`,
  `save_rawbatch` serial); data.table (OpenMP) is unaffected. See GitHub #5
  and `dev/debug_worker_gdb/` for the gdb-based diagnosis.

# swereg 26.5.29

## Unified cohort-derivation flow (single source of truth)

* New internal `.build_cohort_flow()` assembles one ordered participant-flow
  table per enrollment (eligibility exclusions + matching + per-protocol
  analysis dataset), each step tagged with a `kind`
  (start/exclusion/selection/analysis) and a correctly-labelled per-step
  `change_kind` (excluded / not selected (matching) / censored
  (per-protocol)). The CONSORT diagram (`.build_consort_dot`) and the
  attrition worksheet (`.write_attrition_sheet`) now both render from this
  one table, so the diagram, the sheet, and the baseline-table denominators
  can no longer drift apart.
* CONSORT diagrams now render a terminal "Analysis dataset (per-protocol)"
  box after "Enrolled after matching" (post-matching person-trials that
  contributed to the analysis, `n_baseline`), styled distinctly (non-red):
  first-period censoring (protocol deviation or loss to follow-up) is
  analytic censoring handled by IPCW, not an eligibility exclusion. Matching
  is likewise shown as a selection step, not an exclusion.
* `.s3_enrollment_worker()` now records per-arm analysis-set counts
  (`n_baseline_intervention`/`n_baseline_comparator`), so the analysis box
  shows the same per-arm breakdown as the other boxes (falling back to the
  total when the split cannot be reconciled). Regenerating Loop 3a results
  is required to populate the per-arm counts on previously cached runs.
* The attrition worksheet now includes the matching and analysis steps (it
  previously stopped at the eligible cohort) and gains `kind`/`change_kind`
  columns; renamed to "cohort derivation (CONSORT)".
* Fixed `.format_enrollment_summary()`: it previously labelled `n_baseline`
  (the per-protocol analysis-set size) as the "After matching" count. The
  summary now reports the true post-matching count from the matching table
  and labels `n_baseline` as the analysis dataset.
* Added `tests/testthat/test-cohort_flow.R` covering flow assembly and DOT
  rendering of the matching and analysis steps.

## Table 1: honest head-count N + separate sum-of-weights row

* The Table 1 top rows now distinguish counts from weights. Every panel
  (weighted or not) shows an `N` row with the real head-count of
  contributing person-trials; weighted panels additionally show a
  `Sum of weights` row (the effective weighted total). Previously the
  weighted `n` row reported the sum of weights labelled as a count.
* Weighted panels now show the weighted **proportion only** in category
  cells (e.g. `9.7%`) instead of a summed-weight pseudo-count with a
  percentage (e.g. `53,615 (9.7%)`). Unweighted panels are unchanged
  (`count (pct%)`), and weighted continuous rows keep `mean (SD)`.
* Percentage denominators are unchanged (still the weighted totals), so all
  proportions and SMDs are identical to before -- only the displayed counts
  changed.

# swereg 26.5.20

## All-subprocess s1 architecture (OOM fix + clean dispatcher)

Background: the previous s1 design mixed parallel-pool work and main-
thread work in the same loop. After `parallel_pool()` returned for the
multi-scout, the main R process held ~41,686 `(tuples, attrition)`
data.tables (2,194 skeletons x 19 enrollments) in RAM, then layered an
rbindlist of ~2,194 panel chunks on top during the per-enrollment
post-step. On a 19-enrollment run (2,194 skeleton files, 6
workers) this peaked high enough that the parent
process either OOMed at the end of the multi-scout or starved the
loop-3 workers when they spawned.

`$s1_generate_enrollments_and_ipw()` is now a pure dispatcher: every
step that touches multiple skeletons' worth of data runs in a
subprocess and exits when done. The main R process holds only paths,
status flags, and progressors -- never a data.table.

### Sub-step nomenclature

Loop 1 is split into four named sub-steps (s1a..s1d). Each sub-step
runs in its own subprocess and communicates with the next via files
in a per-project work directory:

```
{study$data_meta_dir}/s1_work/{project_prefix}/
```

| Sub-step | Mode                                  | Worker script         |
|----------|---------------------------------------|-----------------------|
| **s1a**  | parallel x skeleton (`n_workers`)     | `worker_s1a_multi.R`  |
| **s1b**  | single subprocess per enrollment      | `worker_s1b.R`        |
| **s1c**  | parallel x (enrollment x skeleton)    | `worker_s1c.R`        |
| **s1d**  | single subprocess per enrollment      | `worker_s1d.R`        |

s1a writes `s1a_cache_*` + `s1a_pre_*` + a per-skeleton sentinel.
s1b reads all `s1a_pre_*` for one enrollment, samples comparators,
writes `s1b_enrolled_ids_*` + `s1b_attrition_*` + the enrollment
counts sidecar + sentinel.
s1c reads `s1a_cache_*` + `s1b_enrolled_ids_*`, builds the panel,
writes `s1c_panel_*` + sentinel.
s1d reads all `s1c_panel_*` for one enrollment, imputes, computes
IPW, truncates, writes the final `file_raw` + `file_imp` + sentinel.

The work directory is removed automatically on a successful end-to-
end run.

### Renames (breaking for internal `:::` callers; no public API impact)

* `.s1b_worker()` is now `.s1c_worker()` (panel build). The two-arg
  in-memory helper used by `dev/verify_*.R` and `dev/profile_*.R` is
  exposed as `.s1c_worker_impl()` (formerly the body of
  `.s1b_worker`).
* `inst/worker_s1b.R` previously dispatched panel build; that role
  moved to `inst/worker_s1c.R`. `inst/worker_s1b.R` is now the
  match worker.
* `.s1a_worker()` (single-enrollment scout) is unchanged. Used only
  by tests/verify/profile; the orchestrator does not call it.
* New internal helpers: `.s1b_worker()`, `.s1d_worker()`,
  `.s1_work_dir()`, and path constructors
  (`.s1a_cache_path()`, `.s1a_pre_path()`, `.s1a_done_path()`,
  `.s1b_enrolled_ids_path()`, `.s1b_attrition_path()`,
  `.s1b_done_path()`, `.s1c_panel_path()`, `.s1c_done_path()`,
  `.s1d_done_path()`), and `.touch_sentinel()`.

### Resume

`resume = TRUE` is now sentinel-based across all four sub-steps. The
master skips any sub-step whose sentinel file is present in the
work directory, so a crash in s1c (for example) only requires
redoing the missing panel chunks -- not the upstream scout or the
downstream post-step.

### `parallel_pool(collect = FALSE)` everywhere

All four sub-steps invoke `parallel_pool()` with `collect = FALSE`.
Workers write their outputs directly to final paths in the work
directory; no result data is shipped back to the master through
qs2 tempfiles. This was the architectural change that eliminated
the post-pool memory hump.

### What didn't change (by design)

* Math/semantics are identical -- the match step uses the same
  `set.seed(enrollment_spec$seed)` and the same `data.table` group-by
  + sample logic; the post step uses the same `tteenrollment_rbind`
  + `s2_ipw` + `s3_truncate_weights` chain; IDs in `enrolled_ids`,
  `enrollment_counts`, `file_raw`, and `file_imp` are bit-identical
  to those produced by 26.5.19.
* `swereg::tteplan_locate_and_load(...)$s1_generate_enrollments_and_ipw(...)`
  -- the user-facing entry point -- has the same signature
  (`output_dir`, `impute_fn`, `stabilize`, `n_workers`,
  `swereg_dev_path`, `resume`).

### Caveats / behaviour change to be aware of

* `impute_fn` is serialised via qs2 across the subprocess boundary
  into s1d. The default `tteenrollment_impute_confounders` is
  namespaced and round-trips cleanly. Custom imputation closures
  that capture unexported state from the caller's session may not
  deserialise; pass them as either `swereg::your_fn`-style refs or
  as self-contained closures.
* The work directory consumes ~10-20 GB during a run of that size
  (cache + pre + panel chunks). Plan accordingly; on success it is
  removed automatically.

### Tests

* `tests/testthat/test-worker_arg_parity.R` now covers all five
  Loop-1 worker scripts (s1a, s1a_multi, s1b, s1c, s1d).

# swereg 26.5.19

## Performance

Large-scale flame-graph-driven optimisation of `s1`. End-to-end output
is bit-identical (verified via A/B against the pre-patch reference on a
real production skeleton: 25 columns x 4.16 M panel rows, plus
direct comparison of `(tuples, attrition)` for all 19 enrollments on
the first skeleton file). On a study of that size (19 enrollments x
2,194 skeleton files, 6 workers) the projected wall savings on a 10-day s1
run total ~21 hours.

### Stage 1a / multi-enrollment scout

* `s1_generate_enrollments_and_ipw()` now does its scout pass per
  *skeleton file* across all enrollments at once, instead of per
  (enrollment x skeleton). Each canonical skeleton (~5 MB qs2, ~3.7 GB
  decompressed for 1,025 columns) is deserialised ONCE per s1 run
  instead of 19 times, saving ~9-11 hours wall on a run of that size.
  Driven by a new internal worker `.s1a_worker_multi()` and worker
  script `inst/worker_s1a_multi.R`.

* The multi-scout worker projects the canonical to the union of columns
  any enrollment uses (typically ~50-100 of ~1,025) immediately after
  load, dropping the rest in place via `:= NULL`. Apply-exclusions etc.
  then operate on a much smaller working data.table. Between enrollment
  iterations we drop only the columns prepare/finalize added (instead
  of `data.table::copy()`-ing the canonical, which itself cost ~3 s
  per iteration). New helper: `.tte_canonical_needed_cols()`.

* `.s1a_worker_multi()` writes a per-skeleton scout checkpoint file
  (`s1_scout_<basename>.qs2`, ~0.5 MB) containing all 19 enrollments'
  `(tuples, attrition)`. The outer dispatch checks for existing
  checkpoints + cache files and skips any skeleton whose scout is
  already complete, so a mid-scout crash on resume only redoes the
  skeletons that hadn't finished. Checkpoint round-trip: 72 s scout vs
  0.08 s read.

* Split `.s1_prepare_skeleton()` into `.s1_load_skeleton()` (qs2 read +
  `setalloccol` + `setkey`) and `.s1_prepare_loaded()` (exclusions +
  treatment + eligibility combine) so the two worker variants share
  internal logic.

* Split `.s1a_worker()`'s post-prep work into
  `.s1a_finalize_on_skeleton()` (attrition + tuples + cache write),
  shared with `.s1a_worker_multi()`.

### Stage 1b / cache projection

* `.s1a_worker()` and `.s1a_worker_multi()` write the per-enrollment
  cache projected to only the columns s1b actually consumes (~30 of
  ~1,025): `id`, `isoyearweek`, `trial_id`, treatment/rd_intervention
  cols, `design$confounder_vars`, `design$outcome_vars`, the
  `eligible_*` cols, plus source variables for any computed confounder.
  Cache file shrinks ~10x (~5 MB -> ~0.5 MB per file); s1b cache read
  drops from ~5 s to ~0.5 s per worker call. Across 19 x 2,194 s1b
  calls / 6 workers this saves ~10 hours wall. New helper:
  `.tte_s1_cache_columns()`.

* Removed the redundant per-enrollment `%in%` filter in
  `private$enroll()` Phase B when `.s1b_worker` has already filtered
  the cache to enrolled persons upstream (gated by the new
  `.tte_filtered_to_enrolled` attribute on the skeleton). Avoids
  allocating a ~3 GB identity copy of the panel per stage-1b worker.

### Batched per-id derivations (s1a)

* `tteplan_apply_exclusions()` and `tteplan_apply_derived_confounders()`
  now collect all per-person (`by = id`) grouped derivations into a
  single `dt[, c(...) := list(...), by = id]` call via the new
  `.tte_apply_eligibility_batch()` helper, instead of one
  `dt[, col := f(x), by = id]` call per criterion. With 12 exclusions
  + 4 computed confounders for 003, this collapses 16 separate radix
  walks of the 17 M-row skeleton into 1. `.s1_prepare_skeleton()`
  fuses both helpers into one combined batch so the skeleton is
  walked exactly once.

* `.s1_compute_attrition()`: fused
  `filtered <- sk[mask]; pt_i <- filtered[, ..., by]` into
  `pt_i <- sk[mask, ..., by]` inside the cumulative-criterion loop,
  eliminating ~220 MB allocation per criterion. Dropped redundant
  `== TRUE` on the cumulative mask; replaced
  `sum(.tte_tx_any == TRUE)` / `sum(.tte_tx_any == FALSE)` with
  `sum(.tte_tx_any)` / `sum(!.tte_tx_any)`.

* `.s1_eligible_tuples()`: fused `[i][, j, by=]` and dropped the
  redundant `setorderv()` (caller already sorted; `any()` doesn't need
  ordered input). Eliminates a 1.18 GB intermediate allocation.

* `.s1_prepare_skeleton()`: collapsed three sequential `[, := ]` calls
  for `rd_intervention` / `baseline_intervention` /
  `eligible_valid_treatment` into one multi-column assignment
  (evaluates `fcase()` once and writes three columns in one dispatch).

* `any_events_prior_to()`: replaced
  `c(FALSE, cum[-n] > 0L)` (three n-vector allocations per call --
  slice, compare, prepend) with
  `data.table::shift(prior_counts, n = 1L, fill = 0L) > 0L` (one
  allocation). The function is called once per person per exclusion
  criterion, so the saving multiplies.

### Faster post-rbind impute + IPW (s1 post)

* `TTEEnrollment$s1_impute_confounders()` is faster on production-scale
  panels. Three changes:
  - Pre-scan baseline rows for NAs and skip the full panel collapse +
    merge entirely when every confounder is complete.
  - When only some confounders need imputing, restrict the group-by
    collapse and merge-back to that subset (`needs_impute`) instead of
    the full `confounder_vars`.
  - Replaced the drop + `merge.data.table` round trip with an
    `[, := mget(paste0("i.", needs_impute)), on = id_var]` update join.
    Avoids allocating a new merged data.table for the 17 M-row panel.
  Measured on a 17 M-row trial panel with 7 confounders:
  impute step dropped from 4.69 s to 4.17 s; full post-rbind block
  (impute + s2_ipw + s3_truncate_weights) 10.19 s -> 8.86 s.

* `private$enroll()` Phase B: replaced
  `data[get(person_id_col) %in% enrolled_person_ids]` with a binary-
  search keyed join `data[.(unique(...)), on = person_id_col]` on the
  existing `(id, isoyearweek)` key. Avoids the temporary hash
  allocation that drove GC pressure on this 17M-row filter.

* `private$enroll()` Phase B: dropped the `setorderv()` immediately
  preceding `setkeyv()` on the same columns (two sorts collapsed into
  one). Included `isoyearweek` in the key so `first(isoyearweek)`
  inside the aggregation remains deterministic.

### Misc

* Replaced `paste0()` with `stringi::stri_c()` at the three hot
  panel-ID construction sites (`r6_tteenrollment.R` Phase B + Phase C,
  `enrollment_id` prefix in `r6_tteplan.R::.s1a_worker`). `stri_c()`
  is ~50% faster than `paste0()` on long character vectors. Stage-1b
  paste0 self time dropped from 9.2% to 4.7%.

* Added `stringi` to `Imports`.

### Progress reporting

* The single combined `progressr::progressor()` is replaced by four
  per-loop progressors, each created lazily right before its loop so
  the handler's "active" bar matches the current phase:
  - `p_eligibility` (2194 steps, per skeleton, parallel) for the
    multi-enrollment scout.
  - `p_match` (19 steps, per enrollment, main process) for the
    comparator-sampling step.
  - `p_panel` (19 x 2194 steps, per enrollment x per skeleton,
    parallel) for `worker_s1b.R`.
  - `p_post` (19 steps, per enrollment, main process) for
    rbind + impute + IPW + truncate + save.
  Each loop is preceded by a `cat()` header explaining what work it
  does and over what unit.

# swereg 26.5.18

## Bug Fixes

* **TTE per-protocol bias fix**: `TTEEnrollment$s4_prepare_for_analysis()` now
  drops censoring-event rows (where `censor_this_period == 1`) from
  `self$data` after the IPCW model is fit. Previously these rows were
  retained with `event = 0`, which biased the downstream weighted outcome
  regression toward the null.

  On a synthetic dataset with a known true per-protocol log-OR of -0.49,
  the previous behavior produced an estimate of -0.38 (bias +0.11); after
  the fix, the estimate is -0.52 (bias -0.02), agreeing closely with the
  canonical CRAN `TrialEmulation` package on the same data.

## New Tests

* `tests/testthat/test-tte_simulation_correctness.R`: end-to-end correctness
  tests using simulated data with a known true PP effect. Skipped on CRAN.
* `tests/testthat/test-tte_vs_trialemulation.R`: cross-package comparison
  against the CRAN `TrialEmulation` package. Skipped when `TrialEmulation`
  is not installed or on CRAN.

# swereg 26.5.17

## Breaking changes

* `RegistryStudy$compute_population()` and
  `RegistryStudy$compute_summary()` are no longer public methods.
  They are now internal and run automatically at the end of
  `$process_skeletons()`.
* Population computation is declarative: pass `population_by_specs`
  to `RegistryStudy$new()` (a list of character vectors, one per
  desired `by` aggregation). Each registered spec is pre-computed
  into the per-batch `meta_*.qs2` sidecar and reduced to
  `population_<spec>.qs2` once per `process_skeletons()` run.
* New getter `study$population(by = c(...))` reads the cached
  population table. Errors with a clear message when `by` is not in
  `population_by_specs`.
* New active binding `study$summary` reads back `summary.qs2`.

## Performance

* `study$population(by = ...)` is now a sub-second meta-only walk
  instead of re-loading every full skeleton from disk on each call.
  First call after `process_skeletons()` is fast because the
  per-batch aggregations were computed in-memory while skeletons
  were already loaded.
* Adding a new spec to `population_by_specs` between runs triggers
  a meta-only refresh on the next `$process_skeletons()`: skeletons
  on disk are not rewritten; only the `meta_*.qs2` sidecars are
  augmented with the missing aggregation.

## Changed

* `$delete_skeletons()` now also removes cached `population_*.qs2`
  and `summary.qs2` files in `data_skeleton_dir`.

## Fixed

* Excel `excel_spec_summary()` renderer: named criteria under each
  enrollment's `additional_inclusion` / `additional_exclusion` blocks
  now render one indent deeper than their parent section header
  (previously they collided with it), and the `Age range:` row is
  bolded like the other criterion names. Adds a third indent level
  (sub-sub) with new styles `st_*_sub_sub_item` / `st_*_sub_sub_label`,
  a new `add_sub_sub_item()` helper, and a `sub_sub` argument on
  `add_kv()` / `add_yellow()` / `add_var()` / `add_derived_var()`.

* Excel `excel_spec_summary()` renderer: surfaces
  `spec$standing_methods$calendar_time` as the first entry in the
  Confounders section when present. Calendar time at trial registration
  is auto-adjusted via the IPW/IPCW models, so rendering it explicitly
  stops protocol reviewers re-asking "what about calendar year?" on
  every TTE.

## Changed

* `self$spec_xlsx` (and therefore `excel_spec_summary()` when called
  without an explicit `path`) now writes to `spec_<version>.xlsx`
  instead of a fixed `spec.xlsx`. The filename mirrors the YAML
  convention (`spec_v003.yaml`, `spec_v004.yaml`, ...) so each spec
  iteration produces a non-overwriting Excel artefact alongside it.
  The previous `FILENAME_SPEC_XLSX` constant is replaced by
  `filename_spec_xlsx(version)`.

# swereg 26.5.16

## Changed

* `status.txt` bucket lines now carry denominators -- e.g.
  `dorsu  90 / 137` shows that 90 of the 137 `dorsu_*` variables
  never matched. Same denominator appears in the per-bucket header
  (`dorsu (90 / 137):`). Makes "is this prefix unusually problematic
  or just big?" readable at a glance.

# swereg 26.5.15

## Changed

* `status.txt`: each section (never-matched, rare) now leads with a
  bucket-count summary (one row per registry prefix, descending by
  size) before the per-bucket detail blocks. Per-bucket lists are
  never collapsed -- every column name appears in full. Rare cutoff
  rendered as "1-9" (clearer than "< 10").

# swereg 26.5.14

## Changed

* `status.txt` rendered by `$compute_summary()` is restructured:
  - The `[ok]` count appears first (above the noise) so the
    headline number ("how many variables look healthy") is
    immediately readable.
  - Never-matched and rare-variable sections are grouped by the
    column's registry-type prefix (everything up to the first
    underscore: `dorsm`, `sv`, `os`, `osd`, `can`, `op`, `rx`, ...),
    sorted by bucket size descending so the dominant problem shows
    first. Avoids the 252-line flat alphabetical list that previously
    forced the reader to scroll past the entire never-matched dump
    to reach the actually-OK count.
  - All numbers comma-formatted (`8,852,776` not `8852776`).
  - Trailing pointer to `summary.qs2` for full per-column detail.

# swereg 26.5.13

## Changed

* Meta sidecar now carries weekly/annual splits and the date range of
  weekly data: `n_rows_weekly` / `n_rows_annual`, `n_persons_weekly` /
  `n_persons_annual`, `weekly_min_isoyearweek` / `weekly_max_isoyearweek`,
  `annual_min_isoyear` / `annual_max_isoyear`. Per-column `$counts` now
  emit `n_person_weeks_with` (TRUE rows where `is_isoyear == FALSE`) and
  `n_person_years_with` (TRUE rows where `is_isoyear == TRUE`)
  separately; previously the single field was misleadingly named since
  it was actually `nrow()` (weekly + annual combined).
* `$compute_summary()` aggregates these splits; `status.txt` now prints
  the WEEKLY and ANNUAL time periods with their respective denominators.
* TSV audit-track gains an `n_person_years_with` column and header
  comments for both periods.

# swereg 26.5.12

## Breaking

* Removed the per-batch code-check warning machinery added in PR #4 +
  26.5.10. Specifically removed: `start_code_check_session()` /
  `end_code_check_session()` exports, `warn_unmatched_codes()` /
  `warn_empty_logical_cols()` exports, the internal `.swereg_codes_pre()`
  / `.swereg_codes_post()` hooks, `.code_check_snapshot()` /
  `.code_check_merge()` / `.code_check_emit()`, and the `code_check_state`
  field on the meta sidecar. Rationale: this information is now exposed
  more usefully via `RegistryStudy$compute_summary()` (see below), so
  having two parallel mechanisms reporting the same data was redundant.
* `RegistryStudy$save_skeleton()` no longer takes a `code_check_state`
  argument (it had no other callers). The meta sidecar shape changed:
  `code_check_state` is dropped, `n_persons` is added, and every entry
  in `applied_registry` now carries a `$counts` sub-field. Older meta
  sidecars (built before this version) keep working but their `$counts`
  field is missing, so `$compute_summary()` will report zero per-column
  counts for those batches. To fix, **delete the affected
  `skeleton_*.qs2` + `meta_*.qs2` files** (or call
  `study$delete_skeletons()`) and re-run `$process_skeletons()`.

## New

* `RegistryStudy$compute_summary()`: aggregates per-batch `meta_*.qs2`
  sidecars into a study-wide sanity report. Always writes `summary.qs2`
  (binary; programmatic reload) and `status.txt` (human-readable flag
  report) to `data_skeleton_dir`. On full runs (every expected batch
  present), also writes a git-tracked
  `summary_<UTC>_<git-sha>_<swereg-ver>.tsv` to the new
  `data_summaries_dir` candidate, with counts below `suppress_below`
  (default 5) masked as `"<N"` (Swedish registry data convention).
  Partial runs explicitly skip the TSV.
* New `data_summaries_dir` constructor parameter (optional). Defaults
  to NULL; when NULL, `$compute_summary()` skips the TSV even on full
  runs.
* `Skeleton$apply_code_entry()` now computes per-column
  `n_persons_with` and `n_person_weeks_with` for every column it adds,
  stored on the entry's `applied_registry` record. These are the
  primitive that `$compute_summary()` rolls up.
* `kept` -- `expand_codes()` / `expand_code_list()` (bracket / range
  expansion utilities from PR #4) remain. The expansion of code
  patterns happens inline in each `add_*()` function; the removed
  pieces were specifically the per-call *warning emission* machinery,
  not the *code expansion* machinery.

# swereg 26.5.11

## Fix

* `.swereg_dev_path()` (helper that hands the package root to callr
  workers so they can `devtools::load_all()` the same dev source as the
  parent) was returning `system.file(package="swereg")` directly, which
  for a dev-loaded package is the `inst/` subdirectory -- not the
  package root that `devtools::load_all()` expects. Workers then loaded
  a broken namespace and `$process_skeletons()` failed on batch 1 with
  `attempt to apply non-function`. Now strips the trailing `/inst` so
  the worker receives the actual package root.

# swereg 26.5.10

## Breaking (on-disk)

* Skeleton schema bumped from version 4 to version 5 to introduce a
  `meta_%05d.qs2` sidecar next to every `skeleton_%05d.qs2`. Existing
  skeleton directories from earlier versions are not readable; run
  `study$delete_skeletons()` and re-run `$process_skeletons()` to
  regenerate. The sidecar is small (a few KB) and lets `$process_skeletons()`
  do an incremental-rebuild check by reading meta only -- avoiding the
  full skeleton deserialise on the common no-change path. For a 2000-batch
  delivery with 500 MB skeletons this turns a ~1 TB read into a few MB.

## New features

* `RegistryStudy$process_skeletons()` now emits a single consolidated
  code-check warning at the end of every run, covering every batch in
  scope. Sequential and parallel runs behave identically because the
  per-batch accumulators flow through the meta sidecars on disk rather
  than via in-memory state.

* `RegistryStudy$skeleton_pipeline_hashes()` now reads the small meta
  sidecars instead of deserialising every skeleton, with a transparent
  fallback to loading the skeleton when meta is missing. Significantly
  faster on large studies.

## Internal

* The code-check session machinery introduced in 26.5.9 is now fully
  internal: the previously-exported `start_code_check_session()`,
  `end_code_check_session()`, `expand_codes()`, `expand_code_list()`,
  `warn_unmatched_codes()`, `warn_empty_logical_cols()` are no longer
  exported. Users running through `$process_skeletons()` get
  cross-batch aggregation automatically; users running manual loops
  outside `RegistryStudy` get per-call warnings (the pre-26.5.9
  behaviour). If you need cross-batch aggregation in a manual loop,
  open an issue and we'll re-export.

# swereg 26.5.9

## New features

* `add_diagnoses()`, `add_operations()`, `add_cods()`, `add_rx()`,
  `add_icdo3s()`, `add_snomed3s()` and `add_snomedo10s()` now accept
  bracket / character-class / range patterns directly (e.g. `"I2[0-5]"`,
  `"FN[ABCDEGW][0-9][0-9]"`, `"!302[A-Z]"`). Bracket expansion runs
  unconditionally; the matchers themselves continue to use `startsWith()`
  on literal prefixes.

* The same `add_*` family now runs pre-call (per-literal source-data) and
  post-call (column-level) sanity checks automatically. Bad patterns
  surface at run-time instead of producing silent empty columns. Both
  checks can be disabled via `options(swereg.check_codes = FALSE)`.

* The pre-call check also runs a cheap, data-free *syntax* check on the
  expanded code list, firing in milliseconds at the first `add_*()` call
  rather than at hour 6 of a multi-hour batched pipeline. It warns when
  any expanded literal is empty or contains regex metacharacters
  (`^ $ * + ? . ( ) | \ [ ]`) that will not match under `startsWith()`.
  Skipped automatically for `add_rx(source = "produkt")` because product
  names are exact-matched via `%chin%` and may legitimately contain
  those characters.

Contributed by @alexengberg (PR #4).

# swereg 26.5.8

## Breaking

* File-naming format width bumped from 3 digits to 5 digits for batch /
  ETT identifiers. Rawbatch files become `00001_rawbatch_lmed.qs2`
  (was `001_rawbatch_lmed.qs2`); skeleton files become
  `skeleton_00001.qs2` (was `skeleton_001.qs2`); ETT identifiers
  become `ETT00001` (was `ETT001`). Existing on-disk files using the
  old 3-digit width will not be recognised by the new code -- callers
  must rename them via shell or regenerate. Affects `RegistryStudy`
  (rawbatch + skeleton) and `TTEPlan` (ett_id, and the
  `file_analysis = "<prefix>_analysis_<ett_id>.qs2"` derived name).

# swereg 26.5.7

## Breaking-ish

* `registrystudy_load()`: parameter renamed `candidate_dir_rawbatch`
  -> `candidate_dir_meta`. Callers must update if they passed by name.
  Behaviour is unchanged for callers that passed the same path used at
  `RegistryStudy$new(data_meta_dir = ...)` (which defaults to the
  rawbatch dir, so existing scripts continue to work positionally).

## New

* `RegistryStudy$new()` gains `data_meta_dir`: candidate paths for the
  directory holding `registrystudy.qs2`. Defaults to `data_rawbatch_dir`
  (full backward compatibility). Pass an explicit value -- e.g. the
  parent of rawbatch -- to keep the singleton control file out of the
  per-batch data directory. Exposed as a read-only active binding.
* `$meta_file` now resolves to `file.path(self$data_meta_dir, "registrystudy.qs2")`.

# swereg 26.5.6

## Performance

* `create_skeleton()` is now ~8x faster and uses ~7.5x less memory at
  every cohort size tested (1k–25k IDs over an 11-year range; linear
  scaling). The win comes from sorting a single time spine once and
  replicating it per id, instead of `expand.grid()`-ing the full
  cartesian product and then sorting the result. Output is identical
  to the previous implementation modulo a hidden `data.table`
  secondary index attribute. Contributed by @gkaramanis (PR #3).

## Internal

* New `dev/bench/` scaffold for tracking performance regressions:
  `Rscript dev/bench/run_all.R` runs all benchmarks against the
  current source, `Rscript dev/bench/diff.R` compares the run to a
  checked-in `baseline.csv` and exits non-zero on >20% time / >50%
  memory regressions. Excluded from the package build via
  `.Rbuildignore`.
* New `tests/testthat/test-create_skeleton-parity.R` (31 tests)
  pinning `create_skeleton()` output against an embedded reference
  oracle plus structural invariants and edge cases (empty / duplicate
  / NA ids, single-day range, ISO W53 year, year boundary).

# swereg 26.4.28

## Breaking changes

* `RegistryStudy$process_skeletons()` now **fails fast** on any
  batch error instead of swallowing it into a `warning()` and
  pushing through the remaining batches. These pipelines run
  unattended for days; if batch 1 fails 10 minutes in (e.g. a
  systematic bug, missing column, unreadable rawbatch file), the
  user wants to SSH in within minutes and see the failure -- not at
  the end of a 4-day run with the remaining batches all failing for
  the same root cause. The failing batch's underlying error is
  surfaced via a `stop()` that includes (a) which batch number
  failed, (b) the original error message, and (c) a hint that
  successful batches are persisted on disk and the user can rerun
  with `batches = ...` to retry from the failed one. In the
  `n_workers > 1` (callr subprocess) path, in-flight workers are
  killed via `kill_tree()` before the `stop()` so they don't keep
  burning compute on what is likely the same systematic failure.
  Callers who intentionally want the old "complete with warnings"
  behaviour can wrap the call in `tryCatch(error = function(e) ...)`.

## New features

* `add_rx()` now supports `"!"`-prefixed exclusion patterns, restoring
  parity with the `add_diagnoses()` family. Previously, `add_rx`'s
  matcher was a one-shot `Reduce(|, ...)` union with no per-pattern
  branching, so `"!"`-prefixed entries were silently treated as
  literal first characters of an ATC code (which never match) or
  literal product names (which would, accidentally, *include* a
  brand named with a leading `!`). Now:

      codes = list(rx_c10_nonstatin = c("C10A", "!C10AA", "!C10AB"))

  matches any lipid-modifying agent except the statin and fibrate
  (C10AA / C10AB) classes. This closes the spec-expressiveness gap that
  previously forced exhaustive enumeration of every desired sub-code. The veto
  is independent per named code (no leak across list entries) and
  applies whether `source = "atc"` (prefix match) or `source =
  "produkt"` (exact match).

## Documentation

* Expanded `add_rx()` `@param codes` to spell out four nuances that
  the `add_diagnoses` family carried implicitly: vetoes are
  independent per named code (no leak across list entries); veto
  match style follows `source` (prefix for atc, exact for produkt
  -- so `"!Sertralin"` does NOT mask `"Sertralin Sandoz"`);
  all-negative pattern sets produce empty columns; and the per-
  source-row veto interacts with the per-week aggregation such that
  a non-vetoed Rx still drives a week to TRUE even if a vetoed Rx
  overlaps in the same week.

* Fixed misleading pattern-syntax documentation on `add_diagnoses()`,
  `add_cods()`, `add_icdo3s()`, `add_snomed3s()`, `add_snomedo10s()`,
  and `add_operations()`. The previous text described patterns as
  regex with auto-prepended `^` anchors and example strings like
  `"^F640"` / `"^8140"` / `"^80146002"`. The actual matcher is
  prefix-only via `startsWith()` -- a literal `^` in a pattern is
  treated as an ordinary character and silently matches nothing.
  Updated each function's `@param codes` to describe the real
  contract (prefix matching, no regex), and added a clear explanation
  of `"!"`-prefixed row-level vetoes (including the important detail
  that the veto operates on the raw source row, not on the
  `(id, isoyearweek)` bucket -- a non-vetoed code in the same week
  still triggers TRUE).
* `add_rx()` documentation now explicitly notes that it does NOT
  support `"!"` exclusion patterns (its matcher is a simple union).

# swereg 26.4.27

## Maintenance

* Renamed user-facing "exposure" terminology in the results workbook
  to match the project's TTE vocabulary, where the assignment is the
  *treatment* and the active arm is the *intervention*. The
  Sensitivity sheet's identifier column header `Exposure` is now
  `Intervention`, and the per-arm measurement columns `Events (exp)`,
  `PY (exp)`, `Rate/100k (exp)` are now `Events (int)`, `PY (int)`,
  `Rate/100k (int)` (paired with the unchanged `(cmp)` columns).
  Existing workbooks already on disk are not rewritten -- re-run
  `plan$export_tables()` to regenerate.

## New features

* `TTEPlan$s3_analyze()` gains a `force` argument (default `FALSE`). When
  `TRUE`, cached `results_enrollment` and `results_ett` entries in the
  targeted scope are dropped before recomputation. Scope follows
  `enrollment_ids` / `ett_ids`: with both `NULL`, all cached results are
  cleared; otherwise only matching entries are dropped. Provides a
  supported way to recompute after a broken environment produced
  `skipped = TRUE` placeholders (e.g. missing `survey` package), without
  poking R6 internals from the calling script.

* `TTEPlan$s3_analyze()` gains an `n_workers` argument (default `1L`).
  Both the enrollment loop and the per-ETT loop now dispatch through
  `parallel_pool()` with the requested concurrency (previously hardcoded
  to `1L` at both call sites). Each subprocess loads its own analysis
  file, so peak RAM scales linearly with `n_workers`; CPU threads per
  worker are auto-partitioned as `floor(detectCores() / n_workers)`.

## Bug Fixes

* CONSORT cascade no longer double-counts the global cohort. In
  `.s1_compute_attrition()`, the per-trial summary tables (`before_row`
  and the per-criterion `rows[[i]]`) were grouped by `trial_id` over a
  `pt0` that included person-weeks outside any trial period. Those rows
  collapsed into a spurious `(trial_id = NA, criterion)` group whose
  `n_persons` later got summed together with the legitimate
  `before_global` / `global_rows[[i]]` row during per-batch aggregation
  (`by = .(trial_id, criterion)` at the centralized matching step),
  roughly doubling the global cohort number in the CONSORT diagram and
  attrition tables. Per-trial counts and arm counts were unaffected.
  Fix filters `pt0[!is.na(trial_id)]` (and `pt_i[!is.na(trial_id)]`)
  before computing per-trial summaries; the global row is still
  computed off the unfiltered `pt0` so it captures the full
  pre-exclusion uniqueN. Existing `enrollment_counts_*.qs2` files need
  to be regenerated by re-running s1 to pick up the corrected counts.

* `inst/worker_s2.R` now passes `sep_by_tx` (matching `.s2_worker()` and the
  plan-side item field) instead of the stale `sep_by_exp`. Previously
  `plan$s2_generate_analysis_files_and_ipcw_pp()` failed at the very first
  ETT with `unused argument (sep_by_exp = params$sep_by_exp)`.

## Maintenance

* Promoted runtime-required packages from `Suggests` to `Imports` so they
  install automatically: `survey`, `survival`, `mgcv`, `MASS`, `scales`,
  `glue`, `openxlsx`, `patchwork`, `DiagrammeR`, `DiagrammeRsvg`, `rsvg`.
  All are referenced unconditionally from R/ (no `requireNamespace()`
  guards or alternative code paths). Previously, missing `survey` caused
  every IRR call in `s3_analyze` to be silently captured as
  `list(skipped = TRUE, reason = "there is no package called 'survey'")`,
  which then produced an empty forest plot in `export_tables` with no
  visible error. Missing `DiagrammeR`/`DiagrammeRsvg`/`rsvg` similarly
  skipped CONSORT sidecars with only a warning.

# swereg 26.4.25

## Maintenance

* Version bump.

# swereg 26.4.22

## New features

* **CONSORT reporting surfaces unique-person counts alongside
  person-trial counts.** Sequential TTE inflates the analytic denominator
  because one person enters many weekly trials; a cohort with 390k women
  can generate 22M person-weeks, and the 60x gap routinely confuses
  reviewers. Attrition bookkeeping now carries both numbers end-to-end:
    - `.s1_compute_attrition()` emits a global `trial_id = NA` row per
      criterion alongside the existing per-trial rows, with a true
      `uniqueN(person_id)` across the whole skeleton. Summing per-trial
      `n_persons` over-counted anyone entering more than one trial; the
      global row is the honest number. Per-trial rows are retained for
      diagnostic slicing.
    - `.build_consort_dot()` now renders one lumped red side-box with a
      bulleted list of every exclusion criterion (CONSORT-2010
      convention) instead of a stacked red box per criterion, and each
      box reports `N persons / M person-trials`. Enrollment titles are
      split at " (" onto two lines so long spec labels don't blow out
      the box width.
    - `TTEEnrollment$rates()` gains an `n_persons` column per treatment
      arm, using `design$person_id_var`.
    - Results workbook: each enrollment's combined-baseline sheet opens
      with a one-line "Cohort: N persons contributed M sequential trial
      enrollments" summary. A companion `Attrition_{id}` sheet carries
      the tabular form of the CONSORT numbers (criterion / n_persons /
      n_person_trials / excluded counts / n_intervention / n_comparator)
      so reviewers can cite exact figures without measuring pixels on
      the PNG sidecar.

* **`$register_codes()` auto-validates the `add_*` contract.** Every
  call to a registered `fn` is now wrapped with a pre/post-state check:
  row count must be preserved, structural columns (`id`, `isoyear`,
  `isoyearweek`, `is_isoyear`) must still be present, and every column
  name in the registration's `codes` list must actually exist on the
  skeleton after the call. Contract violations error loudly with a
  pointer back to the offending `$register_codes(<label>)` entry.
  Custom `add_*` functions plugged into the pipeline (Norwegian
  registries, regional Swedish cohorts, payer claims, …) get this
  enforcement for free; built-ins already pass the checks, so no
  behaviour change for existing registrations.

## Documentation

* **New vignette `builtin-add-functions`** — end-to-end walkthrough of
  every `add_*` function swereg ships (`add_onetime`, `add_annual`,
  `add_diagnoses`, `add_operations`, `add_rx`, `add_cods`,
  `add_quality_registry`), with pattern syntax, collision policies, and
  a typical end-to-end ordering.
* **New vignette `custom-add-functions`** — how to write your own
  `add_*` function for registries swereg doesn't ship support for
  (non-Swedish registries, in-house data, quality registries without
  dedicated built-ins). Covers the contract, reusing built-ins via
  `$register_codes(fn = swereg::add_diagnoses)`, `fn_args` for extra
  knobs like `diag_type = "main"`, a complete `add_vaccinations()`
  worked example run through a real `RegistryStudy`, a demonstration
  of the auto-wrap firing on a deliberately broken function, and a
  design cheat sheet of lessons from the built-ins.

## User experience

* **`RegistryStudy$skeleton_pipeline_hashes()` now reports progress.**
  After the last batch finishes, `$process_skeletons()` calls
  `$write_pipeline_snapshot()`, which in turn calls
  `$skeleton_pipeline_hashes()` to collect per-batch metadata. That
  function has to deserialize every `skeleton_*.qs2` file from disk
  (via `qs2::qs_read()`) to read a handful of hash fields, which with
  many/large batches can take tens of minutes. Previously this ran
  silently, making `$process_skeletons()` look hung after dispatching
  the last batch. It now prints an explanatory message up front and
  ticks a `progressr` bar per file.

# swereg 26.4.16

## Documentation

* Consolidated hand-rolled vignettes from 3-step (skeleton1-create,
  skeleton2-clean, skeleton3-analyze) into a 2-step manual workflow
  (skeleton-create, skeleton-analyze) that matches the actual architecture.
* Renamed pkgdown "Hand-rolled" section to "Manual workflow".
* Updated skeleton-concept, skeleton-pipeline, and CLAUDE.md to reflect
  the 2-step model.

# swereg 26.4.20

## Bug fixes

* **`add_cods()` now accepts `codes =` like all sibling `add_*` functions**
  (previously `cods =`). The old `cods =` name is kept as a deprecated
  alias with a warning. This mismatch caused every `add_cods` entry
  registered via `RegistryStudy$register_codes()` to fail silently with
  `"unused argument (codes = list(...))"` — the dispatcher always passes
  `codes = ...` but `add_cods` was the only one not expecting that name.

* **`RegistryStudy$process_skeletons()` progress bar now advances when
  batches fail.** Previously, the parallel branch's error handler emitted
  a warning but never ticked the progressor, so a single failing batch
  left the bar frozen and — combined with `options("warn")` default of
  `0` buffering warnings until the function returns — produced the
  symptom "progress bar stuck at 0% indefinitely". The bar now ticks on
  both success and failure, and failed-batch messages show up in the
  bar's `(last: ...)` slot with a `FAILED` tag. Warnings are also
  emitted with `immediate. = TRUE` so failures surface in real time
  regardless of the session's `warn` setting. The serial branch gained
  matching error handling (previously a single batch error aborted the
  whole run).

## BREAKING CHANGES

* **TTE vocabulary rename** (`exposure` / `exposed` / `unexposed` →
  `treatment` / `intervention` / `comparator`): the TTE system now uses
  PICO-aligned terminology. `treatment` is the umbrella concept (the
  variable naming the assignment); `intervention` and `comparator` are
  the two arm values. This matches how TTE papers are written up in the
  major medical journals (BMJ, NEJM) and Hernán's canonical target-trial
  references. Migration:

  YAML spec:
  - `exposure:` → `treatment:`
  - `exposed_value:` → `intervention_value:`
  - `arms.exposed:` → `arms.intervention:`

  `TTEDesign$new()` arguments:
  - `exposure_var = ...` → `treatment_var = ...`
  - `time_exposure_var = ...` → `time_treatment_var = ...`

  Skeleton / trial-panel columns produced by the pipeline:
  - `baseline_exposed` → `baseline_intervention`
  - `rd_exposed` → `rd_intervention`
  - `n_exposed` / `n_unexposed` (in CONSORT / matching summaries) →
    `n_intervention` / `n_comparator` (and the `_total` / `_enrolled`
    variants)

  Logical semantics unchanged: `TRUE` = intervention arm, `FALSE` =
  comparator arm. Column names now describe what `TRUE` means, which
  is clearer than the prior "exposed" (ambiguous between an assignment
  and a time-varying status). User-chosen column names (e.g., a column
  named `"exposed"` in user data) are unaffected — the rename only
  touches package-provided API surface.

## New features

* **`RegistryStudy$register_derived_codes(codes, from, as)`**: registers a
  "derived" code entry that doesn't read rawbatch data, but instead ORs
  together already-existing skeleton columns from earlier primary entries.
  For each `nm` in `codes`, writes `<as>_<nm> := <from[1]>_<nm> | <from[2]>_<nm> | ...`.
  Use case: building combined outcome columns like
  `osd_* = os_* | dorsu_* | dorsm_*` where the hospital half comes from
  `add_diagnoses` on OV+SV and the death half comes from `add_cods` on
  DORS -- two functions that can't share a single `combine_as` argument
  because they search different raw-data columns (`hdia`/`dia*` vs
  `ulorsak`/`morsak*`).

  Derived entries are full first-class citizens of the code registry:
  they get their own fingerprint, participate in the per-entry
  incremental sync via `Skeleton$sync_with_registry()`, and fold
  upstream primary fingerprints into their own so that editing an
  upstream primary's `fn_args` / `groups` / `codes` (e.g. flipping
  `cod_type` from `"underlying"` to `"multiple"`) automatically
  cascades into a derived replay. Derived entries must be registered
  AFTER their upstream primaries (apply runs in registration order).

# swereg 26.4.19

## BREAKING CHANGES

* **New `Skeleton` R6 class**: per-batch skeleton files on disk are
  now serialized `Skeleton` R6 objects instead of bare `data.table`s.
  They carry their own phase provenance (framework hash, applied code
  registry fingerprints keyed by their minimal descriptor, and an
  ordered named list of applied phase-3 "randvars" steps). Legacy
  bare-`data.table` skeleton files are auto-wrapped on first load for
  backwards compatibility. Downstream code that reads skeletons via
  `qs2_read()` now needs to unwrap via `sk$data`; the three swereg
  internal call sites (in `.s1_prepare_skeleton()`, `.s1b_worker()`
  cache reuse, and `tteplan_from_spec_and_registrystudy()`) have been
  updated.

* **`RegistryStudy$process_skeletons()` signature changed**: drops the
  `process_fn` callback argument. Callers must pre-register their
  pipeline functions via `$register_framework(fn)` and
  `$register_randvars(name, fn)` before calling `$process_skeletons()`.
  The new three-phase orchestration re-runs each phase only when its
  relevant part of the pipeline has changed:
  - Phase 1 (framework): full rebuild on function body/formals hash
    change.
  - Phase 3 (randvars): ordered per-step divergence-point rewind-and-
    replay. Editing one step replays that step and everything
    downstream of it; upstream steps untouched.
  - Phase 2 (codes): per-entry fingerprint diff. Adding/removing one
    code entry touches only that entry's columns on existing
    skeletons.
  For the typical "edit one ICD-10 code" workflow, this drops the
  re-run cost from a full pipeline rebuild to roughly one-Nth of
  phase 2, where N is the number of registered code entries.

* **`RegistryStudy` schema bump (3 → 4)**: existing
  `registrystudy.qs2` files with an older schema error on load via
  `$check_version()` with an actionable message pointing at
  re-running the upstream generator script.

## New features

* **`Skeleton` R6 class** (`R/r6_skeleton.R`). Public methods:
  `initialize`, `check_version`, `pipeline_hash`, `apply_code_entry`,
  `drop_code_entry`, `sync_with_registry` (phase 2 incremental diff),
  `sync_randvars` (phase 3 divergence-point rewind-and-replay),
  `save`, `print`. `drop_code_entry` uses metadata prediction via
  the new file-level `.entry_columns(reg)` helper rather than a
  runtime column map.

* **New `RegistryStudy` methods**:
  - `$register_framework(fn)` and `$register_randvars(name, fn)` for
    phase 1 / phase 3 registration.
  - `$code_registry_fingerprints()` returning xxhash64 digests of each
    `code_registry` entry's `(codes, label, groups, fn_args,
    combine_as)` tuple.
  - `$pipeline_hash()` returning a single xxhash64 over (framework
    hash, ordered randvars sequence hashes, code registry
    fingerprints). Answer to "what would a freshly-built skeleton
    look like?"
  - `$load_skeleton(batch_number)` / `$save_skeleton(sk)` as thin
    wrappers around `Skeleton$save()` that supply
    `self$data_skeleton_dir` automatically, mirroring the existing
    `$load_rawbatch()` / `$save_rawbatch()` pattern.
  - `$skeleton_pipeline_hashes()` returning a per-batch summary
    `data.table` with each skeleton's current `pipeline_hash`.
    Useful for spotting batches out of sync with each other.
  - `$assert_skeletons_consistent()` as a pre-flight check for
    downstream consumers: errors on mixed-hash or partially-rebuilt
    state.
  - `$write_pipeline_snapshot()` writing a one-row TSV to
    `{data_pipeline_snapshot_dir}/{host_label}.tsv`. Git-trackable,
    concurrency-safe (each host writes only its own file), silently
    skipped when the snapshot candidate directory is not configured
    or not mounted on the current host.
  - `$adopt_runtime_state_from(other)` copies runtime fields
    (`n_ids`, `n_batches`, `batch_id_list`, `groups_saved`) from
    another `RegistryStudy` without touching config fields. Used by
    generator scripts to reload disk state without silently adopting
    stale `group_names` or `code_registry`.

* **New `RegistryStudy` public fields**: `framework_fn`,
  `randvars_fns` (named ordered list), `host_label`,
  `data_pipeline_snapshot_cp` ([CandidatePath]).

* **New active binding `data_pipeline_snapshot_dir`** resolving from
  `data_pipeline_snapshot_cp`, parallel to the existing
  `data_rawbatch_dir` / `data_skeleton_dir` / `data_raw_dir`
  accessors.

* **File-level helpers** in `r6_registrystudy.R`: `.hash_function(fn)`
  (xxhash64 over `list(body(fn), formals(fn))` -- stable across R
  sessions because it excludes the function environment),
  `.fingerprint_entry(reg)`, `.entry_columns(reg)` (vectorized
  wrapper around the previously-private
  `.generated_columns_for_entry()` which has been lifted to file
  level), `.format_batch_range(batches)`, and
  `.process_one_batch(study, i, ...)` (the per-batch orchestration
  shared by `process_skeletons()`'s serial and callr-parallel
  branches).

## Memory footprint note

`$load_skeleton()` calls `data.table::setalloccol(obj$data,
n = getOption("datatable.alloccol", 4096L))` on the loaded
`Skeleton$data` to restore data.table over-allocation slots that qs2
serialization drops. Memory overhead is a new data.table HEADER
(~8-16 bytes per slot, so ~32-64 KB total) -- NOT a full copy of the
column data, which stays shared by reference. This is negligible
compared to the per-batch skeleton size (typically multi-GB). Without
the refresh, subsequent `:=` mutations inside helper functions would
silently reallocate and strand the R6 field pointing at the stale
old-address data.table. Studies that need more than 4096 column slots
can bump via `options(datatable.alloccol = 8192L)` at the top of
their generator script.

## Other changes

* 6 existing `process_skeletons` test cases were rewritten to use
  the new `$register_framework()` idiom.
* Added `tests/testthat/test-r6_skeleton.R` (74 tests),
  `tests/testthat/test-process_skeletons_incremental.R` (47 tests),
  `tests/testthat/test-entry_columns_parity.R` (10 tests), and 56
  new unit tests in `test-registrystudy.R` for the new methods.
* Total test count: 800 tests passing.

# swereg 26.4.18

## BREAKING CHANGES

* **`TTEPlan` schema bump (1 → 2) and new constructor signature**:
  `tteplan_from_spec_and_registrystudy()` now requires `candidate_dir_spec`,
  `candidate_dir_tteplan`, `candidate_dir_results`, and `spec_version`.
  The previous positional `spec` argument is removed — the spec YAML is
  now read from inside the resolved `candidate_dir_spec` using
  `filename_spec(spec_version)`. Existing `_plan.qs2` files error on load
  via `check_version()`; regenerate by re-running `s0_init.R` for each
  project.

* **`RegistryStudy` schema bump (2 → 3)**: private
  `.data_{rawbatch,skeleton,raw}_dir_candidates` and `_cache` fields are
  replaced by public `data_{rawbatch,skeleton,raw}_cp` fields of type
  `CandidatePath`. Existing `registry_study_meta.qs2` files error on load;
  regenerate by re-running `run_generic_create_datasets_v2.R`.

* **Stub-free on-disk filenames**. Project-scoped directories no longer
  need a `{project_id}_` prefix on files. The rename is:
  - `registry_study_meta.qs2` → `registrystudy.qs2`
  - `{project_id}_plan.qs2` → `tteplan.qs2`
  - `{project_id}_spec.xlsx` → `spec.xlsx`
  - `{project_id}_tables.xlsx` → `tables.xlsx`
  - `study_spec_vXXX.yaml` → `spec_vXXX.yaml`
  Use `dev/rename_r6_files.sh` in the downstream repository for the on-disk
  migration.

## New features

* **`CandidatePath` R6 class** (`R/r6_candidate_path.R`). First-class
  representation of "a directory that lives at one of several candidate
  locations depending on host". Owns its candidate list, its resolution
  cache, and its `$resolve()` / `$invalidate()` / `$is_resolved()` /
  `$print()` methods. Both `RegistryStudy` and `TTEPlan` now hold
  `CandidatePath` instances via public `*_cp` fields, so multi-host path
  resolution is structurally identical across classes — cannot drift.

* **`first_existing_path(candidates, label)`** (exported). Generic,
  study-agnostic "first existing path" picker lifted from the old
  `.resolve_path()`. Auto-creates the first candidate whose parent
  directory exists (unchanged from the old behavior).

* **`invalidate_candidate_paths(obj)`** (exported). Walks an R6 object's
  public fields and calls `$invalidate()` on every `CandidatePath` it
  finds, recursing into embedded R6 objects. Called by `RegistryStudy$save_meta()`
  and `TTEPlan$save()` before serialization so on-disk files never carry
  the saving host's cached resolved paths.

* **`tteplan_locate_and_load(candidate_dir_tteplan)`** (exported). Stage
  scripts (`s1.R`, `s2.R`, `s3.R`, `s4_export.R`) use this one-liner to
  load a `tteplan.qs2` from the first candidate directory that exists.

* **`registrystudy_load(candidate_dir_rawbatch)`** (exported). Paired
  with `tteplan_from_spec_and_registrystudy()` so `s0_init.R` reads
  `registrystudy.qs2` from the first rawbatch directory that exists on
  the current host.

* **`TTEPlan` active bindings for owned paths**: `dir_tteplan`, `dir_spec`,
  `dir_results_base`, `dir_results` (appends `spec_version`), `tteplan`,
  `spec_path`, `spec_xlsx`, `tables_xlsx`, plus `data_skeleton` and
  `data_rawbatch` that delegate to the embedded `registrystudy` (no
  duplication). Stage methods default to these bindings, so `s1/s2/s3`
  no longer need an explicit `output_dir =`.

* **Host-portable `skeleton_files`** after load. `tteplan_load()` now
  refreshes `plan$skeleton_files` from `plan$registrystudy$skeleton_files`
  on every load, reapplying the `n_skeleton_files_limit` stored on the
  plan. A plan saved on one host and loaded on another immediately points
  at the current host's skeleton files.

# swereg 26.4.17

## New features

* **Custom Table 1 engine**: `swereg` now ships its own Table 1 builder
  (`.swereg_table1()`), replacing the optional `tableone` dependency.
  `TTEEnrollment$table1()` returns a long-format `data.table` with class
  `swereg_table1` and supports new arguments:
  - `arm_labels` — display labels for the two exposure arms
  - `include_smd` — toggle SMD column (defaults TRUE)
  - `show_missing` — annotate variable names with `"(missing X.X%)"` instead
    of emitting a separate `Missing` row (defaults TRUE).
  Percentages for non-missing levels are computed against the non-missing
  denominator, so they sum to 100 within each column. Multi-level
  categorical SMDs follow the Yang & Dalton (2012) generalisation.

* **Forest plot for Table 3**: the workbook's Table 3 sheet is now a forest
  plot rendered with `ggplot2`. Supplemental `Table S{n+1}` keeps the full
  tabular IRR for all ETTs (per-protocol truncated). The forest plot is
  delivered as a high-resolution PNG (300 dpi) embedded in the worksheet,
  plus a vector PDF sidecar saved next to the workbook.

* **CONSORT flowcharts**: `.write_consort()` now renders a Graphviz DOT
  flowchart via `DiagrammeR` + `DiagrammeRsvg` + `rsvg`, embeds the PNG into
  the worksheet, and saves PNG/PDF sidecars next to the workbook. Falls back
  to the legacy text-table layout when the optional packages are missing.

* **`featured_etts` argument** on `TTEPlan$export_tables()`: filters Tables 2
  and 3 to a user-specified subset of ETT ids; the supplementary tabular IRR
  remains unfiltered. An "Exposure definitions" legend block is written
  above each main table, and the `_Exposed`/`_Unexposed` column suffixes are
  rewritten to spec-derived arm labels when all featured ETTs share a
  single enrollment.

* **`TTEPlan$reload_spec()`**: refreshes cosmetic spec fields (study title,
  enrollment names, exposure-arm labels, outcome names, ETT descriptions)
  from a YAML spec on disk WITHOUT re-running the upstream pipeline.
  Structural changes (confounders, exclusion criteria, follow-up windows,
  matching parameters, etc.) are detected and reported via a loud warning
  but NOT applied — cached results stay bound to the old definitions. The
  new fields `spec_reloaded_at` and `spec_reload_skipped_diffs` are surfaced
  on the Provenance sheet.

* **`TTEPlan$recompute_baselines()`**: re-runs the new Table 1 engine on
  cached enrollment files in-process, used to refresh stale baseline tables
  after upgrading swereg without re-running `$s3_analyze()`.
  `$export_tables()` calls this lazily when it detects pre-26.4.17 cached
  Table 1 results.

## Workbook output changes

* Supplementary baseline panels are renamed:
  - `Raw` → `Unimputed and unweighted`
  - `Unweighted` → `Imputed and unweighted`
  - `IPW` → `Imputed and IPW`
  - `IPW Truncated` → `Imputed and IPW truncated`
* The main Table 1 sheet hides the SMD column and missingness annotations;
  supplementary panels include both.

## Breaking changes

* `TTEEnrollment$table1()` now returns a `data.table` (class
  `c("swereg_table1", "data.table", "data.frame")`) instead of a `tableone`
  S3 object. Code that introspected `TableOne` fields will need to read from
  the long-format columns instead.
* `tableone` is removed from `Suggests`. Cached `results_enrollment` lists
  produced by older versions are recognised and refreshed lazily on the next
  `$export_tables()` call (an `output_dir` is required for the refresh).
* `ggplot2` is now in `Imports` (was `Suggests`) since the forest plot is a
  mandatory part of `$export_tables()`.
* `DiagrammeR`, `DiagrammeRsvg`, and `rsvg` are added to `Suggests`. They
  are required for CONSORT flowcharts; without them `$export_tables()`
  silently falls back to the legacy text CONSORT.

# swereg 26.4.16

## Improvements

* `setup_progress_handlers()`: Pick `handler_progress()` format based on `interactive()`. In interactive sessions use `\r`-based single-line repaint (`clear = TRUE`, no trailing newline) so the bar updates in place like a normal terminal progress bar. In non-interactive sessions (RStudio background jobs, Rscript, CI) use a trailing `\n` with `clear = FALSE` so each step is a new line in the log.

# swereg 26.4.15

## Improvements

* `RegistryStudy$process_skeletons()`: Pass the current timestamp as the progress `message` (both sequential and parallel paths), matching the convention already used in `parallel_pool()`. The `(last: :message)` suffix in the `setup_progress_handlers()` format string now shows the clock time of the last completed batch (e.g. `(last: 14:35:22)`) so you can tell at a glance whether the job is making progress or frozen. Previously called `p()` with no message, so `(last: )` was always blank.

# swereg 26.4.14

## Bug Fixes

* `setup_progress_handlers()`: The real reason progress never showed up in RStudio background jobs -- `progressr` silently suppresses reporting in non-interactive sessions unless you set `options("progressr.enable" = TRUE)`. Background jobs have `interactive() == FALSE`, so the global handler was being installed correctly but `progressor()` calls were emitting no output. Now forces the option on. Also restores `(last: :message)` in the format so you can tell the bar isn't frozen by watching the item label advance.

# swereg 26.4.13

## Improvements

* `setup_progress_handlers()`: Drop the `handler_rstudio` / rstudioapi branch entirely. Use `handler_progress()` with `format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta\n"` and `clear = FALSE` in every context — same recipe as `cs9::set_progressr` and `plnr::Plan$run_all`. The trailing `\n` makes each update a new log line (instead of a `\r` repaint that job logs can't render), and `clear = FALSE` keeps finished bars in the scrollback. Works in interactive R, RStudio's foreground console, *and* RStudio background-job subprocesses without any detection logic or handler switching. `handler_rstudio` has been ineffective for the background-job case in this codebase.

# swereg 26.4.12

## Improvements

* `setup_progress_handlers()`: Collapse the RStudio-detection logic to a single `rstudioapi::isAvailable(child_ok = TRUE)` call. The `child_ok = TRUE` parameter handles both the foreground RStudio console and background-job subprocesses (via IPC to the parent session), so all the earlier `hasFun`/`exists`/`isJob` gymnastics were unnecessary. Also drops the now-redundant feature-test for `jobAdd`/`jobSetProgress`/`jobRemove` — `progressr::handler_rstudio` requires those to exist anyway.

# swereg 26.4.11

## Bug Fixes

* `setup_progress_handlers()`: Stop using `rstudioapi::hasFun("jobAdd")` to feature-test for the job wrappers. `hasFun()` (a) short-circuits to `FALSE` whenever `isAvailable()` is `FALSE`, and (b) looks the name up in the internal `rstudio` namespace where the function is actually called `addJob` (the `rstudioapi::jobAdd()` wrapper forwards to `callFun("addJob", ...)`). So `hasFun("jobAdd")` returned `FALSE` even on systems where `jobAdd()` works fine, which caused the helper to fall through to the text handler every time. Now checks the `rstudioapi` wrapper namespace directly via `exists("jobAdd", envir = asNamespace("rstudioapi"), mode = "function")`.

# swereg 26.4.10

## Bug Fixes

* `setup_progress_handlers()`: Fix detection of RStudio background-job subprocesses. Previously relied on `rstudioapi::isAvailable()`, which returns FALSE inside a `jobRunScript()` subprocess (because `.Platform$GUI` is not "RStudio" there) — so scripts launched via *Source as Background Job* fell through to the text handler and no Jobs-pane progress bar appeared. Now also accepts `rstudioapi::isJob()` as a valid context; in job subprocesses, `rstudioapi::callFun()` auto-delegates `jobAdd`/`jobSetProgress`/`jobRemove` back to the parent RStudio session via IPC, so `handler_rstudio` works correctly.

# swereg 26.4.9

## New Features

* `setup_progress_handlers()`: Helper for run scripts. Feature-detects `rstudioapi::jobAdd()` and installs `progressr::handler_rstudio()` when available, else falls back to `handler_progress()`. Fixes the "no good progress bar" problem when launching run scripts via RStudio's *Source as Background Job* menu — the default text bar renders badly in job logs, the RStudio handler draws a proper Jobs-pane progress bar. Automatically covers every progressr-emitting method (`process_skeletons`, `s1_*`, `s2_*`, `s3_*`) with no per-method changes.

# swereg 26.4.8

## New Features

* `RegistryStudy$compute_population(by)`: Compute a population denominator table from saved skeleton files. Counts unique persons by `isoyear` and user-specified structural variables (e.g. sex, age, register tag). Handles both annual and weekly skeleton rows via `uniqueN(id)`. Produces a complete grid with all combinations (missing cells filled with zero). Saves result as `population.qs2` in the skeleton directory.

# swereg 26.4.7

## Improvements

* `s3_analyze()` now prints the output directory path and count of .qs2 files before processing.
* `s3_analyze()` gains `ett_ids` parameter to run only specific ETTs (e.g. `ett_ids = "ETT01"`).
* Remove heterogeneity test (`het_test`) from `s3_analyze()` — a single call consumed 42GB RAM and 40+ minutes CPU on real data, making full runs infeasible.
* Remove `het_slot` parameter from `tteenrollment_irr_combine()` (no longer needed).

# swereg 26.4.6

## Bug Fixes

* Fix latent bug in `parallel_pool()`: `Filter(Negate(is.null), results)` removed NULLs and shifted indices, breaking positional `item_map` indexing in `s3_analyze`. Workers that fail already raise errors before producing output, so NULL results cannot occur.

## Internal

* Deduplicate `.write_combined_rates()` and `.write_combined_irr()` into shared helper `.prepare_combine_data()`.
* Extract `.build_code_lookup()` helper for code_lookup + fmt_var construction, shared by `print_spec_summary()` and `.write_spec_summary()`.
* Call `parallel::detectCores()` once at top of `s3_analyze()` instead of twice in the enrollment and ETT loops.

# swereg 26.4.5

## New Features

* **`$s3_analyze()`**: New Loop 3 method on TTEPlan that computes all analysis results (baseline characteristics, rates, IRR, heterogeneity tests) and stores them on the plan. Split into enrollment-level results (table1 variants) and ETT-level results (outcome-specific). Degenerate ETTs (GLM failure) are caught and stored with a skip reason instead of crashing. Progress bars via progressr.
* **`$results_summary()`**: Print diagnostic table showing event counts and IRR/rates status per ETT.
* **`$export_tables(path)`**: Export all results to a multi-sheet Excel workbook: enrollment overview, ETT overview, Table 1 (chosen enrollment), Tables 2-3 (combined rates/IRR), per-enrollment combined baselines (4-panel: Raw/Unweighted/IPW/IPW Truncated), CONSORT attrition sheets, supplemental rates/IRR.
* **`tteplan_load(path)`**: Load a TTEPlan from disk with the current class definition, ensuring new methods are available on old serialized objects.
* `$s1_generate_enrollments_and_ipw(resume = TRUE)` and `$s2_generate_analysis_files_and_ipcw_pp(resume = TRUE)` skip completed work based on file timestamps (must be <24h old).

## Bug Fixes

* Fix `'from' must be of length 1` crash in `enroll()` when a skeleton file has no enrolled persons for the current enrollment. data.table evaluates `j` once on 0-row data even with `by`, giving by-variables length 0 instead of scalar. This also produced spurious `-Inf` warnings from `max(logical(0), na.rm = TRUE)` in Phase B. Fix: short-circuit `enroll()` with an empty panel when `entry_dt` has 0 rows.
* IPCW GLM fallback: `fit_and_predict()` in `s6_ipcw_pp` now uses tryCatch; falls back to marginal censoring rate when the model fails (e.g. near-zero events).

# swereg 26.4.3

## Breaking Changes

* `parallel_pool()` rewritten to use `processx` + qs2 tempfiles instead of `future.callr`. Worker logic moved to standalone R scripts in `inst/` (`worker_s1a.R`, `worker_s1b.R`, `worker_s2.R`), launched via `processx::process$new()`. All data passes through qs2 files on disk instead of R's IPC serialization, fixing the loop 1b bottleneck where `enrolled_ids` was serialized N times through pipe buffers. `enrolled_ids` is now written once to a shared tempfile. Dependencies `future`, `future.apply`, `future.callr` removed; `processx` added.

# swereg 26.4.2

## Breaking Changes

* `parallel_pool()` rewritten to use `future.callr` instead of persistent `callr::r_session` workers. Each work item now runs in a fresh R subprocess, eliminating deadlocks caused by accumulated IPC socket state. New dependencies: `future`, `future.apply`, `future.callr`. The `processx` dependency is removed. `callr_kill_workers()` is removed (no longer needed).

## Internal

* Rename `.s2_worker()` (was `.s3_worker()`) to match `$s2_generate_analysis_files_and_ipcw_pp()`.

# swereg 26.3.30

## Improvements

* `callr_pool()` gains a `timeout_minutes` parameter (default: 30). If a work item runs longer than the timeout, its worker is killed and the item is retried once. If the retry also times out, `callr_pool()` calls `stop()`. Disable with `timeout_minutes = NULL`.

## CRAN compliance

* Move `mgcv` from Imports to Suggests (only used conditionally via `requireNamespace()`).
* Add `@importFrom` for `progressr` and `utils::getFromNamespace` to satisfy NAMESPACE checks.
* Replace `swereg:::` calls with `getFromNamespace()` in callr worker sessions.
* Replace `assign(..., globalenv())` with a package-level environment (`.swereg_env`).
* Add `var <- NULL` declarations for all data.table NSE variables.
* Add `.vscode` to `.Rbuildignore`.

# swereg 26.3.23

## Improvements

* `callr_pool()` workers now self-terminate if the parent R session dies (e.g. OOM kill). Each worker spawns a lightweight shell watchdog that polls the parent PID every 5 seconds. Previously, orphaned workers ran indefinitely until manually cleaned up via `callr_kill_workers()`.

## Bug Fixes

* **Critical**: `.s1_eligible_tuples()` used `first(rd_exposed)` to classify exposure at each trial period, which only detected treatment initiation if it happened on the first week of a 4-week trial period. With `period_width = 4`, ~75% of exposed people start treatment mid-period and were silently dropped — their first trial period showed them as unexposed (week 1 was pre-initiation), and the next period excluded them for prior treatment. Fixed by using `any(rd_exposed, na.rm = TRUE)` instead. The existing `no_prior_exposure` exclusion correctly handles the new-user restriction. Verified: eligible exposed count on skeleton_001 went from 19 → 84, matching the old per-week pipeline.

* `.s1_compute_attrition()`: exposure classification now uses `any()` per person-trial instead of checking the first eligible row. Aligns attrition reporting with the `any()` fix in `.s1_eligible_tuples()` — previously the attrition flow underreported exposed counts by ~4x.

* `tteplan_validate_spec()`: missing variables (confounders, outcomes, exclusion criteria, exposure) now `stop()` instead of `warning()`. Previously, a misspelled or renamed variable would silently pass validation and break downstream (e.g. IPW model missing a confounder). Category mismatches (values in spec but not data) remain as warnings since they can occur in small batches.

# swereg 26.3.20

## Bug Fixes

* `.s1_compute_attrition()`: fix undercounting of person-trials for row-level eligibility criteria (e.g. `eligible_valid_exposure`). The old code checked only the first row per person-trial, missing cases where exposure onset occurred after the first week. The new approach filters to eligible rows first, then counts — matching the logic used by `.s1_eligible_tuples()`.

* `.s1_compute_attrition()`: fix negative exposed/comparator deltas in participant flow. The `before_exclusions` baseline now classifies exposure from the first row with non-NA exposure per person-trial, rather than the first overall row (which often has `rd_exposed = NA`). Total person-trial counts remain unfiltered.

## Performance

* TTE s1 pipeline: add `data.table::setkey()` calls to eliminate redundant hash-based grouping. Skeleton reads in `.s1_prepare_skeleton()` and `.s1b_worker()` now set key on `(id, isoyearweek)` (metadata-only, no re-sort). `enroll()` Phase B collapse uses keyed grouping on `(pid, trial_id)`, and Phase D panel expansion uses keyed binary join instead of `merge()`.

## Bug Fixes

* `callr_pool()` PID files now written to `/tmp` instead of `tempdir()` so that orphaned workers from crashed R sessions can be discovered and cleaned up by new sessions.

* `callr_kill_workers()` simplified to orphan-only cleanup: kills workers whose parent R process is dead and removes stale PID files. Own-session cleanup is already handled by `callr_pool()`'s `on.exit()` handler; this function is only needed after hard crashes (SIGKILL, OOM).

## Performance

* `callr_pool()` now uses persistent `callr::r_session` workers instead of spawning a fresh `callr::r_bg()` process per work item. The swereg namespace is loaded once per worker slot rather than once per item, eliminating redundant startup overhead when scaling to large numbers of items.

* Orphan protection: `callr_pool()` writes a PID file per invocation and cleans up orphaned worker sessions from previous crashed runs (e.g. OOM kills) on the next invocation.

## Bug Fixes

* Fixed 3 test failures in `test-tte_spec.R` caused by s1 pipeline changes: added missing `rd_exposed` column to `.s1_compute_attrition` test fixtures, added `n_exposed`/`n_unexposed` to mock attrition data, and updated matching output expectations.

## Performance

* `s1_generate_enrollments_and_ipw()` now caches prepared skeletons between s1a (scout) and s1b (enrollment) passes, eliminating redundant file reads and exclusion processing. Expected ~30-40% reduction in per-enrollment wall-clock time.

* `.s1b_worker()` now subsets the skeleton to enrolled persons before computing derived confounders, avoiding expensive rolling-window operations on non-enrolled persons.

* `TTEEnrollment$new()` accepts `own_data = TRUE` to skip the defensive `data.table::copy()` when the caller will not reuse the data. Used in `.s1b_worker()` where the skeleton is discarded immediately after.

* `enroll()` Phase B now aggregates confounders, time-exposure, and outcome columns in a single groupby pass instead of four separate passes with merges.

## Improvements

* "Valid exposure" (`eligible_valid_exposure`) is now the first exclusion criterion in the TTE attrition flow. Rows where `rd_exposed` is NA are explicitly accounted for rather than silently disappearing between the before-exclusions total and the first real criterion.

* TARGET Item 8 (participant flow) now shows a richer flow diagram with before-exclusion counts, per-step exposed/unexposed breakdown, delta (excluded) and remaining counts at each criterion, right-justified aligned columns, and color-coded output (red for exclusions, cyan for remaining). Post-matching line also reformatted with arrow indicator. "Before exclusions" line no longer shows a meaningless exposed/comparator breakdown.

* `enrollment_counts$attrition` now includes `n_exposed` and `n_unexposed` columns and a `"before_exclusions"` row.

## Bug Fixes

* Fixed `trial_id` missing error caused by `attr<-` breaking data.table's internal self-reference. Replaced with `data.table::setattr()` in `.s1_prepare_skeleton()` and `tteplan_apply_exclusions()` to preserve in-place modification semantics.

* Fixed callr worker stale-namespace bug: after `devtools::load_all()` in a subprocess, worker functions still referenced the old (installed) swereg namespace. Now rebinds the worker function's environment to the freshly-loaded namespace.

## Improvements

* Reorganized `print_spec_summary()` header layout: renamed "Study created" → "RegistryStudy", merged "Skeletons created" + "Skeleton files" into a single nested line with tree connector, renamed "Plan created" → "TTEPlan", and reordered to follow data pipeline order.

* Rewrote TARGET checklist items 6c, 6h, and 7a-h in `print_target_checklist()` as academic prose suitable for copy-pasting into a methods section. Item 6c now dynamically reflects per-enrollment matching ratios from the spec.

## Breaking changes

* **`enrollment_counts` structure changed**: Each element of
  `TTEPlan$enrollment_counts` is now a list with `$attrition` and `$matching`
  sub-elements (was a single data.table). Code accessing
  `plan$enrollment_counts[["01"]]` directly as a data.table must update to
  `plan$enrollment_counts[["01"]]$matching`.

* **`person_trial_id` renamed to `enrollment_person_trial_id`**: The composite
  key column now has a 3-part name matching its 3-part format
  (`enrollment_id.person_id.trial_id`). All code referencing `person_trial_id`
  must be updated.

* **`process_fn` parameter removed from `$s1_generate_enrollments_and_ipw()`**:
  The two-pass spec-driven pipeline is now the only code path. `self$spec` is
  required (create plans with `tteplan_from_spec_and_registrystudy()`). The
  legacy single-pass `.s1_worker()` has been deleted.

* **`.s2_worker()` renamed to `.s3_worker()`**: Internal Loop 2 IPCW-PP worker
  renamed to avoid confusion with the two-pass Loop 1 pipeline.

## New features

* **Two-pass enrollment pipeline**: `$s1_generate_enrollments_and_ipw()` now
  uses a two-pass pipeline that fixes cross-batch matching ratio imbalance:
  1. **Pass 1a (scout)**: Lightweight parallel pass collecting eligible
     `(person_id, trial_id, exposed)` tuples from all batches.
  2. **Centralized matching**: Combines all tuples and performs per-`trial_id`
     matching globally, ensuring the correct ratio across all batches.
  3. **Pass 1b (full enrollment)**: Parallel pass using pre-matched IDs to
     enroll without per-batch matching.

* **`enrollment_counts` on TTEPlan**: New field storing per-trial matching
  counts (total vs enrolled, exposed vs unexposed) for TARGET Item 8 reporting.

* **`.assign_trial_ids()`**: New shared helper function that is the single
  source of truth for `isoyearweek -> trial_id` mapping. Used consistently
  by both scout (s1a) and enrollment (s1b/enroll) phases.

* **`enrolled_ids` parameter on `TTEEnrollment$new()`**: When provided,
  enrollment skips the matching phase and uses pre-decided IDs directly,
  enabling the two-pass pipeline.

* **Per-criterion attrition counts for TARGET Item 8**: The scout pass (s1a)
  now computes cumulative person and person-trial counts at each eligibility
  step. Stored in `plan$enrollment_counts[["01"]]$attrition` as a long-format
  data.table with columns `trial_id`, `criterion`, `n_persons`,
  `n_person_trials`. `$print_target_checklist()` Item 8 auto-populates with
  these counts when available.

# swereg 26.3.21

## New features

* **`$heterogeneity_test()`**: New method on `TTEEnrollment` that tests for
  heterogeneity of treatment effects across trials via a Wald test on the
  `trial_id × exposure` interaction (Hernán 2008, Danaei 2013).

* **`$print_target_checklist()`**: New method on `TTEPlan` that generates a
  self-contained TARGET Statement (Cashin et al., JAMA 2025) 21-item reporting
  checklist. Auto-populates items from the study spec and provides `[FILL IN]`
  placeholders for PI completion.

## Improvements

* **`$irr()` calendar-time adjustment**: Outcome model now includes `trial_id`
  as a covariate to adjust for calendar-time variation in outcome rates across
  enrollment bands (Caniglia 2023, Danaei 2013). Uses `ns(trial_id, df=3)` for
  ≥5 unique trial IDs, linear term for 2-4, omitted for 1.

* **`$irr()` IPW-only guard**: `$irr()` now rejects IPW-only weight columns
  (`ipw`, `ipw_trunc`) after per-protocol censoring has been applied. The swereg
  pipeline applies per-protocol censoring in `$s4_prepare_for_analysis()`, so
  only per-protocol weights (`analysis_weight_pp_trunc`) are valid for the
  censored dataset.

## Documentation

* **Methodology vignette**: New `vignette("tte-methodology")` maps the swereg
  TTE implementation to five reference papers (Hernán 2008/2016, Danaei 2013,
  Caniglia 2023, Cashin 2025). Documents which methods are implemented, which
  are not, and design rationale.

* **Analysis types**: `vignette("tte-nomenclature")` now documents that swereg
  supports **per-protocol** analysis only. ITT analysis is not supported because
  the pipeline censors at protocol deviation. As-treated analysis requires
  time-varying IPW (not implemented).

* **`period_width` documentation**: `vignette("tte-nomenclature")` now explains
  the enrollment band width / residual immortal time bias trade-off, citing
  Caniglia (2023) and Hernán (2016).

* **Matching approach**: `vignette("tte-nomenclature")` now documents the
  per-band stratified matching design choice and alternatives from the literature.

* **`$s2_ipw()` documentation**: Clarified that IPW estimates the propensity
  score for baseline treatment assignment only, not time-varying treatment
  weights.

* **`$irr()` documentation**: Documented IRR ≈ HR for rare events,
  `ns(tstop)` for flexible baseline hazard, `quasipoisson` for overdispersion,
  and computational equivalence to pooled logistic regression.

* **IPCW stabilization**: Documented the simplified marginal stabilization
  approach and its relationship to Danaei (2013).

## Tests

* Added tests for `$rates()`, `$irr()`, `$km()`, `$irr()` with `trial_id`,
  IPW-only guard, and IPCW formula with `trial_id`.

# swereg 26.3.20

## Improvements

* **Band-based enrollment**: Added explicit `isoyearweek` ordering before
  band-level collapse to prevent silent misclassification when input data
  is not pre-sorted by time.
* **IPCW-PP**: Censoring model now includes `trial_id` to account for
  calendar-time variation in censoring patterns across enrollment bands.
* **`person_weeks`**: Now computed from actual source row counts during
  band collapse instead of hardcoded `period_width`. Partial-coverage
  bands (e.g., at data boundaries) now contribute accurate person-time.

## Breaking changes

* **`$irr()`**: Removed the constant (no time adjustment) Poisson model. Only
  the flexible model with natural splines (`splines::ns(tstop, df=3)`) is
  retained. Output columns renamed: `IRR_flex` → `IRR`, `IRR_flex_lower` →
  `IRR_lower`, `IRR_flex_upper` → `IRR_upper`, `IRR_flex_pvalue` →
  `IRR_pvalue`, `warn_flex` → `warn`. All `IRR_const*` and `warn_const`
  columns removed.
* **`tteenrollment_irr_combine()`**: Updated to match new `$irr()` output.
  Columns renamed: `IRR (flexible)` → `IRR`, `95% CI (flexible)` → `95% CI`,
  `p (flexible)` → `p`. Constant-model columns removed.
* **TTE ID semantics**: The composite person-per-trial identifier column is now
  called `person_trial_id` (was `trial_id`). The actual trial identifier (the
  enrollment band) is now exposed as `trial_id` in enrollment output. This fixes
  the semantics so `trial_id` means the trial and `person_trial_id` identifies a
  person's participation in a trial.
* **TTEDesign default**: `id_var` default changed from `"trial_id"` to
  `"person_trial_id"`.
* **`s1_impute_confounders()`**: No longer hardcodes `trial_id`; uses
  `design$id_var` throughout.

## Code quality

* Rename private methods `prepare_outcome` and `ipcw_pp` to `s5_prepare_outcome`
  and `s6_ipcw_pp` to signal their execution order within `s4_prepare_for_analysis()`.
* Reorder `TTEEnrollment` public step methods to match their numeric sequence
  (s1 before s2).

## Breaking changes

* **Band-based enrollment**: `TTEEnrollment` enrollment now uses N-week bands
  (controlled by `period_width` in `TTEDesign`, default 4). Calendar time is
  grouped into bands based on `isoyearweek`, matching is done per-band
  (stratified), and data is collapsed to band level during enrollment. This
  eliminates the separate `$s1_collapse()` step entirely.

* **Step renumbering**: Public workflow methods on `TTEEnrollment` have been
  renumbered after removing `$s1_collapse()`:
  - `$s2_impute_confounders()` -> `$s1_impute_confounders()`
  - `$s3_ipw()` -> `$s2_ipw()`
  - `$s4_truncate_weights()` -> `$s3_truncate_weights()`
  - `$s5_prepare_for_analysis()` -> `$s4_prepare_for_analysis()`

* **`period_width` parameter**: Moved from `TTEPlan$s1_generate_enrollments_and_ipw()`
  to `TTEDesign$new(period_width = 4L)`. Now part of the design contract.

* **`isoyearweek` column required**: Band-based enrollment requires an
  `isoyearweek` column in person-week data.

* **Schema version bump**: `TTEDesign` and `TTEEnrollment` schema versions
  bumped to 2. Objects saved with version 1 will warn on load.

## New features

* **TTEPlan provenance timestamps**: TTEPlan now tracks `created_at` (stamped
  at construction), `registry_study_created_at` (from the source RegistryStudy),
  and `skeleton_created_at` (from the first skeleton file's attribute). All three
  timestamps are shown in `print()` and `print_spec_summary()` when available,
  making it easy to detect stale plans.

* **R6 schema versioning**: All R6 classes (`RegistryStudy`, `TTEPlan`,
  `TTEDesign`, `TTEEnrollment`) now carry a `.schema_version` private field,
  stamped at construction time. A new `$check_version()` public method
  compares the stored version against the current class definition and warns
  when stale. `qs2_read()` automatically calls `$check_version()` on R6
  objects after loading, so outdated serialized objects produce a clear warning
  instead of silently breaking.

* **Deprecation warnings for old `add_*` parameter names**: `add_diagnoses(diags=)`,
  `add_operations(ops=)`, `add_rx(rxs=)`, `add_icdo3s(icdo3s=)`,
  `add_snomed3s(snomed3s=)`, and `add_snomedo10s(snomedo10s=)` now emit a
  deprecation warning when the old parameter name is used. Use `codes=` instead.

## Breaking changes

* **RegistryStudy**: `register_codes()` now takes a declarative signature:
  `register_codes(codes, fn, groups, fn_args, combine_as)`. Each call declares
  codes, the function to apply them, which data groups to use, and optional
  prefix/combine behavior. The five old per-type code fields are removed,
  together with the old `register_codes(icd10_codes = ...)` signature. The
  single `code_registry` list field replaces them.

* **`summary_table()`**: The `type` parameter is removed. The `type` column is
  replaced by `label`. Use `label` to filter.

* **`add_diagnoses()`**, **`add_operations()`**, **`add_rx()`**,
  **`add_icdo3s()`**, **`add_snomed3s()`**, **`add_snomedo10s()`**: The codes
  parameter is renamed to `codes` (was `diags`, `ops`, `rxs`, `icdo3s`,
  `snomed3s`, `snomedo10s`). Old parameter names still work for backwards
  compatibility.

## Refactoring

* Moved `qs2_read()` to its own file (`R/qs2.R`) and inlined the fallback
  logic directly. Removed pointless `.qs_save` wrapper (replaced with direct
  `qs2::qs_save` calls) and `.qs_read` internal helper.

## Breaking changes

* `skeleton_save()` no longer splits batches into sub-files. It saves one file
  per batch as `skeleton_NNN.qs2` (was `skeleton_NNN_SS.qs2`). The
  `ids_per_file` and `id_col` parameters have been removed.

* `RegistryStudy`: `batch_sizes` parameter (integer vector) replaced with
  `batch_size` (single integer, default 1000). The `ids_per_skeleton_file`
  parameter has been removed. All batches are now uniform size.

# swereg 26.3.21

## Breaking changes

* **RENAMED**: Standalone TTE functions renamed to signal which class they
  operate on:
  - `tte_rbind()` → `tteenrollment_rbind()`
  - `tte_rates_combine()` → `tteenrollment_rates_combine()`
  - `tte_irr_combine()` → `tteenrollment_irr_combine()`
  - `tte_impute_confounders()` → `tteenrollment_impute_confounders()`
  - `tte_read_spec()` → `tteplan_read_spec()`
  - `tte_apply_exclusions()` → `tteplan_apply_exclusions()`
  - `tte_apply_derived_confounders()` → `tteplan_apply_derived_confounders()`
  - `tte_validate_spec()` → `tteplan_validate_spec()`
  - `tte_plan_from_spec_and_registrystudy()` → `tteplan_from_spec_and_registrystudy()`
  - `tte_callr_pool()` → `callr_pool()`

* **RENAMED**: Eligibility helpers renamed from `tte_eligible_*` to
  `skeleton_eligible_*` to reflect that they operate on skeleton data.tables,
  not TTE classes:
  - `tte_eligible_isoyears()` → `skeleton_eligible_isoyears()`
  - `tte_eligible_age_range()` → `skeleton_eligible_age_range()`
  - `tte_eligible_no_events_in_window_excluding_wk0()` → `skeleton_eligible_no_events_in_window_excluding_wk0()`
  - `tte_eligible_no_observation_in_window_excluding_wk0()` → `skeleton_eligible_no_observation_in_window_excluding_wk0()`
  - `tte_eligible_no_events_lifetime_before_and_after_baseline()` → `skeleton_eligible_no_events_lifetime_before_and_after_baseline()`
  - `tte_eligible_combine()` → `skeleton_eligible_combine()`

## File reorganization

* **RENAMED**: `R/tte_enrollment_r6.R` → `R/r6_tteenrollment.R`
* **RENAMED**: `R/tte_plan_r6.R` → `R/r6_tteplan.R`
* **RENAMED**: `R/registry_study_r6.R` → `R/r6_registry_study.R`
* **EXTRACTED**: `callr_pool()` to its own file `R/callr_pool.R`
* **MOVED**: Eligibility helpers to `R/skeleton_utils.R`
* **MOVED**: `tteenrollment_impute_confounders()` to `R/r6_tteenrollment.R`

# swereg 26.3.20

## Breaking changes

* **RENAMED**: TTEEnrollment public workflow methods now have step-number
  prefixes to signal execution order:
  - `$collapse()` → `$s1_collapse()`
  - `$impute_confounders()` → `$s2_impute_confounders()`
  - `$ipw()` → `$s3_ipw()`
  - `$truncate()` → `$s4_truncate_weights()`
  - `$prepare_for_analysis()` → `$s5_prepare_for_analysis()`

* **RENAMED**: `$s4_truncate()` → `$s4_truncate_weights()` for clarity.

* **RENAMED**: TTEPlan orchestration methods now have step-number prefixes:
  - `$generate_enrollments_and_ipw()` → `$s1_generate_enrollments_and_ipw()`
  - `$generate_analysis_files_and_ipcw_pp()` → `$s2_generate_analysis_files_and_ipcw_pp()`

* **RENAMED**: Internal worker functions for consistent naming:
  - `.tte_process_skeleton()` → `.s1_worker()`
  - `.loop2_worker()` → `.s2_worker()`

* **REMOVED**: Constructor wrapper functions `tte_design()`, `tte_enrollment()`,
  and `tte_plan()`. Use `TTEDesign$new()`, `TTEEnrollment$new()`, and
  `TTEPlan$new()` directly. The auto-detection and data-copy logic from
  `tte_enrollment()` has been moved into `TTEEnrollment$new()`.

## Improvements

* **REFACTOR**: Inlined 5 of 6 private helper methods into their single
  callers on TTEEnrollment (`.calculate_ipw`, `.calculate_ipcw`,
  `.combine_weights_fn`, `.match_ratio`, `.collapse_periods`). Kept
  `.truncate_weights` as private (used in 2 places). Reduces indirection
  for stateless methods that don't use `self`.

* **TESTS**: Rewrote `test-tte_weights.R` to test through public API
  (`$s1_collapse()`, `$s3_ipw()`, `$s4_truncate()`, `tte_enrollment(ratio=)`)
  instead of accessing inlined private methods.

# swereg 26.3.20

## Improvements

* **REFACTOR**: Inlined 6 weight/matching functions as private methods on
  TTEEnrollment (tte_truncate_weights, tte_calculate_ipw, tte_calculate_ipcw,
  tte_combine_weights, tte_match_ratio, tte_collapse_periods). Removed 2
  orphaned functions (tte_identify_censoring, tte_time_to_event). Users access
  this functionality through R6 methods ($collapse, $ipw, $truncate, etc.).

* **REFACTOR**: Consolidated TTE source files from 7 to 2 (+1 rename):
  - `tte_design.R` + `tte_enrollment.R` + `tte_weights.R` merged into
    `tte_enrollment_r6.R` (TTEDesign + TTEEnrollment + all weight/matching
    functions called by their methods)
  - `tte_plan.R` + `tte_spec.R` + `tte_eligibility.R` merged into
    `tte_plan_r6.R` (TTEPlan + spec functions + eligibility helpers)
  - `registry_study.R` renamed to `registry_study_r6.R`
  - Files containing R6 classes now have `_r6` suffix for discoverability

* **REORDER**: TTEEnrollment public methods now follow workflow execution
  order: collapse -> ipw -> impute_confounders -> truncate ->
  prepare_for_analysis -> extract/summary/diagnostics -> analysis output.

* **DOCS**: Added inline comments documenting data flow in
  `generate_enrollments_and_ipw()` (Loop 1), `.tte_process_skeleton()`,
  `private$enroll()`, `enrollment_spec()`, and `add_one_ett()`.

# swereg 26.3.18

## Improvements

* **Study-specific**: Added `rd_approach3b_{single,multiple}` exposure
  variables that collapse two approach3 exposure levels into one combined
  level. Derived by relabeling the finished approach3 columns. This is valid
  because a switch between two active exposure types never triggers
  "previous".

* **Study-specific**: The 2026 study `add_lmed()` entry point now creates the
  exposure variables (`rd_approach{1,2,3}_{single,multiple}`) internally, via a
  new internal helper. This consolidates all study LMED logic in the package.
  It removes the need for a separate step 14 in external workflow scripts.

* **Study-specific**: Removed 18 sensitivity columns (`*_sensitivity_60p`,
  `*_sensitivity_under60censorallat60`, `*_sensitivity_under60censorrefat65`)
  from that helper. They had a logic issue: rows in one exposure category at
  age >= 65 produced `NA` instead of `FALSE`. The `rd_age_continuous` column is
  no longer required as input.

# swereg 26.2.27

## Improvements

* **VALIDATION**: `tte_validate_spec()` now emits a `warning()` instead of
  `stop()` when spec variables or values are missing from the skeleton.
  This makes validation informational rather than blocking, useful when
  working with small data subsets where rare categories may be absent.

# swereg 26.2.22

## New features

* **EXPORTED**: `tte_callr_pool()` — generic `callr::r_bg()` worker pool,
  generalized from the internal `.tte_callr_pool()`. New API accepts `items`
  (list of arg-lists), `worker_fn`, `item_labels`, and `collect` (FALSE to
  discard results when workers save directly). Eliminates boilerplate when
  scripts need their own parallel loops (e.g., Loop 2 IPCW-PP).

* **NEW**: `TTEPlan$generate_analysis_files_and_ipcw_pp()` — Loop 2 method
  that runs per-ETT IPCW-PP calculation and saves analysis-ready files.
  Mirrors `$generate_enrollments_and_ipw()` (Loop 1). Parameters:
  `output_dir`, `estimate_ipcw_pp_separately_by_exposure`,
  `estimate_ipcw_pp_with_gam`, `n_workers`, `swereg_dev_path`.

## Improvements

* **MEMORY**: `tte_calculate_ipcw()` now uses `mgcv::bam(discrete = TRUE)`
  instead of `mgcv::gam()` when `use_gam = TRUE`. `bam()` discretizes
  covariates to avoid forming the full model matrix, dramatically reducing
  peak memory for large datasets. Model objects are also explicitly freed
  (`rm()` + `gc()`) between exposed/unexposed fits.

* **MEMORY**: `$irr()` and `$km()` now subset to only the columns needed
  before creating `survey::svydesign()`. Previously the full data.table
  (all columns) was copied into the survey object. Model objects and
  intermediate data are freed between fits.

# swereg 26.2.21

## Breaking changes

* **RENAMED**: `$prepare_for_analysis()` parameters
  `estimate_ipcw_separately_by_exposure` → `estimate_ipcw_pp_separately_by_exposure`
  and `estimate_ipcw_with_gam` → `estimate_ipcw_pp_with_gam` for consistency
  with the IPCW-PP method they control.

* **PRIVATE**: `$enroll()`, `$prepare_outcome()`, `$ipcw_pp()`, and
  `$combine_weights()` are now private methods on `TTEEnrollment`.
  - Enrollment: use `tte_enrollment(data, design, ratio = 2, seed = 4)`
    instead of `tte_enrollment(data, design)$enroll(ratio = 2, seed = 4)`.
  - Outcome prep + IPCW: use `$prepare_for_analysis()` (unchanged).
  - Weight combination: handled automatically by `$ipcw_pp()` (unchanged).
  - Tests can access private methods via
    `enrollment$.__enclos_env__$private$method_name()`.

# swereg 26.2.20

## Breaking changes

* **RENAMED**: `$prepare_analysis()` → `$prepare_for_analysis()` on
  `TTEEnrollment`. The new name better communicates that this method
  *prepares* the enrollment *for* analysis (it is not the analysis itself).

## Bug fixes

* **FIXED**: 3 remaining broken test calls (`tte_extract()`,
  `tte_summary()`, `tte_weights()`) migrated to R6 method syntax
  (`$extract()`, `print()`, `$combine_weights()`). Column assertion
  updated: `"weight_pp"` → `"analysis_weight_pp"`.

* **FIXED**: `$impute_confounders()` now appends `"impute"` to
  `steps_completed`, consistent with all other mutating methods.

* **FIXED**: `$ipcw_pp()` IPW column guard moved from after IPCW
  computation to before it (fail-fast).

## Documentation

* **FIXED**: Vignette truncation bounds corrected from "0.5th and 99.5th
  percentiles" to "1st and 99th percentiles" (matching code defaults
  `lower = 0.01, upper = 0.99`).

* **FIXED**: `TTEDesign` roxygen references to removed `tte_match()` /
  `tte_expand()` replaced with `$enroll()`.

* **FIXED**: `$weight_summary()` moved from "Mutating" to "Non-mutating"
  section in `TTEEnrollment` roxygen (it only prints, never modifies data).

# swereg 26.2.13

## New features

* **NEW**: `$prepare_for_analysis()` method on `TTEEnrollment` merges
  `$prepare_outcome()` + `$ipcw_pp()` into one step. Parameters:
  `outcome`, `follow_up`, `separate_by_exposure`, `use_gam`, `censoring_var`.

* **NEW**: `$enrollment_stage` active binding on `TTEEnrollment`. Derives
  lifecycle stage from existing state: `"pre_enrollment"` →
  `"enrolled"` → `"analysis_ready"`. Zero maintenance — reads
  `data_level` and `steps_completed`.

## Bug fixes

* **FIXED**: 24 broken test cases calling removed standalone functions
  (`tte_enroll()`, `tte_collapse()`, `tte_ipw()`, `tte_truncate()`,
  `tte_prepare_outcome()`) migrated to R6 method syntax. Error message
  patterns updated to match method names (e.g., `enroll()` not
  `tte_enroll()`).

# swereg 26.2.12

## Breaking changes

* **RENAMED**: `TTETrial` class → `TTEEnrollment`, `tte_trial()` →
  `tte_enrollment()`, `summary.TTETrial` → `summary.TTEEnrollment`.
  The class represents an enrollment (matching + panel expansion), not an
  individual emulated target trial (ETT). Aligns naming with the ETT grid
  concept in `TTEPlan`.

# swereg 26.2.11

## Breaking changes

* **REMOVED**: 19 standalone TTE functions moved to R6 methods on `TTETrial`
  (15 methods) and `TTEPlan` (4 methods). Pipe chaining
  (`trial |> tte_ipw()`) replaced with `$`-chaining (`trial$ipw()`).

  **TTETrial methods**: `$enroll()`, `$collapse()`, `$ipw()`, `$ipcw_pp()`,
  `$combine_weights()`, `$truncate()`, `$prepare_outcome()`,
  `$impute_confounders()`, `$weight_summary()`, `$extract()`, `$summary()`,
  `$table1()`, `$rates()`, `$irr()`, `$km()`.

  **TTEPlan methods**: `$add_one_ett()`, `$save()`, `$enrollment_spec()`,
  `$generate_enrollments_and_ipw()`.

* **RENAMED**: `TTEPlan$task()` → `TTEPlan$enrollment_spec()`. The method
  returns enrollment metadata (design, enrollment_id, age_range, n_threads),
  not a generic task. The `process_fn` callback parameter convention changes
  from `function(task, file_path)` to `function(enrollment_spec, file_path)`.

  Removed exports: `tte_enroll`, `tte_collapse`, `tte_ipw`, `tte_ipcw_pp`,
  `tte_weights`, `tte_truncate`, `tte_prepare_outcome`, `tte_extract`,
  `tte_summary`, `tte_weight_summary`, `tte_table1`, `tte_rates`, `tte_irr`,
  `tte_km`, `tte_plan_add_one_ett`, `tte_plan_save`, `tte_plan_task`,
  `tte_generate_enrollments_and_ipw`.

  Kept standalone: `tte_rbind()`, `tte_rates_combine()`, `tte_irr_combine()`,
  `tte_impute_confounders()` (thin wrapper for callback default).

* **CHANGED**: TTE classes (`TTEDesign`, `TTETrial`, `TTEPlan`) migrated from
  S7 to R6. Property access changes from `@` to `$` (e.g., `trial@data` →
  `trial$data`, `design@id_var` → `design$id_var`). R6 reference semantics
  eliminate copy-on-write overhead from `trial$data[, := ...]`, reducing peak
  RAM from ~3X to ~2X during the weight-calculation chain (Loop 2).

* **FIXED**: Three S7 `@` accessor bugs that silently produced no-ops:
  - `$ipcw_pp()`: dropping intermediate IPCW columns (`p_censor`, etc.)
  - `$collapse()`: creating `person_weeks` column
  - `$impute_confounders()`: deleting old confounder columns before merge
  All fixed automatically by R6 (in-place modification works).

* **CHANGED**: `$ipcw_pp()` now inlines weight combination and truncation
  (was calling `tte_combine_weights()` and `tte_truncate_weights()` via function
  parameters that created extra refcount). Keeps data.table refcount=1 throughout.

## File reorganization

* Split `tte_classes.R` and `tte_methods.R` into per-class files with methods
  inline: `tte_design.R`, `tte_trial.R`, `tte_plan.R`. `tte_generate.R` reduced
  to thin `tte_impute_confounders()` wrapper + `.tte_callr_pool()` helper.

* Added `S3method(summary, TTETrial)` → delegates to `$summary()`.

## Dependencies

* **ADDED**: R6 package to Imports (S7 retained for skeleton classes).

# swereg 26.2.10

## Bug fixes

* **FIXED**: `tte_ipw()`, `tte_ipcw_pp()`: in-place joins via S7 `@` accessor
  now use extract/modify/reassign pattern (`dt <- trial@data; dt[...]; trial@data <- dt`).
  The previous `trial@data[i, := ...]` silently modified a copy, leaving the S7
  object's data unchanged.

## Performance

* **IMPROVED**: `tte_ipw()`, `tte_ipcw_pp()`, `tte_calculate_ipcw()`: replace
  `merge()` with in-place keyed joins (`data[i, := ...]`), reducing peak RAM
  from ~3x to ~2x panel size during the weight-calculation chain.

## Breaking changes

* **CHANGED**: `tte_ipcw_pp()` now also combines weights (`ipw * ipcw_pp` →
  `analysis_weight_pp`), truncates `analysis_weight_pp`, and drops intermediate
  IPCW columns (`p_censor`, `p_uncensored`, `cum_p_uncensored`, `marginal_p`,
  `cum_marginal`). Callers no longer need `tte_weights()` + `tte_truncate()`
  after `tte_ipcw_pp()`.

* **RENAMED**: `tte_generate_enrollments()` → `tte_generate_enrollments_and_ipw()`.
  Now computes IPW + truncation once on the full combined enrollment (after
  imputation), so the per-ETT Loop 2 no longer needs to call `tte_ipw()`.
  New `stabilize` parameter (default TRUE) controls IPW stabilization.

## New features

* **NEW**: `tte_plan_load()` reads a `.qs2` plan file and reconstructs the
  `TTEPlan` S7 object. Companion to `tte_plan_save()`.

* **CHANGED**: `tte_plan_save()` now persists `project_prefix` and
  `skeleton_files` alongside `ett` and `global_max_isoyearweek`, so
  `tte_plan_load()` can fully reconstruct the object.

* **NEW**: `skeleton_process()` gains `n_workers` parameter for parallel batch
  processing. When > 1, uses `callr::r()` + `parallel::mclapply()` to process
  batches concurrently while avoiding `fork()` + data.table OpenMP segfaults.

# swereg 26.2.9

## Improvements

* **CHANGED**: Migrate serialization from `qs` (archived) to `qs2`.
  `.qs_save`/`.qs_read` wrappers now call `qs2::qs_save`/`qs2::qs_read`
  (standard format, preserves S7 objects). All file extensions changed
  from `.qs` to `.qs2`. The `preset` parameter is no longer used.

* **IMPROVED**: `tte_rates()` now sets `swereg_type` and `exposure_var` attributes on its output;
  `tte_irr()` sets `swereg_type`.

* **RENAMED**: `tte_rates_table()` → `tte_rates_combine()`, `tte_irr_table()` → `tte_irr_combine()`.
  New API accepts `(results, slot, descriptions)` — extracts the rates/irr slot internally,
  removing the need for `lapply(results, [[, "table2")` at call sites. Exposure column is now
  read from the `exposure_var` attribute instead of guessing via `setdiff()`.

## Breaking changes

* **CHANGED**: `tte_plan_add_one_ett()` now requires explicit `enrollment_id` parameter.
  Auto-assignment based on follow_up + age_group removed. Validation that design params
  match within an enrollment_id is preserved.

* **IMPROVED**: `print(plan)` now shows both enrollment grid and full ETT grid.

* **CHANGED**: `tte_plan_add_one_ett()` bundles `age_group`, `age_min`, `age_max`,
  `person_id_var` into an `argset` named list parameter. `time_exposure_var` and
  `eligible_var` no longer have defaults (must be explicit). `exposure_var` removed
  from interface (hardcoded to `"baseline_exposed"`).

* **RENAMED**: `file_id` column in the `ett` data.table → `enrollment_id`. This makes explicit that ETTs sharing the same follow_up + age_group are processed together as one "enrollment" (shared eligibility, matching, collapse, imputation).

* **RENAMED**: `tte_generate_trials()` → `tte_generate_enrollments()`. The function generates enrollments (one per follow_up × age_group), not individual trials.

* **RENAMED**: `tte_plan_task()` return list key `file_id` → `enrollment_id`.

* **UPDATED**: `print(plan)` now shows "Enrollments: N x M skeleton files" instead of "Tasks: N file_id(s) x M skeleton files".

# swereg 26.2.8

## Breaking changes

* **CHANGED**: `tte_plan()` is now infrastructure-only — takes only `project_prefix`, `skeleton_files`, `global_max_isoyearweek`. Use `tte_plan_add_one_ett()` to add ETTs with per-ETT design parameters.

* **REMOVED**: TTEPlan plan-level properties `confounder_vars`, `person_id_var`, `exposure_var`, `time_exposure_var`, `eligible_var`. These are now per-ETT columns in the `ett` data.table.

* **REMOVED**: Internal `.tte_grid()` function. The ETT grid is now built incrementally via `tte_plan_add_one_ett()`.

* **ADDED**: `TTEPlan@project_prefix` property (needed for file naming in `tte_plan_add_one_ett()`).

## New features

* **NEW**: `tte_plan_add_one_ett()` — builder function that adds one ETT row to a plan. Stores design params (confounder_vars, person_id_var, exposure_var, time_exposure_var, eligible_var) per-ETT, allowing different ETTs to use different confounders. Validates that design params match within an enrollment_id (same follow_up + age_group).

* **RENAMED**: `TTEPlan@files` property → `TTEPlan@skeleton_files` for clarity.

# swereg 26.2.7

## Breaking changes

* **REFACTORED**: `tte_generate_enrollments()` (formerly `tte_generate_trials()`) now takes a `TTEPlan` object instead of separate parameters (`ett`, `files`, `confounder_vars`, `global_max_isoyearweek`). The `process_fn` callback signature changes from `function(file_path, design, file_id, age_range, n_threads)` to `function(task, file_path)` where `task` is a list with `design`, `enrollment_id`, `age_range`, and `n_threads`.

## New features

* **NEW**: `TTEPlan` S7 class bundles ETT grid, skeleton file paths, confounder definitions, and design column names into a single object for trial generation.
  - `tte_plan()`: Constructor function
  - `tte_plan_task(plan, i)`: Extract the i-th enrollment task as a list with `design`, `enrollment_id`, `age_range`, `n_threads`
  - `plan[[i]]`: Shorthand for `tte_plan_task(plan, i)`
  - `length(plan)`: Number of unique enrollment_id groups
  - Supports interactive testing: `task <- plan[[1]]; process_fn(task, plan@skeleton_files[1])`

# swereg 26.2.6

## Documentation

* **FIXED**: Add missing topics to pkgdown reference index (TTEDesign, TTETrial, x2026_mht_add_lmed)

# swereg 26.2.5

## Bug fixes

* **FIXED**: Set `eval = FALSE` in skeleton3-analyze vignette to prevent build errors from optional `qs` package dependency

# swereg 26.2.4

## Bug fixes

* **FIXED**: Remove `qs` from Suggests to fix GitHub Actions CI (package not available on CRAN)

# swereg 26.2.3

## Breaking changes

* **REPLACED**: `tte_match()` and `tte_expand()` merged into single `tte_enroll()` function:
  - Old workflow: `tte_trial(data, design) |> tte_match(ratio = 2, seed = 4) |> tte_expand(extra_cols = "isoyearweek")`
  - New workflow: `tte_trial(data, design) |> tte_enroll(ratio = 2, seed = 4, extra_cols = "isoyearweek")`
  - The two operations were tightly coupled and always used together
  - `tte_enroll()` combines sampling (matching) and panel expansion in one step
  - Records "enroll" in `steps_completed` (previously recorded "match" then "expand")

## New features

* **NEW**: Trial eligibility helper functions for composable eligibility criteria:
  - `tte_eligible_isoyears()`: Check eligibility based on calendar years
  - `tte_eligible_age_range()`: Check eligibility based on age range
  - `tte_eligible_no_events_in_window_excluding_wk0()`: Check for no events in prior window (correctly excludes baseline week)
  - `tte_eligible_no_observation_in_window_excluding_wk0()`: Check for no specific value in prior window (for categorical variables)
  - `tte_eligible_combine()`: Combine multiple eligibility columns using AND logic
  - All functions modify data.tables by reference and return invisibly for method chaining

## Documentation

* **IMPROVED**: Clarified that eligibility checks should EXCLUDE the baseline week. Using `cumsum(x) == 0` is incorrect because it includes the current week. The new eligibility functions use `any_events_prior_to()` which correctly excludes the current row.

# swereg 26.1.31

## New features

* **NEW**: S7 object-oriented API for target trial emulation workflows:
  - `TTEDesign` class: Define column name mappings once and reuse across all TTE functions
  - `TTETrial` class: Fluent method chaining with workflow state tracking
  - `tte_design()` / `tte_trial()`: Constructor functions for the S7 classes
  - `tte_match()`, `tte_expand()`, `tte_collapse()`, `tte_ipw()`: S7 methods for data preparation
  - `tte_prepare_outcome()`, `tte_ipcw()`: Outcome-specific per-protocol analysis
  - `tte_weights()`, `tte_truncate()`: Weight combination and truncation
  - `tte_rbind()`: Combine batched trial objects
  - `tte_extract()`, `tte_summary()`: Access data and diagnostics
  - `tte_table1()`, `tte_rates()`, `tte_irr()`, `tte_km()`: Analysis and visualization

## Breaking changes

* **REMOVED**: Deprecated S7 methods replaced by `tte_prepare_outcome()`:
  - `tte_tte()`: Use `tte_prepare_outcome()` which computes `weeks_to_event` internally
  - `tte_set_outcome()`: Use `tte_prepare_outcome(outcome = "...")` instead
  - `tte_censoring()`: Use `tte_prepare_outcome()` which handles censoring internally

## Dependencies

* **ADDED**: S7 package to Imports for object-oriented class system

# swereg 26.1.30

## New features

* **NEW**: Target trial emulation weight functions for causal inference in observational studies:
  - `tte_calculate_ipw()`: Calculate stabilized inverse probability of treatment weights (IPW) for baseline confounding adjustment using propensity scores
  - `tte_calculate_ipcw()`: Calculate time-varying inverse probability of censoring weights (IPCW) for per-protocol analysis using GAM or GLM
  - `tte_identify_censoring()`: Identify protocol deviation and loss to follow-up for per-protocol analysis
  - `tte_combine_weights()`: Combine IPW and IPCW weights for per-protocol effect estimation
  - `tte_truncate_weights()`: Truncate extreme weights at specified quantiles to reduce variance

* **NEW**: Target trial emulation data preparation functions:
  - `tte_match_ratio()`: Sample comparison group at specified ratio (e.g., 2:1 unexposed to exposed)
  - `tte_collapse_periods()`: Collapse fine-grained time intervals (e.g., weekly) to coarser periods (e.g., 4-week)
  - `tte_time_to_event()`: Calculate time to first event for each trial/person

## Dependencies

* **ADDED**: mgcv package to Imports for flexible GAM-based censoring models in `tte_calculate_ipcw()`

# swereg 25.12.24

## API changes

* **SIMPLIFIED**: Removed `validate_source_column()` requirement from `add_diagnoses()`, `add_operations()`, `add_icdo3s()`, `add_snomed3s()`, and `add_snomedo10s()`:
  - The `source` column is no longer required in diagnosis data
  - To track diagnoses by source (inpatient/outpatient/cancer), filter the dataset externally before calling `add_diagnoses()`
  - See `?add_diagnoses` for the recommended pattern

## New features

* **NEW**: `any_events_prior_to()` function for survival analysis:
  - Checks if any TRUE values exist in a preceding time window (excludes current row)
  - Useful for determining if an event occurred in a prior time period
  - Default window of 104 weeks (~2 years) with customizable size
  - Complements `steps_to_first()` for comprehensive time-to-event analysis

* **ENHANCED**: `steps_to_first()` function improvements:
  - Renamed parameter from `window` to `window_including_wk0` for clarity
  - Default window is now 104 (inclusive of current week)
  - Added `@family survival_analysis` tag and cross-reference to `any_events_prior_to()`

## Bug fixes

* **FIXED**: Added slider package to Imports in DESCRIPTION to fix R CMD check warning about undeclared import

## Data

* **BREAKING**: Replaced separate `fake_inpatient_diagnoses` and `fake_outpatient_diagnoses` with unified `fake_diagnoses` dataset:
  - New `SOURCE` column identifies data origin: "inpatient", "outpatient", or "cancer"
  - ~2000 inpatient records, ~2000 outpatient records, ~1000 cancer records
  - Cancer records always have populated `ICDO3` codes
  - Enables testing of source-based filtering and validation

* **ENHANCED**: Added ICD-O-3 and SNOMED-CT columns to fake diagnosis data:
  - `ICDO3`: ICD-O-3 morphology codes (always populated for cancer source)
  - `SNOMED3`: SNOMED-CT version 3 codes
  - `SNOMEDO10`: SNOMED-CT version 10 codes

## Validation

* **ENHANCED**: SOURCE column validation is now optional - filter externally if needed (see API changes above)

## Documentation

* **IMPROVED**: Examples for `add_icdo3s()`, `add_snomed3s()`, and `add_snomedo10s()` are now runnable using package fake data (previously wrapped in `\dontrun{}`)

# swereg 25.12.6

## New features

* **NEW**: `steps_to_first()` function for survival analysis:
  - Calculates the number of steps (e.g., weeks) until the first TRUE value in a forward-looking window
  - Useful for time-to-event calculations in longitudinal registry data
  - Default window of 103 weeks (~2 years) with customizable size
  - Returns NA if no event occurs within the window

## Bug fixes

* **CRITICAL**: Fixed `add_snomed3s()` and `add_snomedo10s()` calling non-existent internal functions
  - Both functions now correctly call `add_diagnoses_or_operations_or_cods_or_icdo3_or_snomed()`
  - These functions would have caused runtime errors before this fix

* **FIXED**: Removed erroneous `icdo10` column references from `add_diagnoses()`:
  - ICD-O only has editions 1, 2, and 3 (not 10)
  - ICD-O-3 codes should be handled via the dedicated `add_icdo3s()` function

* **FIXED**: Added `icd7*` and `icd9*` columns to diagnosis search in `add_diagnoses()`:
  - Historical ICD-7 and ICD-9 columns are now properly searched when `diag_type = "both"`
  - Validation and helper function now consistent

* **FIXED**: Corrected error messages in `add_icdo3s()`, `add_snomed3s()`, and `add_snomedo10s()`:
  - Messages now correctly reference the appropriate data types instead of "operation data"

## Documentation

* **ENHANCED**: `add_diagnoses()` documentation now clearly lists which diagnosis columns are searched:
  - When `diag_type = "both"`: `hdia`, `dia*`, `ekod*`, `icd7*`, `icd9*`
  - When `diag_type = "main"`: `hdia` only

# swereg 25.8.19

## CRAN Submission Preparation

* **CRAN READY**: Package prepared for CRAN submission with comprehensive compliance improvements:
  - Fixed DESCRIPTION file author field duplication issue
  - Updated .Rbuildignore to exclude all development files (docs/, .git/, .Rhistory, etc.)
  - Removed non-portable files (@eaDir directories, .DS_Store files)
  - Added missing global variable declarations to prevent R CMD check warnings
  - Verified URL consistency between DESCRIPTION and package startup messages

* **OPTIMIZED**: Vignette structure significantly improved for CRAN submission:
  - Reduced total vignette content by 31% (626 lines removed)
  - Condensed cookbook-survival-analysis.Rmd (removed verbose descriptive statistics and redundant sections)
  - Simplified skeleton2-clean.Rmd (removed duplicated skeleton1_create workflow)
  - Streamlined skeleton3-analyze.Rmd (removed redundant data loading and best practices sections)
  - Fixed all vignette build errors by ensuring consistent data variable availability
  - All vignettes now compile successfully and use package synthetic data consistently

* **VALIDATED**: All examples are runnable using package fake data - no \\dontrun sections without justification

## Code Quality Improvements

* **CONSISTENCY**: Fixed date_columns parameter usage throughout package:
  - Updated all vignettes to use lowercase date_columns parameters (e.g., "indatum" instead of "INDATUM")
  - Added warning to make_lowercase_names() function when uppercase date_columns are provided
  - Enhanced documentation to clarify that date_columns should use lowercase names
  - Improved user experience with clear guidance and automatic handling of uppercase inputs

* **ELEGANCE**: Enhanced vignette code patterns for better readability:
  - Replaced verbose data() loading patterns with elegant pipe syntax
  - Updated all data loading to use swereg::fake_* |> copy() |> make_lowercase_names() pattern
  - Eliminated clumsy multi-step data preparation code throughout vignettes
  - Improved code flow and professional appearance of package examples
* **VERIFIED**: Package builds successfully with R CMD build and passes CRAN compliance checks
* **CONFIRMED**: inst/ directory contains only files referenced by package functions

# swereg 25.7.30

## New Features

* **NEW**: `make_rowind_first_occurrence()` helper function for rowdep → rowind transformations:
  - Simplifies the common pattern of creating row-independent variables from first occurrence of conditions
  - Automatically handles temp variable creation and cleanup
  - Uses `first_non_na()` for robust aggregation across all variable types
  - Includes comprehensive input validation and clear error messages

* **NEW**: "Understanding rowdep and rowind Variables" vignette:
  - Explains the fundamental distinction between row-dependent and row-independent variables
  - Demonstrates common transformation patterns with practical examples
  - Shows integration with the swereg workflow (skeleton1_create → skeleton2_clean → skeleton3_analyze)
  - Includes best practices for longitudinal registry data analysis

## Documentation

* **ENHANCED**: Helper functions now include `@family data_integration` tags for better organization
* **IMPROVED**: Function examples use existing fake datasets for consistency

# swereg 25.7.16

## New Swedish Date Parsing and Enhanced Data Cleaning

* **NEW**: `parse_swedish_date()` function for handling Swedish registry dates with varying precision:
  - Handles 4-character (YYYY), 6-character (YYYYMM), and 8-character (YYYYMMDD) formats
  - Automatically replaces "0000" with "0701" and "00" with "15" for missing date components
  - Supports custom defaults for missing date parts
  - Includes comprehensive error handling and vectorized processing

* **ENHANCED**: `make_lowercase_names()` now supports automatic date cleaning:
  - New `date_column` parameter to specify which column contains dates
  - Automatically creates cleaned 'date' column using `parse_swedish_date()`
  - Works with both default and data.table methods
  - Maintains backward compatibility with existing code

* **ENHANCED**: All `add_*` functions now require cleaned date columns:
  - `add_diagnoses()`, `add_operations()`, `add_rx()`, `add_cods()` expect 'date' column
  - Clear error messages guide users to use `make_lowercase_names(data, date_column = "...")` 
  - Improved validation ensures data preprocessing consistency

* **ENHANCED**: `create_skeleton()` now includes `personyears` column:
  - Annual rows (is_isoyear==TRUE) have personyears = 1
  - Weekly rows (is_isoyear==FALSE) have personyears = 1/52.25
  - Facilitates person-time calculations for survival analysis

* **IMPROVED**: Survival analysis cookbook vignette updated:
  - Uses weekly data instead of yearly data for more precise analyses
  - Age calculation based on isoyearweeksun instead of isoyear
  - Includes person-time in descriptive statistics
  - Demonstrates proper use of new date cleaning workflow

## Enhanced error handling and validation

* **ENHANCED**: Comprehensive input validation for all `add_*` functions:
  - `add_onetime()`: Validates skeleton structure, ID column exists, checks for ID matches
  - `add_annual()`: Validates isoyear parameter, checks skeleton year coverage
  - `add_diagnoses()`: Validates diagnosis patterns, checks for diagnosis code columns
  - `add_operations()`: Validates operation patterns, checks for operation code columns  
  - `add_rx()`: Validates prescription data structure, checks source columns
  - `add_cods()`: Validates death data structure, checks cause of death columns
* **IMPROVED**: User-friendly error messages with specific guidance:
  - Clear indication when `make_lowercase_names()` is forgotten
  - Helpful suggestions for column naming issues
  - Informative ID mismatch diagnostics with sample values
* **NEW**: Internal validation helper functions for consistent error handling
* **ADDED**: Input validation for pattern lists, data structures, and parameter ranges

## New cookbook documentation

* **NEW**: Comprehensive survival analysis cookbook (`cookbook-survival-analysis.Rmd`):
  - Complete workflow from raw data to Cox proportional hazards model
  - Time-varying covariates (annual income) with heart attack outcome
  - Handles common challenges: missing data, multiple events, competing risks
  - Performance tips for large datasets
  - Practical solutions for real-world registry analysis
* **ENHANCED**: Updated `_pkgdown.yml` with new "Cookbooks" section
* **ADDED**: `survival` package to Suggests dependencies

## Bug fixes

* **FIXED**: Improved ID matching warnings and error messages across all functions
* **CORRECTED**: Better handling of missing data in time-varying covariate analysis
* **ENHANCED**: More robust parameter validation prevents common user errors

# swereg 25.7.16

## Major documentation restructuring

* **RESTRUCTURED**: Complete vignette reorganization for clear learning progression:
  - NEW "Skeleton concept" vignette: Conceptual foundation explaining the skeleton approach without technical implementation
  - "Building the data skeleton (skeleton1_create)": Pure data integration focus - raw data to time-structured skeleton
  - "Cleaning and deriving variables (skeleton2_clean)": Pure data cleaning and variable derivation focus
  - "Production analysis workflows (skeleton3_analyze)": Memory-efficient processing and final analysis datasets
* **IMPROVED**: Clear separation of concerns with focused, single-purpose tutorials
* **ENHANCED**: Systematic learning progression from concept to implementation to production
* **UPDATED**: _pkgdown.yml structure with logical vignette grouping
* **PRESERVED**: All existing technical content while improving organization

## Content improvements

* **NEW**: Comprehensive conceptual introduction based on presentation content
* **IMPROVED**: Each vignette builds systematically on the previous one
* **ENHANCED**: Better explanation of three types of data integration (one-time, annual, event-based)
* **CLARIFIED**: Production workflow patterns with memory-efficient batching strategies
* **STANDARDIZED**: Consistent academic tone and sentence case throughout

# swereg 25.7.15

## Documentation and presentation improvements

* **STANDARDIZED**: Changed all titles and headings to normal sentence case throughout:
  - Vignette titles: "Basic Workflow" → "Basic workflow", "Complete Workflow" → "Complete workflow", etc.
  - README.md section headings: "Core Functions" → "Core functions", "Data Integration" → "Data integration", etc.
  - NEWS.md section headings: "Vignette Restructuring" → "Vignette restructuring", etc.
  - CLAUDE.md section headings: "Project Overview" → "Project overview", "Development Commands" → "Development commands", etc.
* **IMPROVED**: Consistent normal sentence case for better readability and less formal appearance
* **SIMPLIFIED**: Removed subtitle text after colons in vignette titles for cleaner presentation
* **ENHANCED**: Improved Core Concept section in basic workflow vignette with clear explanation of three data types:
  - One-time data (demographics): Added to all rows for each person
  - Annual data (income, family status): Added to all rows for specific year
  - Event-based data (diagnoses, prescriptions, deaths): Added to rows where events occurred
* **CLARIFIED**: Step 1 documentation now properly explains all skeleton columns including `isoyearweeksun`
* **VERIFIED**: All vignettes compile successfully with improved content

## Major documentation and vignette reorganization

* **RESTRUCTURED**: Complete vignette reorganization with improved naming and content flow:
  - `swereg.Rmd` → `basic-workflow.Rmd`: Focused introduction to skeleton1_create
  - `advanced-workflow.Rmd` → `complete-workflow.Rmd`: Two-stage workflow (skeleton1_create + skeleton2_clean)
  - `memory-efficient-batching.Rmd`: Maintained as comprehensive three-stage workflow guide
* **IMPROVED**: Eliminated content redundancy between vignettes for clearer learning progression
* **ENHANCED**: Updated _pkgdown.yml configuration to reflect new vignette structure

## Function documentation improvements

* **ENHANCED**: Comprehensive documentation improvements for all exported functions:
  - Added @family tags for logical grouping (data_integration, skeleton_creation, data_preprocessing)
  - Added @seealso sections with cross-references to related functions and vignettes
  - Replaced placeholder examples with runnable code using synthetic data
  - Improved parameter documentation with detailed descriptions and expected formats
  - Enhanced return value documentation with explicit side effects description
* **STANDARDIZED**: Consistent academic tone throughout all documentation

## Professional presentation updates

* **IMPROVED**: Removed informal elements and adopted academic tone across all documentation
* **UPDATED**: Changed terminology from "fake data" to "synthetic data" throughout
* **ENHANCED**: More professional language in README.md and vignettes
* **STANDARDIZED**: Consistent formal tone appropriate for scientific software

## Technical improvements

* **VERIFIED**: All vignettes compile successfully with updated content
* **TESTED**: Package passes R CMD check with all documentation improvements
* **UPDATED**: CLAUDE.md reflects new vignette structure and documentation standards

# swereg 25.7.1

## Vignette restructuring

* **RESTRUCTURED**: Reorganized vignettes for clearer learning progression:
  - `swereg.Rmd`: Clean skeleton1_create tutorial using full datasets (removed subset filtering)
  - `advanced-workflow.Rmd`: Focused skeleton1→skeleton2 workflow (removed batching and skeleton3 content)
  - `memory-efficient-batching.Rmd`: NEW comprehensive batching vignette with complete skeleton1→skeleton2→skeleton3 workflow for large-scale studies
* **IMPROVED**: GitHub Actions workflow optimization with dependency caching and binary packages for faster CI/CD

## Batching vignette fixes

* **FIXED**: Updated memory-efficient-batching vignette with production-ready improvements:
  - Replace `split()` with `csutil::easy_split` for better batch handling
  - Replace `saveRDS/readRDS` with `qs::qsave/qread` for 2-10x faster file I/O
  - Fix skeleton3_analyze to properly aggregate weekly→yearly data using `swereg::max_with_infinite_as_na`
  - Remove incorrect `is_isoyear == TRUE` filter in skeleton3_analyze
  - Fix analysis results to avoid NaN outputs in treatment rate calculations
  - Add explanations for weekly→yearly data aggregation and qs package performance benefits

## New features

* **NEW**: Added `isoyearweeksun` variable to `create_skeleton()` function - provides Date representing the Sunday (last day) of each ISO week/year for easier date calculations
* **NEW**: Updated package logo
* **IMPROVED**: Updated all vignettes to not assume swereg is loaded - all functions use `swereg::` prefix and `data()` calls use `package="swereg"` argument
* **IMPROVED**: Updated function documentation to clarify that pattern matching functions (`add_diagnoses`, `add_cods`, `add_rx`) automatically add "^" prefix - users should NOT include "^" in their patterns
* **NEW**: Added comprehensive fake Swedish registry datasets for development and vignettes:
  - `fake_person_ids`: 1000 synthetic personal identifiers
  - `fake_demographics`: Demographics data matching SCB format
  - `fake_annual_family`: Annual family status data
  - `fake_inpatient_diagnoses` and `fake_outpatient_diagnoses`: NPR diagnosis data with ICD-10 codes
  - `fake_prescriptions`: LMED prescription data with ATC codes and hormone therapy focus
  - `fake_cod`: Cause of death data
* **NEW**: Added two comprehensive vignettes:
  - `swereg.Rmd`: Basic skeleton1_create workflow tutorial
  - `advanced-workflow.Rmd`: Complete 3-phase workflow (skeleton1 → skeleton2 → skeleton3)
* **NEW**: Replaced magrittr pipe (%>%) with base pipe (|>) throughout codebase
* **NEW**: Added memory-efficient batched processing examples for large registry studies

## Bug fixes

* **CRITICAL**: Fixed incorrect variable names in `fake_cod` dataset - changed from non-Swedish `underlying_cod/contributory_cod1/contributory_cod2` to correct Swedish registry names `ulorsak/morsak1/morsak2`
* **VERIFIED**: Confirmed all fake datasets use correct Swedish registry variable name conventions
* **VERIFIED**: All ICD-10 and ATC codes in fake datasets are properly formatted and realistic

## Documentation improvements

* **BREAKING**: Fixed incorrect function descriptions that were copied from another package
* **NEW**: Added comprehensive roxygen2 documentation for all exported functions:
  - `add_onetime()`: Documents merging one-time/baseline data to skeleton
  - `add_annual()`: Documents merging annual data for specific ISO years
  - `add_cods()`: Documents cause of death analysis with ICD-10 codes
  - `add_diagnoses()`: Documents diagnosis analysis with main/secondary diagnoses
  - `add_operations()`: Documents surgical operation analysis including gender-affirming procedures
  - `add_rx()`: Documents prescription drug analysis with ATC/product codes
  - `create_skeleton()`: Documents longitudinal skeleton creation with detailed return structure
  - `make_lowercase_names()`: Documents generic function with S3 methods
  - The 2023 study `add_lmed()` entry point: Documents a study-specific LMED function
* **NEW**: Added documentation for all helper functions:
  - `min_with_infinite_as_na()`, `max_with_infinite_as_na()`
  - `as_logical_min_with_infinite_as_na()`, `as_logical_max_with_infinite_as_na()`
  - `first_non_na()`, `last_non_na()`
* **NEW**: Added `@param` descriptions for all function parameters
* **NEW**: Added `@return` descriptions explaining function outputs
* **NEW**: Added `@examples` with practical usage demonstrations
* **NEW**: Added `@details` and `@note` sections for complex functions
* **IMPROVED**: Used proper roxygen2 practices including `@rdname` for S3 methods and `@seealso` cross-references

## Package structure

* All exported functions now have complete, accurate documentation suitable for CRAN submission
* Documentation focuses on Swedish registry data analysis workflows
* Examples use `\dontrun{}` appropriately for functions requiring external data
