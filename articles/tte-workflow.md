# TTE workflow: from skeleton to results

## Target Trial Emulation with swereg

This vignette walks through the full target trial emulation (TTE)
workflow in swereg: from skeleton files on disk to ETT-level
per-protocol effect estimates. It tries to keep the epidemiological
rationale and the technical R6/callr mechanics on the same page so you
can read it top-down without losing the plot.

For the methodological mapping to reference papers (Hernán 2008/2016,
Danaei 2013, Caniglia 2023, Cashin 2025) see
[`vignette("tte-methodology")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methodology.md).
For a one-page glossary of the terms used in this vignette see
[`vignette("tte-nomenclature")`](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.md).
For how the skeleton files are built in the first place see
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md).

### The TTE concept in one page

A randomized trial of an intervention gives you the causal effect *for
free* if the randomization is valid: treatment assignment is independent
of potential outcomes, so comparing observed outcomes across arms is an
unbiased estimator of the effect. The problem is that most clinically
important questions can’t be randomized – usually because the
intervention is a drug that is already approved and widely used, or
because randomizing would be unethical, or because the outcome is too
rare to power a feasible trial.

Target trial emulation is a discipline for doing observational analyses
as if they were trials. You **write down the protocol of the target
trial** first (eligibility, treatment arms, assignment, outcome,
follow-up, analysis plan), then use observational data to build a
dataset that mimics the protocol as closely as possible. The mimic is
imperfect in specific, nameable ways, and each imperfection has a
specific statistical correction.

#### The emulation failures

Hernán and Robins (2016) name four specific failure modes that
observational analyses suffer which randomized trials do not:

1.  **Treatment assignment is not random** – people who choose the
    exposure differ systematically from people who don’t. Corrected with
    baseline confounder adjustment, typically via inverse probability of
    treatment weighting (IPW).

2.  **Time zero is not well-defined** – in a trial, time zero is the
    moment of randomization. In observational data there is no such
    moment, so you have to define one. Swereg uses *band-based
    enrollment*: any eligible person-week inside the enrollment period
    becomes a potential time-zero, creating a sequence of overlapping
    trials.

3.  **Follow-up is informatively truncated** – people drop out of
    observation (emigration, death, end of data) in ways that depend on
    their baseline or time-varying characteristics. Corrected with
    inverse probability of censoring weighting (IPCW).

4.  **Treatment is not adherent** – in a trial, adherence is enforced
    (or non-adherence is measured and modeled). In observational data,
    people switch treatments over time. swereg handles this via
    per-protocol censoring at treatment switch, followed by IPCW-PP to
    correct for the selection that creates.

#### The sequence-of-nested-trials construction

The canonical observational TTE (Hernán et al. 2008; Danaei et al. 2013)
builds one trial per eligible enrollment period. Within each period, a
new-user design identifies people initiating the exposure (“exposed
arm”) and people eligible but not initiating (“comparator arm”). Each
person can appear in multiple sequential trials as long as they remain
eligible and non-initiated.

This produces a *long panel*: one row per person per trial per follow-up
week. It’s huge but the structure is uniform and the statistical
operations (matching, IPW, pooled logistic / Poisson regression) are
straightforward once you have it.

### The swereg TTE model

The swereg implementation maps this concept onto three R6 classes and
two parallelized loops.

#### TTEDesign: column name schema

`TTEDesign` holds the names of the columns that define the trial schema:
person ID, exposure, outcome, confounders, time. It’s constructed once
and reused across every enrollment. Think of it as the “schema” of a
trial, not the trial itself.

#### TTEEnrollment: one sequence of trials

`TTEEnrollment` wraps the data for one group of trials that share an
eligibility definition. Its `data_level` field tracks the lifecycle:

- **`"person_week"`**: raw skeleton subset after applying age-range /
  isoyear / exclusion filters. One row per person per ISO week. This is
  the input to enrollment.
- **`"trial"`**: after `$enroll()` (called internally by
  `TTEEnrollment$new(..., ratio = )`), the data has been expanded to the
  counting-process trial panel – one row per person per trial per time
  period. `tstart` / `tstop` columns appear here. This is the form IPW
  and IPCW-PP estimation operates on.

Enrollment itself is per-band stratified matching: within each
`period_width`-week band, swereg samples `matching_ratio` unexposed for
every observed initiator. This is a computational shortcut compared to
full cloning – see
[`vignette("tte-methodology")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methodology.md)
for the trade-offs.

#### TTEPlan: the ETT grid builder

`TTEPlan` holds the **ETT grid**: the Cartesian product of *enrollments
× outcomes × follow-up durations*. One ETT is one final analysis-ready
dataset. The plan knows how to run the two loops that produce those
datasets.

``` r
# spec is a YAML file; study is a RegistryStudy with built skeletons
plan <- swereg::tteplan_from_spec_and_registrystudy(
  spec  = "002-ozel-psychosis/spec_v001.yaml",
  study = study
)
plan$ett
#>     ett_id enrollment_id  outcome_var  follow_up_weeks  file_raw  file_imp  file_analysis
#>  1:  ETT01            01  osd_f20_to_f29              52  ...       ...       ...
#>  2:  ETT02            01  osd_f20_to_f29             156  ...       ...       ...
#>  3:  ETT03            01  osd_f20_to_f29             260  ...       ...       ...
#>  4:  ETT04            01  osd_f20_f25                 52  ...       ...       ...
#>  5:  ETT05            01  osd_f20_f25                156  ...       ...       ...
#> ...
```

Every row in `plan$ett` points at three files: a `file_raw`
(post-enrollment, pre-imputation), a `file_imp` (post-imputation + IPW,
reused across outcomes within the same enrollment_id), and a
`file_analysis` (one per ETT). Loop 1 produces the first two; Loop 2
produces the third.

### The spec YAML

All of the clinical + methodological decisions live in a YAML file that
gets parsed into a nested R list by
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md).
The top-level sections are:

``` yaml
study:
  title: "..."
  principal_investigator: "..."
  description: "..."
  implementation:
    project_prefix: "002-ozel-psychosis"

inclusion_criteria:
  isoyears: [2008, 2023]

exclusion_criteria:
  - name: "Gender dysphoria (ICD-10 F64)"
    rationale: "Cross-sex hormone therapy confounds MHT exposure"
    implementation:
      source_variable: osd_f64
      window: "lifetime_before_and_after_baseline"

  - name: "Prior psychotic disorder (ICD-10 F20-F29)"
    implementation:
      source_variable: osd_f20_to_f29
      window: 104
      computed: true

confounders:
  - name: "Age (continuous)"
    implementation:
      variable: rd_age_continuous

  - name: "Psychotropic medication use in past year"
    implementation:
      source_variable: rx_n05_n06
      window: 52
      computed: true

outcomes:
  - name: "Schizophrenia spectrum (F20-F29)"
    implementation:
      variable: osd_f20_to_f29

follow_up:
  - { label: "1 year",  weeks: 52 }
  - { label: "3 years", weeks: 156 }
  - { label: "5 years", weeks: 260 }

enrollments:
  - id: "01"
    name: "Systemic MHT vs local/none, age 50-59"
    additional_inclusion:
      - type: age_range
        min: 50
        max: 59
        implementation:
          variable: rd_age_continuous
    exposure:
      arms:
        exposed:    "Systemic MHT"
        comparator: "Local or no MHT"
      implementation:
        matching_ratio:  2
        variable:        rd_approach1_single
        exposed_value:   systemic_mht
        comparator_value: local_or_none_mht
        seed:            4
```

#### Anatomy of an exclusion criterion

Each item under `exclusion_criteria` has two halves:

- A **clinical half** (`name`, `rationale`) that medical collaborators
  can review without knowing anything about R.
- An **implementation half** (`implementation:`) that swereg consumes:
  the name of the skeleton column used to evaluate the rule
  (`source_variable`), the time window during which an event in that
  column causes exclusion (`window`), and a `computed` flag marking
  rules that swereg should apply automatically at enrollment time via a
  rolling window.

`window` can be:

- An integer number of weeks (e.g. `104` for “in the past 104 weeks”).
- The string `"lifetime_before_baseline"` (at any time before the
  candidate time zero).
- The string `"lifetime_before_and_after_baseline"` (ever, past or
  future).

The distinction between “lifetime_before” and
“lifetime_before_and_after” matters for prevalent vs incident outcomes:
a prior psychotic disorder excludes you looking backward only (because
forward is the outcome), but gender dysphoria excludes you looking both
directions (because cross-sex hormone use is a permanent state that
confounds exposure regardless of when it happened relative to your
candidate time-zero).

#### Computed confounders

Confounders with `computed: true` under `implementation` are
rolling-window indicators that swereg builds automatically from the
skeleton via
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md).
For example, “psychotropic medication use in past year” is computed as
“any TRUE in `rx_n05_n06` within the last 52 weeks at the candidate
time-zero”. Non-computed confounders read a column directly
(e.g. `rd_age_continuous` is already on the skeleton).

The `computed` flag is the same mechanism swereg uses for computed
exclusion criteria – rolling windows over existing skeleton columns,
nothing fancier.

#### Enrollments and matching

Each item under `enrollments` is one sequence of trials. They share a
global inclusion/exclusion spec but add their own `additional_inclusion`
(almost always an age range) and `additional_exclusion` (if any). The
`exposure.implementation` block names the skeleton column that
classifies exposure (`rd_approach1_single` in this example, a string
column with values like `"systemic_mht"`, `"local_mht"`, `"no_mht"`),
which value counts as the exposed arm, which value counts as the
comparator, and the per-band sampling ratio.

### Loop 1: enrollment + IPW

`plan$s1_generate_enrollments_and_ipw()` runs one iteration per
`enrollment_id` (NOT per ETT – several ETTs with different outcomes can
share the same enrollment). Each iteration spawns a callr subprocess
that:

1.  Loads the skeleton files for the batches assigned to this
    enrollment.
2.  Applies the global inclusion filter (`isoyears`) and global
    exclusion criteria via
    [`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md).
3.  Applies the enrollment’s own `additional_inclusion` /
    `additional_exclusion`.
4.  Computes any `computed: true` confounder columns via
    [`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md).
5.  Creates the `TTEEnrollment` with matching and calls `$s1_collapse()`
    to drop empty rows.
6.  Multiply-imputes missing confounder values via
    `$s2_impute_confounders()`.
7.  Fits the baseline IPW model (stabilized logistic regression of
    exposure on confounders) via `$s3_ipw()`.
8.  Truncates extreme weights at 1/99 percentiles via
    `$s4_truncate_weights()`.
9.  Saves the result as `file_imp` (`{prefix}_imp_{enrollment_id}.qs2`).

Step 2 is where swereg enforces that every skeleton consumed by the plan
is pipeline-consistent. Near the top of
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)
we call `study$assert_skeletons_consistent()`, so if any batch’s
`pipeline_hash` differs from the study’s current pipeline hash, the plan
construction errors loudly. This prevents the failure mode where you
edit a code in `ICD10_CODES`, rebuild skeletons for only some batches,
and silently run an analysis on a half-upgraded pipeline.

Steps 5-7 are all methods on `TTEEnrollment`:

``` r
enrollment <- TTEEnrollment$new(
  data   = person_week_data,
  design = my_design,
  ratio  = 2
)
enrollment$enrollment_stage   # "pre_enrollment"

# Inside TTEEnrollment$new() when `ratio` is passed:
# $enroll() is called privately -- this is where person-week
# becomes trial. `data_level` transitions "person_week" -> "trial".

enrollment$s1_collapse()      # drops empty rows
enrollment$s2_impute_confounders()
enrollment$s3_ipw()
enrollment$s4_truncate_weights()

enrollment$enrollment_stage   # "enrolled"
```

Loop 1 produces two files per enrollment_id: a `file_raw` intermediate
and a `file_imp` final. The reason for the split is that imputation is
stochastic; the `file_raw` intermediate lets you audit the raw matched
trial panel before imputation, which is useful when diagnosing
mismatches with the protocol.

### Loop 2: per-ETT outcome weighting

`plan$s2_generate_analysis_files_and_ipcw_pp()` runs one iteration per
row of `plan$ett` (one per outcome × follow-up combination). Each
iteration:

1.  Loads the `file_imp` for this row’s `enrollment_id`. Imputed
    confounders and baseline IPW weights are already on it.
2.  Prepares the outcome: joins the outcome column, censors at first
    event or end of follow-up window.
3.  Fits the IPCW-PP model (GAM/GLM of censoring on time-varying
    covariates) via `$s5_prepare_for_analysis()`. This includes
    per-protocol censoring at treatment switch, so the output is NOT an
    ITT dataset.
4.  Combines weights: `analysis_weight_pp = ipw * ipcw_pp`.
5.  Truncates and drops intermediate IPCW columns.
6.  Saves the result as `file_analysis`
    (`{prefix}_analysis_{ett_id}.qs2`).

Loop 2 runs sequentially in the main process (not parallelized), because
each ETT’s cost is usually dominated by I/O and the outcome + IPCW-PP
models are fast. If that ever stops being true the parallelization is a
straightforward extension.

### The analysis file: rates, IRRs, KM curves

After Loop 2, each `file_analysis` is a trial-level panel with truncated
combined weights ready for effect estimation. The `TTEEnrollment` R6
class provides three estimation methods that wrap
[`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html) and
[`survey::svykm`](https://rdrr.io/pkg/survey/man/svykm.html) with the
right design specification:

``` r
enrollment <- swereg::qs2_read(x_file_analysis)

# Weighted rates (per person-year) by arm, trial_id, etc.
enrollment$rates(
  weight_col = "analysis_weight_pp_trunc",
  by = c("exposure")
)

# Weighted incidence rate ratio via quasipoisson
# (pooled logistic / IRR-as-HR approximation)
enrollment$irr(
  weight_col = "analysis_weight_pp_trunc",
  formula    = outcome ~ exposure + splines::ns(tstop, df = 3) + trial_id
)

# Weighted Kaplan-Meier with person-level clustered SEs
enrollment$km(
  weight_col = "analysis_weight_pp_trunc"
)
```

The IRR approximation to the hazard ratio is used because the
alternative,
[`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html), is
computationally prohibitive at registry scale. For rare outcomes (which
dominate TTE studies) the quasi-Poisson IRR with
`splines::ns(tstop, df = 3)` is a close approximation to a
flexible-baseline Cox model (Thompson 1977).

#### Heterogeneity across trials

``` r
enrollment$heterogeneity_test()
#>   Wald test on trial_id x exposure interaction
#>   chisq = 3.21, df = 2, p = 0.20
```

Tests whether the exposure effect varies across the sequential trials. A
significant result suggests calendar-time effect modification or
changing selection on exposure initiation.

### Running the full pipeline

The typical project script has the shape:

``` r
# 1. Load the study built by the skeleton pipeline
study <- swereg::registrystudy_load(data_rawbatch_candidates)

# 2. Build the plan from the spec + study
plan <- swereg::tteplan_from_spec_and_registrystudy(
  spec  = "002-ozel-psychosis/spec_v001.yaml",
  study = study
)

# 3. Loop 1: enrollment + IPW (parallel across enrollment_ids)
plan$s1_generate_enrollments_and_ipw(n_workers = 4L)

# 4. Loop 2: per-ETT outcome weighting (sequential, fast)
plan$s2_generate_analysis_files_and_ipcw_pp()

# 5. Per-ETT analysis
for (i in seq_len(nrow(plan$ett))) {
  x_ett_id         <- plan$ett$ett_id[i]
  x_file_analysis  <- plan$ett$file_analysis[i]
  x_outcome        <- plan$ett$outcome_var[i]

  enrollment <- swereg::qs2_read(x_file_analysis)

  irr_result <- enrollment$irr(
    weight_col = "analysis_weight_pp_trunc",
    formula    = as.formula(sprintf("%s ~ exposure + splines::ns(tstop, df = 3) + trial_id", x_outcome))
  )
  # save irr_result to results/ ...
}
```

The `x_` prefix on loop-extracted variables is a swereg convention
(`x_outcome` not `outcome`) so that loop scalars are always visually
distinct from dataset columns inside the body of the loop.

### The TARGET checklist

Cashin et al. (2025) published a 21-item checklist for transparent
reporting of target trial emulations. `plan$print_target_checklist()`
generates a pre-populated version of the checklist, mapping each TARGET
item to the swereg configuration that implements it (pulled from the
spec YAML) and marking items that need manual narrative reporting.

``` r
plan$print_target_checklist()
```

This is meant to be read, edited into a manuscript supplement, and
submitted alongside the paper. It doesn’t guarantee your analysis is
correct, just that the reporting is complete.

### Reproducibility via provenance

Every plan carries references back to its skeletons. Every skeleton
carries a `pipeline_hash()` summarizing the three-phase pipeline that
produced it. Every spec file is versioned in git. Together, these give
you a reproducibility chain from final estimates back to the exact code
that built the time grid, applied the phase-3 randvars, and registered
the code entries.

Before running Loop 1, the plan asserts the skeletons are consistent
(`study$assert_skeletons_consistent()`). If that passes, you’re
guaranteed that every batch was built with the same framework +
randvars + codes configuration. If your `spec_vNNN.yaml` file is
committed and your pipeline snapshot TSV is committed, `git log` can
answer “what was different between run A and run B” without guesswork.

### Summary

- **Three R6 classes**: `TTEDesign` (schema), `TTEEnrollment` (one
  sequence of trials), `TTEPlan` (the ETT grid builder).
- **Two loops**: Loop 1 does enrollment + baseline IPW per enrollment_id
  in parallel callr workers; Loop 2 does per-protocol censoring +
  IPCW-PP per ETT sequentially.
- **Spec YAML** captures every clinical and methodological decision in a
  file that medical collaborators can review and the swereg pipeline can
  execute.
- **Provenance** – `pipeline_hash`, `assert_skeletons_consistent`,
  per-host pipeline snapshots – makes the reproducibility chain
  auditable.
- **Weights**: baseline IPW for confounder adjustment (Loop 1), IPCW-PP
  for per-protocol censoring (Loop 2), combined as
  `analysis_weight_pp = ipw * ipcw_pp`, truncated at 1/99 percentiles.
- **Estimation**: quasi-Poisson IRR approximation to the hazard ratio
  with `splines::ns(tstop, df = 3)` for flexible baseline and
  person-level clustered standard errors via `survey`.

For further reading:

- [`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md)
  – how the skeleton files consumed by a `TTEPlan` are built and
  incrementally rebuilt.
- [`vignette("tte-methodology")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methodology.md)
  – mapping to reference papers.
- [`vignette("tte-nomenclature")`](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.md)
  – one-page glossary.
- [`?TTEPlan`](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md),
  [`?TTEEnrollment`](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md),
  [`?TTEDesign`](https://papadopoulos-lab.github.io/swereg/reference/TTEDesign.md)
  – full method reference.
