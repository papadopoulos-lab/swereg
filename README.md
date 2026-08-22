# swereg

**swereg** builds longitudinal person-week skeletons from Swedish healthcare
registry data (NPR, LMED, DORS, LISA, cancer and quality registries), then
runs target trial emulation (TTE) on them. A four-phase pipeline (framework,
trim, codes, randvars) gives each phase its own fingerprint-based
invalidation. An edit to one code entry re-applies that entry, every derived
entry that reads it, and every randvars step.

## Installation

```r
# install.packages("pak")
pak::pak("papadopoulos-lab/swereg")
```

`TTEPlan$export_tables()` draws CONSORT flowcharts with Graphviz. That path
needs `DiagrammeR`, `DiagrammeRsvg` and `rsvg`, plus the system librsvg
library (`librsvg2-dev` on Debian and Ubuntu). Without them the workbook
falls back to a text-table CONSORT.

## Quick start

```r
library(data.table)
data("fake_person_ids", package = "swereg")
data("fake_diagnoses", package = "swereg")

skeleton <- swereg::create_skeleton(
  ids      = fake_person_ids[1:100],
  date_min = "2020-01-01",
  date_max = "2022-12-31"
)

swereg::make_lowercase_names(fake_diagnoses, date_columns = "indatum")
swereg::add_diagnoses(
  skeleton,
  fake_diagnoses[lopnr %in% fake_person_ids[1:100]],
  id_name = "lopnr",
  codes = list(diabetes = c("E10", "E11"))
)

head(skeleton)
```

## Which function do I want?

| Task | Function |
|---|---|
| Build a person-week time grid by hand | `create_skeleton()` |
| Normalize column names and parse Swedish dates | `make_lowercase_names()` |
| Attach baseline or annual data | `add_onetime()`, `add_annual()` |
| Attach diagnoses, operations, prescriptions, deaths | `add_diagnoses()`, `add_operations()`, `add_rx()`, `add_cods()` |
| Capture a value at a first occurrence | `make_rowind_first_occurrence()` |
| Run the batched production pipeline | `RegistryStudy$process_skeletons()` |
| Build a TTE plan from a spec YAML | `tteplan_from_spec_and_registrystudy()` |
| Run enrollment and baseline IPW | `TTEPlan$s1_generate_enrollments_and_ipw()` |
| Run per-protocol censoring and IPCW | `TTEPlan$s2_generate_analysis_files_and_ipcw_pp()` |
| Estimate rates, IRRs and survival curves | `TTEEnrollment$rates()`, `$irr()`, `$survival_curve()` |
| Set the worker count for one pipeline stage | `default_n_workers()` |

## Two acronyms, two meanings

TTE names the method, and it is the prefix on `TTEPlan`, `TTEDesign` and
`TTEEnrollment`. One emulated target trial (ETT) is one cell of the plan
grid: one outcome, one follow-up duration, one enrollment. ETT is always
countable. Write "one ETT" or "12 ETTs".

## Documentation

Full reference, vignettes and release notes:
https://papadopoulos-lab.github.io/swereg/

Start with these vignettes, in this order. The site lists the rest.

1. [Skeleton concept](https://papadopoulos-lab.github.io/swereg/articles/skeleton-concept.html)
   and [R6 class overview](https://papadopoulos-lab.github.io/swereg/articles/r6-class-overview.html)
   for the person-week grid and the six R6 classes.
2. [Creating the skeleton](https://papadopoulos-lab.github.io/swereg/articles/skeleton-create.html)
   and [Analysing the skeleton](https://papadopoulos-lab.github.io/swereg/articles/skeleton-analyze.html)
   for the manual workflow.
3. [Skeleton pipeline](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.html)
   for the four-phase pipeline and its incremental rebuilds.
4. [TTE workflow](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.html)
   for target trial emulation from spec YAML to estimates, and
   [TTE nomenclature](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.html)
   for the glossary.

At the R console, the same seven pages are `vignette("skeleton-concept")`,
`vignette("r6-class-overview")`, `vignette("skeleton-create")`,
`vignette("skeleton-analyze")`, `vignette("skeleton-pipeline")`,
`vignette("tte-workflow")` and `vignette("tte-nomenclature")`.

## Citation

```
Papadopoulos Lab (2026). swereg: longitudinal skeletons and target
trial emulation for Swedish registry data. R package.
https://github.com/papadopoulos-lab/swereg
```
