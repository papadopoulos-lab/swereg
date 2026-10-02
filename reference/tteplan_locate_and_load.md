# Locate and load a TTEPlan from candidate directories

Walks `candidate_dir_tteplan` to find the first directory that exists on
the current host, then loads `tteplan.qs2` from inside it via
[`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md).
The one-line convenience that `s1.R` / `s2.R` / `s3.R` / `s4_export.R`
stage scripts call to obtain a plan with all directories already
resolved.

## Usage

``` r
tteplan_locate_and_load(
  candidate_dir_tteplan,
  candidate_dir_spec = NULL,
  candidate_dir_results = NULL
)
```

## Arguments

- candidate_dir_tteplan:

  Character vector of candidate directories, in priority order, where
  `tteplan.qs2` might live.

- candidate_dir_spec:

  Optional character vector. When given, it replaces the spec candidates
  stored in the plan.

- candidate_dir_results:

  Optional character vector. When given, it replaces the results
  candidates stored in the plan.

## Value

A
[TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)
with CandidatePath caches cleared and `skeleton_files` refreshed from
the embedded `registrystudy`.

## Details

The plan stores the spec and results candidates that were passed when it
was built. Those name the checkout that built it. A later stage that
runs from another checkout MUST pass its own, or it reads that
checkout's spec and writes its results there. Both replacements live on
the loaded plan, so a later `$save()` writes them into `tteplan.qs2`.

## See also

[`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md),
[`first_existing_path()`](https://papadopoulos-lab.github.io/swereg/reference/first_existing_path.md)

Other tte_plan:
[`registrystudy_load()`](https://papadopoulos-lab.github.io/swereg/reference/registrystudy_load.md),
[`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md),
[`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md)
