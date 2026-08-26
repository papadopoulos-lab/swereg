# Run one target trial emulation pipeline stage

Loads the plan in `dir_tteplan`, runs the stage method, then runs the
steps that follow it. This is the body of the per-project `s1.R`, `s2.R`
and `s3.R` stage scripts as one call.

## Usage

``` r
tte_stage(stage, dir_tteplan, ...)
```

## Arguments

- stage:

  One of `"s1"`, `"s2"` or `"s3"`.

- dir_tteplan:

  Character vector of candidate directories, in priority order, where
  `tteplan.qs2` lives. Passed to
  [`tteplan_locate_and_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_locate_and_load.md),
  which takes the first one that exists.

- ...:

  Arguments for the stage method. Each one MUST be named.

## Value

The
[TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md),
invisibly.

## Details

The stage id selects the method and the steps after it:

|         |                                             |                                             |
|---------|---------------------------------------------|---------------------------------------------|
| `stage` | method                                      | steps after the method                      |
| `"s1"`  | `$s1_generate_enrollments_and_ipw()`        | `$save()`, then `$print_target_checklist()` |
| `"s2"`  | `$s2_generate_analysis_files_and_ipcw_pp()` | n/a                                         |
| `"s3"`  | `$s3_analyze()`                             | `$results_summary()`, then `$save()`        |

Every element of `...` MUST carry a name. `tte_stage()` matches each
name against the formals of the stage method, and forwards by name.

The three methods take their arguments in different orders, and none of
them takes `...`. A positional forward binds the wrong formal and
reports no error. Naming every argument is what makes the forward safe.

## Why an unknown name fails early

`tte_stage()` rejects a name that the stage method does not declare
BEFORE it calls
[`tteplan_locate_and_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_locate_and_load.md).
That load reads the plan from a network share and is slow. A mistyped
argument name therefore costs no load.
[`do.call()`](https://rdrr.io/r/base/do.call.html) would also reject the
name, but only after the load.

The formals come from the
[TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)
generator, so the check needs no plan.

## See also

[`tteplan_locate_and_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_locate_and_load.md),
[TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md).
[`vignette("tte-workflow")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md)
describes the stage scripts.

Other tte_plan:
[`registrystudy_load()`](https://papadopoulos-lab.github.io/swereg/reference/registrystudy_load.md),
[`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md),
[`tteplan_locate_and_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_locate_and_load.md)

## Examples

``` r
if (FALSE) { # \dontrun{
swereg::tte_stage(
  "s1",
  "~/plans/003-iliadis-stroke",
  n_workers = 6L,
  swereg_dev_path = NULL
)
} # }
```
