# Validate spec variables against skeleton data

Checks that all `implementation$variable` references in the spec
actually exist as columns in the skeleton data.table. For categorical
confounders, also checks that the declared categories match the data.
Collects all issues before reporting.

## Usage

``` r
tteplan_validate_spec(spec, skeleton, skeleton_batch = 1L)
```

## Arguments

- spec:

  Parsed study specification from
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md).

- skeleton:

  A data.table skeleton (person-week panel) to validate against.

- skeleton_batch:

  Batch number of `skeleton`. The prevalent-user warning reports it, so
  a reader knows which batch the check measured.

## Value

`invisible(TRUE)` on success; emits a warning with a numbered issue list
if any checks fail.

## Details

It also checks the observation column of every enrollment that names
one: the column MUST exist in the skeleton, and it MUST be logical.
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
cannot run that check, because it reads no data. An enrollment that
declares the `row_presence` sentinel names no column, so there is
nothing to check.

## The prevalent-user check

An enrollment gets its incident-user design from a washout exclusion, an
exclusion of type `no_prior_intervention`. The check warns when no
washout covers the enrollment's intervention level.

Coverage is containment, measured on the weekly rows of `skeleton`. A
washout names a level in `intervention_value` and one or more columns in
`source_variable`. It covers the enrollment when every weekly row at the
intervention level holds that level, in one of those columns. A missing
value counts as uncovered. A multi-source washout covers through the
union of its sources. The enrollment passes when at least one washout
covers it.

The washout column and the treatment column often differ. A
specification can wash out a whole drug class, then compare one sub-type
of that class against no treatment. A test on the two column names
cannot answer that, and it cannot see a washout that names the right
column at the wrong level.

Set `options(swereg.warn_prevalent_user = FALSE)` to silence the
warning. Use it for a discontinuation or switching study, which enrols
prevalent users by design.

## See also

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md),
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
