# Validate spec variables against skeleton data

Checks that all `implementation$variable` references in the spec
actually exist as columns in the skeleton data.table. For categorical
confounders, also checks that the declared categories match the data.
Collects all issues before reporting.

## Usage

``` r
tteplan_validate_spec(spec, skeleton)
```

## Arguments

- spec:

  Parsed study specification from
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md).

- skeleton:

  A data.table skeleton (person-week panel) to validate against.

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

## See also

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md),
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
