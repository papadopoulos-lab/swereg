# Apply exclusion criteria from a study spec to a skeleton

Applies every eligibility criterion in the parsed study specification.
Calls
[`skeleton_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_combine.md)
at the end to AND them into one `eligible` column.

## Usage

``` r
tteplan_apply_exclusions(skeleton, spec, enrollment_spec)
```

## Arguments

- skeleton:

  A data.table skeleton (person-week panel).

- spec:

  Parsed study specification from
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md).

- enrollment_spec:

  Enrollment spec from the plan (must contain `enrollment_id`), as
  returned by `plan[[i]]`.

## Value

The skeleton (modified by reference), with eligibility columns added and
a combined `eligible` column.

## Details

The function applies five groups of criteria, in this order:

- calendar years, from `inclusion_criteria$isoyears`

- global inclusion criteria, from `inclusion_criteria$criteria`

- enrollment-specific additional inclusion, such as an age range

- global exclusion criteria

- enrollment-specific additional exclusion criteria

A global inclusion criterion applies to every enrollment. It declares
`type: has_event`, and an `implementation` block that names a
`source_variable` and a `window`. It adds one
`eligible_has_<variable>_<window>` column to the skeleton.

## See also

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md),
[`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md)
