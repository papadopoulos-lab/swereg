# Apply exclusion criteria from a study spec to a skeleton

Applies calendar year eligibility, enrollment-specific additional
inclusion (e.g., age range), global exclusion criteria, and
enrollment-specific additional exclusion criteria from the parsed study
specification. Calls \[skeleton_eligible_combine()\] at the end to AND
all criteria into a single \`eligible\` column.

## Usage

``` r
tteplan_apply_exclusions(skeleton, spec, enrollment_spec)
```

## Arguments

- skeleton:

  A data.table skeleton (person-week panel).

- spec:

  Parsed study specification from \[tteplan_read_spec()\].

- enrollment_spec:

  Enrollment spec from the plan (must contain \`enrollment_id\`), as
  returned by \`plan\[\[i\]\]\`.

## Value

The skeleton (modified by reference), with eligibility columns added and
a combined \`eligible\` column.

## See also

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md),
[`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md)
