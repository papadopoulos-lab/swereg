# Apply exclusion criteria from a study spec to a skeleton

Applies calendar year eligibility, enrollment-specific additional
inclusion (e.g., age range), global exclusion criteria, and
enrollment-specific additional exclusion criteria from the parsed study
specification. Calls \[tte_eligible_combine()\] at the end to AND all
criteria into a single \`eligible\` column.

## Usage

``` r
tte_apply_exclusions(skeleton, spec, enrollment_spec)
```

## Arguments

- skeleton:

  A data.table skeleton (person-week panel).

- spec:

  Parsed study specification from \[tte_read_spec()\].

- enrollment_spec:

  Enrollment spec from the plan (must contain \`enrollment_id\`), as
  returned by \`plan\[\[i\]\]\` or passed to the \`process_fn\`
  callback.

## Value

The skeleton (modified by reference), with eligibility columns added and
a combined \`eligible\` column.

## See also

Other tte_spec:
[`tte_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_derived_confounders.md),
[`tte_plan_from_spec_and_skeleton_meta()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_from_spec_and_skeleton_meta.md),
[`tte_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_read_spec.md),
[`tte_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_validate_spec.md)
