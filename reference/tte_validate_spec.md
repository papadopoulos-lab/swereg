# Validate spec variables against skeleton data

Checks that all \`implementation\$variable\` references in the spec
actually exist as columns in the skeleton data.table. For categorical
confounders, also checks that the declared categories match the data.
Collects all issues before reporting.

## Usage

``` r
tte_validate_spec(spec, skeleton)
```

## Arguments

- spec:

  Parsed study specification from \[tte_read_spec()\].

- skeleton:

  A data.table skeleton (person-week panel) to validate against.

## Value

\`invisible(TRUE)\` on success; stops with a numbered issue list if any
checks fail.

## See also

Other tte_spec:
[`tte_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_derived_confounders.md),
[`tte_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_exclusions.md),
[`tte_plan_from_spec_and_skeleton_meta()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_from_spec_and_skeleton_meta.md),
[`tte_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_read_spec.md)
