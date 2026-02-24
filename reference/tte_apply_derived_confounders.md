# Compute derived confounder columns from a study spec

For confounders with \`implementation\$computed: true\`, computes
rolling window indicators using
\[tte_eligible_no_events_in_window_excluding_wk0()\]. Requires
\`implementation\$source_variable\` and \`window\` to be set.

## Usage

``` r
tte_apply_derived_confounders(skeleton, spec)
```

## Arguments

- skeleton:

  A data.table skeleton (person-week panel).

- spec:

  Parsed study specification from \[tte_read_spec()\].

## Value

The skeleton (modified by reference), with derived confounder columns
added.

## See also

Other tte_spec:
[`tte_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_exclusions.md),
[`tte_plan_from_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_from_spec.md),
[`tte_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_read_spec.md)
