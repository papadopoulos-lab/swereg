# Create a TTEPlan from a study specification

Builds a \[TTEPlan\] with a full ETT grid (enrollments x outcomes x
follow-up) from the parsed study specification. Also stores each
enrollment's exposure implementation details in the ETT data.table so
they are available via \`plan\[\[i\]\]\$exposure_impl\`.

## Usage

``` r
tte_plan_from_spec(spec, skeleton_files, global_max_isoyearweek)
```

## Arguments

- spec:

  Parsed study specification from \[tte_read_spec()\].

- skeleton_files:

  Character vector of skeleton file paths.

- global_max_isoyearweek:

  Administrative censoring boundary (isoyearweek string, e.g.,
  "2023-52").

## Value

A \[TTEPlan\] object with the full ETT grid.

## See also

Other tte_spec:
[`tte_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_derived_confounders.md),
[`tte_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_exclusions.md),
[`tte_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_read_spec.md)
