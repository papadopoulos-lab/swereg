# Create a TTEPlan from a study specification

Builds a \[TTEPlan\] with a full ETT grid (enrollments x outcomes x
follow-up) from the parsed study specification. Also stores each
enrollment's exposure implementation details in the ETT data.table so
they are available via \`plan\[\[i\]\]\$exposure_impl\`.

## Usage

``` r
tte_plan_from_spec_and_registrystudy(
  spec,
  study,
  n_skeleton_files = NULL,
  global_max_isoyearweek = NULL
)
```

## Arguments

- spec:

  Character path to a YAML spec file, or a parsed spec list from
  \[tte_read_spec()\]. When a path is given, the version extracted from
  the filename (\`\_vNNN.yaml\`) is validated against
  \`spec\$study\$implementation\$version\`.

- study:

  A \[RegistryStudy\] object. The \`\$skeleton_files\` active binding is
  used to obtain the skeleton file paths.

- n_skeleton_files:

  Optional integer: if not NULL, only the first \`n_skeleton_files\`
  files are used (for faster dev iterations).

- global_max_isoyearweek:

  Administrative censoring boundary (isoyearweek string, e.g.,
  "2023-52"). If \`NULL\` (default), auto-detected from
  \`max(isoyearweek)\` in the first skeleton file. Also runs
  \[tte_validate_spec()\] on that skeleton.

## Value

A \[TTEPlan\] object with the full ETT grid.

## See also

Other tte_spec:
[`tte_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_derived_confounders.md),
[`tte_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_exclusions.md),
[`tte_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_read_spec.md),
[`tte_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_validate_spec.md)
