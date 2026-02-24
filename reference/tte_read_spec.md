# Read and validate a YAML study specification

Parses a YAML study specification file, converts human-readable windows
(e.g., "lifetime", "3 years") to weeks, and validates that all required
fields are present.

## Usage

``` r
tte_read_spec(spec_path)
```

## Arguments

- spec_path:

  Path to the YAML specification file.

## Value

A nested list representing the parsed specification, with
\`window_weeks\` fields added to exclusion criteria and confounders.

## Details

Window conversion rules:

- \`"lifetime"\` -\> \`Inf\`

- \`"N year"\` or \`"N years"\` -\> \`N \* 52\`

Validation checks:

- Required sections: study, population, enrollments, outcomes, follow_up

- \`study\$implementation\$project_prefix\` must exist

- Each exclusion criterion must have \`implementation\$variable\`

- Each outcome must have \`implementation\$variable\`

- Each enrollment must have \`id\` and
  \`exposure\$implementation\$variable\`

- Computed confounders must have \`implementation\$source_variable\`

Warns about open questions with \`status: "open"\`.

## See also

Other tte_spec:
[`tte_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_derived_confounders.md),
[`tte_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tte_apply_exclusions.md),
[`tte_plan_from_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tte_plan_from_spec.md)
