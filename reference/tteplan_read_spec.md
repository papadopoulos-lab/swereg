# Read and validate a YAML study specification

Parses a YAML study specification file, converts human-readable windows
(e.g., "lifetime_before_baseline", "3 years") to weeks, and validates
that all required fields are present.

## Usage

``` r
tteplan_read_spec(spec_path)
```

## Arguments

- spec_path:

  Path to the YAML specification file.

## Value

A nested list representing the parsed specification, with
\`window_weeks\` fields added to exclusion criteria and confounders.

## Details

Window conversion rules:

- \`"lifetime_before_baseline"\` -\> \`Inf\`

- \`"N year"\` or \`"N years"\` -\> \`N \* 52\`

Validation checks:

- Required sections: study, enrollments, outcomes, follow_up

- \`study\$implementation\$project_prefix\` must exist

- Each exclusion criterion must have \`implementation\$source_variable\`

- Each outcome must have \`implementation\$variable\`

- Each enrollment must have \`id\` and
  \`treatment\$implementation\$variable\`

- Computed confounders must have \`implementation\$source_variable\`

Warns about open questions with \`status: "open"\`.

## The observation contract

An enrollment states how the data records that a person was under
observation in a week. It is a flat key on the enrollment,
\`observed_var\`, and it takes exactly one of two forms.

    observed_var: {column: rd_observed}      # a real logical person-week column
    observed_var: {sentinel: row_presence}   # the skeleton is trimmed

The \`row_presence\` sentinel asserts that the caller already deleted
every unobserved person-week. A row then exists if and only if the
person was observed that week. Use it when the skeleton already deletes
every person-week the person was not under observation. The production
skeleton is one example. It deletes every person-week up to and
including first immigration, every person-week on or after emigration,
and every person-week after death. It keeps the death week itself. A
real \`observed\` column there would hold \`TRUE\` on every retained
row. It could not represent an absent week. Row presence as a silent
proxy stays forbidden. The sentinel is what makes the assumption
explicit and testable.

Two flat sibling keys carry the arm tolerances:
\`intervention_tolerance_weeks\` and \`comparator_tolerance_weeks\`.
Each MUST be a whole number of at least 0. Each defaults to 0.

Every enrollment MUST declare \`observed_var\`. There is no exemption
for an older spec. A spec that cannot say who was under observation
carries the immortal-time defect silently. It looks exactly like a spec
that can. To migrate a spec, copy it to a new version and add the key to
every enrollment. Never edit a released spec version: that version is
the record of what produced a run.

The function rejects a declaration that gives both \`column\` and
\`sentinel\`, a declaration that gives neither, and a sentinel name
swereg does not know. It cannot check that a named column exists and is
logical, because it reads no data. \[tteplan_validate_spec()\] runs that
check against the skeleton.

## See also

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md),
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md)
