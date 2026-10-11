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

A nested list representing the parsed specification, with `window_weeks`
fields added to exclusion criteria and confounders.

## Details

Window conversion rules:

- `"lifetime_before_baseline"` -\> `Inf`

- `"N year"` or `"N years"` -\> `N * 52`

Validation checks:

- Required sections: study, enrollments, outcomes, follow_up

- `study$implementation$project_prefix` must exist

- Each entry in `inclusion_criteria$criteria` must declare
  `implementation$source_variable`, and must generate an eligibility
  column name no earlier entry generates. It declares
  `type: "has_event"` as its own key, or a washout type under
  `implementation`

- A washout declares `implementation$type`, and the two types are
  `no_prior_value` and `only_prior_value`

- Each exclusion criterion must have `implementation$source_variable`

- An exclusion criterion MUST NOT have the window
  `lifetime_before_and_after_baseline` on a variable of an outcome. That
  window reads the weeks after time zero, so the criterion selects on
  the follow-up outcome. The rule covers the global `exclusion_criteria`
  and each enrollment's `additional_exclusion`

- Each outcome must have `implementation$variable`

- Each enrollment must have `id`, `treatment$implementation$variable`
  and `treatment$implementation$seed`

- Computed confounders must have `implementation$source_variable`

Warns about open questions with `status: "open"`.

## The observation contract

An enrollment states how the data records that a person was under
observation in a week. It is a flat key on the enrollment,
`observed_var`, and it takes exactly one of two forms.

    observed_var: {column: rd_observed}      # a real logical person-week column
    observed_var: {sentinel: row_presence}   # the skeleton is trimmed

The `row_presence` sentinel asserts that the caller already deleted
every unobserved person-week. A row then exists if and only if the
person was observed that week. Use it when the skeleton already deletes
every person-week the person was not under observation. The production
skeleton is one example. It deletes every person-week up to and
including first immigration, every person-week on or after emigration,
and every person-week after death. It keeps the death week itself. A
real `observed` column there would hold `TRUE` on every retained row. It
could not represent an absent week. Row presence as a silent proxy stays
forbidden. The sentinel is what makes the assumption explicit and
testable.

Two flat sibling keys carry the arm tolerances:
`intervention_tolerance_weeks` and `comparator_tolerance_weeks`. Each
MUST be a whole number of at least 0. Each defaults to 0.

Every enrollment MUST declare `observed_var`. There is no exemption for
an older spec. A spec that cannot say who was under observation carries
the immortal-time defect silently. It looks exactly like a spec that
can. To migrate a spec, copy it to a new version and add the key to
every enrollment. Never edit a released spec version: that version is
the record of what produced a run.

The function rejects a declaration that gives both `column` and
`sentinel`, a declaration that gives neither, and a sentinel name swereg
does not know. It cannot check that a named column exists and is
logical, because it reads no data.
[`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md)
runs that check against the skeleton.

## The sparse_rule

A spec MAY declare one `sparse_rule` block. It turns a rare baseline
condition into an exclusion criterion of the whole study.

    sparse_rule:
      min_n: 100
      decided_by: ["03"]
      candidates: [rd_no_osd_i63_everbefore]

- `min_n` MUST be one whole number from 1 to 2147483647, the largest R
  integer.

- `decided_by` MUST name one or more enrollments of the spec. Quote
  every id: YAML reads an unquoted `01` as the integer 1, and the parser
  refuses it.

- Each entry of `candidates` MUST be the `implementation$variable` of a
  confounder with `computed: true`.

s1 counts the persons who hold each candidate condition at baseline, per
arm, in each driving enrollment. A person counts once per arm when any
of her eligible person-trials in that arm holds the condition at its
recruiting week. A condition whose smallest count is below `min_n` is
excluded in every enrollment, and leaves every confounder set. Exclusion
is per person-trial: s1 drops a person-trial when the condition holds at
that trial's own recruiting week. Every other candidate stays a
confounder. The rule is decided from baseline counts in the eligible
population, before the comparator draw and without follow-up outcomes.
[TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)
stores the decision in its `sparse_decision` field.

## See also

[`tteplan_check_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md),
which reports every problem as a row and does not stop.

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md),
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md)
