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

- Required sections: study, enrollments, outcomes, follow_up,
  min_expected_events_per_arm_per_year

- `min_expected_events_per_arm_per_year` MUST be one finite number of at
  least 0

- The retired keys `horizon_rule` and `sparse_rule` are refused. The
  message names the replacement key

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

- Each `id` MUST be one non-empty character string or one number. YAML
  reads an unquoted `01` as the number 1, so `"01"` and `01` are two
  different ids. The error names the position of each enrollment with a
  missing or bad `id`

- Each enrollment MUST have its own `id`. The error names each repeated
  id and the positions of the enrollments that carry it

- An enrollment MAY set `family`, one non-empty character string. When
  one enrollment sets it, every enrollment MUST set it

- An enrollment MAY set `minimum_events_rule_exempt`, one logical
  (`true` or `false`). An enrollment without the key is not exempt. The
  error names the enrollment id of a value that is not one logical. The
  specification exempts an enrollment from the minimum-events rule with
  `true`. s3 still computes the rule for its ETTs, and the Minimum
  events sheets show that verdict. The rule then withholds none of its
  estimates

- The spec MAY set the top-level key `feasibility_follow_up_weeks`, one
  positive whole number of weeks. A logical, a string, `NA`, 0, a
  fraction and a value of length other than 1 stop, and the error names
  the key. The value MAY be longer than every `follow_up` horizon of an
  enrollment. The ETT grid then gets one more follow-up of that length
  for that enrollment and every outcome. s2 builds these
  feasibility-horizon ETTs as any other ETT. s3 computes only the
  minimum-events rule for them, and only the Minimum events sheets show
  them. s1 enrolls each enrollment to the feasibility horizon. It
  truncates `ipw` and counts the fill over the rows up to the longest
  reported follow-up, as it does without the key. A value that is not
  longer than every `follow_up` horizon of an enrollment adds no ETT to
  that enrollment

- A `follow_up` entry MAY set `only_enrollments`, a list of enrollment
  ids. The horizon then applies only to those enrollments, and an entry
  without the key applies to every enrollment. A single id is a list of
  one. An empty list, an element that is not a string, a repeated id and
  an id that names no enrollment stop. The error names the follow-up
  label and the bad value

- A `follow_up` that is not a whole number of years takes the
  minimum-events decision of the next whole year of the same enrollment
  and outcome. When the enrollment has no follow-up of that length, the
  ETT grid adds one, and s3 computes only the rule for it

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

## The rare_condition_rule

A spec MAY declare one `rare_condition_rule` block. It turns a rare
baseline condition into an exclusion criterion of the whole study.

    rare_condition_rule:
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

## The minimum-events rule

Every spec MUST declare the flat key
`min_expected_events_per_arm_per_year`.

    min_expected_events_per_arm_per_year: 5

The value is the minimum number of expected events per arm in each
follow-up year. s3 computes the expected events under the null: the
pooled weighted rate of both arms times the weighted person-years of the
arm. The value MUST be one finite number of at least 0. The function
refuses a missing key, an empty value and a mapping. `$reload_spec()`
copies a changed value onto a cached plan, because the value does not
change who is enrolled or how swereg weights them.

## See also

[`tteplan_check_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md),
which reports every problem as a row and does not stop.

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md),
[`tteplan_excel_spec_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_excel_spec_summary.md),
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[`tteplan_osf_text()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_osf_text.md),
[`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md)
