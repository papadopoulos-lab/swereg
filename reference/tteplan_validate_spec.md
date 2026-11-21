# Validate spec variables against skeleton data

Checks each column, treatment value and subgroup that a parsed spec
names against one skeleton batch. It collects every issue before it
reports, then warns on the soft issues and stops on the hard issues.

## Usage

``` r
tteplan_validate_spec(spec, skeleton, skeleton_batch = 1L)
```

## Arguments

- spec:

  Parsed study specification from
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md).

- skeleton:

  A data.table skeleton (person-week panel) to validate against.

- skeleton_batch:

  Batch number of `skeleton`. The prevalent-user warning reports it, so
  a reader knows which batch the check measured.

## Value

`invisible(TRUE)` when no check stops. A warning does not change the
return value.

## The checks

A hard issue is an error, and a soft issue is a warning. Each issue
below is an error, except a declared category absent from the batch.

- Global rules: every `source_variable` column of each
  `inclusion_criteria$criteria` and `exclusion_criteria` rule MUST be in
  the skeleton.

- `additional_exclusion` rules: every `source_variable` column MUST be
  in the skeleton.

- `additional_inclusion` rules: a rule of type `has_event`,
  `no_prior_value` or `only_prior_value` MUST have every
  `source_variable` column in the skeleton. A rule of type
  `isoyear_range` names no column, so the skeleton MUST have the column
  `isoyear`. Another rule that names `implementation$variable`, such as
  `age_range`, MUST have that column in the skeleton.

- Outcomes: every `variable` column MUST be in the skeleton.

- Confounders that are not computed: the `variable` MUST be a skeleton
  column. When the confounder declares `categories`, a non-missing data
  value outside `categories` is an error. A declared category absent
  from this batch is a warning, because a small batch can lack it.

- Computed confounders (`computed: true`): every `source_variable`
  column MUST be in the skeleton. The function does not check the
  `variable`, because
  [`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md)
  creates that column in s1.

- Enrollments: the treatment `variable` MUST be a skeleton column. That
  column MUST hold the `intervention_value` and the `comparator_value`.

- Observation columns: when an enrollment's `observed_var` names a
  column, that column MUST be in the skeleton, and it MUST be logical.
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
  cannot run this check, because it reads no data. The `row_presence`
  sentinel names no column, so there is nothing to check.

- Subgroups: the `variable` MUST be a skeleton column or the `variable`
  of a computed confounder. It MUST also be the `variable` of a
  confounder. The marginal weights are valid within a stratum only when
  the PS and IPCW models include the subgroup.

## How it fails

The function stops at once when `skeleton` is not a data.table.
Otherwise it runs every check above, then it reports in this order:

1.  When there are soft issues, it warns with a numbered list, under
    `Spec validation: N warning(s)`.

2.  When there are hard issues, it stops with a numbered list, under
    `Spec validation failed: N error(s)`. So one call can warn and then
    stop.

3.  It runs the prevalent-user check below. That check can warn, and it
    can stop.

4.  It shows the message
    `Spec validation passed: N entries checked against M columns`.

## The prevalent-user check

An enrollment gets its incident-user design from a washout rule, of type
`no_prior_value` or `only_prior_value`. The check warns when no washout
covers the enrollment's intervention level. All four rule blocks are
searched: `inclusion_criteria$criteria`, `exclusion_criteria`, and the
enrollment's `additional_inclusion` and `additional_exclusion`. A
washout counts only when it names a `source_variable`, a `value` and one
`window_weeks`.

Coverage is measured on the weekly rows of `skeleton`. A weekly row has
`is_isoyear` equal to `FALSE`, and a skeleton without `is_isoyear` has
only weekly rows. A prevalent week is a week at the intervention level
that follows an earlier week of the same person at that level. A washout
covers the enrollment when it makes every prevalent week ineligible. The
check evaluates the eligibility expression the compiler builds, so it
measures the column the skeleton will hold. A first initiation stays
eligible and is not an uncovered week. The enrollment passes when at
least one washout covers it.

The washout column and the treatment column often differ. A
specification can wash out a whole drug class, then compare one sub-type
of that class against no treatment. A test on the two column names
cannot answer that, and it cannot see a washout that names the right
column at the wrong level.

The check stops in three cases:

- The skeleton has no `id` column.

- The skeleton has no `isoyearweek` column. The stop names the column
  when a weekly row is at the intervention level. Otherwise a washout
  that applies stops in data.table, with the message
  `column not found: [isoyearweek]`.

- The weekly rows of one person at an intervention level run backwards
  in `isoyearweek`. Sort the skeleton by `id` and `isoyearweek` before
  you validate it.

Set `options(swereg.warn_prevalent_user = FALSE)` to skip the check,
with its warning and its stops. Use it for a discontinuation or
switching study, which enrols prevalent users by design.

## See also

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md),
[`tteplan_excel_spec_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_excel_spec_summary.md),
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[`tteplan_osf_text()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_osf_text.md),
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
