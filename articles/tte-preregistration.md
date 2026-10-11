# TTE preregistration: the OSF form

## When to register

Do the [OSF Preregistration](https://osf.io/registries) when the
specification YAML is final and the shared skeletons exist. s0 built the
plan, you generated the Excel specification from it, and no stage after
s0 has run.

Every answer of the form then comes from two files. These are the
specification YAML, `spec_vNNN.yaml`, and the Excel specification,
`spec_vNNN.xlsx`. After s0 the Excel specification holds two sheets,
“Study Specification” and “Target trial protocol”. The protocol sheet
documents one ETT, the first of the grid unless `protocol_ett_id` names
another. Once s1 has run, a “Sparse conditions” sheet follows the Study
Specification sheet when the spec has a `sparse_rule`. This article maps
each OSF section to a YAML key and to a section of the Study
Specification sheet.

## Produce the Excel specification

`$excel_spec_summary()` writes the Excel specification from the plan
that s0 builds. Run
[`tteplan_check_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md)
on the YAML first. It reads the YAML and no data.

``` r
swereg::tteplan_check_spec("/path/to/spec/spec_v001.yaml")

study <- swereg::registrystudy_load("/path/to/meta")
plan <- swereg::tteplan_from_spec_and_registrystudy(
  study = study,
  candidate_dir_spec = "/path/to/spec",
  candidate_dir_tteplan = "/path/to/plans/my_study",
  candidate_dir_results = "/path/to/results",
  spec_version = "v001"
)
plan$excel_spec_summary()
plan$print_target_checklist()
```

s0 reads the specification YAML and the registry codes of the
`RegistryStudy`. It also loads the first skeleton batch to validate the
specification:

- It checks that each named column exists. It reads the outcome columns
  by name only, and reads no outcome value.
- It reads the values of the treatment, washout and categorical
  confounder columns, and the type of the observation column.
- It takes the last week of the data as the administrative end.

A failed check stops s0 or gives a warning. A message can state a count
of prevalent-user weeks, or a category value that the data hold and the
specification lacks. You MAY pass `global_max_isoyearweek` to set the
administrative end yourself, and s0 then skips this validation.

Register after s0 and before s1.

## The OSF sections, field by field

The YAML column names a key of the specification.
[`vignette("tte-spec-schema")`](https://papadopoulos-lab.github.io/swereg/articles/tte-spec-schema.md)
lists every legal key. The Excel column names a section of the Study
Specification sheet, and a row label where one applies. TARGET items
come from `$print_target_checklist()` on the same plan.

| OSF section                                     | YAML key                                                                                                                                             | Excel specification                                                                                                             |
|:------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------|
| Research questions or hypotheses                | `study$description`, `outcomes[]`, `enrollments[]$treatment$arms`                                                                                    | Outcomes; Enrollments, rows “Treatment:” and “Comparator:”                                                                      |
| Foreknowledge of data or evidence               | n/a                                                                                                                                                  | n/a. See “Foreknowledge”.                                                                                                       |
| Study type                                      | Non-randomized study. Adjustment or control-based design: the treatment and censoring weights.                                                       | n/a                                                                                                                             |
| Intention for causal interpretation             | The estimands of TARGET item 6f                                                                                                                      | n/a                                                                                                                             |
| Blinding of experimental treatments             | No blinding is involved.                                                                                                                             | n/a                                                                                                                             |
| Additional blinding during research or analysis | n/a                                                                                                                                                  | n/a. See “Foreknowledge”.                                                                                                       |
| Study design                                    | `target_trial`, `enrollments[]`, `follow_up[]`                                                                                                       | Study, row “Design:”; Follow-up; Enrollments; the Target trial protocol sheet. TARGET items 6a to 6h and 7.                     |
| Randomization                                   | Not applicable. The comparator draw is seeded sampling, with `enrollments[]$treatment$implementation$seed`.                                          | n/a                                                                                                                             |
| Data collection procedures                      | `inclusion_criteria`, `exclusion_criteria`, `enrollments[]$additional_inclusion`, `enrollments[]$additional_exclusion`, `enrollments[]$observed_var` | Inclusion criteria (global); Exclusion criteria (global); Enrollments, rows “Additional inclusion:” and “Additional exclusion:” |
| Sample size                                     | n/a                                                                                                                                                  | n/a. See “Sample size”.                                                                                                         |
| Sample size rationale                           | `enrollments[]$treatment$implementation$comparator_to_intervention_ratio`                                                                            | Enrollments, row “Comparator-to-intervention ratio:”. See “Sample size”.                                                        |
| Starting and stopping rules                     | `inclusion_criteria$isoyears`, `follow_up[]$weeks`, the `global_max_isoyearweek` argument of s0                                                      | Inclusion criteria (global), row “Isoyears:”; Follow-up; Study, row “Admin censoring:”                                          |
| Manipulated variables                           | None                                                                                                                                                 | n/a                                                                                                                             |
| Measured variables                              | `enrollments[]$treatment`, `outcomes[]`, `confounders[]`, the criteria, `subgroups[]`                                                                | Enrollments; Outcomes; Confounders; the criteria sections. The sheet does not list `subgroups[]`.                               |
| Indices                                         | `confounders[]` with `implementation$computed: true`, and their `source_variable` and `window`                                                       | Confounders, rows “Variable:” and “Window:”                                                                                     |
| Statistical models                              | Fixed by swereg                                                                                                                                      | [`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md), section 1. TARGET item 6h.      |
| Transformations                                 | n/a                                                                                                                                                  | See “Transformations”.                                                                                                          |
| Inference criteria                              | `study$implementation$conf_level`, default 0.95                                                                                                      | [`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md), section 1.8.6                   |
| Data inclusion and exclusion                    | The criteria of “Data collection procedures”, and `sparse_rule`                                                                                      | Confounders, rows of “Rare-condition rule (sparse_rule)”. TARGET items 6a and 7. See “The sparse_rule”.                         |
| Missing data                                    | Fixed rules, and the `impute_fn` argument of s1                                                                                                      | [`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md), section 1.8.7. TARGET item 11.  |
| Other planned analyses                          | `subgroups[]`, the untruncated weights, and the intention-to-treat estimand beside the per-protocol one                                              | [`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md), sections 1.8.8 and 1.9          |
| Context and additional information              | `study$implementation$version`                                                                                                                       | Study, row “Version:”. See “Deviations after registration”.                                                                     |

### Statistical models

Section 1 of
[`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md)
states the estimators as swereg implements them. Copy it without change,
so the protocol matches the code. Then state each value of its list
“Values your protocol must state”.

### Transformations

swereg does not transform a confounder. The treatment weight model and
the censoring models hold each confounder as one main-effect term, with
no interaction and no spline. A numeric column enters as one linear
term. A logical column enters as one term for `TRUE`. A character or
factor column enters through the default treatment coding of R, so its
first level is the reference.

Derive any other coding in the skeleton, and register the derived
column. Section 1.8.9 of
[`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md)
states the time terms of the outcome and censoring models.

### Inference criteria

The intervals of the rate ratio and of the risk difference use
`study$implementation$conf_level`. Standard errors are clustered on the
person. swereg applies no correction for multiple outcomes or multiple
comparisons. State your own rule in the form.

## The sparse_rule

A `sparse_rule` block in the specification is a pre-specified rule. Each
candidate confounder held by fewer than `min_n` persons in the smallest
arm of the driving comparisons becomes an exclusion criterion of every
comparison. The other candidates stay confounders. Each candidate is the
variable of a confounder with `computed: true`.

``` yaml
confounders:
  - name: "Prior ischaemic stroke"
    implementation:
      source_variable: osd_i63
      window: lifetime_before_baseline
      computed: true

sparse_rule:
  min_n: 100
  decided_by: ["01"]
  candidates: [rd_no_osd_i63_everbefore]
```

`decided_by` names the driving comparisons. The rule is decided from
baseline counts in the eligible population, before the comparator draw
and without follow-up outcomes. s1 makes that decision.

At registration, nobody knows which conditions the rule excludes.
Register the threshold, the driving comparisons and the candidates. The
Excel specification states the rule and the row “Decision:” reads “not
decided yet (s1 has not run)”. TARGET item 6a gives the same rule as
prose. Paste it into “Data inclusion and exclusion”.

## Sample size

At registration there are no study counts. Base the rationale on
external information, such as a published incidence, or an earlier
extraction that you document as such.

The minimum detectable effect (MDE) columns of the `$export_tables()`
workbook are not a preregistration input. swereg computes them after s3,
from the study’s own events.

To plan from external numbers, apply the naive Poisson formula that the
MDE columns use:

``` r
mde <- function(rate0, py0, py1, power = 0.8, conf_level = 0.95) {
  e0 <- rate0 * py0
  e1 <- rate0 * py1
  se <- sqrt(1 / e1 + 1 / e0)
  k <- qnorm(1 - (1 - conf_level) / 2) + qnorm(power)
  round(c(
    comparator_events = e0,
    intervention_events = e1,
    mde_protective = exp(-k * se),
    mde_harmful = exp(k * se)
  ), 2)
}

# External rate: 4 events per 1,000 person-years in the comparator arm.
# 3,000 intervention person-trials and 6,000 comparator person-trials,
# with a 5-year horizon.
mde(rate0 = 4 / 1000, py0 = 6000 * 5, py1 = 3000 * 5)
#>   comparator_events intervention_events      mde_protective         mde_harmful 
#>              120.00               60.00                0.64                1.56
```

The person-years in this example are the person-trials times the
horizon. That is an upper bound, because loss, death and the end of data
stop follow-up early. The formula ignores the weights, and the repeated
contributions of one person to many trials. So it can differ from the
detectable effect of the weighted estimator with person-clustered
standard errors.

## Foreknowledge

At registration the authors ran s0 only, and s0 reads no outcome value.
State that under “Explanation of foreknowledge”.

After registration, s1 reads outcome data. A person enters a trial only
if they reach time zero free of every outcome of the enrollment. So
every s1 count from the attrition step “Event before time zero” onwards
depends on outcome events before time zero. The s1 panel files,
`file_raw` and `file_imp`, also hold the follow-up outcomes. Do not open
those files before the analysis. If an author sees an outcome-dependent
result before the analysis, record it in the deviations log.

## Deviations after registration

After registration, a change to the specification is a new specification
version, with a new Excel specification:

1.  Copy the registered `spec_vNNN.yaml` to the next version number.
2.  Make the change, and set `study$implementation$version` and `date`.
3.  Build the plan from s0, and write the new Excel specification with
    `$excel_spec_summary()`.
4.  Write one log entry: the date, the change and the reason.

Each version writes to its own results directory, because
`plan$dir_results` appends the version. s0 stops when
`study$implementation$version` differs from its `spec_version` argument.
Do not edit the registered version. Compare two versions with
`git diff --no-index` on their YAML files. Report the log with the
paper.
