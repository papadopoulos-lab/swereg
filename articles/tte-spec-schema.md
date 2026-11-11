# The specification schema

## Why the schema exists

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
refuses every key the schema does not name. Without the gate, swereg
would ignore a key it does not read, and say nothing.

For example, a cohort restriction under
`inclusion_criteria$additional_inclusion` would never reach the
eligibility filter, because swereg does not read that key path. The
study population would stay unrestricted, and no message would say so.
The specification would look exactly like one that works.

One table names every legal key path. It is `.TTE_SPEC_SCHEMA`, in
`R/tteplan_spec_schema.R`, and nothing else in swereg holds that list.
Every table below is generated from that object, so the tables and the
gate use the same list.

## How a key path is written

A path is written the way the walker over the parsed YAML produces it.

- The root is `$`.
- A mapping key appends `/<key>`.
- A sequence index becomes `[]`.

A treatment block therefore has the path `$/enrollments[]/treatment`.
Every element of a sequence shares one path, so the schema states one
rule for all of them.

The path is what separates two keys that share a name.
`$/enrollments[]/additional_inclusion` is accepted.
`$/inclusion_criteria/additional_inclusion` is refused. A table keyed by
the key name alone cannot state that.

Each context declares its own children. There is no generic rule for an
`implementation` block. A treatment implementation, an outcome
implementation and a confounder implementation accept different keys.

## The two key classes

Each key path carries one class.

``` r
schema <- swereg:::.TTE_SPEC_SCHEMA

rows <- do.call(rbind, lapply(names(schema), function(context) {
  node <- schema[[context]]
  keys <- list(
    consumed = node[["consumed"]],
    metadata = node[["metadata"]]
  )
  data.frame(
    context = context,
    key = unlist(keys, use.names = FALSE),
    class = rep(names(keys), lengths(keys)),
    stringsAsFactors = FALSE
  )
}))
rows <- rows[order(rows$context, rows$key), ]

# Wrap a path in backticks. A bare `$` opens inline maths in a pipe table, and
# pandoc then merges the cells it spans.
as_code <- function(x) paste0("`", x, "`")

knitr::kable(
  as.data.frame(table(rows$class), stringsAsFactors = FALSE),
  col.names = c("class", "key paths")
)
```

| class    | key paths |
|:---------|----------:|
| consumed |       130 |
| metadata |        10 |

| class | what swereg does with the key | a specification that carries it |
|:---|:---|:---|
| `consumed` | swereg reads it. | accepted |
| `metadata` | swereg accepts it and never reads it. | accepted |

Every other key is refused. A retired key is refused the same way:
swereg keeps no list of old key names.

### Why one class is not enough

A one-class table holds “swereg reads it”, and refuses everything else.
Under that table, every key swereg does not read is refused. That table
would refuse the valid key paths below.

``` r
meta <- rows[rows$class == "metadata", ]
knitr::kable(
  data.frame(context = as_code(meta$context), key = as_code(meta$key)),
  row.names = FALSE
)
```

| context | key |
|:---|:---|
| `$/open_questions[]` | `resolution` |
| `$/standing_methods` | `admin_censoring` |
| `$/standing_methods` | `comparator_to_intervention_ratio_default` |
| `$/standing_methods` | `matching_ratio_default` |
| `$/standing_methods/admin_censoring` | `handling` |
| `$/standing_methods/admin_censoring` | `note` |
| `$/standing_methods/comparator_to_intervention_ratio_default` | `handling` |
| `$/standing_methods/comparator_to_intervention_ratio_default` | `note` |
| `$/standing_methods/matching_ratio_default` | `handling` |
| `$/standing_methods/matching_ratio_default` | `note` |

Each one records a decision for a human reader.
`$/open_questions[]/resolution` records how the study team settled an
open question. `$/standing_methods/admin_censoring` records the
administrative censoring rule. `R/` names both only in the schema, and
reads neither value. The `metadata` class lets the gate refuse an
unknown key and accept these.

## Every declared key path

The schema declares 140 key paths across 39 mapping contexts.

``` r
knitr::kable(
  data.frame(
    context = as_code(rows$context),
    key = as_code(rows$key),
    class = rows$class
  ),
  row.names = FALSE
)
```

| context | key | class |
|:---|:---|:---|
| `$` | `admin_censoring_isoyearweek` | consumed |
| `$` | `confounders` | consumed |
| `$` | `enrollments` | consumed |
| `$` | `exclusion_criteria` | consumed |
| `$` | `feasibility_follow_up_weeks` | consumed |
| `$` | `follow_up` | consumed |
| `$` | `inclusion_criteria` | consumed |
| `$` | `min_expected_events_per_arm_per_year` | consumed |
| `$` | `open_questions` | consumed |
| `$` | `outcomes` | consumed |
| `$` | `period_width` | consumed |
| `$` | `rare_condition_rule` | consumed |
| `$` | `standing_methods` | consumed |
| `$` | `study` | consumed |
| `$` | `subgroups` | consumed |
| `$` | `target_trial` | consumed |
| `$/confounders[]` | `categories` | consumed |
| `$/confounders[]` | `codes` | consumed |
| `$/confounders[]` | `implementation` | consumed |
| `$/confounders[]` | `name` | consumed |
| `$/confounders[]` | `rationale` | consumed |
| `$/confounders[]/implementation` | `computed` | consumed |
| `$/confounders[]/implementation` | `source_variable` | consumed |
| `$/confounders[]/implementation` | `variable` | consumed |
| `$/confounders[]/implementation` | `window` | consumed |
| `$/enrollments[]` | `additional_exclusion` | consumed |
| `$/enrollments[]` | `additional_inclusion` | consumed |
| `$/enrollments[]` | `comparator_tolerance_weeks` | consumed |
| `$/enrollments[]` | `family` | consumed |
| `$/enrollments[]` | `id` | consumed |
| `$/enrollments[]` | `intervention_tolerance_weeks` | consumed |
| `$/enrollments[]` | `minimum_events_rule_exempt` | consumed |
| `$/enrollments[]` | `name` | consumed |
| `$/enrollments[]` | `observed_var` | consumed |
| `$/enrollments[]` | `treatment` | consumed |
| `$/enrollments[]/additional_exclusion[]` | `implementation` | consumed |
| `$/enrollments[]/additional_exclusion[]` | `name` | consumed |
| `$/enrollments[]/additional_exclusion[]` | `rationale` | consumed |
| `$/enrollments[]/additional_exclusion[]/implementation` | `computed` | consumed |
| `$/enrollments[]/additional_exclusion[]/implementation` | `source_variable` | consumed |
| `$/enrollments[]/additional_exclusion[]/implementation` | `type` | consumed |
| `$/enrollments[]/additional_exclusion[]/implementation` | `value` | consumed |
| `$/enrollments[]/additional_exclusion[]/implementation` | `window` | consumed |
| `$/enrollments[]/additional_inclusion[]` | `implementation` | consumed |
| `$/enrollments[]/additional_inclusion[]` | `max` | consumed |
| `$/enrollments[]/additional_inclusion[]` | `min` | consumed |
| `$/enrollments[]/additional_inclusion[]` | `name` | consumed |
| `$/enrollments[]/additional_inclusion[]` | `rationale` | consumed |
| `$/enrollments[]/additional_inclusion[]` | `type` | consumed |
| `$/enrollments[]/additional_inclusion[]/implementation` | `computed` | consumed |
| `$/enrollments[]/additional_inclusion[]/implementation` | `source_variable` | consumed |
| `$/enrollments[]/additional_inclusion[]/implementation` | `type` | consumed |
| `$/enrollments[]/additional_inclusion[]/implementation` | `value` | consumed |
| `$/enrollments[]/additional_inclusion[]/implementation` | `variable` | consumed |
| `$/enrollments[]/additional_inclusion[]/implementation` | `window` | consumed |
| `$/enrollments[]/observed_var` | `column` | consumed |
| `$/enrollments[]/observed_var` | `sentinel` | consumed |
| `$/enrollments[]/treatment` | `arms` | consumed |
| `$/enrollments[]/treatment` | `description` | consumed |
| `$/enrollments[]/treatment` | `implementation` | consumed |
| `$/enrollments[]/treatment/arms` | `comparator` | consumed |
| `$/enrollments[]/treatment/arms` | `intervention` | consumed |
| `$/enrollments[]/treatment/implementation` | `comparator_to_intervention_ratio` | consumed |
| `$/enrollments[]/treatment/implementation` | `comparator_value` | consumed |
| `$/enrollments[]/treatment/implementation` | `intervention_value` | consumed |
| `$/enrollments[]/treatment/implementation` | `seed` | consumed |
| `$/enrollments[]/treatment/implementation` | `variable` | consumed |
| `$/exclusion_criteria[]` | `implementation` | consumed |
| `$/exclusion_criteria[]` | `name` | consumed |
| `$/exclusion_criteria[]` | `rationale` | consumed |
| `$/exclusion_criteria[]/implementation` | `computed` | consumed |
| `$/exclusion_criteria[]/implementation` | `source_variable` | consumed |
| `$/exclusion_criteria[]/implementation` | `type` | consumed |
| `$/exclusion_criteria[]/implementation` | `value` | consumed |
| `$/exclusion_criteria[]/implementation` | `window` | consumed |
| `$/follow_up[]` | `label` | consumed |
| `$/follow_up[]` | `only_enrollments` | consumed |
| `$/follow_up[]` | `weeks` | consumed |
| `$/inclusion_criteria` | `criteria` | consumed |
| `$/inclusion_criteria` | `isoyears` | consumed |
| `$/inclusion_criteria/criteria[]` | `implementation` | consumed |
| `$/inclusion_criteria/criteria[]` | `name` | consumed |
| `$/inclusion_criteria/criteria[]` | `rationale` | consumed |
| `$/inclusion_criteria/criteria[]` | `type` | consumed |
| `$/inclusion_criteria/criteria[]/implementation` | `computed` | consumed |
| `$/inclusion_criteria/criteria[]/implementation` | `source_variable` | consumed |
| `$/inclusion_criteria/criteria[]/implementation` | `type` | consumed |
| `$/inclusion_criteria/criteria[]/implementation` | `value` | consumed |
| `$/inclusion_criteria/criteria[]/implementation` | `window` | consumed |
| `$/open_questions[]` | `question` | consumed |
| `$/open_questions[]` | `raised_by` | consumed |
| `$/open_questions[]` | `resolution` | metadata |
| `$/open_questions[]` | `status` | consumed |
| `$/outcomes[]` | `description` | consumed |
| `$/outcomes[]` | `implementation` | consumed |
| `$/outcomes[]` | `name` | consumed |
| `$/outcomes[]` | `role` | consumed |
| `$/outcomes[]/implementation` | `variable` | consumed |
| `$/rare_condition_rule` | `candidates` | consumed |
| `$/rare_condition_rule` | `decided_by` | consumed |
| `$/rare_condition_rule` | `min_n` | consumed |
| `$/standing_methods` | `admin_censoring` | metadata |
| `$/standing_methods` | `calendar_time` | consumed |
| `$/standing_methods` | `comparator_to_intervention_ratio_default` | metadata |
| `$/standing_methods` | `matching_ratio_default` | metadata |
| `$/standing_methods/admin_censoring` | `handling` | metadata |
| `$/standing_methods/admin_censoring` | `note` | metadata |
| `$/standing_methods/calendar_time` | `handling` | consumed |
| `$/standing_methods/calendar_time` | `note` | consumed |
| `$/standing_methods/comparator_to_intervention_ratio_default` | `handling` | metadata |
| `$/standing_methods/comparator_to_intervention_ratio_default` | `note` | metadata |
| `$/standing_methods/matching_ratio_default` | `handling` | metadata |
| `$/standing_methods/matching_ratio_default` | `note` | metadata |
| `$/study` | `description` | consumed |
| `$/study` | `design` | consumed |
| `$/study` | `implementation` | consumed |
| `$/study` | `principal_investigator` | consumed |
| `$/study` | `title` | consumed |
| `$/study/implementation` | `conf_level` | consumed |
| `$/study/implementation` | `date` | consumed |
| `$/study/implementation` | `project_prefix` | consumed |
| `$/study/implementation` | `status` | consumed |
| `$/study/implementation` | `version` | consumed |
| `$/subgroups[]` | `implementation` | consumed |
| `$/subgroups[]` | `name` | consumed |
| `$/subgroups[]/implementation` | `variable` | consumed |
| `$/target_trial` | `analysis_plan` | consumed |
| `$/target_trial` | `assignment_procedure` | consumed |
| `$/target_trial` | `causal_contrast` | consumed |
| `$/target_trial` | `eligibility_criteria` | consumed |
| `$/target_trial` | `follow_up_period` | consumed |
| `$/target_trial` | `outcome` | consumed |
| `$/target_trial` | `treatment_strategies` | consumed |
| `$/target_trial/analysis_plan` | `specification` | consumed |
| `$/target_trial/assignment_procedure` | `specification` | consumed |
| `$/target_trial/causal_contrast` | `specification` | consumed |
| `$/target_trial/eligibility_criteria` | `specification` | consumed |
| `$/target_trial/follow_up_period` | `specification` | consumed |
| `$/target_trial/outcome` | `specification` | consumed |
| `$/target_trial/treatment_strategies` | `specification` | consumed |

## The design keys

Every specification MUST declare two flat top-level keys. They are
design choices, so the specification holds them and no function argument
does.

``` yaml
period_width: 4
admin_censoring_isoyearweek: "2025-01"
```

| Key | Meaning | Type | Required |
|:---|:---|:---|:---|
| `period_width` | The width in weeks of the enrollment period. The same width sets each follow-up interval. | One whole number of at least 1 | Yes |
| `admin_censoring_isoyearweek` | The administrative censoring week. Follow-up includes that week. | One string `YYYY-WW`, with the week from 01 to 53 | Yes |

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
stops on a specification without one or both keys, and on a bad value.
The one message names both keys, states each problem and shows the
example above.
[`tteplan_check_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md)
reports the same message as one `read_error` row on the path `$`. s0
reads both values from the specification. It does not compare the week
with the skeleton.

`$/standing_methods/admin_censoring` stays an optional `metadata` note.
swereg does not read it. When the note states a week, keep it equal to
`admin_censoring_isoyearweek`.

## The global inclusion container

`inclusion_criteria` is a fixed container. It holds `isoyears` and
`criteria`, and nothing else.

``` yaml
inclusion_criteria:
  isoyears: [2010, 2023]
  criteria:
    - name: "Recorded gender dysphoria diagnosis"
      rationale: "The study population is people with the diagnosis"
      type: has_event
      implementation:
        source_variable: osd_f64
        window: lifetime_before_baseline
```

Each entry declares one type, and `inclusion_criteria$criteria` accepts
three. It declares `type: has_event` as its own key, one level above
`implementation:`. It declares a washout under `implementation:`
instead, as `type: no_prior_value` or `type: only_prior_value`.
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
refuses any other type, and it refuses an entry that declares both an
outer type and an inner one. A criterion swereg reads and ignores never
restricts the study population, and it looks exactly like one that does.

[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md)
adds one eligibility column for each criterion, and the criterion’s type
sets the column name.

- `has_event` gives `eligible_has_<variable>_<window>`.
- `no_prior_value` gives `eligible_no_<variable>_<window>`.
- `only_prior_value` gives `eligible_only_<variable>_<window>`.

[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md)
then combines every eligibility column into `eligible`.

### How it differs from a per-enrollment `additional_inclusion`

|  | `inclusion_criteria$criteria` | `enrollments[]$additional_inclusion` |
|:---|:---|:---|
| scope | every enrollment | the one enrollment that declares it |
| path | `$/inclusion_criteria/criteria[]` | `$/enrollments[]/additional_inclusion[]` |
| types on the criterion | `has_event` | `has_event`, `age_range`, `isoyear_range` |
| types under `implementation:` | `no_prior_value`, `only_prior_value` | `no_prior_value`, `only_prior_value` |

A `has_event` entry generates the same column name in both places. Two
entries that generate one column name are one criterion written twice,
so
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
refuses the pair. That holds when both entries are global, and when one
is global and one is per-enrollment.

`inclusion_criteria$additional_inclusion` is refused. That name belongs
to an enrollment. In the container, it names a path that swereg does not
read.

An `isoyear_range` entry limits one enrollment to trials registered in a
range of ISO years.

``` yaml
additional_inclusion:
  - name: "Trial registration 2008-2016"
    type: isoyear_range
    min: 2008
    max: 2016
```

[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md)
writes the column `eligible_isoyears_<min>_<max>`, here
`eligible_isoyears_2008_2016`. An `age_range` in the same enrollment
stays in force beside it.
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
refuses the entry in these cases:

- `min` or `max` is missing, or is not one whole number.
- `min` is above `max`.
- The range is not inside `inclusion_criteria$isoyears`.
- The enrollment already declares an `isoyear_range`.
- The entry is in `inclusion_criteria$criteria`.

The range limits trial registration only, and follow-up continues past
`max`. A 4-week enrollment period that crosses `min` or `max` recruits
only from its weeks inside the range.

An enrollment takes at most one `age_range`. Every `age_range` writes
the one column `eligible_age`, so a second entry would replace the
first.
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
refuses the second entry.

## What each rule computes

The schema says which keys a specification may carry. This section says
what a rule built from them computes. Its tables come from
`.TTE_SPEC_RULE_FORMS` and `.TTE_SPEC_WINDOW_FORMS`, in
`R/tteplan_spec_forms.R`. Each worked case below is run when this
vignette is built. Its example is read by
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md),
and then compiled by
[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md)
or
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md).
The printed column is the code’s own output.
`tests/testthat/test-tteplan-spec-forms.R` checks the same cases, and it
fails when the code accepts a rule type that the registry does not hold.

Every eligibility column is combined with AND into `eligible`. A
person-week enters a trial only when every column is TRUE.

### The forms

| form | type | accepted in | column written |
|:---|:---|:---|:---|
| Calendar years | (none) | `inclusion_criteria$isoyears` | `eligible_isoyears` |
| An event before baseline | `has_event` | `inclusion_criteria$criteria`, `enrollments[]$additional_inclusion` | `eligible_has_<variable>_<window>` |
| An age band | `age_range` | `enrollments[]$additional_inclusion` | `eligible_age`, one per enrollment |
| A calendar range for one enrollment | `isoyear_range` | `enrollments[]$additional_inclusion` | `eligible_isoyears_<min>_<max>` |
| No earlier week at a value | `no_prior_value` | `inclusion_criteria$criteria`, `enrollments[]$additional_inclusion`, `exclusion_criteria`, `enrollments[]$additional_exclusion` | `eligible_no_<variable>_<window>` |
| Every observed earlier week at a value | `only_prior_value` | `inclusion_criteria$criteria`, `enrollments[]$additional_inclusion`, `exclusion_criteria`, `enrollments[]$additional_exclusion` | `eligible_only_<variable>_<window>` |
| No event before baseline | (none) | `exclusion_criteria`, `enrollments[]$additional_exclusion` | `eligible_no_<variable>_<window>` |
| No event at any time | (none) | `exclusion_criteria`, `enrollments[]$additional_exclusion` | `eligible_no_<variable>_lifetime_before_and_after_baseline` |
| Any of several columns | (none) | `exclusion_criteria`, `enrollments[]$additional_exclusion` | The form’s pattern, with `<variable>` the joined name |
| A computed confounder | (none) | `confounders` | `rd_no_<variable>_<window>` |

#### Calendar years

- **Source:** The skeleton’s `isoyear` column.
- **Computes:** TRUE in a week whose ISO year lies from the first to the
  second year, both included.
- **Unobserved weeks:** A week’s own ISO year decides. No other week is
  read.

``` yaml
isoyears: [2019, 2019]
```

| isoyearweek | eligible_isoyears |
|:------------|:------------------|
| 2018-49     | FALSE             |
| 2018-50     | FALSE             |
| 2018-51     | FALSE             |
| 2018-52     | FALSE             |
| 2019-01     | TRUE              |
| 2019-02     | TRUE              |
| 2019-03     | TRUE              |
| 2019-04     | TRUE              |

#### An event before baseline

- **Source:** A logical column. A list of columns is combined with OR.
- **Computes:** TRUE in a week when at least one earlier week inside the
  window is TRUE. The week itself is never in its own window.
- **Unobserved weeks:** A week with no row is not in the window. An NA
  week follows [`any()`](https://rdrr.io/r/base/any.html): the result is
  NA when the window holds an NA week and no TRUE week.

``` yaml
name: "Recorded diagnosis before baseline"
type: has_event
implementation:
  source_variable: osd_f64
  window: lifetime_before_baseline
```

| isoyearweek | osd_f64 | eligible_has_osd_f64_everbefore |
|:------------|:--------|:--------------------------------|
| 2018-49     | FALSE   | FALSE                           |
| 2018-50     | FALSE   | FALSE                           |
| 2018-51     | TRUE    | FALSE                           |
| 2018-52     | FALSE   | TRUE                            |
| 2019-01     | FALSE   | TRUE                            |
| 2019-02     | FALSE   | TRUE                            |
| 2019-03     | FALSE   | TRUE                            |
| 2019-04     | FALSE   | TRUE                            |

#### An age band

- **Source:** A numeric column, normally `rd_age_continuous`.
- **Computes:** TRUE in a week whose value is from `min` to `max`, both
  included. The value is compared as stored. On a continuous age,
  `max: 54` excludes an age of 54.5.
- **Unobserved weeks:** A week’s own value decides. No other week is
  read.

``` yaml
name: "Age 40-54"
type: age_range
min: 40
max: 54
implementation:
  variable: rd_age_continuous
```

| isoyearweek | rd_age_continuous | eligible_age |
|:------------|------------------:|:-------------|
| 2018-49     |              39.9 | FALSE        |
| 2018-50     |              40.0 | TRUE         |
| 2018-51     |              40.5 | TRUE         |
| 2018-52     |              50.0 | TRUE         |
| 2019-01     |              54.0 | TRUE         |
| 2019-02     |              54.1 | FALSE        |
| 2019-03     |              54.9 | FALSE        |
| 2019-04     |              55.0 | FALSE        |

#### A calendar range for one enrollment

- **Source:** The skeleton’s `isoyear` column.
- **Computes:** TRUE in a week whose ISO year lies from `min` to `max`,
  both included. The range MUST lie inside
  `inclusion_criteria$isoyears`.
- **Unobserved weeks:** A week’s own ISO year decides. No other week is
  read.

``` yaml
name: "Trial registration in 2019"
type: isoyear_range
min: 2019
max: 2019
```

| isoyearweek | eligible_isoyears_2019_2019 |
|:------------|:----------------------------|
| 2018-49     | FALSE                       |
| 2018-50     | FALSE                       |
| 2018-51     | FALSE                       |
| 2018-52     | FALSE                       |
| 2019-01     | TRUE                        |
| 2019-02     | TRUE                        |
| 2019-03     | TRUE                        |
| 2019-04     | TRUE                        |

#### No earlier week at a value

- **Source:** One column, compared with `value`. Normally a character
  column.
- **Computes:** TRUE in a week when no earlier week inside the window
  holds `value`. The week itself is never in its own window.
- **Unobserved weeks:** A week with no row, and an NA week, hold no
  value, so neither breaks the rule.

``` yaml
name: "No earlier treatment"
implementation:
  type: no_prior_value
  source_variable: rd_tx
  value: treated
  window: lifetime_before_baseline
```

| isoyearweek | rd_tx   | eligible_no_rd_tx_everbefore |
|:------------|:--------|:-----------------------------|
| 2018-49     | control | TRUE                         |
| 2018-50     | NA      | TRUE                         |
| 2018-51     | control | TRUE                         |
| 2018-52     | treated | TRUE                         |
| 2019-01     | control | FALSE                        |
| 2019-02     | control | FALSE                        |
| 2019-03     | control | FALSE                        |
| 2019-04     | control | FALSE                        |

#### Every observed earlier week at a value

- **Source:** One column, compared with `value`. Normally a character
  column.
- **Computes:** TRUE in a week when every earlier week inside the window
  that holds a value holds `value`. The week itself is never in its own
  window.
- **Unobserved weeks:** A week with no row, and an NA week, hold no
  value, so neither breaks the rule. A window with no observed week
  therefore passes: the rule does not require any week at `value`.

``` yaml
name: "Treated in every week of the 2 weeks before baseline"
implementation:
  type: only_prior_value
  source_variable: rd_tx
  value: treated
  window: 2
```

| isoyearweek | rd_tx   | eligible_only_rd_tx_2wk |
|:------------|:--------|:------------------------|
| 2018-49     | treated | TRUE                    |
| 2018-50     | treated | TRUE                    |
| 2018-51     | control | TRUE                    |
| 2018-52     | treated | FALSE                   |
| 2019-01     | treated | FALSE                   |
| 2019-02     | treated | TRUE                    |
| 2019-03     | NA      | TRUE                    |
| 2019-04     | treated | TRUE                    |

#### No event before baseline

- **Source:** A logical column. A list of columns is combined with OR.
- **Computes:** TRUE in a week when no earlier week inside the window is
  TRUE. The week itself is never in its own window.
- **Unobserved weeks:** A week with no row is not in the window. An NA
  week follows [`any()`](https://rdrr.io/r/base/any.html): the result is
  NA when the window holds an NA week and no TRUE week.

``` yaml
name: "No cancer in the 2 weeks before baseline"
implementation:
  source_variable: osd_c
  window: 2
```

| isoyearweek | osd_c | eligible_no_osd_c_2wk |
|:------------|:------|:----------------------|
| 2018-49     | FALSE | TRUE                  |
| 2018-50     | FALSE | TRUE                  |
| 2018-51     | TRUE  | TRUE                  |
| 2018-52     | FALSE | FALSE                 |
| 2019-01     | FALSE | FALSE                 |
| 2019-02     | FALSE | TRUE                  |
| 2019-03     | FALSE | TRUE                  |
| 2019-04     | FALSE | TRUE                  |

#### No event at any time

- **Source:** A logical column. It MUST NOT be an outcome variable,
  because this window reads the weeks after time zero.
- **Computes:** FALSE in every week of a person when any of the person’s
  weeks is TRUE, before or after baseline. It removes the whole person.
- **Unobserved weeks:** An NA week is skipped (`any(na.rm = TRUE)`).

``` yaml
name: "Record flagged in the delivery"
implementation:
  source_variable: ri_flagged
  window: lifetime_before_and_after_baseline
```

| isoyearweek | ri_flagged | eligible_no_ri_flagged_lifetime_before_and_after_baseline |
|:---|:---|:---|
| 2018-49 | FALSE | FALSE |
| 2018-50 | FALSE | FALSE |
| 2018-51 | FALSE | FALSE |
| 2018-52 | FALSE | FALSE |
| 2019-01 | FALSE | FALSE |
| 2019-02 | FALSE | FALSE |
| 2019-03 | FALSE | FALSE |
| 2019-04 | TRUE | FALSE |

#### Any of several columns

- **Source:** A list of logical columns. swereg ORs them into one column
  named after all of them, joined by `__`. The same list works in a
  `has_event` rule and in a computed confounder. Do not give a washout a
  list.
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
  accepts it, and the compiler then stops with a data.table error,
  because OR does not apply to a character column.
- **Computes:** As for the form that holds the list, on the combined
  column.
- **Unobserved weeks:** As for the form that holds the list.

``` yaml
name: "No cancer before baseline, in either register"
implementation:
  source_variable: [osd_c, can_c]
  window: lifetime_before_baseline
```

| isoyearweek | osd_c | can_c | eligible_no_osd_c\_\_can_c_everbefore |
|:------------|:------|:------|:--------------------------------------|
| 2018-49     | FALSE | FALSE | TRUE                                  |
| 2018-50     | FALSE | FALSE | TRUE                                  |
| 2018-51     | FALSE | TRUE  | TRUE                                  |
| 2018-52     | FALSE | FALSE | FALSE                                 |
| 2019-01     | FALSE | FALSE | FALSE                                 |
| 2019-02     | FALSE | FALSE | FALSE                                 |
| 2019-03     | FALSE | FALSE | FALSE                                 |
| 2019-04     | FALSE | FALSE | FALSE                                 |

#### A computed confounder

- **Source:** A logical column. A list of columns is combined with OR.
- **Computes:** TRUE in a week when NO earlier week inside the window is
  TRUE. The name says so: `rd_no_`. swereg names the column, and a
  `variable` key on a computed confounder is replaced. `computed: true`
  is what makes it computed. Every rule block accepts the same key, and
  no rule reads it.
- **Unobserved weeks:** A week with no row is not in the window. An NA
  week follows [`any()`](https://rdrr.io/r/base/any.html): the result is
  NA when the window holds an NA week and no TRUE week.

``` yaml
name: "Diabetes in the 2 weeks before baseline"
implementation:
  computed: true
  source_variable: osd_e11
  window: 2
```

| isoyearweek | osd_e11 | rd_no_osd_e11_2wk |
|:------------|:--------|:------------------|
| 2018-49     | FALSE   | TRUE              |
| 2018-50     | FALSE   | TRUE              |
| 2018-51     | TRUE    | TRUE              |
| 2018-52     | FALSE   | FALSE             |
| 2019-01     | FALSE   | FALSE             |
| 2019-02     | FALSE   | TRUE              |
| 2019-03     | FALSE   | TRUE              |
| 2019-04     | FALSE   | TRUE              |

### The windows

A window counts ISO weeks on the calendar, not rows. A week with no row
therefore still takes up its place in the window.

| written | in the column name | accepted in | meaning |
|:---|:---|:---|:---|
| `lifetime_before_baseline` | `everbefore` | every rule with a window, and every computed confounder | Every earlier week of the person. |
| `12` | `12wk` | every rule with a window, and every computed confounder | The 12 ISO weeks before the week, counted on the calendar and not on the rows. A bare number is weeks. |
| `2 years` | `104wk` | every rule with a window, and every computed confounder | A whole number of years, with `year` or `years`. One year is 52 weeks, so the column name gives the weeks. |
| `lifetime_before_and_after_baseline` | `lifetime_before_and_after_baseline` | `exclusion_criteria` and `additional_exclusion`, on a non-washout | Every week of the person, after baseline included. |
| `12.5` | `12wk` | every rule with a window, with no message | A fraction is truncated to whole weeks: 12.5 reads as 12. Write a whole number. |
| `52 weeks` |  | refused | A unit other than `year` is refused. Write the bare number 52. |
| `6 months` |  | refused | Months are refused. Write the weeks as a bare number. |
| `1.5 years` |  | refused | A fraction of a year is refused. Write the weeks, 78. |

- An inclusion rule MAY leave out `window`. It then reads
  `lifetime_before_baseline`.
- An exclusion rule and a computed confounder MUST state `window`.
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
  stops without one.
- Every window ends at the week before the week it is evaluated in. No
  window ends earlier, and none starts at the week itself.

### What a specification cannot express

- **A count or a duration.** No rule counts weeks, so “at least 12 weeks
  of treatment” has no form. `only_prior_value` over a window of 12
  weeks comes closest, with the limit that its row states. Anything else
  needs a column built in the skeleton.
- **AND across columns.** A list of columns is combined with OR only.
  Two rules give AND, because every eligibility column is combined with
  AND.
- **A threshold on a number.** Only `age_range` compares a number, and
  only against `min` and `max`. Any other threshold needs a logical
  column built in the skeleton.
- **A window that ends before the week before baseline**, such as a lag
  of 4 weeks. Every window ends at the week before.
- **Information after baseline**, except the person-level window, which
  removes the whole person. No inclusion rule reads a week after the
  week it is evaluated in.
- **A derived treatment variable.** `treatment$implementation$variable`
  MUST name a skeleton column, and it is read as it stands.
- **A derived outcome**, other than a list of outcome columns, which is
  combined with OR.

A column built in the skeleton is the route for each of these. A rule
then reads that column as it reads any other.

## The enrollment family

An enrollment MAY set `family`, a short label that groups enrollments in
the results workbook.

``` yaml
enrollments:
  - id: "01"
    name: "Statin initiation vs none"
    family: "Main"
  - id: "02"
    name: "Statin initiation vs none, age 54-60"
    family: "Sens: age 54-60"
```

The label changes no cohort, model or estimate. `$export_tables()` shows
it in the `Family` column of the Summary sheet. `$reload_spec()` applies
a new `family` as a label change, so the cached results stay valid.

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
stops, and
[`tteplan_check_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md)
reports a row of kind `enrollment_family`, in two cases:

- A `family` is not one non-empty character string. Quote a value that
  YAML reads as a number, such as `family: "2008"`.
- Some enrollments set `family`, and others do not. When one enrollment
  sets it, every enrollment MUST set it.

## The minimum-events exemption

An enrollment MAY set `minimum_events_rule_exempt`, one logical. With
`true`, the minimum-events rule withholds no estimate of that
comparison. An enrollment without the key is not exempt.

``` yaml
enrollments:
  - id: "18"
    name: "Statin initiation vs none"
    minimum_events_rule_exempt: true
```

s3 still computes the rule for each ETT of the enrollment and stores its
verdict. The Minimum events sheets show that verdict, and the comparison
name ends with `(exempt)`. `$reload_spec()` applies a changed value as a
cosmetic change, so s1 and s2 output stays valid. `$export()`,
`$export_tables()` and `$print_target_checklist()` then stop until
`$s3_analyze()` reruns.

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
stops, and
[`tteplan_check_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md)
reports a row of kind `enrollment_rule_exempt`, when the value is not
one logical. A number, a string, a null value, `NA` and a sequence are
refused, and the message names the enrollment id.

## The feasibility horizon

A spec MAY set the top-level key `feasibility_follow_up_weeks`, one
positive whole number of weeks. It adds a horizon at which s3 computes
only the minimum-events rule.

``` yaml
feasibility_follow_up_weeks: 260
```

The value MAY be longer than every horizon of an enrollment. The ETT
grid then gets one more ETT of that length for that enrollment and each
outcome. s1 enrolls the enrollment to that horizon. Only the two Minimum
events sheets show these ETTs. A value that is not longer than every
horizon of an enrollment adds no ETT to it. `$reload_spec()` treats a
change of the key as structural.

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
stops, and
[`tteplan_check_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md)
reports a row of kind `feasibility_follow_up_weeks`, when the value is
not one positive whole number. A logical, a string, a null value, `NA`,
0, a negative number, a fraction, an infinite number and a sequence are
refused. The message names the key.

## A horizon for some comparisons only

A `follow_up` entry MAY set `only_enrollments`, a list of enrollment
ids. The horizon then applies only to those enrollments. An entry
without the key applies to every enrollment.

``` yaml
follow_up:
  - { label: "6 months", weeks: 26 }
  - { label: "1 year",   weeks: 52 }
  - { label: "3 years",  weeks: 156, only_enrollments: [M01] }
```

The ETT grid holds the 156-week horizon for enrollment M01 and for no
other enrollment, and a dropped ETT takes no ETT id. YAML reads
`only_enrollments: M01` and `only_enrollments: [M01]` as the same value,
so a single id is a list of one. `$reload_spec()` treats a change of the
key as structural.

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
stops, and
[`tteplan_check_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md)
reports a row of kind `follow_up_only_enrollments`, in these cases:

- The list is empty.
- An element is not a string. Quote an id that YAML reads as a number.
- The list repeats an id.
- An id names no enrollment of the spec.

The message names the follow-up label and the bad value.

`only_enrollments` cannot name the feasibility horizon of
`feasibility_follow_up_weeks`. That horizon is added to an enrollment
when it is longer than every horizon of that enrollment.

The 26-week horizon above is not a whole number of years. It takes the
minimum-events decision of the 52-week horizon of the same comparison
and outcome. Section 1.8.6 of
[`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md)
states that rule.

## What you see when a key is refused

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
reports every refused key in one message, and reads no data first. Each
finding lists the keys its context accepts.

``` r
spec_path <- file.path(tempdir(), "spec_v001.yaml")
writeLines(
  c(
    "inclusion_criteria:",
    "  isoyears: [2010, 2023]",
    "  additional_inclusion:",
    "    - name: \"Recorded gender dysphoria diagnosis\"",
    "      type: has_event",
    "      implementation:",
    "        source_variable: osd_f64",
    "        window: lifetime_before_baseline"
  ),
  spec_path
)
msg <- tryCatch(swereg::tteplan_read_spec(spec_path), error = conditionMessage)
cat(sub(tempdir(), "<tempdir>", msg, fixed = TRUE))
#> <tempdir>/spec_v001.yaml carries 1 key swereg does not accept.
#>   $/inclusion_criteria/additional_inclusion
#>     Unknown key 'additional_inclusion'. $/inclusion_criteria accepts: criteria, isoyears.
```

The report names one key, not six. The walk stops at a refused key,
because the schema declares no context below it. Move the entries into
`criteria`, and swereg reads each one.

A misspelt key gets the same message.

``` r
writeLines(
  c("inclusion_criteria:", "  isoyear: 2010"),
  spec_path
)
msg <- tryCatch(swereg::tteplan_read_spec(spec_path), error = conditionMessage)
cat(sub(tempdir(), "<tempdir>", msg, fixed = TRUE))
#> <tempdir>/spec_v001.yaml carries 1 key swereg does not accept.
#>   $/inclusion_criteria/isoyear
#>     Unknown key 'isoyear'. $/inclusion_criteria accepts: criteria, isoyears.
```

## Where the gate runs

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
calls the gate on the specification as written, before any
normalisation. swereg writes derived keys back into the specification as
it reads it: `window_weeks`, `source_variable_combined`,
`variable_combined`, and the two-field `observed_var`. The schema names
none of them, because the schema describes the input.

`R/` holds two calls to
[`yaml::yaml.load()`](https://yaml.r-lib.org/reference/yaml.load.html).
One is in
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md).
Three functions read a specification through it:
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[`tteplan_excel_spec_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_excel_spec_summary.md)
and the plan’s own `$reload_spec()` method. The other call is in
[`tteplan_check_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md),
which returns problems and no specification. No path into a
specification skips the gate.

## Check a specification without stopping

[`tteplan_check_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md)
reads one specification and returns a data.table with one row per
problem. It reports the keys the gate refuses, and it continues past
them. It also reports these problems:

- An enrollment `id` is missing, or is not one non-empty character
  string or one number.
- Two or more enrollments carry the same `id`.
- `study$implementation$version` differs from the `spec_vNNN` in the
  file name.

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
stops on a missing, bad or repeated `id`, because `$reload_spec()` and
the Summary sheet find an enrollment by its `id`. It does not compare
the version with the file name.
[`?tteplan_check_spec`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_check_spec.md)
lists the eleven problem kinds.

## See also

- [`vignette("tte-workflow", package = "swereg")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md)
  for the whole specification, and for the pipeline that reads it.
- [`?tteplan_read_spec`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
  for the observation contract and the window conversion rules.
