# The specification schema

## Why the schema exists

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
refuses every key the schema does not name. Before that, a key swereg
did not read was ignored in silence.

A collaborator wrote a cohort restriction under
`inclusion_criteria$additional_inclusion`. swereg read
`inclusion_criteria` for `isoyears` and nothing else. The restriction
never reached the eligibility filter. The study population stayed
unrestricted, and no message said so. The specification looked exactly
like one that worked.

One table names every legal key path. It is `.TTE_SPEC_SCHEMA`, in
`R/tteplan_spec_schema.R`, and nothing else in swereg holds that list.
Every table below is generated from that object, so no second copy of
the list can drift away from it.

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

## The three key classes

Each key path carries one class.

``` r
schema <- swereg:::.TTE_SPEC_SCHEMA

rows <- do.call(rbind, lapply(names(schema), function(context) {
  node <- schema[[context]]
  keys <- list(
    consumed = node[["consumed"]],
    metadata = node[["metadata"]],
    legacy = names(node[["legacy"]])
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
| consumed |       115 |
| legacy   |        15 |
| metadata |        10 |

| class      | what swereg does with the key                         | a specification that carries it |
|:-----------|:------------------------------------------------------|:--------------------------------|
| `consumed` | swereg reads it.                                      | accepted                        |
| `metadata` | swereg accepts it and never reads it.                 | accepted                        |
| `legacy`   | swereg refuses it. The message names the replacement. | refused                         |

### Why two classes are not enough

A two-class table holds “swereg reads it” and “swereg refuses it”. Under
that table, every key swereg does not read is refused. These key paths
are then refused, and every one of them is correct today.

``` r
meta <- rows[rows$class == "metadata", ]
knitr::kable(
  data.frame(context = as_code(meta$context), key = as_code(meta$key)),
  row.names = FALSE
)
```

| context                                                       | key                                        |
|:--------------------------------------------------------------|:-------------------------------------------|
| `$/open_questions[]`                                          | `resolution`                               |
| `$/standing_methods`                                          | `admin_censoring`                          |
| `$/standing_methods`                                          | `comparator_to_intervention_ratio_default` |
| `$/standing_methods`                                          | `matching_ratio_default`                   |
| `$/standing_methods/admin_censoring`                          | `handling`                                 |
| `$/standing_methods/admin_censoring`                          | `note`                                     |
| `$/standing_methods/comparator_to_intervention_ratio_default` | `handling`                                 |
| `$/standing_methods/comparator_to_intervention_ratio_default` | `note`                                     |
| `$/standing_methods/matching_ratio_default`                   | `handling`                                 |
| `$/standing_methods/matching_ratio_default`                   | `note`                                     |

Each one records a decision for a human reader.
`$/open_questions[]/resolution` records how the study team settled an
open question. `$/standing_methods/admin_censoring` records the
administrative censoring rule. `R/` names both only in the schema, and
reads neither value. The `metadata` class is what lets the gate refuse
an unknown key without refusing these.

## Every declared key path

The schema declares 140 key paths across 41 mapping contexts.

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

| context                                                       | key                                        | class    |
|:--------------------------------------------------------------|:-------------------------------------------|:---------|
| `$`                                                           | `confounders`                              | consumed |
| `$`                                                           | `enrollments`                              | consumed |
| `$`                                                           | `exclusion_criteria`                       | consumed |
| `$`                                                           | `follow_up`                                | consumed |
| `$`                                                           | `inclusion_criteria`                       | consumed |
| `$`                                                           | `open_questions`                           | consumed |
| `$`                                                           | `outcomes`                                 | consumed |
| `$`                                                           | `standing_methods`                         | consumed |
| `$`                                                           | `study`                                    | consumed |
| `$`                                                           | `subgroups`                                | consumed |
| `$`                                                           | `target_trial`                             | consumed |
| `$/confounders[]`                                             | `categories`                               | consumed |
| `$/confounders[]`                                             | `codes`                                    | consumed |
| `$/confounders[]`                                             | `implementation`                           | consumed |
| `$/confounders[]`                                             | `name`                                     | consumed |
| `$/confounders[]`                                             | `rationale`                                | consumed |
| `$/confounders[]/implementation`                              | `computed`                                 | consumed |
| `$/confounders[]/implementation`                              | `source_variable`                          | consumed |
| `$/confounders[]/implementation`                              | `variable`                                 | consumed |
| `$/confounders[]/implementation`                              | `window`                                   | consumed |
| `$/enrollments[]`                                             | `additional_exclusion`                     | consumed |
| `$/enrollments[]`                                             | `additional_inclusion`                     | consumed |
| `$/enrollments[]`                                             | `comparator_tolerance_weeks`               | consumed |
| `$/enrollments[]`                                             | `id`                                       | consumed |
| `$/enrollments[]`                                             | `intervention_tolerance_weeks`             | consumed |
| `$/enrollments[]`                                             | `name`                                     | consumed |
| `$/enrollments[]`                                             | `observed_var`                             | consumed |
| `$/enrollments[]`                                             | `treatment`                                | consumed |
| `$/enrollments[]/additional_exclusion[]`                      | `implementation`                           | consumed |
| `$/enrollments[]/additional_exclusion[]`                      | `name`                                     | consumed |
| `$/enrollments[]/additional_exclusion[]`                      | `rationale`                                | consumed |
| `$/enrollments[]/additional_exclusion[]/implementation`       | `computed`                                 | consumed |
| `$/enrollments[]/additional_exclusion[]/implementation`       | `intervention_value`                       | consumed |
| `$/enrollments[]/additional_exclusion[]/implementation`       | `source_variable`                          | consumed |
| `$/enrollments[]/additional_exclusion[]/implementation`       | `type`                                     | consumed |
| `$/enrollments[]/additional_exclusion[]/implementation`       | `window`                                   | consumed |
| `$/enrollments[]/additional_inclusion[]`                      | `implementation`                           | consumed |
| `$/enrollments[]/additional_inclusion[]`                      | `max`                                      | consumed |
| `$/enrollments[]/additional_inclusion[]`                      | `min`                                      | consumed |
| `$/enrollments[]/additional_inclusion[]`                      | `name`                                     | consumed |
| `$/enrollments[]/additional_inclusion[]`                      | `rationale`                                | consumed |
| `$/enrollments[]/additional_inclusion[]`                      | `type`                                     | consumed |
| `$/enrollments[]/additional_inclusion[]/implementation`       | `computed`                                 | consumed |
| `$/enrollments[]/additional_inclusion[]/implementation`       | `source_variable`                          | consumed |
| `$/enrollments[]/additional_inclusion[]/implementation`       | `variable`                                 | consumed |
| `$/enrollments[]/additional_inclusion[]/implementation`       | `window`                                   | consumed |
| `$/enrollments[]/observed_var`                                | `column`                                   | consumed |
| `$/enrollments[]/observed_var`                                | `sentinel`                                 | consumed |
| `$/enrollments[]/treatment`                                   | `arms`                                     | consumed |
| `$/enrollments[]/treatment`                                   | `description`                              | consumed |
| `$/enrollments[]/treatment`                                   | `implementation`                           | consumed |
| `$/enrollments[]/treatment/arms`                              | `comparator`                               | consumed |
| `$/enrollments[]/treatment/arms`                              | `intervention`                             | consumed |
| `$/enrollments[]/treatment/implementation`                    | `comparator_to_intervention_ratio`         | consumed |
| `$/enrollments[]/treatment/implementation`                    | `comparator_value`                         | consumed |
| `$/enrollments[]/treatment/implementation`                    | `intervention_value`                       | consumed |
| `$/enrollments[]/treatment/implementation`                    | `matching_ratio`                           | legacy   |
| `$/enrollments[]/treatment/implementation`                    | `seed`                                     | consumed |
| `$/enrollments[]/treatment/implementation`                    | `variable`                                 | consumed |
| `$/exclusion_criteria[]`                                      | `implementation`                           | consumed |
| `$/exclusion_criteria[]`                                      | `name`                                     | consumed |
| `$/exclusion_criteria[]`                                      | `rationale`                                | consumed |
| `$/exclusion_criteria[]/implementation`                       | `computed`                                 | consumed |
| `$/exclusion_criteria[]/implementation`                       | `intervention_value`                       | consumed |
| `$/exclusion_criteria[]/implementation`                       | `source_variable`                          | consumed |
| `$/exclusion_criteria[]/implementation`                       | `type`                                     | consumed |
| `$/exclusion_criteria[]/implementation`                       | `window`                                   | consumed |
| `$/follow_up[]`                                               | `label`                                    | consumed |
| `$/follow_up[]`                                               | `weeks`                                    | consumed |
| `$/inclusion_criteria`                                        | `additional_inclusion`                     | legacy   |
| `$/inclusion_criteria`                                        | `criteria`                                 | consumed |
| `$/inclusion_criteria`                                        | `implementation`                           | legacy   |
| `$/inclusion_criteria`                                        | `isoyears`                                 | consumed |
| `$/inclusion_criteria`                                        | `name`                                     | legacy   |
| `$/inclusion_criteria`                                        | `rationale`                                | legacy   |
| `$/inclusion_criteria/additional_inclusion[]`                 | `implementation`                           | legacy   |
| `$/inclusion_criteria/additional_inclusion[]`                 | `name`                                     | legacy   |
| `$/inclusion_criteria/additional_inclusion[]`                 | `rationale`                                | legacy   |
| `$/inclusion_criteria/additional_inclusion[]`                 | `type`                                     | legacy   |
| `$/inclusion_criteria/additional_inclusion[]/implementation`  | `computed`                                 | legacy   |
| `$/inclusion_criteria/additional_inclusion[]/implementation`  | `source_variable`                          | legacy   |
| `$/inclusion_criteria/additional_inclusion[]/implementation`  | `window`                                   | legacy   |
| `$/inclusion_criteria/criteria[]`                             | `implementation`                           | consumed |
| `$/inclusion_criteria/criteria[]`                             | `name`                                     | consumed |
| `$/inclusion_criteria/criteria[]`                             | `rationale`                                | consumed |
| `$/inclusion_criteria/criteria[]`                             | `type`                                     | consumed |
| `$/inclusion_criteria/criteria[]/implementation`              | `computed`                                 | consumed |
| `$/inclusion_criteria/criteria[]/implementation`              | `source_variable`                          | consumed |
| `$/inclusion_criteria/criteria[]/implementation`              | `window`                                   | consumed |
| `$/inclusion_criteria/implementation`                         | `computed`                                 | legacy   |
| `$/inclusion_criteria/implementation`                         | `source_variable`                          | legacy   |
| `$/inclusion_criteria/implementation`                         | `window`                                   | legacy   |
| `$/open_questions[]`                                          | `question`                                 | consumed |
| `$/open_questions[]`                                          | `raised_by`                                | consumed |
| `$/open_questions[]`                                          | `resolution`                               | metadata |
| `$/open_questions[]`                                          | `status`                                   | consumed |
| `$/outcomes[]`                                                | `description`                              | consumed |
| `$/outcomes[]`                                                | `implementation`                           | consumed |
| `$/outcomes[]`                                                | `name`                                     | consumed |
| `$/outcomes[]`                                                | `role`                                     | consumed |
| `$/outcomes[]/implementation`                                 | `variable`                                 | consumed |
| `$/standing_methods`                                          | `admin_censoring`                          | metadata |
| `$/standing_methods`                                          | `calendar_time`                            | consumed |
| `$/standing_methods`                                          | `comparator_to_intervention_ratio_default` | metadata |
| `$/standing_methods`                                          | `matching_ratio_default`                   | metadata |
| `$/standing_methods/admin_censoring`                          | `handling`                                 | metadata |
| `$/standing_methods/admin_censoring`                          | `note`                                     | metadata |
| `$/standing_methods/calendar_time`                            | `handling`                                 | consumed |
| `$/standing_methods/calendar_time`                            | `note`                                     | consumed |
| `$/standing_methods/comparator_to_intervention_ratio_default` | `handling`                                 | metadata |
| `$/standing_methods/comparator_to_intervention_ratio_default` | `note`                                     | metadata |
| `$/standing_methods/matching_ratio_default`                   | `handling`                                 | metadata |
| `$/standing_methods/matching_ratio_default`                   | `note`                                     | metadata |
| `$/study`                                                     | `description`                              | consumed |
| `$/study`                                                     | `design`                                   | consumed |
| `$/study`                                                     | `implementation`                           | consumed |
| `$/study`                                                     | `principal_investigator`                   | consumed |
| `$/study`                                                     | `title`                                    | consumed |
| `$/study/implementation`                                      | `conf_level`                               | consumed |
| `$/study/implementation`                                      | `date`                                     | consumed |
| `$/study/implementation`                                      | `project_prefix`                           | consumed |
| `$/study/implementation`                                      | `status`                                   | consumed |
| `$/study/implementation`                                      | `version`                                  | consumed |
| `$/subgroups[]`                                               | `implementation`                           | consumed |
| `$/subgroups[]`                                               | `name`                                     | consumed |
| `$/subgroups[]/implementation`                                | `variable`                                 | consumed |
| `$/target_trial`                                              | `analysis_plan`                            | consumed |
| `$/target_trial`                                              | `assignment_procedure`                     | consumed |
| `$/target_trial`                                              | `causal_contrast`                          | consumed |
| `$/target_trial`                                              | `eligibility_criteria`                     | consumed |
| `$/target_trial`                                              | `follow_up_period`                         | consumed |
| `$/target_trial`                                              | `outcome`                                  | consumed |
| `$/target_trial`                                              | `treatment_strategies`                     | consumed |
| `$/target_trial/analysis_plan`                                | `specification`                            | consumed |
| `$/target_trial/assignment_procedure`                         | `specification`                            | consumed |
| `$/target_trial/causal_contrast`                              | `specification`                            | consumed |
| `$/target_trial/eligibility_criteria`                         | `specification`                            | consumed |
| `$/target_trial/follow_up_period`                             | `specification`                            | consumed |
| `$/target_trial/outcome`                                      | `specification`                            | consumed |
| `$/target_trial/treatment_strategies`                         | `specification`                            | consumed |

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

Each entry MUST declare `type: has_event`.
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
refuses any other type. A criterion swereg reads and ignores never
restricts the study population, and it looks exactly like one that does.

[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md)
adds one `eligible_has_<variable>_<window>` column for each criterion.
It then combines every eligibility column into `eligible`.

### How it differs from a per-enrollment `additional_inclusion`

|                | `inclusion_criteria$criteria`     | `enrollments[]$additional_inclusion`     |
|:---------------|:----------------------------------|:-----------------------------------------|
| scope          | every enrollment                  | the one enrollment that declares it      |
| path           | `$/inclusion_criteria/criteria[]` | `$/enrollments[]/additional_inclusion[]` |
| types accepted | `has_event`                       | `has_event` and `age_range`              |

A `has_event` entry generates the same column name in both places. Two
entries that generate one column name are one criterion written twice,
so
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
refuses the pair. That holds when both entries are global, and when one
is global and one is per-enrollment.

`inclusion_criteria$additional_inclusion` is refused. That name belongs
to an enrollment. Copied to the container, it named a path swereg never
read.

## What you see when a key is refused

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
reports every refused key in one message, and reads no data first. A
legacy key carries its migration message.

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
#>     That key is gone. swereg never read it, so it never restricted the study population. Move each entry to inclusion_criteria$criteria. Each criterion there declares name, type: has_event, and implementation$source_variable.
```

The report names one key, not six. The walk stops at a refused key,
because the schema declares no context below it. Move the entries into
`criteria`, and swereg reads each one.

A key the schema does not name anywhere carries the list of keys its
context accepts.

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

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
holds the one call to
[`yaml::yaml.load()`](https://yaml.r-lib.org/reference/yaml.load.html)
in `R/`. Two functions call it: `tteplan_from_spec()`, and the plan’s
own `$reload_spec()` method. No path into a specification skips the
gate.

## See also

- [`vignette("tte-workflow", package = "swereg")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md)
  for the whole specification, and for the pipeline that reads it.
- [`?tteplan_read_spec`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
  for the observation contract and the window conversion rules.
