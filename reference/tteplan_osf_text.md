# Write the OSF preregistration text that a spec determines

Writes the text of each field of the OSF Preregistration form whose
content the study specification and swereg fix: the design, the
sampling, the treatment strategies, the outcomes, the covariates, the
eligibility criteria, the analysis and the code list. Each fact appears
in one field. The other fields point to it as `'Field/Subheading'`.

## Usage

``` r
tteplan_osf_text(
  spec_path,
  code_registry = NULL,
  period_width = 4L,
  global_max_isoyearweek = NULL,
  people = c("person", "persons")
)
```

## Arguments

- spec_path:

  Character(1), the path of the spec YAML. The function reads it with
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md).

- code_registry:

  The code registry that resolves each variable to its codes, as in
  [`tteplan_excel_spec_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_excel_spec_summary.md):
  `NULL`, a
  [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md),
  or a `data.table` with the columns of `RegistryStudy$summary_table()`.
  With `NULL`, every code cell of the code list reads `not resolved`.

- period_width:

  Integer, the width in weeks of the enrollment period. It has the
  default of
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md).

- global_max_isoyearweek:

  Character(1) or `NULL`, the administrative censoring boundary as an
  isoyearweek, for example `"2025-01"`. Follow-up includes that week.
  `NULL` writes "the administrative end of the data".

- people:

  Character(2), the singular and plural noun for a study participant,
  for example `c("woman", "women")`.

## Value

A named character vector. Each element is the Markdown text of one
field, and the names are the OSF field headings: `Study design`,
`Sample size`, `Measured variables/Exposures`,
`Measured variables/Outcomes`, `Measured variables/Baseline covariates`,
`Statistical models`, `Transformations`, `Data inclusion and exclusion`,
`Missing data`, `Other planned analysis` and `Code list`.

## Details

The function reads the spec and the code registry only. It reads no
skeleton, rawbatch or registry data file, so it runs before s0. Every
spec entry appears in the text: no criterion, covariate or outcome is
left out.

The text uses the `name` of each criterion, covariate and outcome, the
`arms` and `description` of each treatment, and the `description` of
each outcome, as the spec writes them. Write those fields for a reader
of the registration. An age range and an ISO-year range are written from
their `min` and `max`, the values that run, and not from their `name`.
The categories of each covariate are data values, so they appear in the
code list only.

Fields that the spec cannot answer are left to the author: the title,
the research question and the primary result, the foreknowledge of the
data, the data sources and the study population, and the ethics
approval.

## See also

[`tteplan_excel_spec_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_excel_spec_summary.md),
[`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md)

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md),
[`tteplan_excel_spec_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_excel_spec_summary.md),
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md),
[`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md)

## Examples

``` r
spec_dir <- tempfile()
dir.create(spec_dir)
spec_path <- file.path(spec_dir, "spec_v001.yaml")
writeLines(
  c(
    "study:",
    "  title: Example study",
    "  implementation:",
    "    project_prefix: example",
    "    version: v001",
    "inclusion_criteria:",
    "  isoyears: [2010, 2020]",
    "exclusion_criteria:",
    "  - name: Prior oophorectomy",
    "    implementation:",
    "      source_variable: os_oophorectomy",
    "      window: lifetime_before_baseline",
    "confounders:",
    "  - name: Age",
    "    implementation:",
    "      variable: rd_age_continuous",
    "outcomes:",
    "  - name: Outcome",
    "    implementation:",
    "      variable: osd_outcome",
    "follow_up:",
    "  - {label: 1 year, weeks: 52}",
    "enrollments:",
    "  - id: '01'",
    "    name: A vs B",
    "    observed_var: {sentinel: row_presence}",
    "    intervention_tolerance_weeks: 0",
    "    comparator_tolerance_weeks: 0",
    "    additional_inclusion:",
    "      - {name: Age 50-59, type: age_range, min: 50, max: 59}",
    "    treatment:",
    "      arms: {intervention: A, comparator: B}",
    "      implementation:",
    "        variable: rd_tx",
    "        intervention_value: a",
    "        comparator_value: b",
    "        comparator_to_intervention_ratio: 2",
    "        seed: 1"
  ),
  spec_path
)
osf <- tteplan_osf_text(spec_path)
cat(osf[["Data inclusion and exclusion"]])
#> Each criterion is assessed in each week of the enrollment period. A look-back window counts the calendar weeks before the week assessed, not that week itself. 'At any time before baseline' means from the start of the data.
#> 
#> Inclusion criteria:
#> 
#> 1. Trial entry in ISO years 2010 to 2020.
#> 2. Age 50 to 59 years.
#> 
#> Exclusion criteria:
#> 
#> 1. Prior oophorectomy, at any time before baseline.
#> 
#> No person is excluded for an extreme value of a measured variable. Extreme weights are truncated, as described under 'Statistical models'.
unlink(spec_dir, recursive = TRUE)
```
