# Write the specification workbook from a spec, without a plan

Writes the Study Specification sheet and the Target trial protocol sheet
from the spec file and a code registry. The function reads no skeleton,
rawbatch or registry data file, so it runs before s0.

## Usage

``` r
tteplan_excel_spec_summary(
  spec_path,
  path,
  code_registry = NULL,
  period_width = 4L,
  global_max_isoyearweek = NULL,
  protocol_ett_id = NULL
)
```

## Arguments

- spec_path:

  Character(1), the path of the spec YAML. The function reads it with
  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md).

- path:

  Character(1), the output `.xlsx` path. The function overwrites an
  existing file.

- code_registry:

  The code registry that resolves each variable to its codes. One of
  these:

  - `NULL`. Every variable is written as unresolved, as for a plan that
    has no code registry.

  - A
    [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md).
    The function uses `$summary_table()` and reads no data.

  - A `data.table` with the columns of `RegistryStudy$summary_table()`:
    `name`, `codes`, `label` and `generated_columns`.

- period_width:

  Integer, the width in weeks of the enrollment period. It has the
  default of
  [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md).
  The Target trial protocol sheet states it.

- global_max_isoyearweek:

  Character(1) or `NULL`, the administrative censoring boundary as an
  isoyearweek, for example `"2023-52"`. `NULL` writes
  `not built (spec only)` in the `Admin censoring:` row.

- protocol_ett_id:

  Character(1) or `NULL`, the ETT that the Target trial protocol sheet
  documents. `NULL` takes the first ETT of the grid. An id that the grid
  does not hold gives a warning, and the sheet then documents the first
  ETT.

## Value

`path`, invisibly.

## Details

Each cell holds the value that `TTEPlan$excel_spec_summary()` writes for
a plan at s0 from the same spec, code registry, `period_width` and
`protocol_ett_id`. The one exception is the `Admin censoring:` row. s0
reads that value from the first skeleton unless you supply it. Here the
row reads `not built (spec only)` unless you supply
`global_max_isoyearweek`.

Only s1 decides the rare-condition rule. The workbook therefore states
the rule as not decided yet, and it has no Sparse conditions sheet. The
function builds the ETT grid with the code of
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
so the protocol sheet documents the same ETT for the same
`protocol_ett_id`.

## See also

[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
[TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md),
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
    "  principal_investigator: Example PI",
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
study <- RegistryStudy$new(data_rawbatch_dir = spec_dir)
study$register_codes(
  codes = list(oophorectomy = c("LAE20", "LAE21", "LAF10")),
  fn = swereg::add_operations,
  groups = list(os = c("inpatient", "outpatient"))
)
out <- tteplan_excel_spec_summary(
  spec_path = spec_path,
  path = file.path(spec_dir, "spec_v001.xlsx"),
  code_registry = study
)
#> Spec summary saved to: /tmp/Rtmp965wPA/file1dc84dff2cd8/spec_v001.xlsx
openxlsx::getSheetNames(out)
#> [1] "Study Specification"   "Target trial protocol"
unlink(spec_dir, recursive = TRUE)
```
