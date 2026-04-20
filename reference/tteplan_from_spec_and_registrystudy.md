# Create a TTEPlan from a study specification

Builds a \[TTEPlan\] with a full ETT grid (enrollments x outcomes x
follow-up) from the parsed study specification and a pre-loaded
\[RegistryStudy\]. Also stores each enrollment's treatment
implementation details in the ETT data.table so they are available via
\`plan\[\[i\]\]\$treatment_impl\`.

## Usage

``` r
tteplan_from_spec_and_registrystudy(
  study,
  candidate_dir_spec,
  candidate_dir_tteplan,
  candidate_dir_results,
  spec_version = NULL,
  project_id = NULL,
  n_skeleton_files = NULL,
  global_max_isoyearweek = NULL,
  period_width = 4L
)
```

## Arguments

- study:

  A \[RegistryStudy\] R6 object, typically loaded via
  \[registrystudy_load()\]. Owns the rawbatch and skeleton path
  candidates.

- candidate_dir_spec:

  Character vector of candidate directories that contain the spec YAML
  \`spec_vXXX.yaml\`. The first existing directory is used to locate the
  spec.

- candidate_dir_tteplan:

  Character vector of candidate directories where \`tteplan.qs2\` lives
  (or will be created by \`plan\$save()\`).

- candidate_dir_results:

  Character vector of candidate directories for the results BASE
  directory (without the version suffix – the plan appends
  \`spec_version\` internally).

- spec_version:

  Optional character scalar like \`"v003"\` selecting the spec YAML.
  When \`NULL\`, read from \`spec\$study\$implementation\$version\`.

- project_id:

  Optional character scalar for display/logging. When \`NULL\`, read
  from \`spec\$study\$implementation\$project_prefix\`.

- n_skeleton_files:

  Optional integer: if not NULL, only the first \`n_skeleton_files\`
  files are used (for faster dev iterations). Stored on the plan as
  \`n_skeleton_files_limit\` so \[tteplan_load()\] can re-apply it after
  a host transfer.

- global_max_isoyearweek:

  Administrative censoring boundary (isoyearweek string, e.g.,
  "2023-52"). If \`NULL\` (default), auto-detected from
  \`max(isoyearweek)\` in the first skeleton file. Also runs
  \[tteplan_validate_spec()\] on that skeleton.

- period_width:

  Integer, band width in weeks for enrollment and time aggregation
  (default: 4L). Stored on the plan and passed through to TTEDesign.

## Value

A \[TTEPlan\] object with the full ETT grid, embedded \`registrystudy\`,
and CandidatePath fields populated.

## Details

Directory-resolution fields (\`dir_tteplan_cp\`, \`dir_spec_cp\`,
\`dir_results_cp\`) are stored on the plan as \[CandidatePath\]
instances. Stage scripts (\`s1.R\`, \`s2.R\`, \`s3.R\`, \`s4_export.R\`)
can then re-load the plan on any host with \[tteplan_locate_and_load()\]
and call \`plan\$save()\`, \`plan\$s1_generate_enrollments_and_ipw()\`,
etc. without re-specifying any paths.

## See also

\[registrystudy_load()\], \[tteplan_locate_and_load()\]

Other tte_spec:
[`tteplan_apply_derived_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_derived_confounders.md),
[`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md),
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md),
[`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md)
