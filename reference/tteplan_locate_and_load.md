# Locate and load a TTEPlan from candidate directories

Walks \`candidate_dir_tteplan\` to find the first directory that exists
on the current host, then loads \`tteplan.qs2\` from inside it via
\[tteplan_load()\]. The one-line convenience that \`s1.R\` / \`s2.R\` /
\`s3.R\` / \`s4_export.R\` stage scripts call to obtain a plan with all
directories already resolved.

## Usage

``` r
tteplan_locate_and_load(candidate_dir_tteplan)
```

## Arguments

- candidate_dir_tteplan:

  Character vector of candidate directories, in priority order, where
  \`tteplan.qs2\` might live.

## Value

A \[TTEPlan\] with CandidatePath caches cleared and \`skeleton_files\`
refreshed from the embedded \`registrystudy\`.

## See also

\[tteplan_load()\], \[first_existing_path()\]

Other tte_plan:
[`registrystudy_load()`](https://papadopoulos-lab.github.io/swereg/reference/registrystudy_load.md),
[`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md)
