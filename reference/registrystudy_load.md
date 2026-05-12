# Locate and load a RegistryStudy from candidate metadata directories

Walks \`candidate_dir_meta\` to find the first directory that exists on
the current host, then reads \`registrystudy.qs2\` from inside it. Used
in \`s0_init.R\` to pass a pre-loaded \`study\` object to
\[tteplan_from_spec_and_registrystudy()\].

## Usage

``` r
registrystudy_load(candidate_dir_meta)
```

## Arguments

- candidate_dir_meta:

  Character vector of candidate metadata directories (where
  \`registrystudy.qs2\` lives). Pass the same path you gave to
  \`RegistryStudy\$new(data_meta_dir = ...)\` – typically either the
  rawbatch directory (legacy default) or its parent.

## Value

A \[RegistryStudy\] R6 object.

## See also

\[first_existing_path()\], \[tteplan_from_spec_and_registrystudy()\]

Other tte_plan:
[`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md),
[`tteplan_locate_and_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_locate_and_load.md)
