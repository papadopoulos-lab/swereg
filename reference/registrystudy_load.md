# Locate and load a RegistryStudy from candidate rawbatch directories

Walks \`candidate_dir_rawbatch\` to find the first directory that exists
on the current host, then reads \`registrystudy.qs2\` from inside it.
Used in \`s0_init.R\` to pass a pre-loaded \`study\` object to
\[tteplan_from_spec_and_registrystudy()\].

## Usage

``` r
registrystudy_load(candidate_dir_rawbatch)
```

## Arguments

- candidate_dir_rawbatch:

  Character vector of candidate rawbatch directories.

## Value

A \[RegistryStudy\] R6 object.

## See also

\[first_existing_path()\], \[tteplan_from_spec_and_registrystudy()\]

Other tte_plan:
[`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md),
[`tteplan_locate_and_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_locate_and_load.md)
