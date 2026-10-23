# Read the command-line flags of a project's `s0_init.R`

Parses the trailing arguments of `Rscript s0_init.R` with `optparse`.
The two valid flags are `--check` and `--feasibility`, and each defaults
to `FALSE`.

## Usage

``` r
tte_s0_flags(args = commandArgs(trailingOnly = TRUE))
```

## Arguments

- args:

  Character vector of arguments. The default reads the trailing
  arguments of the running `Rscript`.

## Value

A list with two logical elements, `check` and `feasibility`.

## Details

The function stops on any other argument, an abbreviation of a valid
flag included. It also stops when `--check` and `--feasibility` both
appear. Each error message names the two valid flags.

## See also

Other tte_plan:
[`registrystudy_load()`](https://papadopoulos-lab.github.io/swereg/reference/registrystudy_load.md),
[`tte_stage()`](https://papadopoulos-lab.github.io/swereg/reference/tte_stage.md),
[`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md),
[`tteplan_locate_and_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_locate_and_load.md)

## Examples

``` r
tte_s0_flags(character(0))
#> $check
#> [1] FALSE
#> 
#> $feasibility
#> [1] FALSE
#> 
tte_s0_flags("--check")
#> $check
#> [1] TRUE
#> 
#> $feasibility
#> [1] FALSE
#> 
tte_s0_flags("--feasibility")
#> $check
#> [1] FALSE
#> 
#> $feasibility
#> [1] TRUE
#> 
```
