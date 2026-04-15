# Load a TTEPlan from disk with the current class definition

R6 objects serialized with \[qs2::qs_save()\] retain the method bindings
from the class version at save time. After a package upgrade that adds
new methods or fields, \[qs2::qs_read()\] returns a stale object. This
function reads the file, then copies all public fields into a fresh
\[TTEPlan\] instance so that new methods are available.

## Usage

``` r
tteplan_load(path)
```

## Arguments

- path:

  Path to a \`tteplan.qs2\` file.

## Value

A \[TTEPlan\] object with the current class definition.

## See also

Other tte_plan:
[`registrystudy_load()`](https://papadopoulos-lab.github.io/swereg/reference/registrystudy_load.md),
[`tteplan_locate_and_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_locate_and_load.md)
