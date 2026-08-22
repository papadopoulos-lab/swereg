# Invalidate every CandidatePath cache inside an R6 object

Walks the public fields of `obj` depth-first. Every field that is a
[CandidatePath](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md)
has its cache cleared via `$invalidate()`. Every field that is another
R6 object is recursed into, so embedded objects (e.g. a
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)
held inside a
[TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md))
are also invalidated.

## Usage

``` r
invalidate_candidate_paths(obj)
```

## Arguments

- obj:

  An R6 object to walk.

## Value

`invisible(obj)`.

## Details

Active bindings are deliberately skipped during the walk: accessing a
`$dir_foo` active binding would call `$resolve()` on the underlying
[CandidatePath](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md)
and immediately re-populate the cache we are trying to clear. We reach
the
[CandidatePath](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md)
instances via their backing public fields instead.

This is called from `$save()` on both
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)
and
[TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md)
before serialization, so the on-disk qs2 file never carries
host-specific resolved paths. After loading on a different host, first
access re-walks the candidate list and caches the path valid on that
host.

## See also

[CandidatePath](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md),
[`first_existing_path()`](https://papadopoulos-lab.github.io/swereg/reference/first_existing_path.md),
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md),
[TTEPlan](https://papadopoulos-lab.github.io/swereg/reference/TTEPlan.md).

Other multi_host_paths:
[`CandidatePath`](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md),
[`first_existing_path()`](https://papadopoulos-lab.github.io/swereg/reference/first_existing_path.md)
