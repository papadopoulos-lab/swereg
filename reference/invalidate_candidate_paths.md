# Invalidate every CandidatePath cache inside an R6 object

Walks the public fields of \`obj\` depth-first. Every field that is a
\[CandidatePath\] has its cache cleared via \`\$invalidate()\`. Every
field that is another R6 object is recursed into, so embedded objects
(e.g. a \[RegistryStudy\] held inside a \[TTEPlan\]) are also
invalidated.

## Usage

``` r
invalidate_candidate_paths(obj)
```

## Arguments

- obj:

  An R6 object to walk.

## Value

\`invisible(obj)\`.

## Details

Active bindings are deliberately skipped during the walk: accessing a
\`\$dir_foo\` active binding would call \`\$resolve()\` on the
underlying \[CandidatePath\] and immediately re-populate the cache we
are trying to clear. We reach the \[CandidatePath\] instances via their
backing public fields instead.

This is called from \`\$save()\` on both \[RegistryStudy\] and
\[TTEPlan\] before serialization, so the on-disk qs2 file never carries
host-specific resolved paths. After loading on a different host, first
access re-walks the candidate list and caches the path valid on that
host.

## See also

\[CandidatePath\], \[first_existing_path()\], \[RegistryStudy\],
\[TTEPlan\].

Other multi_host_paths:
[`CandidatePath`](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md),
[`first_existing_path()`](https://papadopoulos-lab.github.io/swereg/reference/first_existing_path.md)
