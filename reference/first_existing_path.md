# First candidate path that exists

Returns the first element of \`candidates\` for which \[dir.exists()\]
is \`TRUE\`. If none exist but one has a parent directory that exists,
creates that candidate and returns it. Errors if none of the candidates
or their parents exist.

## Usage

``` r
first_existing_path(candidates, label = NULL)
```

## Arguments

- candidates:

  Character vector of candidate paths, in priority order.

- label:

  Optional label used in error messages to describe what kind of path is
  being resolved (e.g. \`"data_rawbatch_dir"\`).

## Value

A single character path: the first candidate that exists, or a
newly-created one.

## Details

This is the stateless primitive used by \[CandidatePath\] for
resolution. Scripts that just need "give me the first of these paths
that exists" can call it directly.

## See also

\[CandidatePath\] for the stateful, caching wrapper used by the R6
classes in this package; \[invalidate_candidate_paths()\] for the
save-time cache clearer that makes objects portable across hosts.

Other multi_host_paths:
[`CandidatePath`](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md),
[`invalidate_candidate_paths()`](https://papadopoulos-lab.github.io/swereg/reference/invalidate_candidate_paths.md)

## Examples

``` r
d <- tempfile()
dir.create(d)
first_existing_path(c("/definitely/not/there", d))
#> [1] "/tmp/RtmpbuU9QG/file1dff3c2d6602"
```
