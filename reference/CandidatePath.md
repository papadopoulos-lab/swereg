# CandidatePath: a directory with multiple candidate locations

Holds a priority-ordered list of candidate paths for a directory that
may live at different filesystem locations on different hosts (e.g. a
shared drive mounted at different points under Linux vs Windows).
\`\$resolve()\` returns the first candidate that exists, caching the
result so subsequent calls are free. If no candidate exists but a
candidate's parent directory does, \`\$resolve()\` creates the candidate
and returns it – this is how "first run on a fresh host" gets a
directory automatically.

\`CandidatePath\` is the single type used by swereg R6 classes
(\[RegistryStudy\], \[TTEPlan\]) to own their multi-host directory
knowledge. Both classes hold \`CandidatePath\` instances as public
fields, so their resolution behavior is structurally identical – it
cannot drift.

The cache is host-specific and must not persist across a save/load
cycle. Containing classes call \[invalidate_candidate_paths()\] from
their \`\$save()\` methods before serialization to clear it.

## Methods

- \`\$new(candidates, label = NULL)\`:

  Construct from a character vector of candidate paths.

- \`\$resolve()\`:

  Return the cached resolved path; otherwise walk the candidate list and
  pick the first that exists (or create the first whose parent exists),
  cache, and return.

- \`\$invalidate()\`:

  Clear the cache. The next \`\$resolve()\` call re-walks the candidate
  list.

- \`\$is_resolved()\`:

  \`TRUE\` if the cache is populated and the cached path still exists;
  \`FALSE\` otherwise.

- \`\$print()\`:

  Show the candidate list, marking the cached-resolved entry with
  \`\>\`.

## See also

\[first_existing_path()\] for the stateless primitive used by
\`\$resolve()\`; \[invalidate_candidate_paths()\] for the save-time
cache clearer.

## Public fields

- `candidates`:

  Character vector of candidate paths, in priority order. Read-only
  after construction (modify via a new instance).

- `label`:

  Short human-readable label used in error messages and \`\$print()\`
  output.

## Methods

### Public methods

- [`CandidatePath$new()`](#method-CandidatePath-new)

- [`CandidatePath$resolve()`](#method-CandidatePath-resolve)

- [`CandidatePath$invalidate()`](#method-CandidatePath-invalidate)

- [`CandidatePath$is_resolved()`](#method-CandidatePath-is_resolved)

- [`CandidatePath$print()`](#method-CandidatePath-print)

- [`CandidatePath$clone()`](#method-CandidatePath-clone)

------------------------------------------------------------------------

### Method `new()`

Construct a CandidatePath.

#### Usage

    CandidatePath$new(candidates, label = NULL)

#### Arguments

- `candidates`:

  Character vector of candidate paths. Must be non-empty.

- `label`:

  Optional label for error messages and printing.

------------------------------------------------------------------------

### Method `resolve()`

Resolve the candidate list to a concrete path on the current host.
Returns the cached value if valid; otherwise walks the candidates and
caches the first that exists (or is creatable).

#### Usage

    CandidatePath$resolve()

#### Returns

A single character path.

------------------------------------------------------------------------

### Method `invalidate()`

Clear the cached resolved path. The next \`\$resolve()\` call will
re-walk the candidate list.

#### Usage

    CandidatePath$invalidate()

#### Returns

\`invisible(self)\`.

------------------------------------------------------------------------

### Method `is_resolved()`

Check whether a cached path exists and is still valid.

#### Usage

    CandidatePath$is_resolved()

#### Returns

\`TRUE\` if cached and the cached directory still exists, \`FALSE\`
otherwise.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the candidate list, marking the cached-resolved entry with \`\>\`.

#### Usage

    CandidatePath$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CandidatePath$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
d <- tempfile()
dir.create(d)
cp <- CandidatePath$new(c("/definitely/not/there", d), "my_dir")
cp$resolve()
#> [1] "/tmp/RtmptOnG71/file1ddb5442bddb"
cp$is_resolved()
#> [1] TRUE
print(cp)
#> <CandidatePath: my_dir>
#>     /definitely/not/there
#>   > /tmp/RtmptOnG71/file1ddb5442bddb
cp$invalidate()
cp$is_resolved()
#> [1] FALSE
```
