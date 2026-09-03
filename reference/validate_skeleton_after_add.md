# Validate a skeleton after an add\_\* function has mutated it

Internal helper. Given a pre-state captured by
[`skeleton_snapshot()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_snapshot.md),
check that an `add_*` function honoured the swereg `add_*` contract:

## Usage

``` r
validate_skeleton_after_add(
  skeleton,
  snapshot,
  expected_new_cols = NULL,
  input_data = NULL,
  context = "add_* function",
  may_replace = character()
)
```

## Arguments

- skeleton:

  The skeleton `data.table` after the `add_*` function ran.

- snapshot:

  Pre-state from
  [`skeleton_snapshot()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_snapshot.md).

- expected_new_cols:

  Optional character vector of column names that the function should
  have added.

- input_data:

  Optional `data.table` to check for mutation.

- context:

  Character label for error messages.

- may_replace:

  Character vector of column names the caller owns. A column named here
  MAY be a new vector afterwards, because an entry that runs a second
  time recomputes its own output. Every other pre-existing column MUST
  still be the same vector.

## Value

Invisibly, `skeleton`. Called for side effects.

## Details

- Reference semantics: `skeleton` is still a `data.table`.

- Row preservation: `nrow(skeleton)` unchanged.

- Structural columns present: `id`, `isoyear`, `isoyearweek`,
  `is_isoyear` all still there.

- Structural columns unchanged: all four are
  [`identical()`](https://rdrr.io/r/base/identical.html) to the ones the
  snapshot holds. This catches a row swap that the row count cannot see.
  It also catches a write into `isoyear`, which neither the row count
  nor the column address can see.

- Column preservation: every column the snapshot holds is still present,
  and is still the SAME vector. The address comparison is what tells a
  growth from a replacement.

- Expected new columns added: if `expected_new_cols` is supplied, every
  name in it is present on the skeleton.

- (Opt-in) Input-data not mutated: only checked when `input_data` is
  passed on both sides.

Called automatically by `.apply_code_entry_impl()` around every
user-registered `fn` in `RegistryStudy$register_codes()`. Not exported.
