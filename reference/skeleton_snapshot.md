# Snapshot a skeleton's structural state for post-hoc validation

Internal helper. Captures cheap structural metadata (row count, column
set) before an `add_*` function runs, so that
[`validate_skeleton_after_add()`](https://papadopoulos-lab.github.io/swereg/reference/validate_skeleton_after_add.md)
can check afterwards that the function honoured the `add_*` contract
(modify by reference, no row changes, no structural column drops).

## Usage

``` r
skeleton_snapshot(skeleton, input_data = NULL)
```

## Arguments

- skeleton:

  A skeleton `data.table` (must have the standard structural columns
  `id`, `isoyear`, `isoyearweek`, `is_isoyear`).

- input_data:

  Optional `data.table` whose row count and column set to fingerprint as
  well. The pipeline always passes `NULL` (input mutation is harmless
  when rawbatch data is discarded after each call), but the parameter
  exists so interactive / test callers can opt in.

## Value

A list tagged with class `swereg_skeleton_snapshot`, consumed by
[`validate_skeleton_after_add()`](https://papadopoulos-lab.github.io/swereg/reference/validate_skeleton_after_add.md).

## Details

Called automatically by `.apply_code_entry_impl()` around every
user-registered `fn` in `RegistryStudy$register_codes()`, so any custom
`add_*` plugged into the pipeline is contract- validated for free. Not
exported – users should not call this directly; instead, register custom
`add_*` functions via `RegistryStudy$register_codes()` and let the
pipeline do the bookkeeping.
