# Process batches through a user-defined function

For each batch: loads rawbatch data via \[skeleton_load_rawbatch()\],
calls \`process_fn\`, then runs \`gc()\`. Wraps in
\[progressr::with_progress()\] for a progress bar.

## Usage

``` r
skeleton_process(skeleton_meta, ...)
```

## Arguments

- skeleton_meta:

  A \[SkeletonMeta\] object.

- ...:

  Method dispatch. Pass \`process_fn\` (a function with signature
  \`function(batch_data, batch_number, config)\` where \`batch_data\` is
  a named list from \[skeleton_load_rawbatch()\], \`batch_number\` is
  the integer batch index, and \`config\` is the \[SkeletonConfig\]
  object), optionally \`batches\` (integer vector of batch indices or
  \`NULL\` for all), and optionally \`n_workers\` (integer, default
  \`1L\` for sequential processing; when \> 1, runs a pool of
  \[callr::r_bg()\] subprocesses that dynamically replace finished
  workers, keeping all slots busy; threads are auto-distributed as
  \`floor(detectCores() / n_workers)\` per worker).

## Value

A list with: - \`skeleton_meta\`: updated \[SkeletonMeta\] with
\`skeleton_files\` re-scanned - \`results\`: list of \`process_fn\`
return values (one per batch, unprocessed slots are \`NULL\`)

## Details

\## Interactive development pattern

Process functions are typically defined in their own R file with
\`plnr::is_run_directly()\` at the top to enable line-by-line
development:

“\`r skeleton_create_mht \<- function(batch_data, batch_number, config)
if (plnr::is_run_directly()) skel_meta \<-
qs2::qs_read(config@meta_file) batch_number \<- 1 batch_data \<-
skeleton_load_rawbatch(skel_meta, batch_number) config \<-
skel_meta@config \# ... processing code ... “\`

## See also

Other skeleton_methods:
[`skeleton_delete_rawbatches()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_delete_rawbatches.md),
[`skeleton_delete_skeletons()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_delete_skeletons.md),
[`skeleton_load_rawbatch()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_load_rawbatch.md),
[`skeleton_reset()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_reset.md),
[`skeleton_save_rawbatch()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_save_rawbatch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Process all batches
result <- skeleton_process(skel_meta, function(batch_data, batch_number, config) {
  batch_data[["lmed"]] <- batch_data[["lmed"]][!produkt %in% excluded]
  skeleton_create_mht(batch_data, batch_number, config)
})
qs2::qs_save(result$skeleton_meta, config@meta_file)

# Test run: process only the first 2 batches
result <- skeleton_process(skel_meta, my_process_fn, batches = 1:2)

# Parallel: 3 workers via callr worker pool (each batch in a fresh subprocess)
result <- skeleton_process(skel_meta, my_process_fn, n_workers = 3L)
} # }
```
