# Atomically write an object to a qs2 file

Writes to a uniquely-named temporary file in the same directory, then
renames it into place. Rename-into-place is atomic on POSIX filesystems
(and server-side atomic on SMB/CIFS), so an interrupted write – SIGKILL,
crash, dropped mount – leaves the destination either absent (a later
resume rebuilds that batch) or complete, never a truncated file that
[`qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)
would halt on. `...` is forwarded to
[`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html).

## Usage

``` r
qs2_write_atomic(object, path, ...)
```

## Arguments

- object:

  Object to serialize.

- path:

  Destination path.

- ...:

  Passed to [`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html)
  (e.g. `nthreads`).

## Value

`path`, invisibly.

## Details

What this does **not** promise, stated because the tempting reading is
wrong:

- **It is not durability.**
  [`file.rename()`](https://rdrr.io/r/base/files.html) is atomic with
  respect to other *readers*; it is not an `fsync`. A power loss can
  still lose a renamed file whose data has not reached the disk. This
  protects against a killed process, not a killed machine.

- **It is not a lock.** Two concurrent writers of the same `path` each
  produce a complete file and the last rename wins. No reader sees a
  torn file, but nothing here decides *which* writer should have won.

- **It does not always clean up after itself.** The partial temp file is
  removed on an R-level error, but
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html) cannot run after a
  `SIGKILL` – so a hard-killed worker leaves its randomly-named `.tmp`
  behind. The *destination* is still absent-or-complete, which is the
  guarantee that matters; the litter is not.

The temporary file is created with
[`tempfile()`](https://rdrr.io/r/base/tempfile.html) in the destination
directory rather than `paste0(path, ".tmp", Sys.getpid())`. The PID
suffix was not collision-proof: PIDs are unique only among *live
processes on one host*, and this package's data lives on a share that
two hosts mount at once – so the same PID on two machines could pick the
same temp path for the same target. Same directory is required:
[`file.rename()`](https://rdrr.io/r/base/files.html) is not atomic
across filesystems.

The implementation now lives in
[`batchit::write_qs2_atomically()`](https://papadopoulos-lab.github.io/batchit/reference/write_qs2_atomically.html);
this is a thin delegation, and the contract above is what swereg
promises its own users. One visible consequence: the rename-failure
error is raised by batchit, so its prefix reads `write_qs2_atomically()`
rather than `qs2_write_atomic()`.
