# Atomically write an object to a qs2 file

Writes to a temporary file in the same directory, then renames it into
place. Rename-into-place is atomic on POSIX filesystems (and server-side
atomic on SMB/CIFS), so an interrupted write – SIGKILL, crash, dropped
mount – leaves the destination either absent (a later resume rebuilds
that batch) or complete, never a truncated file that \`qs2_read()\`
would halt on. \`...\` is forwarded to \[qs2::qs_save()\].

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

  Passed to \`qs2::qs_save()\` (e.g. \`nthreads\`).

## Value

\`path\`, invisibly.
