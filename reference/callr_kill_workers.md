# Kill orphaned swereg callr worker sessions

Scans PID files in \`/tmp\` written by \[callr_pool()\]. Only kills
workers whose parent R process is dead (i.e. was OOM-killed or crashed).
Workers owned by live R sessions are left untouched. Stale or empty PID
files are removed.

## Usage

``` r
callr_kill_workers()
```

## Value

Invisible integer: number of orphaned workers killed.

## Details

Normal cleanup is handled automatically by \[callr_pool()\]'s
\`on.exit()\` handler. This function is only needed to clean up after
hard crashes (SIGKILL, OOM) where \`on.exit()\` never fires.
