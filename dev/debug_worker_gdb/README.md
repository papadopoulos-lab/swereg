# Debugging native crashes in `parallel_pool()` workers (gdb)

`parallel_pool()` runs each work item in a short-lived (~5–10 s) R subprocess
launched via `processx`. A **native** segfault in a worker (TBB/RcppParallel,
qs2, data.table, any C/C++) is hard to debug: the worker is ephemeral so you
can't attach a debugger in time, and core dumps need root (`ulimit -c` hard
limit is 0 in unprivileged containers). This stashes a reusable technique —
**launch every worker under gdb** — so a fault is caught with a full native
backtrace and surfaced through the normal worker-error path.

## What it caught (2026-05-31)

s1/s2/s3 workers segfaulted nondeterministically at `0xfffffffffffffff7` (the
crash batch drifted: 1160 → 1398 → 1652 → 1792). gdb pinned it to **TBB's
scheduler building its worker-thread pool**:

```
#0 tbb::internal::generic_scheduler::allocate_task(...)   scheduler.cpp:322   t = 0xffffffffffffffff
#1 ...::generic_scheduler(market&)
#5 ...::create_worker(market&, index=19)        <- the 20th (= detectCores()) TBB thread
#6 tbb::internal::market::create_one_job()
#7 tbb::internal::rml::private_worker::run() / thread_routine()
```

Triggered by **qs2 multithreaded (`nthreads > 1`)** (de)serialization, which
uses RcppParallel/TBB. It's a rare race in TBB pool startup, so it only bites
under enough exposure: each s1a worker does ~43 multithreaded-qs2 ops (1 skeleton
read + 21 enrollments × 2 saves) vs ~7 for a regen worker — hence s1 crashed and
regen (mostly) didn't.

**Fix:** `nthreads = 1L` for qs2 *inside* `parallel_pool` workers (s1/s2/s3) —
process-level parallelism already covers throughput. qs2 stays multithreaded in
the main process and the lower-exposure regen path (`skeleton$save`,
`load_rawbatch`, `save_rawbatch` serial). Not a resource limit
(`vm.max_map_count`/RAM were ruled out by measurement); source-building
RcppParallel did not help (same TBB).

## How to re-enable the debugger (2-minute patch)

1. Patch `R/parallel_pool.R` — gate on the env var **and** gdb being installed so
   it is a no-op otherwise:

   ```r
   rscript_bin <- file.path(R.home("bin"), "Rscript")
   gdb_wrap <- Sys.getenv("SWEREG_GDB_WRAPPER", "")
   use_gdb  <- nzchar(gdb_wrap) && file.exists(gdb_wrap) && nzchar(Sys.which("gdb"))
   # ...then in .launch_worker(), pick the command:
   processx::process$new(
     command = if (use_gdb) gdb_wrap else rscript_bin,
     args = cmd_args, stdout = "|", stderr = "|", cleanup_tree = TRUE
   )
   ```
2. `export SWEREG_GDB_WRAPPER="$(Rscript -e 'cat(system.file(package="swereg"))')/../dev/debug_worker_gdb/gdb_rscript_wrapper.sh"`
   (or just an absolute path to the script in this folder).
3. Run the stage normally (e.g. `plan$s1_generate_enrollments_and_ipw()`).
4. On a crash the backtrace appears in the run log via `.check_worker_error`
   (the wrapped worker exits 134; clean workers exit 0).

Requires `gdb` installed and ptrace permitted (default for a process's own
children). Adds gdb-startup + ptrace overhead per worker, so use for debugging
only — never ship it enabled.
