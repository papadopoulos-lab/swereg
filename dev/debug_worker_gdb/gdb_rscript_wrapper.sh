#!/bin/bash
# Run a parallel_pool worker (an Rscript-style invocation) under gdb so a native
# SIGSEGV in the worker is caught with a backtrace and propagated as exit 134.
# Point swereg's worker launcher at this via SWEREG_GDB_WRAPPER (see README.md).
# Args mirror Rscript:  $1 = R flag (e.g. --vanilla), $2 = script.R, $3.. = script args.
set -u
RHOME="$(R RHOME)"
export R_HOME="$RHOME"
export LD_LIBRARY_PATH="$RHOME/lib:${LD_LIBRARY_PATH:-}"
CMDS="$(cd "$(dirname "$0")" && pwd)/worker_cmds.gdb"
flag="$1"; script="$2"; shift 2
exec gdb -batch -nx -x "$CMDS" \
  --args "$RHOME/bin/exec/R" "$flag" --no-echo --no-restore --file="$script" --args "$@"
