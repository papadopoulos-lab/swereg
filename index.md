What’s inside

01

### Person-week skeletons

One row per person per ISO week from NPR, LMED, DORS, LISA, cancer and
quality registries, with derived diagnosis, medication, and operation
columns.

02

### Incremental three-phase pipeline

Framework / randvars / codes phases, each with fingerprint-based
invalidation. Edit one code entry and only that entry re-applies; edit a
randvars step and it rewinds-and-replays everything downstream.

03

### Target trial emulation

A YAML spec plus an R6 `TTEPlan` builds the ETT grid and runs parallel
enrollment/IPW and sequential per-protocol censoring, producing
analysis-ready files.
