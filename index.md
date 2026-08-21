What’s inside

01

### Person-week skeletons

One row per person per ISO week from NPR, LMED, DORS, LISA, cancer and
quality registries, with derived diagnosis, medication, and operation
columns.

02

### Incremental four-phase pipeline

Framework / trim / codes / randvars phases, each with fingerprint-based
invalidation. Edit one code entry and it re-applies, with every derived
entry that reads it and every randvars step. Edit a randvars step and
the rewind-and-replay starts there.

03

### Target trial emulation

A YAML spec plus an R6 `TTEPlan` runs target trial emulation (TTE). Each
grid cell is one emulated target trial (ETT): one outcome, one
follow-up, one enrollment. Parallel enrollment/IPW, then sequential
per-protocol censoring.
