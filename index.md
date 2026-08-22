---
title: swereg
---

<p class="rw-section">What's inside</p>

<div class="rw-cards">
<div class="rw-card"><div class="rw-card-num">01</div><h3>Person-week skeletons</h3><p>One row per person per ISO week from NPR, LMED, DORS, LISA, cancer and quality registries, with derived diagnosis, medication, and operation columns.</p></div>
<div class="rw-card"><div class="rw-card-num">02</div><h3>Incremental four-phase pipeline</h3><p>Framework / trim / codes / randvars phases, each with fingerprint-based invalidation. Edit one code entry and it re-applies, with every derived entry that reads it and every randvars step. Edit a randvars step and the rewind-and-replay starts there.</p></div>
<div class="rw-card"><div class="rw-card-num">03</div><h3>Target trial emulation</h3><p>A YAML spec plus an R6 <code>TTEPlan</code> runs target trial emulation (TTE). Each grid cell is one emulated target trial (ETT): one outcome, one follow-up, one enrollment. Parallel enrollment/IPW, then sequential per-protocol censoring.</p></div>
</div>
