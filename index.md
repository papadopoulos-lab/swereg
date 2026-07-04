---
title: swereg
---

<p class="rw-section">What's inside</p>

<div class="rw-cards">
<div class="rw-card"><div class="rw-card-num">01</div><h3>Person-week skeletons</h3><p>One row per person per ISO week from NPR, LMED, DORS, LISA, cancer and quality registries, with derived diagnosis, medication, and operation columns.</p></div>
<div class="rw-card"><div class="rw-card-num">02</div><h3>Incremental three-phase pipeline</h3><p>Framework / randvars / codes phases, each with fingerprint-based invalidation. Edit one code entry and only that entry re-applies; edit a randvars step and it rewinds-and-replays everything downstream.</p></div>
<div class="rw-card"><div class="rw-card-num">03</div><h3>Target trial emulation</h3><p>A YAML spec plus an R6 <code>TTEPlan</code> builds the ETT grid and runs parallel enrollment/IPW and sequential per-protocol censoring, producing analysis-ready files.</p></div>
</div>
