# Known issues / follow-ups

Robustness follow-ups flagged by a high-effort adversarial review (2026-07-09) of the
`survival_curve()` + `export()` + forest work. None block the 002-ozel-psychosis v009 run
(that config has all outcomes role-tagged, non-NA `age_group`, exposure-grouped forest), but
they are real edge cases to harden.

## Survival curves
- **[P0] `survival_curve` exhibit selects ETT by `age_group == spec$age_group`** — `NA == NA`
  is not TRUE and `== NULL` errors, so an unstratified / NA-age ETT (or a manifest omitting
  `age_group`) finds 0 rows and stops. Use NA-aware matching or normalise a sentinel.
- **[P1] "deaths are censored" is documented but not enforced** in the estimator — it groups
  whatever rows are in `self$data`. If upstream trial data still holds post-death person-weeks
  (`event=0`), the dead stay in the risk set and survival is biased up. Either filter by death
  here or state the method assumes pre-censored trial data.
- **[P1] treatment validation is weak** — the no-plot path returns before any check; the plot
  path lets logical `NA` and single-arm data through silently. Require non-missing treatment
  and (for a contrast) both arms.

## Forest / export / role
- **[P1] role default label renders `"(NA)"`** when only *some* outcomes have `role:` — the
  producer switches to `"{outcome_name} ({outcome_role})"` if *any* outcome is tagged. Append
  role conditionally, or store missing role as `""` and suppress empty parens. Consider
  validating all-or-none + exactly one primary.
- **[P1] `group_by="outcome"` discards the manifest exposure labels** — it row-labels with
  `{enrollment_name}` instead of the (validated) `names(spec$exposures)`. Carry the exposure
  label through per `ett_id`.
- **[P1] outcome-group ordering uses `unique(self$ett$outcome_name)`** (whole-plan order), not
  the manifest/spec outcome order the comment claims. Order by `keep_ids`/`self$spec$outcomes`.
- **[P2] effectively-zero IRR blanking is too broad** — `.ff_irr_ci` blanks every
  `irr < irr_lo_bound`, so a real finite IRR of e.g. 0.004 is indistinguishable from
  not-estimated. Test the no-intervention-events condition explicitly.
- **[P2] `export()` manifest validation is thin** — vector `type`, atomic spec, empty
  `estimands`, missing `enrollment`/files fall through to cryptic low-level errors. Validate
  spec shape and check analysis files exist before `qs2_read()`.
- **[P2] `reload_spec()` cannot clear a removed role** — it only writes `!is.na(new_role)`, so
  a role deleted from the spec persists on the ett. Overwrite to `NA_character_` when absent.
