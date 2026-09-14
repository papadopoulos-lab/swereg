# The fixture spec (spec_3x2x2.yaml) deliberately has no new-user exclusion,
# so the prevalent-user warning from tteplan_read_spec() would fire in every
# fixture-driven test. Silence it suite-wide; test-spec_newuser_warning.R
# re-enables it locally to test the warning itself.
options(swereg.warn_prevalent_user = FALSE)

# An inherited scratch root would relocate every s1 fixture, and the sweep
# could then delete entries in a real scratch root. Unset both, suite-wide.
options(swereg.s1_work_root = NULL, swereg.scratch_max_age_days = NULL)
Sys.unsetenv(c("SWEREG_S1_WORK_ROOT", "SWEREG_SCRATCH_MAX_AGE_DAYS"))
