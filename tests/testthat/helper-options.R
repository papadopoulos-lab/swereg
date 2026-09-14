# The fixture spec (spec_3x2x2.yaml) deliberately has no new-user exclusion,
# so the prevalent-user warning from tteplan_read_spec() would fire in every
# fixture-driven test. Silence it suite-wide; test-spec_newuser_warning.R
# re-enables it locally to test the warning itself.
options(swereg.warn_prevalent_user = FALSE)
