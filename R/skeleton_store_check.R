# One implementation of "is this skeleton store usable", called from both
# places that need it: `tteplan_from_spec_and_registrystudy()` when a plan is
# built, and `$s1_generate_enrollments_and_ipw()` when s1 is about to read.
#
# Two call sites rather than one because they answer different questions at
# different times. Plan construction asks "is there any point starting". s1
# asks "is what I am about to read one generation". The store can change
# between them: a plan is saved, reloaded on another host hours later, and
# `tteplan_load()` re-resolves `skeleton_files`. A check only at construction
# is an early warning, not a gate.
#
# `check` is validated rather than passed to `isTRUE()`. `isTRUE(NA)` is
# FALSE, so a caller that wrote `check_skeletons = NA` would silently get no
# checking at all from a gate whose default is on.
.assert_skeleton_store <- function(study, files, check, where = "") {
  if (!is.logical(check) || length(check) != 1L || is.na(check)) {
    stop(
      "`check_skeletons` must be TRUE or FALSE, not ",
      paste(utils::capture.output(utils::str(check)), collapse = " "),
      call. = FALSE
    )
  }
  if (!check) {
    return(invisible(FALSE))
  }
  if (!is.function(study$assert_skeletons_consistent)) {
    # Fail CLOSED. `study` only has to supply `$skeleton_files`, so a
    # duck-typed list is a supported input -- but a checking gate that
    # cannot check must not report success. The caller asked for the
    # check; if it cannot run, say so and make them opt out on purpose.
    stop(
      "check_skeletons is TRUE but `study` has no ",
      "$assert_skeletons_consistent(): it is not a RegistryStudy, so the ",
      "skeleton store cannot be checked",
      if (nzchar(where)) paste0(" (", where, ")") else "",
      ".\nPass a RegistryStudy, or check_skeletons = FALSE to proceed ",
      "without the check.",
      call. = FALSE
    )
  }
  study$assert_skeletons_consistent(files = files)
  return(invisible(TRUE))
}
