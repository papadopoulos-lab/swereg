# One copy per person in the TTE
#
# A study built with `$set_matched_sets()` gives every membership of a person
# its own `skeleton_id`, and `$save_rawbatch()` writes the person's whole
# history once per `skeleton_id`. A study built with `$set_batches()` MAY put
# one ID in several batches. Either way one person can have several copies in
# the skeleton files, each under its own (batch, skeleton id).
#
# The TTE enrolls a person at most once per enrollment period, and clusters
# all of a person's enrollments on the person. Copies break both: each copy
# is enrolled on its own, and each copy is its own cluster. So s1 reads one
# copy per person. It keeps the first copy by (batch, skeleton id) among the
# skeleton files the run reads, and s1a drops the rows of every other copy
# straight after it loads a skeleton. Every count, the comparator draw, the
# s1a cache and the panels then hold the person once, under one ID, and the
# existing clustering on `person_id_var` is a clustering on the person.
#
# The copies of a person hold the same history, so dropping one changes no
# enrollment. `tteplan_from_spec_and_registrystudy()` checks that on one
# batch, see `.tte_check_copies_agree()`.
#
# A study where no person has two copies drops nothing, and s1 runs exactly
# as it does without this file.

# The batch number of each skeleton file, from its `skeleton_NNNNN.qs2` name,
# or NA when the name has another form.
.skeleton_batch_number <- function(paths) {
  m <- regmatches(
    basename(paths),
    regexec("^skeleton_(\\d+)\\.qs2$", basename(paths))
  )
  return(vapply(
    m,
    function(x) {
      if (length(x) < 2L) {
        return(NA_integer_)
      }
      return(as.integer(x[[2L]]))
    },
    integer(1)
  ))
}

# A field of a study, which is a `RegistryStudy` or a plain list. NULL when the
# study does not hold it.
.tte_study_field <- function(study, name) {
  if (is.environment(study)) {
    if (!exists(name, envir = study, inherits = FALSE)) {
      return(NULL)
    }
    return(get(name, envir = study, inherits = FALSE))
  }
  if (is.list(study)) {
    return(study[[name]])
  }
  return(NULL)
}

# The copies of every person in the given batches: one row per
# (batch, skeleton_id), with the person it belongs to. On a matched-set study
# the person comes from the roster. Otherwise the batch ID list is used, and
# the ID is the person. NULL when the study holds neither, or when `batches`
# holds NA.
.tte_copies <- function(study, batches) {
  if (length(batches) == 0L || anyNA(batches)) {
    return(NULL)
  }
  roster <- .tte_study_field(study, "matched_set_roster")
  if (!is.null(roster)) {
    id_col <- .tte_study_field(study, "id_col")
    parts <- lapply(batches, function(b) {
      r <- if (b <= length(roster)) roster[[b]] else NULL
      if (is.null(r) || nrow(r) == 0L) {
        return(NULL)
      }
      return(data.table::data.table(
        batch = b,
        skeleton_id = r[["skeleton_id"]],
        person = r[[id_col]]
      ))
    })
  } else {
    ids <- .tte_study_field(study, "batch_id_list")
    if (is.null(ids)) {
      return(NULL)
    }
    parts <- lapply(batches, function(b) {
      x <- if (b <= length(ids)) ids[[b]] else NULL
      if (length(x) == 0L) {
        return(NULL)
      }
      return(data.table::data.table(batch = b, skeleton_id = x, person = x))
    })
  }
  copies <- data.table::rbindlist(parts)
  if (nrow(copies) == 0L) {
    return(NULL)
  }
  copies <- unique(copies, by = c("batch", "skeleton_id"))
  data.table::setorderv(copies, c("batch", "skeleton_id"))
  return(copies)
}

# The skeleton IDs s1a drops from each file: every copy of a person except the
# first by (batch, skeleton id) among `files`. Returns a list with
# `drop`, named by file basename and holding only the files that drop
# something, and `n_dropped`, the number of copies dropped. `drop` is an empty
# list when no person has two copies.
.tte_copies_to_drop <- function(study, files) {
  none <- list(drop = list(), n_dropped = 0L)
  batches <- .skeleton_batch_number(files)
  copies <- .tte_copies(study, batches)
  if (is.null(copies)) {
    return(none)
  }
  extra <- copies[duplicated(copies[["person"]])]
  if (nrow(extra) == 0L) {
    return(none)
  }
  file_of <- basename(files)[match(extra[["batch"]], batches)]
  drop <- split(extra[["skeleton_id"]], file_of)
  return(list(drop = drop, n_dropped = nrow(extra)))
}

# Drop the rows of the copies `drop_ids` from a loaded skeleton. A NULL or
# empty `drop_ids` returns the skeleton untouched, so the path without copies
# is the path of a study without copies.
.tte_drop_copies <- function(skeleton, drop_ids) {
  if (length(drop_ids) == 0L) {
    return(skeleton)
  }
  id <- isoyearweek <- NULL # nolint
  skeleton <- skeleton[!(id %in% drop_ids)]
  # A subset is a new table with data.table's default spare slots. Give it
  # the skeleton figure that `qs2_read()` gave the table it came from.
  skeleton <- data.table::setalloccol(skeleton, n = .DT_ALLOC_SPARE_SLOTS)
  data.table::setkey(skeleton, id, isoyearweek)
  return(skeleton)
}

# The number of distinct persons in a study: the persons of the roster on a
# matched-set study, otherwise the distinct IDs of the batch ID list. NULL when
# the study holds neither.
.tte_study_n_persons <- function(study) {
  roster <- .tte_study_field(study, "matched_set_roster")
  if (!is.null(roster)) {
    id_col <- .tte_study_field(study, "id_col")
    persons <- unlist(
      lapply(roster, function(r) r[[id_col]]),
      use.names = FALSE
    )
    return(as.integer(data.table::uniqueN(persons)))
  }
  ids <- .tte_study_field(study, "batch_id_list")
  if (is.null(ids)) {
    return(NULL)
  }
  return(as.integer(data.table::uniqueN(unlist(ids, use.names = FALSE))))
}

# s0 check: the copies of a person agree on every column the spec reads.
#
# The columns are the ones s1a keeps for the spec,
# `.tte_canonical_needed_cols()`.
#
# It loads the skeleton files that hold the first person with more than one
# copy among `files`, and compares every person whose copies all lie in those
# files. For each such person, every copy MUST hold the same weeks, and every
# column the spec reads MUST hold the same value in each week. Otherwise it
# stops and names the column. It checks those files only, not every batch, and
# it runs in s0 so that s1 never meets a copy that disagrees.
.tte_check_copies_agree <- function(study, files, spec, enrollment_specs) {
  batch <- person <- n_copies <- i.person <- i.n_copies <- NULL # nolint
  batches <- .skeleton_batch_number(files)
  copies <- .tte_copies(study, batches)
  if (is.null(copies)) {
    return(invisible(NULL))
  }
  multi <- copies[, .N, by = "person"][get("N") > 1L][["person"]]
  if (length(multi) == 0L) {
    return(invisible(NULL))
  }
  first <- copies[copies[["person"]] %in% multi][1L][["person"]]
  load_batches <- unique(copies[copies[["person"]] == first][["batch"]])
  inside <- copies[
    copies[["person"]] %in% multi,
    list(inside = all(get("batch") %in% load_batches), n_copies = .N),
    by = "person"
  ][get("inside") == TRUE]
  checked <- copies[copies[["person"]] %in% inside[["person"]]]

  cols <- NULL
  rows <- data.table::rbindlist(lapply(load_batches, function(b) {
    sk <- .s1_load_skeleton(files[match(b, batches)], 1L)
    if (is.null(cols)) {
      # The columns s1a keeps for the spec, as s1a computes them.
      cols <<- setdiff(
        .tte_canonical_needed_cols(spec, enrollment_specs, names(sk)),
        c("id", "isoyearweek")
      )
    }
    sid <- checked[checked[["batch"]] == b][["skeleton_id"]]
    keep <- intersect(c("id", "isoyearweek", cols), names(sk))
    x <- sk[sk[["id"]] %in% sid, keep, with = FALSE]
    x[, batch := b]
    return(x)
  }), use.names = TRUE, fill = TRUE)
  rows[
    checked,
    on = c("batch", id = "skeleton_id"),
    person := i.person
  ]
  weeks <- rows[, .N, by = c("person", "isoyearweek")]
  weeks[inside, on = "person", n_copies := i.n_copies]
  if (any(weeks[["N"]] != weeks[["n_copies"]])) {
    p <- weeks[weeks[["N"]] != weeks[["n_copies"]]][1L][["person"]]
    stop(
      "tteplan_from_spec_and_registrystudy(): the copies of person ",
      p,
      " in the skeleton files hold different weeks. s1 reads one copy per ",
      "person, so every copy MUST hold the same history.",
      call. = FALSE
    )
  }
  for (col in intersect(cols, names(rows))) {
    n_values <- rows[,
      list(n = data.table::uniqueN(.SD[[1L]])),
      by = c("person", "isoyearweek"),
      .SDcols = col
    ]
    if (any(n_values[["n"]] > 1L)) {
      p <- n_values[n_values[["n"]] > 1L][1L][["person"]]
      stop(
        "tteplan_from_spec_and_registrystudy(): the spec reads column '",
        col,
        "', and the copies of person ",
        p,
        " hold different values in it. s1 reads one copy per person, so a ",
        "column the spec reads MUST be the same in every copy.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}
