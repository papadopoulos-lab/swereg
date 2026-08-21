# RegistryStudy methods that read, write and delete the study's files on disk,
# and the skeleton manifest commit protocol that guards the skeleton dataset.

#' @include r6_registrystudy.R
#' @description Save rawbatch files for one group.
#' @param group Character. Group name (must be in group_names).
#' @param data data.table or named list of data.tables.
#' @param n_workers Integer. Number of parallel writers (default 1L,
#'   serial). When > 1L, slices are written concurrently via mirai
#'   daemons; the 'mirai' package must be installed. Each splittable
#'   data.table gets `BID` added in-place and is keyed on it
#'   (`setkey(dt, BID)`), so per-batch slices are O(log n) keyed
#'   lookups instead of O(n) `%in%` scans. RAM stays ~1x the input
#'   (no split materialisation).
RegistryStudy$set(
  "public",
  "save_rawbatch",
  function(
    group,
    data,
    n_workers = default_n_workers("rawbatch")
  ) {
    # Validate FIRST -- before the group check, before the already-saved
    # early return, before converting. as.integer() first silently turned 2.5
    # into 2; and the "already saved" return let an invalid explicit count (or
    # a bad SWEREG_N_WORKERS_RAWBATCH) report success without ever being seen.
    n_workers <- .validate_n_workers(n_workers, "save_rawbatch()")

    if (!group %in% self$group_names) {
      stop(
        "group '",
        group,
        "' not in group_names: ",
        paste(self$group_names, collapse = ", ")
      )
    }

    if (group %in% self$groups_saved) {
      cat(
        "Skipping '",
        group,
        "' -- all rawbatch files already exist\n",
        sep = ""
      )
      return(invisible(self))
    }

    id_col <- self$id_col
    if (n_workers > 1L && !requireNamespace("mirai", quietly = TRUE)) {
      stop("save_rawbatch(n_workers > 1) requires the 'mirai' package")
    }
    n_batches_local <- self$n_batches

    id_to_batch <- data.table::data.table(
      .id_ = unlist(self$batch_id_list, use.names = FALSE),
      BID = rep.int(seq_len(n_batches_local), lengths(self$batch_id_list))
    )
    data.table::setnames(id_to_batch, ".id_", id_col)
    data.table::setkeyv(id_to_batch, id_col)

    # Annotate each splittable data.table with BID and key on it.
    # Both modifications are in-place; peak RAM stays ~1x the table.
    prepare_dt <- function(dt) {
      if (!(data.table::is.data.table(dt) && id_col %in% names(dt))) {
        return(FALSE)
      }
      dt[id_to_batch, on = id_col, BID := i.BID]
      data.table::setkey(dt, BID)
      TRUE
    }

    data_is_dt <- data.table::is.data.table(data)
    if (data_is_dt) {
      prepared <- prepare_dt(data)
      payload_for_batch <- function(b) {
        sl <- data[.(b), nomatch = NULL]
        if (prepared) {
          sl[, BID := NULL]
        }
        sl
      }
      cleanup_caller_state <- function() {
        if (prepared && "BID" %in% names(data)) data[, BID := NULL]
      }
    } else {
      prepared <- vapply(data, prepare_dt, logical(1))
      payload_for_batch <- function(b) {
        out <- vector("list", length(data))
        names(out) <- names(data)
        for (nm in names(data)) {
          if (prepared[[nm]]) {
            sl <- data[[nm]][.(b), nomatch = NULL]
            sl[, BID := NULL]
            out[[nm]] <- sl
          } else {
            out[[nm]] <- data[[nm]]
          }
        }
        out
      }
      cleanup_caller_state <- function() {
        for (nm in names(data)) {
          if (prepared[[nm]] && "BID" %in% names(data[[nm]])) {
            data[[nm]][, BID := NULL]
          }
        }
      }
    }
    on.exit(cleanup_caller_state(), add = TRUE)

    # Normalize the resolved rawbatch dir to an ABSOLUTE path before building
    # outpaths: the n_workers > 1 path declares these as batchit `outputs`,
    # and batchit's atomic commit requires absolute output paths (a relative
    # `data_rawbatch_dir` would work serially but be rejected in parallel).
    # Normalizing the DIR (which exists) rather than the not-yet-written files
    # keeps both the serial and parallel branches writing to the same place.
    outpaths <- file.path(
      normalizePath(self$data_rawbatch_dir, mustWork = FALSE),
      sprintf("%05d_rawbatch_%s.qs2", seq_len(n_batches_local), group)
    )

    if (n_workers > 1L) {
      # Shape B through the ONE generic runner: the parent IS the producer
      # (payload_for_batch() materialises each slice lazily), the item is the
      # data slice itself, and .batch_stream's bounded queue is the
      # backpressure this block used to hand-roll with its own inflight
      # list. The runner allocates its own private mirai compute profile per
      # invocation (never mirai's default -- daemons(n) there would destroy
      # the caller's configuration), the per-item timeout, both-end validation
      # and loud failure all come with the runner instead of being
      # reimplemented here. The daemon loads swereg before executing, so the
      # target uses the real qs2_write_atomic() -- no hand-inlined temp+rename.
      #
      # Serial (n_workers = 1, the default) deliberately does NOT dispatch:
      # it calls qs2_write_atomic() directly in-process below (NOT
      # .rawbatch_write_worker() -- that worker's
      # .batch_where_to_write_output("rawbatch") only resolves inside an
      # active style = "staged_writer" run). No process boundary means no
      # dispatcher and no mirai requirement -- mirai stays a Suggests that
      # only a parallel save_rawbatch() needs.
      #
      # Declared-output commit (style = "staged_writer"): each item's
      # `outputs` entry names its ONE destination ("rawbatch" ->
      # outpaths[b]), aligned positionally to `ids`. The worker writes there
      # via .batch_where_to_write_output("rawbatch") instead of receiving
      # the path as an argument; batchit commits it atomically once the
      # target returns.
      ids <- sprintf("%05d_%s", seq_len(n_batches_local), group)
      .batch_stream(
        target = .batch_target("swereg", ".rawbatch_write_worker"),
        ids = ids,
        producer = function(id) {
          b <- match(id, ids)
          if (b %% 100L == 0L) {
            cat("  dispatched", b, "/", n_batches_local, "->", group, "\n")
          }
          list(
            slice = payload_for_batch(b),
            # One thread per daemon write: parallelism comes from the
            # daemons, matching the old inline `nthreads = 1L`.
            n_threads = 1L
          )
        },
        outputs = lapply(seq_len(n_batches_local), function(b) {
          c(rawbatch = outpaths[b])
        }),
        style = "staged_writer",
        n_workers = n_workers,
        dev_path = .swereg_dev_path(),
        # Drain-side completion reporting, matching the old block's periodic
        # "completed b / n" cats: .batch_stream calls p once per drained
        # item, and any function accepting `message` serves -- no progressr
        # session needed for a cat-style pipeline log.
        p = local({
          done <- 0L
          function(...) {
            done <<- done + 1L
            if (done %% 100L == 0L) {
              cat(
                "  completed",
                done,
                "/",
                n_batches_local,
                "->",
                group,
                "\n"
              )
            }
          }
        })
      )
    } else {
      n_threads <- .safe_n_cores()
      for (b in seq_len(n_batches_local)) {
        qs2_write_atomic(
          payload_for_batch(b),
          outpaths[b],
          nthreads = n_threads
        )
        cat(
          "  batch",
          b,
          "/",
          n_batches_local,
          "(",
          length(self$batch_id_list[[b]]),
          "IDs) ->",
          group,
          "\n"
        )
      }
    }

    self$groups_saved <- sort(unique(c(self$groups_saved, group)))
    invisible(self)
  }
)

#' @description Load rawbatch files for a single batch.
#' @param batch_number Integer. 1-indexed batch number.
#' @return Named list of data.tables.
RegistryStudy$set("public", "load_rawbatch", function(batch_number) {
  if (batch_number < 1 || batch_number > self$n_batches) {
    stop(
      "batch_number must be between 1 and ",
      self$n_batches,
      " (got ",
      batch_number,
      ")"
    )
  }

  n_threads <- data.table::getDTthreads()
  result <- list()

  for (g in self$group_names) {
    fpath <- file.path(
      self$data_rawbatch_dir,
      sprintf("%05d_rawbatch_%s.qs2", batch_number, g)
    )
    if (!file.exists(fpath)) {
      stop("Rawbatch file missing: ", fpath)
    }
    obj <- qs2_read(fpath, nthreads = n_threads)

    if (is.list(obj) && !data.table::is.data.table(obj)) {
      for (nm in names(obj)) {
        result[[nm]] <- obj[[nm]]
      }
    } else {
      result[[g]] <- obj
    }
  }

  result
})

#' @description Load a skeleton file for `batch_number` as a
#'   [Skeleton] R6 object. Returns `NULL` if the file is missing
#'   (caller rebuilds from scratch). Errors if the file on disk is
#'   not a `Skeleton` R6 object (e.g. corrupted or from an
#'   incompatible version of swereg).
#' @param batch_number Integer batch index.
#' @return A [Skeleton], or `NULL` if the file is missing.
RegistryStudy$set("public", "load_skeleton", function(batch_number) {
  path <- file.path(
    self$data_skeleton_dir,
    sprintf("skeleton_%05d.qs2", as.integer(batch_number))
  )
  if (!file.exists(path)) {
    return(NULL)
  }

  obj <- qs2::qs_read(path)
  if (!inherits(obj, "Skeleton")) {
    stop(
      "Skeleton file is not a Skeleton R6 object: ",
      path,
      "\n",
      "Delete the file and re-run $process_skeletons() to rebuild.",
      call. = FALSE
    )
  }
  obj$check_version()
  # qs2 round-tripping drops data.table over-allocation
  # (`truelength` becomes 0). Without this refresh, the first
  # `:=` that adds a column would silently reallocate the
  # data.table at a new memory address, leaving `obj$data`
  # pointing at the old version (because data.table rebinds the
  # caller's variable on realloc, and the caller here is the
  # helper function that received `self$data` by value).
  #
  # `setalloccol()` allocates a new data.table HEADER with
  # N free column slots (N = `getOption("datatable.alloccol",
  # 4096L)`). The actual column DATA stays shared by reference,
  # so memory overhead is ~8-16 bytes per over-allocation slot
  # -- ~32-64 KB per skeleton regardless of row count, not a
  # full copy. The assignment rebinds `obj$data` (a public R6
  # field) to the new header so it survives subsequent `:=`
  # in-place mutations without reallocation.
  #
  # 4096 headroom slots comfortably supports code registries
  # with several hundred entries. Studies that grow beyond that
  # can bump via `options(datatable.alloccol = 8192L)` at the
  # top of their generator script.
  obj$data <- data.table::setalloccol(
    obj$data,
    n = getOption("datatable.alloccol", 4096L)
  )
  obj
})

#' @description Save a [Skeleton] to this study's skeleton directory,
#'   plus a small `meta_%05d.qs2` sidecar capturing provenance hashes
#'   and the per-batch code-check accumulator snapshot. Subsequent
#'   `$process_skeletons()` runs read the meta first and skip loading
#'   the heavy skeleton entirely when every hash still matches.
#'
#'   Skeleton is written first, then meta. A crash between the two
#'   leaves a stale meta on disk; the next run reads it, finds the
#'   hashes don't match the current pipeline, falls through to the
#'   slow path, and rewrites both.
#'
#'   This is the one site that computes the per-column code-entry
#'   counts. `sk$refresh_code_entry_counts()` runs before either
#'   file is written, so the skeleton file and the meta both carry
#'   counts that describe the data being written.
#' @param sk A [Skeleton] to persist.
#' @return The full path the skeleton file was written to, invisibly.
RegistryStudy$set("public", "save_skeleton", function(sk) {
  stopifnot(inherits(sk, "Skeleton"))
  sk$refresh_code_entry_counts()
  sk_path <- sk$save(self$data_skeleton_dir)
  self$write_skeleton_meta(sk)
  invisible(sk_path)
})

#' @description Delete all rawbatch files from disk.
RegistryStudy$set("public", "delete_rawbatches", function() {
  files <- list.files(
    self$data_rawbatch_dir,
    pattern = "\\d+_rawbatch_.*\\.qs2$",
    full.names = TRUE
  )
  if (length(files) > 0) {
    cat("Deleting", length(files), "rawbatch files\n")
    file.remove(files)
  }
  self$groups_saved <- character(0)
  invisible(self)
})

#' @description Delete all skeleton output files (and their meta
#'   sidecars, plus any cached `population_*.qs2` and
#'   `summary.qs2` artefacts) from disk.
RegistryStudy$set("public", "delete_skeletons", function() {
  files <- list.files(
    self$data_skeleton_dir,
    pattern = "^(skeleton|meta)_\\d+\\.qs2$",
    full.names = TRUE
  )
  if (length(files) > 0) {
    cat("Deleting", length(files), "skeleton/meta files\n")
    file.remove(files)
  }
  derived <- list.files(
    self$data_skeleton_dir,
    pattern = "^(population_.*|summary)\\.qs2$",
    full.names = TRUE
  )
  if (length(derived) > 0) {
    cat("Deleting", length(derived), "derived population/summary files\n")
    file.remove(derived)
  }
  invisible(self)
})

#' @description Delete the metadata file from disk.
RegistryStudy$set("public", "delete_meta_file", function() {
  if (file.exists(self$meta_file)) {
    cat("Deleting", self$meta_file, "\n")
    file.remove(self$meta_file)
  }
  invisible(self)
})

#' @description Save this study object as metadata. Captures the
#'   destination path first, then clears host-specific [CandidatePath]
#'   caches before writing, so the on-disk file never carries a resolved
#'   path from the saving host.
RegistryStudy$set("public", "save_meta", function() {
  dest <- self$meta_file # resolves dir_rawbatch before invalidation
  invalidate_candidate_paths(self)
  qs2_write_atomic(self, dest)
  cat("Saved", dest, "\n")
  invisible(self)
})

# Clear the skeleton commit record, on disk, before any batch is touched.
#
# This is the opening half of the commit protocol. From here until
# `.commit_skeleton_manifest()` succeeds there is, by definition, no
# trustworthy skeleton dataset -- and a kill, a crash or a failed batch
# must leave it that way. Clearing up-front is what makes that true: a
# manifest that survived an interrupted run would vouch for skeletons it
# no longer describes, which is worse than having no manifest at all.
# Do NOT gate on self$skeleton_manifest being non-NULL: callers routinely
# construct a FRESH study (whose field is NULL) and then
# $adopt_runtime_state_from(<study read from disk>), which deliberately copies
# only runtime state and NOT the manifest. The in-memory field therefore says
# nothing about what is on disk, and gating on it leaves the previous run's
# manifest in place for the whole rebuild -- defeating the protocol in exactly
# the caller it exists to protect.
#
# Read-modify-write the ON-DISK study rather than calling $save_meta(), which
# serialises the whole in-memory object. At this point that object may be LESS
# complete than the file: a caller that had not adopted runtime state would
# overwrite a good registrystudy.qs2 with an empty one, turning a no-op run
# into data loss. Deleting one field from the file cannot clobber anything,
# and skipping the write when there is nothing to clear keeps a first run from
# creating a meta file before set_ids() has ever populated one.
RegistryStudy$set("private", ".invalidate_skeleton_manifest", function() {
  self$skeleton_manifest <- NULL
  path <- self$meta_file
  if (!file.exists(path)) {
    return(invisible(NULL))
  }
  on_disk <- qs2_read(path)
  if (is.null(on_disk$skeleton_manifest)) {
    return(invisible(NULL))
  }
  on_disk$skeleton_manifest <- NULL
  invalidate_candidate_paths(on_disk)
  qs2_write_atomic(on_disk, path)
  cat("Cleared the previous skeleton manifest\n")
  invisible(NULL)
})

# Commit the skeleton manifest, but only if the dataset on disk earns it.
#
# Four ways to fail, each of which would otherwise let s1 run for 14 hours
# on data that cannot support it:
#   * unreadable provenance        -> no sidecar and the skeleton itself is
#                                     not a Skeleton object. NOTE this checks
#                                     the SIDECARS, not the skeletons: the
#                                     meta fast path in
#                                     $skeleton_pipeline_hashes() trusts a
#                                     readable sidecar without opening the
#                                     skeleton beside it. Opening 2,194
#                                     skeletons would mean reading GBs on
#                                     every run. The residual hole is a
#                                     skeleton replaced while its old sidecar
#                                     survives -- both writes are atomic
#                                     (qs2_write_atomic), so neither can be
#                                     torn, but they are separate writes and
#                                     a crash between them leaves a new
#                                     skeleton with a stale sidecar. Closing
#                                     that needs the pair written as one unit.
#   * >1 distinct pipeline hash    -> interrupted replay: a MIX of old and
#                                     new skeletons
#   * hash != study's current hash -> uniformly OBSOLETE. Internal agreement
#                                     is not currency; without this the
#                                     dataset looks perfect and is simply
#                                     out of date.
#   * batch IDs != seq_len(n)      -> incomplete. A count alone cannot tell
#                                     1..N from 2..N+1, and a *first* build
#                                     interrupted at batch 272 leaves 272
#                                     mutually-consistent skeletons that a
#                                     hash-only check waves through.
#
# `full_run` distinguishes "process everything" from a deliberate subset
# (`batches = 1:10`). It does NOT change what is validated -- that is always
# the whole directory, because that is what s1 reads, so a subset run still
# commits when the resulting dataset validates. It changes only what happens
# on FAILURE: a full run raises (otherwise the caller's file-count gate
# reports success for a dataset nothing will accept), a subset returns
# quietly, since a subset cannot be expected to complete the dataset.
RegistryStudy$set("private", ".commit_skeleton_manifest", function(full_run) {
  ph <- self$skeleton_pipeline_hashes()
  current <- self$pipeline_hash()

  fail <- function(msg) {
    # No write here, deliberately. .invalidate_skeleton_manifest() already
    # cleared the manifest ON DISK before any work started, so the file is
    # already in exactly the state this failure wants. Calling $save_meta()
    # would serialise the whole in-memory study over it -- the same clobber
    # hazard the surgical invalidation exists to avoid, on the one path where
    # the in-memory object is least trustworthy.
    self$skeleton_manifest <- NULL
    if (full_run) {
      stop(
        "Refusing to commit a skeleton manifest: ",
        msg,
        "\nThe skeleton dataset is NOT usable by s1. Re-run ",
        "$process_skeletons() to completion.",
        call. = FALSE
      )
    }
    cat(sprintf("Skeleton manifest NOT committed: %s\n", msg))
    invisible(NULL)
  }

  if (nrow(ph) == 0L) {
    return(fail("no skeleton files found"))
  }
  n_bad <- sum(is.na(ph$pipeline_hash))
  if (n_bad > 0L) {
    return(fail(sprintf(
      "%d of %d skeleton files are unreadable or are not Skeleton objects",
      n_bad,
      nrow(ph)
    )))
  }
  hashes <- sort(table(ph$pipeline_hash), decreasing = TRUE)
  if (length(hashes) > 1L) {
    return(fail(sprintf(
      "%d distinct pipeline hashes across %d batches (%s) -- the run did not replay every batch",
      length(hashes),
      nrow(ph),
      paste(
        sprintf("%s x%d", names(hashes), as.integer(hashes)),
        collapse = ", "
      )
    )))
  }
  if (!identical(names(hashes)[1], current)) {
    return(fail(sprintf(
      paste0(
        "skeletons are uniform at %s but the study's current pipeline is %s ",
        "-- they are internally consistent yet obsolete"
      ),
      names(hashes)[1],
      current
    )))
  }
  n_expected <- self$expected_skeleton_file_count
  expected_ids <- seq_len(n_expected)
  found_ids <- sort(ph$batch)
  if (!identical(as.integer(found_ids), as.integer(expected_ids))) {
    missing <- setdiff(expected_ids, found_ids)
    extra <- setdiff(found_ids, expected_ids)
    return(fail(sprintf(
      "batch inventory is wrong: found %d, expected %d (%d missing, %d unexpected)",
      length(found_ids),
      n_expected,
      length(missing),
      length(extra)
    )))
  }

  data.table::setorder(ph, batch)
  self$skeleton_manifest <- list(
    manifest_version = 1L,
    committed_at = Sys.time(),
    swereg_version = as.character(utils::packageVersion("swereg")),
    n_batches = nrow(ph),
    batches = as.integer(ph$batch),
    pipeline_hash = current,
    # Identity of this GENERATION of the data, not of the code that made it.
    # pipeline_hash is derived from function hashes, so rebuilding from
    # changed raw data with unchanged code leaves it identical; built_at
    # moves on every save. Batch IDs and per-batch timestamps are kept
    # associated and numeric -- collapsing them to a sorted set of strings
    # would drop exactly the multiplicity and precision that make this an
    # identity.
    identity = digest::digest(
      list(
        batch = as.integer(ph$batch),
        pipeline_hash = ph$pipeline_hash,
        built_at = as.numeric(ph$saved_at)
      ),
      algo = "xxhash64"
    )
  )
  self$save_meta()
  cat(sprintf(
    "Skeleton manifest committed: %d batches, pipeline %s, identity %s\n",
    nrow(ph),
    current,
    self$skeleton_manifest$identity
  ))
  invisible(NULL)
})
