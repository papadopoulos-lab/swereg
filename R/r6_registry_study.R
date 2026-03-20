# Resolve a path from multiple candidates
.resolve_path <- function(candidates, label) {
  if (length(candidates) == 0) {
    stop(label, ": no paths provided")
  }

  for (p in candidates) {
    if (dir.exists(p)) return(p)
  }
  if (length(candidates) == 1) {
    return(candidates)
  }
  stop(
    label,
    ": none of the candidate paths exist:\n",
    paste("  -", candidates, collapse = "\n")
  )
}

# Detect if swereg was loaded via devtools::load_all()
.swereg_dev_path <- function() {
  pkg_path <- system.file(package = "swereg")
  if (!nzchar(pkg_path)) {
    return(NULL)
  }
  in_library <- any(vapply(
    .libPaths(),
    function(lp) startsWith(pkg_path, lp),
    logical(1)
  ))
  if (!in_library) pkg_path else NULL
}

# Detect rawbatch groups on disk
.detect_rawbatch_groups <- function(rawbatch_dir, group_names, n_batches) {
  saved <- character(0)
  for (g in group_names) {
    all_exist <- all(vapply(
      seq_len(n_batches),
      function(b) {
        file.exists(file.path(
          rawbatch_dir,
          sprintf("%03d_rawbatch_%s.qs2", b, g)
        ))
      },
      logical(1)
    ))
    if (all_exist) saved <- c(saved, g)
  }
  saved
}

# Detect skeleton files on disk
.detect_skeleton_files <- function(skeleton_dir) {
  if (!dir.exists(skeleton_dir)) {
    return(character(0))
  }
  files <- list.files(
    skeleton_dir,
    pattern = "skeleton_\\d+\\.qs2$",
    full.names = TRUE
  )
  sort(files)
}

# Format byte counts for display
.format_bytes <- function(bytes) {
  if (bytes >= 1e9) {
    sprintf("%.1f GB", bytes / 1e9)
  } else if (bytes >= 1e6) {
    sprintf("%.1f MB", bytes / 1e6)
  } else if (bytes >= 1e3) {
    sprintf("%.1f KB", bytes / 1e3)
  } else {
    paste(bytes, "B")
  }
}


.REGISTRY_STUDY_SCHEMA_VERSION <- 1L

# =============================================================================
# RegistryStudy R6 Class
# =============================================================================
# Unified class managing the full skeleton pipeline lifecycle:
#   - Batch configuration
#   - Runtime state
#   - Declarative code registry (register_codes)
#   - Batch processing with parallel support
#
# Directory resolution is portable across machines: the constructor stores
# candidate paths, and active bindings lazily resolve to the first existing
# directory. Resolved paths are cached but auto-invalidated when the cached
# path no longer exists (e.g. after transferring meta to another computer).
#
# Code registry: each register_codes() call declares codes, the function to
# apply them, which data groups to use, and optional prefixing/combining.
# =============================================================================

#' RegistryStudy: Unified R6 class for skeleton pipeline
#'
#' Manages batch directories, batch splitting, code registries, and
#' skeleton processing.
#'
#' @section Portable Directory Resolution:
#' Directories are stored as candidate path vectors and resolved lazily via
#' active bindings. The first existing directory wins and is cached. If the
#' cached path becomes invalid (e.g. after moving to a different machine),
#' the binding automatically re-resolves from the candidate list.
#'
#' @section Code Registry:
#' Code registrations are declarative. Each `register_codes()` call specifies
#' codes, the function to apply them (e.g. `add_diagnoses`), which data groups
#' to use, and optional prefixing/combining. This replaces the old system of
#' separate fields per code type.
#'
#' @examples
#' \dontrun{
#' study <- RegistryStudy$new(
#'   data_generic_dir = c("/linux/path/generic/", "C:/windows/path/generic/"),
#'   data_raw_dir = c("/linux/path/raw/", "C:/windows/path/raw/"),
#'   group_names = c("lmed", "inpatient", "outpatient", "cancer", "dors", "other")
#' )
#' study$register_codes(
#'   codes = list("stroke_any" = c("I60", "I61", "I63")),
#'   fn = add_diagnoses,
#'   groups = list(ov = "outpatient", sv = "inpatient", dors = "dors", can = "cancer"),
#'   combine_as = "osdc"
#' )
#' study$register_codes(
#'   codes = list("rx_n05a" = c("N05A")),
#'   fn = add_rx,
#'   fn_args = list(source = "atc"),
#'   groups = list("lmed")
#' )
#' study$set_ids(ids)
#' study$save_rawbatch("lmed", lmed_data)
#' study$describe_codes()
#' result <- study$process_skeletons(my_fn, n_workers = 4L)
#' }
#'
#' @export
RegistryStudy <- R6::R6Class(
  "RegistryStudy",
  public = list(
    # --- Config fields (set at construction, rarely changed) ---

    #' @field group_names Character vector. Names of rawbatch groups.
    group_names = NULL,

    #' @field batch_size Integer. Number of IDs per batch.
    batch_size = NULL,

    #' @field seed Integer. Shuffle seed for reproducibility.
    seed = NULL,

    #' @field id_col Character. Person ID column name.
    id_col = NULL,

    # --- Runtime state ---

    #' @field n_ids Integer. Total number of IDs across all batches.
    n_ids = NULL,

    #' @field n_batches Integer. Number of batches.
    n_batches = NULL,

    #' @field batch_id_list List of ID vectors, one per batch.
    batch_id_list = NULL,

    #' @field groups_saved Character vector of rawbatch groups saved to disk.
    groups_saved = NULL,

    # --- Code registry ---

    #' @field code_registry List of code registration entries. Each entry is a list
    #'   with: codes, fn, fn_args, groups, combine_as, label.
    code_registry = NULL,

    #' @field created_at POSIXct. Timestamp when this study was created.
    created_at = NULL,

    # --- Constructor ---

    #' @description Create a new RegistryStudy object.
    #' @param data_generic_dir Character vector of candidate paths for
    #'   rawbatch and (by default) skeleton files. Resolved lazily.
    #' @param group_names Character vector of rawbatch group names.
    #' @param skeleton_dir Character vector of candidate paths for skeleton output.
    #'   Defaults to same candidates as `data_generic_dir`.
    #' @param data_raw_dir Character vector of candidate paths for raw registry
    #'   files (optional). NULL if raw data paths are managed externally.
    #' @param batch_size Integer. Number of IDs per batch. Default: 1000L.
    #' @param seed Integer. Shuffle seed.
    #' @param id_col Character. Person ID column name.
    initialize = function(
      data_generic_dir,
      group_names = c(
        "lmed",
        "inpatient",
        "outpatient",
        "cancer",
        "dors",
        "other"
      ),
      skeleton_dir = data_generic_dir,
      data_raw_dir = NULL,
      batch_size = 1000L,
      seed = 4L,
      id_col = "lopnr"
    ) {
      private$.data_generic_dir_candidates <- data_generic_dir
      private$.skeleton_dir_candidates <- skeleton_dir
      if (!is.null(data_raw_dir)) {
        private$.data_raw_dir_candidates <- data_raw_dir
      }
      self$group_names <- group_names
      self$batch_size <- as.integer(batch_size)
      self$seed <- as.integer(seed)
      self$id_col <- id_col

      # Initialize empty state
      self$n_ids <- 0L
      self$n_batches <- 0L
      self$batch_id_list <- list()
      self$groups_saved <- character(0)

      # Initialize empty code registry
      self$code_registry <- list()

      self$created_at <- Sys.time()

      private$.schema_version <- .REGISTRY_STUDY_SCHEMA_VERSION

      invisible(self)
    },

    #' @description Check if this object's schema version matches the current class version.
    #' Warns if the object was saved with an older schema version.
    #' @return `invisible(TRUE)` if versions match, `invisible(FALSE)` otherwise.
    check_version = function() {
      current <- .REGISTRY_STUDY_SCHEMA_VERSION
      saved <- private$.schema_version %||% 0L
      if (saved < current) {
        warning(
          "This ", class(self)[1], " was saved with schema version ", saved,
          " but current version is ", current, ". Re-create this object.",
          call. = FALSE
        )
      }
      invisible(saved == current)
    },

    # --- Code registry methods ---

    #' @description Register code definitions for the code registry.
    #'
    #' Each call declares codes, the function to apply them, which batch data
    #' groups to use, and optional prefixing/combining. Appends to
    #' `self$code_registry`.
    #'
    #' @param codes Named list of code vectors (e.g. ICD-10, ATC, operation codes).
    #' @param fn Function to call (e.g. `add_diagnoses`, `add_rx`).
    #' @param groups Named list mapping prefixes to group names. Unnamed elements
    #'   get no prefix. Each element is a character vector of group names to
    #'   rbindlist before calling `fn`.
    #' @param fn_args Named list of extra arguments to pass to `fn`
    #'   (e.g. `list(source = "atc")`).
    #' @param combine_as Character or NULL. If non-NULL, also run `fn` on all
    #'   groups combined, using this as the prefix.
    #' @param label Character. Human-readable label for describe_codes() output.
    #'   Defaults to deparse(substitute(fn)).
    register_codes = function(
      codes,
      fn,
      groups,
      fn_args = list(),
      combine_as = NULL,
      label = NULL
    ) {
      if (is.null(label)) {
        label <- deparse(substitute(fn))
      }

      # Normalize groups: ensure it's a list
      if (!is.list(groups)) {
        groups <- as.list(groups)
      }

      entry <- list(
        codes = codes,
        fn = fn,
        fn_args = fn_args,
        groups = groups,
        combine_as = combine_as,
        label = label
      )
      self$code_registry[[length(self$code_registry) + 1L]] <- entry
      invisible(self)
    },

    #' @description Print human-readable description of all registered codes.
    describe_codes = function() {
      if (length(self$code_registry) == 0) {
        cat("No codes registered.\n")
        return(invisible(self))
      }

      for (reg in self$code_registry) {
        codes <- reg$codes
        cat(sprintf(
          "\n=== %s (%d entries) ===\n",
          reg$label,
          length(codes)
        ))

        # Describe groups
        group_descs <- vapply(seq_along(reg$groups), function(i) {
          prefix <- names(reg$groups)[i]
          grps <- reg$groups[[i]]
          if (is.null(prefix) || !nzchar(prefix)) {
            paste(grps, collapse = " + ")
          } else {
            paste0(prefix, " (", paste(grps, collapse = " + "), ")")
          }
        }, character(1))
        cat(sprintf("  Groups: %s\n", paste(group_descs, collapse = ", ")))

        if (!is.null(reg$combine_as)) {
          cat(sprintf("  Combined as: %s_*\n", reg$combine_as))
        }

        # Extra fn_args
        if (length(reg$fn_args) > 0) {
          args_str <- paste(
            names(reg$fn_args),
            vapply(reg$fn_args, deparse, character(1)),
            sep = " = ",
            collapse = ", "
          )
          cat(sprintf("  Extra args: %s\n", args_str))
        }

        cat("\n")
        for (nm in names(codes)) {
          cat(sprintf(
            "  %s: %s\n",
            nm,
            paste(codes[[nm]], collapse = ", ")
          ))
          gen_cols <- private$.generated_columns_for_entry(reg, nm)
          cat(sprintf(
            "    -> columns: %s\n",
            paste(gen_cols, collapse = ", ")
          ))
        }
      }
      invisible(self)
    },

    #' @description Return a data.table summarizing all registered codes.
    #' @return data.table with columns: name, codes, label, generated_columns.
    summary_table = function() {
      rows <- list()

      for (reg in self$code_registry) {
        for (nm in names(reg$codes)) {
          gen_cols <- private$.generated_columns_for_entry(reg, nm)
          rows[[length(rows) + 1L]] <- list(
            name = nm,
            codes = paste(reg$codes[[nm]], collapse = ", "),
            label = reg$label,
            generated_columns = paste(gen_cols, collapse = ", ")
          )
        }
      }

      if (length(rows) == 0) {
        return(data.table::data.table(
          name = character(0),
          codes = character(0),
          label = character(0),
          generated_columns = character(0)
        ))
      }
      data.table::rbindlist(rows)
    },

    # --- Apply codes to skeleton ---

    #' @description Apply all registered codes to a skeleton data.table.
    #' @param skeleton data.table. The person-week skeleton to modify in place.
    #' @param batch_data Named list of data.tables from load_rawbatch().
    apply_codes_to_skeleton = function(skeleton, batch_data) {
      id_name <- self$id_col

      for (reg in self$code_registry) {
        # Per-group calls
        for (i in seq_along(reg$groups)) {
          group_names <- reg$groups[[i]]
          prefix <- names(reg$groups)[i]

          # Get data: rbindlist if multiple groups
          data_list <- Filter(
            function(x) !is.null(x) && nrow(x) > 0,
            lapply(group_names, function(g) batch_data[[g]])
          )
          if (length(data_list) == 0) next
          data <- data.table::rbindlist(
            data_list,
            use.names = TRUE,
            fill = TRUE
          )

          # Prefix code names
          if (!is.null(prefix) && nzchar(prefix)) {
            prefixed_codes <- stats::setNames(
              reg$codes,
              paste0(prefix, "_", names(reg$codes))
            )
          } else {
            prefixed_codes <- reg$codes
          }

          # Call fn
          do.call(
            reg$fn,
            c(
              list(skeleton, data, id_name = id_name, codes = prefixed_codes),
              reg$fn_args
            )
          )
        }

        # Combined (combine_as)
        if (!is.null(reg$combine_as)) {
          all_groups <- unique(unlist(reg$groups))
          data_list <- Filter(
            function(x) !is.null(x) && nrow(x) > 0,
            lapply(all_groups, function(g) batch_data[[g]])
          )
          if (length(data_list) > 0) {
            combined_data <- data.table::rbindlist(
              data_list,
              use.names = TRUE,
              fill = TRUE
            )
            combined_codes <- stats::setNames(
              reg$codes,
              paste0(reg$combine_as, "_", names(reg$codes))
            )
            do.call(
              reg$fn,
              c(
                list(
                  skeleton,
                  combined_data,
                  id_name = id_name,
                  codes = combined_codes
                ),
                reg$fn_args
              )
            )
          }
        }
      }

      invisible(skeleton)
    },

    # --- Batch pipeline methods ---

    #' @description Set IDs and split into batches.
    #' @param ids Vector of person IDs.
    set_ids = function(ids) {
      ids <- unique(ids)
      set.seed(self$seed)
      ids <- sample(ids)

      n_chunks <- ceiling(length(ids) / self$batch_size)
      batch_id_list <- split(ids, ceiling(seq_along(ids) / self$batch_size))

      self$n_ids <- as.integer(length(ids))
      self$n_batches <- as.integer(length(batch_id_list))
      self$batch_id_list <- batch_id_list

      # Scan disk for existing rawbatch groups
      self$groups_saved <- .detect_rawbatch_groups(
        self$data_generic_dir,
        self$group_names,
        self$n_batches
      )

      invisible(self)
    },

    #' @description Save rawbatch files for one group.
    #' @param group Character. Group name (must be in group_names).
    #' @param data data.table or named list of data.tables.
    save_rawbatch = function(group, data) {
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
      n_threads <- parallel::detectCores()

      for (b in seq_along(self$batch_id_list)) {
        batch_ids <- self$batch_id_list[[b]]
        if (data.table::is.data.table(data)) {
          batch_data <- data[get(id_col) %in% batch_ids]
        } else {
          batch_data <- lapply(data, function(dt) {
            if (data.table::is.data.table(dt)) {
              dt[get(id_col) %in% batch_ids]
            } else {
              dt
            }
          })
        }
        outfile <- file.path(
          self$data_generic_dir,
          sprintf("%03d_rawbatch_%s.qs2", b, group)
        )
        qs2::qs_save(batch_data, outfile, nthreads = n_threads)
        cat(
          "  batch",
          b,
          "/",
          self$n_batches,
          "(",
          length(batch_ids),
          "IDs) ->",
          group,
          "\n"
        )
      }

      self$groups_saved <- sort(unique(c(self$groups_saved, group)))
      invisible(self)
    },

    #' @description Load rawbatch files for a single batch.
    #' @param batch_number Integer. 1-indexed batch number.
    #' @return Named list of data.tables.
    load_rawbatch = function(batch_number) {
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
          self$data_generic_dir,
          sprintf("%03d_rawbatch_%s.qs2", batch_number, g)
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
    },

    #' @description Process batches through a user-defined function.
    #' @param process_fn Function with signature
    #'   `function(batch_data, batch_number, config)`.
    #' @param batches Integer vector of batch indices, or NULL for all.
    #' @param n_workers Integer. Number of parallel workers (1 = sequential).
    #' @param ... Additional arguments (unused).
    #' @return List with `study` (self, updated) and `results`.
    process_skeletons = function(
      process_fn,
      batches = NULL,
      n_workers = 1L,
      ...
    ) {
      if (is.null(batches)) {
        batches <- seq_len(self$n_batches)
      }
      results <- vector("list", length = self$n_batches)

      if (n_workers <= 1L) {
        progressr::with_progress({
          p <- progressr::progressor(steps = length(batches))
          for (i in batches) {
            batch_data <- self$load_rawbatch(i)
            raw_result <- process_fn(batch_data, i, self)
            if (is.list(raw_result) && "skeleton" %in% names(raw_result)) {
              skeleton_save(
                raw_result$skeleton,
                batch_number = i,
                output_dir = self$skeleton_dir
              )
              results[[i]] <- raw_result$profiling
            } else {
              results[[i]] <- raw_result
            }
            rm(raw_result, batch_data)
            gc()
            p()
          }
        })
      } else {
        threads_per_worker <- max(
          1L,
          floor(parallel::detectCores() / n_workers)
        )
        cat(sprintf(
          "Running %d batches: %d workers x %d threads each\n",
          length(batches),
          n_workers,
          threads_per_worker
        ))
        dev_path <- .swereg_dev_path()

        .launch_batch <- function(batch_idx, study_snapshot) {
          callr::r_bg(
            func = function(
              study_snapshot,
              batch_idx,
              process_fn,
              threads_per_worker,
              dev_path
            ) {
              requireNamespace("data.table")
              data.table::setDTthreads(threads_per_worker)
              if (!is.null(dev_path)) {
                getExportedValue("devtools", "load_all")(dev_path, quiet = TRUE)
              } else {
                requireNamespace("swereg")
              }
              batch_data <- study_snapshot$load_rawbatch(batch_idx)
              raw_result <- process_fn(batch_data, batch_idx, study_snapshot)
              rm(batch_data)
              gc()

              if (is.list(raw_result) && "skeleton" %in% names(raw_result)) {
                swereg::skeleton_save(
                  raw_result$skeleton,
                  batch_number = batch_idx,
                  output_dir = study_snapshot$skeleton_dir
                )
                raw_result$profiling
              } else {
                raw_result
              }
            },
            args = list(
              study_snapshot = study_snapshot,
              batch_idx = batch_idx,
              process_fn = process_fn,
              threads_per_worker = threads_per_worker,
              dev_path = dev_path
            ),
            package = FALSE,
            supervise = TRUE
          )
        }

        progressr::with_progress({
          p <- progressr::progressor(steps = length(batches))
          active <- list()
          next_i <- 1L

          while (length(active) < n_workers && next_i <= length(batches)) {
            slot <- as.character(length(active) + 1L)
            active[[slot]] <- list(
              proc = .launch_batch(batches[next_i], self),
              idx = next_i
            )
            next_i <- next_i + 1L
          }

          while (length(active) > 0) {
            Sys.sleep(0.5)
            finished_slots <- c()
            for (slot in names(active)) {
              if (!active[[slot]]$proc$is_alive()) {
                finished_slots <- c(finished_slots, slot)
                idx <- active[[slot]]$idx
                tryCatch(
                  {
                    results[[batches[idx]]] <- active[[slot]]$proc$get_result()
                    p()
                  },
                  error = function(e) {
                    warning(
                      "Batch ",
                      batches[idx],
                      " failed: ",
                      conditionMessage(e),
                      call. = FALSE
                    )
                  }
                )
              }
            }
            for (slot in finished_slots) {
              active[[slot]] <- NULL
              if (next_i <= length(batches)) {
                active[[slot]] <- list(
                  proc = .launch_batch(batches[next_i], self),
                  idx = next_i
                )
                next_i <- next_i + 1L
              }
            }
          }
        })
      }

      list(study = self, results = results)
    },

    #' @description Delete all rawbatch files from disk.
    delete_rawbatches = function() {
      files <- list.files(
        self$data_generic_dir,
        pattern = "\\d+_rawbatch_.*\\.qs2$",
        full.names = TRUE
      )
      if (length(files) > 0) {
        cat("Deleting", length(files), "rawbatch files\n")
        file.remove(files)
      }
      self$groups_saved <- character(0)
      invisible(self)
    },

    #' @description Delete all skeleton output files from disk.
    delete_skeletons = function() {
      files <- list.files(
        self$skeleton_dir,
        pattern = "skeleton_\\d+\\.qs2$",
        full.names = TRUE
      )
      if (length(files) > 0) {
        cat("Deleting", length(files), "skeleton files\n")
        file.remove(files)
      }
      invisible(self)
    },

    #' @description Reset the pipeline: delete rawbatches, skeletons, and meta file.
    reset = function() {
      self$delete_rawbatches()
      self$delete_skeletons()
      if (file.exists(self$meta_file)) {
        cat("Deleting", self$meta_file, "\n")
        file.remove(self$meta_file)
      }
      cat("Reset complete\n")
      invisible(self)
    },

    #' @description Save this study object as metadata.
    save_meta = function() {
      qs2::qs_save(self, self$meta_file)
      cat("Saved", self$meta_file, "\n")
      invisible(self)
    },

    #' @description Print method for RegistryStudy.
    print = function(...) {
      cat("<RegistryStudy>\n")

      # Created timestamp
      if (!is.null(self$created_at)) {
        cat("  Created:", format(self$created_at, "%Y-%m-%d %H:%M:%S"), "\n")
      }

      cat("  IDs:", format(self$n_ids, big.mark = ","), "total\n")

      # Code registry
      if (length(self$code_registry) > 0) {
        parts <- vapply(self$code_registry, function(reg) {
          sprintf("%d %s", length(reg$codes), reg$label)
        }, character(1))
        cat("  Code registry:", paste(parts, collapse = ", "), "\n")
        # Count generated columns
        n_cols <- sum(vapply(self$code_registry, function(reg) {
          n_codes <- length(reg$codes)
          n_groups <- length(reg$groups)
          n_combine <- if (!is.null(reg$combine_as)) 1L else 0L
          n_codes * (n_groups + n_combine)
        }, integer(1)))
        cat("  Generated columns:", n_cols, "\n")
      }

      # Pipeline status: batches → rawbatch → skeleton (grouped together)
      cat(
        "  Batches:",
        self$n_batches,
        "(",
        format(self$batch_size, big.mark = ","),
        "IDs each)\n"
      )

      # Rawbatch info
      if (length(self$groups_saved) > 0) {
        rb_files <- list.files(
          self$data_generic_dir,
          pattern = "\\d+_rawbatch_.*\\.qs2$",
          full.names = TRUE
        )
        rb_size <- sum(file.size(rb_files), na.rm = TRUE)
        cat(
          "  Rawbatch groups saved:",
          paste(self$groups_saved, collapse = ", "),
          "(",
          length(rb_files),
          "files,",
          .format_bytes(rb_size),
          ")\n"
        )
      } else {
        cat("  Rawbatch groups saved: (none)\n")
      }

      # Skeleton info
      n_observed <- length(self$skeleton_files)
      n_expected <- self$expected_skeleton_file_count
      if (n_observed > 0) {
        sk_size <- sum(file.size(self$skeleton_files), na.rm = TRUE)
        cat(
          "  Skeleton files:",
          n_observed,
          "/",
          n_expected,
          "expected",
          "(",
          .format_bytes(sk_size),
          ")\n"
        )
      } else {
        cat("  Skeleton files: 0 /", n_expected, "expected\n")
      }

      # Dirs — show all candidates, mark resolved one with >
      .print_dir_candidates <- function(label, candidates, resolved) {
        cat("  ", label, ":\n", sep = "")
        for (p in candidates) {
          prefix <- if (identical(p, resolved)) "  > " else "    "
          cat(prefix, p, "\n", sep = "")
        }
      }

      .print_dir_candidates(
        "Data generic",
        private$.data_generic_dir_candidates,
        self$data_generic_dir
      )
      if (
        !identical(
          private$.skeleton_dir_candidates,
          private$.data_generic_dir_candidates
        )
      ) {
        .print_dir_candidates(
          "Skeleton dir",
          private$.skeleton_dir_candidates,
          self$skeleton_dir
        )
      }
      if (!is.null(private$.data_raw_dir_candidates)) {
        .print_dir_candidates(
          "Data raw",
          private$.data_raw_dir_candidates,
          self$data_raw_dir
        )
      }

      invisible(self)
    }
  ),

  active = list(
    #' @field data_generic_dir Character (read-only). Resolved path for rawbatch
    #'   and (by default) skeleton files. Lazily resolved from candidates.
    data_generic_dir = function(value) {
      if (!missing(value)) {
        stop("data_generic_dir is read-only; set via constructor")
      }
      private$.resolve_dir(
        private$.data_generic_dir_candidates,
        ".data_generic_dir_cache",
        "data_generic_dir"
      )
    },

    #' @field skeleton_dir Character (read-only). Resolved path for skeleton output.
    skeleton_dir = function(value) {
      if (!missing(value)) {
        stop("skeleton_dir is read-only; set via constructor")
      }
      private$.resolve_dir(
        private$.skeleton_dir_candidates,
        ".skeleton_dir_cache",
        "skeleton_dir"
      )
    },

    #' @field data_raw_dir Character or NULL (read-only). Resolved path for raw
    #'   registry files. NULL if not configured.
    data_raw_dir = function(value) {
      if (!missing(value)) {
        stop("data_raw_dir is read-only; set via constructor")
      }
      if (is.null(private$.data_raw_dir_candidates)) {
        return(NULL)
      }
      private$.resolve_dir(
        private$.data_raw_dir_candidates,
        ".data_raw_dir_cache",
        "data_raw_dir"
      )
    },

    #' @field skeleton_files Character vector (read-only). Skeleton output file
    #'   paths detected on disk. Scans `skeleton_dir` on each access.
    skeleton_files = function(value) {
      if (!missing(value)) {
        stop("skeleton_files is read-only; populated from disk")
      }
      .detect_skeleton_files(self$skeleton_dir)
    },

    #' @field expected_skeleton_file_count Integer (read-only). Expected number
    #'   of skeleton files (one per batch).
    expected_skeleton_file_count = function() {
      as.integer(self$n_batches)
    },

    #' @field meta_file Character. Path to the metadata file.
    meta_file = function() {
      file.path(self$data_generic_dir, "registry_study_meta.qs2")
    }
  ),

  private = list(
    .schema_version = NULL,

    # --- Directory candidates and caches (persisted in meta file) ---
    .data_generic_dir_candidates = NULL,
    .data_generic_dir_cache = NULL,
    .skeleton_dir_candidates = NULL,
    .skeleton_dir_cache = NULL,
    .data_raw_dir_candidates = NULL,
    .data_raw_dir_cache = NULL,

    .resolve_dir = function(candidates, cache_field, label) {
      cached <- private[[cache_field]]
      if (!is.null(cached) && dir.exists(cached)) {
        return(cached)
      }
      resolved <- .resolve_path(candidates, label)
      private[[cache_field]] <- resolved
      resolved
    },

    # Compute generated column names for a single code entry in a registration
    .generated_columns_for_entry = function(reg, code_name) {
      cols <- character(0)
      for (i in seq_along(reg$groups)) {
        prefix <- names(reg$groups)[i]
        if (!is.null(prefix) && nzchar(prefix)) {
          cols <- c(cols, paste0(prefix, "_", code_name))
        } else {
          cols <- c(cols, code_name)
        }
      }
      if (!is.null(reg$combine_as)) {
        cols <- c(cols, paste0(reg$combine_as, "_", code_name))
      }
      cols
    }
  )
)
