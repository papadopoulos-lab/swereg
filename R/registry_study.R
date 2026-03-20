# =============================================================================
# Internal helpers (shared across package)
# =============================================================================

# Serialization wrappers (qs2 standard format)
.qs_save <- function(x, file, nthreads = 1L, ...) {
  qs2::qs_save(x, file, nthreads = nthreads)
}

.qs_read <- function(file, nthreads = 1L, ...) {
  tryCatch(
    qs2::qd_read(file, nthreads = nthreads),
    error = function(e) {
      if (grepl("qs2 format", conditionMessage(e))) {
        qs2::qs_read(file, nthreads = nthreads)
      } else {
        stop(e)
      }
    }
  )
}

#' Read a qs2 file (auto-detecting format)
#'
#' Reads files saved with either `qs2::qd_save` (qdata format) or
#' `qs2::qs_save` (standard format). Tries qdata first, falls back to standard.
#'
#' @param file Path to the .qs2 file.
#' @param nthreads Number of threads for decompression.
#' @return The deserialized R object.
#' @export
qs2_read <- function(file, nthreads = 1L) {
  .qs_read(file, nthreads = nthreads)
}

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
    label, ": none of the candidate paths exist:\n",
    paste("  -", candidates, collapse = "\n")
  )
}

# Detect if swereg was loaded via devtools::load_all()
.swereg_dev_path <- function() {
  pkg_path <- system.file(package = "swereg")
  if (!nzchar(pkg_path)) return(NULL)
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
  if (!dir.exists(skeleton_dir)) return(character(0))
  files <- list.files(
    skeleton_dir,
    pattern = "skeleton_\\d+_\\d+\\.qs2$",
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


# =============================================================================
# RegistryStudy R6 Class
# =============================================================================
# Unified class managing the full skeleton pipeline lifecycle:
#   - Batch configuration
#   - Runtime state
#   - Code registry (4 code types with auto-prefixing)
#   - Batch processing with parallel support
#
# Directory resolution is portable across machines: the constructor stores
# candidate paths, and active bindings lazily resolve to the first existing
# directory. Resolved paths are cached but auto-invalidated when the cached
# path no longer exists (e.g. after transferring meta to another computer).
#
# Code registry types:
#   icd10_codes      -> 5 cols per entry: ov_, sv_, dors_, can_, osdc_ (OR'd)
#   rx_atc_codes     -> 1 col per entry, no prefix
#   rx_produkt_codes -> 1 col per entry, no prefix
#   operation_codes  -> 1 col per entry, no prefix
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
#' Four named lists define diagnosis/medication/operation codes:
#' \describe{
#'   \item{icd10_codes}{ICD-10 codes applied to 4 registries (ov, sv, dors, can).
#'     Generates 5 columns per entry: `ov_`, `sv_`, `dors_`, `can_`, `osdc_` (all combined).}
#'   \item{rx_atc_codes}{ATC codes applied to lmed. 1 column per entry, no prefix.}
#'   \item{rx_produkt_codes}{Product names applied to lmed. 1 column per entry, no prefix.}
#'   \item{operation_codes}{Operation codes applied to sv+ov combined. 1 column per entry, no prefix.}
#' }
#'
#' @examples
#' \dontrun{
#' study <- RegistryStudy$new(
#'   data_generic_dir = c("/linux/path/generic/", "C:/windows/path/generic/"),
#'   data_raw_dir = c("/linux/path/raw/", "C:/windows/path/raw/"),
#'   group_names = c("lmed", "inpatient", "outpatient", "cancer", "dors", "other")
#' )
#' study$icd10_codes <- list("stroke_any" = c("I60", "I61", "I63"))
#' study$rx_atc_codes <- list("rx_n05a" = c("N05A"))
#' study$set_ids(ids)
#' study$save_rawbatch("lmed", lmed_data)
#' study$describe_codes()
#' result <- study$process_skeletons(my_fn, n_workers = 4L)
#' }
#'
#' @export
RegistryStudy <- R6::R6Class("RegistryStudy",
  public = list(
    # --- Config fields (set at construction, rarely changed) ---

    #' @field group_names Character vector. Names of rawbatch groups.
    group_names = NULL,

    #' @field batch_sizes Integer vector. IDs per batch (first = dev, second = rest).
    batch_sizes = NULL,

    #' @field seed Integer. Shuffle seed for reproducibility.
    seed = NULL,

    #' @field id_col Character. Person ID column name.
    id_col = NULL,

    #' @field ids_per_skeleton_file Integer. IDs per skeleton sub-file.
    ids_per_skeleton_file = NULL,

    # --- Runtime state ---

    #' @field n_ids Integer. Total number of IDs across all batches.
    n_ids = NULL,

    #' @field n_batches Integer. Number of batches.
    n_batches = NULL,

    #' @field batch_id_list List of ID vectors, one per batch.
    batch_id_list = NULL,

    #' @field groups_saved Character vector of rawbatch groups saved to disk.
    groups_saved = NULL,

    # --- 4 code lists ---

    #' @field icd10_codes Named list of character vectors. ICD-10 codes applied to
    #'   4 diagnosis registries (ov, sv, dors, can) + combined (osdc).
    icd10_codes = NULL,

    #' @field rx_atc_codes Named list of character vectors. ATC codes applied to lmed.
    rx_atc_codes = NULL,

    #' @field rx_produkt_codes Named list of character vectors. Product names applied to lmed.
    rx_produkt_codes = NULL,

    #' @field operation_codes Named list of character vectors. Operation codes applied to sv+ov.
    operation_codes = NULL,

    #' @field icdo3_codes Named list of character vectors. ICD-O-3 topography codes
    #'   applied to the cancer registry.
    icdo3_codes = NULL,

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
    #' @param batch_sizes Integer vector. First = dev batch size, second = rest.
    #' @param seed Integer. Shuffle seed.
    #' @param id_col Character. Person ID column name.
    #' @param ids_per_skeleton_file Integer. IDs per skeleton sub-file.
    initialize = function(
      data_generic_dir,
      group_names = c("lmed", "inpatient", "outpatient", "cancer", "dors", "other"),
      skeleton_dir = data_generic_dir,
      data_raw_dir = NULL,
      batch_sizes = c(1000L, 10000L),
      seed = 4L,
      id_col = "lopnr",
      ids_per_skeleton_file = 1000L
    ) {
      private$.data_generic_dir_candidates <- data_generic_dir
      private$.skeleton_dir_candidates <- skeleton_dir
      if (!is.null(data_raw_dir)) {
        private$.data_raw_dir_candidates <- data_raw_dir
      }
      self$group_names <- group_names
      self$batch_sizes <- as.integer(batch_sizes)
      self$seed <- as.integer(seed)
      self$id_col <- id_col
      self$ids_per_skeleton_file <- as.integer(ids_per_skeleton_file)

      # Initialize empty state
      self$n_ids <- 0L
      self$n_batches <- 0L
      self$batch_id_list <- list()
      self$groups_saved <- character(0)

      # Initialize empty code lists
      self$icd10_codes <- list()
      self$rx_atc_codes <- list()
      self$rx_produkt_codes <- list()
      self$operation_codes <- list()
      self$icdo3_codes <- list()

      self$created_at <- Sys.time()

      invisible(self)
    },

    # --- Code registry methods ---

    #' @description Register code definitions for the code registry.
    #' @param icd10_codes Named list of ICD-10 code vectors (optional).
    #' @param icdo3_codes Named list of ICD-O-3 code vectors (optional).
    #' @param operation_codes Named list of operation code vectors (optional).
    #' @param rx_atc_codes Named list of ATC code vectors (optional).
    #' @param rx_produkt_codes Named list of product name vectors (optional).
    register_codes = function(
      icd10_codes = NULL,
      icdo3_codes = NULL,
      operation_codes = NULL,
      rx_atc_codes = NULL,
      rx_produkt_codes = NULL
    ) {
      if (!is.null(icd10_codes)) self$icd10_codes <- icd10_codes
      if (!is.null(icdo3_codes)) self$icdo3_codes <- icdo3_codes
      if (!is.null(operation_codes)) self$operation_codes <- operation_codes
      if (!is.null(rx_atc_codes)) self$rx_atc_codes <- rx_atc_codes
      if (!is.null(rx_produkt_codes)) self$rx_produkt_codes <- rx_produkt_codes
      invisible(self)
    },

    #' @description Print human-readable description of all registered codes.
    #' @param type Optional character. Filter to one type: "icd10", "rx_atc",
    #'   "rx_produkt", "operation", "icdo3". NULL shows all.
    describe_codes = function(type = NULL) {
      types <- private$.code_type_map(type)

      for (info in types) {
        codes <- self[[info$field]]
        if (length(codes) == 0) next

        cat(sprintf("\n=== %s (%d entries) ===\n", info$label, length(codes)))
        cat(sprintf("  Applied to: %s\n", info$applied_to))
        cat(sprintf("  Prefix: %s\n\n", info$prefix_desc))

        for (nm in names(codes)) {
          cat(sprintf("  %s: %s\n", nm, paste(codes[[nm]], collapse = ", ")))
          if (info$field == "icd10_codes") {
            cols <- paste0(
              c("ov_", "sv_", "dors_", "can_", "osdc_"), nm
            )
            cat(sprintf("    -> columns: %s\n", paste(cols, collapse = ", ")))
          }
        }
      }
      invisible(self)
    },

    #' @description Return a data.table summarizing all registered codes.
    #' @param type Optional character filter (see describe_codes).
    #' @return data.table with columns: name, codes, type, generated_columns.
    summary_table = function(type = NULL) {
      types <- private$.code_type_map(type)
      rows <- list()

      for (info in types) {
        codes <- self[[info$field]]
        if (length(codes) == 0) next

        for (nm in names(codes)) {
          if (info$field == "icd10_codes") {
            gen_cols <- paste0(
              c("ov_", "sv_", "dors_", "can_", "osdc_"), nm
            )
          } else {
            gen_cols <- nm
          }
          rows[[length(rows) + 1L]] <- list(
            name = nm,
            codes = paste(codes[[nm]], collapse = ", "),
            type = info$type,
            generated_columns = paste(gen_cols, collapse = ", ")
          )
        }
      }

      if (length(rows) == 0) {
        return(data.table::data.table(
          name = character(0),
          codes = character(0),
          type = character(0),
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

      # --- ICD-10 codes: 4 registries + combined ---
      if (length(self$icd10_codes) > 0) {
        source_map <- list(
          ov = "outpatient",
          sv = "inpatient",
          dors = "dors",
          can = "cancer"
        )

        for (prefix in names(source_map)) {
          group_name <- source_map[[prefix]]
          registry_data <- batch_data[[group_name]]
          if (is.null(registry_data) || nrow(registry_data) == 0) next

          prefixed_diags <- stats::setNames(
            self$icd10_codes,
            paste0(prefix, "_", names(self$icd10_codes))
          )
          add_diagnoses(
            skeleton,
            registry_data,
            id_name = id_name,
            diags = prefixed_diags
          )
        }

        # Combined (osdc_) = all 4 sources OR'd together
        all_diag_data <- data.table::rbindlist(
          Filter(
            function(x) !is.null(x) && nrow(x) > 0,
            list(
              batch_data[["outpatient"]],
              batch_data[["inpatient"]],
              batch_data[["dors"]],
              batch_data[["cancer"]]
            )
          ),
          use.names = TRUE,
          fill = TRUE
        )
        if (nrow(all_diag_data) > 0) {
          osdc_diags <- stats::setNames(
            self$icd10_codes,
            paste0("osdc_", names(self$icd10_codes))
          )
          add_diagnoses(
            skeleton,
            all_diag_data,
            id_name = id_name,
            diags = osdc_diags
          )
        }
      }

      # --- ICD-O-3 codes: cancer registry only ---
      if (length(self$icdo3_codes) > 0) {
        cancer_data <- batch_data[["cancer"]]
        if (!is.null(cancer_data) && nrow(cancer_data) > 0) {
          add_icdo3s(
            skeleton,
            cancer_data,
            id_name = id_name,
            icdo3s = self$icdo3_codes
          )
        }
      }

      # --- Operation codes: inpatient + outpatient combined ---
      if (length(self$operation_codes) > 0) {
        ops_data <- data.table::rbindlist(
          Filter(
            function(x) !is.null(x) && nrow(x) > 0,
            list(batch_data[["inpatient"]], batch_data[["outpatient"]])
          ),
          use.names = TRUE,
          fill = TRUE
        )
        if (nrow(ops_data) > 0) {
          add_operations(
            skeleton,
            ops_data,
            id_name = id_name,
            ops = self$operation_codes
          )
        }
      }

      # --- ATC rx codes: lmed ---
      if (length(self$rx_atc_codes) > 0) {
        lmed_data <- batch_data[["lmed"]]
        if (!is.null(lmed_data) && nrow(lmed_data) > 0) {
          add_rx(
            skeleton,
            lmed_data,
            id_name = id_name,
            rxs = self$rx_atc_codes,
            source = "atc"
          )
        }
      }

      # --- Product rx codes: lmed ---
      if (length(self$rx_produkt_codes) > 0) {
        lmed_data <- batch_data[["lmed"]]
        if (!is.null(lmed_data) && nrow(lmed_data) > 0) {
          add_rx(
            skeleton,
            lmed_data,
            id_name = id_name,
            rxs = self$rx_produkt_codes,
            source = "produkt"
          )
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

      bs <- self$batch_sizes
      first_size <- bs[1]
      rest_size <- if (length(bs) >= 2) bs[2] else bs[1]

      batch_id_list <- list()
      if (length(ids) <= first_size) {
        batch_id_list <- list(ids)
      } else {
        batch_id_list[[1]] <- ids[seq_len(first_size)]
        remaining <- ids[(first_size + 1L):length(ids)]
        n_remaining <- length(remaining)
        n_chunks <- ceiling(n_remaining / rest_size)
        for (i in seq_len(n_chunks)) {
          start <- (i - 1L) * rest_size + 1L
          end <- min(i * rest_size, n_remaining)
          batch_id_list[[i + 1L]] <- remaining[start:end]
        }
      }

      self$n_ids <- as.integer(length(ids))
      self$n_batches <- as.integer(length(batch_id_list))
      self$batch_id_list <- batch_id_list

      # Scan disk for existing rawbatch groups
      self$groups_saved <- .detect_rawbatch_groups(
        self$data_generic_dir, self$group_names, self$n_batches
      )

      invisible(self)
    },

    #' @description Save rawbatch files for one group.
    #' @param group Character. Group name (must be in group_names).
    #' @param data data.table or named list of data.tables.
    save_rawbatch = function(group, data) {
      if (!group %in% self$group_names) {
        stop(
          "group '", group, "' not in group_names: ",
          paste(self$group_names, collapse = ", ")
        )
      }

      if (group %in% self$groups_saved) {
        cat("Skipping '", group, "' -- all rawbatch files already exist\n", sep = "")
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
        .qs_save(batch_data, outfile, nthreads = n_threads)
        cat(
          "  batch", b, "/", self$n_batches,
          "(", length(batch_ids), "IDs) ->", group, "\n"
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
          "batch_number must be between 1 and ", self$n_batches,
          " (got ", batch_number, ")"
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
        obj <- .qs_read(fpath, nthreads = n_threads)

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
    process_skeletons = function(process_fn, batches = NULL, n_workers = 1L, ...) {
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
                output_dir = self$skeleton_dir,
                ids_per_file = self$ids_per_skeleton_file,
                id_col = "id"
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
        threads_per_worker <- max(1L, floor(parallel::detectCores() / n_workers))
        cat(sprintf(
          "Running %d batches: %d workers x %d threads each\n",
          length(batches), n_workers, threads_per_worker
        ))
        dev_path <- .swereg_dev_path()

        .launch_batch <- function(batch_idx, study_snapshot) {
          callr::r_bg(
            func = function(study_snapshot, batch_idx, process_fn,
                            threads_per_worker, dev_path) {
              requireNamespace("data.table")
              data.table::setDTthreads(threads_per_worker)
              if (!is.null(dev_path)) {
                getExportedValue("devtools", "load_all")(dev_path, quiet = TRUE)
              } else {
                requireNamespace("swereg")
              }
              batch_data <- study_snapshot$load_rawbatch(batch_idx)
              raw_result <- process_fn(batch_data, batch_idx, study_snapshot)
              rm(batch_data); gc()

              if (is.list(raw_result) && "skeleton" %in% names(raw_result)) {
                swereg::skeleton_save(
                  raw_result$skeleton,
                  batch_number = batch_idx,
                  output_dir = study_snapshot$skeleton_dir,
                  ids_per_file = study_snapshot$ids_per_skeleton_file,
                  id_col = "id"
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
                      "Batch ", batches[idx], " failed: ", conditionMessage(e),
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
        pattern = "skeleton_\\d+_\\d+\\.qs2$",
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
      .qs_save(self, self$meta_file)
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
      n_icd10 <- length(self$icd10_codes)
      n_atc <- length(self$rx_atc_codes)
      n_prod <- length(self$rx_produkt_codes)
      n_ops <- length(self$operation_codes)
      n_icdo3 <- length(self$icdo3_codes)
      n_total <- n_icd10 + n_atc + n_prod + n_ops + n_icdo3
      if (n_total > 0) {
        parts <- c()
        if (n_icd10 > 0) parts <- c(parts, sprintf("%d icd10", n_icd10))
        if (n_atc > 0) parts <- c(parts, sprintf("%d rx_atc", n_atc))
        if (n_prod > 0) parts <- c(parts, sprintf("%d rx_produkt", n_prod))
        if (n_ops > 0) parts <- c(parts, sprintf("%d operation", n_ops))
        if (n_icdo3 > 0) parts <- c(parts, sprintf("%d icdo3", n_icdo3))
        cat("  Code registry:", paste(parts, collapse = ", "), "\n")
        # Count generated columns
        n_cols <- n_icd10 * 5 + n_atc + n_prod + n_ops + n_icdo3
        cat("  Generated columns:", n_cols, "\n")
      }

      # Pipeline status: batches → rawbatch → skeleton (grouped together)
      if (length(self$batch_sizes) == 1 || self$n_batches <= 1) {
        cat(
          "  Batches:", self$n_batches,
          "(", format(self$batch_sizes[1], big.mark = ","), "IDs)\n"
        )
      } else {
        cat(
          "  Batches:", self$n_batches,
          "(first:", format(self$batch_sizes[1], big.mark = ","),
          "IDs, rest:", format(self$batch_sizes[2], big.mark = ","),
          "IDs)\n"
        )
      }

      # Rawbatch info
      if (length(self$groups_saved) > 0) {
        rb_files <- list.files(
          self$data_generic_dir,
          pattern = "\\d+_rawbatch_.*\\.qs2$",
          full.names = TRUE
        )
        rb_size <- sum(file.size(rb_files), na.rm = TRUE)
        cat(
          "  Rawbatch groups saved:", paste(self$groups_saved, collapse = ", "),
          "(", length(rb_files), "files,",
          .format_bytes(rb_size), ")\n"
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
          "  Skeleton files:", n_observed, "/", n_expected, "expected",
          "(", .format_bytes(sk_size), ")\n"
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
      if (!identical(
        private$.skeleton_dir_candidates,
        private$.data_generic_dir_candidates
      )) {
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
      if (!missing(value)) stop("data_generic_dir is read-only; set via constructor")
      private$.resolve_dir(
        private$.data_generic_dir_candidates,
        ".data_generic_dir_cache",
        "data_generic_dir"
      )
    },

    #' @field skeleton_dir Character (read-only). Resolved path for skeleton output.
    skeleton_dir = function(value) {
      if (!missing(value)) stop("skeleton_dir is read-only; set via constructor")
      private$.resolve_dir(
        private$.skeleton_dir_candidates,
        ".skeleton_dir_cache",
        "skeleton_dir"
      )
    },

    #' @field data_raw_dir Character or NULL (read-only). Resolved path for raw
    #'   registry files. NULL if not configured.
    data_raw_dir = function(value) {
      if (!missing(value)) stop("data_raw_dir is read-only; set via constructor")
      if (is.null(private$.data_raw_dir_candidates)) return(NULL)
      private$.resolve_dir(
        private$.data_raw_dir_candidates,
        ".data_raw_dir_cache",
        "data_raw_dir"
      )
    },

    #' @field skeleton_files Character vector (read-only). Skeleton output file
    #'   paths detected on disk. Scans `skeleton_dir` on each access.
    skeleton_files = function(value) {
      if (!missing(value)) stop("skeleton_files is read-only; populated from disk")
      .detect_skeleton_files(self$skeleton_dir)
    },

    #' @field expected_skeleton_file_count Integer (read-only). Expected number
    #'   of skeleton files based on batch configuration.
    expected_skeleton_file_count = function() {
      if (self$n_batches == 0L) return(0L)
      as.integer(sum(vapply(
        self$batch_id_list,
        function(ids) as.integer(ceiling(length(ids) / self$ids_per_skeleton_file)),
        integer(1)
      )))
    },

    #' @field meta_file Character. Path to the metadata file.
    meta_file = function() {
      file.path(self$data_generic_dir, "registry_study_meta.qs2")
    }
  ),

  private = list(
    # --- Directory candidates and caches (persisted in meta file) ---
    .data_generic_dir_candidates = NULL,
    .data_generic_dir_cache = NULL,
    .skeleton_dir_candidates = NULL,
    .skeleton_dir_cache = NULL,
    .data_raw_dir_candidates = NULL,
    .data_raw_dir_cache = NULL,

    .resolve_dir = function(candidates, cache_field, label) {
      cached <- private[[cache_field]]
      if (!is.null(cached) && dir.exists(cached)) return(cached)
      resolved <- .resolve_path(candidates, label)
      private[[cache_field]] <- resolved
      resolved
    },

    .code_type_map = function(type = NULL) {
      all_types <- list(
        list(
          type = "icd10", field = "icd10_codes",
          label = "ICD-10 Diagnosis Codes",
          applied_to = "ov (outpatient), sv (inpatient), dors (cause of death), can (cancer)",
          prefix_desc = "ov_{name}, sv_{name}, dors_{name}, can_{name}, osdc_{name}"
        ),
        list(
          type = "icdo3", field = "icdo3_codes",
          label = "ICD-O-3 Topography Codes",
          applied_to = "cancer registry",
          prefix_desc = "{name} (no prefix)"
        ),
        list(
          type = "rx_atc", field = "rx_atc_codes",
          label = "ATC Prescription Codes",
          applied_to = "lmed (prescribed drug register)",
          prefix_desc = "{name} (no prefix)"
        ),
        list(
          type = "rx_produkt", field = "rx_produkt_codes",
          label = "Product Name Prescription Codes",
          applied_to = "lmed (prescribed drug register)",
          prefix_desc = "{name} (no prefix)"
        ),
        list(
          type = "operation", field = "operation_codes",
          label = "Operation Codes",
          applied_to = "sv (inpatient) + ov (outpatient) combined",
          prefix_desc = "{name} (no prefix)"
        )
      )

      if (!is.null(type)) {
        all_types <- Filter(function(x) x$type == type, all_types)
        if (length(all_types) == 0) {
          stop("Unknown code type: '", type, "'. Use: icd10, icdo3, rx_atc, rx_produkt, operation")
        }
      }
      all_types
    }
  )
)
