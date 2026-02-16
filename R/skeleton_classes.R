# =============================================================================
# S7 Classes for Skeleton Pipeline
# =============================================================================
# SkeletonConfig: Immutable specification for rawbatch/skeleton directories
# SkeletonMeta: Runtime state -- batched IDs + disk file tracking
#
# The skeleton pipeline processes large registry data in memory-friendly batches:
#   original (raw registry files)
#   -> rawbatch (split into groups per batch of IDs)
#   -> skeleton (processed weekly panel output)
# =============================================================================

# Internal wrappers for qs (archived from CRAN, cannot list in DESCRIPTION)
.qs_save <- function(...) getExportedValue("qs", "qsave")(...)
.qs_read <- function(...) getExportedValue("qs", "qread")(...)

# Internal: resolve a path from multiple candidates
# - Multiple candidates + none exist -> error
# - Single candidate + doesn't exist -> accept as-is (will be created)
# - Multiple candidates, one exists -> return that one
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


# -----------------------------------------------------------------------------
# SkeletonConfig: Pipeline specification (immutable)
# -----------------------------------------------------------------------------

#' SkeletonConfig class for skeleton pipeline
#'
#' Holds directory paths, group names, batch sizing, and other configuration
#' for the rawbatch/skeleton pipeline. Analogous to [TTEDesign] -- specifies
#' the "schema" without holding data.
#'
#' @param rawbatch_dir Character, resolved single path for rawbatch files.
#' @param skeleton_dir Character, resolved single path for skeleton output.
#' @param group_names Character vector, names of rawbatch groups
#'   (e.g., `c("lmed", "diagnoses_and_operations", "other")`).
#' @param batch_sizes Integer vector, IDs per batch. First element is used for
#'   batch 1 (development batch), second element for all remaining batches.
#'   Default: `c(1000L, 10000L)`.
#' @param seed Integer, shuffle seed for reproducibility. Default: `4L`.
#' @param id_col Character, name of the person ID column. Default: `"lopnr"`.
#' @param ids_per_skeleton_file Integer, number of IDs per skeleton sub-file.
#'   Default: `1000L`.
#' @param meta_file Character (read-only). Full path to the
#'   `skeleton_meta.qs` file, derived from `rawbatch_dir`.
#'
#' @examples
#' \dontrun{
#' config <- skeleton_config(
#'   rawbatch_dir = c(
#'     "/data/argos/Bronze/Embla_data/_MHT/2026/generic/",
#'     "C:/Users/isbeihm/Uppsala/argos/Bronze/Embla_data/_MHT/2026/generic/"
#'   ),
#'   group_names = c("lmed", "diagnoses_and_operations", "other")
#' )
#' }
#'
#' @family skeleton_classes
#' @seealso [skeleton_meta()] for creating runtime objects
#' @export
SkeletonConfig <- S7::new_class(
  "SkeletonConfig",
  properties = list(
    rawbatch_dir = S7::class_character,
    skeleton_dir = S7::class_character,
    group_names = S7::class_character,
    batch_sizes = S7::class_integer,
    seed = S7::class_integer,
    id_col = S7::class_character,
    ids_per_skeleton_file = S7::class_integer,
    meta_file = S7::new_property(
      class = S7::class_character,
      getter = function(self) file.path(self@rawbatch_dir, "skeleton_meta.qs")
    )
  ),
  validator = function(self) {
    if (length(self@rawbatch_dir) != 1) {
      return("rawbatch_dir must be length 1 (resolved)")
    }
    if (length(self@skeleton_dir) != 1) {
      return("skeleton_dir must be length 1 (resolved)")
    }
    if (length(self@group_names) == 0) {
      return("group_names cannot be empty")
    }
    if (length(self@batch_sizes) == 0) {
      return("batch_sizes must have at least one element")
    }
    if (any(self@batch_sizes <= 0L)) {
      return("batch_sizes must be positive integers")
    }
    if (length(self@seed) != 1) return("seed must be length 1")
    if (length(self@id_col) != 1) return("id_col must be length 1")
    if (length(self@ids_per_skeleton_file) != 1 ||
        self@ids_per_skeleton_file <= 0L) {
      return("ids_per_skeleton_file must be a single positive integer")
    }
    NULL
  }
)


#' Create a skeleton pipeline configuration
#'
#' Constructor function for [SkeletonConfig] objects. Accepts multiple candidate
#' paths (like [org::initialize_project()]) and resolves each to the first
#' existing directory at runtime.
#'
#' @param rawbatch_dir Character vector of candidate paths for rawbatch files.
#'   The first existing directory is used. If only one path is given and it
#'   doesn't exist, it is accepted as-is (will be created).
#' @param group_names Character vector, names of rawbatch groups.
#' @param skeleton_dir Character vector of candidate paths for skeleton output.
#'   Defaults to the same resolved value as `rawbatch_dir`.
#' @param batch_sizes Integer vector. First element = IDs in batch 1 (dev),
#'   second = IDs in remaining batches. Default: `c(1000L, 10000L)`.
#' @param seed Integer, shuffle seed. Default: `4L`.
#' @param id_col Character, person ID column name. Default: `"lopnr"`.
#' @param ids_per_skeleton_file Integer, IDs per skeleton sub-file.
#'   Default: `1000L`.
#'
#' @return A [SkeletonConfig] object.
#'
#' @examples
#' \dontrun{
#' config <- skeleton_config(
#'   rawbatch_dir = c(
#'     "//argos.rudbeck.uu.se/MyGroups$/Bronze/Embla_data/_MHT/2026/generic/",
#'     "/data/argos/Bronze/Embla_data/_MHT/2026/generic/",
#'     "C:/Users/isbeihm/Uppsala/argos/Bronze/Embla_data/_MHT/2026/generic/"
#'   ),
#'   group_names = c("lmed", "diagnoses_and_operations", "other")
#' )
#' }
#'
#' @family skeleton_classes
#' @seealso [SkeletonConfig] for class details
#' @export
skeleton_config <- function(
    rawbatch_dir,
    group_names,
    skeleton_dir = rawbatch_dir,
    batch_sizes = c(1000L, 10000L),
    seed = 4L,
    id_col = "lopnr",
    ids_per_skeleton_file = 1000L
) {
  resolved_rawbatch <- .resolve_path(rawbatch_dir, "rawbatch_dir")
  resolved_skeleton <- .resolve_path(skeleton_dir, "skeleton_dir")

  SkeletonConfig(
    rawbatch_dir = resolved_rawbatch,
    skeleton_dir = resolved_skeleton,
    group_names = group_names,
    batch_sizes = as.integer(batch_sizes),
    seed = as.integer(seed),
    id_col = id_col,
    ids_per_skeleton_file = as.integer(ids_per_skeleton_file)
  )
}


#' @name print.SkeletonConfig
#' @title Print method for SkeletonConfig
#' @param x A [SkeletonConfig] object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the object.
#' @keywords internal
S7::method(print, SkeletonConfig) <- function(x, ...) {
  cat("<SkeletonConfig>\n")
  cat("  Rawbatch dir:", x@rawbatch_dir, "\n")
  if (x@skeleton_dir != x@rawbatch_dir) {
    cat("  Skeleton dir:", x@skeleton_dir, "\n")
  }
  cat("  Groups:", paste(x@group_names, collapse = ", "), "\n")
  if (length(x@batch_sizes) == 1) {
    cat("  Batch size:", format(x@batch_sizes, big.mark = ","), "IDs\n")
  } else {
    cat(
      "  Batch sizes: first =",
      format(x@batch_sizes[1], big.mark = ","),
      "IDs, rest =",
      format(x@batch_sizes[2], big.mark = ","),
      "IDs\n"
    )
  }
  cat("  ID col:", x@id_col, "\n")
  cat("  Seed:", x@seed, "\n")
  invisible(x)
}


# -----------------------------------------------------------------------------
# SkeletonMeta: Runtime state (mutable via method returns)
# -----------------------------------------------------------------------------

#' SkeletonMeta class for skeleton pipeline
#'
#' Holds a [SkeletonConfig] plus runtime state: batched ID lists and
#' disk file tracking. Analogous to [TTETrial] -- carries data through
#' a method-chaining workflow.
#'
#' @param config A [SkeletonConfig] object.
#' @param n_ids Integer, total number of IDs across all batches.
#' @param n_batches Integer, number of batches.
#' @param batch_id_list List of ID vectors, one per batch.
#' @param groups_saved Character vector of rawbatch group names that exist
#'   on disk.
#' @param skeleton_files Character vector of skeleton output file paths
#'   detected on disk.
#'
#' @family skeleton_classes
#' @seealso [skeleton_meta()] for creating objects,
#'   [skeleton_save_rawbatch()], [skeleton_load_rawbatch()],
#'   [skeleton_process()]
#' @export
SkeletonMeta <- S7::new_class(
  "SkeletonMeta",
  properties = list(
    config = SkeletonConfig,
    n_ids = S7::class_integer,
    n_batches = S7::class_integer,
    batch_id_list = S7::class_list,
    groups_saved = S7::class_character,
    skeleton_files = S7::class_character
  ),
  validator = function(self) {
    if (length(self@n_ids) != 1) return("n_ids must be length 1")
    if (length(self@n_batches) != 1) return("n_batches must be length 1")
    if (length(self@batch_id_list) != self@n_batches) {
      return("batch_id_list length must equal n_batches")
    }
    NULL
  }
)


#' Create a skeleton set from config and IDs
#'
#' Constructor function for [SkeletonMeta] objects. Shuffles IDs using the
#' config seed, splits into batches per `config@batch_sizes`, and scans
#' disk for existing rawbatch and skeleton files.
#'
#' @param config A [SkeletonConfig] object.
#' @param ids Vector of person IDs to split into batches.
#'
#' @return A [SkeletonMeta] object.
#'
#' @examples
#' \dontrun{
#' config <- skeleton_config(
#'   rawbatch_dir = "/data/processed/",
#'   group_names = c("lmed", "diagnoses_and_operations", "other")
#' )
#' ids <- unique(grunduppgifter$lopnr)
#' skel_meta <- skeleton_meta(config, ids)
#' }
#'
#' @family skeleton_classes
#' @seealso [SkeletonConfig], [skeleton_save_rawbatch()]
#' @export
skeleton_meta <- function(config, ids) {
  ids <- unique(ids)
  set.seed(config@seed)
  ids <- sample(ids)

  # Split IDs into batches
  bs <- config@batch_sizes
  first_size <- bs[1]
  rest_size <- if (length(bs) >= 2) bs[2] else bs[1]

  batch_id_list <- list()
  if (length(ids) <= first_size) {
    batch_id_list <- list(ids)
  } else {
    batch_id_list[[1]] <- ids[seq_len(first_size)]
    remaining <- ids[(first_size + 1L):length(ids)]
    # Split remaining into chunks of rest_size
    n_remaining <- length(remaining)
    n_chunks <- ceiling(n_remaining / rest_size)
    for (i in seq_len(n_chunks)) {
      start <- (i - 1L) * rest_size + 1L
      end <- min(i * rest_size, n_remaining)
      batch_id_list[[i + 1L]] <- remaining[start:end]
    }
  }

  # Scan disk for existing rawbatch files
  groups_saved <- .detect_rawbatch_groups(
    config@rawbatch_dir,
    config@group_names,
    length(batch_id_list)
  )

  # Scan disk for existing skeleton files
  skeleton_files <- .detect_skeleton_files(config@skeleton_dir)

  SkeletonMeta(
    config = config,
    n_ids = as.integer(length(ids)),
    n_batches = as.integer(length(batch_id_list)),
    batch_id_list = batch_id_list,
    groups_saved = groups_saved,
    skeleton_files = skeleton_files
  )
}


# Internal: detect which rawbatch groups have all files on disk
.detect_rawbatch_groups <- function(rawbatch_dir, group_names, n_batches) {
  saved <- character(0)
  for (g in group_names) {
    all_exist <- all(vapply(
      seq_len(n_batches),
      function(b) {
        file.exists(file.path(
          rawbatch_dir,
          sprintf("%03d_rawbatch_%s.qs", b, g)
        ))
      },
      logical(1)
    ))
    if (all_exist) saved <- c(saved, g)
  }
  saved
}


# Internal: detect skeleton files on disk
.detect_skeleton_files <- function(skeleton_dir) {
  if (!dir.exists(skeleton_dir)) return(character(0))
  files <- list.files(
    skeleton_dir,
    pattern = "skeleton_\\d+_\\d+\\.qs$",
    full.names = TRUE
  )
  sort(files)
}


# -----------------------------------------------------------------------------
# skeleton_save_rawbatch: Split data by batch IDs and save
# -----------------------------------------------------------------------------

#' Save rawbatch files for one group
#'
#' Splits `data` by batch IDs from the [SkeletonMeta] and saves as
#' `{BBB}_rawbatch_{group}.qs` files. Skips if all files for this group
#' already exist on disk.
#'
#' @param skeleton_meta A [SkeletonMeta] object.
#' @param group Character, group name (must be in `config@group_names`).
#' @param data A single data.table or named list of data.tables.
#'
#' @return The [SkeletonMeta] with updated `groups_saved`.
#'
#' @examples
#' \dontrun{
#' lmed <- data.table::fread("lmed.csv")
#' skel_meta <- skeleton_save_rawbatch(skel_meta, "lmed", lmed)
#' rm(lmed); gc()
#' }
#'
#' @family skeleton_methods
#' @export
skeleton_save_rawbatch <- S7::new_generic(
  "skeleton_save_rawbatch",
  "skeleton_meta"
)

S7::method(skeleton_save_rawbatch, SkeletonMeta) <- function(
    skeleton_meta,
    group,
    data
) {
  config <- skeleton_meta@config

  if (!group %in% config@group_names) {
    stop(
      "group '", group, "' not in config@group_names: ",
      paste(config@group_names, collapse = ", ")
    )
  }

  # Skip if all files for this group already exist
  if (group %in% skeleton_meta@groups_saved) {
    cat("Skipping '", group, "' -- all rawbatch files already exist\n", sep = "")
    return(skeleton_meta)
  }

  id_col <- config@id_col
  n_threads <- parallel::detectCores()

  for (b in seq_along(skeleton_meta@batch_id_list)) {
    batch_ids <- skeleton_meta@batch_id_list[[b]]
    if (data.table::is.data.table(data)) {
      batch_data <- data[get(id_col) %in% batch_ids]
    } else {
      # Named list of data.tables
      batch_data <- lapply(data, function(dt) {
        if (data.table::is.data.table(dt)) {
          dt[get(id_col) %in% batch_ids]
        } else {
          dt
        }
      })
    }
    outfile <- file.path(
      config@rawbatch_dir,
      sprintf("%03d_rawbatch_%s.qs", b, group)
    )
    .qs_save(batch_data, outfile, preset = "balanced", nthreads = n_threads)
    cat(
      "  batch", b, "/", skeleton_meta@n_batches,
      "(", length(batch_ids), "IDs) ->", group, "\n"
    )
  }

  # Update groups_saved
  skeleton_meta@groups_saved <- sort(unique(c(skeleton_meta@groups_saved, group)))
  skeleton_meta
}


# -----------------------------------------------------------------------------
# skeleton_load_rawbatch: Load all group files for one batch
# -----------------------------------------------------------------------------

#' Load rawbatch files for a single batch
#'
#' Reads all group files for one batch and returns a named list. Group files
#' that are themselves named lists (e.g., "other") are flattened into the
#' result. No project-specific exclusions are applied -- the user can filter
#' after loading.
#'
#' @param skeleton_meta A [SkeletonMeta] object.
#' @param batch_number Integer batch number (1-indexed).
#'
#' @return Named list of data.tables (one entry per group, with list-type
#'   groups flattened).
#'
#' @examples
#' \dontrun{
#' batch_data <- skeleton_load_rawbatch(skel_meta, batch_number = 1)
#' # Apply project-specific exclusions
#' batch_data[["lmed"]] <- batch_data[["lmed"]][!produkt %in% excluded]
#' }
#'
#' @family skeleton_methods
#' @export
skeleton_load_rawbatch <- S7::new_generic(
  "skeleton_load_rawbatch",
  "skeleton_meta"
)

S7::method(skeleton_load_rawbatch, SkeletonMeta) <- function(
    skeleton_meta,
    batch_number
) {
  config <- skeleton_meta@config
  if (batch_number < 1 || batch_number > skeleton_meta@n_batches) {
    stop(
      "batch_number must be between 1 and ", skeleton_meta@n_batches,
      " (got ", batch_number, ")"
    )
  }

  n_threads <- max(1L, floor(parallel::detectCores() / 2))
  result <- list()

  for (g in config@group_names) {
    fpath <- file.path(
      config@rawbatch_dir,
      sprintf("%03d_rawbatch_%s.qs", batch_number, g)
    )
    if (!file.exists(fpath)) {
      stop("Rawbatch file missing: ", fpath)
    }
    obj <- .qs_read(fpath, nthreads = n_threads)

    if (is.list(obj) && !data.table::is.data.table(obj)) {
      # Named list of data.tables -> flatten into result
      for (nm in names(obj)) {
        result[[nm]] <- obj[[nm]]
      }
    } else {
      result[[g]] <- obj
    }
  }

  result
}


# -----------------------------------------------------------------------------
# skeleton_process: Iterate over batches with progress + gc
# -----------------------------------------------------------------------------

#' Process batches through a user-defined function
#'
#' For each batch: loads rawbatch data via [skeleton_load_rawbatch()],
#' calls `process_fn`, then runs `gc()`. Wraps in
#' [progressr::with_progress()] for a progress bar.
#'
#' @param skeleton_meta A [SkeletonMeta] object.
#' @param process_fn A function with signature
#'   `function(batch_data, batch_number, config)` where:
#'   - `batch_data`: named list from [skeleton_load_rawbatch()]
#'   - `batch_number`: integer batch index
#'   - `config`: the [SkeletonConfig] object
#'
#'   The function can return anything (profiling data.table, NULL, etc.).
#'   To save skeleton output files, call [skeleton_save()] inside the
#'   function.
#'
#' @param batches Integer vector of batch indices to process, or `NULL`
#'   (default) to process all batches. Useful for test runs, e.g.
#'   `batches = 1:2` to process only the first two batches.
#'
#' @return A list with:
#'   - `skeleton_meta`: updated [SkeletonMeta] with `skeleton_files` re-scanned
#'   - `results`: list of `process_fn` return values (one per batch,
#'     unprocessed slots are `NULL`)
#'
#' @details
#' ## Interactive development pattern
#'
#' Process functions are typically defined in their own R file with
#' `plnr::is_run_directly()` at the top to enable line-by-line development:
#'
#' ```r
#' skeleton_create_mht <- function(batch_data, batch_number, config) {
#'   if (plnr::is_run_directly()) {
#'     skel_meta <- qs::qread(config@meta_file)
#'     batch_number <- 1
#'     batch_data <- skeleton_load_rawbatch(skel_meta, batch_number)
#'     config <- skel_meta@config
#'   }
#'   # ... processing code ...
#' }
#' ```
#'
#' @examples
#' \dontrun{
#' # Process all batches
#' result <- skeleton_process(skel_meta, function(batch_data, batch_number, config) {
#'   batch_data[["lmed"]] <- batch_data[["lmed"]][!produkt %in% excluded]
#'   skeleton_create_mht(batch_data, batch_number, config)
#' })
#' qs::qsave(result$skeleton_meta, config@meta_file)
#'
#' # Test run: process only the first 2 batches
#' result <- skeleton_process(skel_meta, my_process_fn, batches = 1:2)
#' }
#'
#' @family skeleton_methods
#' @export
skeleton_process <- S7::new_generic("skeleton_process", "skeleton_meta")

S7::method(skeleton_process, SkeletonMeta) <- function(
    skeleton_meta,
    process_fn,
    batches = NULL
) {
  if (is.null(batches)) {
    batches <- seq_len(skeleton_meta@n_batches)
  }
  results <- vector("list", length = skeleton_meta@n_batches)

  progressr::with_progress({
    p <- progressr::progressor(steps = length(batches))
    for (i in batches) {
      batch_data <- skeleton_load_rawbatch(skeleton_meta, i)
      results[[i]] <- process_fn(batch_data, i, skeleton_meta@config)
      rm(batch_data)
      gc()
      p()
    }
  })

  # Re-scan skeleton_dir for output files
  skeleton_meta@skeleton_files <- .detect_skeleton_files(
    skeleton_meta@config@skeleton_dir
  )

  list(skeleton_meta = skeleton_meta, results = results)
}


# -----------------------------------------------------------------------------
# skeleton_delete_rawbatches: Remove all rawbatch files
# -----------------------------------------------------------------------------

#' Delete all rawbatch files from disk
#'
#' Removes all `{BBB}_rawbatch_{group}.qs` files from the rawbatch directory.
#'
#' @param skeleton_meta A [SkeletonMeta] object.
#'
#' @return The [SkeletonMeta] with empty `groups_saved`.
#'
#' @family skeleton_methods
#' @export
skeleton_delete_rawbatches <- S7::new_generic(
  "skeleton_delete_rawbatches",
  "skeleton_meta"
)

S7::method(skeleton_delete_rawbatches, SkeletonMeta) <- function(skeleton_meta) {
  config <- skeleton_meta@config
  files <- list.files(
    config@rawbatch_dir,
    pattern = "\\d+_rawbatch_.*\\.qs$",
    full.names = TRUE
  )
  if (length(files) > 0) {
    cat("Deleting", length(files), "rawbatch files\n")
    file.remove(files)
  }
  skeleton_meta@groups_saved <- character(0)
  skeleton_meta
}


# -----------------------------------------------------------------------------
# skeleton_delete_skeletons: Remove all skeleton output files
# -----------------------------------------------------------------------------

#' Delete all skeleton output files from disk
#'
#' Removes all `skeleton_{BBB}_{SS}.qs` files from the skeleton directory.
#'
#' @param skeleton_meta A [SkeletonMeta] object.
#'
#' @return The [SkeletonMeta] with empty `skeleton_files`.
#'
#' @family skeleton_methods
#' @export
skeleton_delete_skeletons <- S7::new_generic(
  "skeleton_delete_skeletons",
  "skeleton_meta"
)

S7::method(skeleton_delete_skeletons, SkeletonMeta) <- function(skeleton_meta) {
  config <- skeleton_meta@config
  files <- list.files(
    config@skeleton_dir,
    pattern = "skeleton_\\d+_\\d+\\.qs$",
    full.names = TRUE
  )
  if (length(files) > 0) {
    cat("Deleting", length(files), "skeleton files\n")
    file.remove(files)
  }
  skeleton_meta@skeleton_files <- character(0)
  skeleton_meta
}


# -----------------------------------------------------------------------------
# print method for SkeletonMeta
# -----------------------------------------------------------------------------

#' @name print.SkeletonMeta
#' @title Print method for SkeletonMeta
#' @param x A [SkeletonMeta] object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the object.
#' @keywords internal
S7::method(print, SkeletonMeta) <- function(x, ...) {
  config <- x@config
  cat("<SkeletonMeta>\n")
  cat("  IDs:", format(x@n_ids, big.mark = ","), "total\n")

  if (length(config@batch_sizes) == 1 || x@n_batches == 1) {
    cat(
      "  Batches:", x@n_batches,
      "(", format(config@batch_sizes[1], big.mark = ","), "IDs)\n"
    )
  } else {
    cat(
      "  Batches:", x@n_batches,
      "(first:", format(config@batch_sizes[1], big.mark = ","),
      "IDs, rest:", format(config@batch_sizes[2], big.mark = ","),
      "IDs)\n"
    )
  }

  # Rawbatch info
  if (length(x@groups_saved) > 0) {
    rb_files <- list.files(
      config@rawbatch_dir,
      pattern = "\\d+_rawbatch_.*\\.qs$",
      full.names = TRUE
    )
    rb_size <- sum(file.size(rb_files), na.rm = TRUE)
    cat(
      "  Rawbatch groups saved:", paste(x@groups_saved, collapse = ", "),
      "(", length(rb_files), "files,",
      .format_bytes(rb_size), ")\n"
    )
  } else {
    cat("  Rawbatch groups saved: (none)\n")
  }

  # Skeleton info
  if (length(x@skeleton_files) > 0) {
    sk_size <- sum(file.size(x@skeleton_files), na.rm = TRUE)
    cat(
      "  Skeleton files:", length(x@skeleton_files), "files",
      "(", .format_bytes(sk_size), ")\n"
    )
  } else {
    cat("  Skeleton files: (none)\n")
  }

  # Total disk usage
  all_files <- c(
    list.files(
      config@rawbatch_dir,
      pattern = "\\d+_rawbatch_.*\\.qs$",
      full.names = TRUE
    ),
    x@skeleton_files
  )
  if (length(all_files) > 0) {
    total <- sum(file.size(all_files), na.rm = TRUE)
    cat("  Total disk usage:", .format_bytes(total), "\n")
  }

  # Dirs
  if (config@rawbatch_dir == config@skeleton_dir) {
    cat("  Dir:", config@rawbatch_dir, "\n")
  } else {
    cat("  Rawbatch dir:", config@rawbatch_dir, "\n")
    cat("  Skeleton dir:", config@skeleton_dir, "\n")
  }

  invisible(x)
}


# Internal: format bytes to human-readable string
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
