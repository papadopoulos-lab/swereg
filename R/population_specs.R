# Population by-specs: their keys, their validation, and their per-batch aggregation.

# Canonical, deterministic key for a population by-spec. Sorted so
# `c("a", "b")` and `c("b", "a")` collapse to the same entry.
.population_spec_key <- function(spec) {
  return(paste(sort(spec), collapse = "+"))
}

# Filesystem-safe variant of the spec key for use in `population_*.qs2`
# filenames. `+` is technically legal on POSIX but we replace it with
# `__` to keep paths boring.
.population_spec_filename_key <- function(spec) {
  return(gsub("\\+", "__", .population_spec_key(spec), fixed = FALSE))
}

# Validate the constructor's `population_by_specs` argument. Returns a
# normalised list (drops names, casts each element to character).
.validate_population_by_specs <- function(specs) {
  if (is.null(specs)) {
    return(list())
  }
  if (!is.list(specs)) {
    stop(
      "population_by_specs must be a list of character vectors",
      call. = FALSE
    )
  }
  for (i in seq_along(specs)) {
    s <- specs[[i]]
    if (
      !is.character(s) || length(s) == 0L || any(is.na(s)) || any(!nzchar(s))
    ) {
      stop(
        "population_by_specs[[",
        i,
        "]] must be a non-empty character vector with no NA / empty entries",
        call. = FALSE
      )
    }
  }
  # Deduplicate by canonical key
  keys <- vapply(specs, .population_spec_key, character(1))
  return(unname(specs[!duplicated(keys)]))
}

# Compute one batch's per-spec aggregation. Each spec produces a small
# data.table keyed by `isoyear + spec`, with column `n` = unique-person
# count. Errors if any spec column is missing from the skeleton.
.compute_population_aggregations <- function(skeleton_dt, specs) {
  if (length(specs) == 0L) {
    return(list())
  }
  out <- list()
  for (spec in specs) {
    cols_needed <- unique(c("id", "isoyear", spec))
    missing <- setdiff(cols_needed, names(skeleton_dt))
    if (length(missing) > 0L) {
      stop(
        "Skeleton is missing columns required by population spec ",
        .population_spec_key(spec),
        ": ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    sub <- unique(skeleton_dt[, cols_needed, with = FALSE])
    agg <- sub[, .(n = .N), by = c("isoyear", spec)]
    out[[.population_spec_key(spec)]] <- agg
  }
  return(out)
}
