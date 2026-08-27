# What a RegistryStudy already has on disk, read from the directories themselves.

# Detect rawbatch groups on disk
.detect_rawbatch_groups <- function(rawbatch_dir, group_names, n_batches) {
  saved <- character(0)
  for (g in group_names) {
    all_exist <- all(vapply(
      seq_len(n_batches),
      function(b) {
        return(file.exists(file.path(
          rawbatch_dir,
          sprintf("%05d_rawbatch_%s.qs2", b, g)
        )))
      },
      logical(1)
    ))
    if (all_exist) saved <- c(saved, g)
  }
  return(saved)
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
  return(sort(files))
}
