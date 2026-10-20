# Portable forms of three assertions that differ on Windows. There, a
# text-mode connection writes "\r\n" for each "\n", `dirname()` writes every
# separator as "/", and an absolute path starts with a drive letter. On any
# other platform, each helper gives the assertion it replaces.

# The md5 of `x`, written one element per line, each line ending in "\n".
# The binary connection writes the same bytes on every platform.
md5_of_lines <- function(x) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  con <- file(tmp, open = "wb")
  tryCatch(writeLines(x, con, useBytes = TRUE), finally = close(con))
  return(unname(tools::md5sum(tmp)))
}

# The regular expression for the start of an absolute path on `os`. It is
# "/" on Unix, and a drive letter with a separator on Windows. It follows
# batchit:::.batch_is_absolute_path(), which refuses a relative declared
# output. Anchor it with "^" before use.
abs_path_start <- function(os = .Platform$OS.type) {
  if (identical(os, "windows")) {
    return("[A-Za-z]:[/\\\\]")
  }
  return("/")
}

# `path` with its separators as `dirname()` writes them on `os`. On Windows
# that is "/" for every separator. On Unix it is `path` unchanged.
dirname_form <- function(path, os = .Platform$OS.type) {
  if (identical(os, "windows")) {
    return(chartr("\\", "/", path))
  }
  return(path)
}
