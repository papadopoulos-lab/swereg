#' @import data.table
.onAttach <- function(libname, pkgname) {
    version <- tryCatch(
      utils::packageDescription("swereg", fields = "Version"),
      warning = function(w){
        1
      }
    )

  packageStartupMessage(paste0(
    "swereg ",
    version,
    "\n",
    "https://github.com/papadopoulos-lab/swereg/"
  ))
}
