#' @import data.table
.onAttach <- function(libname, pkgname) {
    version <- tryCatch(
      utils::packageDescription("swereg", fields = "Version"),
      warning = function(w){
        return(1)
      }
    )

  packageStartupMessage(paste0(
    "swereg ",
    version,
    "\n",
    "https://papadopoulos-lab.github.io/swereg/"
  ))
}
