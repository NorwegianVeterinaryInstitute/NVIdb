# Give a startup message if NVIconfig less than required version is not installed


#' @title Check if an argument is an installed package of required version
#' @description checks if an package is installed and if it is installed, check if the version is
#'     equal or higher than the required version.
#' @details This function will eventually be included in NVIcheckmate.
#'
#' @param x Object to check.
#' @param version The required version of the installed package. May be \code{NULL}.
#' @return If package of required version is installed, then TRUE, else an error message
#' @noRd


# Do after loading NVIdb
.onAttach <- function(libname, pkgname) {

  # check if "NVIconfig" is installed
  msg <- NVIcheckmate::check_package(x = "NVIconfig", version = "0.9.0")

  # Print a startup message if not required version is installed
  if (!isTRUE(msg)) {
    msg <- paste(msg,
                 "On windows you can install 'NVIconfig' by using:",
                 "  remotes::install_github('NorwegianVeterinaryInstitute/NVIconfig',",
                 "                          auth_token = 'your_GitHub-PAT',",
                 "                          upgrade = FALSE,",
                 "                          build = TRUE,",
                 "                          build_vignettes = TRUE)",
                 sep = "\n")
    packageStartupMessage(msg)
  }

  invisible()
}
