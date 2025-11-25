#' @title Remove objects if they exist in the environment
#' @description Convenience wrapper around \code{rm}. The function accepts a vector
#'     with named objects and checks if the objects exist before trying removing
#'     them.
#' @param object [\code{character}]\cr
#' Objects that should be removed.
#' @param envir [\code{environment}]\cr
#' The environment from which the objects should be removed. Defaults to the
#'     function's parent environment, i.e. the environment from which the
#'     function was called.
#' @return none, objects are removed from the environment.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' library(NVIdb)
#' xx <- 12345
#' remove_objects("xx")
#'
#' xy <- 123
#' xz <- 456
#' remove_objects(c("xy", "xz", "zz"))
#' }
#'
remove_obects <- function(object, envir = parent.env(environment())) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  ## object
  checkmate::assert_character(object, any.missing = FALSE, all.missing = FALSE, add = checks)
  ## envir
  checkmate::assert_environment(envir, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # REMOVE OBJECTS FROM THE ENVIRONMENT ----
  # Identifies which objects that are in the environment
  objects_to_remove <- intersect(ls(name = envir), object)
  # Removes existing objects from the environment
  rm(list = objects_to_remove, envir = envir)
}
