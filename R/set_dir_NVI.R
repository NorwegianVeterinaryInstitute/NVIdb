#' @title Set directories for data sources at NVI
#' @description Set the directories for various data sources at NVI's network.
#' @details The Norwegian Veterinary Institute has standard data sources at fixed
#'     directories. The function returns the standard directory for the given
#'     data source. Thereby hard coding of the paths may be avoided.
#'
#' The path ends with a slash as default. To facilitate the use of
#'     \ifelse{html}{\code{\link[base:file.path]{file.path}}}{\code{file.path}}
#'     you can use the argument \code{slash = FALSE} to avoid ending slash.
#'
#' @param datasource [\code{character(1)}]\cr
#'      The data source that one want to access. The input can be abbreviated
#'      and case is ignored. To identify short names for the available
#'      directories, use \code{set_dir_NVI(datasource = "?")}.
#' @param slash [\code{logical(1)}]\cr
#'      If \code{TRUE} the path ends with a slash. Defaults to \code{TRUE}.
#'
#' @return The full path for the directory at NVI's network. The path ends with
#'      "/" as default, see details.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @export
#' @examples
#' \dontrun{
#' # Set path_ProdTilskudd to path for Prodtilskudd at the NVI network
#' prodtilskudd_path <- set_dir_NVI(datasource = "ProdTilskudd")
#'
#' # Set pathname to a file using 'file.path'
#' pathname <- file.path(set_dir_NVI(datasource = "ProdTilskudd", slash = FALSE),
#'                       "subdir",
#'                       "filename")
#' }
#'
set_dir_NVI <- function(datasource,
                        slash = TRUE) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  datasource <- NVIcheckmate::match_arg(x = datasource,
                                        choices = names(NVIconfig:::path_NVI),
                                        several.ok = FALSE,
                                        ignore.case = TRUE,
                                        add = checks)
  # slash
  checkmate::assert_flag(x = slash, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # SETTING THE PATH ----
  # The paths are defined in the package NVIconfig
  pathname <- unname(NVIconfig:::path_NVI[datasource])

  # To ensure that old versions of NVIconfig will still work
  if (!isTRUE(NVIcheckmate::check_package("NVIconfig", version = "0.9.0"))) {
    pathname <- sub("//vetinst.no/dfs-felles/", "", pathname)
  }

  # Path to StasjonK if on Windows
  if (Sys.info()["sysname"] == "Windows") {
    pathname <- file.path("//vetinst.no/dfs-felles", pathname)
  # Path to StasjonK if on posit
  } else {
    pathname <- file.path(Sys.getenv("HOME"), pathname)
  }
  # Remove slash
  if (isFALSE(slash)) {
    pathname <- cut_slash(pathname)
  }
  return(pathname)
}
