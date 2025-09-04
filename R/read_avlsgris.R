#' @title Read Register with avlsgris herds
#' @description Functions to read the register with avlsgris herds, i.e. 
#'     swine nucleus and multiplier herds. 
#' @details The avlsgris-register includes information on the avlsgris herds.
#'     The register is updated from the industry at least once a year.
#' \code{read_avlsris} reads the avlsgris-register into a \code{data.frame}. The 
#'     default is to read the last updated version of the register. You may input 
#'     wanted date or year and month to read the register nearest 
#'     (either before or after) the wanted date. See help for 
#'     \code{\link{find_file_near_date}} for a full description of the use of 
#'     wanted date or year and month.
#'
#' @param from_path [\code{character(1)}]\cr
#' Full path for the directory with the avlgris files. Defaults to standard 
#'     directory at NVI's network.
#' @param wanted_date [\code{character}(1)]\cr
#' The date for which the register date should be nearest. Defaults to 
#'     \code{Sys.Date()}.
#' @param wanted_year [\code{character}(1)] | [\code{numeric}(1)]\cr
#' The year from which the register should be read, see details. Defaults to 
#'     \code{NULL}.
#' @param wanted_month [\code{character}]\cr
#' The month for which the register should be read, see details. Defaults to
#'     \code{NULL}.
#' @param nearest_wanted [\code{character(1)}]\cr
#' Whether the nearest register date before the wanted date or the nearest date 
#'     after the wanted date should be selected, see details. Must be one of 
#'     \code{c("before", "after")}. Defaults to \code{"before"}.

#' @param \dots	Other arguments to be passed to
#'     \ifelse{html}{\code{\link[utils:csv2]{utils::csv2}}}{\code{utils::csv2}}.
#'
#' @return A \code{data.frame} with the avlsgris-register.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading the last version from the standard directory at NVI's network
#' avlsgris <- read_avlsgris()
#'
#' # Reading from standard directory at NVI's network and
#' #     selecting a specific version of the register
#' avlsgris2021 <- read_avlsgris(wanted_year = 2021, wanted_month = "12")
#' }
#'
read_avlsgris <- function(from_path = file.path(set_dir_NVI("EksterneDatakilder", slash = FALSE),
                                                "Avlsgris", "FormaterteData"),
                          wanted_date = Sys.Date(),
                          wanted_year = NULL,
                          wanted_month = NULL,
                          nearest_wanted = "before",
                          ...) {
  
  # PREPARE ARGUMENT ----
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  # from_path
  checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  # date
  checkmate::assert_date(wanted_date, 
                         null.ok = TRUE, 
                         len = 1,
                         lower = as.Date("1994-01-01"), upper = Sys.Date(),
                         add = checks)
  # year
  checkmate::assert_integerish(wanted_year,
                               null.ok = TRUE,
                               len = 1,
                               lower = 1994, upper = as.numeric(format(Sys.Date(), "%Y")),
                               all.missing = FALSE, any.missing = FALSE,
                               add = checks)
  # month
  checkmate::assert_character(wanted_month, 
                              len = 1,
                              null.ok = TRUE,
                              add = checks)
  checkmate::assert_subset(wanted_month, 
                           choices = c("01", "02", "03", "04", "05", "06",
                                       "07", "08", "09", "10", "11", "12"), 
                           empty.ok = TRUE,
                           add = checks)
  NVIcheckmate::assert(NVIcheckmate::check_non_null(list(wanted_date, wanted_year)),
                       NVIcheckmate::check_non_null(list(wanted_date, wanted_month)),
                       combine = "and",
                       comment = "You must input either the date or the year and month.",
                       add = checks)
  # nearest_wanted
  checkmate::assert_subset(nearest_wanted, 
                           choices = c("after", "before"), 
                           add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
    # READ IN ALL FILES IN THE DIRECTORY AND MAKE A DATA FRAME OF THE SELECTED FILE NAMES
  # Read data for the selected year and months
  filelist <- find_file_near_file(from_path = from_path,
                                  partial_filename = c("avlsgris", "per_"),
                                  extension = "csv",
                                  wanted_date = wanted_date,
                                  wanted_year = wanted_year,
                                  wanted_month = wanted_month,
                                  nearest_wanted = nearest_wanted) 
  
  # READ DATA FROM THE SELECTED FILE
  # Read the colclasses
  colclasses <- standardize_columns(file.path(from_path, filelist[1, "filename"]),
                                    dbsource = "avlsgris",
                                    property = "colclasses")
  
  # Read data
  df1 <- utils::read.csv2(file = file.path(from_path, filelist[1, "filename"]),
                          colClasses = colclasses,
                          ...)
  
  # Return data frame with data
  return(df1)
}



