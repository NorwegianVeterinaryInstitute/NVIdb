#' @title Read Register with avlsgrisbesetninger
#' @description Functions to read versions of the
#'     avlsgris-register.
#' @details The avlsgris-register includes information on the avlsgris herds.
#'     The register is updated from the industry at least once a year. This
#'     function automatically selects the last updated
#'     version of the register.
#'
#'     \code{read_avlsris} reads the avlsgris-register into a
#'     data frame. The function gives options to select year and month. If there
#'     are no available version from the selected month, the last available
#'     version before the chosen month, will be selected. The
#'     standard settings will read in the files from NVI's internal network and
#'     select the latest updated file.
#'
#' @param from_path [\code{character(1)}]\cr
#'     Path for the produksjonstilskuddsregister. Defaults to the standard
#'     directory at the NVI network.
#' @param year [\code{character(1)}] | [\code{numeric(1)}]\cr
#'     The year from which the register should be read. Options is "last", or
#'     a year. Defaults to "last".
#' @param month [\code{character(1)}]\cr
#'     The month for which the register should be read. Defaults to "12".
#' @param \dots	Other arguments to be passed to
#'     \ifelse{html}{\code{\link[data.table:fread]{data.table::fread}}}{\code{data.table::fread}}.
#'
#' @return A \code{data.frame} with the avlsgris-register.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' avlsgris <- read_avlsgris()
#'
#' # Reading from standard directory at NVI's network and
#' #     selecting a specific version of the register
#' avlsgris2021 <- read_avlsgris(year = 2021, Pkode_month = "03")
#' }
#'
read_avlsgris <- function(from_path = file.path(set_dir_NVI("EksterneDatakilder", slash = FALSE),
                                                "Avlsgris", "FormaterteData"),
                          year = "last",
                          month = "12",
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
  # month
  checkmate::assert_subset(month, choices = c("01", "02", "03", "04", "05", "06",
                                              "07", "08", "09", "10", "11", "12"), add = checks)
  # year
  checkmate::assert(checkmate::check_integerish(as.numeric(year[grep('[[:alpha:]]', year, invert = TRUE)]),
                                                len = 1,
                                                lower = 2020,
                                                upper = as.numeric(format(Sys.Date(), "%Y")),
                                                any.missing = FALSE,
                                                all.missing = FALSE,
                                                unique = TRUE),
                    checkmate::check_choice(year, choices = c("last")),
                    add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
  
  
  # READ IN ALL FILES IN THE DIRECTORY AND MAKE A DATA FRAME OF THE SELECTED FILE NAMES
  # Read data for the selected year and months
  filelist <- select_files(from_path = from_path,
                           filename_text = c("avlsgris"),
                           file_extension = "csv",
                           year = as.character(year),
                           month = month,
                           extracted_date = NULL)
  
  # Check if any version of the register was found and give an ERROR if not
  NVIcheckmate::assert_data_frame(filelist, min.rows = 1,
                                  comment = paste("No versions of 'avlsgris' available for year",
                                                  year,
                                                  "and month",
                                                  month))
  # READ DATA FROM THE SELECTED FILE
  # Read the colclasses
  colclasses <- standardize_columns(file.path(from_path, filelist[1, "filename"]),
                                    dbsource = "avlsgris",
                                    property = "colclasses")
  
  # Read data
  df1 <- data.table::fread(file = file.path(from_path, filelist[1, "filename"]),
                           colClasses = colclasses,
                           encoding = "UTF-8",
                           showProgress = FALSE,
                           data.table = FALSE,
                           ...)
  
  # Return data frame with data
  return(df1)
}



