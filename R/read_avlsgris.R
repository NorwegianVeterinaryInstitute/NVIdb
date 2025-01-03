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
  # Pkode_month
  checkmate::assert_subset(month, choices = c("01", "02", "03", "04", "05", "06",
                                              "07", "08", "09", "10", "11", "12"), add = checks)
  # Pkode_year
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
  filelist <- select_file_for_date(from_path = from_path,
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

###   ----

### select_prodtilskudd_files ----


#' @title Selects files based on date in file name
#' @description List file names that are selected based on the date information
#'     in the file name.
#' @details Reads the file names of files with the register date version in the
#'     file name. The routine assumes that the first number in the file name
#'     that can be a year, is the year (\%Y), year_month (\%Y\%m) or
#'     year_month_day (\%Y\%m\%d) for the register date. The register date
#'     is extracted from the file name and the file name of correct source
#'     file(s) are thereafter selected.
#'
#'     The function is called from \code{read_avlsgris}.
#'
#' @param from_path [\code{character(1)}]\cr
#'     Path for the files.
#' @param filename_text [\code{character}]\cr
#' @param file_extension [\code{character}]\cr
#' @param year [\code{character}] | [\code{numeric}]\cr
#' The year(s) from which the register should be read. Options is "last", or a
#'     vector with one or more years.
#' @param month [\code{character}]\cr
#'     The month for which the register should be read, see details. Defaults to
#'     \code{NULL}.
#' @param day [\code{character}]\cr
#' @param match_date [\code{character(1)}]\cr
#' @param extracted_date [\code{character(1)}]\cr
#'
#' @return A data frame with file names of the files that should be selected.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @examples
#' \dontrun{
#' # Making the filelist for read_Prodtilskudd or copy_Prodtilskudd
#' filelist <- select_prodtilskudd_files(from_path = from_path,
#'                                       Pkode_year = Pkode_year,
#'                                       Pkode_month = Pkode_month)
#' }
#' @keywords internal

select_file_for_date <- function(from_path,
                                  filename_text,
                                  file_extension,
                                  year,
                                  month = "12",
                                  day = NULL,
                                  match_date = "last_before",
                                  extracted_date = NULL) {

  # READ ALL FILES IN DIRECTORY WITH A FILENAME IN ACCORD WITH filename_text AND file_extension
  # Read filelist
  filelist <- list.files(path = from_path,
                         pattern = filename_text[1],
                         ignore.case = TRUE,
                         include.dirs = FALSE)
  # Select if more criteria than one in filename
  if (length(filename_text) > 1) {
    for (text in filename_text[2:length(filename_text)]) {
      filelist <- filelist[grepl(pattern = text, x = filelist, ignore.case = TRUE)]
    }
  }
  filelist <- data.frame("filename" = filelist)
  # Select extension
  filelist$extension <- tools::file_ext(filelist$filename)
  filelist <- subset(filelist, filelist$extension %in% file_extension)

  # IDENTIFY YEAR, MONTH AND DATE IN FILENAME
  filelist$position <- regexpr(pattern = "[_[:space:]]20", filelist[, "filename"])
  filelist$date <- as.Date(substr(filelist$filename, filelist$position + 1, filelist$position + 9), "%Y%m%d")
  filelist$year <- format(filelist$date, "%Y")
  filelist$month <- format(filelist$date, "%m")

# SORT DATA FROM LATEST TO FIRST
  filelist <- filelist[order(filelist$year, filelist$month, filelist$date, decreasing = TRUE), ]

  if (is.null(extracted_date)) {
    # filelist <- subset(filelist, filelist$uttrekk_dato == filelist$x)
    if ("last" %in% year) {
      filelist <- utils::head(filelist, 1)
      }
    if (!"last" %in% year) {
      filelist[which(filelist$year <= year), "match_year"] <- "LE"
      filelist[which(filelist$month <= month), "match_month"] <- "LE"
      filelist <- subset(filelist, filelist$match_year == "LE" & filelist$match_month == "LE")
      filelist <- utils::head(filelist, 1)
    }
  }

  return(filelist)
}
