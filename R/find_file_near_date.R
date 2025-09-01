#' @title Finds a file based on date in file name
#' @description Identifies the file name of a file based on date information in 
#'     the file name. This is usually used to identify the file name of a file 
#'     with data that has a register date nearest the wanted date. The register
#'     date must be included in the file name and written as _per_DATE, see 
#'     details.
#' @details Reads the file names of files in a given directory and selects the 
#'     file nearest a wanted date based on date information in the file name. 
#'     The register date is used to identify the file name of the data that either 
#'     has the last register date before the wanted date (the default) or the 
#'     first register date after the wanted date. 
#' The function assumes that the register date for the data is written in the 
#'     file name including "_per_DATE" where DATE is given either as a date 
#'     (formatted as \%Y\%m\%d), a year_month (formatted as \%Y\%m) or a year 
#'     (formatted as \%Y). Usually "per_DATE" will be just before the file 
#'     extension.
#' If one want the last register date before the wanted date and the 
#'     "per_DATE" is given as year, the file will be identified as before the 
#'     wanted date for any wanted date within the register year. Likewise, If 
#'     one want the last register date after the wanted date and the 
#'     "per_DATE" is given as year, the file will be identified as after the 
#'     wanted date for any wanted date within the register year. I.e. for the 
#'     file name "data_per_2025.csv" this file name will be found for the dates 
#'     c("2025-01-01", "2025-12-31") and all between for both 
#'     nearest_to_per_date = "before" and nearest_to_per_date = "after". This 
#'     assumes that there are no other files for that year with a more precise 
#'     register date. 
#' If one want the last register date before the wanted date and the 
#'     "per_DATE" is given as year_month, the file will be identified as before 
#'     the wanted date for any wanted date within the register month. Likewise, 
#'     If one want the last register date after the wanted date and the 
#'     "per_DATE" is given as year_month, the file will be identified as after the 
#'     wanted date for any wanted date within the register month. I.e. for the 
#'     file name "data_per_202503.csv" this file name will be found for the dates 
#'     c("2025-03-01", "2025-03-31") and all between for both 
#'     nearest_to_per_date = "before" and nearest_to_per_date = "after". This 
#'     assumes that there are no other files for that year_month with a more 
#'     precise register date. 
#'
#' @param from_path [\code{character(1)}]\cr
#' Full path for the directory with the files. Defaults to \code{NULL}.
#' @param partial_filename [\code{character}]\cr
#' One or more patterns within the file name that will be used to identify a 
#'    subset of files in the directory that can be selected. Defaults to 
#'    \code{NULL}.
#' @param extension [\code{character}(1)]\cr
#' The file extension of the files that can be selected. Defaults to \code{NULL}.
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
#'
#' @return A data frame with the file name of the file that fulfills the 
#'     selection criteria.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Identify the filename of the last Purkering register before November 2023
#' filelist <- find_file_near_date(
#'   from_path = file.path(NVIdb::set_dir_NVI("Ekst", slash = FALSE), 
#'   "Purkeringer", "FormaterteData"),
#'   partial_filename = c("Purkering", "per"),
#'   extension = "csv",
#'   wanted_date = NULL,
#'   wanted_year = 2023,
#'   wanted_month = "11",
#'   nearest_wanted = "before") 
#' }

find_file_near_date <- function(from_path,
                                partial_filename = NULL,
                                extension = NULL,
                                wanted_date = Sys.Date(),
                                wanted_year = NULL,
                                wanted_month = NULL,
                                nearest_wanted = "before") {
  
  # READ ALL FILES IN DIRECTORY WITH A FILENAME IN ACCORD WITH file name_text AND file_extension
  # Read file list
  filelist <- list.files(path = from_path,
                         pattern = partial_filename[1],
                         ignore.case = TRUE,
                         include.dirs = FALSE)
  # Select if more criteria than one in filename
  if (length(partial_filename) > 1) {
    for (text in partial_filename[2:length(partial_filename)]) {
      filelist <- filelist[grepl(pattern = text, x = filelist, ignore.case = TRUE)]
    }
  }
  filelist <- data.frame("filename" = filelist)
  # Select extension
  filelist$file_extension <- tools::file_ext(filelist$filename)
  filelist <- subset(filelist, filelist$file_extension %in% extension)
  
  # IDENTIFY YEAR, MONTH AND DATE IN FILENAME
  filelist$position <- regexpr(pattern = "per[_[:space:]][12]", filelist[, "filename"])
  rownr <- grepl(pattern = "per[_[:space:]][12][[:digit:]]{3}", filelist[, "filename"])
  filelist[rownr, "per_year"] <- substr(filelist[rownr, "filename"], 
                                        filelist[rownr, "position"] + 4, 
                                        filelist[rownr, "position"] + 7)
  rownr <- grepl(pattern = "per[_[:space:]][12][[:digit:]]{5}", filelist[, "filename"])
  filelist[rownr, "per_month"] <- substr(filelist[rownr, "filename"], 
                                         filelist[rownr, "position"] + 8, 
                                         filelist[rownr, "position"] + 9)
  rownr <- grepl(pattern = "per[_[:space:]][12][[:digit:]]{7}", filelist[, "filename"])
  filelist[rownr, "per_day"] <- substr(filelist[rownr, "filename"], 
                                       filelist[rownr, "position"] + 10, 
                                       filelist[rownr, "position"] + 11)
  
  # Generates month and day if missing in file name. 
  # If nearest is after, day is set to first in the month and month is set to January when missing
  # If nearest is before, day is set to last in the month and month is set to December when missing
  if (nearest_wanted == "before") {
    filelist[is.na(filelist$per_month), "per_month"] <- "01"
    filelist[is.na(filelist$per_day), "per_day"] <- "01"
  }
  if (nearest_wanted == "after") {
    filelist[is.na(filelist$per_month), "per_month"] <- "12"
    rownr <- which(is.na(filelist$per_day))
    filelist[rownr, "per_day"] <- sapply(as.Date(paste(filelist[rownr, "per_year"],
                                                       filelist[rownr, "per_month"],
                                                       rep("01", length(rownr)), sep = "-")), 
                                         FUN = last_date_in_month,
                                         output = "day")
  }
  filelist$per_date <- as.Date(paste(filelist$per_year, filelist$per_month, filelist$per_day, sep = "-"))
  
  # TRANSFORM INPUT TO wanted_date ----
  if (is.null(wanted_date)) {
    wanted_date <- as.Date(paste(wanted_year, wanted_month, "01", sep = "-"))
    if (nearest_wanted == "before") {
      wanted_date <- as.Date(last_date_in_month(wanted-date))
    }
  }
  
  # FIND BEST MATCH ----
  if (nearest_wanted == "before") {
    filelist <- subset(filelist, filelist$per_date <= wanted_date)
    NVIcheckmate::assert_data_frame(
      filelist, 
      min.rows = 1,
      comment = paste0("No versions of the register fulfilling the selection ",
                       "criteria is available with date before the wanted date: ",
                       wanted_date,
                       "."))
    filelist <- filelist[order(filelist$per_date, decreasing = TRUE), ]
    filelist <- utils::head(filelist, 1)
  }
  
  if (nearest_wanted == "after") {
    filelist <- subset(filelist, filelist$per_date >= wanted_date)
    NVIcheckmate::assert_data_frame(
      filelist, 
      min.rows = 1,
      comment = paste0("No versions of the register fulfilling the selection ",
                       "criteria is available with date after the wanted date: ",
                       wanted_date,
                       "."))
    filelist <- filelist[order(filelist$per_date), ]
    filelist <- utils::head(filelist, 1)
  }
  
  # RETURN ----
  return(filelist)
}


#' @title Identifies the last date in a month
#' @description Identifies the last date or the last day for a given month 
#'     within a year. 
#' @details The function is restricted to the period from year 1700 to year
#'     2099. One must either input a date or the month and year.
#' One can chose if the output should be the last date within the month or the 
#'     last day number within the month.
#' @param date [\code{date}(1)]\cr
#'     The date for which the last date should be identified. Defaults to 
#'     \code{NULL}.
#' @param year [\code{numeric(1)} | \code{character(1)}]\cr
#'     The year of the month for which the last date should be identified. 
#'     Defaults to \code{NULL}.
#' @param month [\code{character(1)}]\cr
#'     The month within the year for which the last date should be 
#'     identified. Defaults to \code{NULL}.
#' @param output [\code{character(1)}]\cr
#'     Should the output be the last date or the last day of the month. Must be 
#'     one of c("date", "day"). Defaults to \code{"date"}.
#' 
#' @return The last date or the last day of the month depending on value for 
#'     output.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @examples
#' \dontrun {
#' last_date_in_month(date = as.Date("2024-01-24"), output = "date")
#' last_date_in_month(as.Date("2024-01-24"), output = "day")
#' last_date_in_month(year = "2024", month = "02", output = "date")
#' last_date_in_month(year = "2024", month = "02", output = "day")
#' last_date_in_month(as.Date("2025-02-01"), output = "date")
#' last_date_in_month(as.Date("2025-02-28"), output = "day")
#' last_date_in_month(year = "2025", month = "08", output = "date")
#' last_date_in_month(year = "2025", month = "08", output = "day")
#' }
#' @keywords internal
#' 
last_date_in_month <- function(date = NULL, year = NULL, month = NULL, output = "date") {
  
  # PREPARE INPUT BEFORE ARGUMENT CHECKING
  if (exists("year") && !all(is.null(year)) && !all(is.na(year))) {
    year <- as.numeric(year)
  }
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  # date
  checkmate::assert_date(date, 
                         null.ok = TRUE, 
                         len = 1,
                         add = checks)
  # year
  checkmate::assert_integerish(year,
                               null.ok = TRUE,
                               len = 1,
                               lower = 1700, upper = 2099,
                               all.missing = FALSE, any.missing = FALSE,
                               add = checks)
  # month
  checkmate::assert_character(month, 
                              len = 1,
                              null.ok = TRUE,
                              add = checks)
  checkmate::assert_subset(month, 
                           choices = c("01", "02", "03", "04", "05", "06",
                                       "07", "08", "09", "10", "11", "12"), 
                           empty.ok = TRUE,
                           add = checks)
  
  NVIcheckmate::assert(NVIcheckmate::check_non_null(list(date, year)),
                       NVIcheckmate::check_non_null(list(date, month)),
                       combine = "and",
                       comment = "You must input either the date or the year and month.",
                       add = checks)
  # output
  checkmate::assert_subset(output, choices = c("date", "day"), add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
  # IDENTIFY LAST MONTH ----
  # If no date input, the input year and month are transformed to date
  if (!exists("date") || is.null(date) || is.na(date)) {
    date <- as.Date(paste(as.character(year), month, "01", sep = "-"))
  }
  # Creates a vector starting at the input date
  days <- seq(from = date, length.out = 31, by = "day")
  # Selects the highest date within the input month
  days <- days[max(which(format(days, "%m") == format(date, "%m")))]
  # Output the day-part of the date
  if (output == "day") {days <- format(days, "%d")}
  # Transforms the date to character as the date is given as non-formatted number if used in sapply
  if (output == "date") {days <- as.character(days)}
  # RETURN RESULT ----
  return(days)
}
