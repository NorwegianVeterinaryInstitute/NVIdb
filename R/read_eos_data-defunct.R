#' @title Read EOS data from RaData
#' @description Reads EOS data from RaData. Includes historical data if these exists.
#'     It is possible to limit the data to one or more years.
#' @details read_eos_data uses
#'     \ifelse{html}{\code{\link[data.table:fread]{data.table::fread}}}{\code{data.table::fread}}
#'     to read the data with the settings \code{showProgress = FALSE} and
#'     \code{data.table = FALSE}. Other arguments can be passed to
#'     \ifelse{html}{\code{\link[data.table:fread]{data.table::fread}}}{\code{data.table::fread}}
#'     if necessary.
#'
#' The eos_table name is the same name as the name as in the EOS data base.
#'
#' @param from_path [\code{character(1)}]\cr
#'     Path for raw data from eos_data.
#' @param eos_table [\code{character(1)}]\cr
#'     The name of the table with eos raw data.
#' @param year [\code{character} | \code{numeric}]\cr
#'     The years to be included in the result. Can be both numeric
#'     or character. Defaults to \code{NULL}, i.e. no selection.
#' @param colClasses [\code{character}]\cr
#'     The class of the columns, as in
#'     \ifelse{html}{\code{\link[utils:read.table]{utils::read.table}}}{\code{utils::read.table}}.
#'     Defaults to \code{"character"}.
#' @param encoding [\code{character(1)}]\cr
#'     The encoding, one of c("UTF-8", "latin1"). Defaults to \code{"UTF-8"}.
#' @param \dots	Other arguments to be passed to
#'     \ifelse{html}{\code{\link[data.table:fread]{data.table::fread}}}{\code{data.table::fread}}.
#'
#' @return A data frame with data from EOS.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @name read_eos_data-dedefunct
#' @keywords internal
#'
NULL

#' @export
#' @rdname NVIdb-defunct
#' @keywords internal
#'
read_eos_data <- function(...) {

  .Defunct(new = "NVIpjsr::read_eos_data", package = "NVIdb")
  # ,
  #          msg = paste("'set_credentials_EOS' is replaced by 'set_credentials('EOS')'",
  #                      "as the wrapper 'set_credentials_EOS'",
  #                      "was rarely used and therefore considered unnecessary."))

  # # DEPRECATED ----
  # .Deprecated(new = "read_eos_data",
  #             package = "NVIdb",
  #             msg = paste("'read_eos_data' is replaced by
  #                         'NVIpjsr::read_eos_data'"))
  #
  # if (isTRUE(NVIcheckmate::check_package(x = "NVIpjsr", type = "installed"))) {
  #   select_statement <- NVIpjsr::read_eos_data(eos_table = eos_table,
  #                                              from_path = from_path,
  #                                              year = year,
  #                                              colClasses = colClasses,
  #                                              encoding = encoding,
  #                                              ...)
  #
  #   return(select_statement)
  # } else {
  #   # PREPARE ARGUMENTS BEFORE ARGUMENT CHECKING
  #   # Removing ending "/" and "\\" from pathnames
  #   from_path <- sub("/+$|\\\\+$", "", from_path)
  #   # Change any character year to numeric
  #   if (!is.null(year)) {year <- as.numeric(year)}
  #
  #   # ARGUMENT CHECKING ----
  #   # Object to store check-results
  #   checks <- checkmate::makeAssertCollection()
  #   # Perform checks
  #   ## eos_table
  #   checkmate::assert_string(eos_table, min.chars = 1, add = checks)
  #   ## from_path / filename
  #   checkmate::assert_file_exists(file.path(from_path, paste0(eos_table, ".csv")), access = "r", add = checks)
  #   ## year
  #   checkmate::assert_integerish(year,
  #                                lower = 1995,
  #                                upper = as.numeric(format(Sys.Date(), "%Y")),
  #                                any.missing = FALSE,
  #                                all.missing = FALSE,
  #                                unique = TRUE,
  #                                null.ok = TRUE,
  #                                add = checks)
  #   ## colClasses
  #   checkmate::assert_character(colClasses, min.chars = 1, min.len = 1,
  #                               any.missing = FALSE,
  #                               all.missing = FALSE,
  #                               null.ok = TRUE,
  #                               add = checks)
  #
  #   ## encoding
  #   checkmate::assert_subset(encoding, choices = c("UTF-8", "latin1"), add = checks)
  #
  #   # Report check-results
  #   checkmate::reportAssertions(checks)
  #
  #
  #   # Import of data from csv-files retrieved from EOS
  #   # EOS inneholder data fra siste og foregående år og antas å være i kontinuerlig endring
  #   eos_data <- data.table::fread(file = file.path(from_path, paste0(eos_table, ".csv")),
  #                                 colClasses = colClasses,
  #                                 encoding = encoding,
  #                                 showProgress = FALSE,
  #                                 data.table = FALSE,
  #                                 ...)
  #
  #   column_names <- colnames(eos_data)
  #   colnames(eos_data) <- tolower(column_names)
  #
  #   # Import av historiske data fra EOS
  #   # EOS-historikkfiler oppdateres 1 x årlig. Data hentes ut etter oppdatering og lagres i csv-filer
  #   # Hentes derfor fra csv-filen for bruk i OK-statistikken
  #   if (file.exists(file.path(from_path, paste0(eos_table, "_historikk.csv")))) {
  #     eos_data_historikk <- data.table::fread(file = file.path(from_path, paste0(eos_table, "_historikk.csv")),
  #                                             colClasses = colClasses,
  #                                             encoding = encoding,
  #                                             showProgress = FALSE,
  #                                             data.table = FALSE,
  #                                             ...)
  #
  #     # Fjerner år fa historikktabellen som også ligger i driftstabellen. Setter deretter sammen tabellene
  #     first_year_in_eos_data <- min(substr(eos_data$saksnr, 1, 4))
  #     colnames(eos_data_historikk) <- tolower(column_names)
  #     eos_data_historikk <- subset(eos_data_historikk, substr(eos_data_historikk$saksnr, 1, 4) < first_year_in_eos_data)
  #     eos_data <- rbind(eos_data, eos_data_historikk)
  #   }
  #
  #   if (!is.null(year)) {
  #     eos_data <- subset(eos_data, substr(eos_data$saksnr, 1, 4) %in% year)
  #   }
  #
  #   colnames(eos_data) <- column_names
  #   return(eos_data)
  # }
}
#
