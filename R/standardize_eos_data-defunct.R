#' @title Standardising EOS-data
#' @description Standardising EOS-data. This standardising should always be performed.
#'     Otherwise summary numbers can be wrong.
#'
#' @details The function performs the following standardising of data extracted from EOS:
#' \itemize{
#'   \item The column names are standardised using \code{\link{standardize_columns}}.
#'   \item Numeric variables are transformed to numbers.
#'   \item Datetime variables are transformed to dates.
#'   \item Double registrations of a "Sak" due to the municipality being divided
#'         between two Food Safety Authority office, are merged into one and for
#'         these, the information on Food Safety Authority office is removed.
#'   \item Splits saksnr into saksnr and fagnr if saksnr combines both.
#'   \item Breed is transformed to species.
#'   \item Number of examined samples are corrected so it don't exceed the number
#'         of received samples.
#'   \item Redundant variables are deleted.
#'   }
#' Standardisation of column names may be set to \code{FALSE}. This should only be
#'     done if the column names have been standardised previously as a new
#'     standardisation of column names may give unpredicted results. Remark that all
#'     other standardisations are dependent on standard column names, so the function
#'     will not work if the data do not have standard column names.
#'
#' Transformation from breed to species is only performed when species is included
#'     in the data. You need to import the translation table for PJS-codes to perform
#'     the translation, use \code{PJS_codes_2_text <- read_PJS_codes_2_text()}.
#'
#' Correction of number of tested samples is only done when both number of received
#'     and number of tested are included in the data.
#'
#' There are a few reduntant varibles in some data sets. In CWD data both "sist_overfort"
#'     and "sist_endret" keeps the same information. "sist_endret" is deleted. In
#'     Salmonella and Campylobacter data, "prove_identitet" is always \code{NULL} and
#'     "prove_id" is \code{NULL} for salmonella data and equal ti "id_nr" for Campylobacter
#'     data. Both are deleted. Set \code{delete_redundant = FALSE} to keep them.
#'
#' @param data [\code{data.frame}]\cr
#'     The data retrieved from EOS.
#' @param dbsource [\code{character(1)}]\cr
#'     If specified, this will be used for fetching standard column names by
#'    \code{\link{standardize_columns}}. Defaults to the name of the input data.
#' @param standards [\code{data.frame}]\cr
#'     The translation table to standard column names. Defaults to \code{NULL}.
#' @param standardize_colnames [\code{logical(1)}]\cr
#'     If \code{TRUE}, the column names will be standardised. Defaults to \code{TRUE}.
#' @param breed_to_species [\code{logical(1)}]\cr
#'     If \code{TRUE}, breed is translated back to species. Defaults to \code{TRUE}.
#' @param adjust_n_examined [\code{logical(1)}]\cr
#'     If \code{TRUE}, the number of examined samples is adjusted so it is at maximum
#'     the number of received samples. Defaults to \code{TRUE}.
#' @param delete_redundant [\code{logical(1)}]\cr
#'     If \code{TRUE}, redundant variables in the data is deleted. Defaults to \code{TRUE}.
#' @param \dots Other arguments to be passed to \code{\link{standardize_columns}}.
#'
#' @return \code{data.frame} with standardized EOS-data.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @name standardize_eos_data-defunct
#' @keywords internal
#'
NULL

#' @export
#' @rdname NVIdb-defunct
#' @keywords internal
#'
standardize_eos_data <- function(...) {

  .Defunct(new = "NVIpjsr::standardize_eos_data", package = "NVIdb")

  # .Deprecated(new = "standardize_eos_data",
  #             package = "NVIdb",
  #             msg = paste("'standardize_eos_data' is replaced by
  #                         'NVIpjsr::standardize_eos_data'"))
  #
  # if (isTRUE(NVIcheckmate::check_package(x = "NVIpjsr", type = "installed"))) {
  #   select_statement <- NVIpjsr::standardize_eos_data(data = data,
  #                                                     dbsource = dbsource,
  #                                                     standards = standards,
  #                                                     standardize_colnames = standardize_colnames,
  #                                                     breed_to_species = breed_to_species,
  #                                                     adjust_n_examined = adjust_n_examined,
  #                                                     delete_redundant = delete_redundant,
  #                                                     ...)
  #
  #   return(select_statement)
  # } else {
  #   # ARGUMENT CHECKING ----
  #   # Object to store check-results
  #   checks <- checkmate::makeAssertCollection()
  #   # Perform checks
  #   checkmate::assert_data_frame(data, add = checks)
  #   checkmate::assert_character(dbsource, len = 1, min.chars = 1, add = checks)
  #   checkmate::assert_data_frame(standards, null.ok = TRUE, add = checks)
  #   checkmate::assert_flag(standardize_colnames, add = checks)
  #   checkmate::assert_flag(breed_to_species, add = checks)
  #   checkmate::assert_flag(adjust_n_examined, add = checks)
  #   checkmate::assert_flag(delete_redundant, add = checks)
  #   # Report check-results
  #   checkmate::reportAssertions(checks)
  #
  #   # STANDARDISE DATA ----
  #   # Standardise column names
  #   if (isTRUE(standardize_colnames)) {
  #     data <- standardize_columns(data = data,
  #                                 dbsource = dbsource,
  #                                 standards = standards,
  #                                 property = "colnames",
  #                                 ...)
  #   }
  #
  #   if (isTRUE(delete_redundant)) {
  #     data$sist_overfort2 <- NULL
  #     data$prove_id <- NULL
  #     data$prove_identitet <- NULL
  #   }
  #
  #   # Change to numeric for ID-numbers and counts
  #   # Performed before trimming character variables to reduce variables that needs to be trimmed
  #   cols_2_modify <- intersect(colnames(data), c("lopenr", "aar", "innsendelsenr", "avvik_i_registrering",
  #                                                "ant_prover", grep("ant_und", colnames(data), value = TRUE)))
  #   # for (number in cols_2_modify) {
  #   #   data[, number] <- as.numeric(data[, number])
  #   # }
  #   data[, cols_2_modify] <- lapply(data[, cols_2_modify], as.numeric)
  #
  #   # Change to date for date-variables
  #   # Performed before trimming character variables to reduce variables that needs to be trimmed
  #   cols_2_modify <- intersect(colnames(data), c("mottatt", "uttatt", "avsluttet", "sist_endret", "sist_overfort", "sist_overfort2"))
  #   for (dato in cols_2_modify) {
  #     data[, dato] <- as.Date(substr(data[, dato], 1, 10), format = "%Y-%m-%d")
  #   }
  #
  #   # remove double rows due to one Sak being assigned to two MT offices
  #   # It varies which variables keep the information on MT office
  #   proveid <- intersect(c("saksnr", "id_nr", "art", "driftsform"),
  #                        colnames(data))
  #   groupvar <- intersect(c("rekvirent", "rekvirentnr", "mt_avdelingnr", "mt_avdeling"),
  #                         colnames(data))
  #   data <- data |>
  #     dplyr::add_count(dplyr::across(dplyr::all_of(proveid)), name = "ant_per_sak") |>
  #     dplyr::add_count(dplyr::across(dplyr::all_of(c(proveid, groupvar))), name = "ant_per_MT")
  #
  #   rownums <- which(data$ant_per_sak == (2 * data$ant_per_MT))
  #   column_names <- intersect(c("lopenr", "rekvirent", "rekvirentnr", "mt_avdelingnr", "mt_avdeling"),
  #                             colnames(data))
  #   data[rownums, column_names] <- rep(NA_integer_, length(column_names))
  #   data[, c("ant_per_sak", "ant_per_MT")] <- c(NULL, NULL)
  #   data <- unique(data)
  #
  #   # Split saksnr into saksnr and fagnr if saksnr combines both
  #   if (any(grepl("/", data$saksnr))) {
  #     data$fagnr <- gsub(pattern = "[[:digit:]]*/", replacement = "", x = data$saksnr)
  #     data$saksnr <- gsub(pattern = "/[[:alnum:]' ']*", replacement = "", x = data$saksnr)
  #   }
  #
  #   # backtranslate breed to species
  #   if (isTRUE(breed_to_species) & "art" %in% colnames(data)) {
  #     data <- add_PJS_code_description(data = data,
  #                                      PJS_variable_type = "artrase",
  #                                      code_colname = "art",
  #                                      new_column = "artkode",
  #                                      backward = TRUE,
  #                                      impute_old_when_missing = TRUE)
  #
  #     data <- add_PJS_code_description(data = data,
  #                                      PJS_variable_type = "art",
  #                                      code_colname = "artkode",
  #                                      new_column = "art",
  #                                      position = "keep",
  #                                      overwrite = TRUE,
  #                                      impute_old_when_missing = TRUE)
  #     data$artkode <- NULL
  #   }
  #
  #   # adjust number of examined
  #   if (isTRUE(adjust_n_examined) & "ant_prover" %in% colnames(data)) {
  #     ant_und <- grep("ant_und", colnames(data), value = TRUE)
  #     for (i in ant_und) {
  #       data[, i] <- pmin(data[, "ant_prover"], data[, i])
  #     }
  #   }
  #   return(data)
  # }
}
