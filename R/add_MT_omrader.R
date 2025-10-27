#' @title Manage translation from komnr to MT-omrader
#' @description Function to add columns with the various MT-omrader. In addition,
#'    there are functions to read and copy the translation tables.
#' @details \code{add_MT_omrader} can be used to translate the komnr into
#'     MT-omrader. The organisation of MT in areas was revised in 2025. If the
#'     input year >= 2025, the komnr will be translated to the structure from 2025:
#'     MT_divisjonnr, MT_divisjon, MT_avdelingnr,MT_avdeling, MT_enhetrn,
#'     MT_enhet. If the input year is between 2004 and 2024, the komnr will be
#'     translated to the structure from 2004 to 2024: MT_avdelingnr, MT_avdeling,
#'     MT_regionnr and MT_region.
#'
#' The function can also be used to translate MT_avdelingnr into
#'     MT_avdeling, MT_regionnr and MT_region or to translate MT_regionnr
#'     into MT_region.
#'
#' When the \code{code_column} in the data frame is
#'     not named one of c("komnr", "MT_avdelingnr", "MT_regionnr") the
#'     \code{code_column} can be input as a named vector. Likewise, if
#'     the new columns should be given other names than c("MT_avdelingnr",
#'     "MT_avdeling", "MT_regionnr", "MT_region"), the \code{new_column}
#'     can be input as a named vector, see examples.
#'
#' \code{add_MT_omrader} uses a pre made translation tables
#'     ("komnr_2_MT_avdeling.csv" and "komnr_2_MT_enhet.csv"). These data need
#'     to be loaded by \code{read_MT_omrader} before running \code{add_MT_omrader},
#'     see example. "komnr_2_MT_avdeling.csv" is made based on information in
#'     PJS adresseregister and "komnr_2_MT_enhet.csv" has been given by MT in
#'     2025. The translation table is updated when we know there is a need.
#'
#' \code{position} is used to give the place if the new columns in the
#'     data frame. For \code{position = "right"} the new variables are
#'     placed to the right of the code_variable. Likewise, for
#'     \code{position = "left"} the new variables are placed to the left of
#'     the code_variable. If \code{position = "first"} or
#'     \code{position = "last"} the new columns are placed first or last,
#'     respectively, in the data.frame. A special case occurs for
#'     \code{position = "keep"} which only has meaning when the new column
#'     has the same name as an existing column and overwrite = TRUE. In these
#'     cases, the existing column will be overwritten with new data and have
#'     the same position.
#'
#' \code{read_MT_omrader} reads the files "komnr_2_MT_avdeling.csv",
#'     "MT_omrader.csv", and "komnr_2_MT_enhet.csv" into a data frame, usually
#'     named komnr_2_MT_omrader. This file is used by \code{add_MT_omrader}. If
#'     no options to the
#'     function is given, the function will read the latest updated files
#'     from NVI's internal network. If changing the \code{from_path}, the
#'     function can be used to read the translation file from other
#'     directories. This can be useful if having a script that don't have
#'     access to NVI's internal network.
#'
#' \code{copy_MT_omrader} Copies the csv-files "komnr_2_MT_avdeling.csv",
#'     "MT_omrader.csv", and "komnr_2_MT_enhet.csv" to another directory.
#'     Thereby, these files are available for \code{read_MT_omrader} if they
#'     should be read from another directory.
#'
#' @param data [\code{data.frame}]\cr
#' Data with a column with kommunenummer (komnr).
#' @param year [\code{integer(1)}]\cr
#' year for which the organisation of MT should be selected. Defaults to current
#'     year.
#' @param fag [\code{character(1)}]\cr
#' fag is the production or topic in the organisation of MT. This is used for
#'     year >= 2025. Defaults to \code{NULL}.
#' @param translation_table [\code{data.frame}]\cr
#' The translation table for translating komnr to MT-areas. Defaults
#'     to \code{komnr_2_MT_omrader}.
#' @param code_column [\code{character(1)}]\cr
#' The column with the code value. Valid values are one
#'     of c("komnr", "MT_avdelingnr", "MT_regionnr", "MT_divisjonnr"). If the column in data
#'     has another name, it can be input as a named vector, see examples.
#'     Defaults to "komnr".
#' @param new_column [\code{character}]\cr
#' The name(s) of the new column(s) that should be added to the data,
#'     see examples. Defaults to c("MT_avdelingnr", "MT_avdeling",
#'     "MT_enhetnr", "MT_enhetnr").
#' @template position
#' @template overwrite
#' @param filename [\code{list}]\cr
#' File names of the source files for the translation table. Defaults to
#'     list("komnr_2_MT_avdeling.csv", "MT_omrader.csv", "komnr_2_MT_enhet.csv").
#' @template from_path_add
#' @template to_path_add
#' @param \dots	Other arguments to be passed to
#'     \ifelse{html}{\code{\link[utils:read.csv2]{utils::read.csv2}}}{\code{utils::read.csv2}}.
#'
#' @return \code{add_MT_omrader} A data frame where the number and name of the
#'     MT-areas have been added in the column to the right of the column with
#'     the komnr.
#'
#' \code{read_MT_omrader} A data frame with the table for translating from komnr
#'     to MT-omrader as read from the source csv file. If not changing standard
#'     input to the function, the standard files at NVI's internal network is read.
#'
#' \code{copy_MT_omrader} Copies the csv-files "komnr_2_MT_avdeling.csv",
#'     "MT_omrader.csv", and "komnr_2_MT_enhet.csv" to another directory. If the
#'     target files already exists the source files are only copied if they are
#'     newer than the target files.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' komnr_2_MT_omrader <- read_MT_omrader()
#'
#' # Copy the csv-files used to generate the translation table from the standard location to
#' # the subdirectory Data below the working directory
#' copy_MT_omrader(to_path = "./Data/")
#'
#' # Reading from the subdirectory Data below the working directory
#' komnr_2_MT_omrader <- read_MT_omrader(from_path = "./Data/")
#'
#' # Add new columns with MT_avdelingnr, MT_avdeling, MT_regionnr, and MT_region based on komnr
#' # Remember to load "komnr_2_MT_omrader" by "read_MT_omrader()" before running "add_MT_omrader",
#' # see above.
#' newdata <- add_MT_omrader(olddata,
#'                        translation_table = list(komnr_2_MT_omrader),
#'                        code_column = "komnr",
#'                        new_column = c("MT_avdelingnr", "MT_avdeling", "MT_regionnr", "MT_region"))
#'
#' # Add new columns with MT_avdelingnr and MT_avdeling based on komnr. The colname of the column
#' # with komnr is komnr and the new columns are renamed to MT_avdnr and MT_avd.
#' # Remember to load "komnr_2_MT_omrader" by "read_MT_omrader()" before running "add_MT_omrader",
#' # see above.
#' newdata <- add_MT_omrader(olddata,
#'                        translation_table = list(komnr_2_MT_omrader),
#'                        code_column = c("komnr" = "komnr"),
#'                        new_column = c("MT_avdnr" = "MT_avdelingnr", "MT_avd" = "MT_avdeling"))
#'
#' # Add new columns with MT_region based on MT_regionnr. MT_region is renamed to MT_regionnavn
#' # Remember to load "komnr_2_MT_omrader" by "read_MT_omrader()" before running "add_MT_omrader",
#' # see above.
#' newdata <- add_MT_omrader(olddata,
#'                        translation_table = list(komnr_2_MT_omrader),
#'                        code_column = "MT_region",
#'                        new_column = c("MT_regionnavn" = "MT_region"))
#' }
#'
add_MT_omrader <- function(data,
                           year = as.numeric(format(Sys.Date(), "%Y")),
                           fag = NULL,
                           translation_table = komnr_2_MT_omrader,
                           code_column = c("komnr"),
                           new_column = c("MT_avdelingnr", "MT_avdeling", "MT_enhetnr", "MT_enhet"),
                           position = "right",
                           overwrite = FALSE) {

  # Ensure that code_column and new_column are named vectors by using the internal function set_name_vector()
  # Thereby, the following code can assume these to be named vectors
  code_column <- set_name_vector(code_column)
  new_column <- set_name_vector(new_column)

  # GENERATE TRANSLATION TABLE FOR fag
  text <- "produkt_fag;input_fag
Akvakultur;Akvakultur
Akvakultur;Laks
Akvakultur;\u00d8rret
Akvakultur;Skjell
Landdyr - produksjonsdyr;Landdyr - produksjonsdyr
Landdyr - produksjonsdyr;Storfe
Landdyr - produksjonsdyr;Sau
Landdyr - produksjonsdyr;Geit
Landdyr - produksjonsdyr;Svin
Landdyr - produksjonsdyr;Gris
Landdyr - produksjonsdyr;Hjort
Landdyr - produksjonsdyr;Vilt
Landdyr - kj\u00e6ledyr og hest;Landdyr - kj\u00e6ledyr og hest
Landdyr - kj\u00e6ledyr og hest;Hund
Landdyr - kj\u00e6ledyr og hest;Katt
Landdyr - kj\u00e6ledyr og hest;Hest
Slakteri r\u00f8dt kj\u00f8tt;Slakteri r\u00f8dt kj\u00f8tt
Slakteri r\u00f8dt kj\u00f8tt;Storfeslakt
Slakteri r\u00f8dt kj\u00f8tt;Saueslakt
Slakteri r\u00f8dt kj\u00f8tt;Geiteslakt
Slakteri r\u00f8dt kj\u00f8tt;Svineslakt
Slakteri r\u00f8dt kj\u00f8tt;Griseslakt
Slakteri r\u00f8dt kj\u00f8tt;Hjorteslakt
Slakteri r\u00f8dt kj\u00f8tt;Hesteslakt
Slakteri r\u00f8dt kj\u00f8tt;r\u00f8dt kj\u00f8tt
Slakteri hvitt kj\u00f8tt;Slakteri hvitt kj\u00f8tt
Slakteri hvitt kj\u00f8tt;hvitt kj\u00f8tt
Slakteri hvitt kj\u00f8tt;fj\u00f8rfeslakt
Mat - matproduksjon - kjeder;Mat - matproduksjon - kjeder
Mat - matproduksjon;Mat - matproduksjon
Mat - servering og omsetning - kjeder;Mat - servering og omsetning - kjeder
Mat - servering og omsetning;Mat - servering og omsetning
Grensekontroll;Grensekontroll
Mat - samhandel/import - ikke grensekontroll;Mat - samhandel/import - ikke grensekontroll
Mat - kosttilskudd og kosmetikk;Mat - kosttilskudd og kosmetikk
Mat - kosttilskudd og kosmetikk;Kosttilskudd
Mat - kosttilskudd og kosmetikk;Kosmetikk
Planter;Planter
F\u00f4r og biprodukter;F\u00f4r og biprodukter
F\u00f4r og biprodukter;F\u00f4r
F\u00f4r og biprodukter;Biprodukter
Drikkevann;Drikkevann"

  # Read the string into a data frame
  produkt_fag <- utils::read.table(text = text, sep = ";", header = TRUE)


  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checks <- assert_add_functions(data = data,
                                 translation_table = translation_table,
                                 code_column = code_column,
                                 new_column = new_column,
                                 overwrite = overwrite,
                                 add = checks)
  # position
  position <- NVIcheckmate::match_arg(x = position,
                                      choices = c("first", "left", "right", "last", "keep"),
                                      several.ok = TRUE,
                                      ignore.case = FALSE,
                                      add = checks)
  # year
  checkmate::assert_integerish(year,
                               len = 1,
                               lower = 2004,
                               upper = as.numeric(format(Sys.Date(), "%Y")) + 1,
                               any.missing = FALSE,
                               all.missing = FALSE,
                               add = checks)
  # fag
  if (!is.null(year) && !is.na(year) && year >= 2025) {
    checkmate::assert_choice(x = tolower(fag),
                             choices = tolower(produkt_fag$input_fag),
                             add = checks)
  }
  # Report check-results
  checkmate::reportAssertions(checks)

  # PREPARE TRANSLATION TABLE ----
  # Makes the translation table with code_column and new_column.
  # Selects rows for year
  translation_table[which(is.na(translation_table$to_year)), "to_year"] <- as.numeric(format(Sys.Date(), "%Y")) + 1
  translation_table <- subset(translation_table, translation_table$from_year <= year & translation_table$to_year >= year)

  if (2025 <= year) {
    produkt_fag <- produkt_fag[tolower(produkt_fag$input_fag) == tolower(fag), ]
    translation_table <- translation_table[translation_table$produkt_fag == produkt_fag$produkt_fag, ]
  }

  # unique() is necessary to avoid duplicate rows when code_column is not "komnr"
  code_2_new <- unique(translation_table[, c(unname(code_column), unname(new_column))])

  # ADD NEW COLUMN(S) ----
  # Set up of parameters for the internal function add_new_column(). names() is used to select the column names
  # in the input data and unname() is used to select the column names in the translation table. n_columns_at_once
  # is the number of new columns that should be added.
  data <- add_new_column(data,
                         ID_column = names(code_column),
                         new_colname = names(new_column),
                         translation_tables = list(code_2_new),
                         ID_column_translation_table = unname(code_column),
                         to_column_translation_table = unname(new_column),
                         position = position,
                         overwrite = overwrite,
                         n_columns_at_once = length(new_column)
  )

  return(data)
}

# To avoid checking of the variable kommune_fylke as default input argument in the function
utils::globalVariables("komnr_2_MT_omrader")
