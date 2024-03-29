#' @title Manage adding extra information to aquaculture sites
#' @description Function to add a column with current aquaculture
#'    zone and/or geo-coordinates. In addition there are function
#'    to read the translation table.
#' @details \code{add_lokalitet} can be used to add aquaculture
#'    zone and/or geo-coordinates to aquaculture sites. The new
#'    columns can be one or more of c("sone", "EastUTM_33N_WGS84",
#'    "NorthUTM_33N_WGS84", "Longitude_WGS84", "Latitude_WGS84").
#'    If the new columns in the result data frame should have
#'    other names, \code{new_column} can be input as a named
#'    vector, see examples.
#'
#' \code{position} is used to give the position if the new columns
#'     in the data.frame. For \code{position = "right"} the new
#'     variables are placed to the right of the code_variable.
#'     Likewise, for \code{position = "left"} the new variables
#'     are placed to the left of the code_variable. If
#'     \code{position = "first"} or \code{position = "last"} the
#'     new columns are placed first or last, respectively, in the
#'     data frame. A special case occurs for \code{position = "keep"}
#'     which only has meaning when the new column has the same name
#'     as an existing column and \code{overwrite = TRUE}. In these
#'     cases, the existing column will be overwritten with new data
#'     and have the same position.
#'
#' \code{read_sonetilhorighet} reads the file "sonetilhorighet.txt"
#'     into a data frame that can be used by other routines. Standard
#'     setting will the file read in the latest updated file from
#'     NVI's internal network. If changing the from_path, the
#'     function can be used to read the translation file from
#'     other directories. This can be useful if having a stand
#'     alone app with no connection the NVI's internal network.
#'     In other cases, it should be avoided.
#'
#' @param data [\code{data.frame}]\cr
#' Data with a column with an aquaculture site number ("LokNr")
#' @param translation_table [\code{data.frame}]\cr
#' Table for translating from loknr to the property in question.
#' @param code_column [\code{character(1)}]\cr
#' The column with the coded value. Valid values are one of c("LokNr"). If the column in
#'     data has another name, it can be input as a named vector, see examples.
#' @param new_column [\code{character}]\cr
#' The new columns that should be included into the data frame.
#' @template position
#' @template overwrite
#' @param filename [\code{list}]\cr
#' The filenames of the source files with the tables for generating the translation table.
#' @param from_path [\code{character(1)}]\cr
#' Path for the source files for the translation table.
#'
#' @return \code{add_lokalitet}: \code{data.frame} where the aquaculture
#'     zone and / or geo-coordinates have been added in the column to the
#'     right of the column with the LokNr.
#'
#' \code{read_sonetilhorighet}: \code{data.frame} with "LokNr",
#'     aquaculture zone and geo-coordinates. If not changing standard
#'     input to the function, the standard file at NVI's internal
#'     network is read.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#'
#' # READ TRANSLATION TABLE
#' # Reading from standard directory at NVI's network
#' sonetilhorighet <- read_sonetilhorighet()
#'
#' # ADD AQUACULTURE ZONE
#' eier_lokalitetnr <- c("10298", "10318", "10735", "10814")
#' olddata <- as.data.frame(eier_lokalitetnr)
#'
#' # Add new column with aquculture zone
#' newdata <- add_lokalitet(olddata,
#'                         translation_table = sonetilhorighet,
#'                         code_column = c("eier_lokalitetnr" = "LokNr"),
#'                         new_column = c("produksjonsomraade" = "sone"),
#'                         position = "left")
#'
#' # ADD COORDINATES
#' eier_lokalitetnr <- c("10298", "10318", "10735", "10814")
#' olddata <- as.data.frame(eier_lokalitetnr)
#'
#' # Add new columns with longitude and lattitude
#' newdata <- add_lokalitet(olddata,
#'                         translation_table = sonetilhorighet,
#'                         code_column = c("eier_lokalitetnr" = "LokNr"),
#'                         new_column = c("longitude" = "Longitude_WGS84",
#'                                        "latitude" = "Latitude_WGS84"))
#' }
#'
add_lokalitet <- function(data,
                          translation_table,
                          code_column,
                          new_column,
                          position = "right",
                          overwrite = FALSE) {

  # Ensure that code_column and new_column are named vectors by using the internal function set_name_vector()
  # Thereby, the following code can assume these to be named vectors
  code_column <- set_name_vector(code_column)
  new_column <- set_name_vector(new_column)

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
  # Report check-results
  checkmate::reportAssertions(checks)

  # PREPARE TRANSLATION TABLE ----
  # Makes the translation table with code_column and new_column. unique() is necessary to avoid duplicate
  # rows when code_column is not "kommunenr"
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
                         impute_old_when_missing = FALSE,
                         n_columns_at_once = length(new_column)
  )


  return(data)
}
