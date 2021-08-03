# assert_arguments
#
# assert collections that developed to fit functions with standardized input arguments.
# - assert_add_functions
#
#
#' @title Collection of assertions for add_functions 
#' @description Collection of assertions used in standard add_functions. 
#'
#' @details All add functions except one, have the same arguments and the 
#'     assertion can be standardized. 
#'
#' @param data Argument to the add-function to be asserted. 
#' @param translation_table Argument to the add-function to be asserted. 
#' @param code_column Argument to the add-function to be asserted. 
#' @param new_column Argument to the add-function to be asserted. 
#' @param position Argument to the add-function to be asserted. 
#' @param overwrite Argument to the add-function to be asserted. 
#'
#' @return \code{TRUE } if none of the assertions failed. If any of the assertions 
#'     failed, one or more error messages are returned. 
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @keywords internal
#'
  assert_add_function <- function(data,
                                  translation_table,
                                  code_column,
                                  new_column,
                                  position,
                                  overwrite) {
    
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  
  # Perform checks
  # data
  checkmate::assert_data_frame(data, add = checks)
  # translation_table
  checkmate::assert_data_frame(translation_table, add = checks)
  # code_column
  checkmate::assert_vector(code_column, any.missing = FALSE, len = 1, add = checks)
  NVIcheckmate::assert_names(unname(code_column), 
                             type = "named", 
                             subset.of = colnames(translation_table), 
                             comment = paste0("The value for code_column must be a column in the translation table, ",
                                            #  deparse(substitute(translation_table)),
                                              "but '",
                                              unname(code_column), 
                                              "' is not a column name in the translation table"),
                             add = checks)
  NVIcheckmate::assert_names(names(code_column), 
                             type = "named", 
                             subset.of = colnames(data), 
                             comment = paste0("The name of the code_column must be a column in the data",
                                              # deparse(substitute(data)),
                                              ", but '",
                                              base::setdiff(names(code_column), colnames(data)), 
                                              "' is not a column in the data. You must name the code_column ",
                                              "if the code_column in data is different from in the translation table"),
                             add = checks)
  # new_column
  checkmate::assert_vector(new_column, any.missing = FALSE, min.len = 1, add = checks)
  NVIcheckmate::assert_names(unname(new_column), 
                             type = "named", 
                             subset.of = colnames(translation_table), 
                             comment = paste0("The value(s) for new_column must be column name(s) in the translation table",
                                              # deparse(substitute(translation_table)),
                                              ", but '",
                                              unname(new_column), 
                                              "' are not column name(s) in the translation table"),
                             add = checks)
  if (isFALSE(overwrite)) {
    NVIcheckmate::assert_names(names(new_column), 
                               type = "named", 
                               disjunct.from = setdiff(colnames(data), code_column),
                               comment = paste0("The column name(s): '",
                                                intersect(colnames(data), names(new_column)),
                                                "' already exist in '",
                                                deparse(substitute(data)),
                                                "`. Either give new column name(s) for the column(s) called '", 
                                                intersect(colnames(data), names(new_column)),
                                                "' or specify overwrite = TRUE to replace values in the existing column(s) with new content"),
                               add = checks)
  }
  NVIcheckmate::assert_names(names(new_column), 
                             type = "named", 
                             disjunct.from = names(code_column), 
                             comment = paste0("You cannot give any of the new column(s) the same name as the code_column '",
                                              names(code_column),
                                              "' in the data" #,
                                              # deparse(substitute(data)), "`"
                                              ),
                             add = checks)
  # position
  checkmate::assert_choice(position, choices = c("first", "left", "right", "last", "keep"), add = checks)
  # overwrite
  checkmate::assert_logical(overwrite, any.missing = FALSE, len = 1, add = checks)
  
  
  # Report check-results
  checkmate::reportAssertions(checks)
  
}