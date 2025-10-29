#### UTILITY FUNCTIONS ----
# The utility functions are functions reused within the package to avoid rewriting of code. They are not intended for use in the R-scripts,
# and are therefore not exported to Namespace.
#

### copy_file_if_updated ----
#' @title Copy updated files
#' @description Copies .
#' @details Compares the creation date of source file and target file. If the target file don't exist, the source file is copied.
#'     If the the creation date of the source file is later than the target file, the source file is copied and overwrites the target
#'     file.
#'
#'     The intention of checking the creation date before copying files is to avoid unnecessary copying. Some of the source files
#'     may be large and by first checking the creation date computer time can be saved.
#'
#'
#' @param filename Filename of the file that should be copied or updated
#' @param from_path Path for the source file
#' @param to_path Path for the target file
#' @return Copies the source file if the target file is missing or older than source file
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @examples
#' \dontrun{
#' copy_file_if_updated(filename, from_path, to_path)
#' }
#' @keywords internal

copy_file_if_updated <- function(filename, from_path, to_path) {

  # Check if from_path and to_path ends in "/". If not, "/" is added.
  from_path <- sub("/+$|\\\\+$", "", from_path)
  to_path <- sub("/+$|\\\\+$", "", to_path)
  # if (!endsWith(from_path, "/")) { from_path <- paste0(from_path, "/") }
  # if (!endsWith(to_path, "/")) { to_path <- paste0(to_path, "/") }

  # Get creation date of source file
  if (dir.exists(from_path)) {
    files <- list.files(from_path, pattern = filename, ignore.case = TRUE)
    if (grep(filename, files)) {
      source_file_created <- file.mtime(file.path(from_path, filename))
    }
  }

  # Get creation date of target file
  target_file_created <- 0
  if (dir.exists(to_path)) {
    files <- list.files(to_path, pattern = filename, ignore.case = TRUE)
    if (length(files) == 0) {
      target_file_created <- 0
    } else {
      if (grep(filename, files)) {
        target_file_created <- file.mtime(file.path(to_path, filename))
      }
    }
  }

  # Copies the source file if source file is newer
  if (source_file_created > target_file_created) {
    file.copy(from = file.path(from_path, filename),
              to = to_path,
              overwrite = TRUE,
              copy.date = TRUE)

  }
}

### ----

### add_new_column ----
#' @title Add new column
#' @description Add a new column with content based on a column in the dataset.
#' @details Add a new column with content based on a column in the dataset. The column is translated to the new content based on
#'     a translation table. The translation table is joined with the original data.frame. The new column is placed to the right for the
#'     from_column.
#'
#'     The function is called by functions for translating kommunenr into gjeldende_kommunenr, PJS-codes into descriptive text,
#'     kommunenr into MT_avdeling and MT_region.
#'
#'     The function is internal and can only be called from NVIdb-functions
#'
#' @param data A data.frame with the variable that should be translated
#' @param ID_column A vector with the column name(s) of the column(s) with values that shall be translated.
#' @param new_colname A vector with column name(s) of the column(s) where the translated value should be put
#' @param translation_tables A list with translation tables
#' @param ID_column_translation_table A vector with the column name(s) of the columns in the translation table(s) that should
#'       be merged with the ID column in the data, i.e. the column in the translation table with ID values
#' @param to_column_translation_table A vector with the column name(s) in the translation tables that have the translated values
#' @param position position for the new columns, can be one of c("first", "left", "right", "last")
#' @param overwrite When the new column(s) already exist, the content in the existing column(s) is replaced by new data if TRUE. When FALSE,
#'     a warning is issued and new variables are created.
#' @param impute_old_when_missing [\code{logical(1)}] \cr
#'     Should existing value be transferred if no value for the code is found?
#'     Defaults to \code{FALSE}.
#'
#' @return A data.frame with a new column with the translated value. The new
#'     column is placed in the position given in position.
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @keywords internal

add_new_column <- function(data,
                           ID_column,
                           new_colname,
                           translation_tables,
                           ID_column_translation_table,
                           to_column_translation_table,
                           position = "right",
                           overwrite = FALSE,
                           impute_old_when_missing = FALSE,
                           n_columns_at_once = 1) {

  # Transforms the data to a data.frame and removes other classes
  # I'm afraid that this might be dependent on packages making the alternative classes (i.e. dplyr) must be loaded
  #   if it is dplyr that makes is.data.frame to work for these classes
  if (is.data.frame(data) & length(class(data)) > 1) {
    data <- as.data.frame(data)
  }

  # Add row to keep original sort order of data
  data$original_sort_order <- seq_len(nrow(data))


  for (i in 1:length(ID_column)) {

    # First and last column in the translation table if a code should be translated to more than one variable at once
    # This used in add_MT_area to add several MT area desciptions based on komnr
    first_to_colnum <- (1 + (n_columns_at_once * (i - 1)))
    last_to_colnum <- i * n_columns_at_once

    # Make a subset with only the codes that is relevant for the actual variable
    translation_table <- translation_tables[[i]]
    code_2_new_text <- translation_table[, c(ID_column_translation_table[i], to_column_translation_table[c(first_to_colnum:last_to_colnum)])]

    # Changes the name of the new column in the translation table to the name wanted in the df
    # Rename ID_column_translation_table[i] in translation table to ID_column_name_zz
    #   that is supposed to be unique and thereby avoid column name conflicts
    colnames(code_2_new_text) <- c("ID_column_name_zz", new_colname[c(first_to_colnum:last_to_colnum)])

    # If new variable names already exists in data frame and overwrite = TRUE
    # Identifies all columns with common names in data and in new columns to add
    existing_names <- intersect(colnames(data), new_colname[c(first_to_colnum:last_to_colnum)])

    # Replace position = keep with right if overwrite = FALSE
    if (!overwrite | length(existing_names) == 0) {position <- gsub("keep", "right", position)}
    if (length(existing_names) > 0 & overwrite) {
      if (position == "keep") {
        keep_colnum <- min(which(colnames(data) %in% existing_names))
      }
      # Removes already existing names so that they can be replaced with new data (overwritten)
      for (j in existing_names) {
        data[, j] <- NULL
      }
    }
    # Finds the column number for the code variable
    code_colnum <- which(colnames(data) == ID_column[i])

    # Trim trailing spaces from the coded variable
    # This may be necessary for data taken from PJS before merging
    data[, ID_column[i]] <- trimws(data[, ID_column[i]])

    # joins the dataset with the code description
    data <- merge(data,
                  code_2_new_text,
                  by.x  = ID_column[i],
                  # by.y  = ID_column_translation_table[i],
                  by.y  = "ID_column_name_zz",
                  all.x = TRUE)

    # Imputes with values in code variable in old dataset in the case that no merge was performed
    # Only if impute_old_when_missing = TRUE
    if (isTRUE(impute_old_when_missing)) {
      data[which(is.na(data[, new_colname])), new_colname] <- data[which(is.na(data[, new_colname])), ID_column]
    }

    # Rearrange columns
    # Merge places the by-columns first and the new columns last in the data frame
    # 1. Put by-column back to original place (= code_colnum). If code_colnum == 1, the column is already correct placed
    if (code_colnum > 1) {
      data <- data[, c(2:code_colnum,
                       1,
                       (code_colnum + 1):dim(data)[2])]
    }
    # 2. Move the new columns to their new position.
    #    The new position is specified by the parameter position = c("first", "left", "right", "last")

    # Identifies column number of first new column
    new_colnum <- which(colnames(data) == new_colname[first_to_colnum])

    # position == "right" Move column with description to the right of the column with code
    if (position == "right") {
      # If already to the right, no need to do any change
      if (code_colnum != (new_colnum - 1)) {
        # move to the right of the code_column
        data <- data[, c(1:code_colnum,
                         c(new_colnum:(new_colnum + n_columns_at_once - 1)),
                         (code_colnum + 1):(new_colnum - 1))]
      }
    }
    # position == "left" Move column with description to the left of the column with code
    if (position == "left") {
      # move to the left of the code_column
      if (code_colnum == 1) {
        data <- data[, c(c(new_colnum:(new_colnum + n_columns_at_once - 1)),
                         (code_colnum):(new_colnum - 1))]
      } else {
        data <- data[, c(1:(code_colnum - 1),
                         c(new_colnum:(new_colnum + n_columns_at_once - 1)),
                         (code_colnum):(new_colnum - 1))]
      }
    }
    # position == "last" No need to change anything as merge place the new columns last
    if (position == "last") {
      # data <- data
    }
    # position == "first" Move column with description to the first column
    if (position == "first") {
      # move to the right of the code_column
      data <- data[, c(c(new_colnum:(new_colnum + n_columns_at_once - 1)),
                       1:(new_colnum - 1))]
    }
    # position == "keep" Move column with description to the same column as the column that is replaced/overwritten
    if (position == "keep") {
      # move to the left of the code_column
      if (keep_colnum == 1) {
        data <- data[, c(c(new_colnum:(new_colnum + n_columns_at_once - 1)),
                         (keep_colnum):(new_colnum - 1))]
      } else {
        data <- data[, c(1:(keep_colnum - 1),
                         c(new_colnum:(new_colnum + n_columns_at_once - 1)),
                         (keep_colnum):(new_colnum - 1))]
      }
    }

  }


  # Sorts data in original order and removes sort key
  data <- data[order(data$original_sort_order), ]
  data$original_sort_order <- NULL

  return(data)
}


### ----

# ### read_csv_file ----
# #' @title Read csv-file
# #' @description Reads files with data .
# #' @details Used to read csv files with data for use in scripts.
# #'
# #' @param filename Filename of the file that should be read
# #' @param from_path Path for the source file
# #' @param options A list with the arguments colClasses and fileEncoding. If
# #'    \code{options = NULL} it defaults to
# #'    \code{list(colClasses = NA, fileEncoding = "UTF-8")}.
# #' @param \dots	Other arguments to be passed to \code{data.table::fread}.
#
# #' @return A data.frame with the data from the source file.
# #' @author Petter Hopp Petter.Hopp@@vetinst.no
# #'
# #' @examples
# #' \dontrun{
# #' read_csv_file(filename, from_path, columnclasses, fileencoding)
# #' }
# #' @keywords internal
#
# read_csv_file <- function(filename, from_path, options = NULL, ...) {
#
#   # Removes trailing "/" and "\\".
#   from_path <- sub("/+$|\\\\+$", "", from_path)
#   # # Check if from_path ends in "/". If not, "/" is added.
#   # if (!endsWith(from_path, "/")) { from_path <- paste0(from_path, "/") }
#
#   # if (is.null(sep)) {sep <- ";"}
#   # if (!exists("dec")) {dec <- ","}
#
#   if (is.null(options)) {
#     options <- list(colClasses = NA, fileEncoding = "UTF-8")
#   } else {
#     if (is.null(options$colClasses)) {options$colClasses <- NA}
#     if (is.null(options$fileEncoding)) {options$fileEncoding <- "UTF-8"}
#   }
#   # Get creation date of source file
#   if (dir.exists(from_path)) {
#     if (file.exists(file.path(from_path, filename))) {
#       df <- data.table::fread(file = file.path(from_path, filename),
#                               colClasses = options$colClasses,
#                               encoding = options$fileEncoding,
#                               showProgress = FALSE,
#                               data.table = FALSE,
#                               ...)
#       # df <- utils::read.table(file = file.path(from_path, filename),
#       #                         colClasses = options$colClasses,
#       #                        fileEncoding = options$fileEncoding,
#       #                         stringsAsFactors = options$stringsAsFactors,
#       #                        sep = sep,
#       #                        dec = dec,
#       #                        header = TRUE,
#       #                        ...)
#     }
#   }
#   return(df)
#   # return(as.data.frame(df))
# }

###   ----

### set_name_vector ----
#' @title set_name_vector
#' @description Ensures that all elements in a vector are named.
#' @details Used to name all elements in vectors with column names that should
#'     be added to data frames. Thereby it can easily be differentiated between
#'     standard column names (vector elements) and wanted column names in the
#'     generated tables (names). The user can input a named vector to the
#'     function. This is considered easier for the user to understand than
#'     inputting two vectors.
#'
#'     This function is used in the add-functions before calling add_new_variable.
#'
#' @param x [\code{character}]\cr
#' A unnamed, partly named or fully named vector of character variables input
#'     in add-functions.

#' @return A named vector where previously unnamed elements have been named with
#'     the element value as name.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @keywords internal
#' @examples
#' \dontrun{
#' new_columns <- c("name11" = "column1", "name2" = "column2", "column3")
#' new_columns <- set_name_vector(new_columns)
#' }
#'
# Function that ensure that all elements in a vector are named
# For elements that aren't named, the vector value are used as name
set_name_vector <- function(x) {
  new_names <- x
  new_names[which(names(x) != "")] <- names(x[which(names(x) != "")])
  x <- stats::setNames(x, new_names)
  return(x)
}

# set_name_vector <- function(colname_vector) {
#   # Existing names to the vector name
#   name <- names(colname_vector)
#
#   # vector values to unnamed vector
#   column <- unname(colname_vector)
#
#   # Check if any elements are named
#   if (!is.null(name)) {
#     # if some elements are named, move element value to unnamed elements
#     for (i in 1:length(name)) {
#       if (name[i] == "") {name[i] <- column[i]}
#     }
#
#     # if no elements are named, set element values as names
#   } else {name <- column }
#
#   return(stats::setNames(colname_vector, name))
# }

###   ----

### find_n_th_word ----
#' @title find_n_th_word
#' @description Funtion to find the n'th word in a string.
#' @details Funtion to find the n'th word in a string. The function is used in data cleaning.
#'     For example when all filenames in a directory has been read (see read_Produksjonstilskudd) this function is used to identify
#'     word in the filename that can be used to select the latest filefor a certain period.
#'
#' @param x a string where the n'th word should be selected
#' @param position the position of the word that should be selected.

#' @return The n'th word in a character string separated by spaces.If less than n words, NA is returned.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @examples
#' \dontrun{
#' #' Find second word in a string
#' find_n_th_word("This is a text", 2)
#'
#' #' Find second word in all rows in a column with a string
#' data <- rbind("This is a text", "The text is short", "Short", "Or a little bit longer")
#' colnames(data) <- "text"
#' data$word2 <- sapply(data$text, FUN = find_n_th_word, position = 2)
#' }
#' @noRd

find_n_th_word <- function(x, position) {strsplit(x, " ")[[1]][position]}


### cut_slash ----
#' @title Cut away ending slash from string
#' @description Removes ending slash or backslash from string.
#'     This is used to clean pathnames so that elements in a path
#'     can be combined using
#'     \ifelse{html}{\code{\link[base:file.path]{file.path}}}{\code{file.path}}
#'     in stead of
#'     \ifelse{html}{\code{\link[base:paste0]{paste0}}}{\code{paste0}}.
#'
#' @param x  [\code{character}]\cr
#'   Strings, typically file paths, where ending slash should be removed.
#'
#' @return Object without ending slash in character strings.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' # Remove from string
#' cut_slash("C:/temp/")
#' cut_slash("C:\\temp\\")
#' cut_slash(c("C:/temp/", "C:\\temp\\"))
#' cut_slash(list("C:/temp/", "C:\\temp\\"))
cut_slash <- function(x) {
  x <- gsub("/+$|\\\\+$", "", x)
  return(x)
}
