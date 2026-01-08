#' @title Standardize columns for scripts and reports
#' @description Standardizes column names, labels, column width
#'     for variables in external databases.
#'
#' @details The standardisation table is under development. This
#'     function only works when being connected to the NVI network.
#'
#' Variables in internal and external data sources uses
#'     different variable names for the same content.
#'     \code{standardize_columns} standardizes column names for
#'     use in scripts. In addition, it standardises column labels
#'     and column widths for Excel. Furthermore, input values for
#'     the parameter \code{colClasses} for
#'     \ifelse{html}{\code{\link[utils:read.csv2]{read.csv2}}}{\code{read.csv2}}
#'     and
#'     \ifelse{html}{\code{\link[data.table:fread]{data.table::fread}}}{\code{data.table::fread}}
#'     can be generated.
#'
#' \code{standards} gives the source file or the data.frame with the standards
#'     for formatting the columns. Default is to give the general source csv
#'     file. It can also be a data.frame as for example the
#'     \ifelse{html}{\code{\link[OKplan:OK_column_standards]{OKplan::OK_column_standards}}}{\code{OKplan::OK_column_standards}}
#'     giving the standards when generating
#'     selection files for the Norwegian surveillance programmes. As this file
#'     is embedded into OKplan-package, it may be convenient to update the
#'     source file and load it as a csv file. On some occasions, it may be most
#'     easy to input the column standards directly using a \code{list}.
#'
#'     #' The list input to column_standards must follow a specific format.
#'     It is a \code{list} with at least three named vectors:
#' \itemize{
#' \item \code{colname}: a vector of all columns in in the source file that
#'     should be included in the Excel report with the selection list.
#' \item \code{collabel}: A vector with the labels that should be used in the
#'     Excel report.
#' \item \code{colwidth}: A vector with the column width that should be used
#'     in the Excel report.
#' }
#'
#'     In addition one may input:
#'
#' \itemize{
#' \item \code{colorder}: the order of the columns to be used in the Excel report.
#'     The default is to use the same order as they are entered in the vectors.
#' \item \code{column_db}: input added as a possibility to keep on the same format
#'     as \code{column_standards}. Not necessary to input.
#' \item \code{table_db}: input added as a possibility to keep on the same format
#'     as \code{column_standards}. Must be the same as \code{dbsource}.
#'     Not necessary to input.
#' }
#'
#' All vectors must have the same order and the same length.
#'
#' \code{property = "colnames"} will replace the column names
#'     in a data frame with standardized column names. All
#'     standard column names is snake_case. If no standard name
#'     is defined for a variable name, the variable
#'     name is translated to snake_case and the national characters
#'     c("æ", "ø", "å") are translated to
#'     c("ae", "oe", "aa").
#'
#' \code{property = "colclasses"} will generate a named vector
#'     with the column classes for variables that may not be read
#'     correct when importing data from a csv-file. This applies
#'     for example to numbers with leading zero that must be imported
#'     as character. This vector can be used as a parameter for
#'     \code{colClasses}.
#'
#' The default \code{fileEncoding} is assumed to be "UTF-8".
#'     If another encoding is needed, one must give an additional
#'     argument like \code{fileEncoding = "latin1"}.
#'
#' \code{property = "collabels"} will generate a vector with column
#'     labels that can be used to replace the column names in the
#'     header of the data table. The column names are not standardised
#'     automatically but can be standardised by first using
#'     \code{standardize_columns} with \code{property = "colname"}.
#'     If no standard column label for the column name is defined,
#'     the column name as Sentence case is used as column label.
#'     If English names are used and no English column label exists,
#'     the Norwegian column label is used instead.
#'
#' \code{property = "colwidths_Excel"} will generate a vector with
#'     column widths for Excel. To be used as input parameter to
#'     \ifelse{html}{\code{\link[openxlsx:setColWidths]{openxlsx::setColWidths}}}{\code{openxlsx::setColWidths}}.
#'     If no standard column width is defined, the Excel standard
#'     width of 10.78 is used. Be aware that the generation of column
#'     widths are based on the column names. Do not change the column
#'     names to labels before the column widths are generated.
#'
#' \code{property = "colorder"} will generate a data frame with
#'     the column names in a predefined order. The column names
#'     should first have been standardised. No standard order will
#'     be given unless the dbsource is defined in the column_standards
#'     table. If \code{exclude = FALSE} (the standard) the columns
#'     with no predefined order will be moved to the last columns
#'     in the same order as they appeared in the original data frame.
#'     If \code{exclude = TRUE} all columns with no predefined order
#'     is excluded from the data frame. This option is mainly
#'     intended for well defined and worked through routines like
#'     making selections lists for the Food Safety Authority. Do
#'     not use \code{exclude = TRUE} unless you are certain that
#'     all columns that should be included are defined in the
#'     column_standards table for this dbsource. If uncertain,
#'     you may first try with \code{exclude = FALSE} and thereafter
#'     compare with \code{exclude = TRUE} to check if you loose
#'     important information.
#'
#' @param data [\code{data.frame} | \code{character(1)}]\cr
#' The data source. If \code{property = "colclasses"} the path and
#'     file name of the csv-file used as data source should be given.
#' @param dbsource [\code{character(1)}]\cr
#' The database that is the source of data. Should be the name of
#'     the data source as registered in column_standards table. Defaults
#'     to \code{deparse(substitute(data))}.
#' @param standards [\code{data.frame} | \code{list} | \code{character(1)}]\cr
#' For giving alternatives to the standard table for column_standards using
#'     different formats, see details. Defaults to
#'     file.path(NVIdb::set_dir_NVI("ProgrammeringR", slash = FALSE),
#'               "standardization", "colnames", "column_standards.csv").
#' @param property [\code{character(1)}]\cr
#' Property of the column that should be standardized. Must be one
#'     of c("colnames", "colclasses", "collabels", "colwidths_Excel",
#'     "colorder"). Defaults to \code{NULL}.
#' @param language [\code{character(1)}]\cr
#' Language for labels. Must be one of c("no", "en"). Defaults to "no".
#' @param exclude [\code{logical(1)}]\cr
#' Used in combination with \code{property = "colorder"}. If \code{TRUE},
#'     all columns with no predefined column order are excluded.
#'     Defaults to \code{FALSE}.
#' @param \dots	Other arguments to be passed to
#'     \ifelse{html}{\code{\link[utils:read.csv2]{read.csv2}}}{\code{read.csv2}}
#'     when \code{property = "colclasses"}.
#'
#' @return \code{property = "colnames"}: A data frame with standard column names.
#'
#' \code{property = "colclasses"}: A named vector of column classes to
#'     be used as input to functions for reading csv-files, see details.
#'
#' \code{property = "collabels"}: A vector with labels for the columns
#'     in the data frame.
#'
#' \code{property = "colwidths_Excel"}: A vector with column widths for Excel.
#'      To be used as input parameter to
#'     \ifelse{html}{\code{\link[openxlsx:setColWidths]{openxlsx::setColWidths}}}{\code{openxlsx::setColWidths}}.
#'
#' \code{property = "colorder"}: A data frame with column names in predefined
#'     order. If \code{exclude = TRUE} only columns with a defined order is included.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
# @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' # Generate data frame to be standardized
#' df <- cbind("\u00C5r" = 2020, "Hensiktkode" = "01001", komnr = "5001")
#' colnames(df)
#'
#' # Standardize column names
#' df <- standardize_columns(data = df, property = "colnames")
#' colnames(df)
#'
#' # Generate vector with standard labels
#' labels <- standardize_columns(data = df, property = "collabels")
#' # use the labels as column names
#' colnames(df) <- labels
#'
#' # Generate vector with standard column widths for Excel
#' colwidths <- standardize_columns(data = df, property = "colwidths_Excel")
#' colwidths
#' }
#'
standardize_columns <- function(data,
                                dbsource = deparse(substitute(data)),
                                standards = file.path(NVIdb::set_dir_NVI("ProgrammeringR", slash = FALSE),
                                                      "standardization", "colnames", "column_standards.csv"),
                                property,
                                language = "no",
                                exclude = FALSE,
                                ...) {
  # TO DO: replace reading column standards with including column standards in sysdata for the package.

  # PREPARE ARGUMENTS BEFORE CHECKING ----
  if (is.null(standards)) {
    standards <- file.path(NVIdb::set_dir_NVI("ProgrammeringR", slash = FALSE),
                           "standardization", "colnames", "column_standards.csv")
  }

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  if (tolower(property) == "colclasses") {
    checkmate::assert_file_exists(data, access = "r", add = checks)
  } else {
    checkmate::assert_data_frame(data, add = checks)
  }

  # if dbsource is a vector with length more than one, e.g a filename made by paste
  if (tolower(property) == "colclasses") {
    dbsource <- dbsource[1]
  }
  checkmate::assert_character(dbsource, len = 1, min.chars = 1, add = checks)

  # standards
  # checkmate::assert_data_frame(standards, null.ok = TRUE, add = checks)
  checkmate::assert(checkmate::check_class(standards, classes = c("data.frame")),
                    checkmate::check_class(standards, classes = c("list")),
                    checkmate::check_class(standards, classes = c("character")),
                    add = checks)
  if (inherits(standards, what = "character")) {
    checkmate::assert_file_exists(standards, add = checks)
  }
  if (inherits(standards, what = "list")) {
    lengths_standard <- lengths(standards)
    NVIcheckmate::assert_integer(lengths_standard, lower = lengths_standard[1], upper = lengths_standard[1],
                                 min.len = 3, max.len = 6,
                                 comment = "When input as a list, all elements must have the same length",
                                 add = checks)
    checkmate::assert_subset(names(standards), choices = c("table_db", "colname_db", "colname", "collabel", "colwidth", "colorder"),
                             add = checks)
  }
  if (inherits(standards, what = "data.frame")) {
    checkmate::assert_data_frame(standards, min.rows = 1, min.cols = 6, add = checks)
  }
  # property
  checkmate::assert_subset(tolower(property),
                           choices = c("colnames", "colclasses",
                                       "collabels", "colwidths_excel",
                                       "colwidths_DT", "colorder"),
                           add = checks)
  # language
  checkmate::assert_subset(language, choices = c("no", "en"), add = checks)

  checkmate::assert_logical(exclude, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  property <- tolower(property)
  dbsource <- tolower(dbsource)

  if (inherits(standards, what = "character")) {
    column_standards <- utils::read.csv2(file = standards, fileEncoding = "UTF-8")
  }
  if (inherits(standards, what = "list")) {
    column_standards <- as.data.frame((standards))

    if (!"table_db" %in% colnames(column_standards)) {
      column_standards$table_db <- dbsource
    }

    if (!"colname_db" %in% colnames(column_standards)) {
      column_standards$colname_db <- column_standards$colname
    }

    if (!"colorder" %in% colnames(column_standards)) {
      column_standards$colorder <- c(1:dim(column_standards)[1])
    }
    colnames(column_standards)[which(colnames(column_standards) == "collabel")] <- "label_1_no"
    colnames(column_standards)[which(colnames(column_standards) == "colwidth")] <- "colwidth_Excel"
  }
  if (inherits(standards, what = "data.frame")) {
    column_standards <- standards
  }

  # CHANGE DATABASE VARIABLE NAMES INTO STANDARD COLUMN NAMES FOR USE IN DATA FRAMES ----
  if (property == "colnames") {

    column_values <- find_standard_values_for_property(column_names = colnames(data),
                                                       dbsource = dbsource,
                                                       org_values_in_column = "colname_db",
                                                       new_values_in_column = "colname",
                                                       standard = column_standards)


    # # Generate data frame with the column names in one column named V1
    # columnnames <- as.data.frame(matrix(colnames(data), ncol = 1))
    # # columnnames <- as.data.frame(list(new_column_value = colnames(data)))
    # # Generate column with original order of column names
    # #  Necessary to avoid change in order when using merge
    # columnnames$original_sort_order <- seq_len(nrow(columnnames))
    #
    # # standard <- column_standards |>
    # #   # Filter to include only information for relevant column names and with property information
    # #   dplyr::filter(.data$colname_db %in% columnnames$V1) |>
    # #   dplyr::filter(!is.na(.data$colname)) |>
    # #   dplyr::select(.data$table_db, .data$colname_db, .data$colname) |>
    # #   dplyr::distinct()
    # standard <- column_standards
    # # Filter to include only information for relevant column names and with property information
    # standard <- subset(standard, standard$colname_db %in% columnnames$V1)
    # standard <- subset(standard, !is.na(standard$colname))
    # standard <- standard[, c("table_db", "colname_db", "colname")]
    # # standard <- unique(standard)
    #
    # # Keep information on relevant table name and combine information for all other tables
    # standard[which(standard$table_db != dbsource), "table_db"] <- NA
    # standard <- unique(standard)
    #
    # if (dim(standard)[1] > 0) {
    #   # standard <- standard |>
    #   #   # Identify column names with only one suggested column width
    #   #   dplyr::add_count(.data$colname_db, name = "n") |>
    #   #   dplyr::ungroup() # |>
    #   # Identify column names with only one suggested column width
    #   aggregated_value <- stats::aggregate(stats::as.formula(paste("cbind(n = colname)",
    #                                                                "~",
    #                                                                paste(c("colname_db"), collapse = " + "))),
    #                                        data = standard,
    #                                        FUN = function(x) {length(x)})
    #   standard <- merge(x = standard, y = aggregated_value, by = "colname_db", all.x = TRUE)
    #   # # Select column width either if only one suggested or for the current table
    #   # dplyr::filter(.data$n == 1 | .data$table_db == dbsource & .data$n > 1) |>
    #   # dplyr::select(.data$colname_db, .data$colname) |>
    #   # dplyr::distinct()
    #   # Select column width either if only one suggested or for the current table
    #   standard <- subset(standard, standard$n == 1 | (standard$table_db == dbsource & standard$n > 1))
    #   standard <- standard[, c("colname_db", "colname")]
    #   standard <- unique(standard)
    # }
    #
    # # # Standardize column names
    # # if (dbsource %in% column_standards[which(column_standards$unique_colnames == 0), "table_db"]) {
    # #   stand_columnnames <- unique(column_standards[which(column_standards$unique_colnames == 0 & column_standards$table_db == dbsource),
    # #                                                c("colname_db", "colname")])
    # #   # New column with standard column names
    # #   columnnames <- merge(columnnames, stand_columnnames, by.x = "V1", by.y = "colname_db", all.x = TRUE)
    # #   columnnames[which(!is.na(columnnames$colname)), "V1"] <- columnnames[which(!is.na(columnnames$colname)), "colname"]
    # #   columnnames$colname <- NULL
    # #
    # # }
    # #
    # # stand_columnnames <- unique(column_standards[which(column_standards$unique_colnames == 1), c("colname_db", "colname")])
    #
    #
    # # New column with standard column names
    # columnnames <- merge(columnnames, standard, by.x = "V1", by.y = "colname_db", all.x = TRUE)
    # # Impute with snake case of column name in case standard column name isn't defined
    # columnnames[which(is.na(columnnames$colname)), "colname"] <-
    #   snakecase::to_snake_case(columnnames[which(is.na(columnnames$colname)), "V1"], transliterations = c("danish", "Latin-ASCII"))
    #
    # # Sorts data in original order
    # columnnames <- columnnames[order(columnnames$original_sort_order), ]
    #
    # vector with new column names
    # columnnames <- columnnames[, "colname"]

    # Impute with snake case of column name in case standard column name isn't defined
    column_values[which(is.na(column_values$colname)), "colname"] <-
      snakecase::to_snake_case(column_values[which(is.na(column_values$colname)), "V1"], transliterations = c("danish", "Latin-ASCII"))

    # vector with new column names
    column_values <- column_values[, "colname"]


    # Change source db column names to standard column names
    colnames(data) <- column_values

    # Return data frame with standardized column names
    return(data)
  }

  # READ FIRST LINE OF CSV-FILE, IDENTIFY COLUMN CLASSES AND PRODUCE A NAMED VECTOR FOR THE colclasses PARAMETER ----
  if (property == "colclasses") {
    # Read standard colclasses for database variable names
    stand_character <- unique(column_standards[which(!is.na(column_standards$colclasses)), c("colname_db", "colclasses")])

    # Identifies columns that can look like numbers but should be treated as characters, usually because of leading zero
    # Read first line of csv-file
    if (!exists("fileEncoding")) {
      fileEncoding <- "UTF-8"
    }
    colcharacter <- utils::read.csv2(file = data, header = FALSE, nrow = 1, fileEncoding = fileEncoding)
    # Transform the header into a data frame with one column
    colcharacter <- as.data.frame(matrix(colcharacter, ncol = 1))
    # Merge (inner join) to identify variable names with colclass definition
    colcharacter <- merge(stand_character, colcharacter, by.x = "colname_db", by.y = "V1")

    # Make a named vector for the colclasses parameter in read.csv2
    colcharacters <- colcharacter[, "colclasses"]
    names(colcharacters) <- colcharacter[, "colname_db"]

    # Return a named vector
    return(colcharacters)
  }

  # STANDARDIZE COLLABELS ----
  if (property == "collabels") {
    # Generate data frame with the column names in one column named V1
    collabels <- as.data.frame(matrix(colnames(data), ncol = 1))
    # Generate column with original order of column names
    #  Necessary to avoid change in order when using merge
    collabels$original_sort_order <- seq_len(nrow(collabels))

    ## Norwegian column labels ----
    # Standard labels in Norwegian is always generated as is used to impute missing labels in other languages
    # standard <- column_standards |>
    #   # Filter to include only information for relevant column names and with property information
    #   dplyr::filter(.data$colname %in% collabels$V1) |>
    #   dplyr::filter(!is.na(.data$label_1_no)) |>
    #   dplyr::select(.data$table_db, .data$colname, .data$label_1_no) |>
    #   dplyr::distinct()
    standard <- column_standards
    # Filter to include only information for relevant column names and with property information
    standard <- subset(standard, standard$colname %in% collabels$V1)
    standard <- subset(standard, !is.na(standard$label_1_no))
    standard <- standard[, c("table_db", "colname", "label_1_no")]
    # standard <- unique(standard)

    # Keep information on relevant table name and combine information for all other tables
    standard[which(standard$table_db != dbsource), "table_db"] <- NA
    standard <- unique(standard)

    if (dim(standard)[1] > 0) {
      # standard <- standard |>
      #   # Identify column names with only one suggested column width
      #   dplyr::add_count(.data$colname, name = "n") |>
      #   dplyr::ungroup() # |>
      aggregated_value <- stats::aggregate(stats::as.formula(paste("cbind(n = label_1_no)",
                                                                   "~",
                                                                   paste(c("colname"), collapse = " + "))),
                                           data = standard,
                                           FUN = function(x) {length(x)})
      standard <- merge(x = standard, y = aggregated_value, by = "colname", all.x = TRUE)
      # Select column width either if only one suggested or for the current table
      # dplyr::filter(.data$n == 1 | (.data$table_db == dbsource & .data$n > 1)) |>
      # dplyr::select(.data$colname, label = .data$label_1_no) |>
      # dplyr::distinct()
      standard <- subset(standard, standard$n == 1 | (standard$table_db == dbsource & standard$n > 1))
      standard <- standard[, c("colname", "label_1_no")]
      colnames(standard) <- c("colname", "label")
      standard <- unique(standard)

    }

    ## English column labels ----
    if (language == "en") {
      standard_en <- column_standards # |>
      # dplyr::filter(.data$colname %in% collabels$V1) |>
      # dplyr::filter(!is.na(.data$label_1_en)) |>
      # dplyr::select(.data$table_db, .data$colname, .data$label_1_en) |>
      # dplyr::distinct()
      # Filter to include only information for relevant column names and with property information
      standard_en <- subset(standard_en, standard_en$colname %in% collabels$V1)
      standard_en <- subset(standard_en, !is.na(standard_en$label_1_en))
      standard_en <- standard_en[, c("table_db", "colname", "label_1_en")]
      standard_en <- unique(standard_en)

      # Keep information on relevant table name and combine information for all other tables
      standard_en[which(standard_en$table_db != dbsource), "table_db"] <- NA
      standard_en <- unique(standard_en)

      if (dim(standard_en)[1] > 0) {
        # standard_en <- standard_en |>
        #   # Identify column names with only one suggested column width
        #   dplyr::add_count(.data$colname, name = "n") |>
        #   dplyr::ungroup() # |>
        aggregated_value <- stats::aggregate(stats::as.formula(paste("cbind(n = label_1_en)",
                                                                     "~",
                                                                     paste(c("colname"), collapse = " + "))),
                                             data = standard_en,
                                             FUN = function(x) {length(x)})
        standard_en <- merge(x = standard_en, y = aggregated_value, by = "colname", all.x = TRUE)
        # dplyr::filter(.data$n == 1 | (.data$table_db == dbsource & .data$n > 1)) |>
        # dplyr::select(.data$colname, .data$label_1_en) |>
        # dplyr::distinct()
        standard_en <- subset(standard_en, standard_en$n == 1 | (standard_en$table_db == dbsource & standard_en$n > 1))
        standard_en <- standard_en[, c("colname", "label_1_en")]
        standard_en <- unique(standard_en)
      }

      # Impute missing labels with Norwegian labels
      # standard <- standard_en |>
      #   dplyr::full_join(standard, by = c("colname" = "colname")) |>
      #   dplyr::mutate(label = dplyr::coalesce(.data$label_1_en, .data$label)) |>
      #   dplyr::select(.data$colname, .data$label)
      standard <- merge(x = standard, y = standard_en, by = "colname", all.x = TRUE)
      standard[which(!is.na(standard$label_1_en)), "label"] <- standard[which(!is.na(standard$label_1_en)), "label_1_en"]
      standard <- standard[, c("colname", "label")]
    }

    ## Impute Sentence case for those without defined label ----¨
    collabels <- merge(collabels, standard, by.x = "V1", by.y = "colname", all.x = TRUE)
    # Impute with Sentence case of column name in case standard column name isn't defined
    collabels[which(is.na(collabels$label)), "label"] <-
      snakecase::to_sentence_case(collabels[which(is.na(collabels$label)), "V1"],
                                  transliterations = c("aa" = "\u00e5", "Aa" = "\u00e5",
                                                       "AA" = "\u00e5", "aA" = "\u00e5",
                                                       "oe" = "\u00f8", "Oe" = "\u00f8",
                                                       "OE" = "\u00f8", "oE" = "\u00f8",
                                                       "ae" = "\u00e6", "Ae" = "\u00e6",
                                                       "AE" = "\u00e6", "aE" = "\u00e6")
      )

    ## Make vector with column labels
    # Sorts data in original order
    collabels <- collabels[order(collabels$original_sort_order), ]

    # vector with column labels
    collabels <- collabels[, "label"]

    # Return data frame with standardized column names
    return(collabels)
  }

  # STANDARDIZE COLUMN WIDTHS FOR EXCEL ----
  if (property == "colwidths_excel") {

    column_values <- find_standard_values_for_property(column_names = colnames(data),
                                                       dbsource = dbsource,
                                                       org_values_in_column = "colname",
                                                       new_values_in_column = "colwidth_Excel",
                                                       standard = column_standards)

    # # Generate data frame with the column names in one column named V1
    # colwidths <- as.data.frame(matrix(colnames(data), ncol = 1))
    # # Generate column with original order of column names
    # #  Necessary to avoid change in order when using merge
    # colwidths$original_sort_order <- seq_len(nrow(colwidths))
    #
    # # column_standards$dbsource <- dbsource
    # # print(head(column_standards))
    #
    # # Standardize colwidths
    # # standard <- column_standards |>
    # #   # Filter to include only information for relevant column names and with property information
    # #   dplyr::filter(.data$colname %in% colwidths$V1) |>
    # #   dplyr::filter(!is.na(.data$colwidth_Excel)) |>
    # #   dplyr::select(.data$table_db, .data$colname, colwidth = .data$colwidth_Excel)
    # # dplyr::distinct()
    # standard <- column_standards
    # # Filter to include only information for relevant column names and with property information
    # standard <- subset(standard, standard$colname %in% colwidths$V1)
    # standard <- subset(standard, !is.na(standard$colwidth_Excel))
    # standard <- standard[, c("table_db", "colname", "colwidth_Excel")]
    # colnames(standard) <- c("table_db", "colname", "colwidth")
    # # Keep information on relevant table name and combine information for all other tables
    # standard[which(standard$table_db != dbsource), "table_db"] <- NA
    # standard <- unique(standard)
    #
    # # if there are information on column widths
    # if (dim(standard)[1] > 0) {
    #   # standard <- standard |>
    #   #   # Identify column names with only one suggested column width
    #   #   dplyr::add_count(.data$colname, name = "n") |>
    #   #   dplyr::ungroup() # |>
    #   aggregated_value <- stats::aggregate(stats::as.formula(paste("cbind(n = colwidth)",
    #                                                                "~",
    #                                                                paste(c("colname"), collapse = " + "))),
    #                                        data = standard,
    #                                        FUN = function(x) {length(x)})
    #   standard <- merge(x = standard, y = aggregated_value, by = "colname", all.x = TRUE)
    #   # # Select column width either if only one suggested or for the current table
    #   # dplyr::filter(.data$n == 1 | .data$table_db == dbsource & .data$n > 1) |>
    #   # dplyr::select(.data$colname, .data$colwidth) |>
    #   # dplyr::distinct()
    #   # Select column width either if only one suggested or for the current table
    #   standard <- subset(standard, standard$n == 1 | (standard$table_db == dbsource & standard$n > 1))
    #   standard <- standard[, c("colname", "colwidth")]
    #   standard <- unique(standard)
    # }
    #
    # # New column with standard column names¨
    # colwidths <- merge(colwidths, standard, by.x = "V1", by.y = "colname", all.x = TRUE)
    # # Impute with snake case of column name in case standard column name isn't defined
    #
    # # Sorts data in original order
    # colwidths <- colwidths[order(colwidths$original_sort_order), ]
    #
    # vector with new column names
    column_values <- column_values[, "colwidth_Excel"]
    column_values[which(is.na(column_values))] <- 10.71

    # Return data frame with standardized column names
    return(column_values)
  }

  # STANDARDIZE COLUMN ORDER ----
  if (property == "colorder") {
    if (!dbsource %in% column_standards[which(!is.na(column_standards$colorder)), "table_db"]) {
      warning("No sorting done as column order is not known for this table. Please update column_standards or use another dbsource")
    } else {
      # Generate data frame with the column names in one column named V1
      columnorder <- as.data.frame(matrix(colnames(data), ncol = 1))
      # Generate column with original order of column names
      #  Necessary to avoid change in order when using merge
      columnorder$original_sort_order <- seq_len(nrow(columnorder))

      ## Norwegian column labels ----
      # Standard labels in Norwegian is always generated as is used to impute missing labels in other languages
      standard <- column_standards # |>
      # Filter to include only information for relevant column names and with property information
      # dplyr::filter(.data$table_db == dbsource) |>
      # dplyr::filter(.data$colname %in% columnorder$V1) |>
      # dplyr::filter(!is.na(.data$colorder)) |>
      # dplyr::select(.data$colname, .data$colorder) |>
      # dplyr::distinct() |>

      standard <- subset(standard, standard$table_db == dbsource)
      standard <- subset(standard, standard$colname %in% columnorder$V1)
      standard <- subset(standard, !is.na(standard$colorder))
      standard <- standard[, c("colname", "colorder")]
      standard <- unique(standard)

      # removes colorders with more than suggested position
      # standard <- standard |>
      #   dplyr::add_count(.data$colname, name = "n") |>
      #   dplyr::ungroup() # |>
      aggregated_value <- stats::aggregate(stats::as.formula(paste("cbind(n = colorder)",
                                                                   "~",
                                                                   paste(c("colname"), collapse = " + "))),
                                           data = standard,
                                           FUN = function(x) {length(x)})
      standard <- merge(x = standard, y = aggregated_value, by = "colname", all.x = TRUE)
      # dplyr::filter(.data$n == 1) |>
      # dplyr::select(.data$colname, .data$colorder) |>
      # # Sort according to first column
      # dplyr::arrange(.data$colorder)

      standard <- subset(standard, standard$n == 1)
      standard <- standard[, c("colname", "colorder")]
      # Sort according to colorder
      standard <- standard[order(standard$colorder), ]

      # Order in accord with standard.
      # Keep non-ordered columns in last columns if exclude = FALSE
      if (exclude == FALSE) {
        colorder <- c(standard$colname, base::setdiff(columnorder$V1, standard$colname))
        # okweb[, ] <- NA
        # okweb <- okweb[, c(OK_kolonner, base::setdiff(colnames(okweb), OK_kolonner))]
      }
      # Exclude non-ordered columns if exclude = TRUE
      if (exclude == TRUE) {
        colorder <- c(standard$colname)
      }

      # Change order of columns and eventually exclude non-selected columns
      data <- data[, colorder]
    }
    # Return data frame with standardized column names
    return(data)
  }


  # LAST PROPERTY ASSIGNED ----
}



### find_standard_values_for_property ----
#' @title Find standard column values for given property
#' @description Ensures that all elements in a vector are named.
#' @details Sub-function called by standardize columns to standardise code.
#'
#' @param column_names [\code{character}]\cr
#' The column names of the data source. Defaults to \code{colnames(data)}.
#' @param dbsource [\code{character(1)}]\cr
#' The database that is the source of data. Should be the name of
#'     the data source as registered in column_standards table. Defaults
#'     to \code{dbsource}.
#' @param org_values_in_column [\code{character(1)}]\cr
#' The column name in the standards table in which the column names for which
#'     one will find the property values. Must be one
#'     of c("colname_db", "colname"). Defaults to \code{NULL}.
#' @param new_values_in_column [\code{character(1)}]\cr
#' The column name in the standards table in which one will find the property
#'     values. Must be one of c("colname", "colclass", "collabel", "colwidths_Excel",
#'     "colorder"). Defaults to \code{NULL}.
#' @param standard [\code{data.frame}]\cr
#' Table with column_standards. Defaults to column_standards.
#' @param require_table [\code{logical(1)}]\cr
#' If \code{TRUE}, only registrations in column_standards where table_db equals 
#'     \code{dbsource} are included. Defaults to \code{FALSE}.

#' @return A named vector where previously unnamed elements have been named with
#'     the element value as name.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @keywords internal

find_standard_values_for_property <- function(column_names,
                                              dbsource,
                                              org_values_in_column = NULL,
                                              new_values_in_column = NULL,
                                              standard,
                                              require_table = FALSE) {

  # Generate data frame with the column names in one column named V1
  column_names <- as.data.frame(matrix(column_names, ncol = 1))
  # column_names <- as.data.frame(matrix(colnames(data), ncol = 1))
  # Generate column with original order of column names
  #  Necessary to avoid change in order when using merge
  column_names$original_sort_order <- seq_len(nrow(column_names))

  # Filter to include only information for relevant column names and with property information
  standard <- standard[which(standard[, org_values_in_column] %in% column_names$V1), ] # column_names[, property]), ]
  # standard <- subset(standard, standard$colname_db %in% columnnames$V1)
  standard <- standard[!is.na(standard[, new_values_in_column]), ]
  # standard <- subset(standard, !is.na(standard$colname))
  standard <- standard[, c("table_db", org_values_in_column, new_values_in_column)]
  # standard <- standard[, c("table_db", "colname_db", "colname")]
  # standard <- unique(standard)
  if (isTRUE(require_table)) {
    standard <- standard[which(standard[, "table_db"] == dbsource), ]
  }

  # Keep information on relevant table name and combine information for all other tables
  standard[which(standard$table_db != dbsource), "table_db"] <- NA
  standard <- unique(standard)

  if (dim(standard)[1] > 0) {
    # Identify column names with only one suggested value
    aggregated_value <- stats::aggregate(stats::as.formula(paste("cbind(n = ", new_values_in_column, ")",
                                                                 "~",
                                                                 paste(c(org_values_in_column), collapse = " + "))),
                                         data = standard,
                                         FUN = function(x) {length(x)})
    standard <- merge(x = standard, y = aggregated_value, by = org_values_in_column, all.x = TRUE)
    # # Select column width either if only one suggested or for the current table
    # Select column width either if only one suggested or for the current table
    standard <- subset(standard, standard$n == 1 | (standard$table_db == dbsource & standard$n > 1))
    standard <- standard[, c(org_values_in_column, new_values_in_column)]
    standard <- unique(standard)
  }
  # New column with standard column names
  column_values <- merge(column_names, standard, by.x = "V1", by.y = org_values_in_column, all.x = TRUE)

  # Sorts data in original order
  column_values <- column_values[order(column_values$original_sort_order), ]


  return(column_values)
}
