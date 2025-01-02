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
#'     The month for which the register should be read. Defaults to \code{NULL}.
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
                          month = NULL,
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
  
  
  
  # READ IN ALL FILES IN THE DIRECTORY AND MAKE A LIST OF THE SELECTED VERSIONS OF EXTRACTS FROM PKODEREGISTERET
  filelist <- select_files_for_date(from_path = from_path,
                                    filename_text = c("avlsgris", "11"),
                                    file_extension = "csv",
                                    year = as.character(year),
                                    month = month,
                                    extracted_date = NULL)
  
  # Read data for the selected year and months from Pkoderegisteret and
  # combine into one data frame
  # Check if any version of the register was found and give an ERROR if not
  NVIcheckmate::assert_data_frame(filelist, min.rows = 1,
                                  comment = paste("No versions of Produksjonstilskudd available for year",
                                                  Pkode_year,
                                                  "and month",
                                                  Pkode_month,
                                                  "."))
  for (i in 1:dim(filelist)[1]) {
    
    # Identifies column names with fylke, kommune and prodnr
    # thereby these are flexible if input files changes.
    colchar <- utils::read.csv2(file.path(set_dir_NVI("Prodtilskudd"), "FormaterteData", filelist[i, "filename"]),
                                header = FALSE,
                                nrow = 1,
                                fileEncoding = "UTF-8")
    
    colchar <- colchar[which(regexpr("kom", colchar, ignore.case = TRUE) > 0 |
                               regexpr("fylk", colchar, ignore.case = TRUE) > 0 |
                               regexpr("prodn", colchar, ignore.case = TRUE) > 0)]
    
    colchars <- as.vector(rep("character", length(colchar)))
    names(colchars) <- colchar
    
    # read single files
    # tempdf <- read_csv_file(filename = filelist[i, "filename"],
    #                         from_path = from_path,
    #                         options = list(colClasses = colchars,
    #                                        fileEncoding = "UTF-8"))
    tempdf <- data.table::fread(file = file.path(from_path, filelist[i, "filename"]),
                                colClasses = colchars,
                                encoding = "UTF-8",
                                showProgress = FALSE,
                                data.table = FALSE,
                                ...)
    if (exists("df1")) {
      df1[setdiff(names(tempdf), names(df1))] <- NA
      tempdf[setdiff(names(df1), names(tempdf))] <- NA
      df1 <- rbind(df1, tempdf)
    } else {
      df1 <- tempdf
    }
  }
  
  # TO DO: COMBINE SPRING AND AUTOMN INTO ONE FILE IF month = "both"
  
  # Standardize column names
  # To be replaced by standardizing column names in the source files
  columnnames <- colnames(df1)
  columnnames <- sub("Prodnr", "prodnr", columnnames)
  colnames(df1) <- columnnames
  
  # Return dataframe with data for all selected year and months
  return(df1)
}

###   ----

### select_prodtilskudd_files ----


#' @title List selected files from Søknad om register for produksjonstilskudd
#' @description List selected files with extracts from Søknad om register
#'     for produksjonstilskudd.
#' @details Reads the filenames of files with extracts from Søknad om register
#'     for produksjonstilskudd into a data frame. The function gives options to
#'     select year and month and path for the files. The function is called from
#'     \code{read_Prodtilskudd} and \code{copy_Prodtilskudd}.
#'
#' @param from_path Path for the source translation table for PJS-codes
#' @param Pkode_year The year(s) from which the register should be read. Options is "last", or a vector with one or more years.
#' @param Pkode_month the month for which the register should be read. The options are c("05", "10", "both", "last") for Pkode_year = 2017
#'     and c("03", "10", "both", "last") for Pkode_year >= 2018.
#'
#' @return A data frame with filenames of the files with the selected extracts of Prodtilskudd.
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

select_files_for_date <- function(from_path,
                                  filename_text,
                                  file_extension,
                                  year,
                                  month,
                                  day = NULL,
                                  extracted_date = NULL) {
  
  # READ ALL FILES IN DIRECTORY WITH A FILENAME IN ACCORD WITH filename_text AND file_extension
  # Read filelist
  filelist <- list.files(path = from_path, 
                                       pattern = filename_text[1], 
                                       ignore.case = TRUE, 
                                       include.dirs = FALSE)
  # Select if more criteria than one in filename
  if (length(filename_text) > 1){
    for (text in filename_text[2:length(filename_text)]) {
      filelist <- filelist[grepl(pattern = text, x = filelist, ignore.case = TRUE)]
    }
  }
  filelist <- data.frame("filename" = filelist)
  # Select extension
  filelist$extension <- tools::file_ext(filelist$filename)
  filelist <- subset(filelist, filelist$extension %in% file_extension)
  
  # IDENTIFY YEAR, MONTH AND DATE IN FILENAME
  filelist$position <- gregexpr(pattern = "[_[:space:]]20", filelist[, "filename"])
  
  
  #  FUNKER IKKE strcapture("\\d{4}", x = year,  proto = filelist)
  
  # MAKE A LIST OF THE LAST VERSION OF ALL UTREKK FRO PKODEREGISTERET
  filelist$fileinfo <- sub("per ", "", filelist$filename)
  filelist$fileinfo <- sub("fra ", "", filelist$fileinfo)
  filelist$fileinfo <- sub(".csv", "", filelist$fileinfo)
  filelist$pkodeaar <- substr(filelist$fileinfo, 6, 9)
  filelist$pkodemonth <- substr(filelist$fileinfo, 10, 11)
  filelist$content <- sapply(filelist$fileinfo, FUN = find_n_th_word, position = 2)
  filelist <- subset(filelist,
                     tolower(substr(filelist$fileinfo, 1, 5)) == "pkode" &
                       filelist$pkodeaar > "1994" & filelist$pkodeaar < "2099" &
                       filelist$content %in% c("Koordinater", "Uttrekk"))
  filelist$contenttype <- sapply(filelist$fileinfo, FUN = find_n_th_word, position = 4)
  filelist <- subset(filelist, filelist$contenttype == "UTF8")
  
  filelist$uttrekk_dato <- as.Date(sapply(filelist$fileinfo, FUN = find_n_th_word, position = 3), format = "%Y%m%d")
  max_uttrekk_dato <- stats::aggregate(filelist$uttrekk_dato, by = list(filelist$pkodeaar, filelist$pkodemonth), FUN = max)
  filelist <- merge(filelist, max_uttrekk_dato, by.x = c("pkodeaar", "pkodemonth"), by.y = c("Group.1", "Group.2"))
  filelist <- filelist[, c("filename", "pkodeaar", "pkodemonth", "uttrekk_dato", "x")]
  filelist <- filelist[order(filelist$pkodeaar, filelist$pkodemonth, filelist$uttrekk_dato, decreasing = TRUE), ]
  
  if (is.null(extracted_date)) {
    filelist <- subset(filelist, filelist$uttrekk_dato == filelist$x)
    if ("last" %in% Pkode_year) {
      filelist <- filelist[c(1:2), ]
      if (!"both" %in% Pkode_month) {
        if ("last" %in% Pkode_month) {
          filelist <- filelist[1, ]
        } else {
          filelist <- subset(filelist, filelist$pkodemonth %in% Pkode_month)
        }
      }
    }
    if (!"last" %in% Pkode_year) {
      filelist <- subset(filelist, filelist$pkodeaar %in% Pkode_year)
      if (!"both" %in% Pkode_month) {
        if ("last" %in% Pkode_month) {
          filelist <- filelist[1, ]
        } else {
          filelist <- subset(filelist, filelist$pkodemonth %in% Pkode_month)
        }
      }
    }
  }
  # Selection for uttrekk_dato
  if (!is.null(extracted_date)) {
    filelist <- subset(filelist, filelist$pkodeaar %in% Pkode_year)
    filelist <- subset(filelist, filelist$pkodemonth %in% Pkode_month)
    checkmate::assert_choice(as.Date(extracted_date), choices = filelist$uttrekk_dato)
    filelist <- subset(filelist, filelist$uttrekk_dato %in% as.Date(extracted_date))
  }
  
  return(filelist)
}
