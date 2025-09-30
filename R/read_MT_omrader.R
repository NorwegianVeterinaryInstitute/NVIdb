#' @export
#' @rdname add_MT_omrader

read_MT_omrader <- function(filename = list("komnr_2_MT_enhet.csv"),
                            from_path = file.path(set_dir_NVI("GrunndataLand", slash = FALSE), 
                                                  "FormaterteData"),
                            year = format(Sys.Date(), "%Y"),
                            ...) {
  
  # PREPARE INPUT BEFORE CHECKING
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  year <- as.numeric(year)
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checks <- assert_read_functions(filename = filename, from_path = from_path, add = checks)
  # year
  checks <- checkmate::assert_integerish(year, 
                                         lower = 2000, 
                                         upper = as.numeric(format(Sys.Date(), "%Y")))
  # Report check-results
  checkmate::reportAssertions(checks)
  
  # READ DATA ----
  if (year <= 2024){
    komnr_2_MT_avdeling <- utils::read.csv2(file = file.path(from_path, filename[[1]]),
                                            colClasses = "character",
                                            fileEncoding = "UTF-8",
                                            ...)
    
    komnr_2_MT_avdeling <- komnr_2_MT_avdeling[, c("kommuneidentifikator", "forekomstidentifikator")]
    colnames(komnr_2_MT_avdeling) <- c("komnr", "MT_avdelingnr")
    komnr_2_MT_avdeling$MT_regionnr <- paste0(substr(komnr_2_MT_avdeling$MT_avdelingnr, 1, 3), "000")
    
    # MT_omrader <- read_csv_file(filename = filename[[2]],
    #                             from_path = from_path,
    #                             options = list(colClasses = "character", fileEncoding = "UTF-8"))
    MT_omrader <- utils::read.csv2(file = file.path(from_path, filename[[2]]),
                                   colClasses = "character",
                                   fileEncoding = "UTF-8",
                                   ...)
    
    MT_omrader <- MT_omrader[, c("MT_IDnr", "MT_navn")]
    
    komnr_2_MT_avdeling <- merge(komnr_2_MT_avdeling,
                                 MT_omrader,
                                 by.x = "MT_avdelingnr",
                                 by.y = "MT_IDnr",
                                 all.x = TRUE)
    
    colnames(komnr_2_MT_avdeling)[which(colnames(komnr_2_MT_avdeling) == "MT_navn")] <- "MT_avdeling"
    
    
    komnr_2_MT_avdeling <- merge(komnr_2_MT_avdeling,
                                 MT_omrader,
                                 by.x = "MT_regionnr",
                                 by.y = "MT_IDnr",
                                 all.x = TRUE)
    
    colnames(komnr_2_MT_avdeling)[which(colnames(komnr_2_MT_avdeling) == "MT_navn")] <- "MT_region"
    
    komnr_2_MT_omrader <- komnr_2_MT_avdeling[, c("komnr", "MT_avdelingnr", "MT_avdeling", "MT_regionnr", "MT_region")]
  }
  
  if (year >= 2025) {
    komnr_2_MT_omrader <- utils::read.csv2(file = file.path(from_path, filename[[1]]),
                                            colClasses = "character",
                                            fileEncoding = "UTF-8",
                                            ...)
    
  }
  return(komnr_2_MT_omrader)
}
