#' @export
#' @rdname add_MT_omrader

read_MT_omrader <- function(filename = list("komnr_2_MT_avdeling.csv", "MT_omrader.csv", "komnr_2_MT_enhet.csv"),
                            from_path = file.path(set_dir_NVI("GrunndataLand", slash = FALSE), 
                                                  "FormaterteData"),
                            # year = format(Sys.Date(), "%Y"),
                            ...) {
  
  # PREPARE INPUT BEFORE CHECKING ----
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checks <- assert_read_functions(filename = filename, from_path = from_path, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
  # READ DATA ----
  ## Read MT-omrader for 2004 - 2024 (MT-regioner and MT-avdeling)
  komnr_2_MT_avdeling <- utils::read.csv2(file = file.path(from_path, filename[[1]]),
                                          colClasses = "character",
                                          fileEncoding = "UTF-8",
                                          ...)
  
  komnr_2_MT_avdeling <- komnr_2_MT_avdeling[, c("kommuneidentifikator", "forekomstidentifikator")]
  colnames(komnr_2_MT_avdeling) <- c("komnr", "MT_avdelingnr")
  komnr_2_MT_avdeling$MT_regionnr <- paste0(substr(komnr_2_MT_avdeling$MT_avdelingnr, 1, 3), "000")
  
  MT_omrader <- utils::read.csv2(file = file.path(from_path, filename[[2]]),
                                 colClasses = "character",
                                 fileEncoding = "UTF-8",
                                 ...)
  
  ### Include names
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
  
  ### Include from_year and to_year
  komnr_2_MT_avdeling$from_year <- 2004
  komnr_2_MT_avdeling$to_year <- 2024
  
  
  ## Read MT-omrader for 2025 - today (MT-divisjon, MT-avdeling, and MT-enhet)
  komnr_2_MT_enhet <- utils::read.csv2(file = file.path(from_path, filename[[3]]),
                                       colClasses = "character",
                                       fileEncoding = "UTF-8",
                                       ...)
  
  komnr_2_MT_enhet$from_year <- 2025
  komnr_2_MT_enhet$to_year <- NA
  
  ### Standardize names. Can rewrite using NVIdb::standardize_columns
  colnames(komnr_2_MT_enhet)[which(colnames(komnr_2_MT_enhet) == "Avdeling_kode")] <- "MT_avdelingnr"
  colnames(komnr_2_MT_enhet)[which(colnames(komnr_2_MT_enhet) == "Avdeling")] <- "MT_avdeling"
  colnames(komnr_2_MT_enhet)[which(colnames(komnr_2_MT_enhet) == "Divisjon_kode")] <- "MT_divisjonnr"
  colnames(komnr_2_MT_enhet)[which(colnames(komnr_2_MT_enhet) == "Divisjon")] <- "MT_divisjon"
  colnames(komnr_2_MT_enhet)[which(colnames(komnr_2_MT_enhet) == "Enhet_kode")] <- "MT_enhetnr"
  colnames(komnr_2_MT_enhet)[which(colnames(komnr_2_MT_enhet) == "Enhet")] <- "MT_enhet"
  colnames(komnr_2_MT_enhet)[which(colnames(komnr_2_MT_enhet) == "Produkt_fag")] <- "produkt_fag"
  
  
  # COMBINE FOR BOTH PERIODS ----
  komnr_2_MT_omrader <- merge(komnr_2_MT_avdeling, komnr_2_MT_enhet,
                              by = c("from_year", "to_year", "komnr", 
                                     "MT_avdelingnr", "MT_avdeling"),
                              all = TRUE)
  
  
  # Standardize order Can rewrite using NVIdb::standardize_columns
  komnr_2_MT_omrader <- komnr_2_MT_omrader[, c("from_year", "to_year", "komnr", 
                                               "MT_divisjonnr", "MT_divisjon", 
                                               "MT_regionnr", "MT_region", 
                                               "MT_avdelingnr", "MT_avdeling",
                                               "MT_enhetnr", "MT_enhet",
                                               "produkt_fag")]
  # RETURN DATA ----
  return(komnr_2_MT_omrader)
}
