# library(NVIdb)
library(testthat)

test_that("Standardize colnames from PJS", {
  # Generate data frame with selected column names from V1_SAK_M_RES_EIER
  # data is fom saksnr 2018-01-1557
  df <- as.data.frame(cbind("aar" = 2018, "ansvarlig_seksjon" = "01", "innsendelsesnummer" = 1557,
                            "mottatt_dato" = "15.05.18", "uttaksdato" = "11.05.18", "sak_avsluttet" = "16.05.18",
                            "kommunenr" = NA, "landnr" = NA,
                            "driftsformkode" = NA, "artkode" = "03090101001016", "provematerialekode" = "05002", "forbehandlingkode" = NA,
                            "provetypekode" = "01", "fys_stad_kode" = NA, "alder" = NA,
                            "uttaksdato_parprove" = NA, "mottatt_dato_parprove" = NA,
                            "id_nr" = NA, "id_nr_type" = NA,
                            "undersokelsesnummer" = 1, "metodekode" = "020002", "godkjent_dato" = "16.05.18", "avsluttet_dato" = "16.05.18",
                            "resultatnummer" = 1, "analyttkode_funn" = "0416010103", "kjennelse_resultat" = "0201",
                            "konklusjonsnummer" = 1, "konkl_provenummer" = 1, "konklusjonkode" = "0201", "konkl_analyttkode" = "0416010103", "konklusjonstype" = 1,
                            "NAVN" = "EJ", "POSTNR" = "2410"))


  # Make a vector with correct column names after translation
  correct_result <- c("aar", "ansvarlig_seksjon", "innsendelsenr", "mottatt", "uttatt", "avsluttet",
                      "komnr", "landnr",
                      "driftsformkode", "artkode", "provematerialekode", "forbehandlingkode", "provetypekode", "fysiologisk_stadiumkode", "alder",
                      "uttatt_parprove", "mottatt_parprove", "id_nr", "id_nr_type",
                      "undnr", "metodekode", "und_godkjent", "und_avsluttet",
                      "resnr", "res_analyttkode", "res_kjennelsekode",
                      "konklnr", "konkl_provenr", "konkl_kjennelsekode", "konkl_analyttkode", "konkl_typekode",
                      "eier_lokalitet", "postnr")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(colnames(standardize_columns(data = df,
                                                property = "colnames")),
                   correct_result)

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(colnames(standardize_columns(data = df,
                                                standards = file.path(NVIdb::set_dir_NVI("ProgrammeringR", slash = FALSE),
                                                                      "standardization", "colnames", "column_standards.csv"),
                                                property = "colnames")),
                   correct_result)

})


test_that("Standardize colnames from EOS scrapie", {
  # Generate data frame with selected column names from V1_SAK_M_RES_EIER
  # data is fom saksnr 2018-01-1557
  df <- as.data.frame(cbind("id" = 1, "hensiktkode" = "0200124002", "hensikt" = "Scrapie – normalslakt",
                            "mottatt_dato" = "2019-02-27 00:00:00", "uttaks_dato_inns" = "2019-02-26 00:00:00",
                            "fodselsdato" = NA, "skrottnr" = "210861",
                            "aar" = "2019", "ansvarlig_seksjon" = "04", "innsendelsesnummer" = "6832", "saksnr" = "2019-04-6832/SC 1437",
                            "eier_type" = "PROD", "eier_navn" = "EO", "eier_nummer" = "###########", "annen_akt\u00F8r_type" = "AUTO", "annen_akt\u00F8r_navn" = "Slakthuset Eidsmo Dullum AS avd Oppdal",
                            "id_nr" = "60203", "art" = "Sau", "kjennelse" = "Ikke p\u00E5vist", "sist_oppdatert" = "2020-10-06 01:30:03.427", "annen_akt\u00F8r_nr" = "123",
                            "rekvirent_type" = "MTA", "rekvirent_nr" = "M22110", "rekvirent" = "Gl\u00E5mdal og \u00D8sterdal", "avvik_i_registrering" = "0", "antall_und_prover" = "1"))


  # Make a vector with correct column names after translation
  correct_result <- c("lopenr", "hensiktkode", "hensikt", "mottatt", "uttatt",
                      "fodselsdato", "skrottnr",
                      "aar", "ansvarlig_seksjon", "innsendelsenr", "saksnr",
                      "eier_lokalitettype", "eier_lokalitet", "eier_lokalitetnr", "annen_aktortype", "annen_aktor",
                      "id_nr", "art", "scrapie", "sist_overfort", "annen_aktornr",
                      "rekvirenttype", "rekvirentnr", "rekvirent", "avvik_i_registrering", "ant_und_prover")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(colnames(standardize_columns(data = df,
                                                dbsource = "proveresultat_scrapie",
                                                property = "colnames")),
                   correct_result)

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(colnames(standardize_columns(data = df,
                                                dbsource = "proveresultat_scrapie",
                                                standards = file.path(NVIdb::set_dir_NVI("ProgrammeringR", slash = FALSE),
                                                                      "standardization", "colnames", "column_standards.csv"),
                                                property = "colnames")),
                   correct_result)

})



test_that("colClasses for csv-files", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Prodtilskudd
  colclasses <- standardize_columns(data = paste0(set_dir_NVI("Prodtilskudd"), "FormaterteData/Pkode20191001 Uttrekk per 20200624 UTF8.csv"), property = "colclasses")

  # Make a vector with correct column names after translation
  correct_result <- c("Epost" = "character", "gjeldende_fylke" = "character", "gjeldende_fylkenr" = "character",
                      "gjeldende_kommune" = "character", "gjeldende_komnr" = "character", "gjeldende_prodnr8" = "character", "komnr" = "character",
                      "Mobilnummer" = "character", "Orgnr" = "character", "prodnr10" = "character",
                      "prodnr8" = "character", "Telledato" = "character")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_equivalent(colclasses,
                    correct_result)

  # Pkode_2_text
  colclasses <- standardize_columns(data = paste0(set_dir_NVI("Prodtilskudd"), "StotteData/Produksjonstilskuddskoder2_UTF8.csv"), property = "colclasses")

  # Make a vector with correct column names after translation
  correct_result <- c("beskrivelse" = "character", "enhet" = "character", "soknadmnd" = "character", "telledato" = "character")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_equivalent(colclasses,
                    correct_result)

  # Leveranseregisteret
  colclasses <- standardize_columns(data = paste0(set_dir_NVI("LevReg"), "FormaterteData/LevReg_202001_202006.csv"), property = "colclasses")

  # Make a vector with correct column names after translation
  correct_result <- c("anleggskode" = "character", "gjeldende_prodnr8" = "character", "hovednr" = "character", "id_nr" = "character",
                      "komnr" = "character", "prodnr10" = "character", "prodnr8" = "character",
                      "produsentfodselsnr" = "character", "varekategorikode" = "character", "varekode" = "character")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(colclasses,
                   correct_result)

})


test_that("Standardize colwidths for Excel", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  PJStest <- readRDS(file.path(".", "PJS_testdata.rds"))
  # PJStest <- readRDS("./tests/testthat/PJS_testdata.rds")

  #   # Make a vector with correct column names after translation
  correct_result <- c(5.00, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.00, 10.71, 10.71,
                      10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 20.00, 20.00, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 11.00, 11.00, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 11.00, 10.71,  8.00, 10.71, 10.71, 10.71, 10.71,
                      30.00, 10.71, 10.71, 10.71, 11.00, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 10.71, 8.00)

  expect_equal(standardize_columns(data = PJStest, property = "colwidths_Excel"),
               correct_result)


  # Standardisere kolonnenavn
  PJStest <- standardize_columns(data = PJStest, property = "colnames")

  #   # Make a vector with correct column names after translation
  correct_result <- c(5.00, 10.71, 10.71, 10.71, 10.71, 10.71, 10.00, 10.00, 10.00, 10.00,
                      10.00, 10.71, 10.71, 5.00, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 20.00, 20.00, 10.71, 10.71, 10.71, 10.71, 10.71,
                      5.00, 11.00, 11.00, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.00, 10.00, 11.00, 10.71, 8.00, 10.71, 10.71, 10.71, 10.71,
                      30.00, 10.71, 10.71, 10.71, 11.00, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      8.00, 10.00, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 30.00, 8.00)

  expect_equal(standardize_columns(data = PJStest, property = "colwidths_Excel"),
               correct_result)

  # Transforms to tibble before checking
  PJStest <- tibble::as_tibble(PJStest)

  #   # Make a vector with correct column names after translation
  correct_result <- c(5.00, 10.71, 10.71, 10.71, 10.71, 10.71, 10.00, 10.00, 10.00, 10.00,
                      10.00, 10.71, 10.71, 5.00, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 20.00, 20.00, 10.71, 10.71, 10.71, 10.71, 10.71,
                      5.00, 11.00, 11.00, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.00, 10.00, 11.00, 10.71, 8.00, 10.71, 10.71, 10.71, 10.71,
                      30.00, 10.71, 10.71, 10.71, 11.00, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      8.00, 10.00, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71, 10.71,
                      10.71, 10.71, 10.71, 30.00, 8.00)

  expect_equal(standardize_columns(data = PJStest, property = "colwidths_Excel"),
               correct_result)


})


test_that("Standardize English collabels", {

  # Generate column labels
  # Example with produksjonstilskudd in English
  df <- as.data.frame(cbind("soknadaar" = "2020", "gjeldende_prodnr8" = "30303030",
                            "prodnr8" = "30303030", "prodnr10" = "3030303030",
                            "orgnr" = "988888888", "komnr" = "3030",
                            "kommune" = "Nesbyen"))


  # Make a vector with correct column names after translation
  correct_result <- c("S\u00F8knads\u00E5r", "Gjeldende produsentnr", "Producer no.", "Producer no.", "Organisation number",
                      "Municipality no.", "Municipality")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(standardize_columns(data = df,
                                       property = "collabels",
                                       language = "en"),
                   correct_result)

  expect_identical(standardize_columns(data = df,
                                       standards = file.path(NVIdb::set_dir_NVI("ProgrammeringR", slash = FALSE),
                                                             "standardization", "colnames", "column_standards.csv"),
                                       property = "collabels",
                                       language = "en"),
                   correct_result)

})

test_that("Column order", {

  # Generate column labels
  # Example with produksjonstilskudd in English
  df <- as.data.frame(cbind("soknadaar" = "2020", "gjeldende_prodnr8" = "30303030", "Orgnr" = 99999999,
                            "prodnr10" = "3030303030", "Fj\u00F8rfe" = 1, "prodnr8" = "30303030",
                            "orgnr" = "988888888", "komnr" = "3030",
                            "kommune" = "Nesbyen"))

  df <- standardize_columns(data = df,
                            dbsource = "Produksjonstilskudd",
                            property = "colnames")

  df <- standardize_columns(data = df,
                            dbsource = "Produksjonstilskudd",
                            property = "colorder")

  # Make a vector with correct column names after translation
  correct_result <- c("soknadaar", "komnr", "gjeldende_prodnr8", "prodnr8", "prodnr10", "orgnr", "fjorfe", "kommune")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(colnames(df), correct_result)

  df <- standardize_columns(data = df,
                            dbsource = "Produksjonstilskudd",
                            property = "colorder",
                            exclude = TRUE)

  correct_result <- c("soknadaar", "komnr", "gjeldende_prodnr8", "prodnr8", "prodnr10", "orgnr", "fjorfe")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(colnames(df), correct_result)

})


test_that("List input to standards", {
  # Make example data
  df <- as.data.frame(cbind("ok_aar" = 2021, "rapport" = "Brucellose hos geit, utvalgsliste",
                            "mt_avdelingnr" = "M21150", "mt_avdeling" = "Romerike",
                            "mt_regionnr" = "M21000", "mt_region" = "Region Stor-Oslo",
                            "eier_lokalitetnr" = "30303030", "eier_lokalitet" = "XXX XXXXX", "orgnr" = 989989989,
                            "postnr" = "0468", "poststed" = "OSLO", "ant_prover" = 26))
  # Probably first makes a matrix, therefore "Antall prøver" is character.
  df$ant_prover <- as.numeric(df$ant_prover)

  df <- standardize_columns(data = df,
                            standards =
                              list("colname" = c("ok_aar", "rapport", "mt_regionnr", "mt_region", "mt_avdelingnr",
                                                 "mt_avdeling", "eier_lokalitetnr", "orgnr", "eier_lokalitet", "postnr", "poststed",
                                                 "ant_prover"),
                                   "collabel" = c("År", "Rapport", "MT regionnr", "MT region", "MT avdelingsnr",
                                                  "MT avdeling", "Produsentnr", "Virksomhetnr", "Virksomhet", "Postnr", "Poststed",
                                                  "Antall prøver"),
                                   "colwidth" = c(5, 9, 12.5, 16, 13, 30, 12, 12, 30, 8, 15, 8.5)),
                            dbsource = "brucella_geit",
                            property = "colorder",
                            exclude = TRUE)

  expect_equal(colnames(df),
               c("ok_aar", "rapport", "mt_regionnr", "mt_region", "mt_avdelingnr", "mt_avdeling", "eier_lokalitetnr", "orgnr",
                  "eier_lokalitet", "postnr", "poststed", "ant_prover"))

  expect_equal(standardize_columns(data = df,
                            standards =
                              list("colname" = c("ok_aar", "rapport", "mt_regionnr", "mt_region", "mt_avdelingnr",
                                                 "mt_avdeling", "eier_lokalitetnr", "orgnr", "eier_lokalitet", "postnr", "poststed",
                                                 "ant_prover"),
                                   "collabel" = c("År", "Rapport", "MT regionnr", "MT region", "MT avdelingsnr",
                                                  "MT avdeling", "Produsentnr", "Virksomhetnr", "Virksomhet", "Postnr", "Poststed",
                                                  "Antall prøver"),
                                   "colwidth" = c(5, 9, 12.5, 16, 13, 30, 12, 12, 30, 8, 15, 8.5)),
                            dbsource = "brucella_geit",
                            property = "collabels",
                            exclude = TRUE),
               c("År", "Rapport",
                 "MT regionnr", "MT region", "MT avdelingsnr", "MT avdeling",
                 "Produsentnr", "Virksomhetnr", "Virksomhet",
                 "Postnr", "Poststed", "Antall prøver"))
})

test_that("standardize_columns argument checking", {
  linewidth <- options("width")
  options(width = 80)

  PJStest <- readRDS(file.path(".", "PJS_testdata.rds"))
  # PJStest <- readRDS("./tests/testthat/PJS_testdata.rds")

  expect_error(standardize_columns(data = PJStestX, property = "colNames", language = "no", exclude = FALSE),
               regexp = "object 'PJStestX' not found")

  expect_error(standardize_columns(data = PJStest, property = "columnNames", language = "no", exclude = FALSE),
               regexp = "property")

  expect_error(standardize_columns(data = PJStest, standards = 1, property = "colNames", language = "no", exclude = FALSE),
               regexp = "but has class 'numeric'")

  expect_error(standardize_columns(data = PJStest, property = "colClasses", language = "no", exclude = FALSE),
               regexp = "No file provided.")

  expect_error(standardize_columns(data = PJStest, property = "colLabels", language = "sa", exclude = FALSE),
               regexp = "Variable 'language': Must be a subset of ")

  expect_error(standardize_columns(data = PJStest, property = "colNames", language = "no", exclude = "FALSE"),
               regexp = "Variable 'exclude': Must be of type 'logical', not 'character'.")

  options(width = unlist(linewidth))
})
