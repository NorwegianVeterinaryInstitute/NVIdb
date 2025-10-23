# library(NVIdb)
library(testthat)

# Assigns temporary dir to td
td <- tempdir()

test_that("Copy MT-omrader", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # copy_kommune_fylke
  copy_MT_omrader(to_path = td)

  expect_true(file.exists(file.path(td, "MT_omrader.csv")))
  expect_true(file.exists(file.path(td, "komnr_2_MT_avdeling.csv")))
  expect_true(file.exists(file.path(td, "komnr_2_MT_enhet.csv")))

})

test_that("Correct merging of MT-avdeling og MT-region basert på organisering fra 2025", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))
  
  # Load translation table for MT_omrader
  komnr_2_MT_omrader <- read_MT_omrader()
  # kommune_fylke <- read_kommune_fylke()
  
  # Make a dataframe with kommunenr that should be translated
  kommuner <- as.data.frame(list("komnr" = c("0123", "3014", "3118", "0301", "1142", "5001")))

  # Make a dataframe with the correct result
  correct_result <- as.data.frame(
    list("MT_avdelingnr" = c("M42200", "M42200", "M42200", "M42200", "M42200", "M42200"),
         "MT_avdeling" = c("Avdeling landdyr", "Avdeling landdyr", "Avdeling landdyr",
                           "Avdeling landdyr", "Avdeling landdyr", "Avdeling landdyr"),
         "MT_enhetnr" = c("M42250", "M42250", "M42250", "M42250", "M42220", "M42220"),
         "MT_enhet" = c("Seksjon produksjonsdyr øst", "Seksjon produksjonsdyr øst", 
                        "Seksjon produksjonsdyr øst", "Seksjon produksjonsdyr øst", 
                        "Seksjon produksjonsdyr midt", "Seksjon produksjonsdyr midt"),
         "komnr" = c("0123", "3014", "3118", "0301", "1142", "5001")))

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(add_MT_omrader(data = kommuner,
                                  year = 2025,
                                  fag = "Storfe",
                                  translation_table = komnr_2_MT_omrader,
                                  code_column = "komnr",
                                  new_column = c("MT_avdelingnr", "MT_avdeling", "MT_enhetnr", "MT_enhet"),
                                  position = "first"),
                   correct_result)
  
})


test_that("Correct merging of MT-avdeling og MT-region basert på kommunenr for 2024", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))
  
  # Load translation table for MT_omrader
  MT_omrader <- read_MT_omrader()
  
  # Make a dataframe with kommunenr that should be translated
  kommuner <- as.data.frame(c("0123", "0301", "1142", "5001"), stringsAsFactors = FALSE)
  colnames(kommuner) <- "komnr"
  
  # Make a dataframe with the correct result
  correct_result <- cbind(as.data.frame(c("M21000", "M21000", "M23000", "M24000"), stringsAsFactors = FALSE),
                          as.data.frame(c("Region Stor-Oslo", "Region Stor-Oslo", "Region Sør og Vest", "Region Midt"), stringsAsFactors = FALSE),
                          as.data.frame(c("M21130", "M21140", "M23140", "M24140"), stringsAsFactors = FALSE),
                          as.data.frame(c("Østfold og Follo", "Oslo, Asker og Bærum", "Sør-Rogaland, Sirdal og Flekkefjord", "Trondheim og omland"), stringsAsFactors = FALSE),
                          as.data.frame(c("0123", "0301", "1142", "5001"), stringsAsFactors = FALSE))
  colnames(correct_result) <- c("MT_regionnr", "MT_region", "MT_avdelingnr", "MT_avdeling", "komnr")
  
  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(add_MT_omrader(data = kommuner,
                                  year = 2024,
                                  translation_table = MT_omrader,
                                  code_column = "komnr",
                                  new_column = c("MT_regionnr", "MT_region", "MT_avdelingnr", "MT_avdeling"),
                                  position = "first"),
                   correct_result)
  
})


test_that("Correct result when using overwrite and keep for 2024", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Load translation table for kommune_fylke
  MT_omrader <- read_MT_omrader()

  # Make a dataframe with komnr that should be translated
  kommuner <- as.data.frame(c("0123", "0301", "1142", "5001"), stringsAsFactors = FALSE)
  colnames(kommuner) <- "komnr"

  # Make a dataframe with the correct result
  correct_result <- cbind(as.data.frame(c("M21000", "M21000", "M23000", "M24000"), stringsAsFactors = FALSE),
                          as.data.frame(c("Region Stor-Oslo", "Region Stor-Oslo", "Region Sør og Vest", "Region Midt"), stringsAsFactors = FALSE),
                          as.data.frame(c("M21130", "M21140", "M23140", "M24140"), stringsAsFactors = FALSE),
                          as.data.frame(c("Østfold og Follo", "Oslo, Asker og Bærum", "Sør-Rogaland, Sirdal og Flekkefjord", "Trondheim og omland"), stringsAsFactors = FALSE),
                          as.data.frame(c("0123", "0301", "1142", "5001"), stringsAsFactors = FALSE))
  colnames(correct_result) <- c("MT_regionnr", "MT_region", "MT_avdelingnr", "MT_avdeling", "komnr")


  # Compare Add kommune, current komnr and current kommune with correct result
  kommuner <- add_MT_omrader(data = kommuner,
                             year = 2024,
                             translation_table = MT_omrader,
                             code_column = "komnr",
                             new_column = c("MT_regionnr", "MT_region", "MT_avdelingnr", "MT_avdeling"),
                             position = "left")

  expect_identical(kommuner,
                   correct_result)

  # Compare add_MT_omrader, with overwrite = TRUE, new position
  kommuner <- add_MT_omrader(data = kommuner,
                             year = 2024,
                             translation_table = MT_omrader,
                             code_column = "komnr",
                             new_column = c("MT_avdelingnr", "MT_avdeling"),
                             position = "last",
                             overwrite = TRUE)

  expect_identical(kommuner,
                   correct_result[, c("MT_regionnr", "MT_region", "komnr", "MT_avdelingnr", "MT_avdeling")])

  # Compare add_MT_omrader, with overwrite = TRUE, keep position
  kommuner <- add_MT_omrader(data = kommuner,
                             year = 2024,
                             translation_table = MT_omrader,
                             code_column = "komnr",
                             new_column = c("MT_regionnr", "MT_region"),
                             position = "keep",
                             overwrite = TRUE)


  expect_identical(kommuner,
                   correct_result[, c("MT_regionnr", "MT_region", "komnr", "MT_avdelingnr", "MT_avdeling")])


})


test_that("Correct result when using abbreviated position for 2024", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Load translation table for kommune_fylke
  MT_omrader <- read_MT_omrader()

  # Make a dataframe with komnr that should be translated
  kommuner <- as.data.frame(c("0123", "0301", "1142", "5001"), stringsAsFactors = FALSE)
  colnames(kommuner) <- "komnr"

  # Make a dataframe with the correct result
  correct_result <- cbind(as.data.frame(c("M21000", "M21000", "M23000", "M24000"), stringsAsFactors = FALSE),
                          as.data.frame(c("Region Stor-Oslo", "Region Stor-Oslo", "Region Sør og Vest", "Region Midt"), stringsAsFactors = FALSE),
                          as.data.frame(c("M21130", "M21140", "M23140", "M24140"), stringsAsFactors = FALSE),
                          as.data.frame(c("Østfold og Follo", "Oslo, Asker og Bærum", "Sør-Rogaland, Sirdal og Flekkefjord", "Trondheim og omland"), stringsAsFactors = FALSE),
                          as.data.frame(c("0123", "0301", "1142", "5001"), stringsAsFactors = FALSE))
  colnames(correct_result) <- c("MT_regionnr", "MT_region", "MT_avdelingnr", "MT_avdeling", "komnr")


  # Compare Add kommune, current komnr and current kommune with correct result
  kommuner <- add_MT_omrader(data = kommuner,
                             year = 2024,
                             translation_table = MT_omrader,
                             code_column = "komnr",
                             new_column = c("MT_regionnr", "MT_region", "MT_avdelingnr", "MT_avdeling"),
                             position = "le")

  expect_identical(kommuner,
                   correct_result)

  # Compare add_MT_omrader, with overwrite = TRUE, new position
  kommuner <- add_MT_omrader(data = kommuner,
                             year = 2024,
                             translation_table = MT_omrader,
                             code_column = "komnr",
                             new_column = c("MT_avdelingnr", "MT_avdeling"),
                             position = "la",
                             overwrite = TRUE)

  expect_identical(kommuner,
                   correct_result[, c("MT_regionnr", "MT_region", "komnr", "MT_avdelingnr", "MT_avdeling")])

  # Compare add_MT_omrader, with overwrite = TRUE, keep position
  kommuner <- add_MT_omrader(data = kommuner,
                             year = 2024,
                             translation_table = MT_omrader,
                             code_column = "komnr",
                             new_column = c("MT_regionnr", "MT_region"),
                             position = "k",
                             overwrite = TRUE)


  expect_identical(kommuner,
                   correct_result[, c("MT_regionnr", "MT_region", "komnr", "MT_avdelingnr", "MT_avdeling")])


})


test_that("errors for read_MT_omrader", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(read_MT_omrader(filename = NULL, from_path = tempdir()),
               regexp = "Variable 'filename': One of the following must apply:",
               fixed = TRUE)

  expect_error(read_MT_omrader(filename = "filename.csv", from_path = tempdir()),
               regexp = "File does not exist:")

  options(width = unlist(linewidth))
})
