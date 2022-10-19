library(NVIdb)
library(testthat)

test_that("build query ND outbreak", {
  query <- build_query_outbreak(year = 2022,
                                utbrudd = "27",
                                hensikt = c("0100101014", # "Mistanke"
                                            "0100102005", # "Oppfølging"
                                            "0100103005", # "Oppfølging"
                                            "0100104029", # "Paramyxo hos due"
                                            "0200130%"), # "Overvåking villfugl"
                                analytt = "01130301%",
                                metode = c("070171", "070175", "070331"))

  correct_result <- paste("SELECT *",
                          "FROM v2_sak_m_res",
                          "WHERE aar = 2022 AND",
                          "(hensiktkode IN ('0100101014', '0100102005', '0100103005', '0100104029') OR",
                          "hensiktkode LIKE '0200130%' OR",
                          "utbrudd_id = '27' OR",
                          "metodekode IN ('070171', '070175', '070331') OR",
                          "konkl_analyttkode LIKE '01130301%' OR",
                          "analyttkode_funn LIKE '01130301%')")

  expect_equivalent(query["selection_v2_sak_m_res"], correct_result)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                          "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                          "FROM v_innsendelse AS sak",
                          "INNER JOIN v_sakskonklusjon",
                          "ON (v_sakskonklusjon.aar = sak.aar AND",
                          "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                          "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                          "WHERE sak.aar = 2022 AND (analyttkode LIKE '01130301%')")

  expect_equivalent(query["selection_sakskonklusjon"], correct_result)

})


# test_that("build query HPAI outbreak", {
#   query <- build_query_outbreak(year = 2020,
#                                 analytt = c("012601%", "15020804"),
#                                 metode = "070183")
#
#   correct_result <- paste("SELECT *",
#                           "FROM v2_sak_m_res",
#                           "WHERE aar = 2020 AND",
#                           "( metodekode = '070183' OR",
#                           "konkl_analyttkode = '15020804' OR konkl_analyttkode LIKE '012601%' OR",
#                           "analyttkode_funn = '15020804' OR analyttkode_funn LIKE '012601%' )")
#
#   expect_equivalent(query["selection_v2_sak_m_res"], correct_result)
#
#   correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
#                           "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
#                           "FROM v_innsendelse AS sak",
#                           "INNER JOIN v_sakskonklusjon",
#                           "ON (v_sakskonklusjon.aar = sak.aar AND",
#                           "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
#                           "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
#                           "WHERE sak.aar = 2020 AND ( analyttkode = '15020804' OR analyttkode LIKE '012601%' )")
#
#   expect_equivalent(query["selection_sakskonklusjon"], correct_result)
#
# })


# test_that("build query P. ovis outbreak", {
#   query <- build_query_outbreak(year = 2020,
#                                 analytt = "0406020202%")
#
#   correct_result <- paste("SELECT *",
#                           "FROM v2_sak_m_res",
#                           "WHERE aar = 2020 AND",
#                           "( konkl_analyttkode LIKE '0406020202%' OR analyttkode_funn LIKE '0406020202%' )")
#
#   expect_equivalent(query["selection_v2_sak_m_res"], correct_result)
#
#   correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
#                           "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
#                           "FROM v_innsendelse AS sak",
#                           "INNER JOIN v_sakskonklusjon",
#                           "ON (v_sakskonklusjon.aar = sak.aar AND",
#                           "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
#                           "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
#                           "WHERE sak.aar = 2020 AND ( analyttkode LIKE '0406020202%' )")
#
#   expect_equivalent(query["selection_sakskonklusjon"], correct_result)
#
# })


test_that("build query maedi outbreak", {
  query <- build_query_outbreak(year = c(2019:2022),
                                hensikt = c("0100104020", "0100104054", "0100105007", "0100105008", "0100106007",
                                             "0700605", "0400101", "0400109001", "0200113", "0200135", "0200141", "0200163"))

  correct_result <- paste("SELECT *",
                          "FROM v2_sak_m_res",
                          "WHERE aar >= 2019 AND",
                          "(hensiktkode IN ('0100104020', '0100104054', '0100105007', '0100105008', '0100106007',",
                          "'0700605', '0400101', '0400109001', '0200113', '0200135', '0200141', '0200163'))")

  expect_equivalent(query["selection_v2_sak_m_res"], correct_result)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                          "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                          "FROM v_innsendelse AS sak",
                          "INNER JOIN v_sakskonklusjon",
                          "ON (v_sakskonklusjon.aar = sak.aar AND",
                          "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                          "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                          "WHERE sak.aar >= 2019")

  expect_equivalent(query["selection_sakskonklusjon"], correct_result)

})



test_that("build query outbreak error testing", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(build_query_outbreak(analytt = "0406020202"),
               regexp = 'argument "year" is missing')

  expect_error(build_query_outbreak(year = 2020, metode = "070183"),
               regexp = "Variable 'list(utbrudd, hensikt, analytt)': At least one of the", fixed = TRUE)

  expect_error(build_query_outbreak(year = (as.numeric(format(Sys.Date(), "%Y")) + 1), analytt = "0406020202"),
               regexp = "Variable 'year': Element ")

  options(width = unlist(linewidth))
})