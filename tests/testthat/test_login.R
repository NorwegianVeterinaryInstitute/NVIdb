# library(NVIdb)
library(testthat)

test_that("Log in to db services", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(isTRUE(NVIcheckmate::check_credentials("PJS")))

  linewidth <- options("width")
  options(width = 80)

  odbc_connected <- login("PJS")
  expect_true(as.vector(odbc_connected) >= 1)
  RODBC::odbcClose(odbc_connected)

  options(width = unlist(linewidth))
})

test_that("Errors or warnings for login", {
  linewidth <- options("width")
  options(width = 80)

  expect_error(login("PHS"),
               regexpr = "Variable 'dbservice': Must be element of")

  expect_warning(login(dbservice = "PJS",
                       dbdriver = "x",
                       db = "y",
                       dbserver = "z",
                       dbport = "x",
                       dbprotocol = "y",
                       dbinterface = NULL),
                 regexpr = "ODBC connection failed")

  expect_error(login(dbservice = "EOS", dbinterface = "noodbc"),
               regexpr = "Variable 'dbinterface': Must be element of set")

  expect_error(login(dbservice = "PJS", dbinterface = "noodbc"),
               regexpr = "Variable 'dbinterface': Must be element of set")

  options(width = unlist(linewidth))
})
