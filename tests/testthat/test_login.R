library(NVIdb)
library(testthat)

test_that("Log in to db services", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(isTRUE(NVIcheckmate::check_credentials("PJS")))

  odbc_connected <- login_PJS()
  expect_true(as.vector(odbc_connected) >= 1)
  RODBC::odbcClose(odbc_connected)

  odbc_connected <- login("PJS")
  expect_true(as.vector(odbc_connected) >= 1)
  RODBC::odbcClose(odbc_connected)

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
                       dbprotocol = "y"),
                 regexpr = "ODBC connection failed")

  options(width = unlist(linewidth))
})