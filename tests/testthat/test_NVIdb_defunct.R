library(testthat)

test_that("errors for defunct functions", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(set_PAT(),
               regexp = "'set_PAT', 'get_PAT', and 'remove_PAT' should be replaced by",
               fixed = TRUE)

  expect_error(get_PAT(),
               regexp = "'set_PAT', 'get_PAT', and 'remove_PAT' should be replaced by",
               fixed = TRUE)

  expect_error(remove_PAT(),
               regexp = "'set_PAT', 'get_PAT', and 'remove_PAT' should be replaced by",
               fixed = TRUE)

  expect_error(login_EOS(),
               regexp = "'login_EOS' is replaced by 'login('EOS')' as the wrapper",
               fixed = TRUE)

  expect_error(login_by_input_EOS(),
               regexp = "'login_by_input_EOS' is replaced by 'login_by_input('EOS')' as",
               fixed = TRUE)

  expect_error(login_by_credentials_EOS(),
               regexp = "'login_by_credentials_EOS' is replaced by 'login_by_credentials('EOS')'",
               fixed = TRUE)

  options(width = unlist(linewidth))
})
