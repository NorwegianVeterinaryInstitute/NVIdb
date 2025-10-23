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

  options(width = unlist(linewidth))
})
