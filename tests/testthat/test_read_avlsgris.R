# library(NVIdb)
library(testthat)
library(checkmate)

test_that("read_avlsgris", {
  avlsgris <- read_avlsgris(year = 2020, month = "11")
  test_data_frame(avlsgris, nrows = 90)
  
  avlsgris <- read_avlsgris(year = 2021, month = "09")
  test_data_frame(avlsgris, nrows = 90)
  
  avlsgris <- read_avlsgris(year = 2024)
  test_data_frame(avlsgris, nrows = 61)
})


test_that("Argument testing in read_avlsgris", {
  linewidth <- options("width")
  options(width = 80)
  
  expect_error(read_avlsgris(year = 2020, month = "13"),
               regexp = "This is wrong")
  
  options(width = unlist(linewidth))
})


test_that("Errors in read_avlsgris", {
  linewidth <- options("width")
  options(width = 80)
  
  expect_error(read_avlsgris(year = 2020, month = "06"),
               regexp = "This is wrong")
  
  options(width = unlist(linewidth))
})

