# library(NVIdb)
library(testthat)
library(checkmate)

test_that("read_avlsgris", {
  # Test if access to NVI internal files
  skip_if_not(dir.exists(set_dir_NVI("FAG")))
  
  avlsgris <- read_avlsgris(year = 2020, month = "11")
  test_data_frame(avlsgris, nrows = 78)
  
  avlsgris <- read_avlsgris(year = 2021, month = "09")
  test_data_frame(avlsgris, nrows = 78)
  
  avlsgris <- read_avlsgris(year = 2024)
  test_data_frame(avlsgris, nrows = 61)
})


test_that("Argument testing in read_avlsgris", {
  linewidth <- options("width")
  options(width = 80)
  
  expect_error(read_avlsgris(year = 2020, month = "13"),
               regexp = "Variable 'month': Must be a subset of")
  
  options(width = unlist(linewidth))
})


test_that("Errors in read_avlsgris", {
  # Test if access to NVI internal files
  skip_if_not(dir.exists(set_dir_NVI("FAG")))
  
  linewidth <- options("width")
  options(width = 80)
  
  expect_error(read_avlsgris(year = 2020, month = "06"),
               regexp = "Must have at least 1 rows, but has 0 rows")
  
  options(width = unlist(linewidth))
})

