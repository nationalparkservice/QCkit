dir <- here::here("tests", "testthat", "good", "BICY_Veg_Data_Package_Example")

####### testing get_custom_flags
test_that("get_custom_flags works", {
  expect_type(get_custom_flags(dir, output = "all"), "list")
})

test_that("get_custom_flags outputs 2 data frames when output = all", {
  x <- get_custom_flags(dir, output = "all")
  expect_equal(length(seq_along(x)), 2)
})

test_that("get_custom_flags outputs correct column names for files", {
  x <- get_custom_flags(dir, output = "files")
  expect_equal(names(x), c("File Name", "A", "AE", "P",  "R", "% Accepted"))
})

test_that("get_custom_flags outputs correct column names for columns", {
  x <- get_custom_flags(dir, output = "columns")
  expect_equal(names(x), c("File Name", "Measure", "Number of Records", "A",
                           "AE", "R", "P", "% Accepted" ))
})

##### Tests for deprecated function:

##### Test get_dp_flags
test_that("get_dp_flags is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(get_dp_flags(directory = dir), class = "defunctError")
})

test_that("get_dp_flags works", {
  x <- suppressWarnings(get_dp_flags(directory = dir))
  expect_type(x, "list")
})

test_that("get_dp_flags returns correct column names", {
  x <- suppressWarnings(get_dp_flags(directory = dir))
  expect_equal(names(x), c("A_flag", "AE_flag", "R_flag", "P_flag", "Cell_count", "RRU"))
})


##### test get_df_flags
test_that("get_df_flags is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(get_df_flags(directory = dir), class = "defunctError")
})

test_that("get_df_flags works", {
  x <- suppressWarnings(get_df_flags(directory = dir))
  expect_type(x, "list")
})

test_that("get_df_flags returns correct column names", {
  x <- suppressWarnings(get_df_flags(directory = dir))
  expect_equal(names(x), c("filename", "A_flag", "AE_flag", "R_flag", "P_flag", "Cell_count", "RRU"))
})

##### test get_dc_flags
test_that("get_dc_flags is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(get_dc_flags(directory = dir), class = "defunctError")
})

test_that("get_dc_flags works", {
  x <- suppressWarnings(get_dc_flags(directory = dir))
  expect_type(x, "list")
})

test_that("get_dc_flags returns correct column names", {
  x <- suppressWarnings(get_dc_flags(directory = dir))
  expect_equal(names(x), c("filename", "flagged_col", "A_flag", "AE_flag", "R_flag", "P_flag", "Flagged Data Totals", "RRU"))
})