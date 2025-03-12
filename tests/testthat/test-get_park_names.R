exampleDF <- data.frame(Park_Code = c("ROMO", "ROMN", "R", "ROO", "ROM", "ROMOO"),
                        exampleData = c("test", "test", "test", "test", "test", "test"))

exampleTibble <- tibble::tibble(Park_Code = c("ROMO", "ROMN", "R", "ROO", "ROM", "ROMOO"),
                                exampleData = c("test", "test", "test", "test", "test", "test"))

# test output class
test_that("data frame input returns data frame", {
  expect_equal(is.data.frame(get_park_names(exampleDF)), TRUE)
})

# test output class
test_that("tibble input returns data frame", {
  expect_equal(is.data.frame(get_park_names(exampleTibble)), TRUE)
})

# check length of returned data frame
test_that("returned data frame has one added column", {
  x <- get_park_names(exampleDF)
  expect_equal(length(x), length(exampleDF) + 1)
})

# test that bad unit codes return NAs in parkName column
test_that("bad codes return NAs in data frame", {
  x <- get_park_names(exampleDF)
  expect_equal(dplyr::filter(x, Park_Code %in% c("R", "ROO", "ROM", "ROMOO")), dplyr::filter(x, is.na(parkName)))
})

# test that bad unit codes return NAs in parkName column
test_that("bad codes return NAs in tibble", {
  x <- get_park_names(exampleTibble)
  expect_equal(dplyr::filter(x, Park_Code %in% c("R", "ROO", "ROM", "ROMOO")), dplyr::filter(x, is.na(parkName)))
})

# test message for codes not found
test_that("returns message for codes not found", {
  expect_message(get_park_names(exampleDF), "The following unit codes were not found: ")
})

# test message for multiple codes found
test_that("returns message for codes resolve to many names", {
  expect_message(get_park_names(exampleDF), "The following unit codes resolved to multiple park names: ")
})
