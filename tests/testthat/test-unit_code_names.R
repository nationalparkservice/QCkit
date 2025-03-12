test_that("unit_codes_to_names returns a data frame", {
  expect_equal(is.data.frame(unit_codes_to_names("ROMO")),
  TRUE)
})