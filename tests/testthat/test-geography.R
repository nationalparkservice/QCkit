test_that("get_park_polygon returns an object of class sfc_POLYGON", {
  x <- get_park_polygon("ROMO")
  expect_equal(class(x)[1], "sfc_POLYGON")
})

