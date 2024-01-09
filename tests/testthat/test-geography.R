test_that("get_park_polygon returns an object of class sfc_POLYGON", {
  x <- get_park_polygon("ROMO")
  expect_equal(class(x)[1], "sfc_POLYGON")
})

test_that("validate_coord properly validates valid coordinate", {
  x <- validate_coord(unit_code = "ROMO",
                      lat = 40.32764,
                      lon = -105.70421)
  expect_equal(x, TRUE)
})

test_that("validate_coord properly validates invalid coordinate", {
  x <- validate_coord(unit_code = "ROMO",
                      lat = 40.32764,
                      lon = 105.70421)
  expect_equal(x, FALSE)
})



