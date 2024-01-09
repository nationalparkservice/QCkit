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

test_that("get_utm_zone returns correct utm zone", {
  expect_equal(get_utm_zone(-105.70421), 13)
})

test_that("get_utm_zone handles longitudes > 180 decimal degrees", {
  expect_equal(get_utm_zone(181), NULL)
})

test_that("get_utm_zone handles longitudes < -180 decimal degrees", {
  expect_equal(get_utm_zone(-181), NULL)
})

test_that("convert_long_to_utm returns correct utm zone", {
  expect_equal(suppressWarnings(convert_long_to_utm(-105.70421)), 13)
})

test_that("long2UTM returns correct utm zone", {
  expect_equal(suppressWarnings(convert_long_to_utm(-105.70421)), 13)
})
