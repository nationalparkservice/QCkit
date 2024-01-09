test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

need_elev <- data.frame(decimalLongitude = c(-105.73012,
                                               -105.73044,
                                               -105.83044),
                           decimalLatitude = c(40.56321,
                                                40.78333,
                                                40.26921)
                           )

test_that("get_elevation returns the correct columns", {
  x <- get_elevation(df = need_elev,
                     decimal_lat = "decimalLatitude",
                     decimal_long = "decimalLongitude",
                     spatial_ref = "4326",
                     force = TRUE)
  expect_equal(names(x), c("decimalLongitude",
                           "decimalLatitude",
                           "minimumElevationInMeters",
                           "maximumElevationInMeters"))
})