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
wrong_hemisphere <- data.frame(decimalLongitude = c(105.73012,
                                                    105.73044,
                                                    105.83044),
                               decimalLatitude = c(-40.56321,
                                                   -40.78333,
                                                   -40.26921)
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

test_that("get_elevation returns correct columns for GPS in wrong hemisphere", {
  x <- get_elevation(df = wrong_hemisphere,
                     decimal_lat = "decimalLatitude",
                     decimal_long = "decimalLongitude",
                     spatial_ref = "4326",
                     force = TRUE)
  expect_equal(names(x), c("decimalLongitude",
                           "decimalLatitude",
                           "minimumElevationInMeters",
                           "maximumElevationInMeters"))
})

gh_req_json <- httr::content(req, "text")
gh_req_rjson <- jsonlite::fromJSON(gh_req_json)