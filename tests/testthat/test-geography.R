##### validate_coord_list
test_that("validate_coord_list returns a logical", {
  x <- validate_coord_list(47.84532, -105.23451, "ROMO")
  expect_equal(class(x)[1], "logical")
})

##### .get_unit_boundary
test_that(".get_unit_boundary returns an sf object",{
  x <- .get_unit_boundary("ROMO")
  expect_equal(class(x)[1], "sf")
})

##### get_park_polygon
test_that("get_park_polygon returns an object of class sfc_POLYGON", {
  x <- get_park_polygon("ROMO")
  expect_equal(class(x)[1], "sfc_POLYGON")
})

##### validate_coord
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

#### get_umt_zone
test_that("get_utm_zone returns correct utm zone", {
  expect_equal(get_utm_zone(-105.70421), 13)
})

test_that("get_utm_zone handles longitudes > 180 decimal degrees", {
  expect_equal(get_utm_zone(181), NULL)
})

test_that("get_utm_zone handles longitudes < -180 decimal degrees", {
  expect_equal(get_utm_zone(-181), NULL)
})

#### convert_long_to_utm
test_that("convert_long_to_utm returns correct utm zone", {
  expect_equal(suppressWarnings(convert_long_to_utm(-105.70421)), 13)
})

#### long2UTM
test_that("long2UTM returns correct utm zone", {
  expect_equal(suppressWarnings(long2UTM(-105.70421)), 13)
})

#### fuzz_location
test_that("fuzz_location handles non-numeric lat and lon", {
  x <- fuzz_location(lat = "non-numeric",
                     lon = "non-numeric",
                     coord_ref_sys = 4326,
                     fuzz_level = "Fuzzed - 1km")
  expect_equal(x, NULL)
})

test_that("fuzz_location returns well known text", {
  x <- fuzz_location(lat = 40.32764,
                     lon = -105.70421,
                     coord_ref_sys = 4326,
                     fuzz_level = "Fuzzed - 1km")
  expect_equal(is.na(wk::wk_problems(wk::new_wk_wkt(x))), TRUE)
})

test_that("fuzz_location returns well known text with 10km fuzzing", {
  x <- fuzz_location(lat = 40.32764,
                     lon = -105.70421,
                     coord_ref_sys = 4326,
                     fuzz_level = "Fuzzed - 10km")
  expect_equal(is.na(wk::wk_problems(wk::new_wk_wkt(x))), TRUE)
})

test_that("fuzz_location returns well known text with 100m fuzzing", {
  x <- fuzz_location(lat = 40.32764,
                     lon = -105.70421,
                     coord_ref_sys = 4326,
                     fuzz_level = "Fuzzed - 100m")
  expect_equal(is.na(wk::wk_problems(wk::new_wk_wkt(x))), TRUE)
})

test_that("fuzz_location returns well known text datum 4269", {
  x <- fuzz_location(lat = 40.32764,
                     lon = -105.70421,
                     coord_ref_sys = 4269,
                     fuzz_level = "Fuzzed - 100m")
  expect_equal(is.na(wk::wk_problems(wk::new_wk_wkt(x))), TRUE)
})

test_that("fuzz_location returns well known text datum 32616", {
  x <- fuzz_location(lat = 40.32764,
                     lon = -105.70421,
                     coord_ref_sys = 32616,
                     fuzz_level = "Fuzzed - 100m")
  expect_equal(is.na(wk::wk_problems(wk::new_wk_wkt(x))), TRUE)
})

test_that("fuzz_location returns NULL if coord_ref_sys is not decimal degree WGS84 or UTM/GS84", {
  x <- fuzz_location(lat = 40.32764,
                     lon = -105.70421,
                     coord_ref_sys = 1234567789,
                     fuzz_level = "Fuzzed - 100m")
  expect_equal(x, NULL)
})

test_that("fuzz_location returns well known text for souther hemisphere latitudes", {
  x <- fuzz_location(lat = -40.32764,
                     lon = -105.70421,
                     coord_ref_sys = 32616,
                     fuzz_level = "Fuzzed - 100m")
  expect_equal(is.na(wk::wk_problems(wk::new_wk_wkt(x))), TRUE)
})

#### generate_ll_from_utm
test_that("generate_ll_from_utm adds the correct columns", {
  mydataframe <- tibble::tibble(EastingCol = c(-105.70421,
                                               -105.70431,
                                               -105.7451),
                                NorthingCol = c(40.70421,
                                                40.70431,
                                                40.70451),
                                zone = 13,
                                datum = "NAD83")
  x <- generate_ll_from_utm(df = mydataframe,
                         EastingCol = EastingCol,
                         NorthingCol = NorthingCol,
                         ZoneCol = zone,
                         DatumCol = datum)
  expect_equal(names(x), c("EastingCol", "NorthingCol", "zone", "datum", "decimalLatitude", "decimalLongitude", "LatLong_CRS"))
})

#### generate_ll_from_utm
test_that("generate_ll_from_utm outputs the same number of rows as the input dataframe", {
  mydataframe <- tibble::tibble(EastingCol = c(-105.70421,
                                               -105.70431,
                                               -105.7451),
                                NorthingCol = c(40.70421,
                                                40.70431,
                                                40.70451),
                                zone = 13,
                                datum = "NAD83")
  mydataframe_na <- mydataframe
  mydataframe_na$EastingCol[1] <- NA

  x <- generate_ll_from_utm(df = mydataframe,
                            EastingCol = EastingCol,
                            NorthingCol = NorthingCol,
                            ZoneCol = zone,
                            DatumCol = datum)
  y <- generate_ll_from_utm(df = mydataframe,
                            EastingCol = EastingCol,
                            NorthingCol = NorthingCol,
                            ZoneCol = zone,
                            DatumCol = datum)

  expect_equal(nrow(x), nrow(mydataframe))
  expect_equal(nrow(y), nrow(mydataframe_na))
})

#### generate_ll_from_utm
test_that("generate_ll_from_utm conversion matches that of convert_utm_to_ll", {
  mydataframe <- tibble::tibble(EastingCol = c(-105.70421,
                                               -105.70431,
                                               -105.7451),
                                NorthingCol = c(40.70421,
                                                40.70431,
                                                40.70451),
                                zone = 13,
                                datum = "WGS84")

  x <- generate_ll_from_utm(df = mydataframe,
                            EastingCol = EastingCol,
                            NorthingCol = NorthingCol,
                            ZoneCol = zone,
                            DatumCol = datum,
                            latlong_datum = "WGS84")
  y <- convert_utm_to_ll(df = mydataframe,
                            EastingCol = EastingCol,
                            NorthingCol = NorthingCol,
                            zone = 13,
                            datum = "WGS84")

  expect_equal(x$decimalLatitude, y$decimalLatitude)
  expect_equal(x$decimalLongitude, y$decimalLongitude)
})

#### convert_utm_to_ll (superseded)
test_that("convert_utm_to_ll adds the correct columns", {
  mydataframe <- tibble::tibble(EastingCol = c(-105.70421,
                                               -105.70431,
                                               -105.7451),
                                NorthingCol = c(40.70421,
                                                40.70431,
                                                40.70451),
                                zone = 13)
  x <- convert_utm_to_ll(df = mydataframe,
                       EastingCol = EastingCol,
                       NorthingCol = NorthingCol,
                       zone = 13,
                       datum = "WGS84")
  expect_equal(names(x), c("EastingCol", "NorthingCol", "zone", "decimalLongitude", "decimalLatitude"))
})

#### utm_to_ll (deprecated)
test_that("utm_to_ll adds the correct columns", {
  mydataframe <- tibble::tibble(EastingCol = c(-105.70421,
                                               -105.70431,
                                               -105.7451),
                                NorthingCol = c(40.70421,
                                                40.70431,
                                                40.70451),
                                zone = 13)
  x <- suppressWarnings(utm_to_ll(df = mydataframe,
                         EastingCol = EastingCol,
                         NorthingCol = NorthingCol,
                         zone = 13,
                         datum = "WGS84"))
  expect_equal(names(x), c("EastingCol", "NorthingCol", "zone", "decimalLongitude", "decimalLatitude"))
})

