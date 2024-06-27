test_that("fix_utc_offset fixes offsets", {
  datetimes <- c("2023-11-16T03:32:49+07:00","2023-11-16T03:32:49-07","2023-11-16T03:32:49","2023-11-16T03:32:49Z")
  new_datetimes <- suppressWarnings(fix_utc_offset(datetimes))
  expect_equal(new_datetimes, c("2023-11-16T03:32:49+0700",
                                            "2023-11-16T03:32:49-0700",
                                            "2023-11-16T03:32:49",
                                            "2023-11-16T03:32:49+0000"))
})

test_that("convert_datetime_format returns datetimes in r format", {
  new_datetimes <- convert_datetime_format(c("MM/DD/YYYY",
                                             "YY-MM-DD",
                                             "YYYY-MM-DDThh:mm:ss",
                                             "YYYY-MM-DDThh:mm:ss-hhmm",
                                             "YYYY-MM-DDThh:mm:ss+hhhh",
                                             "YYYY-MM-DDThh:mm:ss-hh:mm",
                                             "YYYY-MM-DDThh:mm+hh",
                                             "YYYY-MM-DDThh:mm:ssZ"))
  fix_z <- convert_datetime_format("YYYY-MM-DDThh:mm:ssZ", convert_z = TRUE)

  expect_equal(new_datetimes, c("%m/%d/%Y",
                                "%y-%m-%d",
                                "%Y-%m-%dT%H:%M:%S",
                                "%Y-%m-%dT%H:%M:%S%z",
                                "%Y-%m-%dT%H:%M:%S%z",
                                "%Y-%m-%dT%H:%M:%S%z",
                                "%Y-%m-%dT%H:%M%z",
                                "%Y-%m-%dT%H:%M:%SZ"))
  expect_equal(fix_z, "%Y-%m-%dT%H:%M:%S%z")
})