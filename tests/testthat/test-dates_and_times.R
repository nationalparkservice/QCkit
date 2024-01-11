test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

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
                                             "YY-MM-DD"))
  expect_equal(new_datetimes, c("%m/%d/%Y", "%y-%m-%d"))
})