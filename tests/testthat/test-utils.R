test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that(".QC_ds_api returns correct url", {
  expect_equal(.QC_ds_api(x), "https://irmaservices.nps.gov/datastore/v6/rest/")
})

test_that(".QC_ds_secure_api returns correct url", {
  expect_equal(.QC_ds_secure_api(x), "https://irmaservices.nps.gov/datastore-secure/v6/rest/")
})

test_that(".QC_ds_dev_api returns correct url", {
  expect_equal(.QC_ds_dev_api(x), "https://irmadevservices.nps.gov/datastore-secure/v6/rest/")
})