df <- tibble::tibble(scientificName=c("Lynx canadensis", "Mimulus guttatus", "guttata", "Mimulus", "Erythranthe", "Phrymaceae"))

test_that("get_taxon_rank adds column called taxonRank", {
  df_new <- get_taxon_rank(df, sciName_col = "scientificName")
  expect_equal(names(df_new), c("scientificName", "taxonRank"))
})

test_that("get_taxon_rank taxonRank column has correct contents", {
  df_new <- get_taxon_rank(df, sciName_col = "scientificName")
  expect_equal(df_new$taxonRank, c("species", "species", "genus",
                                   "genus", "genus", "family"))
})

test_that("te_check returns correct list of NPS units", {
  nps_unit_list <- "ROMO"
  x<-check_te(df, species_col = "scientificName",
              park_code = nps_unit_list,
              expansion = FALSE)
  expect_equal(x$Park_code[-1], nps_unit_list)
})

test_that("te_check returns no hits for common taxa", {
  df2 <- df[-1,] #remove threatened (in ROMO) species
  x <- check_te(df2, "scientificName", "ROMO", expansion = FALSE)
  expect_equal(nrow(x)*ncol(x), 0)
})

test_that("te_check expansion = TRUE returns correct columns", {
  x <- check_te(df, "scientificName", "ROMO", expansion = TRUE)
  expect_equal(names(x), c("Park_code", "Species", "In_data", "status_code", "status_explanation"))
})

##### Tests for deprecated function:

test_that("te_check is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(te_check(), class = "defunctError")
})

test_that("te_check returns correct list of NPS units", {
  nps_unit_list <- "ROMO"
  x <- suppressWarnings(te_check(df,
                              species_col = "scientificName",
                              park_code = nps_unit_list,
                              expansion = FALSE))
  expect_equal(x$Park_code[-1], nps_unit_list)
})

test_that("te_check returns no hits for common taxa", {
  df2 <- df[-1,] #remove threatened (in ROMO) species
  x <- suppressWarnings(te_check(df2,
                                 "scientificName",
                                 "ROMO",
                                 expansion = FALSE))
  expect_equal(nrow(x)*ncol(x), 0)
})

test_that("te_check expansion = TRUE returns correct columns", {
  x <- suppressWarnings(te_check(df,
                                 "scientificName",
                                 "ROMO",
                                 expansion = TRUE))
  expect_equal(names(x), c("Park_code",
                           "Species",
                           "In_data",
                           "status_code",
                           "status_explanation"))
})