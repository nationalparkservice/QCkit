df <- tibble::tibble(scientificName=c("Mimulus guttatus", "guttata", "Mimulus", "Erythranthe", "Phrymaceae"))

test_that("get_taxon_rank adds column called taxonRank", {
  df_new <- get_taxon_rank(df, sciName_col = "scientificName")
  expect_equal(names(df_new), c("scientificName", "taxonRank"))
})

test_that("get_taxon_rank taxonRank column has correct contents", {
  df_new <- get_taxon_rank(df, sciName_col = "scientificName")
  expect_equal(df_new$taxonRank, c("species", "genus", "genus", "genus", "family"))

})



test_that("te_check does not flag common species as species of concern")

