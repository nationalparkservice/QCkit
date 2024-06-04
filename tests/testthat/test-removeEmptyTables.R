test_that("removeEmptyTables works", {
  my_list <- list(item_a = tibble::tibble(),
                  item_b = mtcars,
                  item_c = iris)
  expect_warning(removeEmptyTables(my_list))
  expect_equal(names(suppressWarnings(removeEmptyTables(my_list))), c("item_b", "item_c"))

  my_list$item_d <- tibble::tibble()
  expect_warning(removeEmptyTables(my_list))
  expect_equal(names(suppressWarnings(removeEmptyTables(my_list))), c("item_b", "item_c"))

  tidy_list <- suppressWarnings(removeEmptyTables(my_list))
  expect_no_warning(removeEmptyTables(tidy_list))
  expect_equal(removeEmptyTables(tidy_list), tidy_list)
})
