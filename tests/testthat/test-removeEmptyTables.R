test_that("remove_empty_tables works", {
  my_list <- list(item_a = tibble::tibble(),
                  item_b = mtcars,
                  item_c = iris)
  expect_warning(remove_empty_tables(my_list))
  expect_equal(names(suppressWarnings(remove_empty_tables(my_list))), c("item_b", "item_c"))

  my_list$item_d <- tibble::tibble()
  expect_warning(remove_empty_tables(my_list))
  expect_equal(names(suppressWarnings(remove_empty_tables(my_list))), c("item_b", "item_c"))

  tidy_list <- suppressWarnings(remove_empty_tables(my_list))
  expect_no_warning(remove_empty_tables(tidy_list))
  expect_equal(remove_empty_tables(tidy_list), tidy_list)
})
