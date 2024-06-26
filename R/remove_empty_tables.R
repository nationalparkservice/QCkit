#' Remove empty tables from a list
#'
#' @param df_list A list of tibbles or dataframes.
#'
#' @return The same list but with empty tables removed.
#' @export
#'
#' @examples
#'
#' test_list <- list(item_a = tibble::tibble,
#'                   item_b = mtcars,
#'                   item_c = iris)
#'
#' tidy_list <- removeEmptyTables(test_list)
#'
removeEmptyTables <- function(df_list) {
  non_empty_list <- purrr::compact(df_list)  # Remove empty dataframes
  tables_removed <- setdiff(names(df_list), names(non_empty_list))  # Get names of removed dataframes

  # Warn about removed empty dataframes
  if (length(tables_removed) > 0) {
    warning(cli::pluralize("{length(tables_removed)} empty table{?s} {?was/were} omitted from dataset. Affected table{?s}: {paste(tables_removed, collapse = ", ")}"))
  }

  return(non_empty_list)
}
