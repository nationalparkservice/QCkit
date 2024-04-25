#' Get all columns from each csv and columns that occur more than once across those csv's
#'
#' @description `get_columns_from_files()` produces two dataframes: one that lists all columns from within each of the .csv's in a specified working directory and another that lists all columns which appear more than once across those .csv's
#'
#' @details `get_columns_from_files()` can be used as an intitial step in data processing, particularly if the goal of the data processing is to merge multiple files into one, larger flat file. The function allows the user to preview and list out what columns exist within any given number of .csv's in a format that is more digestible. If the user chooses, they can also find commonalities across the columns in those files, highlighting any columns that serve as key variables upon which dataframes can then be joined.
#'
#'