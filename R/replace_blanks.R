#' Replaces all blank cells with NA
#'
#' @details `replace_blanks()` is particularly useful for exporting data from a database (such as access) and converting it to a data package with metadata.
#'
#' `replace_blanks()` will import all .csv files in the current working directory. The files are then written back out to the same directory, overwriting the old .csv files. Any blank cells in the original .csv files will be replaced with NA.
#'
#' One exception is if a .csv contains NO data (i.e. just column names and no data in any of the cells). In this case, the blanks will not be replaced with NA (as the function cannot determine how many NAs to include).
#'
#' @return list of data frames (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' replace_blanks()
#' }
replace_blanks <- function() {
  #get list of .csv file names
  my_path <- list.files(pattern="*.csv")
  #import .csvs as dataframes; each dataframe is an item in the list "my_data"
  my_data <- lapply(my_path, function(x) read_csv(x, show_col_types=FALSE))
  #give each dataframe a name basd on the filename where it originated
  names(my_data) <- gsub(".csv", "", my_path)
  #write each dataframe back to .csv
  lapply(1:length(my_data),
         function(i) write.csv(my_data[[i]],
                               file = paste0(names(my_data[i]), ".csv"),
                               row.names = FALSE))
  return(invisible())
}
