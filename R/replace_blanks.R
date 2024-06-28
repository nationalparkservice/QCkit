#' Replaces all blank cells a missing value code of your choice
#'
#' @description `replace_blanks()` is particularly useful for exporting data
#' from a database (such as access) and converting it to a data package with
#' metadata.
#'
#' `replace_blanks()` will import all .csv files in the specified working
#' directory. The files are then written back out to the same directory,
#' overwriting the old .csv files. Any blank cells (or cells with "NA" in the
#' original .csv files) will be replaced with the specified string or integer.
#' If no missing value is specified, the function defaults to replacing all
#' blanks with "NA".
#'
#' Please keep in mind the "missing" is a general term for all data
#' not present in the data file or data package. Although you may have a very
#' good reason for not providing data and that data may not, from the data
#' package creator's perspective, be "missing" (maybe you never intended to
#' collect it) from a data package user's perspective any data that is not in
#' the data package is effectively "missing" from the data package. Therefore,
#' it is critical to document in metadata any data that are absent with an
#' appropriate "missingValueCode" and "missingValueDefinition". These terms are
#' defined by the metadata schema and are broadly used to apply to any data not
#' present.
#'
#' This function will replace all empty cells and all cells with NA with a
#' "missingValueCode" of your choice (although it defaults to NA).
#'
#' @details One exception is if a .csv contains NO data (i.e. just column names
#' and no data in any of the cells). In this case, the blanks will not be
#' replaced with NA (as the function cannot determine how many NAs to include).
#'
#' @param directory String. Path to the file(s) to have blanks replaced with
#' NAs. Defaults to the working directory of the project (here::here())
#'
#' @param missing_val_code String, integer, double, or float. Defaults to NA.
#'
#' @return list of data frames (invisibly)
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' #replaces all blank cells in .csvs in the current directory with NA:
#'  replace_blanks()
#'
#' #replace all blank cells in .csvs in the directory ./test_data with "NODATA"
#'  dir <- here::here("test_data")
#'  replace_blanks(directory = dir, missing_val_code = "NODATA")
#'
#' #replace all blank cells in .csvs in the current directory with -99999
#' replace_blanks(missing_val_code = -99999)
#' }
replace_blanks <- function(directory = here::here(), missing_val_code = NA) {
  #get list of .csv file names
  my_path <- list.files(path = directory, pattern="*.csv",
                        full.names = TRUE)

  #import .csvs as dataframes; each dataframe is an item in the list "my_data"
  my_data <- lapply(my_path,
                    function(x) readr::read_csv(x,
                                                show_col_types = FALSE))
  #if there is only one dataframe, for some reason it imports as a list;
  #change it to a dataframe
  if (length(seq_along(my_path)) < 2) {
    my_data <- data.frame(my_data)
    my_data <- list(my_data)
  }

  #extract just the file name
  my_path <- basename(my_path)

  #give each dataframe a name based on the filename where it originated
  names(my_data) <- gsub(".csv", "", my_path)

  #replace all <NA> with the designated missing value code.
  if (!is.na(missing_val_code)) {
    for (i in seq_along(my_data)) {
    my_data[[i]] <- data.frame(lapply(my_data[[i]],
                                 as.character))
    }
    for (i in seq_along(my_data)) {
      my_data[[i]][is.na(my_data[[i]])] <- missing_val_code

      #my_data[[i]] <- my_data[[i]] %>%
      #  dplyr::mutate
    }
  }

  #write each dataframe back to .csv
  for (i in seq_along(my_data)) {
    readr::write_csv(my_data[[i]], file = paste0(directory, "/",
                                                 names(my_data)[[i]], ".csv"))
  }
  return(invisible())
}
