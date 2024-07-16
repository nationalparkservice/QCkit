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


#' Handles multiple missing values
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' `r lifecycle::badge("questioning")`
#' Given a file name (.csv only) and path, the function will search the
#' columns for any that contain multiple user-specified missing value codes.
#' For any column with multiple missing value codes, all the missing values
#' (including blanks) will be replaced with NA. A new column will be generated
#' and, populated with the given missing value code from the origin column.
#' Values that were not missing will be populated with "not_missing". The
#' newly generate column of categorical variables can be used do describe
#' the various/multiple reasons for why data is absent in the original column.
#'
#' The function will then write the new dataframe to a file, overwriting the
#' original file. If it is important to keep a copy of the original file, make
#' a copy prior to running the function.
#'
#' WARNING: this function will replace any blank cells in your data with NA!
#'
#' @details Blank cells will be treated as NA.
#'
#' @param file_name String. The name of the file to inspect
#' @param directory String. Location of file to read/write. Defaults to the current working directory.
#' @param colname `r lifecycle::badge("experimental")` String. The columns to inspect. CURRENTLY ONLY WORKS AS SET TO DEFAULT "NA".
#' @param missing_val_codes List. A list of strings containing the missing value code or codes to search for.
#' @param replace_value String. The value (singular) to replace multiple missing values with. Defaults to NA.
#'
#' @return writes a new dataframe to file. Return invisible.
#' @export
#'
#' @examples
#' \dontrun{
#' document_missing_values(file_name = "mydata.csv",
#'                         directory = here::here(),
#'                         colname = NA, #do not change during function development
#'                         missing_val_codes = c("missing", "blank", "no data"),
#'                         replace_value = NA)
#'                         }
document_missing_values <- function(file_name,
                                             directory = here::here(),
                                             colname = NA,
                                             missing_val_codes = NA,
                                             replace_value = NA) {

  #read in a dataframe:
  df <- readr::read_csv(paste0(directory, "/", file_name),
                        show_col_types = FALSE)
  #generate list of missing values
  missing_val_codes <- append(missing_val_codes, NA)
  missing_val_codes <- unique(missing_val_codes)

  data_names <- colnames(df)

  if (is.na(colname)) {
    y <- ncol(df)
    for (i in 1:y) {
      #if here are multiple missing value codes in a column:
      if (sum(df[[data_names[i]]] %in% missing_val_codes) >
                sum(is.na(df[[data_names[i]]]))) {
        #generate new column of data:
        df$x <- with(df,
                     ifelse(df[[data_names[i]]] %in% missing_val_codes,
                            df[[data_names[i]]], "not_missing"))
        #replace old missing values with replacement value
        df[[data_names[i]]] = ifelse(df[[data_names[i]]] %in%
                                       missing_val_codes,
                                     replace_value, df[[data_names[i]]])
        #rename new column:
        names(df)[names(df) == "x"] <- paste0("custom_",
                                              data_names[i],
                                              "_MissingValues")
      }
    }
  }
  #write the file back out:
  readr::write_csv(df, paste0(directory, "/", file_name))

  return(invisible)

}
