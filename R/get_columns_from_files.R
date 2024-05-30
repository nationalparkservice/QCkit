#' Retrieve all columns from each dataset and columns that occur more than once across those datasets
#'
#' @description `get_columns_from_files()` produces two dataframes: one that lists all columns from within each of the .csv's in a specified working directory and another that lists all columns which appear more than once across those .csv's.
#'
#' @details `get_columns_from_files()` can be used as an initial step in data processing, particularly if the goal of the data processing is to merge multiple files into one, larger flat file. The function allows the user to preview and list out what columns exist within any given number of .csv's in a format that is more digestible. If the user chooses, they can also find commonalities across the columns in those files, highlighting any columns that serve as key variables upon which dataframes can then be joined.
#'
#' @param wd String. Your specified working directory; points to the directory where the .csv files you want to work with live. I.e., `getwd()`, `setwd("./data examples")`.
#'
#' @param common Logical. Defaults to `FALSE`. In default status, the function returns a full dataframe of all columns within each of the files in your working directory. If set to `TRUE`, the function returns a single list of columns that occur more than once across all of your files.
#'
#' @return one of two dataframes. If common set to `FALSE`, returns a dataframe of all columns in the files within your working directory. If common set to `TRUE`, returns a list of common columns across your files.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_columns_from_files(wd = setwd("./data examples"), common = TRUE)
#' }
#'
get_columns_from_files <- function(wd = getwd(), common = FALSE){

fileList <- list.files(path = wd, pattern = "\\.csv$") # create a list with the names of all csv's

dfList <- suppressMessages(lapply(fileList, readr::read_csv)) # create a nested list of all datasets

names(dfList) <- fileList # name each list within dfList by the name of it's respective file name

maxCols <- list() # create an empty list to find the max number of columns from the provided datasets

for(i in 1:max(length(dfList))){
  maxCols[i] <- length(names(dfList[[i]]))
} # fill in the list with the number of columns from each dataset

maxCols <- max(unlist(maxCols)) # find the max column number from the list

allCols <- tibble::tibble(.rows = maxCols) # create an empty dataframe with the maximum number of rows being the maximum number of columns from the datasets provided

for(i in 1:max(length(dfList))){

  columns <- names(dfList[[i]])

  length(columns) <- maxCols

  tempdf <- tibble::tibble(columns)
  names(tempdf)[1] <- names(dfList)[i]

  allCols <- cbind(allCols, tempdf)

} # add a list of all columns from each provided dataset to the empty dataframe

df <- unlist(allCols)

y <- list()
z <- list()


for(i in 1:length(df)){
  y[i] <- length(which(df == df[i]))
} # find how many times a value within the column names is repeated. Generates a list with the number of times each respective column name is repeated across all column names


for(i in 1:length(y)){
  ifelse(
    y[i] > 1,
    z[i] <- df[i],
    next)
} # if a column name appears more than once across all column names, capture that column in a list (z)

z <- Filter(Negate(is.null), z) # get rid of Null values
commonCols <- z %>% unique() %>% tibble::tibble() # Filter to a unique list of the column names that appear more than once
colnames(commonCols)[1] <- "Common Columns"

ifelse(common == FALSE,
       return(allCols),
       return(commonCols)
)


}