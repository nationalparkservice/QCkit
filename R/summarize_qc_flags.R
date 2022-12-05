#' Create Table of Data Quality Flags Found in a Data Package
#'
#' @description get_dp_flags (dp=data package) returns a data frame that list the number of cells in the entire data package with relevant flags (A, AE, R, P) as well as the total number of non-NA cells in the data package (including data flagging columns). Unweighted Relative Response (RRU) is calculated as the total number of accepted data points (A, AE, and data that are not flagged).
#'
#' @details The function can be run from within the working directory where the data package is, or the directory can be specified. The function only supports .csv files and assumes that all .csv files in the folder are part of the data package. It also assumes that the values A, AE, R, and P have only been used for flagging. It assumes that there are no additional characters in the flagging cells (such as leading or trailing white spaces). NAs are assumed to be empty cells or missing data.
#'
#' @param directory is the path to the data package .csv files (defaults to the current working directory).
#'
#' @param force is a logical. Defaults to `FALSE`. When `force = FALSE` the function prints the resulting dataframe to the screen. setting `force = TRUE` suppresses output to the screen.
#'
#' @return a dataframe named dp_flag that contains the four flags, the count of each flag and total number of data points in the entire data package.
#'
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' \dontrun{
#' get_dp_flags("~/my_data_package_directory")
#' get_dp_flags() # if your current working directory IS the data package directory.
#' }
#'
get_dp_flags <- function(directory = here::here(), force = FALSE) {
  fileList <- list.files(path = directory, pattern = "\\.csv$",
                         full.names = TRUE)

  dfList <- suppressMessages(sapply(fileList, readr::read_csv))

  # count all instances of each flag:
  A_flag <- 0
  AE_flag <- 0
  R_flag <- 0
  P_flag <- 0

  for (i in seq_along(dfList)) {
    # get just flagging columns:
    flags_only <- dfList[[i]] %>% dplyr::select(ends_with("_flag"))

    # count each flag type; don't count NAs. Should count all cells that
    # start with the flagging letter and ignore anything (i.e. Quality
    # Assessment codes)
    A <- suppressWarnings(sum(stringr::str_count(flags_only, "\\bA"), 
                              na.rm = TRUE))
    AE <- suppressWarnings(sum(stringr::str_count(flags_only, "\\bAE"),
                               na.rm = TRUE))
    R <- suppressWarnings(sum(stringr::str_count(flags_only, "\\bR"),
                              na.rm = TRUE))
    P <- suppressWarnings(sum(stringr::str_count(flags_only, "\\bP"),
                              na.rm = TRUE))

    # Increment flag counts:
    A_flag <- sum(A_flag + A)
    AE_flag <- sum(AE_flag + AE)
    R_flag <- sum(R_flag + R)
    P_flag <- sum(P_flag + P)
  }

  # get total number of datapoints across entire data package, exclude NAs:
  Cell_count <- 0
  for (i in seq_along(dfList)) {
    Cell_count <- (Cell_count + sum(!is.na(dfList[[i]])))
  }

  # remove flagging columns from cell count:
  all_flags <- sum(A_flag + AE_flag + R_flag + P_flag)
  Cell_count <- Cell_count - all_flags

  # Accepted count: remove provisional and rejected data proxies from Cell_count
  # (using proxies because it is assumed that for each R or P there is a
  # corresponding data point that should be removed):
  A_count <- (Cell_count - R_flag - P_flag)

  # Calculate RRU as Accepted/all where AE and A are accepted, R & P are not:
  RRU <- (A_count) / Cell_count

  dp_flags <- data.frame(A_flag, AE_flag, R_flag, P_flag, Cell_count, RRU)

  if (force == FALSE) {
    print(dp_flags)
  }

  return(dp_flags)
}



#' Create Table of Data Quality Flags Found in Data Files within a Data Package
#'
#' @description get_df_flags (df = data files) returns a data frame that lists the number of cells in each data file in the entire data package (excluding NAs) with relevant flags (A, AE, R, P) as well as the total number of data points in each .csv (including data flagging columns, but excluding NAs). Unweighted Relative Response (RRU) is calculated as the total number of accepted data points (A, AE, and data that are not flagged).
#'
#' @details The function can be run from within the working directory where the data package is, or the directory can be specified. The function only supports .csv files and assumes that all .csv files in the folder are part of the data package. It also assumes that the values A, AE, R, and P have only been used for flagging. It assumes that there are no additional characters in the flagging cells (such as leading or trailing white spaces).
#'
#'
#' @inheritParams get_dp_flags
#'
#' @return a dataframe named df_flag that contains a row for each .csv file in the directory with the file name, the count of each flag and total number of data points in each .csv (including data flagging columns).
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' get_df_flags("~/my_data_package_directory")
#' get_df_flags() # if your current working directory IS the data package directory.
#' }
#'
get_df_flags <- function(directory = here::here(), force = FALSE) {
  # get list of .csv files in the specified directory
  fileList <- list.files(path = directory, pattern = "\\.csv$", 
                         full.names = TRUE)

  # import all data from all files
  dfList <- suppressMessages(sapply(fileList, readr::read_csv))

  # get rid of paths in the tibbles for each data file
  names(dfList) <- base::basename(names(dfList))

  df_flags <- NULL # set up empty data frame to populate later

  # for each file in the data package:
  for (i in seq_along(dfList)) {
    # get just flagging columns:
    flags_only <- dfList[[i]] %>% dplyr::select(ends_with("_flag"))

    # count each flag type; don't count NAs. Should count all cells that
    # start with the flagging letter and ignore anything (i.e. Quality
    # Assessment codes)
    A_flag <- suppressWarnings(sum(stringr::str_count(flags_only, "\\bA"),
      na.rm = TRUE
    ))
    AE_flag <- suppressWarnings(sum(stringr::str_count(flags_only, "\\bAE"),
      na.rm = TRUE
    ))
    R_flag <- suppressWarnings(sum(stringr::str_count(flags_only, "\\bR"),
      na.rm = TRUE
    ))
    P_flag <- suppressWarnings(sum(stringr::str_count(flags_only, "\\bP"),
      na.rm = TRUE
    ))
    # do some math:
    accepted_flags <- sum(A_flag + AE_flag)
    not_accepted <- sum(R_flag, P_flag)
    all_flags <- sum(accepted_flags, not_accepted)

    # get cell count in file, exclude NAs and flags:
    Cell_count <- sum(!is.na(dfList[[i]])) - all_flags

    # make a dataframe with data:
    flags <- assign(
      paste0(names(dfList)[i]),
      data.frame(
        names(dfList)[i],
        A_flag, AE_flag, R_flag, P_flag, Cell_count
      )
    )

    # Accepted count: remove provisional and rejected data proxies from
    # Cell_count (using proxies because it is assumed that for each R or P there
    # is a corresponding data point that should be removed):
    A_count <- (Cell_count - R_flag - P_flag)

    # Calculate RRU as accepted data/all data
    flags$RRU <- ((A_count) /
      flags$Cell_count)

    # add to df_flags dataframe:
    df_flags <- rbind(df_flags, flags)
  }

  # rename column
  colnames(df_flags)[1] <- "filename"

  if (force == FALSE) {
    print(df_flags)
  }

  return(df_flags)
}


#' Create Table of Data Quality Flags in Flagging Columns within individual data columns
#'
#' @description get_dc_flags (dc=data columns) returns a data frame that, for each data file in a data package lists the name of each data flagging column and the number of each flag type within that column (A, AE, R, P) as well as the total number of data points in the data flagging columns for each .csv, excluding NAs. Unweighted Relative Response (RRU) is calculated as the total number of accepted data points (A, AE, and data that are not flagged).
#'
#' @details The function can be run from within the working directory where the data package is, or the directory can be specified. The function only supports .csv files and assumes that all data flagging columns have column names ending in "_flag". It assumes that there are no additional characters (other than A, AE, R, P) in the flagging cells (such as leading or trailing white spaces).
#'
#' @inheritParams get_dp_flags
#'
#' @return a dataframe named dc_flag that contains a row for each .csv file in the directory with the file name, the count of each flag and total number of data points in each .csv (including data flagging columns).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_df_flags("~/my_data_package_directory")
#' get_df_flags() # if your current working directory IS the data package directory.
#' }
#'
get_dc_flags <- function(directory = here::here(), force = FALSE) {
  fileList <- list.files(path = directory, pattern = "\\.csv$", 
                         full.names = TRUE)

  dfList <- suppressMessages(sapply(fileList, readr::read_csv))

  names(dfList) <- base::basename(names(dfList))

  dc_flags <- NULL
  for (i in seq_along(dfList)) {
    print(paste0("i=", i))

    # get just flagging columns:
    flags_only <- dfList[[i]] %>% dplyr::select(ends_with("_flag"))

    # for data files with flags:
    if (ncol(flags_only) > 0) {
      # for each column in data each data file with flags:
      for (j in seq_along(flags_only)) {
        print(paste0("j=", j))
        # count each flag type; don't count NAs. Should count all cells that
        # start with the flagging letter and ignore anything (i.e. Quality
        # Assessment codes)
        A_flag <- suppressWarnings(sum(stringr::str_count(flags_only[j], 
                                                          "\\bA"),
                                                          na.rm = TRUE))
        AE_flag <- suppressWarnings(sum(stringr::str_count(flags_only[j],
                                                           "\\bAE"),
                                                          na.rm = TRUE))
        R_flag <- suppressWarnings(sum(stringr::str_count(flags_only[j], 
                                                          "\\bR"),
                                                          na.rm = TRUE))
        P_flag <- suppressWarnings(sum(stringr::str_count(flags_only[j],
                                                          "\\bP"),
                                                          na.rm = TRUE))
        # do some math:
        accepted_flags <- sum(A_flag, AE_flag)
        not_accepted <- sum(R_flag, P_flag)
        all_flags <- sum(accepted_flags, not_accepted)

        # get cell count in file, exclude NAs and flags:
        Cell_count <- sum(!is.na(flags_only[j]))

        filename <- names(dfList)[i]
        flagged_col <- colnames(flags_only)[j]

        # make a dataframe with data:
        flags <- assign(
          paste0(names(dfList)[i]),
          data.frame(
            names(dfList)[i], flagged_col,
            A_flag, AE_flag, R_flag, P_flag, Cell_count
          )
        )

        # Accepted count: remove provisional and rejected data proxies from
        # Cell_count (using proxies because it is assumed that for each R or P
        # there is a corresponding data point that should be removed):
        A_count <- (Cell_count - R_flag - P_flag)

        # Calculate RRU as accepted data/all data
        flags$RRU <- (A_count / Cell_count)

        colnames(flags)[1] <- "filename"

        # add to df_flags dataframe:
        dc_flags <- rbind(dc_flags, flags)
      }
    }

    # if there are no flagging columns:
    if (ncol(flags_only) == 0) {
      filename <- names(dfList)[i]
      flagged_col <- "No columns flagged"
      A_flag <- NA
      AE_flag <- NA
      R_flag <- NA
      P_flag <- NA
      Cell_count <- NA
      RRU <- NA

      flags <- data.frame(
        filename, flagged_col, A_flag, AE_flag, R_flag, P_flag,
        Cell_count, RRU
      )

      dc_flags <- rbind(dc_flags, flags)
    }
  }

  colnames(dc_flags)[7] <- "Flagged Data Totals"

  if (force == FALSE) {
    print(dc_flags)
  }
  return(dc_flags)
}

#' Create a table of Data Quality Flags based on a custom set of user defined columns headings.
#'
#' @description get_custom_flags returns a data frame that, for each data file in a data package lists the name of each column selected by the user. By default, any flagged columns are automatically included. The number of each flag type for each column (A, AE, R, P) are reported. Unflagged columns are assumed to have only accepted data. The total number of data points in the specified columns (and data flagging columns for) each .csv are also reported. NAs are excluded. An Unweighted Relative Response (RRU) is calculated as the total number of accepted data points (A, AE, and data that are not flagged) devided by the total number of data points in all specified columns (and the flagged columns).
#'
#' @details Flagged columns must have names ending in "*_flag". Missing values must be specified as NA.
#'
#' @inheritParams get_dp_flags
#' @param cols A comma delimited list of column names.
#'
#' @return a dataframe named cust_flag that contains a row for each column indicated in each .csv file in the directory with the file name, the count of each flag and total number of data points in each .csv (including data flagging columns) and RRU.
#' @export
#'
#' @examples
#' \dontrun{
#' # specify path and columns by hand:
#' get_custom_flags("~/my_data_package_directory", cols = c(
#'   "scientificName",
#'   "locality"
#' ))
#'
#' # get custom column names from your csv, use default working directory. Note that in this example column names come from a single .csv but all the .csvs in the directory will be checked for these column names:
#' cols <- colnames(read.csv("mydata.csv"))[c(1:4, 7, 10)]
#' get_custom_flags(cols = cols) # if your current working directory IS the data package directory.
#' }
get_custom_flags <- function(directory = here::here(),
                             cols = c(""),
                             force = FALSE) {
  fileList <- list.files(path = directory, pattern = "\\.csv$", 
                         full.names = TRUE)

  dfList <- suppressMessages(sapply(fileList, readr::read_csv))

  names(dfList) <- base::basename(names(dfList))

  cust_flags <- NULL
  for (i in seq_along(dfList)) {
    print(paste0("i=", i))


    # get custom columns and flagging columns:
    cust_cols <- dfList[[i]] %>% dplyr::select(any_of(cols))
    if (ncol(cust_cols) > 0) {
      for (j in seq_along(cust_cols)) {
        A_flag <- sum(!is.na(cust_cols[j]))
        AE_flag <- 0
        R_flag <- 0
        P_flag <- 0
        RRU <- 1.0

        Cell_count <- A_flag
        filename <- names(dfList)[i]
        column <- colnames(cust_cols)[j]
        flags <- assign(
          paste0(names(dfList)[i]),
          data.frame(
            names(dfList[i]),
            column,
            A_flag,
            AE_flag,
            R_flag,
            P_flag,
            Cell_count,
            RRU
          )
        )
        colnames(flags)[1] <- "filename"

        # add to df_flags dataframe:
        cust_flags <- rbind(cust_flags, flags)
      }
    }

    # get just flagging columns:
    flags_only <- dfList[[i]] %>% dplyr::select(ends_with("_flag"))

    if (ncol(flags_only) > 0) {
      # for each column in data and each data flags:
      for (j in seq_along(flags_only)) {
        print(paste0("j=", j))
        # count each flag type; don't count NAs. Should count all cells that
        # start with the flagging letter and ignore anything (i.e. Quality
        # Assessment codes)
        A_flag <- suppressWarnings(sum(stringr::str_count(
          flags_only[j],
          "\\bA"
        ), na.rm = TRUE))
        AE_flag <- suppressWarnings(sum(stringr::str_count(
          flags_only[j],
          "\\bAE"
        ), na.rm = TRUE))
        R_flag <- suppressWarnings(sum(stringr::str_count(
          flags_only[j],
          "\\bR"
        ), na.rm = TRUE))
        P_flag <- suppressWarnings(sum(stringr::str_count(
          flags_only[j],
          "\\bP"
        ), na.rm = TRUE))
        # do some math:
        accepted_flags <- sum(A_flag, AE_flag)
        not_accepted <- sum(R_flag, P_flag)
        all_flags <- sum(accepted_flags, not_accepted)

        # get cell count in file, exclude NAs and flags:
        Cell_count <- sum(!is.na(flags_only[j]))

        filename <- names(dfList)[i]
        column <- colnames(flags_only)[j]

        # make a dataframe with data:
        flags <- assign(
          paste0(names(dfList)[i]),
          data.frame(
            names(dfList)[i],
            column,
            A_flag,
            AE_flag,
            R_flag,
            P_flag,
            Cell_count
          )
        )

        # Accepted count: remove provisional and rejected data proxies from
        # Cell_count (using proxies because it is assumed that for each R or P
        # there is a corresponding data point that should be removed):
        A_count <- (Cell_count - R_flag - P_flag)

        # Calculate RRU as accepted data/all data
        flags$RRU <- (A_count / Cell_count)

        colnames(flags)[1] <- "filename"

        # add to df_flags dataframe:
        cust_flags <- rbind(cust_flags, flags)
      }
    }

    # if there are no flagging columns:
    if (ncol(flags_only) == 0 && ncol(cust_cols) == 0) {
      filename <- names(dfList)[i]
      column <- "No columns selected"
      A_flag <- NA
      AE_flag <- NA
      R_flag <- NA
      P_flag <- NA
      Cell_count <- NA
      RRU <- NA

      flags <- data.frame(
        filename, column, A_flag, AE_flag, R_flag, P_flag,
        Cell_count, RRU
      )

      cust_flags <- rbind(cust_flags, flags)
    }
  }
  colnames(cust_flags)[7] <- "Flagged Data Totals"
  if (force == FALSE) {
    print(cust_flags)
  }
  return(cust_flags)
}
