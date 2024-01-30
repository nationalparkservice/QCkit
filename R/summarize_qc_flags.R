#' Create Table of Data Quality Flags Found in a Data Package
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' get_dp_flags (dp=data package) returns a data frame that list
#' the number of cells in the entire data package with relevant flags (A, AE,
#' R, P) as well as the total number of non-NA cells in the data package
#' (including data flagging columns). Unweighted Relative Response (RRU) is
#' calculated as the total number of accepted data points (A, AE, and data that
#' are not flagged).
#'
#' @details The function can be run from within the working directory where the
#'  data package is, or the directory can be specified. The function only
#'  supports .csv files and assumes that all .csv files in the folder are part
#'  of the data package. The function counts cells within "*_flag"
#'  columns that start with one of the flagging characters (A, AE, R, P) and
#'  ignores trailing characters and whitespaces. NAs are assumed to be empty
#'  cells or missing data.
#'
#' @keywords internal
#'
#' @param directory is the path to the data package .csv files (defaults to the
#' current working directory).
#'
#' @return a dataframe named dp_flag that contains the four flags, the count of
#' each flag and total number of data points in the entire data package.
#'
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' \dontrun{
#' get_dp_flags("~/my_data_package_directory")
#' get_dp_flags() # if your current working directory IS the data package
#' directory.
#' # ->
#' get_custom_flags(output="package")
#' }
#'
get_dp_flags <- function(directory = here::here()) {

  lifecycle::deprecate_warn("1.0.1", "get_dp_flags()", "get_custom_flags()")

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

  return(dp_flags)
}



#' Create Table of Data Quality Flags Found in Data Files within a Data Package
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' get_df_flags (df = data files) returns a data frame that lists
#' the number of cells in each data file in the entire data package (excluding
#' NAs) with relevant flags (A, AE, R, P) as well as the total number of data
#' points in each .csv (including data flagging columns, but excluding NAs).
#' Unweighted Relative Response (RRU) is calculated as the total number of
#' accepted data points (A, AE, and data that are not flagged).
#'
#' @details The function can be run from within the working directory where the
#' data package is, or the directory can be specified. The function only
#' supports .csv files and assumes that all .csv files in the folder are part
#' of the data package. It also assumes that the values A, AE, R, and P have
#' only been used for flagging. It assumes that there are no additional
#' characters in the flagging cells (such as leading or trailing white spaces).
#'
#' @inheritParams get_dp_flags
#'
#' @keywords internal
#'
#' @return a dataframe named df_flag that contains a row for each .csv file in
#' the directory with the file name, the count of each flag and total number of
#' data points in each .csv (including data flagging columns).
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' get_df_flags("~/my_data_package_directory")
#' get_df_flags() # if your current working directory IS the data package
#' directory.
#' # ->
#' get_custom_flags(output="files")
#' }
#'
get_df_flags <- function(directory = here::here()) {

  lifecycle::deprecate_warn("1.0.1", "get_dp_flags()", "get_custom_flags()")

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
    flags$RRU <- ((A_count) / flags$Cell_count)

    # add to df_flags dataframe:
    df_flags <- rbind(df_flags, flags)
  }

  # rename column
  colnames(df_flags)[1] <- "filename"

  return(df_flags)
}


#' Create Table of Data Quality Flags in Flagging Columns within individual
#' data columns
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' get_dc_flags (dc=data columns) returns a data frame that, for
#' each data file in a data package lists the name of each data flagging column
#' and the number of each flag type within that column (A, AE, R, P) as well as
#' the total number of data points in the data flagging columns for each .csv,
#' excluding NAs. Unweighted Relative Response (RRU) is calculated as the total
#' number of accepted data points (A, AE, and data that are not flagged).
#'
#' @details The function can be run from within the working directory where the
#' data package is, or the directory can be specified. The function only
#' supports .csv files and assumes that all data flagging columns have column
#' names ending in "_flag". It counts cells within those columns that start with
#' one of the flagging characters (A, AE, R, P) and ignores trailing characters
#' and whitespaces.
#'
#' @inheritParams get_dp_flags
#'
#' @keywords internal
#'
#' @return a dataframe named dc_flag that contains a row for each .csv file in
#' the directory with the file name, the count of each flag and total number of
#' data points in each .csv (including data flagging columns).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_df_flags("~/my_data_package_directory")
#' get_df_flags() # if your current working directory IS the data package
#' directory.
#' # ->
#' get_custom_flags(output="columns")
#' }
#'
get_dc_flags <- function(directory = here::here()) {

  lifecycle::deprecate_warn("1.0.1", "get_dp_flags()", "get_custom_flags()")

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

  return(dc_flags)
}

#' Creates dataframe(s) summarizing data quality
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' get_custom_flags returns data frames that that summarize data
#' quality control flags (one that summarizes at the data file level and one for each column). The summaries include all data
#' with quality control flagging (a column name that ends in "_flag") and
#' optionally any additional custom columns the user specifies, either by column
#' name or number.
#'
#' The use can specify which of the 2 data frames (or all as a list of
#' dataframes) should be returned.
#'
#' The number of each flag type for each column (A, AE, R, P) is reported.
#' Unflagged columns are assumed to have only accepted (or missing) data. The
#' total number of data points in the specified columns (and data flagging
#' columns for) each .csv are also reported. NAs considered missign data. An
#' Unweighted Relative Response (RRU) is calculated as the total number of
#' accepted data points (A, AE, and data that are not flagged) divided by the
#'total number of data points (excluding missing values) in all specified
#' columns (and the flagged columns).
#'
#' @details Flagged columns must have names ending in "_flag". Missing values
#' must be specified as NA.  The function counts cells within "*_flag" columns
#' that start with one of the flagging characters (A, AE, R, P) and ignores
#' trailing characters and white spaces. For custom columns that do not include
#' a specific flagging column, all non-missing (non-NA) values are considered
#' Accepted (A).
#'
#' The intent of get_custom_flags is for integration into reports on data
#' quality, such as Data Release Reports (DRRs).
#'
#' @param directory is the path to the data package .csv files (defaults to the
#' current working directory).
#' @param cols A comma delimited list of column names. If left unspecified,
#' defaults to just flagged columns.
#' @param output A string indicating what output should be provided. "columns"
#' returns a summary table of QC flags and RRU values in each specified column
#' for every data file. "files" returns a summary table of total QC flags and
#' mean across each data file. "all" will return all three
#' data frames in a single list.
#'
#' @return a dataframe with quality control summary information summarized at
#' the specified level(s).
#'
#' @importFrom stats sd
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' get_custom_flags("~/my_data_package_directory", cols = c("scientificName",
#'                                                          "locality"),
#'                                                          output="all")
#' cols <- colnames(read.csv("mydata.csv"))[c(1:4, 7, 10)]
#' get_custom_flags(cols = cols, output="files")
#' }
get_custom_flags <- function(directory = here::here(),
                             cols = (""),
                             output = c("all",
                                        "files",
                                        "columns")) {

  fileList <- list.files(path = directory, pattern = "\\.csv$",
                         full.names = TRUE)


  dfList <- suppressMessages(lapply(fileList, readr::read_csv))

  names(dfList) <- base::basename(fileList)



  cust_flags <- NULL

  for (i in seq_along(dfList)) {
    # get custom columns:
    cust_cols <- dfList[[i]] %>% dplyr::select(any_of(cols) & !contains("_flag"))
    if (ncol(cust_cols) > 0) {
      for (j in seq_along(cust_cols)) {
        A_flag <- sum(!is.na(cust_cols[j]))
        AE_flag <- 0
        R_flag <- 0
        P_flag <- 0
        RRU <- A_flag / (nrow(cust_cols[j]))
        Cell_count <- nrow(cust_cols[j])
        filename <- names(dfList)[i]
        column <- colnames(cust_cols)[j]
        flags <- assign(
          paste0(names(dfList)[i]),
          tibble(
            filename = names(dfList[i]),
            column,
            Cell_count,
            A_flag,
            AE_flag,
            R_flag,
            P_flag,
            RRU
          )
        )

        # add to df_flags dataframe:
        cust_flags <- rbind(cust_flags, flags)
      }
    }

    # get just flagging columns:
    flags_only <- dfList[[i]] %>% dplyr::select(any_of(cols) & contains("_flag"))

    if (ncol(flags_only) > 0) {
      # for each column in data and each data flags:
      for (j in seq_along(flags_only)) {
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
        Cell_count <- nrow(flags_only[j])

        RRU <- (A_flag + AE_flag) / Cell_count

        filename <- names(dfList)[i]
        column <- colnames(flags_only)[j]

        # make a dataframe with data:
        flags <- assign(
          paste0(names(dfList)[i]),
          tibble(
            filename = names(dfList)[i],
            column,
            Cell_count,
            A_flag,
            AE_flag,
            R_flag,
            P_flag,
            RRU
          )
        )

        # add to df_flags dataframe:
        cust_flags <- rbind(cust_flags, flags)
      }
    }

    # if there are no flagging columns:
    if (ncol(flags_only) == 0 && ncol(cust_cols) == 0) {
      filename <- names(dfList)[i]
      column <- NA
      A_flag <- NA
      AE_flag <- NA
      R_flag <- NA
      P_flag <- NA
      Cell_count <- NA
      RRU <- NA

      flags <- data.frame(
        filename = names(dfList)[i],
        column,
        Cell_count,
        A_flag,
        AE_flag,
        R_flag,
        P_flag,
        RRU
      )

      cust_flags <- rbind(cust_flags, flags)
    }
  }

  #generate summary statistics for each column:
  data_file_summaries <- cust_flags %>%
    dplyr::group_by(filename) %>%
    dplyr::summarize("A" = sum(A_flag),
                     "AE" = sum(AE_flag),
                     "P" = sum(P_flag),
                     "R" = sum(R_flag),
                     "% Accepted" = mean(RRU)) %>%
    rename("File Name" = filename) %>%
    mutate(`% Accepted` = scales::percent(`% Accepted`, accuracy = 0.1))

  cust_flags <- cust_flags %>% mutate(column = str_remove(column, "_flag"), RRU = scales::percent(RRU, accuracy = 0.1)) %>% select("File Name" = filename, "Measure" = column, "Number of Records" = Cell_count, "A" = A_flag, "AE" = AE_flag, "R" = R_flag, "P" = P_flag, "% Accepted" = RRU)


  qc_summary <- list(cust_flags,
                     data_file_summaries)

  names(qc_summary) <- c("Column Level QC Summaries",
                         "Data File Level QC Summaries")

  if (output == "files") {
    return(qc_summary[[2]])
  }
  if (output == "columns") {
    return(qc_summary[[1]])
  }
  if (output == "all") {
    return(qc_summary)
  }
}
