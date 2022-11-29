#' Create Table of Data Quality Flags Found in a Data Package
#'
#' @description get_dp_flags (dp=data package) returns a data frame that list the number of cells in the entire data package with relevant flags (A, AE, R, P) as well as the total number of cells in the data package (including data flagging columns). 
#'
#' @details The function can be run from within the working directory where the data package is, or the directory can be specified. The function only supports .csv files and assumes that all .csv files in the folder are part of the data package. It also assumes that the values A, AE, R, and P have only been used for flagging. It assumes that there are no additional characters in the flagging cells (such as leading or trailing white spaces). 
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
#' get_dp_flags() #if your current working directory IS the data package directory.
#' }
#'
get_dp_flags <- function(directory = here::here(), force=FALSE){
  
  fileList <- list.files(path = directory, pattern = "\\.csv$", full.names=TRUE)
  
  dfList <- sapply(fileList, read.csv)
  
  #count all instances of each flag:
  A_flag<-sum(stringr::str_count(unlist(dfList),
                                "\\bA\\b"), na.rm=TRUE)
  AE_flag<-sum(stringr::str_count(unlist(dfList), 
                                 "\\bAE\\b"), na.rm=TRUE)
  R_flag<-sum(stringr::str_count(unlist(dfList), 
                                "\\bR\\b"), na.rm=TRUE)
  P_flag<-sum(stringr::str_count(unlist(dfList), 
                                 "\\bP\\b"), na.rm=TRUE)
  
  #get total number of datapoints across entire data package:
  Cell_count<-0
  for(i in seq_along(dfList)){
    Cell_count<-Cell_count + 
                       (nrow(dfList[[i]]) * 
                       ncol(dfList[[i]]))
  }
  dp_flags <- data.frame(A_flag, AE_flag, R_flag, P_flag, Cell_count)
  
  if(force == FALSE){
  print(dp_flags)
  }
  
  return(dp_flags)
}



#' Create Table of Data Quality Flags Found in Data Files within a Data Package
#'
#' @description get_df_flags (df = data files) returns a data frame that lists the number of cells in each data file in the entire data package with relevant flags (A, AE, R, P) as well as the total number of data points in each .csv (including data flagging columns). 
#'
#' @details The function can be run from within the working directory where the data package is, or the directory can be specified. The function only supports .csv files and assumes that all .csv files in the folder are part of the data package. It also assumes that the values A, AE, R, and P have only been used for flagging. It assumes that there are no additional characters in the flagging cells (such as leading or trailing white spaces). 
#' 
#'
#' @inheritParams get_dp_flags 
#'
#' @return a dataframe named df_flag that contains a row for each .csv file in the directory with the file name, the count of each flag and total number of data points in each .csv (including data flagging columns).
#' 
#' @export
#'
#' @examples 
#' \dontrun{
#' get_df_flags("~/my_data_package_directory")
#' get_df_flags() #if your current working directory IS the data package directory.
#' }
#'
get_df_flags <- function(directory = here::here(), force=FALSE){
  
  fileList <- list.files(path = directory, pattern = "\\.csv$", full.names=TRUE)
  
  dfList <- sapply(fileList, read.csv)
  
  #count all instances of each flag in each .csv:
  df_flags<-NULL
  for(i in seq_along(dfList)){
    A_flag<-sum(stringr::str_count(unlist(dfList[[i]]),
                                 "\\bA\\b"), na.rm=TRUE)
    AE_flag<-sum(stringr::str_count(unlist(dfList[[i]]), 
                                  "\\bAE\\b"), na.rm=TRUE)
    R_flag<-sum(stringr::str_count(unlist(dfList[[i]]), 
                                 "\\bR\\b"), na.rm=TRUE)
    P_flag<-sum(stringr::str_count(unlist(dfList[[i]]), 
                                 "\\bP\\b"), na.rm=TRUE)
  
    #get total number of datapoints in each csv:
    Cell_count <- (nrow(dfList[[i]]) * ncol(dfList[[i]]))

    flags <- assign(paste0(base::basename(names(dfList)[i])), 
                      data.frame(names(dfList)[i],
                      A_flag, AE_flag, R_flag, P_flag, Cell_count))
    df_flags <- rbind(df_flags, flags)
  }
  #rename column
  colnames(df_flags)[1]<-"filename"
  
  if(force == FALSE){
  print(df_flags)
  }
  
  return(df_flags)
}
  

#' Create Table of Data Quality Flags in Flagging Columns within individual data columns
#'
#' @description get_dc_flags (dc=data columns) returns a data frame that, for each data file in a data package lists the name of each data flagging column and the number of each flag type within that column (A, AE, R, P) as well as the total number of data points in the data flagging columns for each .csv. 
#'
#' @details The function can be run from within the working directory where the data package is, or the directory can be specified. The function only supports .csv files and assumes that all data flagging columns have column names ending in "_flag". It assumes that there are no additional characters (other than A, AE, R, P) in the flagging cells (such as leading or trailing white spaces). Discrepancies between the sum of the data flagging values and the total number of the cells in the columns may be due to NAs.
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
#' get_df_flags() #if your current working directory IS the data package directory.
#' }
#'
get_dc_flags <- function(directory = here::here(), force=FALSE){
  
  fileList <- list.files(path = directory, pattern = "\\.csv$", full.names=TRUE)
  
  dfList <- sapply(fileList, read.csv)
  
  dc_flags <- NULL
  for(i in seq_along(dfList)){
    #if there are columns ending in "_flag" do the following:
    if(length(dfList[[i]][, grepl("_flag", names(dfList[[i]]))])>0){
      #make dataframe; same name as .csv but only _flag columns:
      assign(paste0(fileList[i]), dfList[[i]][, grepl("_flag",
                                                      names(dfList[[i]]))])
      
      flags<-NULL
      #for each column, generate a list of flag counts and a list of all cells
      for(j in 1:ncol(get(fileList[i]))){
        A_flag <- sum(stringr::str_count(get(fileList[i])[[j]],
                                       "\\bA\\b"), na.rm=TRUE)
        AE_flag <- sum(stringr::str_count(get(fileList[i])[[j]], 
                                        "\\bAE\\b"), na.rm=TRUE)
        R_flag <- sum(stringr::str_count(get(fileList[i])[[j]], 
                                       "\\bR\\b"), na.rm=TRUE)
        P_flag <- sum(stringr::str_count(get(fileList[i])[[j]], 
                                       "\\bP\\b"), na.rm=TRUE)
        cell_count <- nrow(get(fileList[i])[j])
        filename <- base::basename(fileList[i])
        flagged_col <- colnames(get(fileList[i]))[j]
        
        #create a dataframe
        flags <- data.frame(filename, flagged_col, 
                            A_flag, AE_flag, R_flag, P_flag, cell_count)
        
        #add to existing dataframe
        dc_flags <- rbind (dc_flags, flags)
      }
    }
    # if there's no flagged columns, generate NAs
    if (length(dfList[[i]][, grepl("_flag", names(dfList[[i]]))]) <= 0){
      flags<-NULL  
      filename <-fileList[i]
      flagged_col <- "No columns flagged"
      A_flag <- NA
      AE_flag <- NA
      R_flag <- NA
      P_flag <- NA
      cell_count <- NA
      #generate dataframe
      flags <- data.frame(filename, flagged_col, A_flag, AE_flag, 
                          R_flag, P_flag, cell_count)
      #add to existing dataframe
      dc_flags <- rbind (dc_flags, flags)
    }
  }
  if(force == FALSE){
    print(dc_flags)
  }
  return(dc_flags)
}

  
 