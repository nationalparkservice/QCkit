#' Add elevation to a dataset
#'
#' @description `get_elevation()` takes a dataframe that includes GPS coordinates (in decimal degrees) and returns a dataframe with two new columns added to it, minimumElevationInMeters and maximumElevationInMeters. The function requires that the data supplied are numeric and that missing values are specified with NA.
#'
#' @details `get_elevation()` uses the USGS API for [The National Map](https://apps.nationalmap.gov/epqs/) to identify the evevation for a given set of GPS coordinates. To reduce API queries (and time to completion), the function will only search for unique GPS coordinates in your dataframe. This could take some time. If you have lots of GPS coordinates, you can also perform a [manual bulk upload](https://apps.nationalmap.gov/bulkpqs/) (maximum = 500 points).
#'
#' Note that both new columns (minimumElevationInMeters and maximumElevationInMeters) contain the same elevation; this is expected behavior as a single GPS coordinate should have the same maximum and minimum elevations. The column names are generated in accordance with the simple [Darwin Core Standards](https://dwc.tdwg.org/).
#'
#' Points outside of the US may return NA values as they are not part of The National Map.
#'
#'
#' @param df a data frame containing GPS decimal coordinates for individual points with latitude and longitude in separate columns.
#' @param decimal_lat String. The name of the column containing longitudes
#' @param decimal_long String. The name of the column containing latitudes
#' @param spatial_ref Categorical. Defaults to 4326. Can also be set to 102100.
#' @param force Logical. Defaults to FALSE. Returns verbose comments, interactions, and information. Set to TRUE to remove all interactive components and reduce/remove all comments and informative print statements.
#'
#' @return a data frame with two new columns, minimumElevationInMeters and maximumElevationInMeters
#' @export
#'
#' @examples
#'  \dontrun{
#'  new_dataframe <- get_elevation(dataframe, "decimalLatitude", "decimalLongitude", spatial_ref="4326")
#'  new_dataframe <- get_elevation(dataframe, "decimalLatitude", "decimalLongitude", spatial_ref="102100", force=TRUE)
#'  }
get_elevation <- function(df,
                          decimal_lat,
                          decimal_long,
                          spatial_ref = c(4326, 102100),
                          force = FALSE){

  #check that spatial ref is either 4326 or 102100:
  spatial_ref <- match.arg(spatial_ref)

  #reduce data frame to just unique gps coordinates
  df2 <- unique(df[,c(decimal_lat, decimal_long)])

  #test & warn for correct lat/long specification:
  if(force == FALSE){
    lat_test <- df2 %>% dplyr::filter(df2[,1] < 0)
    if(nrow(lat_test > 0)){
      cat("Some latitudes appear to be below the equator. Make sure you correctly designated latitude and longitude.\n")
    }
    long_test <- df2 %>% dplyr::filter(df2[,2] > 0)
    if(nrow(long_test > 0)){
      cat("Some latitudes appear to be in the Eastern Hemisphere. Makre sure you correctly designated latitude and longitude.\n")
    }
  }
  #test for numeric data:
  if(!is.numeric(df2[,1]) | !is.numeric(df2[,2])){
    message <- "Non-numeric columns supplied. Please supply colums with decimal based GPS coordinates."
    stop(message)
  }
  elev <- NULL
  for(i in 1:nrow(df2)){
    lat <- df2[i,1]
    long <- df2[i, 2]
    if(!is.na(lat) | !is.na(long)){
      url <- paste0("https://epqs.nationalmap.gov/v1/json?x=",
                               long, "&y=",
                               lat, "&wkid=",
                               spatial_ref, "&units=Meters",
                               "&includDate=false")
      req <- httr::GET(url)
      #if the response was good:
      if(req$status_code == 200){
        elevation <- httr::content(req)$value
        elev <- append(elev, elevation)
        } else{
        if(force == FALSE){
          elev <- append (elev, NA)
          cat("Bad response for ",
              crayon::blue$bold(lat), ", ",
              crayon::blue$bold(long), ".\n",
              sep = "")
          cat("Missing values (NA) generated.\n")
          }
        }
    } else{
      elev <- append (elev, NA)
    }
  }
  df2$minimumElevationInMeters <- elev
  df2$maximumElevationInMeters <- elev

  #merge elevation data back into original data frame; return a new df (df3)
  df3 <- df %>% dplyr::right_join(df2, by=c(decimal_lat, decimal_long))
}
