#' Coordinate Conversion from UTM to Latitude and Longitude 
#'
#' @description UTMtoLL takes your dataframe with UTM coordinates in separate Easting and Northing columns, and adds on an additional two columns with the converted decimalLatitude and decimalLongitude coordinates using the reference coordinate system WGS84.
#' 
#' @details Define the name of your dataframe, the easting and northing columns within it, the UTM zone within which those coordinates are located, and the reference coordinate system (datum). UTM Northing and Easting columns must be in separate columns prior to running the function. 
#' 
#' If a datum is not defined, the function will default to "WGS84".
#' 
#' If there are missing coordinates in your dataframe they will be preserved, however they will be moved to the end of your dataframe. 
#'
#' @param Data - The dataframe with UTM coordinates you would like to convert. Input the name of your dataframe.  
#' @param EastingCol - The name of your Easting UTM column. Input the name in quotations, ie. "EastingCol".
#' @param NorthingCol - The name of your Northing UTM column. Input the name in quotations, ie. "NorthingCol". 
#' @param Zone - The UTM Zone. Input the zone number in quotations, ie. "17".
#'@param Datum - The datum used in the coordinate reference system of your coordinates. Input in quotations, ie. "WGS84"
#'
#' @return The function returns your dataframe, mutated with an additional two columns of decimal Longitude and decimal Latitude. 
#' @export
#'
#' @examples UTMtoLL(Data = mydataframe, EastingCol = "EastingCoords", NorthingCol = "NorthingCoords", Zone = "17", Datum = "WGS84")

UTMtoLL <- function(Data, EastingCol, NorthingCol, Zone, Datum = "WGS84"){

  Base <- as.data.frame(Data)
  Base <- dplyr::rename(Base, "b" = EastingCol,  "a" = NorthingCol)
  
  Mid <- Base[!is.na(Base$"b" & Base$"a"), ]
  
  Mid2 <- Base[is.na(Base$"b" & Base$"a"), ]
  
  Final <- dplyr::select(Mid, "b", "a")
  
  Final[1:2] <- lapply(Final[1:2], FUN = function(z){as.numeric(z)})
  
  
  Final <- cbind(Final$b, Final$a)
  
  
  
  v <- terra::vect(Final, crs=paste0("+proj=utm +zone=", Zone, " +datum=", Datum, " +units=m"))
  v
  
  converted <- terra::project(v, "+proj=longlat +datum=WGS84")
  converted
  
  lonlat <- terra::geom(converted)[, c("x", "y")]
  
  
  Data <-  cbind(Mid, lonlat)
  Data <- plyr::rbind.fill(Data, Mid2)
  Data <- dplyr::rename(Data, EastingCol = "b", NorthingCol = "a", "decimalLongitude" = x , "decimalLatitude" = y) 
  return(Data)
} 

