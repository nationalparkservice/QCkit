#' Retrieve the polygon information for the park unit from NPS REST services
#'
#' \code{qc_getParkPolygon} retrieves a geoJSON string for a polygon of a park unit. This is not the official boundary.
#'#'
#' @param Unit_Code is the four-character unit code as designated by NPS.
#'
#' @examples
#' qc_getParkPolygon("OBRI")
qc_getParkPolygonIRMA<-function(Unit_Code){
  # get geography from NPS Rest Services
  UnitsURL<-paste0('https://irmaservices.nps.gov/v2/rest/unit/',Unit_Code,'/geography')
  xml<-httr::content(httr::GET(UnitsURL))

  # Create spatial feature from polygon info returned from NPS
  parkpolygon<-sf::st_as_sfc(xml[[1]]$Geography,geoJSON=TRUE)

  return(parkpolygon)

}

#' Check whether a coordinate pair is within the polygon of a park unit
#'
#' \code{qc_ValidateCoordinates} compares a coordinate pair (in decimal degrees) to the polygon for a park unit as provided through the NPS
#' Units rest services. The function returns a value of TRUE or FALSE.
#'
#'
#' @param Unit_Code is the four-character unit code as designated by NPS.
#' @param lat latitude, in decimal degrees
#' @param lon longitude, in decimal degrees.
#' @param validationsource is either "IRMA" (for the polygon provided by the NPS rest services), or
#' "OPBOUNDS" for the park boundary included in the operational boundaries data set (not currently functional).
#'
#' @examples
#' qc_ValidateCoordinates("OBRI",36.07951,-84.65610,"IRMA")
qc_ValidateCooridnates<-function(Unit_Code,lat,lon,validationsource){

  # get geography from NPS Rest Services
  park<-qc_getParkPolygonIRMA(Unit_Code)

  # Create point feature from latitude and longitude
  point<-sf::st_point(c(lon,lat),dim="XY")

  # Test whether the coordinates provided are within the polygon spatial feature
  result<-sf::st_covers(park,point,sparse=FALSE)[,1]

  return(result)
}


