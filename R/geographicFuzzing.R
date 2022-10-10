#' Convert coordinates for a location into a polygon to obscure specific location
#'
#' \code{dp_fuzzLocation} "fuzzes" a specific location to something less precise prior to public release of information
#' about sensitive resources for which data are not to be released to the public. This function takes coordinates in either UTM or decimal degrees, converts to UTM (if in decimal degrees), creates a bounding box based on rounding of UTM coordinates to the 100s or 1000s place, and then creating a polygon from the resultant points. The function returns a string in Well-Known-Text
#' format.
#'
#' @param lat latitude in either UTMs or decimal degrees.
#' @param lon longitude in either UTMs or decimal degrees
#' @param coordrefsys coordinate reference system. Either 4326 for decimal degrees / WGS84 datum, or 326xx for UTMs / WGS84 datum, where the xx
#' is the northern UTM zone. For example 32616 is for UTM zone 16N.
#' @param fuzzlevel either 100m or 1000m. Use either "Fuzzed - 1km" or "Fuzzed - 100m"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dp_fuzzLocation(703977, 4035059, 32616, "Fuzzed - 1km")
#' dp_fuzzLocation(36.43909, -84.72429, 4326, "Fuzzed - 1km")
#' }
dp_fuzzLocation <- function(lat, lon, coordrefsys, fuzzlevel) {
  # for decimal degrees, convert to UTM locations and identify proper crs
  if (coordrefsys == 4326) {
    long2UTM <- function(long) {
      ## Function to get the UTM zone for a given longitude
      (floor((long + 180) / 6) %% 60) + 1
    }

    tempcrs <- if (lat > 0) {
      long2UTM(lon) + 32600
    } else {
      long2UTM(lon) + 32700
    }

    point <- sf::st_point(c(lon, lat))
    point <- sf::st_sfc(point, crs = 4326)
    pointutm <- sf::st_transform(x = point, crs = tempcrs)
    locationlat <- pointutm[[1]][1]
    locationlon <- pointutm[[1]][2]
  } else {
    locationlat <- lat
    locationlon <- lon
  }

  # do rounding of UTMs based on fuzzlevel
  if (fuzzlevel == "Fuzzed - 1km") {
    fuzzfactor <- 1000
  } else if (fuzzlevel == "Fuzzed - 100m") {
    fuzzfactor <- 100
  } else {
    fuzzfactor <- 1
  }

  locationlat <- locationlat / fuzzfactor
  locationlon <- locationlon / fuzzfactor
  locationlathi <- ceiling(locationlat) * fuzzfactor
  locationlatlo <- floor(locationlat) * fuzzfactor
  locationlonhi <- ceiling(locationlon) * fuzzfactor
  locationlonlo <- floor(locationlon) * fuzzfactor

  # construct simple feature geometry polygon from UTM points
  polygon_list <- list(rbind(
    c(locationlatlo, locationlonlo),
    c(locationlatlo, locationlonhi),
    c(locationlathi, locationlonhi),
    c(locationlathi, locationlonlo),
    c(locationlatlo, locationlonlo)
  ))
  utmsfg <- sf::st_polygon(polygon_list)

  # convert sfg to sfc with appropriate crs
  if (coordrefsys == 4326) {
    utmsfc <- sf::st_sfc(utmsfg, crs = tempcrs)
  } else {
    utmsfc <- sf::st_sfc(utmsfg, crs = coordrefsys)
  }

  # convert polygon to decimal degrees
  wkt <- sf::st_transform(x = utmsfc, crs = 4326)
  wkt <- sf::st_as_text(wkt)

  # return WKT string
  return(wkt)
}
