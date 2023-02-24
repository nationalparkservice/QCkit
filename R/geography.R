#' Retrieve the polygon information for the park unit from NPS REST services
#'
#' \code{get_park_polygon} retrieves a geoJSON string for a polygon of a park unit. This is not the official boundary.
#' #'
#' @param unit_code is the four-character unit code as designated by NPS.
#'
#' @export
#' @examples
#' \dontrun{
#' qc_getParkPolygon("OBRI")
#' }
get_park_polygon <- function(unit_code) {
  # get geography from NPS Rest Services
  UnitsURL <- paste0("https://irmaservices.nps.gov/v2/rest/unit/", unit_code, "/geography")
  xml <- httr::content(httr::GET(UnitsURL))

  # Create spatial feature from polygon info returned from NPS
  park_polygon <- sf::st_as_sfc(xml[[1]]$Geography, geoJSON = TRUE)

  return(park_polygon)
}

#' Check whether a coordinate pair is within the polygon of a park unit
#'
#' \code{validate_coord} compares a coordinate pair (in decimal degrees) to the polygon for a park unit as provided through the NPS
#' Units rest services. The function returns a value of TRUE or FALSE.
#'
#'
#' @param unit_code is the four-character unit code as designated by NPS.
#' @param lat latitude, in decimal degrees
#' @param lon longitude, in decimal degrees.
#'
#' @examples
#' \dontrun{
#' qc_ValidateCoordinates("OBRI", 36.07951, -84.65610)
#' }
validate_coord <- function(unit_code, lat, lon) {
  # get geography from NPS Rest Services
  park <- get_park_polygon(unit_code)

  # Create point feature from latitude and longitude
  point <- sf::st_point(c(lon, lat), dim = "XY")

  # Test whether the coordinates provided are within the polygon spatial feature
  result <- sf::st_covers(park, point, sparse = FALSE)[, 1]

  return(result)
}

QCkit::fuzz_location()
QCkit::te_check()


#' Convert Coordinates Into a Polygon to Obscure Specific Location
#'
#' @description `fuzz_location()` "fuzzes" a specific location to something less precise prior to public release of information about sensitive resources for which data are not to be released to the public. This function takes coordinates in either UTM or decimal degrees, converts to UTM (if in decimal degrees), creates a bounding box based on rounding of UTM coordinates, and then creates a polygon from the resultant points. The function returns a string in Well-Known-Text format.
#'
#' @details Details will be defined later.
#'
#' \code{fuzz_location} 
#' 
#' @param lat latitude in either UTMs or decimal degrees.
#' @param lon longitude in either UTMs or decimal degrees
#' @param coord_ref_sys coordinate reference system. Either 4326 for decimal degrees / WGS84 datum, or 326xx for UTMs / WGS84 datum, where the xx
#' is the northern UTM zone. For example 32616 is for UTM zone 16N.
#' @param fuzz_level either 100m or 1000m. Use either "Fuzzed - 1km" or "Fuzzed - 100m"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fuzz_location(703977, 4035059, 32616, "Fuzzed - 1km")
#' fuzz_location(36.43909, -84.72429, 4326, "Fuzzed - 1km")
#' }
fuzz_location <- function(lat, lon, coord_ref_sys, fuzz_level) {
  # for decimal degrees, convert to UTM locations and identify proper crs
  if (coord_ref_sys == 4326) {
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

  # do rounding of UTMs based on fuzz_level
  if (fuzz_level == "Fuzzed - 1km") {
    fuzzfactor <- 1000
  } else if (fuzz_level == "Fuzzed - 100m") {
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
  if (coord_ref_sys == 4326) {
    utmsfc <- sf::st_sfc(utmsfg, crs = tempcrs)
  } else {
    utmsfc <- sf::st_sfc(utmsfg, crs = coord_ref_sys)
  }

  # convert polygon to decimal degrees
  wkt <- sf::st_transform(x = utmsfc, crs = 4326)
  wkt <- sf::st_as_text(wkt)

  # return WKT string
  return(wkt)
}

#' Coordinate Conversion from UTM to Latitude and Longitude
#'
#' @description utm_to_ll takes your dataframe with UTM coordinates in separate Easting and Northing columns, and adds on an additional two columns with the converted decimalLatitude and decimalLongitude coordinates using the reference coordinate system WGS84.
#'
#' @details Define the name of your dataframe, the easting and northing columns within it, the UTM zone within which those coordinates are located, and the reference coordinate system (datum). UTM Northing and Easting columns must be in separate columns prior to running the function. If a datum is not defined, the function will default to "WGS84". If there are missing coordinates in your dataframe they will be preserved, however they will be moved to the end of your dataframe. Note that some parameter names are not in snake_case but instead reflect DarwinCore naming conventions.
#'
#' @param df - The dataframe with UTM coordinates you would like to convert. Input the name of your dataframe.
#' @param EastingCol - The name of your Easting UTM column. Input the name in quotations, ie. "EastingCol".
#' @param NorthingCol - The name of your Northing UTM column. Input the name in quotations, ie. "NorthingCol".
#' @param zone - The UTM Zone. Input the zone number in quotations, ie. "17".
#' @param datum - The datum used in the coordinate reference system of your coordinates. Input in quotations, ie. "WGS84"
#'
#' @return The function returns your dataframe, mutated with an additional two columns of decimal Longitude and decimal Latitude.
#' @export
#'
#' @examples
#' \dontrun{
#' utm_to_ll(
#'   df = mydataframe,
#'   EastingCol = "EastingCoords",
#'   NorthingCol = "NorthingCoords",
#'   zone = "17",
#'   datum = "WGS84"
#' )
#' }
utm_to_ll <- function(df, EastingCol, NorthingCol, zone, datum = "WGS84") {
  Base <- as.data.frame(df)
  Base <- dplyr::rename(Base, "b" = EastingCol, "a" = NorthingCol)
  Mid <- Base[!is.na(Base$"b" & Base$"a"), ]
  Mid2 <- Base[is.na(Base$"b" & Base$"a"), ]
  Final <- dplyr::select(Mid, "b", "a")
  Final[1:2] <- lapply(Final[1:2], FUN = function(z) {
    as.numeric(z)
  })

  Final <- cbind(Final$b, Final$a)

  v <- terra::vect(Final, crs = paste0(
    "+proj=utm +zone=",
    zone,
    " +datum=",
    datum, "
                                       +units=m"
  ))

  converted <- terra::project(v, "+proj=longlat +datum=WGS84")

  lonlat <- terra::geom(converted)[, c("x", "y")]

  df <- cbind(Mid, lonlat)
  df <- plyr::rbind.fill(df, Mid2)
  df <- dplyr::rename(df,
    EastingCol = "b", NorthingCol = "a",
    "decimalLongitude" = x, "decimalLatitude" = y
  )
  return(df)
}
