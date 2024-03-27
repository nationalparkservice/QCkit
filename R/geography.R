#' Test whether decimal GPS coordinates are inside a park unit
#'
#' This function can take a list of coordinates and park units as input. In
#' addition to being vectorized, depending on the park borders, it can be a
#' major improvement on `validate_coord()`.
#'
#' @param lat numeric. An individual or vector of numeric values representing
#' the decimal degree latitude of a coordinate
#' @param lon numeric. An individual or vector of numeric values representing
#' the decimal degree longitude of a coordinate
#' @param park_units String. Or list of strings each containing the four letter
#' park unit designation
#'
#' @return logical
#' @export
#'
#' @examples
#' \dontrun{
#' x <- validate_coord_list(lat = 105.555, long = -47.4332, park_units = "DRTO")
#'
#' # or a dataframe with many coordinates and potentially many park units:
#' x <- validate_coord_list(lat = df$decimalLatitutde,
#'                 lon = df$decimalLongitude,
#'                 park_units = df$park_units)
#' # you can then merge it back in to the original dataframe:
#' df$test_GPS_coord <- x
#' }
validate_coord_list <- function(lat, lon, park_units) {
  # get geography from ArcGIS Rest Services
  park <- .get_unit_boundary(park_units)

  #create a multipolygon sf object
  valid_park <- sf::st_make_valid(park)
  # create sf dataframe from coordinates
  points_df <- data.frame(lon = lon, lat = lat)
  points <- sf::st_as_sf(points_df,
                         coords = c("lon", "lat"),
                         crs = 4326,
                         na.fail = FALSE)
  # Test whether the coordinates provided are within the polygon spatial feature
  results <- as.data.frame(sf::st_covers(valid_park,
                                         points,
                                         sparse = FALSE))
  # Turn data frame into a list of logicals
  colnames(results) <- park_units
  rownames(results) <- unique(park_units)
  in_park <- NULL
  for (i in seq(ncol(results))) {
    for (j in seq(nrow(results))) {
      if (colnames(results)[i] == rownames(results)[j]) {
        in_park <- append(in_park, results[j,i])
      }
    }
  }
return(in_park)
}

#' Gets NPS unit boundaries from Arc GIS
#'
#' @param park_units String. Or list of strings.
#'
#' @return sf dataframe
#' @examples
#' \dontrun{
#' .get_unit_boundary("ROMO")
#' }
.get_unit_boundary <- function(park_units) {

  all_localities <- data.frame()

  temp_output <- file.path("temp.geojson")
  feature_service_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/NPS_Land_Resources_Division_Boundary_and_Tract_Data_Service/FeatureServer/2" # NPS unit boundary polygons, updated quarterly

  unique_localities <- unique(park_units)

  for (locality in unique_localities) {
    # Request feature in WGS84 spatial reference (outSR=4326)
    feature_service_path <- paste0(
      "query?where=UNIT_CODE+%3D+%27",
      locality,
      "%27&outFields=*&returnGeometry=true&outSR=4326&f=pjson")
    feature_service_request <- paste(feature_service_url,
                                     feature_service_path,
                                     sep = "/")
    geo_json_feature <- jsonlite::fromJSON(feature_service_request)

    # Have to save to temp file
    json_feature <- utils::download.file(feature_service_request,
                                        temp_output,
                                        mode = "w",
                                        quiet = TRUE)
    # For rgdal 1.2+, layer (format) does not need to be specified
    feature_polygon <- sf::st_read(dsn = temp_output, quiet = TRUE)
    # featurePoly <- readOGR(dsn = tempOutput)

    all_localities <- rbind(all_localities, feature_polygon)
  }
  suppressWarnings(file.remove("temp.geojson"))
  #featurePoly <- readOGR(dsn = tempOutput, layer = "OGRGeoJSON")
  return(all_localities)
}

#' Retrieve the polygon information for the park unit from NPS REST services
#'
#' @description `get_park_polygon()` retrieves a geoJSON string for a polygon of
#'a park unit. This is not the official boundary.
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
  units_url <- paste0("https://irmaservices.nps.gov/v2/rest/unit/",
                      unit_code,
                      "/geography")
  xml <- httr::content(httr::GET(units_url))

  # Create spatial feature from polygon info returned from NPS
  park_polygon <- sf::st_as_sfc(xml[[1]]$Geography, geoJSON = TRUE)

  return(park_polygon)
}

#' Check whether a coordinate pair is within the polygon of a park unit
#'
#' @description `validate_coord()` compares a coordinate pair (in decimal
#' degrees) to the polygon for a park unit as provided through the NPS
#' Units rest services. The function returns a value of TRUE or FALSE.
#'
#'
#' @param unit_code is the four-character unit code as designated by NPS.
#' @param lat latitude, in decimal degrees.
#' @param lon longitude, in decimal degrees.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' validate_coord("OBRI", 36.07951, -84.65610)
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

#' Return UTM Zone
#'
#' @description `get_utm_zone()` replaces `convert_long_2_utm()` as this
#' function name is more descriptive. `get_utm_zone()` takes a longitude
#' coordinate and returns the corresponding UTM zone.
#'
#' @details Input a longitude (decimal degree) coordinate and this simple
#' function returns the number of the UTM zone where that point falls.
#'
#' @param lon - Decimal degree longitude value
#'
#' @return The function returns a numeric UTM zone (between 1 and 60).
#' @export
get_utm_zone <- function(lon) {
  if (lon > 180 | lon < -180) {
    cat("Longitude must be <180 and >-180 decimal degrees.")
    return()
  }
  ## Function to get the UTM zone for a given longitude
  return((floor((lon + 180) / 6) %% 60) + 1)
}

#' Return UTM Zone
#'
#' @description `r lifecycle::badge("deprecated")`
#' `convert_long_2_utm()` was deprecated in favor of `get_utm_zone()` as the
#' new funciton name more accurately reflects what the function does.
#' `convert_long_to_utm()` take a longitude coordinate and returns the
#' corresponding UTM zone.
#'
#' @details Input a longitude (decimal degree) coordinate and this simple
#' function returns the number of the UTM zone where that point falls.
#'
#' @param lon - Decimal degree longitude value
#'
#' @return The function returns a numeric UTM zone (between 1 and 60).
#' @export
convert_long_to_utm <- function(lon) {
  lifecycle::deprecate_soft(when = "0.1.4",
                            "convert_long_to_utm()",
                            "get_utm_zone()")
  ## Function to get the UTM zone for a given longitude
  return((floor((lon + 180) / 6) %% 60) + 1)
}

#' Return UTM Zone
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `long2UTM` was deprecated in favor of `convert_long_to_utm()` to enforce a
#' consistent function naming pattern across the package and to conform to the
#' tidyverse style guide.
#'
#' `long2UTM()` take a longitude coordinate and returns the corresponding UTM
#' zone.
#'
#' @details Input a longitude (decimal degree) coordinate and this simple
#' function returns the number of the UTM zone where that point falls.
#'
#' @param lon - Decimal degree longitude value
#'
#' @return The function returns a numeric UTM zone (between 1 and 60).
#' @keywords internal
#' @export
long2UTM <- function(lon) {
  lifecycle::deprecate_warn("0.1.0.3", "long2UTM()", "convert_long_to_utm()")
  ## Function to get the UTM zone for a given longitude
  return((floor((lon + 180) / 6) %% 60) + 1)
}

#' Convert Coordinates Into a Polygon to Obscure Specific Location
#'
#' @description `fuzz_location()` "fuzzes" a specific location to something less
#' precise prior to public release of information about sensitive resources for
#' which data are not to be released to the public. This function takes
#' coordinates in either UTM or decimal degrees, converts to UTM (if in decimal
#' degrees), creates a bounding box based on rounding of UTM coordinates, and
#' then creates a polygon from the resultant points. The function returns a
#' string in Well-Known-Text format.
#'
#' @details Details will be defined later.
#'
#' @param lat - The latitude in either UTMs or decimal degrees.
#' @param lon - The longitude in either UTMs or decimal degrees
#' @param coord_ref_sys - The EPSG coordinate system of the latitude and
#' longitude coordinates. Either 4326 for decimal degrees/WGS84 datum, 4269 for
#' decimal degrees/NAD83, or 326xx for UTM/WGS84 datum (where the xx is the
#' northern UTM zone). For example 32616 is for UTM zone 16N.
#' @param fuzz_level - Use "Fuzzed - 10km", "Fuzzed - 1km", or "Fuzzed - 100m"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fuzz_location(703977, 4035059, 32616, "Fuzzed - 1km")
#' fuzz_location(36.43909, -84.72429, 4326, "Fuzzed - 1km")
#' }
fuzz_location <- function(lat,
                          lon,
                          coord_ref_sys = 4326,
                          fuzz_level = "Fuzzed - 1km") {
  #ensure require inputs are there
  if (is.numeric(lat) == FALSE || is.numeric(lon) == FALSE) {
    cat("ERROR: Latitude or longitude are missing or non-numeric.")
    return()
  }
  #for decimal degrees, convert to UTM locations and identify proper CRS
  if (coord_ref_sys == 4326 || coord_ref_sys == 4269) {
    #coordinates are in decimal degrees WGS84 or NAD83 and we need to convert to
    #UTM; find the appropriate UTM EPSG code
    if (lat > 0) {
      #northern hemisphere (N) UTM zones start at 32601 and go to 32660
      tempcrs <- get_utm_zone(lon) + 32600
    } else {
      #southern hemisphere (S) UTM zones start at 32701 and go to 32760
      tempcrs <- get_utm_zone(lon) + 32700
    }

    #convert points to UTM given their CRS (decimal degree WGS84 or NAD83)
    point <- sf::st_point(c(lon, lat))
    point <- sf::st_sfc(point, crs = coord_ref_sys)
    pointutm <- sf::st_transform(x = point, crs = tempcrs)
    locationlat <- pointutm[[1]][1]
    locationlon <- pointutm[[1]][2]
  #for UTM, no need to convert to UTM, we can proceed
  } else if (coord_ref_sys >= 32601 && coord_ref_sys <= 32760) {
    locationlat <- lat
    locationlon <- lon
  #not decimal degrees WGS84/NAD83 or UTM, so we don't have a path forward
  } else {
    #throw an error
    cat("ERROR: coord_ref_sys is not decimal degree WGS84 or UTM/WGS84. Please provide coordinates in either of these systems.", sep = "")
    return()
  }

  #do rounding of UTMs based on fuzz_level
  if (fuzz_level == "Fuzzed - 10km") {
    fuzzfactor <- 10000
  } else if (fuzz_level == "Fuzzed - 1km") {
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

  #convert sfg to sfc with appropriate crs
  if (coord_ref_sys == 4326 || coord_ref_sys == 4269) {
    utmsfc <- sf::st_sfc(utmsfg, crs = tempcrs)
  } else {
    utmsfc <- sf::st_sfc(utmsfg, crs = coord_ref_sys)
  }

  #convert polygon to decimal degrees
  wkt <- sf::st_transform(x = utmsfc, crs = 4326)
  wkt <- sf::st_as_text(wkt)

  #return WKT string
  return(wkt)
}

#' Coordinate Conversion from UTM to Latitude and Longitude
#'
#' @description `convert_utm_to_ll()` takes your dataframe with UTM coordinates
#' in separate Easting and Northing columns, and adds on an additional two
#' columns with the converted decimalLatitude and decimalLongitude coordinates
#' using the reference coordinate system WGS84. You may need to turn the VPN OFF
#' for this function to work properly.
#'
#' @details Define the name of your dataframe, the easting and northing columns
#' within it, the UTM zone within which those coordinates are located, and the
#' reference coordinate system (datum). UTM Northing and Easting columns must be
#' in separate columns prior to running the function. If a datum is not defined,
#' the function will default to "WGS84". If there are missing coordinates in
#' your dataframe they will be preserved, however they will be moved to the end
#' of your dataframe. Note that some parameter names are not in snake_case but
#' instead reflect DarwinCore naming conventions.
#'
#' @param df - The dataframe with UTM coordinates you would like to convert.
#' Input the name of your dataframe.
#' @param EastingCol - The name of your Easting UTM column. Input the name in
#' quotations, ie. "EastingCol".
#' @param NorthingCol - The name of your Northing UTM column. Input the name in
#' quotations, ie. "NorthingCol".
#' @param zone - The UTM Zone. Input the zone number in quotations, ie. "17".
#' @param datum - The datum used in the coordinate reference system of your
#' coordinates. Input in quotations, ie. "WGS84"
#'
#' @return The function returns your dataframe, mutated with an additional two
#' columns of decimal Longitude and decimal Latitude.
#' @export
#'
#' @examples
#' \dontrun{
#' convert_utm_to_ll(
#'   df = mydataframe,
#'   EastingCol = "EastingCoords",
#'   NorthingCol = "NorthingCoords",
#'   zone = "17",
#'   datum = "WGS84"
#' )
#' }
convert_utm_to_ll <- function(df,
                              EastingCol,
                              NorthingCol,
                              zone,
                              datum = "WGS84") {
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

#' Coordinate Conversion from UTM to Latitude and Longitude
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `utm_to_ll()` was deprecated in favor of `convert_utm_to_ll()` to enforce a consistent naming scheme for functions across the package and to conform with the tidyverse style guide.
#'
#' utm_to_ll takes your dataframe with UTM coordinates in separate Easting and Northing columns, and adds on an additional two columns with the converted decimalLatitude and decimalLongitude coordinates using the reference coordinate system WGS84.
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
#' @keywords internal
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

  lifecycle::deprecate_warn("0.1.0.3", "utm_to_ll()", "convert_utm_to_ll()")

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
