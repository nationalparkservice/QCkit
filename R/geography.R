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


#' Return UTM Zone
#'
#' @description `long2UTM()` take a longitude coordinate and returns the corresponding UTM zone.
#'
#' @details Input a longitude (decimal degree) coordinate and this simple function returns the number of the UTM zone where that point falls.
#'
#' @param lon - Decimal degree longitude value
#'
#' @return The function returns a numeric UTM zone (between 1 and 60).
#' @export
long2UTM <- function(lon) {
  ## Function to get the UTM zone for a given longitude
  return((floor((lon + 180) / 6) %% 60) + 1)
}

#' Convert Coordinates Into a Polygon to Obscure Specific Location
#'
#' @description `fuzz_location()` "fuzzes" a specific location to something less precise prior to public release of information about sensitive resources for which data are not to be released to the public. This function takes coordinates in either UTM or decimal degrees, converts to UTM (if in decimal degrees), creates a bounding box based on rounding of UTM coordinates, and then creates a polygon from the resultant points. The function returns a string in Well-Known-Text format.
#'
#' @details Details will be defined later.
#' 
#' @param lat - The latitude in either UTMs or decimal degrees.
#' @param lon - The longitude in either UTMs or decimal degrees
#' @param coord_ref_sys - The EPSG coordinate system of the latitude and longitude coordinates. Either 4326 for decimal degrees/WGS84 datum, 4269 for decimal degrees/NAD83, or 326xx for UTM/WGS84 datum (where the xx is the northern UTM zone). For example 32616 is for UTM zone 16N.
#' @param fuzz_level - Use "Fuzzed - 10km", "Fuzzed - 1km", or "Fuzzed - 100m"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fuzz_location(703977, 4035059, 32616, "Fuzzed - 1km")
#' fuzz_location(36.43909, -84.72429, 4326, "Fuzzed - 1km")
#' }
fuzz_location <- function(lat, lon, coord_ref_sys = 4326, fuzz_level = "Fuzzed - 1km") {
  #ensure require inputs are there
  if (is.numeric(lat) == FALSE || is.numeric(lon) == FALSE) {
    cat("ERROR: Latitude or longitude are missing or non-numeric.")
    return("")
    #stop()
  }
  #for decimal degrees, convert to UTM locations and identify proper CRS
  if (coord_ref_sys == 4326 || coord_ref_sys == 4269) {
    #coordinates are in decimal degrees WGS84 or NAD83 and we need to convert to UTM; find the appropriate UTM EPSG code
    if (lat > 0) {
      #northern hemisphere (N) UTM zones start at 32601 and go to 32660
      tempcrs <- long2UTM(lon) + 32600
    } else {
      #southern hemisphere (S) UTM zones start at 32701 and go to 32760
      tempcrs <- long2UTM(lon) + 32700
    }
    
    #convert the points to UTM given their existing CRS (decimal degree WGS84 or NAD83)
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
    cat("ERROR: CRS is not decimal degree WGS84 or UTM/WGS84. Please provide coordinates in either of these systems.", sep="")
    return("")
    #stop()
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

#' Map WKT geometry (points and polygons)
#'
#' @description `map_WKT()` takes a well-known text (WKT) geometry column and maps points and polygons onto a gray leaflet map. All NA geometry is dropped before mapping.
#'
#' @details Define your dataframe, the column that contains WKT, and an option to map specific geometry types.
#'
#' @param df - The name of the data frame that contains WKT geometry.
#' @param wellknowntext - The name of the specific column within the data frame that contains the WKT geometry. This parameter is currently not fully implemented and defaults to the Darwin Core 'footprintsWKT'.
#' @param type -  Pick one from "points", "polygons", or "all" to map specific geometry types.
#'
#' @return The function returns a dynamic, zoomable leaflet map with the specific geometry plotted.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' #map species observations
#' map_WKT(my_NPS_species_obs)
#' 
#' #map species observations - points only
#' map_WKT(my_NPS_species_obs, wellknowntext = "footprintWKT", type = "points")
#'
map_WKT <- function(df, wellknowntext = "footprintWKT", type = "all") {
  #convert to spatial data frame
  df <- df %>%
  filter(!is.na(footprintWKT)) %>%
  st_as_sf(wkt = "footprintWKT")

  #new column in data frame for the geometry type
  df$geometry_types <- st_geometry_type(df)

  #use the geometry_type column to filter only for POINT
  df_pts <- df[df$geometry_types == "POINT",]
  #use the geometry_type column to filter only for POLYGON
  df_polys <- df[df$geometry_types == "POLYGON",]
  
  ## Make a cool map!
  map <- leaflet(df, options = leafletOptions(preferCanvas = TRUE)) %>%
    #addTiles(group = "OSM (default)") %>%
    
    addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(
      updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
      updateWhenIdle = TRUE           # map won't load new tiles when panning
    )) %>%
    
    addCircles(
      data = df_pts, # data source is the filtered vector we created called all.point.geometry
      color = "blue",
    ) %>% 
    
    addPolygons(
      data = df_polys, # data source is the filtered vector we created called all.polygons.geometry
      color = "red",
    )
  return(map)
}