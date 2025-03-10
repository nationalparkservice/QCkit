#' Adds a unit name column into a data frame with a unit code column
#'
#' @description `get_park_names` takes a data frame and the name or index number of the column containing unit codes
#' and returns the same data frame with an added column containing the corresponding park names using a public IRMA API.
#' For example if given the code "ROMO" the function will add "Rocky Mountain National Park" into the park name column.
#' If some park codes are not found, prints a statement with the list of codes it could not recognize.
#'
#' @param df is a dataframe with a unit code column
#' @param unit_column defaults to `Park_Code`, is the index number (int) or the name (char) of the column containing unit codes
#'
#' @returns the dataframe df with a new column parkName containing the expanded unit codes
#' @export
#' @examples
#' \dontrun{
#'  get_park_names(exampleDF)
#'  get_park_names(exampleDF, 2)
#'  get_park_names(exampleDF, "parkCode")
#'  }

get_park_names <- function(df, unit_column) {

  #vector containing unit codes
  unit_code <- df[[unit_column]]

  #vector that will get populated with unit names
  unit_names <- NULL

  #vector that will get populated with not found unit codes
  unit_codes_na <- NULL

  #copied from Rob's function
  for (i in 1:length(unit_code)) {
    #generate unit code specific URL for API request
    url <- paste0("https://irmaservices.nps.gov/Unit/v2/api/",
                  unit_code[i])
    #request information from IRMA API
    req <- httr::GET(url)
    #get request status
    status_code <- httr::stop_for_status(req)$status_code
    #if API call fails, alert user and remind them to log on to VPN:
    if (!status_code == 200) {
      stop("IRMA connection failed.")
    }

    #translate API result into something more useful
    ref_data <- jsonlite::fromJSON(httr::content(req, "text"))

    #if ref_data list is empty (no corresponding park name) assign park_name to NA and add unit code to na list
    #else if ref_data corresponds to more than one park name, assign park_name to NA and add unit code to na list
    #else assign park name to the full unit (park) name
    if (length(ref_data) == 0) {
      park_name <- NA_character_
      unit_codes_na <- append(unit_codes_na, unit_code[i])
    } else if (length(ref_data[[1]]) != 1) {
      park_name <- NA_character_
      unit_codes_na <- append(unit_codes_na, unit_code[i])
    } else {
      park_name <- ref_data$FullName
    }

    #add the newest full unit (park) name to the growing list of names
    unit_names <- append(unit_names, park_name)
  }

  #create new dataframe with unit name column
  df2 <- df %>%
    mutate(parkName = unit_names) %>%
    relocate(parkName, .after = unit_column)

  #warning message for park codes that weren't found
  if (length(unit_codes_na > 0)) {
    unit_codes_na <- as.character(unique(unit_codes_na))
    unit_codes_str <- paste(unit_codes_na, collapse = ", ")
    print(paste("The following unit codes were not found:", unit_codes_str))
  }

  return(df2)
}