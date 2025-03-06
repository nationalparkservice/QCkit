#' Converts unit codes to full unit (park) names
#'
#' @description `unit_code_to_names` takes a single unit code or vector of unit codes and returns a data frame of full unit names using a public IRMA API. For example if given the code "ROMO" the function will return "Rocky Mountain National Park".
#'
#' @param unit_code string or list of strings consisting of (typically) four-letter unit codes.
#'
#' @returns a data frame consisting of a single column of full park names
#' @export
#'
#' @examples
#' \dontrun{
#'  unit_codes_to_names("ROMO")
#'  unit_codes_to_names(c("ROMO", "GRYN"))
#'  }
unit_codes_to_names <- function(unit_code) {

  #set up object to store full park names in
  unit_names <- NULL

  #loop through each unit code in "unit_codes"
  for (i in 1:length(seq_along(unit_code))) {
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
    #extract the full unit (park) name
    park_name <- ref_data$FullName

    #add the newest full unit (park) name to the growing list of names
    unit_names <- append(unit_names, park_name)
  }
  #turn the list into a dataframe (make it easier to add to a data file)
  unit_names <- data.frame(unit_names)
  #return the data frame to the user
  return(unit_names)
}