unit_codes_to_names <- function(unit_code) {

  unit_names <- NULL

  for (i in 1:length(seq_along(unit_code))) {
    print(i)
    print(unit_code[i])
    url <- paste0("https://irmaservices.nps.gov/Unit/v2/api/",
                  unit_codest[i])
    req <- httr::GET(url)
    status_code <- httr::stop_for_status(req)$status_code
    #if API call fails, alert user and remind them to log on to VPN:
    if (!status_code == 200) {
      stop("DataStore connection failed.")
    }

    #get API request info:
    ref_data <- jsonlite::fromJSON(httr::content(req, "text"))
    park_name <- ref_data$FullName

    unit_names <- append(unit_names, park_name)
  }
  unit_names(data.frame(unit_names))
  return(unit_names)
}