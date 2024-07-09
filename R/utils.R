#assign global package variables

#initiate new environment accessible from within package:
.pkgglobalenv <- new.env(parent=emptyenv())

#data_store API base URL:
assign("QC_ds_api", "https://irmaservices.nps.gov/datastore/v7/rest/", envir=.pkgglobalenv)

#data_store secure API base URL:
assign("QC_ds_secure_api", "https://irmaservices.nps.gov/datastore-secure/v7/rest/", envir=.pkgglobalenv)

#data_store dev api (requires secure)
assign("QC_ds_dev_api", "https://irmadevservices.nps.gov/datastore-secure/v7/rest/", envir = .pkgglobalenv)

.QC_ds_api <- function(x){
  get("QC_ds_api", envir = .pkgglobalenv)
}

.QC_ds_secure_api <- function(x){
  get("QC_ds_secure_api", envir = .pkgglobalenv)
}

.QC_ds_dev_api <- function(x){
  get("QC_ds_dev_api", envir = .pkgglobalenv)
}

#this gets rid of the "no visible binding for global variable 'x'" error in build checks:
globalVariables(c("any_of",
                  "contains",
                  "ends_with",
                  "filter",
                  "species_col.y",
                  "species_col.x",
                  "x",
                  "y",
                  "capture.output",
                  "title",
                  "% Accepted",
                  "_UTMJOINCOL",
                  "decimalLatitude",
                  "decimalLongitude",
                  "LatLong_CRS"))


#' Retrieves an NPS user's email address
#'
#' @details The function accesses the system username and then uses a powershell wrapper to access NPS active directory and supply information about the user. That information is then parsed down to the user's email address.
#'
#' This function probaby won't work for anyone outside of NPS and likely won't work for anyone who is not using a Windows machine. So build those prerequisites (NPS = FALSE) in when calling it from within other function.
#'
#' @return String. The user's email address.
#' @export
#'
#' @examples
#' \dontrun{
#' email <- get_user_email()
#' }
get_user_email <- function() {
  powershell_command <- '([adsisearcher]\\"(samaccountname=$env:USERNAME)\\").findone().properties.proxyaddresses'

  proxy_addresses <- system2("powershell",
                             args = c("-command",
                                      powershell_command),
                             stdout = TRUE)

  email_address <- stringr::str_subset(proxy_addresses, "@") |>
    stringr::str_remove("SMTP:")

  return(email_address)
}
