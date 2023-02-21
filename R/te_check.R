#' Threatened Or Endangered Species Checker Function
#'
#' @description te_check creates a subset of the species list you provide, all of which fall under the Federal Conservation List Taxa for Data Protection. These species should be removed from your dataset if your data is to be made public.
#'
#' @details Define your species dataset name, column name with the scientific names of your species, and your four letter park code.
#'
#' The function downloads the Federal Conservation list using the IRMA odata API service and matches this species list to the list of scientific names in your dataframe.
#'
#' Keep in mind that this is a Federal list, not a state list. Changes in taxa names may also cause some species to be missed.
#'
#' Because the Federal Conservation list is not publicly available (???), you must be logged in to the NPS VPN or in the office to use this function.
#'
#'
#'
#'
#' @param df - The name of your dataframe containing species observations
#' @param species_col - The name of the column within your dataframe containing the scientific names of the species
#' @param park_code -  The four letter park code
#'
#' @return The function returns a list in your global environment called "TorE" with the names of all the species that fall under the federal conservation list.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' te_check(df = my_species_dataframe, species_col = "scientificNames", park_code = "BICY")
#' }
#'
te_check <- function(df, species_col, park_code) {
  
  odata_url <- paste0paste0("https://irmadev.nps.gov/PrototypeCSVtoAPI/odata/FederalConservationListTaxaforDataProtection2272462?$filter=ParkCode%20eq%20%27", park_code, "%27%20or%20ParkCode%20eq%20%27All%27")
  
  fedlist <- ODataQuery::retrieve_data(odata_url)
  
  #subset incoming data:
  fedspp <- as.data.frame(fedlist$value$ProtectedSci)
  fedspp <- cbind(fedspp, fedlist$value$MatchListStatus)
  
  #get date data were pulled from FWS:
  fed_date <- fedlist$value$DateListImported[1]
  fed_date <- substr(fed_date, 1, 10)
  
  #get URL data were accessed from:
  url <- fedlist$value$DataSource[1]
  
  #rename columns
  colnames(fedspp)<-c("species_col", "status_code")
  
  #get just species from user data frame:
  Species <- tibble::tibble(df) %>%
    dplyr::select(all_of(species_col))
  colnames(Species)<-"species_col"
  
  #find all T&E species
  TorE <- dplyr::inner_join(Species, fedspp, by = "species_col")
  
  #keep just those with status code T, E, or C
  TorE <- TorE[which(TorE$status_code == "Fed-E" |
                       TorE$status_code == "Fed-C" |
                       TorE$status_code == "Fed-T"),]
  
  #If no species of concern, state that an exit function.
  if(nrow(TorE)*ncol(TorE)==0){
    cat("No T&E species found in your dataset.\n")
    #print date and source of data:
    cat("Your T&E check used data pulled from: ",
        crayon::bold$red(url), " on ", crayon::bold$red(fed_date), ".", sep="")
    
    return()
  }
  
  #if species of concern, define the status codes
  else{
    TorE<-TorE %>% mutate(status = case_when(
      status_code %in% "Fed-E" ~ "Endangered",
      status_code %in% "Fed-T" ~ "Threatened",
      status_code %in% "Fec-D" ~ "Considered for Listing"
    ))
  }
  #rename columns
  colnames(TorE)<-c("Consider removing", "status code", "status explanation")
  
  #print date and source of data:
  cat("Your T&E check used data pulled from: ",
      crayon::bold$red(url), " on ", crayon::bold$red(fed_date), ".", sep="")
  
  return(TorE)
}
