#' Threatened Or Endangered Species Checker Function
#'
#' @description TECheck creates a subset of the species list you provide, all of which fall under the Federal Conservation List Taxa for Data Protection. These species should be removed from your dataset if your data is to be made public.
#'
#' @details Define your species dataset name, column name with the scientific names of your species, and your four letter park code.
#'
#' The function downloads the Federal Conservation list using the IRMA odata API service and matches this species list to the list of scientific names in your dataframe.
#' 
#' Because the Federal Conservation list is not publicaly available (???), you must be logged in to the NPS VPN or in the office to use this function. 
#'
#'
#' @param SpeciesData - The name of your dataframe containing species observations
#' @param SciNameCol - The name of the column within your dataframe containing the scientific names of the species
#' @param ParkID -  The four letter park code
#'
#' @return The function returns a list in your global environment called "TorE" with the names of all the species thnat fall under the federal conservation list.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' TECheck(SpeciesData = my_species_dataframe, SciNameCol = "scientific_names", ParkID = "BICY")
#' }
#'
TECheck <- function(SpeciesData, SciNameCol, ParkID) { # Specify DF and Scientific Name column name


  fedlist <- ODataQuery::retrieve_data(paste0("https://irmadev.nps.gov/PrototypeCSVtoAPI/odata/FederalConservationListTaxaforDataProtection2272462?$filter=ParkCode%20eq%20%27", ParkID, "%27%20or%20ParkCode%20eq%20%27All%27"))

  fedlist <- as.data.frame(fedlist$value$ProtectedSci)

  fedlist <- plyr::rename(fedlist, c("fedlist$value$ProtectedSci" = SciNameCol))

  Species <- tibble::tibble(SpeciesData) %>%
    dplyr::select(SciNameCol)

  TorE <- dplyr::semi_join(Species, fedlist, by = SciNameCol) %>%
    dplyr::select("Remove Me!" = SciNameCol)

  return(TorE)
}
