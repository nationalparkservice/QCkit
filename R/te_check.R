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
#' @param x - The name of your dataframe containing species observations
#' @param species_col - The name of the column within your dataframe containing the scientific names of the species
#' @param park_code -  The four letter park code
#' @param expansion - Logical. Defaults to FALSE. The default setting will return only exact matches between your the scientific binomial (genera and specific epithet) in your dataset and the federal matchlist. Setting expansion = TRUE will expand the list of matches to return all species (and subspecies) that from the matchlist that match any genera listed in your dataset, regardless of whether a given species is actually in your dataset. An addtional column indicating whether the species returned is in your dataset ("In Data") or has been exapnded to ("Expansion") is generated.
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
te_check <- function(x, species_col, park_code, expansion=FALSE) {
  #generate URL for odata services:
  odata_url <- paste0("https://irmadev.nps.gov/PrototypeCSVtoAPI/odata/FederalConservationListTaxaforDataProtection2272462?$filter=ParkCode%20eq%20%27", park_code, "%27%20or%20ParkCode%20eq%20%27All%27")
 #trycatch for VPN connections: 
  tryCatch(
    {
      fedlist <- ODataQuery::retrieve_data(odata_url)},
    error = function(e){
      cat(crayon::red$bold("ERROR: "),
          "Your connection timed out.\n",
          "Make sure you are logged on to the VPN before running ",
          crayon::green$bold("te_check()"),
          ".", sep="")
          stop()
    })
  #subset incoming data:
  fedspp <- as.data.frame(fedlist$value$ProtectedSci)
  fedspp <- cbind(fedspp, fedlist$value$MatchListStatus)
  #rename columns
  colnames(fedspp)<-c("species_col", "status_code")
  # add column explaining Fed T and E codes. From:
  # https://ecos.fws.gov/ecp0/html/db-status.html
  #---- code folding ----
  fedspp<-fedspp %>% mutate(status = case_when(
    status_code == "Fed-E" ~ "Endangered",
    status_code == "Fed-T" ~ "Threatened",
    status_code == "Fed-EmE" ~ "Emergency Listing, Endangered",
    status_code == "Fed-EmT" ~ "Emergency Listing Threatened",
    status_code == "Fed-EXPE" ~ "Experimental Population, Essential",
    status_code == "Fed-XE" ~ "Experimental Population, Essential",
    status_code == "Fed-EXPN" ~ "Experimental Population, Non-Essential",
    status_code == "XN" ~ "Experimental Population, Non-Essential",
    status_code == "Fed-SAE" ~ "Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-E(S/A)" ~ "Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-SAT" ~ "Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-T(S/A)" ~ "Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-SC" ~ "Species of Concern",
    status_code == "Fed-SU" ~ "Status Undefined",
    status_code == "Fed-UR" ~ "Under Review in the Candidate or Petition Process",
    status_code == "Fed-PE" ~ "Proposed Endangered",
    status_code == "Fed-PT" ~ "Proposed Threatened",
    status_code == "Fed-PEXPE" ~ "Proposed Experimental Population, Essential",
    status_code == "Fed-PXE" ~ "Proposed Experimental Population, Essential",
    status_code == "Fed-PEXPN" ~ "Proposed Experimental Population, Non-Essential",
    status_code == "Fed-PXN" ~ "Proposed Experimental Population, Non-Essential",
    status_code == "Fed-PSAE" ~"Proposed Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-PE(S/A)" ~"Proposed Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-PSAT" ~ "Proposed Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-PT(S/A)" ~ "Proposed Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-RT" ~ "Resolved Taxon",
    status_code == "Fed-C" ~ "Candidate Taxon, Ready for Proposal",
    status_code == "Fed-C2" ~ "Heritage Program Taxon of Concern, FWS NOR Taxon before 1996",
    status_code == "Fed-D3A" ~ "Delisted Taxon, Evidently Extinct",
    status_code == "Fed-D3B" ~ "Delisted Taxon, Invalid Name in Current Scientific Opinion",
    status_code == "Fed-D3C" ~ "Delisted Taxon, Recovered",
    status_code == "Fed-DA" ~ "Delisted Taxon, Amendment of the Act",
    status_code == "Fed-DM" ~ "Delisted Taxon, Recovered, Being Monitored First Five Years",
    status_code == "Fed-DO" ~ "Delisted Taxon, Original Commercial Data Erroneous",
    status_code == "Fed-DP" ~ "Delisted Taxon, Discovered Previously Unknown Additional Populations and/or Habitat",
    status_code == "Fed-DR" ~ "Delisted Taxon, Taxonomic Revision (Improved Understanding)",
    status_code == "Fed-AD" ~ "Proposed Delisting",
    status_code == "Fed-AE" ~ "Proposed Reclassification to Endangered",
    status_code == "Fed-AT" ~ "Proposed Reclassification to Threatened",
    status_code == "Fed-DNS" ~ "Original Data in Error - Not a listable entity",
    status_code == "Fed-NL" ~ "Not Listed",
    status_code == "Fed-Unlist" ~ "Pre-Act Delisting (or clearance--removal from the Lists) (code no longer in use)",
    status_code == "Fed-E*" ~ "Endangered Genus (code no longer in use)"
  ))
  #---- end code folding ----
  #get date data were pulled from FWS:
  fed_date <- fedlist$value$DateListImported[1]
  fed_date <- substr(fed_date, 1, 10)
  #get URL data were accessed from:
  url <- fedlist$value$DataSource[1]
  #get just species from user data frame:
  Species<-x[grepl(species_col, colnames(x))]
  colnames(Species)<-"species_col"
  # if any member of a genera in the dataset is protected, return all members
  # of that genera, even if they aren't in the observational data:
  if(expansion == TRUE){
    #get genus name in input dataframe:
    Species$genus_col <- gsub(" .*$", "", Species$species_col)
    #get genus name in fedspp:
    fedspp$genus_col <- gsub(" .*$", "", fedspp$species_col)
    #inner join based on genera:
    TorE <- dplyr::inner_join(Species, fedspp, by="genus_col")
      
    #if no species in the list:
    if(nrow(TorE)*ncol(TorE)==0){
      cat("No T&E species found in your dataset.\n")
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ", crayon::bold$red(fed_date), ".", sep="")
      return()
    }
    #if there are species returned:
    if(nrow(TorE)*ncol(TorE) > 0){
      #add a column indicating if entry was in the original dataset or just shares genus name)
      TorE <- TorE %>%
        dplyr::mutate(InData = ifelse(species_col.x == species_col.y,
            "In your Data", "Expansion"))
      #clean up dataframe:    
      TorE<-TorE[, c(3,6,4,5)]    
      colnames(TorE) <- c("Species",
                          "InData",
                          "status_code",
                          "status_explanation")
      #format output for easy digestion:
      TorE<-huxtable::as_hux(TorE)
      TorE<-huxtable::map_text_color(TorE, by_values("In your Data" = "red"))
      TorE<-huxtable::theme_basic(TorE)
      #print data source and date:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ", crayon::bold$red(fed_date), ".", sep="")
      return(TorE)
    }
  }
  #if expansion = FALSE:
  if(expansion == FALSE){
    #find all T&E species
    TorE <- dplyr::inner_join(Species, fedspp, by = "species_col")
    #If no species of concern, state that and exit function.
    if(nrow(TorE)*ncol(TorE)==0){
      cat("No T&E species found in your dataset.\n")
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
        crayon::bold$red(url), " on ", crayon::bold$red(fed_date), ".\n", sep="")
      return(TorE)
    }
    #if there are species in the list, return list (and data source/date):
    if(nrow(TorE)*ncol(TorE) > 0){
      colnames(TorE)<-c("Species", "status_code", "status_explanation")
      TorE<-huxtable::as_hux(TorE)
      TorE<-huxtable::theme_basic(TorE)
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
        crayon::bold$red(url), " on ", crayon::bold$red(fed_date), ".", sep="")
      return(TorE)
    }
  }
}
