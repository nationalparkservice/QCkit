#' Taxonomic Rank Determination Function
#'
#' @description `get_taxon_rank()` generates a new column in your selected data set called taxonRank that will show the taxonomic rank of the most specific name in the given scientific name column. This is a required column in the Simple Darwin Core rule set and guidelines. This function will be useful in creating and auto populating a required Simple Darwin Core field.
#'
#' @details Define your species data set name and the column name with the scientific names of your species (if you are following a Simple Darwin Core naming format, this column should be scientificName, but any column name is fine).
#'
#' The function will read the various strings in your species name column and identify them as either a family, genus, species, or subspecies. This function only works with cleaned and parsed scientific names. If the scientific name is higher than family, the function will not work correctly. Subfamily and Tribe names (which, similar to family names end in "ae*") will be designated Family.
#'
#' @param df - The name of your data frame containing species observations
#' @param sciName_col - The name of the column within your data frame containing the scientific names of the species.
#'
#' @return The function returns a new column in the given data frame named taxonRank with the taxonomic rank of the corresponding scientific name in each column. If there is no name in a row, then it returns as NA for that row.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mydf <- get_taxon_rank(df = mydf, sciName_col = "scientificName")
#' }
#'
get_taxon_rank <- function(df, sciName_col) {
  sciName_col <- df[[sciName_col]]
  dplyr::mutate(df, taxonRank = dplyr::case_when(
    stringr::str_detect(sciName_col, "\\s[^\\s]*\\s") ~ "subspecies", #regex says match a space, followed by any number of characters, followed by another space
    stringr::str_detect(sciName_col, "\\s.*") ~ "species", #regex says match a space followed by any number of character
    stringr::str_detect(sciName_col, "ae$") ~ "family", #regex says match to any word that has ae at the end of it
    stringr::str_detect(sciName_col, "^\\S*$") ~ "genus")) #regex says match to any number of characters that DO NOT have a space in front and then ends
}

#' Threatened Or Endangered Species Checker Function
#'
#' @description `check_te()` generates a list of species you should consider removing from your dataset before making it public by matching the scientific names within your data set to the Federal Conservation List. `check_te()` should be considered a helpful tool for identifying federally listed endangered and threatened species in your data. Each National Park has a park-specific Protected Data Memo that outlines which data should be restricted. Threatened and endangered species are often - although not always - listed on these Memos. Additional species (from state conservation lists) or non-threatened and non-endangered species of concern or other biological or non-biological resources may be listed on Memos. Consult the relevant park-specific Protected Data Memo prior to making decisions on restricting or releasing data.
#'
#' @details Define your species data set name, column name with the scientific names of your species, and your four letter park code.
#'
#' The `check_te()` function downloads the Federal Conservation list using the IRMA odata API service and matches this species list to the list of scientific names in your data frame. Keep in mind that this is a Federal list, not a state list. Changes in taxa names may also cause some species to be missed. Because the odata API service is not publicly available, you must be logged in to the NPS VPN or in the office to use this function.
#'
#'  For the default, expansion = FALSE, the function will perform an exact match between the taxa in your scientificName column and the federal Conservation List and then filter the results to keep only species that are listed as endangered, threatened, or considered for listing. If your scientificName column contains information other than the binomial (genus and species), no matches will be returned. For instance, if you have an Order or just a genus listed, these will not be matched to the Federal Conservation List.
#'
#'  If you set expansion = TRUE, the function will truncate each item in your scientificName column to the first word in an attempt to extract a genus name. If you only have genera listed, these will be retained. If you have have higher-order taxa listed such as Family, Order, or Phyla again the first word will be retained. This first word (typically a genus) will be matched to just the generic name of species from the Federal Conservation List. All matches, regardless of listing status, are retained. The result is that for a given species in your scientificName column, all species within that genus that are on the Federal Conservation List will be returned (along with their federal conservation listing codes and a column indicating whether the species is actually in your data or is part of the expanded search).
#'
#' @param x - The name of your data frame containing species observations
#' @param species_col - The name of the column within your data frame containing the scientific names of the species (genus and specific epithet).
#' @param park_code -  A four letter park code. Or a list of park codes.
#' @param expansion - Logical. Defaults to FALSE. The default setting will return only exact matches between your the scientific binomial (genera and specific epithet) in your data set and the federal match list. Setting expansion = TRUE will expand the list of matches to return all species (and subspecies) that from the match list that match any genera listed in your data set, regardless of whether a given species is actually in your data set. An additional column indicating whether the species returned is in your data set ("In your Data") or has been expanded to ("Expansion") is generated.
#'
#' @return The function returns a (modified) data frame with the names of all the species that fall under the federal conservation list. The resulting data frame may have multiple instances of a given species if it is listed in multiple parks (park codes for each listing are supplied). Technically it is a huxtable, but it should function identically to a data frame for downstream purposes.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' #for individual parks:
#' check_te(x = my_species_dataframe, species_col = "scientificName", park_code = "BICY")
#' list<-check_te(data, "scientificName", "ROMO", expansion=TRUE)
#' # for a list of parks:
#' park_code<-c("ROMO", "YELL", "SAGU")
#' list<-check_te(data, "scientificName", park_code, expansion=TRUE)
#' }
#'
check_te <- function(x, species_col, park_code, expansion=FALSE) {
  #generate URL for odata services:
  url<-"https://irmadev.nps.gov/PrototypeCSVtoAPI/odata/FederalConservationListTaxaforDataProtection2272462?$filter=ParkCode%20eq%20%27"
  for(i in seq_along(park_code)){
    url <- paste0(url, park_code[i], "%27%20or%20ParkCode%20eq%20%27")
  }
  odata_url <- paste0(url, "All%27")
  #trycatch for VPN connections:
  tryCatch(
    {
      fedlist <- ODataQuery::retrieve_data(odata_url)},
    error = function(e) {
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
  fedspp <- cbind(fedspp, fedlist$value$ParkCode)
  #rename columns
  colnames(fedspp)<-c("species_col", "status_code", "park_code")
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
    status_code == "Fed-PSAE" ~ "Proposed Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-PE(S/A)" ~ "Proposed Similarity of Appearance to an Endangered Taxon",
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
  species_col_grepl <- paste0('\\b', species_col, '\\b')
  Species <- x[grepl(species_col_grepl, colnames(x))]
  colnames(Species) <- "species_col"
  # if any member of a genera in the dataset is protected, return all members
  # of that genera, even if they aren't in the observational data:
  if (expansion == TRUE) {
    #get genus name in input dataframe:
    Species$genus_col <- gsub(" .*$", "", Species$species_col)
    #get genus name in fedspp:
    fedspp$genus_col <- gsub(" .*$", "", fedspp$species_col)
    #inner join based on genera:
    TorE <- dplyr::inner_join(Species, fedspp, by = "genus_col")

    #if no species in the list:
    if (nrow(TorE) * ncol(TorE) == 0) {
      cat("No T&E species found in your dataset.\n")
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".", sep = "")
      return()
    }
    #if there are species returned:
    if (nrow(TorE) * ncol(TorE) > 0) {
      #add a column indicating if entry was in the original dataset or just shares genus name)
      TorE <- TorE %>%
        dplyr::mutate(InData = ifelse(species_col.x == species_col.y,
                                      "In your Data", "Expansion"))
      #clean up dataframe:
      TorE <- TorE[, c(5, 3, 7, 4, 6)]
      colnames(TorE) <- c("Park_code",
                          "Species",
                          "In_data",
                          "status_code",
                          "status_explanation")
      #format output for easy digestion:
      TorE <- huxtable::as_hux(TorE)
      TorE <- huxtable::map_text_color(TorE,
                                     huxtable::by_values("In your Data" = "green",
                                                         "Threatened" = "darkorange2",
                                                         "Endangered" = "red",
                                                         "Concern" = "yellow3",
                                                         "Candidate" = "yellow3"))
      TorE <- huxtable::theme_basic(TorE)
      #print data source and date:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep = "")
      return(TorE)
    }
  }
  #if expansion = FALSE:
  if (expansion == FALSE) {
    #find all T&E species
    TorE <- dplyr::inner_join(Species, fedspp, by = "species_col")
    #keep only rows with Fed-E, Fed-T, Fed-C and Fed-C2 status codes
    TorE <- TorE[which(TorE$status_code == "Fed-C" |
                         TorE$status_code == "Fed-T" |
                         TorE$status_code == "Fed-E" |
                         TorE$status_code == "Fed-C2"),]
    #If no species of concern, state that and exit function.
    if (nrow(TorE) * ncol(TorE) == 0) {
      cat("No T&E species found in your dataset.\n")
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep = "")
      return(TorE)
    }
    #if there are species in the list, return list (and data source/date):
    if (nrow(TorE) * ncol(TorE) > 0) {
      TorE <- TorE[, c(3,1,2,4)]
      colnames(TorE) <- c("Park_code", "Species", "status_code",
                          "status_explanation")
      TorE <- huxtable::as_hux(TorE)
      TorE <- huxtable::map_text_color(TorE,
                                     huxtable::by_values("Threatened" = "darkorange2",
                                                         "Endangered" = "red",
                                                         "Concern" = "yellow3",
                                                         "Candidate" = "yellow3"))
      TorE <- huxtable::theme_basic(TorE)
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep = "")
      return(TorE)
    }
  }
}

#' Threatened Or Endangered Species Checker Function
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated in favor of `check_te()`. The function name was changed to promote constancy in function naming across the package and to conform with tidyverse style guides. `te_check()` is no longer updated and may not reference the latest version of the federal endangered and threatened species listings.
#'
#' `te_check()` generates a list of species you should consider removing from your dataset before making it public by matching the scientific names within your data set to the Federal Conservation List. `te_check()` should be considered a helpful tool for identifying federally listed endangered and threatened species in your data. Each National Park has a park-specific Protected Data Memo that outlines which data should be restricted. Threatened and endangered species are often - although not always - listed on these Memos. Additional species (from state conservation lists) or non-threatened and non-endangered species of concern or other biological or non-biological resources may be listed on Memos. Consult the relevant park-specific Protected Data Memo prior to making decisions on restricting or releasing data.
#'
#' @details Define your species data set name, column name with the scientific names of your species, and your four letter park code.
#'
#' The `te_check()` function downloads the Federal Conservation list using the IRMA odata API service and matches this species list to the list of scientific names in your data frame. Keep in mind that this is a Federal list, not a state list. Changes in taxa names may also cause some species to be missed. Because the odata API service is not publicly available, you must be logged in to the NPS VPN or in the office to use this function.
#'
#'  For the default, expansion = FALSE, the function will perform an exact match between the taxa in your scientificName column and the federal Conservation List and then filter the results to keep only species that are listed as endangered, threatened, or considered for listing. If your scientificName column contains information other than the binomial (genus and species), no matches will be returned. For instance, if you have an Order or just a genus listed, these will not be matched to the Federal Conservation List.
#'
#'  If you set expansion = TRUE, the function will truncate each item in your scientificName column to the first word in an attempt to extract a genus name. If you only have genera listed, these will be retained. If you have have higher-order taxa listed such as Family, Order, or Phyla again the first word will be retained. This first word (typically a genus) will be matched to just the generic name of species from the Federal Conservation List. All matches, regardless of listing status, are retained. The result is that for a given species in your scientificName column, all species within that genus that are on the Federal Conservation List will be returned (along with their federal conservation listing codes and a column indicating whether the species is actually in your data or is part of the expanded search).
#'
#' @param x - The name of your data frame containing species observations
#' @param species_col - The name of the column within your data frame containing the scientific names of the species (genus and specific epithet).
#' @param park_code -  A four letter park code. Or a list of park codes.
#' @param expansion - Logical. Defaults to FALSE. The default setting will return only exact matches between your the scientific binomial (genera and specific epithet) in your data set and the federal match list. Setting expansion = TRUE will expand the list of matches to return all species (and subspecies) that from the match list that match any genera listed in your data set, regardless of whether a given species is actually in your data set. An additional column indicating whether the species returned is in your data set ("In your Data") or has been expanded to ("Expansion") is generated.
#'
#' @return The function returns a (modified) data frame with the names of all the species that fall under the federal conservation list. The resulting data frame may have multiple instances of a given species if it is listed in multiple parks (park codes for each listing are supplied). Technically it is a huxtable, but it should function identically to a data frame for downstream purposes.
#' @importFrom magrittr %>%
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #for individual parks:
#' te_check(x = my_species_dataframe, species_col = "scientificName", park_code = "BICY")
#' list<-te_check(data, "scientificName", "ROMO", expansion=TRUE)
#' # for a list of parks:
#' park_code<-c("ROMO", "YELL", "SAGU")
#' list<-te_check(data, "scientificName", park_code, expansion=TRUE)
#' }
#'
te_check <- function(x, species_col, park_code, expansion = FALSE) {
  lifecycle::deprecate_warn("0.1.0.3", "te_check", "check_te()")
  #generate URL for odata services:
  url<-"https://irmadev.nps.gov/PrototypeCSVtoAPI/odata/FederalConservationListTaxaforDataProtection2272462?$filter=ParkCode%20eq%20%27"
  for(i in seq_along(park_code)){
    url <- paste0(url, park_code[i], "%27%20or%20ParkCode%20eq%20%27")
  }
  odata_url <- paste0(url, "All%27")
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
  fedspp <- cbind(fedspp, fedlist$value$ParkCode)
  #rename columns
  colnames(fedspp)<-c("species_col", "status_code", "park_code")
  # add column explaining Fed T and E codes. From:
  # https://ecos.fws.gov/ecp0/html/db-status.html
  #---- code folding ----
  fedspp < -fedspp %>% mutate(status = case_when(
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
    status_code == "Fed-PSAE" ~ "Proposed Similarity of Appearance to an Endangered Taxon",
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
  species_col_grepl<-paste0('\\b', species_col, '\\b')
  Species<-x[grepl(species_col_grepl, colnames(x))]
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
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".", sep="")
      return()
    }
    #if there are species returned:
    if(nrow(TorE)*ncol(TorE) > 0){
      #add a column indicating if entry was in the original dataset or just shares genus name)
      TorE <- TorE %>%
        dplyr::mutate(InData = ifelse(species_col.x == species_col.y,
                                      "In your Data", "Expansion"))
      #clean up dataframe:
      TorE<-TorE[, c(5,3,7,4,6)]
      colnames(TorE) <- c("Park_code",
                          "Species",
                          "In_data",
                          "status_code",
                          "status_explanation")
      #format output for easy digestion:
      TorE<-huxtable::as_hux(TorE)
      TorE<-huxtable::map_text_color(TorE,
                                     huxtable::by_values("In your Data" = "green",
                                                         "Threatened" = "darkorange2",
                                                         "Endangered" = "red",
                                                         "Concern" = "yellow3",
                                                         "Candidate" = "yellow3"))
      TorE<-huxtable::theme_basic(TorE)
      #print data source and date:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep="")
      return(TorE)
    }
  }
  #if expansion = FALSE:
  if(expansion == FALSE){
    #find all T&E species
    TorE <- dplyr::inner_join(Species, fedspp, by = "species_col")
    #keep only rows with Fed-E, Fed-T, Fed-C and Fed-C2 status codes
    TorE <- TorE[which(TorE$status_code == "Fed-C" |
                         TorE$status_code == "Fed-T" |
                         TorE$status_code == "Fed-E" |
                         TorE$status_code == "Fed-C2"),]
    #If no species of concern, state that and exit function.
    if(nrow(TorE)*ncol(TorE)==0){
      cat("No T&E species found in your dataset.\n")
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep="")
      return(TorE)
    }
    #if there are species in the list, return list (and data source/date):
    if(nrow(TorE)*ncol(TorE) > 0){
      TorE<-TorE[, c(3,1,2,4)]
      colnames(TorE)<-c("Park_code", "Species", "status_code", "status_explanation")
      TorE<-huxtable::as_hux(TorE)
      TorE<-huxtable::map_text_color(TorE,
                                     huxtable::by_values("Threatened" = "darkorange2",
                                                         "Endangered" = "red",
                                                         "Concern" = "yellow3",
                                                         "Candidate" = "yellow3"))
      TorE<-huxtable::theme_basic(TorE)
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep="")
      return(TorE)
    }
  }
}
