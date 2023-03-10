#' Taxonomic Rank Determination Function
#'
#' @description `taxonRank()` Generates a new column in your selected data set called taxonRank that will show the taxonomic rank of the most specific name in the given scientific name column. This is a required column in the Simple Darwin Core rule set and guidelines. This function will be useful in creating and auto populating a required Simple Darwin Core field.
#'
#' @details Define your species data set name and the column name with the scientific names of your species (if you are following a Simple Darwin Core naming format, this column should be scientificName, but any column name is fine).
#'
#' The function will read the various strings in your species name column and identify them as either a family, genus, species, or subspecies. This function only works with cleaned and parsed scientific names. If the scientific name is higher than family, the function will not work correctly.
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
#' mydf <- taxonRank(df = mydf, sciName_col = "scientificName")
#' }
#'
taxonRank <- function(df, sciName_col) 
{
  sciName_col <- df[[sciName_col]]
  dplyr::mutate(df, taxonRank = dplyr::case_when(
    stringr::str_detect(sciName_col, "\\s[^\\s]*\\s") ~ "subspecies", #regex says match a space, followed by any number of characters, followed by another space    
    stringr::str_detect(sciName_col, "\\s.*") ~ "species", #regex says match a space followed by any number of character     
    stringr::str_detect(sciName_col, "ae$") ~ "family", #regex says match to any word that has ae at the end of it    
    stringr::str_detect(sciName_col, "^\\S*$") ~ "genus")) #regex says match to any number of characters that DO NOT have a space in front and then ends   
  }