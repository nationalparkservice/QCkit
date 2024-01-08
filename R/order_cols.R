


#' Ordering Columns Function 03-21-2023
#'
#' @description `order_cols()` Checks and orders columns with TDWG Darwin Core naming standards and custom names in a dataset
#'
#' @details Check to see if you have three (highly) recommended columns (locality, type, basisOfRecord) and various suggested columns present in your dataset. Print a list of which columns are present and which are not. Then, order all the columns in your dataset in the following order: (highly) recommended columns, suggested columns, the rest of the Darwin Core columns, "custom_" (non-Darwin Core) columns, and finally sensitive species data columns.
#'
#' Any columns that are not darwinCore term names, do not start with "custom_" or are not "scientificName_flag" will be placed after the darwinCore columns and before the "custom_" columns.
#'
#' One exception is if your dataset includes the column custom_TaxonomicNotes, it will be placed directly after namePublishedIn, if that column exists.
#'
#' Suggested darwinCore column names (plus scientificName_flag) include (in the order they will be placed): eventDate, eventDate_flag, scientificName, scientificName_flag, taxonRank, verbatimIdentification, vernacularName, namePublishedIn, recordedBy, individualCount, decimalLongitude, decimalLatitude, coordinate_flag, geodeticDatum", verbatimCoordinates, verbatimCoordinateSystem, verbatimSRS,coordinateUncertaintyInMeters. Note that suggested names include some custom, non-Darwin Core names such as "scientificName_flag".
#'
#' sensitive species data columns are defined as: informationWithheld, dataGeneralizations, and footprintWKT.
#'
#'
#'
#' @param df - This is the dataframe you want to run against the function. To call, simply type df = "the name of your dataframe".
#'
#' @return - The function returns a list of required and suggested columns to include in your dataset. When assigning to an object, the object contains your new dataset with all columns ordered properly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' order_cols(df)
#' }
order_cols <- function(df) {

  suggested <- c("eventDate",
                 "eventDate_flag",
                 "scientificName",
                 "scientificName_flag",
                 "taxonRank",
                 "verbatimIdentification",
                 "vernacularName",
                 "namePublishedIn",
                 "recordedBy",
                 "individualCount",
                 "decimalLongitude",
                 "decimalLatitude",
                 "coordinate_flag",
                 "geodeticDatum",
                 "verbatimCoordinates",
                 "verbatimCoordinateSystem",
                 "verbatimSRS",
                 "coordinateUncertaintyInMeters")

  recommended <- c("locality", "type", "basisOfRecord")

  allofem <- c("locality",
               "type",
               "basisOfRecord",
               "eventDate",
               "eventDate_flag",
               "scientificName",
               "scientificName_flag",
               "taxonRank",
               "verbatimIdentification",
               "vernacularName",
               "namePublishedIn",
               "custom_TaxonomicNotes",
               "recordedBy",
               "individualCount",
               "decimalLongitude",
               "decimalLatitude",
               "coordinate_flag",
               "geodeticDatum",
               "verbatimCoordinates",
               "verbatimCoordinateSystem",
               "verbatimSRS",
               "coordinateUncertaintyInMeters")

  print(lapply(recommended,
               function(x) ifelse(x %in% names(df),
                                  paste0("Looking great! The recommended field \'",
                                       x, "\' exists within your data"),
                                  paste0("Please include the recommended field \'",
                                       x, "\' in your dataset"))))

  print(lapply(suggested,
               function(x) ifelse(x %in% names(df),
                                  paste0("The field \'", x, "\' is present"),
                                paste0("The suggested field \'", x,
                                       "\' is NOT present. If data for this field exists, please include it in your dataset"))))

  df <- list(df)
  df <- lapply(df, function(x) data.table::setcolorder(x,
                                                       intersect(allofem,
                                                                 names(x))))
  df <- data.frame(df)
  customs <- df %>% dplyr::select(tidyselect::starts_with("custom_"))
  df <-      df %>% dplyr::select(-tidyselect::starts_with("custom_"))
  sensitives <- df %>% dplyr::select(any_of(c("informationWithheld",
                                              "dataGeneralizations",
                                              "footprintWKT")))
  df <- df %>% dplyr::select(-dplyr::any_of(c("informationWithheld",
                                              "dataGeneralizations",
                                              "footprintWKT")))
  df <- cbind(df, customs, sensitives)
  if (sum(grepl("namePublishedIn", names(df))) > 0){
    df <- df %>% dplyr::relocate(any_of("custom_TaxonomicNotes"),
                               .after = "namePublishedIn")
  }
  return(df)
}