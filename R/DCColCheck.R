
#' TDWG Darwin Core Column Name Check 08-23-2022
#'
#' @description DCColCheck checks to see if the column names in your dataframe match the standardized simple Darwin Core names established by the Taxonomic Databases Working Group
#'
#' @details A dataframe is created with all the simple DarwinCore terms, drawn from Darwin Core reference guide: https://dwc.tdwg.org/terms/ last updated 07-15-2021. We have chosen to align ourselves mostly with the simple Darwin Core rules: https://dwc.tdwg.org/simple/. The function runs through each of the column names in your working dataframe to see if they match 1. A standard simple DarwinCore name 2. A name with a pattern of strings matching "custom_", indicating a custom made column  or 3. A name with a pattern of strings matching "_DQ", indicating a data quality flag. If the column name does not fit within any of the three categories, a "Fix me" statement is printed alongside the column name. The function then counts all of the names fitting within each category and prints a summary table.
#'
#' @param workingdf - This is the dataframe you want to run against the function. To call, simply write workingdf = "the name of your dataframe".
#'
#' @return - The function returns a list of the column names you should fix (not fitting with simple Darwin Core terms, custom name formatting, data quality flagging formatting). Additionally, a small summary table is printed with the counts of the columns falling under each category (DarwinCore, Custom, DQ, Fix Me).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' "DCColCheck(yourdataframe)"
#' }
DCColCheck <- function(workingdf) {
  dwcterms <- data.frame(DarwinCore = c("type", "modified", "language", "license", "rightsHolder", "accessRights", "bibliographicCitation", "references", "institutionID", "collectionID", "datasetID", "institutionCode", "collectionCode", "datasetName", "ownerInstitutionCode", "basisOfRecord", "informationWithheld", "dataGeneralizations", "dynamicProperties", "occurrenceID", "catalogNumber", "recordNumber", "recordedBy", "recordedByID", "individualCount", "organismQuantity", "organismQuantityType", "sex", "lifeStage", "reproductiveCondition", "behavior", "establishmentMeans", "degreeOfEstablishment", "pathway", "georeferenceVerificationStatus", "occurrenceStatus", "preparations", "disposition", "associatedMedia", "associatedReferences", "associatedSequences", "associatedTaxa", "otherCatalogNumbers", "occurrenceRemarks", "organismID", "organismName", "organismScope", "associatedOrganisms", "previousIdentifications", "organismRemarks", "eventID", "parentEventID", "fieldNumber", "eventDate", "eventTime", "startDayOfYear", "endDayOfYear", "year", "month", "day", "verbatimEventDate", "habitat", "samplingProtocol", "sampleSizeValue", "sampleSizeUnit", "samplingEffort", "fieldNotes", "eventRemarks", "locationID", "higherGeographyID", "higherGeography", "continent", "waterBody", "islandGroup", "island", "country", "countryCode", "stateProvince", "county", "municipality", "locality", "verbatimLocality", "minimumElevationInMeters", "maximumElevationInMeters", "verbatimElevation", "verticalDatum", "minimumDepthInMeters", "maximumDepthInMeters", "verbatimDepth", "minimumDistanceAboveSurfaceInMeters", "maximumDistanceAboveSurfaceInMeters", "locationAccordingTo", "locationRemarks", "decimalLatitude", "decimalLongitude", "geodeticDatum", "coordinateUncertaintyInMeters", "coordinatePrecision", "pointRadiusSpatialFit", "verbatimCoordinates", "verbatimLatitude", "verbatimLongitude", "verbatimCoordinateSystem", "verbatimSRS", "footprintWKT", "footprintSRS", "footprintSpatialFit", "georeferencedBy", "georeferencedDate", "georeferenceProtocol", "georeferenceSources", "georeferenceRemarks", "geologicalContextID", "earliestEonOrLowestEonothem", "latestEonOrHighestEonothem", "earliestEraOrLowestErathem", "latestEraOrHighestErathem", "earliestPeriodOrLowestSystem", "latestPeriodOrHighestSystem", "earliestEpochOrLowestSeries", "latestEpochOrHighestSeries", "earliestAgeOrLowestStage", "latestAgeOrHighestStage", "lowestBiostratigraphicZone", "highestBiostratigraphicZone", "lithostratigraphicTerms", "group", "formation", "member", "bed", "identificationID", "verbatimIdentification", "identificationQualifier", "typeStatus", "identifiedBy", "identifiedByID", "dateIdentified", "ientificationReferences", "identificationVerificationStatus", "identificationRemarks", "taxonID", "scientificNameID", "acceptedNameUsageID", "parentNameUsageID", "originalNameUsageID", "naemAccordingToID", "naemPublishedInID", "taxonConceptID", "scientificName", "acceptedNameUsage", "parentNameUsage", "originalNameUsage", "nameAccordingTo", "namePublishedIn", "namePublishedInYear", "higherClassification", "kingdom", "phylum", "class", "order", "family", "subfamily", "genus", "genericName", "subgenus", "infragenericEpithet", "specificEpithet", "infraspecificEpithet", "cultivarEpithet", "taxonRank", "verbatimTaxonRank", "scientificNameAuthorship", "vernacularName", "nomenclaturalCode", "taxonomicStatus", "nomenclaturalStatus", "taxonRemarks", "measurementID", "measurementType", "measurementValue", "measurementAccuracy", "measurementUnit", "measurementDeterminedBy", "measurementDeterminedDate", "measurementMethod", "measurementRemarks", "resourceRelationshipID", "resourceID", "reationshipOfResourceID", "relatedResourceID", "relationshipOfResource", "relationshipAccordingTo", "relationshipEstablishedDate", "relationshipRemarks"))


  for (i in all_of(colnames(workingdf))) {
    ifelse(test = i %in% dwcterms$DarwinCore,
      yes = next,
      no = ifelse(test = grepl("custom_", i, ignore.case = TRUE) == TRUE,
        yes = next,
        no = ifelse(test = grepl("_dq", i, ignore.case = TRUE) == TRUE,
          yes = next,
          no = print(paste0(i, " is not a valid Darwin Core name! Fix me!"))
        )
      )
    )
  }

  DCproper <- 0
  CustomNames <- 0
  Flags <- 0
  FixMe <- 0

  for (i in all_of(colnames(workingdf))) {
    ifelse(test = i %in% dwcterms$DarwinCore,
      yes = DCproper <- (DCproper + 1),
      no = ifelse(test = grepl("custom_", i, ignore.case = TRUE) == TRUE,
        yes = CustomNames <- (CustomNames + 1),
        no = ifelse(test = grepl("_dq", i, ignore.case = TRUE) == TRUE,
          yes = Flags <- (Flags + 1),
          no = FixMe <- (FixMe + 1)
        )
      )
    )
  }

  DCtable <- data.frame(DCproper = DCproper, CustomNames = CustomNames, Flags = Flags, FixMe = FixMe)
  return(DCtable)
}
