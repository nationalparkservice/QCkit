% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_cols.R
\name{order_cols}
\alias{order_cols}
\title{Ordering Columns Function 03-21-2023}
\usage{
order_cols(df)
}
\arguments{
\item{df}{\itemize{
\item This is the dataframe you want to run against the function. To call, simply type df = "the name of your dataframe".
}}
}
\value{
\itemize{
\item The function returns a list of required and suggested columns to include in your dataset. When assigning to an object, the object contains your new dataset with all columns ordered properly.
}
}
\description{
\code{order_cols()} Checks and orders columns with TDWG Darwin Core naming standards and custom names in a dataset
}
\details{
Check to see if you have three (highly) recommended columns (locality, type, basisOfRecord) and various suggested columns present in your dataset. Print a list of which columns are present and which are not. Then, order all the columns in your dataset in the following order: (highly) recommended columns, suggested columns, the rest of the Darwin Core columns, "custom_" (non-Darwin Core) columns, and finally sensitive species data columns.

Any columns that are not darwinCore term names, do not start with "custom_" or are not "scientificName_flag" will be placed after the darwinCore columns and before the "custom_" columns.

One exception is if your dataset includes the column custom_TaxonomicNotes, it will be placed directly after namePublishedIn, if that column exists.

Suggested darwinCore column names (plus scientificName_flag) include (in the order they will be placed): eventDate, eventDate_flag, scientificName, scientificName_flag, taxonRank, verbatimIdentification, vernacularName, namePublishedIn, recordedBy, individualCount, decimalLongitude, decimalLatitude, coordinate_flag, geodeticDatum", verbatimCoordinates, verbatimCoordinateSystem, verbatimSRS,coordinateUncertaintyInMeters. Note that suggested names include some custom, non-Darwin Core names such as "scientificName_flag".

sensitive species data columns are defined as: informationWithheld, dataGeneralizations, and footprintWKT.
}
\examples{
\dontrun{
order_cols(df)
}
}
