% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxonomy.R
\name{get_taxon_rank}
\alias{get_taxon_rank}
\title{Taxonomic Rank Determination Function}
\usage{
get_taxon_rank(df, sciName_col)
}
\arguments{
\item{df}{\itemize{
\item The name of your data frame containing species observations
}}

\item{sciName_col}{\itemize{
\item The name of the column within your data frame containing the scientific names of the species.
}}
}
\value{
The function returns a new column in the given data frame named taxonRank with the taxonomic rank of the corresponding scientific name in each column. If there is no name in a row, then it returns as NA for that row.
}
\description{
\code{get_taxon_rank()} generates a new column in your selected data set called taxonRank that will show the taxonomic rank of the most specific name in the given scientific name column. This is a required column in the Simple Darwin Core rule set and guidelines. This function will be useful in creating and auto populating a required Simple Darwin Core field.
}
\details{
Define your species data set name and the column name with the scientific names of your species (if you are following a Simple Darwin Core naming format, this column should be scientificName, but any column name is fine).

The function will read the various strings in your species name column and identify them as either a family, genus, species, or subspecies. This function only works with cleaned and parsed scientific names. If the scientific name is higher than family, the function will not work correctly. Subfamily and Tribe names (which, similar to family names end in "ae*") will be designated Family.
}
\examples{
\dontrun{
mydf <- get_taxon_rank(df = mydf, sciName_col = "scientificName")
}

}
