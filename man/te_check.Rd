% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxonomy.R
\name{te_check}
\alias{te_check}
\title{Threatened Or Endangered Species Checker Function}
\usage{
te_check(x, species_col, park_code, expansion = FALSE)
}
\arguments{
\item{x}{\itemize{
\item The name of your data frame containing species observations
}}

\item{species_col}{\itemize{
\item The name of the column within your data frame containing the scientific names of the species (genus and specific epithet).
}}

\item{park_code}{\itemize{
\item A four letter park code. Or a list of park codes.
}}

\item{expansion}{\itemize{
\item Logical. Defaults to FALSE. The default setting will return only exact matches between your the scientific binomial (genera and specific epithet) in your data set and the federal match list. Setting expansion = TRUE will expand the list of matches to return all species (and subspecies) that from the match list that match any genera listed in your data set, regardless of whether a given species is actually in your data set. An additional column indicating whether the species returned is in your data set ("In your Data") or has been expanded to ("Expansion") is generated.
}}
}
\value{
The function returns a (modified) data frame with the names of all the species that fall under the federal conservation list. The resulting data frame may have multiple instances of a given species if it is listed in multiple parks (park codes for each listing are supplied). Technically it is a huxtable, but it should function identically to a data frame for downstream purposes.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

This function has been deprecated in favor of \code{check_te()}. The function name was changed to promote constancy in function naming across the package and to conform with tidyverse style guides. \code{te_check()} is no longer updated and may not reference the latest version of the federal endangered and threatened species listings.

\code{te_check()} generates a list of species you should consider removing from your dataset before making it public by matching the scientific names within your data set to the Federal Conservation List. \code{te_check()} should be considered a helpful tool for identifying federally listed endangered and threatened species in your data. Each National Park has a park-specific Protected Data Memo that outlines which data should be restricted. Threatened and endangered species are often - although not always - listed on these Memos. Additional species (from state conservation lists) or non-threatened and non-endangered species of concern or other biological or non-biological resources may be listed on Memos. Consult the relevant park-specific Protected Data Memo prior to making decisions on restricting or releasing data.
}
\details{
Define your species data set name, column name with the scientific names of your species, and your four letter park code.

The \code{te_check()} function downloads the Federal Conservation list using the IRMA odata API service and matches this species list to the list of scientific names in your data frame. Keep in mind that this is a Federal list, not a state list. Changes in taxa names may also cause some species to be missed. Because the odata API service is not publicly available, you must be logged in to the NPS VPN or in the office to use this function.

For the default, expansion = FALSE, the function will perform an exact match between the taxa in your scientificName column and the federal Conservation List and then filter the results to keep only species that are listed as endangered, threatened, or considered for listing. If your scientificName column contains information other than the binomial (genus and species), no matches will be returned. For instance, if you have an Order or just a genus listed, these will not be matched to the Federal Conservation List.

If you set expansion = TRUE, the function will truncate each item in your scientificName column to the first word in an attempt to extract a genus name. If you only have genera listed, these will be retained. If you have have higher-order taxa listed such as Family, Order, or Phyla again the first word will be retained. This first word (typically a genus) will be matched to just the generic name of species from the Federal Conservation List. All matches, regardless of listing status, are retained. The result is that for a given species in your scientificName column, all species within that genus that are on the Federal Conservation List will be returned (along with their federal conservation listing codes and a column indicating whether the species is actually in your data or is part of the expanded search).
}
\examples{
\dontrun{
#for individual parks:
te_check(x = my_species_dataframe, species_col = "scientificName", park_code = "BICY")
list<-te_check(data, "scientificName", "ROMO", expansion=TRUE)
# for a list of parks:
park_code<-c("ROMO", "YELL", "SAGU")
list<-te_check(data, "scientificName", park_code, expansion=TRUE)
}

}
\keyword{internal}
