% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_dc_cols.R
\name{DC_col_check}
\alias{DC_col_check}
\title{TDWG Darwin Core Column Name Check 08-23-2022}
\usage{
DC_col_check(working_df)
}
\arguments{
\item{working_df}{\itemize{
\item This is the dataframe you want to run against the function. To call, simply write working_df = "the name of your dataframe".
}}
}
\value{
\itemize{
\item The function returns a list of the column names you should fix (not fitting with simple Darwin Core terms, custom name formatting, data quality flagging formatting). Additionally, a small summary table is printed with the counts of the columns falling under each category (DarwinCore, Custom, DQ, Fix Me).
}
}
\description{
\code{lifecycle::badge("deprecated")}

\code{DC_col_check()} was deprecated in favor of \code{check_dc_cols()} to enforce consistency in function naming throughout the package and to be consistent with tidyverse style guides.

DC_col_check checks to see if the column names in your dataframe match the standardized simple Darwin Core names established by the Taxonomic Databases Working Group
}
\details{
A dataframe is created with all the simple DarwinCore terms, drawn from Darwin Core reference guide: https://dwc.tdwg.org/terms/ last updated 07-15-2021. We have chosen to align ourselves mostly with the simple Darwin Core rules: https://dwc.tdwg.org/simple/. The function runs through each of the column names in your working dataframe to see if they match 1. A standard simple DarwinCore name 2. A name with a pattern of strings matching "custom_", indicating a custom made column  or 3. A name with a pattern of strings matching "_DQ", indicating a data quality flag. If the column name does not fit within any of the three categories, a "Fix me" statement is printed alongside the column name. The function then counts all of the names fitting within each category and prints a summary table.
}
\examples{
\dontrun{
DC_col_check(yourdataframe)
}
}
\keyword{internal}
