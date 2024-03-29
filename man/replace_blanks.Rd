% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_blanks.R
\name{replace_blanks}
\alias{replace_blanks}
\title{Replaces all blank cells with NA}
\usage{
replace_blanks(directory = here::here())
}
\arguments{
\item{directory}{String. Path to the file(s) to have blanks replaced with
NAs. Defaults to the working directory of the project (here::here())}
}
\value{
list of data frames (invisibly)
}
\description{
Replaces all blank cells with NA
}
\details{
\code{replace_blanks()} is particularly useful for exporting data from a
database (such as access) and converting it to a data package with metadata.

\code{replace_blanks()} will import all .csv files in the current working
directory. The files are then written back out to the same directory,
overwriting the old .csv files. Any blank cells in the original .csv files
will be replaced with NA.

One exception is if a .csv contains NO data (i.e. just column names and no
data in any of the cells). In this case, the blanks will not be replaced with
NA (as the function cannot determine how many NAs to include).
}
\examples{
\dontrun{
replace_blanks()
}
}
