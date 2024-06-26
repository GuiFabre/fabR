% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00-utils.R
\name{read_csv_any_formats}
\alias{read_csv_any_formats}
\title{Read a csv file using read_csv and avoid errors}
\usage{
read_csv_any_formats(filename)
}
\arguments{
\item{filename}{A character string of the path of the csv file.}
}
\value{
A tibble corresponding to the csv read.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
The csv file is read twice to detect the number of lines to use in
attributing the column type ('guess_max' parameter of read_csv). This avoids
common errors when reading csv files.
}
\examples{
{

try(read_csv_any_formats(filename = tempfile()),silent = TRUE)

}

}
\seealso{
\code{\link[readr:read_delim]{readr::read_csv()}}, \code{\link[readr:read_delim]{readr::read_csv2()}}
}
