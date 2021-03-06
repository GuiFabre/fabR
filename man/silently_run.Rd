% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00-lazy.R
\name{silently_run}
\alias{silently_run}
\title{Shortcut to silently run a code chunk avoiding error (with try), messages and warnings}
\usage{
silently_run(...)
}
\arguments{
\item{...}{R code}
}
\description{
Shortcut avoiding user to get messages, warnings and being stopped by an error.
The usage is very similar to [base::suppressWarnings()]. This function is targeted
for function creators where user experience enhancement is sought.
}
\examples{
\dontrun{
# example xxx

silently_run()

}

}
