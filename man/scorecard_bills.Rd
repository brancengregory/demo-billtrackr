% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scorecards.R
\name{scorecard_bills}
\alias{scorecard_bills}
\title{Get bills from a specific scorecard}
\usage{
scorecard_bills(scorecard_id)
}
\arguments{
\item{scorecard_id}{The unique identifier of the scorecard}
}
\value{
A tibble of bills contained in the specified scorecard
}
\description{
This function retrieves all bills contained in a specified scorecard.
}
\examples{
scorecard_bills(12345)

}
