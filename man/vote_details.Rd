% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bills.R
\name{vote_details}
\alias{vote_details}
\title{Get detailed information about a specific vote}
\usage{
vote_details(vote_id)
}
\arguments{
\item{vote_id}{The unique identifier of the vote}
}
\value{
A tibble containing detailed vote information including legislator votes
}
\description{
This function retrieves detailed information about a vote by its ID.
}
\examples{
vote_details(67890)

}
