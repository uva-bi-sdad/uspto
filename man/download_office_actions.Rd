% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_office_actions.R
\name{download_office_actions}
\alias{download_office_actions}
\title{Download Office Actions}
\usage{
download_office_actions(criteria = "*:*", outFile = NULL, start = 0,
  limit = FALSE, overwrite = FALSE,
  endpoint = "https://developer.uspto.gov/ds-api/oa_actions/v1/records",
  compress = TRUE, cores = detectCores() - 2, verbose = TRUE)
}
\arguments{
\item{criteria}{Search criteria, in the form of \code{"field:value"} (e.g., \code{"examinerEmployeeNumber:80488"}).
See available \href{https://developer.uspto.gov/ds-api/oa_actions/v1/fields}{fields}.}

\item{outFile}{Path to the resulting JSON file; defaults to the hash of the query in the current working directory.}

\item{start}{Initial record; useful if making requests in chunks.}

\item{limit}{Maximum number of office actions to return; under 1,000, or in steps of 1,000; defaults to all found.}

\item{overwrite}{Logical; if \code{TRUE}, will overwrite an existing file with the same \code{outFile}.}

\item{endpoint}{API endpoint.}

\item{compress}{Logical; if \code{FALSE}, will not write a compressed file.}

\item{cores}{Number of CPU cores to split calls across if necessary.}

\item{verbose}{Logical; if \code{FALSE}, will not print status messages.}
}
\value{
A list of office actions.
}
\description{
Download a set of Office actions based on a query to the
\href{https://developer.uspto.gov/api-catalog/uspto-office-action-text-retrieval-api}{Office Action Text Retrieval API}.
}
\examples{
\dontrun{
# retrieve the office actions associated with a particular application
office_actions <- download_office_actions("patentApplicationNumber:13877637")
}
}
