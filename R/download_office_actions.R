#' Download Office Actions
#'
#' Download a set of Office actions based on a query to the
#' \href{https://developer.uspto.gov/api-catalog/uspto-office-action-text-retrieval-api}{Office Action Text Retrieval API}.
#' @param criteria Search criteria, in the form of \code{"field:value"} (e.g., \code{"examinerEmployeeNumber:80488"}).
#' See available \href{https://developer.uspto.gov/ds-api/oa_actions/v1/fields}{fields}.
#' @param outFile Path to the resulting JSON file; defaults to the hash of the query in the current working directory.
#' @param start Initial record; useful if making requests in chunks.
#' @param rows Number of rows to return; defaults to 9999.
#' @param overwrite Logical; if \code{TRUE}, will overwrite an existing file with the same \code{outFile}.
#' @param endpoint API endpoint.
#' @param verbose Logical; if \code{FALSE}, will not print status messages.
#' @return A list of office actions.
#' @examples
#' \dontrun{
#' # retrieve the office actions associated with a particular application
#' office_actions <- download_office_actions("patentApplicationNumber:13877637")
#' }
#' @export

download_office_actions <- function(criteria = "*:*", outFile = NULL, start = 0, rows = 9999, overwrite = FALSE,
                                    endpoint = "https://developer.uspto.gov/ds-api/oa_actions/v1/records", verbose = TRUE) {
  final <- normalizePath(paste0(if (is.null(outFile)) {
    paste0(sub(":", "_", criteria, fixed = TRUE), "_", Sys.Date())
  } else {
    sub("\\.json$", "", outFile)
  }, ".json"), "/", FALSE)
  if (!overwrite && file.exists(final)) {
    if (verbose) message("Reading in existing results")
    return(jsonlite::read_json(final))
  }
  query <- httr::POST(
    endpoint, httr::add_headers("Content-Type" = "application/x-www-form-urlencoded", Accept = "application/json"),
    body = paste0("criteria=", criteria, "&start=", start, "&rows=", rows)
  )
  if (200 != httr::status_code(query)) {
    error <- httr::http_error(query)
    stop("query failed:\n", if (is.logical(error)) httr::content(query) else error, call. = FALSE)
  }
  res <- httr::content(query)$response
  if (verbose) message("Found ", res$numFound, " office actions for ", criteria, "; including ", res$start, ":", length(res$docs))
  dir.create(dirname(final), FALSE, TRUE)
  jsonlite::write_json(res$docs, final, auto_unbox = TRUE, pretty = TRUE)
  invisible(res$docs)
}
