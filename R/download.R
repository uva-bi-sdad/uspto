#' Download Patent Examination Data
#'
#' Download a bundled query response from the \href{https://ped.uspto.gov/peds}{Patent Examination Data System}.
#'
#' @param searchText Text to search for; \code{"*:*"} for none (default); see
#' the FAQ on the \href{https://ped.uspto.gov/peds}{PEDS site}.
#' @param filters A list of characters representing filter conditions in the form of \code{"field:value"}
#' (e.g., \code{list('appStatus:"Patented Case"')}).
#' @param outFile Name of the final JSON file.
#' @param start Initial record.
#' @param minMatch Minimum number of terms required to match.
#' @param overwrite Logical; overwrite any previous queries with the same body hash.
#' @param waits Number of times to check for a completed bundle before giving up.
#' @param wait Number of seconds to wait between retries (how long \code{waits} are).
#' @param endpoint PEDs API endpoint.
#' @param verbose Logical; if \code{FALSE}, will not print status messages.
#' @return An invisible list with an entries for \code{ID} (the query ID; if one was made) and
#' \code{content} (a list with an entry for each year in the set of patent metadata).
#' @examples
#' \dontrun{
#' # like case 3 in the API Documentation > API Tutorial on https://ped.uspto.gov/peds
#' download_peds(
#'   "firstNamedApplicant:(Google)",
#'   list('appStatus:"Patented Case"', "appFilingDate:[2013-01-01T00:00:00Z TO 2013-03-31T23:59:59Z]")
#' )
#' }
#' @export

download_peds <- function(searchText = "*:*", filters = list("*:*"), outFile = NULL, start = 0, minMatch = "100%", overwrite = FALSE,
                          waits = 20, wait = 10, endpoint = "https://ped.uspto.gov/api/queries/", verbose = TRUE) {
  # https://ped.uspto.gov/api/search-params
  body <- list(
    searchText = searchText,
    fq = filters,
    fl = "*",
    mm = minMatch,
    df = "patentTitle",
    qf = paste(
      "appEarlyPubNumber applId appLocation appType appStatus_txt appConfrNumber appCustNumber",
      "appGrpArtNumber appCls appSubCls appEntityStatus_txt patentNumber patentTitle primaryInventor",
      "firstNamedApplicant appExamName appExamPrefrdName appAttrDockNumber appPCTNumber",
      "appIntlPubNumber wipoEarlyPubNumber pctAppType firstInventorFile appClsSubCls rankAndInventorsList"
    ),
    facet = FALSE,
    sort = "applId asc",
    start = start
  )
  hash <- digest::digest(body)
  final <- normalizePath(paste0(if (is.null(outFile)) hash else sub("\\.json$", "", outFile), ".json"), "/", FALSE)
  if (!overwrite && file.exists(final)) {
    if (verbose) message("Reading in existing results")
    return(list(ID = "", content = jsonlite::read_json(final)))
  }
  bundle <- paste0(normalizePath(tempdir(), "/"), "/", hash, ".zip")
  output <- list(ID = "", content = list())
  on.exit(return(invisible(output)))
  if (overwrite || !file.exists(bundle)) {
    query <- httr::POST(
      endpoint, httr::add_headers("Content-Type" = "application/json", Accept = "application/json"),
      body = body, encode = "json"
    )
    if (200 != httr::status_code(query)) {
      error <- httr::http_error(query)
      errorContent <- httr::content(query)
      stop("query failed:\n", if (length(errorContent)) errorContent else if (is.logical(error)) httr::status_code(query) else error, call. = FALSE)
    }
    res <- httr::content(query)
    output$ID <- ID <- res$queryId
    if (0 == res$queryResults$searchResponse$response$numFound) stop("No records found for ", ID, call. = FALSE)
    httr::PUT(paste0(endpoint, ID, "/package?format=JSON"))
    if (verbose) message("Package requested for ", res$queryResults$searchResponse$response$numFound, " record(s): ", ID)
    retries <- 1
    while (jsonlite::read_json(paste0(endpoint, ID))$jobStatus != "COMPLETED") {
      if (retries > waits) stop("hit wait limit without seeing COMPLETED status for ", ID)
      retries <- retries + 1
      if (verbose) cat(paste0("\rwaiting for bundle (try: ", retries, ") ... "))
      Sys.sleep(wait)
    }
    if (verbose) cat("complete\n")
    url <- paste0(endpoint, ID, "/download?format=JSON")
    download.file(url, bundle, mode = "wb")
  }
  if (verbose) message("Unpacking ", bundle)
  years <- unzip(bundle, list = TRUE)$Name
  output$content <- lapply(years, function(f) {
    con <- unz(bundle, f)
    on.exit(close(con))
    jsonlite::fromJSON(paste(scan(con, "", quote = "", na.strings = "", quiet = TRUE), collapse = " "))$PatentData
  })
  names(output$content) <- sub(".json", "", years, fixed = TRUE)
  if (verbose) message("writing final results: ", normalizePath(final, "/", FALSE))
  dir.create(dirname(final), FALSE, TRUE)
  jsonlite::write_json(output$content, final, auto_unbox = TRUE, pretty = TRUE)
}

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
