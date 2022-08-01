#' Download Patent Examination Data
#'
#' Download a bundled query response from the \href{https://ped.uspto.gov/peds}{Patent Examination Data System}.
#'
#' @param searchText Text to search for; \code{"*:*"} for none (default); see
#' the FAQ on the \href{https://ped.uspto.gov/peds}{PEDS site}.
#' @param filters A list of characters representing filter conditions in the form of \code{"field:value"}
#' (e.g., \code{list('appStatus:"Patented Case"')}).
#' @param outFile Name of the final JSON file, or directory in which to save files with parameter-hash names.
#' @param start Initial record.
#' @param minMatch Minimum number of terms required to match.
#' @param overwrite Logical; overwrite any previous queries with the same body hash.
#' @param waits Number of times to check for a completed bundle before giving up.
#' @param wait Number of seconds to wait between retries (how long \code{waits} are).
#' @param endpoint PEDs API endpoint.
#' @param compress Logical; if \code{FALSE}, will not xz-compress the \code{outFile}.
#' @param load Logical; if \code{FALSE}, will not return the downloaded content, or load in existing content.
#' @param verbose Logical; if \code{FALSE}, will not print status messages.
#' @return A list with an entry for each year in the set of patent metadata.
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
                          waits = 50, wait = 10, endpoint = "https://ped.uspto.gov/api/queries/", compress = TRUE, load = TRUE, verbose = TRUE) {
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
  final <- normalizePath(paste0(if (is.null(outFile) || dir.exists(outFile)) {
    paste0(if (is.null(outFile)) tempdir() else outFile, "/", hash)
  } else {
    sub("\\.json.*$", "", outFile)
  }, ".json", if (compress) ".xz"), "/", FALSE)
  if (!overwrite && file.exists(final)) {
    if (load) {
      if (verbose) message("Reading in existing results")
      return(invisible(jsonlite::read_json(final, simplifyDataFrame = FALSE)))
    } else {
      if (verbose) message("Results already exist")
      return()
    }
  }
  bundle <- paste0(normalizePath(tempdir(), "/"), "/", hash, ".zip")
  exdir <- sub(".zip", "", bundle, fixed = TRUE)
  output <- list(ID = "", content = list())
  if (load) on.exit(return(invisible(output$content)))
  if (overwrite || (!file.exists(bundle) && !dir.exists(exdir))) {
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
  if (file.exists(bundle)) {
    if (verbose) message("Unpacking ", bundle)
    system2("unzip", c("-d", shQuote(exdir), shQuote(bundle)), stdout = TRUE)
    unlink(bundle)
  }
  output$content <- unlist(lapply(list.files(exdir, full.names = TRUE), function(f) {
    res <- tryCatch(jsonlite::read_json(f, simplifyDataFrame = FALSE)$PatentData, error = function(e) NULL)
    if (is.null(res)) {
      con <- gzfile(f)
      on.exit(close(con))
      res <- tryCatch(fromJSON(paste(
        c(scan(con, "", quote = "", na.strings = "", quiet = TRUE), "]}"),
        collapse = " "
      ), simplifyDataFrame = FALSE)$PatentData, error = function(e) NULL)
    }
    res
  }), recursive = FALSE, use.names = FALSE)
  if (verbose) message("writing final results: ", normalizePath(final, "/", FALSE))
  dir.create(dirname(final), FALSE, TRUE)
  if (compress) {
    final <- xzfile(final)
    on.exit(close(final), add = TRUE)
  }
  jsonlite::write_json(output$content, final, auto_unbox = TRUE)
}
