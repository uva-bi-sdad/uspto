#' Search for Patents or Patent Applications
#'
#' Searches the U.S. Pre-Grant Publications (US-PGPUB), Patents (USPAT), and/or
#' Optical Character Recognition (USOCR) databases. See the \href{https://ppubs.uspto.gov/pubwebapp/static/pages/landing.html}{web app}
#' for an interactive search.
#'
#' @param query A query string; for example \code{"noodle"}, \code{"photographic AND noodle"},
#' \code{"wet adj4 silicon"}, or \code{"G01W.CPCL."}. See the Patent Public Search
#' \href{https://ppubs.uspto.gov/pubwebapp/static/pages/searchable-indexes.html}{training materials}.
#' @param databases A character vector specifying which databases to search in; a selection from
#' \code{c("US-PGPUB", "USPAT", "USOCR")}.
#' @param outFile Name of a \code{.csv} file to save results to. If this file exists, it will
#' be loaded instead of searching.
#' @param limit Maximum number of results to return; defaults to all results.
#' @param sort How to sort results; defaults to publication date (\code{"date_publ desc"}).
#' @param english_only Logical; if \code{FALSE}, will return patent and/or applications of any language.
#' @param spellCheck Logical; if \code{TRUE}, will spellcheck \code{query} terms.
#' @param plurals Logical; if \code{TRUE}, will look for plural and singular forms of \code{query} terms.
#' @param britishEquivalents Logical; if \code{TRUE}, will look for British and American forms of \code{query} terms.
#' @param verbose Logical; if \code{FALSE}, does not print status messages.
#' @return A \code{data.frame} of results, if any were found; otherwise \code{NULL}.
#' @examples
#' \dontrun{
#' # search for applications with a meteorology classification:
#' # https://www.uspto.gov/web/patents/classification/cpc/html/cpc-G01W.html
#' results <- uspto_search("G01W.CPCL.", "US-PGPUB")
#'
#' # search by date and keyword:
#' results <- uspto_search("20020801.pd. AND motorcycle")
#' }
#' @export

uspto_search <- function(query, databases = c("US-PGPUB", "USPAT", "USOCR"), outFile = NULL, limit = FALSE, sort = "date_publ desc",
                         english_only = TRUE, spellCheck = FALSE, plurals = TRUE, britishEquivalents = TRUE, verbose = FALSE) {
  if (!is.null(outFile)) {
    outFile <- sub("\\.csv.*$", ".csv", outFile)
    if (file.exists(outFile)) {
      if (verbose) message("loading existing results")
      return(read.csv(outFile))
    }
  }
  databases <- unname(c(b = "US-PGPUB", t = "USPAT", r = "USOCR")[unique(tolower(substring(databases, nchar(databases))))])
  if (!length(databases)) stop("no recognized databases entered", call. = FALSE)
  session_request <- POST(
    "https://ppubs.uspto.gov/dirsearch-public/users/me/session",
    add_headers("Content-Type" = "application/json"),
    body = "-1"
  )
  session <- content(session_request)
  if (is.null(session$userSessionId)) stop("failed to start a session: ", session, call. = FALSE)
  query_object <- list(
    britishEquivalents = britishEquivalents,
    caseId = session$userCase$caseId,
    databaseFilters = lapply(databases, function(s) list(databaseName = s, countryCodes = list())),
    highlights = "0",
    hl_snippets = "2",
    ignorePersist = TRUE,
    op = "OR",
    plurals = plurals,
    q = query,
    qt = "brs",
    queryName = query,
    searchType = 1,
    spellCheck = spellCheck,
    userEnteredQuery = query,
    viewName = "tile"
  )
  count_request <- POST(
    "https://ppubs.uspto.gov/dirsearch-public/searches/counts",
    add_headers("Content-Type" = "application/json"),
    body = toJSON(list(
      britishEquivalents = britishEquivalents,
      caseId = session$userCase$caseId,
      databaseFilters = lapply(databases, function(s) list(databaseName = s, countryCodes = list())),
      highlights = "0",
      hl_snippets = "2",
      ignorePersist = TRUE,
      op = "OR",
      plurals = plurals,
      q = query,
      qt = "brs",
      queryName = query,
      searchType = 1,
      spellCheck = spellCheck,
      userEnteredQuery = query,
      viewName = "tile"
    ), auto_unbox = TRUE)
  )
  count <- content(count_request)
  if (is.null(count$numResults)) stop("failed to execute query: ", count, call. = FALSE)
  if (verbose) message("matches: ", count$numResults)
  if (count$numResults != 0) {
    result_request <- POST(
      "https://ppubs.uspto.gov/dirsearch-public/searches/searchWithBeFamily",
      add_headers("Content-Type" = "application/json"),
      body = toJSON(list(
        start = 0,
        pageCount = if (is.numeric(limit)) limit else 99999999,
        sort = sort,
        docFamilyFiltering = "familyIdFiltering",
        searchType = 1,
        familyIdEnglishOnly = english_only,
        familyIdFirstPreferred = "USPAT",
        familyIdSecondPreferred = "US-PGPUB",
        familyIdThirdPreferred = "FPRS",
        showDocPerFamilyPref = "showEnglish",
        queryId = 0,
        tagDocSearch = FALSE,
        query = query_object
      ), auto_unbox = TRUE)
    )
    if (status_code(result_request) != 200) stop("failed to retrieve results: ", status_code(result_request), call. = FALSE)
    result <- content(result_request)
    if (is.null(result$patents)) stop("failed to retrieve results: ", result, call. = FALSE)
    res <- do.call(rbind, lapply(result$patents, function(d) {
      if (is.null(d$collapsed)) d$collapsed <- TRUE
      if (is.null(d$lastChild)) d$lastChild <- TRUE
      if (is.null(d$familyCount)) d$familyCount <- 1
      lapply(d, function(e) if (!length(e)) NA else if (is.list(e)) paste0(e, collapse = "; ") else e)
    }))
    res <- as.data.frame(lapply(as.data.frame(res), unlist))
    if (!is.null(outFile)) {
      if (verbose) message("saving results: ", outFile)
      write.csv(res, outFile, row.names = FALSE)
    }
    res
  }
}
