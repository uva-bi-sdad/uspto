#' Download Office Actions
#'
#' Download a set of Office actions based on a query to the
#' \href{https://developer.uspto.gov/api-catalog/uspto-office-action-text-retrieval-api}{Office Action Text Retrieval API}.
#' @param criteria Search criteria, in the form of \code{"field:value"} (e.g., \code{"examinerEmployeeNumber:80488"}).
#' See available \href{https://developer.uspto.gov/ds-api/oa_actions/v1/fields}{fields}.
#' @param outFile Path to the resulting JSON file; defaults to the hash of the query in the current working directory.
#' @param start Initial record; useful if making requests in chunks.
#' @param limit Maximum number of office actions to return; under 1,000, or in steps of 1,000; defaults to all found.
#' @param overwrite Logical; if \code{TRUE}, will overwrite an existing file with the same \code{outFile}.
#' @param endpoint API endpoint.
#' @param compress Logical; if \code{FALSE}, will not write a compressed file.
#' @param cores Number of CPU cores to split calls across if necessary.
#' @param verbose Logical; if \code{FALSE}, will not print status messages.
#' @return A list of office actions.
#' @examples
#' \dontrun{
#' # retrieve the office actions associated with a particular application
#' office_actions <- download_office_actions("patentApplicationNumber:13877637")
#' }
#' @export

download_office_actions <- function(criteria = "*:*", outFile = NULL, start = 0, limit = FALSE, overwrite = FALSE,
                                    endpoint = "https://developer.uspto.gov/ds-api/oa_actions/v1/records",
                                    compress = TRUE, cores = detectCores() - 2, verbose = TRUE) {
  final <- normalizePath(paste0(if (is.null(outFile)) {
    paste0(tempdir(), "/", if (!missing(criteria)) paste0(sub(":", "_", criteria, fixed = TRUE), "_"), Sys.Date())
  } else {
    sub("\\.json.*$", "", outFile)
  }, ".json", if (compress) ".xz"), "/", FALSE)
  if (!overwrite && file.exists(final)) {
    if (verbose) message("Reading in existing results")
    return(invisible(jsonlite::read_json(final)))
  }
  query <- httr::POST(
    endpoint, httr::add_headers("Content-Type" = "application/x-www-form-urlencoded", Accept = "application/json"),
    body = paste0("criteria=", criteria, "&start=", start, "&rows=", min(1000, limit))
  )
  if (200 != httr::status_code(query)) {
    error <- httr::http_error(query)
    stop("query failed:\n", if (is.logical(error)) httr::content(query) else error, call. = FALSE)
  }
  res <- httr::content(query)$response
  limit <- if (is.numeric(limit)) min(limit, res$numFound) else res$numFound
  if (verbose) {
    message(
      "Found ", res$numFound, " office actions for ",
      if (nchar(criteria) > 104) paste(substring(criteria, 1, 100), "...") else criteria
    )
  }
  if (length(res$docs) < limit) {
    pages <- ceiling((limit - length(res$docs)) / 1000)
    get_page <- function(i) {
      query <- httr::POST(
        endpoint, httr::add_headers("Content-Type" = "application/x-www-form-urlencoded", Accept = "application/json"),
        body = paste0("criteria=", criteria, "&start=", 100 + (i - 1) * 1000, "&rows=1000")
      )
      if (200 == httr::status_code(query)) {
        httr::content(query)$response$docs
      }
    }
    res2 <- if (cores > 1) {
      cl <- parallel::makeCluster(max(1, min(pages, cores)))
      parallel::clusterExport(cl, "criteria", environment())
      on.exit(parallel::stopCluster(cl), TRUE)
      parallel::parLapply(cl, seq_len(pages), get_page)
    } else {
      lapply(seq_len(pages), get_page)
    }
    res$docs <- c(res$docs, unlist(res2, recursive = FALSE))
  }
  dir.create(dirname(final), FALSE, TRUE)
  if (compress) {
    con <- xzfile(final)
    on.exit(close(con), add = TRUE)
  }
  json <- toJSON(res$docs, auto_unbox = TRUE)
  writeLines(json, if (compress) con else final)
  invisible(fromJSON(json))
}
