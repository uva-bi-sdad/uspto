#' Download Patents or Patent Applications
#'
#' Downloads from the U.S. Pre-Grant Publications (US-PGPUB), Patents (USPAT), and/or
#' Optical Character Recognition (USOCR) databases. See the \href{https://ppubs.uspto.gov/pubwebapp/static/pages/landing.html}{web app}
#' to search for and view documents from the same source.
#'
#' @param guid Vector of document numbers (e.g., \code{US-20220230294-A1}). Can also be a \code{data.frame} of results,
#' as returned from \code{\link{uspto_search}}; must have a \code{guid} column containing document numbers,
#' and a \code{type} column with the source database abbreviation (e.g., \code{US-PGPUB}).
#' @param outDir Path to a directory in which to save each individual document. If not specified, these are
#' saved to a temporary directory.
#' @param type A vector the same length as \code{guid} (repeated as necessary), indicating the source database of
#' the \code{guid}. Defaults to \code{US-PGPUB} for every ID.
#' @param cores Number of CPU cores to split requests across.
#' @param verbose Logical; if \code{FALSE}, does not print status messages.
#' @return A \code{data.frame} with a row for each document.
#' @seealso You can more efficiently download granted patents using \code{\link{download_patents}}.
#' @examples
#' \dontrun{
#' # start with a search
#' results <- uspto_search("G01W.CPCL.", "US-PGPUB")
#'
#' # then download those results
#' applications <- uspto_download(results)
#' }
#' @export

uspto_download <- function(guid, outDir = tempdir(), type = "US-PGPUB", cores = detectCores() - 2, verbose = FALSE) {
  if (missing(type) && !is.null(guid$type)) type <- guid$type
  if (!is.character(guid)) guid <- guid$guid
  if (!length(guid)) stop("unrecognized guid input; should be a data.frame with a guid column, or a character vector", call. = FALSE)
  type <- rep_len(type, length(guid))
  outDir <- paste0(normalizePath(outDir, "/", FALSE), "/")
  dir.create(outDir, FALSE)
  urls <- paste0("https://ppubs.uspto.gov/dirsearch-public/patents/", guid, "/highlight?queryId=0&source=", type)
  if (verbose) message("retrieving ", length(urls), " documents; saving in ", outDir)
  retrieve_document <- function(url, out = outDir) {
    file <- paste0(out, gsub("^.*patents/|/highlight.*$", "", url), ".json")
    doc <- NULL
    if (file.exists(file)) {
      doc <- as.data.frame(read_json(file)[[1]])
      doc$score <- as.numeric(doc$score)
    } else {
      req <- GET(url)
      if (req$status_code == 200) {
        res <- content(req)
        doc <- as.data.frame(lapply(res, function(e) if (is.null(e)) "N/A" else if (is.list(e)) paste0(e, collapse = "; ") else e))
        html <- grep("Html$", names(doc), TRUE, value = TRUE)
        for (e in html) {
          doc[[e]] <- gsub('<figref idref="([^"]+)">FIG\\. (\\d+)</figref>', "FIGREF FIGTYPE\\1 FIGID\\2",
            gsub("\\[(\\d+)\\]", "SECID SEC\\1",
              doc[[e]],
              perl = TRUE
            ),
            perl = TRUE
          )
        }
        write_json(doc, file, auto_unbox = TRUE)
      }
    }
    doc
  }
  res <- if (cores > 1) {
    cl <- parallel::makeCluster(max(1, min(length(urls), cores)))
    on.exit(parallel::stopCluster(cl), TRUE)
    parallel::parLapply(cl, urls, retrieve_document)
  } else {
    lapply(urls, retrieve_document)
  }
  invisible(do.call(rbind, res))
}
