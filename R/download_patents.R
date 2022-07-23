#' Retrieve Full Patent Text
#'
#' Search for and return a set of full patent texts.
#'
#' @param ids A vector of application numbers, patent numbers, or document IDs (e.g., \code{"US9748552"}).
#' @param outFile Path to a \code{.csv} file to write results to.
#' @param ids_type Specifies what \code{ids} are, between \code{applications} (default), \code{patents},
#' \code{publication} (for publication document IDs), or \code{grant} (for grant document IDs).
#' @param query Search query; either a term/phrase (\code{searchText}) or set of Boolean criteria (\code{criteriaSearchText}).
#' @param inventionTitle Invention title, as it appears on the first page of the specification.
#' @param inventionSubjectMatterCategory Patent category (e.g., \code{DESIGN}).
#' @param assigneeEntityName Name of the patent assignee.
#' @param fromDate,toDate,filingDateFromDate,filingDateToDate Publication/grant and/or filing date ranges
#' in \code{YYYY-MM-DD} format (e.g., \code{2019-01-20}).
#' @param inventorNameText,claimText,abstractText,descriptionText Text to search for in specific portions;
#' \code{query} will search in all portions if it is a term or phrase.
#' @param sortField Field by which to sort results; defaults to date.
#' @param ascending Logical; if \code{TRUE}, will sort in ascending order.
#' @param grant Logical; if \code{TRUE}, searches for grants rather than publications.
#' @param limit Maximum number of results to return, in steps of 100.
#' @param series Series code to prepend to \code{ids} if they are application or patent numbers and not already included.
#' @param cores Number of CPU cores to use when retrieving multiple pages of results.
#' @param verbose Logical; if \code{TRUE}, will print status messages.
#' @return Path to the downloaded package, if it exists and \code{await} was a number; otherwise \code{NULL}.
#' @examples
#' \dontrun{
#' # retrieve a specific set of patents:
#' patents <- download_patents(c(16978600, 17087826, 16041592, 16727940, 17070594))
#' }
#' @export

download_patents <- function(ids = NULL, outFile = NULL, ids_type = "application", query = NULL,
                             inventionTitle = NULL, inventionSubjectMatterCategory = NULL, assigneeEntityName = NULL,
                             fromDate = NULL, toDate = NULL, filingDateFromDate = NULL,
                             filingDateToDate = NULL, inventorNameText = NULL, claimText = NULL,
                             abstractText = NULL, descriptionText = NULL, sortField = NULL, ascending = FALSE,
                             grant = FALSE, limit = FALSE, series = "US", cores = detectCores() - 2, verbose = FALSE) {
  if (!is.null(outFile) && file.exists(outFile)) {
    if (verbose) message("reading in existing file: ", outFile)
    res <- read.csv(outFile)
  } else {
    args <- list(
      inventionSubjectMatterCategory = inventionSubjectMatterCategory,
      filingDateFromDate = filingDateFromDate,
      filingDateToDate = filingDateToDate,
      inventionTitle = inventionTitle,
      assigneeEntityName = assigneeEntityName,
      inventorNameText = inventorNameText,
      claimText = claimText,
      abstractText = abstractText,
      descriptionText = descriptionText,
      sortField = sortField,
      sortOrder = if (ascending) "asc" else "desc"
    )
    if (!is.null(ids)) {
      id_field <- c(
        ap = "patentApplicationNumber", pa = "patentNumber",
        do = paste0(if (grant) "grant" else "publication", "DocumentIdentifier"),
        pu = "publicationDocumentIdentifier", gr = "grantDocumentIdentifier"
      )[tolower(substring(ids_type, 1, 2))]
      if (!grant && grepl("grant", id_field, fixed = TRUE)) grant <- TRUE
      if (is.na(id_field)) stop("ids_type not recognized", call. = FALSE)
      args[id_field] <- format_numbers(ids, if (grepl("Document", id_field, fixed = TRUE)) "" else series)
    }
    if (!is.null(query)) {
      isCriteria <- grepl(":", query, fixed = TRUE)
      args[if (isCriteria) "criteriaSearchText" else "searchText"] <- query
      if (isCriteria) args$largTextSearchFlag <- "Y"
    }
    type <- if (grant) "grant" else "publication"
    if (!is.null(toDate)) args[paste0(type, "ToDate")] <- toDate
    if (!is.null(fromDate)) args[paste0(type, "FromDate")] <- fromDate
    if (is.numeric(limit) && limit < 100) args$rows <- limit
    args <- Filter(length, args)
    req <- GET(paste0(
      "https://developer.uspto.gov/ibd-api/v1/application/", if (grant) "grants" else "publications",
      "?", paste(paste0(names(args), "=", unlist(args, use.names = FALSE)), collapse = "&")
    ))
    res <- tryCatch(content(req), error = function(e) http_status(req))
    if (length(res) && !is.null(res$error)) stop("request failed: ", res$status, ": ", res$error, call. = FALSE)
    retrieved <- length(res$results)
    if (!retrieved) stop("no results were not found", call. = FALSE)
    n <- if (is.numeric(limit)) min(res$recordTotalQuantity, limit) else res$recordTotalQuantity
    if (n > retrieved) {
      pages <- ceiling(n / 100) - 1
      base_url <- paste0(
        "https://developer.uspto.gov/ibd-api/v1/application/", if (grant) "grants" else "publications",
        "?", paste(paste0(names(args), "=", unlist(args, use.names = FALSE)), collapse = "&"), "&start="
      )
      urls <- vapply(seq_len(pages), function(i) paste0(base_url, i * 100), "")
      get_page <- function(url) {
        res <- tryCatch(content(GET(url))$results, error = function(e) NULL)
        if (is.null(res)) warning("failed to retrieve ", url)
        res
      }
      res2 <- if (cores > 1) {
        cl <- parallel::makeCluster(max(1, min(pages, cores)))
        on.exit(parallel::stopCluster(cl), TRUE)
        parallel::parLapply(cl, urls, get_page)
      } else {
        lapply(urls, get_page)
      }
      if (length(res2)) {
        res$results <- c(res$results, unlist(res2, recursive = FALSE))
      } else if (verbose) warning("failed to retrieve more than the initial results", call. = FALSE)
    }
    res <- as.data.frame(do.call(cbind, lapply(as.data.frame(do.call(rbind, lapply(res$results, function(d) {
      lapply(d, function(e) if (is.null(e)) NA else if (is.list(e)) paste(e, collapse = " ") else e)
    }))), unlist)))
    if (verbose) message("found ", nrow(res))
    if (!is.null(outFile)) {
      if (verbose) message("writing results to ", outFile)
      dir.create(dirname(outFile), FALSE, TRUE)
      write.csv(res, outFile, row.names = FALSE)
    }
  }
  res
}

format_numbers <- function(numbers, series) {
  paste(sub("^[A-Z]{2}([A-Z]{2})", "\\1", paste0(series, gsub("[/,]+", "", numbers)), FALSE, perl = TRUE), collapse = ",")
}
