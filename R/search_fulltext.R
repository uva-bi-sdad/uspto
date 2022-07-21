#' Search for Patents or Patent Applications (legacy)
#'
#' Searches the Application (AppFT) or Patent (PatFT)
#' \href{https://appft.uspto.gov}{Full-Text Databases}, and returns
#' application/patent numbers and titles.
#'
#' @param query A query string. This can be a set of terms (e.g., \code{"term1 OR term2"}) or
#' an exact phrase (e.g., \code{'"a phrase"'}). To search within a particular
#' \href{https://appft.uspto.gov/netahtml/PTO/help/helpflds.html}{field}, prefix with the field code
#' and a forward slash (e.g., \code{"CPCL/G01W"}, which searches for "G01W" in the CPCL field).
#' @param applications Logical; specifies whether to search in the application database
#' (AppFT; \code{TRUE}; default) or the patent database (PatFT; \code{FALSE}).
#' @param limit A limit to the number of results retrieved, in steps of 50. By default, will
#' try to retrieve all results.
#' @param retries Number of retries for a given results page.
#' @param outdir Path to a directory in which to save results. Only writes if specified.
#' @param overwrite Logical; if \code{TRUE}, will overwrite caches results.
#' @param cores Number of CPU cores to use when reading in multiple pages.
#' @param verbose Logical; if \code{FALSE}, does not print status messages.
#' @return A \code{data.frame} with columns for \code{index} (index of the result),
#' \code{id} (patent number or application document number), and \code{title} (title of patent or application).
#' @examples
#' \dontrun{
#' # search for applications with a meteorology classification:
#' # https://www.uspto.gov/web/patents/classification/cpc/html/cpc-G01W.html
#' results <- search_fulltext("CPCL/G01W")
#'
#' # search for patents by date and title keyword
#' results <- search_fulltext("isd/1/8/2002 and motorcycle", FALSE)
#' }
#' @export

search_fulltext <- function(query, applications = TRUE, limit = NULL, retries = 20, outdir = NULL,
                            overwrite = FALSE, cores = parallel::detectCores() - 2, verbose = TRUE) {
  base_url <- paste0(
    "https://", if (applications) "app" else "pat",
    "ft.uspto.gov/netacgi/nph-Parser?Sect1=PTO2&Sect2=HITOFF&u=/netahtml/PTO/search-adv.html&r=0&f=S&l=50&d=",
    if (applications) "PG01" else "PTXT", "&p="
  )
  query <- URLencode(gsub("\\s+", "+", query, perl = TRUE))
  outFile <- normalizePath(paste0(outdir, "/", digest::digest(query), ".csv"), "/", FALSE)
  if (overwrite) unlink(outFile)
  if (file.exists(outFile)) {
    if (verbose) message("reading existing file: ", outFile)
    read.csv(outFile)
  } else {
    retrieve_page <- function(input, attempt = retries, parse = TRUE) {
      results <- tryCatch(
        {
          con <- curl::curl(paste0(base_url, input, "&Query=", query))
          on.exit(close(con))
          readLines(con, warn = FALSE)
        },
        error = function(e) e$message
      )
      if (length(results) < 100) {
        if (attempt > 0) {
          Sys.sleep(.5)
          retrieve_page(input, attempt - 1, parse)
        } else {
          NULL
        }
      } else {
        if (parse) parse_page(results) else results
      }
    }
    parse_page <- function(results) {
      rows <- c(grep("^<TR><TD", results, perl = TRUE), length(results))
      res <- as.data.frame(do.call(rbind, lapply(seq_len(length(rows) - 1), function(i) {
        row <- results[seq(rows[i], rows[i + 1])]
        split_row <- strsplit(c(row[1:2], paste(row[-(1:2)], collapse = " ")), "[><]")
        c(split_row[[1]][5], split_row[[2]][5], split_row[[3]][if (split_row[[3]][5] == "") 11 else 5])
      })), make.names = FALSE, optional = TRUE)
      colnames(res) <- c("index", "id", "name")
      res$index <- as.integer(res$index)
      res$name <- sub("\\s+$", "", gsub("\\s+", " ", res$name, perl = TRUE), perl = TRUE)
      res
    }
    initial <- retrieve_page(1, retries, FALSE)
    if (length(initial) < 30) stop("Failed to retrieve initial page: ", initial, call. = FALSE)
    report <- grep("Results of Search", initial[1:100], fixed = TRUE)
    if (!length(report)) stop("Response is in an unexpected format; check ", paste0(base_url, 1, "&Query=", query), call. = FALSE)
    n <- as.numeric(regmatches(initial[report], regexec("(\\d+)\\s\\w+\\.$", initial[report]))[[1]][2])
    if (verbose) message("found ", n, " results", if (is.numeric(limit)) paste("; retrieving", limit))
    if (is.numeric(limit)) n <- min(limit, n)
    pages <- ceiling(n / 50)
    res <- if (pages > 1) {
      res <- if (cores > 1) {
        cl <- parallel::makeCluster(max(1, min(pages, cores)))
        parallel::clusterExport(cl, c("base_url", "query", "retrieve_page", "parse_page"), environment())
        on.exit(parallel::stopCluster(cl), TRUE)
        parallel::parLapply(cl, seq(2, pages), retrieve_page)
      } else {
        lapply(seq(2, pages), retrieve_page)
      }
      if (verbose) {
        missed <- vapply(res, length, 0) == 0
        if (any(missed)) warning("missed pages: ", paste(which(missed) + 1, collapse = ", "))
      }
      rbind(parse_page(initial), do.call(rbind, res))
    } else {
      parse_page(initial)
    }
    if (!is.null(outdir)) {
      dir.create(outdir, FALSE, TRUE)
      write.csv(res, outFile, row.names = FALSE)
    }
    res
  }
}
