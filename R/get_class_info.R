#' Get Patent Classification Information
#'
#' Retrieve U.S. patent Class definitions and subclasses from
#' \href{https://www.uspto.gov/web/patents/classification}{USPTO Classification Resources}.
#'
#' @param code A vector of USPC classification symbols (e.g., \code{D14}).
#' @returns A list with an entry for \code{class} and \code{description} for the entered code, and \code{subclasses}
#' containing a \code{data.frame} with a \code{subclass} (the subclass code) and \code{description} column.
#' @examples
#' \dontrun{
#' classifications <- get_class_info(c(428, 429))
#' }
#' @export

get_class_info <- function(code, dir = tempdir()) {
  if (length(code) > 1) {
    return(Filter(length, lapply(structure(code, names = code), function(cc) {
      tryCatch(get_class_info(cc, dir), error = function(e) {
        warning("failed to retrieve code ", cc, call. = FALSE)
        NULL
      })
    })))
  }
  code <- toupper(code)
  dir.create(dir, FALSE, TRUE)
  dir <- normalizePath(dir, "/")
  output <- paste0(dir, "/uspc", code, ".json")
  if (file.exists(output)) {
    read_json(output, simplifyVector = TRUE)
  } else {
    req <- GET(paste0("https://www.uspto.gov/web/patents/classification/uspc", code, "/sched", code, ".htm"))
    if (req$status_code == 200) {
      res <- strsplit(rawToChar(req$content), "\n+")[[1]]
      res <- res[seq(
        grep("<table summary", res, fixed = TRUE)[[1]],
        grep("Start global footer", res, fixed = TRUE)
      )]
      title <- regmatches(res[1], gregexec(">([^<]+)</", res[1]))[[1]][2, 2]
      rows <- grep("<tr", res, fixed = TRUE)
      subclasses <- as.data.frame(do.call(rbind, lapply(strsplit(res[rows[-(1:2)]], "</td>", fixed = TRUE), function(r) {
        if (length(r) > 5) {
          sm <- regexec("', '([0-9.]+)'", r[2])
          dm <- regexec("> *([A-Za-z][^>]+)<", r[5])
          if (length(sm[[1]]) == 2 && length(dm[[1]]) == 2) {
            c(
              subclass = regmatches(r[2], sm)[[1]][2],
              description = sub(" *$", "", regmatches(r[5], dm)[[1]][2])
            )
          }
        }
      })))
      colnames(subclasses) <- c("subclass", "description")
      subclasses$subclass <- as.numeric(subclasses$subclass)
      res <- list(class = code, description = title, subclasses = subclasses)
      write_json(res, output, auto_unbox = TRUE)
      res
    } else {
      stop("failed to retrieve definition of code ", code, call. = FALSE)
    }
  }
}
