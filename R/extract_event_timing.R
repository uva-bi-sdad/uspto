#' Extract Timings from Examination Events
#'
#' Extract timings between select event codes within an examination timeline.
#' @param record A matrix-like object with dates in the first column (recognizable by
#' \code{\link{as.Date}}) and event codes (recognizable by the patterns in \code{span}) in the second.
#' @param spans A list with entries defining each event window. Each entry should be a list with
#' entries for \code{start}, \code{end}, and \code{before}, each containing regular expression
#' strings identifying event codes.
#' @returns A list with a \code{data.frame} for each entry in \code{span}.
#' @examples
#' extract_event_timing(data.frame(
#'   date = c("2020-01-15", "2020-01-20"),
#'   event = c("FWDX", "CTNF")
#' ))
#' @export

extract_event_timing <- function(record, spans = list(
                                   "Initial Classification" = list(start = "^PGPC$", end = "^(?:OI|PG$)", before = "^DOCK$"),
                                   "Art Unit to Examiner" = list(start = "^DOCK$", end = "^FWDX$", before = "^(?:CTFR|EX\\.R|N/=\\.)"),
                                   "Examiner to Action" = list(start = "^FWDX$", end = "^(?:CT|EX\\.R|N/=\\.)", before = "^(?:DOCK$|A)")
                                 )) {
  if (is.list(record) && !is.data.frame(record)) {
    if ("prosecutionHistoryDataBag" %in% names(record)) {
      record <- record$prosecutionHistoryDataBag$prosecutionHistoryData
    } else if ("prosecutionHistoryDataBag" %in% names(record[[1]])) {
      timings <- lapply(record, function(r) extract_event_timing(r$prosecutionHistoryDataBag$prosecutionHistoryData))
      names(timings) <- vapply(record, function(r) r$patentCaseMetadata$applicationNumberText$value, "")
      return(timings)
    }
    record <- as.data.frame(lapply(
      as.data.frame(do.call(rbind, record)), unlist,
      recursive = FALSE, use.names = FALSE
    ))
  }
  if (ncol(record) < 2) stop("record is not in the expected format", call. = FALSE)
  record[, 1] <- as.Date(record[, 1], tryFormats = "%Y-%m-%d", options = TRUE)
  if (anyNA(record[, 1])) record <- record[!is.na(record[, 1]), ]
  record <- record[order(record[, 1]), ]
  dates <- record[, 1]
  events <- record[, 2]
  n <- length(events)
  lapply(spans, function(s) {
    pos <- lapply(s, grep, events)
    if (length(pos$start)) {
      res <- list()
      lastEnd <- 0
      current <- 0
      for (i in seq_along(pos$start)) {
        p <- pos$start[i]
        b <- pos$before[pos$before > p]
        b <- if (length(b)) b[1] else Inf
        e <- pos$end[pos$end > current & pos$end > p & pos$end < b]
        if (length(e)) {
          current <- e[1]
          res[[i]] <- data.frame(
            start = n - p + 1, startDate = dates[p], startEvent = events[p],
            end = n - current + 1, endDate = dates[current], endEvent = events[current],
            days = as.numeric(dates[current] - dates[p])
          )
        }
      }
      do.call(rbind, res)
    }
  })
}
