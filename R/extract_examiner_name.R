#' Get Examiner Name from Office Actions
#'
#' Extract the examiner's name from office action body text.
#' @param office_action A list of office action, as retrieved with \code{\link{download_office_actions}};
#' each entry should be a list with entries for \code{bodyText} (the office action text) and
#' \code{examinerEmplyeeNumber}.
#' @param collapse Logical; if \code{FALSE}, will return a row for every entered office action.
#' @return A character vector with the Examiner's employee number and two possible names: one
#' extracted from the language (\code{intext}) and another from the sign-off (\code{signed}). The name
#' will be \code{NA} if there is no examiner (such as if a non-examiner submitted the action), or
#' if the format is not as expected. If \code{office_action} is a list of multiple office actions,
#' these will make up rows of a \code{data.frame}, which will be as many as the length of
#' \code{office_actions} if \code{collapse} is \code{FALSE}, or the length of unique IDs.
#' @examples
#' \dontrun{
#' # retrieve the office actions associated with a particular application
#' office_actions <- download_office_actions("patentApplicationNumber:13877637")
#'
#' # get the name of the examiner associated with the first action:
#' extract_examiner_name(office_actions[[1]])
#' }
#' @export

extract_examiner_name <- function(office_action, collapse = TRUE) {
  if (length(office_action) > 1 && "bodyText" %in% names(office_action[[1]])) {
    names <- do.call(rbind, lapply(office_action, extract_examiner_name))
    if (collapse) {
      nas <- is.na(names[, 1])
      if (any(nas)) names[nas, 1] <- paste0("NA", seq_len(sum(nas)))
      names <- do.call(rbind, lapply(split(as.data.frame(names), names[, 1]), function(e) {
        if (nrow(e) < 2) {
          e[1, ]
        } else {
          intext <- e[, 2]
          intext <- intext[!is.na(intext)]
          signed <- e[, 3]
          signed <- signed[!is.na(signed)]
          data.frame(
            id = e[1, "id"],
            intext = if (length(intext)) intext[which.max(nchar(intext))] else NA,
            signed = if (length(signed)) signed[which.max(nchar(signed))] else NA
          )
        }
      }))
    }
    return(names)
  }
  if (!is.list(office_action)) office_action <- list(bodyText = office_action)
  if (is.list(office_action$bodyText)) office_action$bodyText <- unlist(office_action$bodyText, FALSE, FALSE)
  body <- sub("\\s+$", "", office_action$bodyText, perl = TRUE)
  if (!length(body)) stop("office_action is not in the expected format", call. = FALSE)
  intext <- regmatches(body, regexec(
    "directed to (?:primary )?(?:examiner )?([A-Z][^/\\n\\t]+?) who(?: may be reached|se (?:telephone )?number)",
    body,
    perl = TRUE
  ))
  intext <- if (length(intext) && length(intext[[1]])) {
    intext <- gsub("(?:Primary\\s|Petitions\\s)?Examiner\\s|,$", "", intext[[1]][2], perl = TRUE)
  } else {
    NULL
  }
  l <- nchar(body)
  foot <- paste0(sub("\\s{10,}", " ", if (l > 1000) substring(body, l - 2000) else body, perl = TRUE), "XXEE")
  parts <- strsplit(
    gsub("\\s+[\\w.]+@uspto\\.gov|number='\\d+'|[A-Z][a-z]+ \\d+, \\d{4}", "", foot, perl = TRUE),
    paste0(
      "(?:[^\\w\\s]\\W*|[\\d/\\t\\n-]+)(?:",
      "Examiner|Primary\\s+Examiner|Petitions\\s+Examiner",
      ")\\s?(?:[A-Z]{2}|Art|Unit|[\\d\\n,]+|XX)"
    ),
    perl = TRUE
  )[[1]]
  signed <- if (length(parts) > 1) {
    parts <- strsplit(parts[1], "[\\n/]+", perl = TRUE)[[1]]
    sub("_x000d_.*|\\s+$", "", gsub("\\s+", " ", parts[length(parts)], perl = TRUE))
  } else {
    NULL
  }
  c(
    id = if (is.null(office_action$examinerEmployeeNumber)) NA else trimws(office_action$examinerEmployeeNumber[[1]]),
    intext = if (length(intext) && intext != "") intext else NA,
    signed = if (length(signed) && signed != "") signed else NA
  )
}
