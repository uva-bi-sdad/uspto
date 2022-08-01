#' Get inventor information
#'
#' Extract inventor information from patent/application documents or examination records.
#'
#' @param input A \code{data.frame} of documents (as returned from \code{\link{uspto_download}})
#' or a list of examination records (as returned from \code{\link{download_peds}}).
#' @returns A \code{data.frame} with inventor information.
#' @examples
#' \dontrun{
#' # download an application
#' application <- uspto_download("US-20040216465-A1")
#'
#' # get inventor information from it
#' extract_inventors(application)
#'
#' # get the same from examination record
#' extract_inventors(download_peds("applId:10612573"))
#' }
#' @export

extract_inventors <- function(input) {
  if (is.data.frame(input)) {
    do.call(rbind, unname(lapply(split(
      input[, c("guid", "applicationNumber", "inventorsName", "inventorCountry", "inventorState", "inventorCity")],
      unlist(input$guid, FALSE, FALSE)
    ), function(d) {
      res <- if (grepl(";", d$inventorState, fixed = TRUE)) {
        i <- lapply(d[, -c(1:2)], function(e) strsplit(e[[1]], "; ", fixed = TRUE)[[1]])
        data.frame(
          as.list(unlist(d[, 1:2])),
          do.call(rbind, lapply(strsplit(i$inventorsName, ",?\\s+"), function(n) {
            data.frame(
              firstName = n[2], middleName = if (length(n) > 2) paste(n[-(1:2)], collapse = " "), lastName = n[1]
            )
          })),
          i[-1]
        )
      } else {
        d
      }
      rownames(res) <- NULL
      res
    })))
  } else {
    ex <- function(r) {
      if (length(r) == 1) r <- r[[1]]
      party <- r$patentCaseMetadata$partyBag$applicantBagOrInventorBagOrOwnerBag
      app <- list(
        guid = sub("(\\d+)", "-\\1-", r$patentCaseMetadata$patentPublicationIdentification$publicationNumber),
        applicationNumber = sub("(\\d{2})", "\\1/", r$patentCaseMetadata$applicationNumberText$value)
      )
      if (!length(app$guid)) app$guid <- ""
      if (!is.null(party)) {
        i <- which(vapply(party, function(p) if (length(p)) names(p) else "", "") == "inventorOrDeceasedInventor")
        if (length(i)) {
          do.call(rbind, lapply(party[[i]]$inventorOrDeceasedInventor, function(g) {
            do.call(rbind, lapply(g$contactOrPublicationContact, function(p) {
              data.frame(
                app,
                p$name$personNameOrOrganizationNameOrEntityName[[1]]$personStructuredName,
                inventorCountry = p$countryCode, inventorState = p$geographicRegionName$value, inventorCity = p$cityName
              )
            }))
          }))
        }
      }
    }
    if ("patentCaseMetaData" %in% names(input[[1]])) do.call(rbind, lapply(input, ex)) else ex(input)
  }
}
