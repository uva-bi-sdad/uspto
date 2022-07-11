source("eda/functions.R")

dir <- "eda/original/"

# retrieve all office actions from examiner 80488
office_actions <- download_office_actions(
  "sections.examinerEmployeeNumber:80488",
  outFile = paste0(dir, "80488_office_actions")
)

# retrieve full examination records for each of those applications
application_ids <- unique(vapply(office_actions$docs, function(a) a$patentApplicationNumber[[1]], ""))
examination <- download_peds(
  filters = list(paste0("applId:(", paste(application_ids, collapse = " "), ")")),
  outFile = paste0(dir, "80488_peds")
)

# calculate examination times within each year
examination_time <- unlist(lapply(examination$content, function(year) {
  vapply(year$prosecutionHistoryDataBag$prosecutionHistoryData, function(history) {
    time <- NA
    s <- which(history$eventCode == "FWDX")
    if (length(s)) {
      e <- grep("^(?:CTNF|CTFR|EX\\.R|N/=\\.)", history$eventCode)
      if (length(e)) {
        time <- as.numeric(Reduce("-", as.Date(history[c(min(e), min(s)), "eventDate"])))
      }
    }
    time
  }, 0)
}))

library(splot)
splot(
  examination_time ~ substring(names(examination_time), 1, 4),
  title = "Examination Times by Year for Examiner 80488",
  labx = "Year", laby = "Time (days)"
)
