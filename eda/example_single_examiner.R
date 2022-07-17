library(uspto)
library(splot)

dir <- "eda/original/"

# retrieve all office actions from examiner 80488
office_actions <- download_office_actions(
  "sections.examinerEmployeeNumber:80488",
  outFile = paste0(dir, "80488_office_actions.json")
)

# retrieve full examination records for each of those applications
application_ids <- unique(vapply(office_actions, function(a) a$patentApplicationNumber[[1]], ""))
examination <- download_peds(
  filters = list(paste0("applId:(", paste(application_ids, collapse = " "), ")")),
  outFile = paste0(dir, "80488_peds.json")
)

# calculate examination times within each year
examination_time <- unlist(lapply(examination$content, function(year) {
  vapply(if (is.data.frame(year)) year$prosecutionHistoryDataBag$prosecutionHistoryData else year, function(y) {
    history <- if (is.data.frame(y)) y else as.data.frame(do.call(rbind, y$prosecutionHistoryDataBag$prosecutionHistoryData))
    time <- NA
    s <- which(history$eventCode == "FWDX")
    if (length(s)) {
      e <- grep("^(?:CTNF|CTFR|EX\\.R|N/=\\.)", history$eventCode)
      if (length(e)) {
        time <- as.numeric(Reduce("-", as.Date(unlist(history[c(min(e), min(s)), "eventDate"]))))
      }
    }
    time
  }, 0)
}))
examination_time <- examination_time[!is.na(examination_time)]

splot(
  examination_time ~ seq_along(examination_time),
  title = "Examination Times by Index for Examiner 80488",
  labx = "Index", laby = "Time (days)", myl = c(0, 40)
)
 
