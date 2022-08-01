library(jsonlite)
library(uspto)

# search for applications
files <- paste0("eda/searches/", c("pharmaceutical.csv", "mechanical.csv"), ".xz")
if (file.exists(files[1])) {
  pharmaceutical <- read.csv(gzfile(files[1]))
} else {
  pharmaceutical <- do.call(rbind, lapply(1:9, function(i) uspto_search(
    "pharmaceutical.ab.", "US-PGPUB", verbose = TRUE, start = (i - 1) * 1e4, limit = 1e4
  )))
  write.csv(pharmaceutical, xzfile(files[1]), row.names = FALSE)
}
if (file.exists(files[2])) {
  mechanical <- read.csv(gzfile(files[2]))
} else {
  mechanical <- do.call(rbind, lapply(1:9, function(i) uspto_search(
    "F01?.cpcl.", "US-PGPUB", verbose = TRUE, start = (i - 1) * 1e4, limit = 1e4
  )))
  write.csv(mechanical, xzfile(files[2]), row.names = FALSE)
}

# collect examination records
application_numbers <- sort(unique(c(
  pharmaceutical$applicationNumber, mechanical$applicationNumber
)))

n <- length(application_numbers)
filters <- lapply(
  split(sub("/", "", application_numbers, fixed = TRUE), rep(seq(1, ceiling(n / 750)), each = 750)[seq(1, n)]),
  function(set) paste0("applId:(", paste(set, collapse = " "), ")")
)
for (f in filters) {
  download_peds(filters = list(f), outFile = "../searches/examination_records", load = FALSE)
}

# download applications
for (f in list.files("../searches/examination_records", full.names = TRUE)) {
  record <- read_json(f)
  doc_ids <- sub(
    "-?(\\d+)-?", "-\\1-", perl = TRUE,
    vapply(record, function(r) {
      id <- r$patentCaseMetadata$patentPublicationIdentification$publicationNumber
      if (length(id)) id else NA_character_
    }, "")
  )
  doc_ids <- doc_ids[!is.na(doc_ids)]
  if (any(!file.exists(paste0("../searches/applications/", doc_ids, ".json.xz")))) {
    uspto_download(doc_ids, outDir = "../searches/applications", verbose = TRUE, load = FALSE)
  }
}

# unpack records bundles
record_files <- list.files("../searches/examination_records", full.names = TRUE)
for (f in record_files) {
  r <- read_json(f)
  if (!grepl("-", r[[1]]$patentCaseMetadata$patentPublicationIdentification$publicationNumber, fixed = TRUE)) {
    message("writing app records for ", basename(f))
    for (d in r) {
      id <- sub("-?(\\d+)-?", "-\\1-", d$patentCaseMetadata$patentPublicationIdentification$publicationNumber, perl = TRUE)
      d$patentCaseMetadata$patentPublicationIdentification$publicationNumber <- id
      d$patentCaseMetadata$applicationNumberText$value <- sub("^(\\d{2})/?", "\\1/", d$patentCaseMetadata$applicationNumberText$value, perl = TRUE)
      file <- paste0("../searches/examinations/", id, ".json.xz")
      if (!file.exists(file)) {
        con <- xzfile(file)
        write_json(d, con, auto_unbox = TRUE)
        close(con)
      }
    }
  }
}


# gather inventors from records
inventors <- list()
timing <- list()
files <- list.files("../searches/examinations", full.names = TRUE)
ids <- sub(".json.xz", "", basename(files), fixed = TRUE)
for (i in which(!ids %in% names(inventors))) {
  id <- ids[i]
  r <- read_json(files[i])
  inventors[[id]] <- extract_inventors(r)
  timing[[id]] <- extract_event_timing(r)
  cat("\r", length(inventors))
}
inventors <- do.call(rbind, inventors)
write.csv(inventors, xzfile("eda/searches/inventors.csv.xz"), row.names = FALSE)
timings_flat <- list()
for (a in names(timing)) {
  d <- timing[[a]]
  for (n in names(d)) if (!is.null(d[[n]])) d[[n]] <- cbind(application = a, eventType = n, d[[n]])
  res <- do.call(rbind, d)
  rownames(res) <- NULL
  timings_flat[[a]] <- res
}
timing <- do.call(rbind, timings_flat)
write.csv(timing, bzfile("eda/searches/timing.csv.bz2"), row.names = FALSE)

# predict demographics
if (!exists("inventors")) inventors <- read.csv(gzfile("eda/searches/inventors.csv.xz"))
if (!exists("timing")) timing <- read.csv(gzfile("eda/searches/timing.csv.bz2"))

## remotes::install_github("miserman/lusilab")
library(lusilab)
library(GenderInfer)
library(gender)
library(rethnicity)

inventors[inventors$inventorCountry == "" & inventors$inventorState != "", "inventorCountry"] <- "US"
inventors$fullname <- sub(
  "\\s{2,}", " ", do.call(paste, inventors[, c("firstName", "middleName", "lastName")]), perl = TRUE
)

# sex inventors

## Social Security US
us_names <- get_baby_names("../names")
inventors$sex_us_fem_prob <- .5
rownames(us_names$summary) <- tolower(rownames(us_names$summary))
su <- tolower(inventors$firstName) %in% rownames(us_names$summary)
inventors[su, "sex_us_fem_prob"] <- us_names$summary[tolower(inventors[su, "firstName"]), 2]

## Social Security US/UK combined
inventors$id <- paste0("i", seq_len(nrow(inventors)))
rownames(inventors) <- inventors$id
usuk <- GenderInfer::assign_gender(inventors, "firstName")
usuk <- usuk[!duplicated(usuk$id),]
inventors$sex_usuk <- "U"
inventors[usuk$id, "sex_usuk"] <- usuk$gender
