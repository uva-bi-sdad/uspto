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

#
# download data
#

if (!dir.exists("../searches")) {
  dir.create("../searches")
    
  # collect examination records
  application_numbers <- sort(unique(c(
    pharmaceutical$applicationNumber, mechanical$applicationNumber
  )))
  
  # download examination records
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
  library(parallel)
  cl = makeCluster(detectCores() - 1)
  summaries <- parLapplyLB(cl, list.files("../searches/examinations", full.names = TRUE), function(f) {
    library(uspto)
    id <- sub(".json.xz", "", basename(f), fixed = TRUE)
    r <- jsonlite::read_json(f)
    inventors <- extract_inventors(r)
    if (is.null(inventors)) {
      inventors <- data.frame(
        guid = r$patentCaseMetadata$patentPublicationIdentification$publicationNumber,
        applicationNumber = r$patentCaseMetadata$applicationNumberText$value,
        firstName = NA, middleName = NA, lastName = NA, inventorCountry = NA, inventorState = NA,
        inventorCity = NA
      )
    }
    inventors$date <- r$patentCaseMetadata$patentPublicationIdentification$publicationDate
    inventors$art_unit <- if (is.null(r$patentCaseMetadata$groupArtUnitNumber$value)) NA else r$patentCaseMetadata$groupArtUnitNumber$value
    inventors$status <- if (is.null(r$patentCaseMetadata$applicationStatusCategory)) NA else r$patentCaseMetadata$applicationStatusCategory
    inventors$category <- r$patentCaseMetadata$applicationTypeCategory
    inventors$classification <- paste(unlist(
      r$patentCaseMetadata$patentClassificationBag$cpcClassificationBagOrIPCClassificationOrECLAClassificationBag,
      TRUE, FALSE
    ), collapse = " ")
    inventors$business <- r$patentCaseMetadata$businessEntityStatusCategory
    inventors$first_inventor <- if (is.null(r$patentCaseMetadata$firstInventorToFileIndicator)) "false" else r$patentCaseMetadata$firstInventorToFileIndicator
    
    timings <- extract_event_timing(r)
    inventors$time_initial_classificaiton <- if (!is.null(timings$`Initial Classification`)) timings$`Initial Classification`[1, "days"] else NA
    if (!is.null(timings$`Examiner to Action`)) {
      d <- timings$`Examiner to Action`
      inventors$time_first_action <- d[1, "days"]
      inventors$examination_rounds <- nrow(d)
      inventors$any_accepts <- if (nrow(d)) any(!d$endEvent %in% c("CTNF", "CTFR")) else FALSE
    } else {
      inventors$time_first_action <- NA
      inventors$examination_rounds <- 0
      inventors$any_accepts <- FALSE
    }
    inventors
  })
  stopCluster(cl)

  inventors <- do.call(rbind, summaries)
  write.csv(inventors, xzfile("eda/searches/inventors.csv.xz"), row.names = FALSE)
}

# predict demographics
if (!exists("inventors")) inventors <- read.csv(gzfile("eda/searches/inventors.csv.xz"))

## remotes::install_github("miserman/lusilab")
library(lusilab)
library(GenderInfer)
library(gender)
library(rethnicity)
library(DemografixeR)

inventors <- inventors[!is.na(inventors$firstName) & inventors$firstName != "" & !is.na(inventors$lastName),]
inventors$search_set <- c("mechanical", "pharmaceutical")[as.numeric(inventors$guid %in% pharmaceutical$guid) + 1]
inventors[inventors$inventorCountry == "" & inventors$inventorState != "", "inventorCountry"] <- "US"
inventors$fullname <- sub(
  "\\s{2,}", " ", do.call(paste, inventors[, c("firstName", "middleName", "lastName")]), perl = TRUE
)
inventors$id <- paste0("i", seq_len(nrow(inventors)))
rownames(inventors) <- inventors$id

# sex inventors

## Social Security US
inventors[is.na(inventors$inventorCountry), "inventorCountry"] = ""
demo_preds <- predict_demographics(
  tolower(inventors$firstName), tolower(inventors$lastName), inventors$inventorCountry,
  dir = "../names"
)
inventors <- cbind(inventors, demo_preds[, -(1:3)])

## Social Security US/UK combined
usuk <- GenderInfer::assign_gender(inventors, "firstName")
usuk <- usuk[!duplicated(usuk$id),]
inventors$sex_usuk <- "U"
inventors[usuk$id, "sex_usuk"] <- usuk$gender

## U.S. Social Security Administration, U.S. Census Integrated Public Use Microdata Series
## North Atlantic Population Project (1801-1910 censuses)
firstNames <- unique(inventors$firstName)
gender_preds <- lapply(c("ssa", "ipums", "napp"), function(m) {
  res <- gender::gender(firstNames, method = m)
  structure(res$proportion_female, names = res$name)
})
names(gender_preds) <- c("ssa", "ipums", "napp")

inventors[, paste0("prob_fem_", names(gender_preds))] <- .5
for (s in names(gender_preds)) {
  su <- inventors$firstName %in% names(gender_preds[[s]])
  inventors[su, paste0("prob_fem_", s)] <- gender_preds[[s]][inventors[su, "firstName"]]
}

## searching
inventor_query <- gsub("\\s+", " ", do.call(
  paste, inventors[, c("firstName", "middleName", "lastName", "inventorCountry")]
), perl = TRUE)

### adds to the aggregate searches
if (FALSE) {
  unique_queries <- unique(inventor_query)
  add_to_agg <- function(n = 1000, engine = "https://www.google.com/search?q=") {
    agg <- read.csv("../sex_guess_agg.csv")
    set <- unique_queries[!unique_queries %in% agg$name]
    set <- sample(set, n)
    d <- guess_sex(set, search_source = engine, retry = FALSE)
    d <- d[(d$female + d$male) != 0,]
    message("new results: ", nrow(d))
    if (nrow(d)) {
      d$source = sub("^.*\\.(\\w+)\\.com.*$", "\\1", engine, perl = TRUE)
      write.csv(rbind(agg, d), "../sex_guess_agg.csv", row.names = FALSE)
    }
  }
  add_to_agg(2000, "https://www.bing.com/search?q=")
  add_to_agg(2000, "https://www.ask.com/web?q=")
}

agg <- read.csv("../sex_guess_agg.csv")
agg <- agg[!duplicated(agg$name),]
rownames(agg) <- agg$name
agg$prob_fem <- agg$female / (agg$female + agg$male)
su <- inventor_query %in% agg$name
inventors$prob_fem_search <- NA
inventors[su, "prob_fem_search"] <- agg[inventor_query[su], "prob_fem"]

# predict inventor ethnicity

## rethnicity
ethnicity <- rethnicity::predict_ethnicity(
  inventors$firstName, inventors$lastName, threads = parallel::detectCores() - 2
)
probs <- grep("^prob", colnames(ethnicity), value = TRUE)
inventors[, probs] <- ethnicity[, probs]


write.csv(
  inventors[inventors$search_set == "pharmaceutical",],
  xzfile("eda/searches/inventors_pharmaceutical.csv.xz"),
  row.names = FALSE
)
write.csv(
  inventors[inventors$search_set == "mechanical",],
  xzfile("eda/searches/inventors_mechanical.csv.xz"),
  row.names = FALSE
)
