#' Search PatentsView
#'
#' Submits a query to one of the \href{https://patentsview.org}{PatentsView} endpoints.
#'
#' @param query A query string or list; for example \code{list(patent_number = 7861317)} or
#' \code{'{"_text_all": {"patent_abstract": ["photographic", "noodle"]}}'}. See
#' \href{https://patentsview.org/apis/api-query-language}{API Query Language}.
#' @param fields A vector of fields to return; defaults to all for the given endpoint. Set to \code{NULL},
#' to use the API's default.
#' @param sort A list or list of lists specifying how to sort results (e.g., \code{list(patent_date = "asc")}).
#' @param outDir Directory in which to save results; defaults to a temporary directory.
#' @param include_related Logical; if \code{TRUE}, includes subentities related to entities matching the search criteria.
#' @param start Page to start collecting results on.
#' @param limit Maximum number of results to return; sets page size, up to 10,000 per page.
#' @param endpoint Name of the API endpoint to search in, as listed on
#' \href{https://patentsview.org/apis/api-endpoints}{PatentsView}.
#' @param post Logical; if \code{FALSE}, will make a GET rather than POST request.
#' @param retry Maximum number of times to retry a request, in the event of a rate limit.
#' @param verbose Logical; if \code{FALSE}, does not print status messages.
#' @param legacy Logical; if \code{FALSE}, will use the beta API, which requires a key.
#' @param key Beta API key (\href{https://patentsview.org/apis/keyrequest}{Request a key});
#' defaults to the \code{PATENTSVIEW_KEY} environment variable.
#' @param cores Number of CPU cores to use when retrieving multiple pages of results.
#' @param overwrite Logical; if \code{TRUE}, overwrites an existing result.
#' @return A \code{data.frame} of results, if any were found. This may contain list entries
#' of \code{data.frame} with varying dimensions, depending on the \code{endpoint} and \code{fields}.
#' @examples
#' \dontrun{
#' # search for patents with a meteorology classification:
#' # https://www.uspto.gov/web/patents/classification/cpc/html/cpc-G01W.html
#' results <- uspto_search_patentsview(list(cpc_group_id = "G01W"))
#'
#' # search by date and keyword:
#' results <- uspto_search_patentsview(list(
#'   "_and" = list(
#'     list(patent_date = "2002-01-08"),
#'     list("_text_any" = list(patent_abstract = "motorcycle"))
#'   )
#' ))
#' }
#' @export

uspto_search_patentsview <- function(query, fields = NULL, sort = NULL, outDir = tempdir(), include_related = FALSE, start = 1,
                                     limit = 1e4, endpoint = "patent", post = TRUE, retry = 10, verbose = FALSE, legacy = TRUE,
                                     key = Sys.getenv("PATENTSVIEW_KEY"), cores = detectCores() - 1, overwrite = FALSE) {
  if (key == "") legacy <- TRUE
  if (legacy) {
    page_limit <- 1e4
    if (is.null(fields)) {
      all_fields <- GET(paste0(
        "https://raw.githubusercontent.com/PatentsView/PatentsView-API/master/querymodule/tests/testspec/",
        endpoint, "_master_field_list.json"
      ))
      if (all_fields$status_code != 200) stop(endpoint, " is not a recognized endpoint", call. = FALSE)
      all_fields <- fromJSON(rawToChar(all_fields$content))$fields
    }
    if (!grepl("s$", endpoint)) endpoint <- paste0(endpoint, "s")
    url <- paste0("https://api.patentsview.org/", endpoint, "/query")
    per_page <- min(page_limit, limit)
    start <- ceiling(start / per_page)
    options <- list(page = start, per_page = per_page, matched_subentities_only = include_related)
  } else {
    page_limit <- 1e3
    url <- paste0("https://search.patentsview.org/api/v1/", endpoint, "/")
    all_fields <- switch(endpoint,
      patent = c(
        "patent_number", "patent_title", "patent_date", "patent_type", "patent_country", "patent_year",
        "patent_abstract", "patent_kind", "patent_num_foreign_documents_cited", "patent_num_us_applications_cited",
        "patent_us_patents_cited", "patent_num_total_documents_cited", "patent_num_times_cited_by_us_patents",
        "patent_earliest_application_date", "patent_patent_processing_days", "patent_uspc_current_mainclass_average_patent_processing_days",
        "patent_cpc_current_group_average_patent_processing_days", "patent_term_extension", "patent_detail_desc_length",
        "assignees_at_grant", "inventors_at_grant", "cpc_current"
      ),
      inventor = c(
        "inventor_id", "name_first", "name_last", "lastknown_city", "lastknown_state", "lastknown_country",
        "first_seen_date", "last_seen_date", "num_assignees", "num_patents", "years_active", "inventor_years"
      ),
      assignee = c(
        "assignee_id", "first_seen_date", "last_seen_date", "lastknown_city", "lastknown_country", "lastknown_latitude",
        "lastknown_longitude", "lastknown_state", "name_first", "name_last", "num_inventors", "num_patents", "organization",
        "type", "years_active", "assignee_years"
      ),
      cpc_subsection = c(
        "cpc_section_id", "cpc_subsection_id", "cpc_subsection_title", "cpc_subsection_num_patents", "cpc_subsection_num_assignees",
        "cpc_subsection_num_inventors", "cpc_subsection_first_seen_date", "cpc_subsection_last_seen_date", "cpc_subsection_years_active"
      ),
      cpc_group = c(
        "cpc_group_id", "cpc_group_title", "cpc_group_num_patents", "cpc_group_num_assignees", "cpc_group_num_inventors",
        "cpc_group_first_seen_date", "cpc_group_last_seen_date", "cpc_group_years_active"
      ),
      cpc_subgroup = c("cpc_subgroup_id", "cpc_subgroup_title"),
      uspc_mainclass = c(
        "uspc_mainclass_id", "uspc_mainclass_title", "uspc_mainclass_num_patents", "uspc_mainclass_num_assignees",
        "uspc_mainclass_num_inventors", "uspc_mainclass_first_seen_date", "uspc_mainclass_last_seen_date", "uspc_mainclass_years_active"
      ),
      uspc_subclass = c("uspc_subclass_id", "uspc_subclass_title"),
      nber_category = c("nber_category_id", "nber_category_title"),
      nber_subcategory = c(
        "nber_subcategory_id", "nber_subcategory_title", "nber_subcategory_num_patents",
        "nber_subcategory_num_assignees", "nber_subcategory_num_inventors", "nber_subcategory_first_seen_date",
        "nber_subcategory_last_seen_date", "nber_subcategory_years_active"
      ),
      patent_citation = c("patent_number", "cited_patent_number", "citation_category", "citation_sequence", "citation_date"),
      application_citation = c(
        "patent_number", "cited_application_number", "citation_category", "citation_kind", "citation_sequence", "citation_date"
      ),
      NULL
    )
    if (is.null(all_fields)) stop(endpoint, " is not a recognized endpoint", call. = FALSE)
    per_page <- min(page_limit, limit)
    start <- ceiling(start / per_page)
    options <- list(size = per_page, matched_subentities_only = include_related)
  }
  if (is.character(query)) query <- jsonlite::fromJSON(query)
  fs <- if (is.null(fields)) all_fields else fields
  rf <- if (post) {
    function(page, attempt = 1) {
      if (is.null(options$page)) {
        options$offset <- page * 1e3
      } else {
        options$page <- page + 1
      }
      body <- list(
        q = query,
        f = fs,
        s = sort,
        o = options
      )
      req <- httr::POST(
        url,
        httr::add_headers("X-Api-Key" = key, "Content-Type" = "application/json", Accept = "application/json"),
        body = body, encode = "json"
      )
      if (req$status_code == 429 && attempt < retry) {
        wait <- as.integer(req$headers[["retry-after"]])
        if (length(wait)) Sys.sleep(wait)
        return(rf(page, attempt + 1))
      }
      if (req$status_code == 200) {
        jsonlite::fromJSON(rawToChar(req$content))
      } else {
        req
      }
    }
  } else {
    function(page, attempt = 1) {
      if (is.null(options$page)) {
        options$offset <- page * 1e3
      } else {
        options$page <- page + 1
      }
      body <- paste0(
        "?q=", jsonlite::toJSON(query, auto_unbox = TRUE),
        paste0('&f=["', paste(fs, collapse = '","'), '"]'),
        if (length(sort)) paste0("&s=", jsonlite::toJSON(sort, auto_unbox = TRUE)),
        paste0("&o=", jsonlite::toJSON(options, auto_unbox = TRUE))
      )
      req <- httr::GET(
        URLencode(paste0(url, body)),
        httr::add_headers("X-Api-Key" = key, "Content-Type" = "application/json", Accept = "application/json")
      )
      if (req$status_code == 429 && attempt < retry) {
        wait <- as.integer(req$headers[["retry-after"]])
        if (length(wait)) Sys.sleep(wait)
        return(rf(page, attempt + 1))
      }
      if (req$status_code == 200) {
        jsonlite::fromJSON(rawToChar(req$content))
      } else {
        req
      }
    }
  }
  dir.create(outDir, FALSE, TRUE)
  cache <- paste0(normalizePath(outDir, "/"), "/", digest::digest(list(url, query, fs, sort, options)), ".json")
  if (!overwrite && file.exists(cache)) {
    if (verbose) message("reading existing results")
    return(jsonlite::read_json(cache, simplifyVector = TRUE))
  }
  res <- rf(0)
  if (!is.null(res$status_code)) {
    stop(
      "request failed: ", res$status_code, "; ", if (res$status_code == 400) {
        paste0(
          res$headers$`x-status-reason`,
          if (!is.null(res$headers$`x-status-reason-code`)) paste0(" (", res$headers$`x-status-reason-code`, ")")
        )
      } else {
        jsonlite::toJSON(jsonlite::fromJSON(rawToChar(res$content)), auto_unbox = TRUE, pretty = TRUE)
      },
      call. = FALSE
    )
  }
  if (!is.null(res$error) && res$error) {
    stop(
      "request failed: ", jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE),
      call. = FALSE
    )
  }
  total_name <- grep("total_", names(res), fixed = TRUE, value = TRUE)
  rn <- names(res)[!names(res) %in% c(total_name, "count", "total_hits", "error")]
  total <- res[[total_name]]
  if (is.null(total) || total == 0) stop("no results found", call. = FALSE)
  limit <- min(limit, total) - res$count
  if (limit > 0) {
    pages <- ceiling(limit / page_limit)
    cores <- min(cores, pages)
    res2 <- if (cores > 1) {
      env <- new.env(parent = globalenv())
      environment(rf) <- env
      cl <- makeCluster(cores)
      on.exit(stopCluster(cl))
      for (o in c("cl", "start", "pages", "key", "rf", "url", "query", "fs", "sort", "options", "retry")) env[[o]] <- get(o)
      clusterExport(cl, ls(envir = env), environment())
      eval(expression(parallel::parLapply(cl, seq(start, pages), rf)), envir = env)
    } else {
      lapply(seq(start, pages), rf)
    }
    res[[rn]] <- do.call(rbind, c(list(res[[rn]]), lapply(res2, "[[", rn)))
  }
  if (verbose) message("found ", total, ", returning ", nrow(res[[rn]]))
  jsonlite::write_json(res[[rn]], cache, auto_unbox = TRUE)
  res[[rn]]
}
