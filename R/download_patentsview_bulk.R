#' Download PatentsView Bulk
#'
#' Download one of the bulk tables from \href{https://patentsview.org}{PatentsView}, and optionally
#' make an Arrow dataset from it.
#'
#' @param table Name of the table to download, as listed on the
#' \href{https://patentsview.org/download/data-download-tables}{Data Downloads page}.
#' @param dir Directory in which to save original tables and the dataset if \code{make_db} is \code{TRUE}.
#' @param pregrant Logical; if \code{TRUE}, will download the pre-grant version of \code{table}.
#' @param partition A vector of column names to be used as partition keys, if \code{make_db} is \code{TRUE}.
#' Can be a named list of functions, which will be used to create new columns, then partition by those columns
#' (e.g., \code{list(series_code = function(d) substr(d$patent_id, 1, 2))}). If \code{NULL} not specified, will choose
#' a column with the number of unique values closest to 20.
#' @param make_db Logical; if \code{TRUE}, will make an Arrow dataset out of the downloaded table.
#' @param format Format of the dataset, if \code{make_db} is \code{TRUE}.
#' @param return_table Logical; if \code{FALSE}, returns the path to the file, rather than the read-in table.
#' @param overwrite Logical; if \code{TRUE}, overwrites any existing files (raw and prepared).
#' @return The original table (if \code{return_table} is \code{TRUE}; as a \code{tibble}), an opened dataset
#' (if the path to the dataset exists), or the path to the downloaded file.
#' @examples
#' \dontrun{
#' # download the application table of granted patents
#' download_patentsview_bulk(".", "application", partition = "series_code")
#' }
#' @export

download_patentsview_bulk <- function(table, dir = tempdir(), pregrant = FALSE, partition = NULL, make_db = FALSE,
                                      format = "parquet", return_table = TRUE, overwrite = FALSE) {
  url <- paste0(
    "https://s3.amazonaws.com/data.patentsview.org/",
    if (pregrant) "pregrant_publications" else "download",
    "/", table, ".tsv.zip"
  )
  dir.create(dir, FALSE, TRUE)
  dir <- paste0(normalizePath(dir, "/"), "/")
  file_zip <- paste0(dir, table, ".tsv.zip")
  file <- paste0(dir, table, if (pregrant) "_pg" else "_g", ".tsv")
  dbd <- paste0(dir, table, "_db")
  if (overwrite) unlink(c(file_zip, file, dbd), TRUE, TRUE)
  if (!file.exists(file)) {
    if (!file.exists(file_zip)) download.file(url, file_zip)
    unzip(file_zip, exdir = dir)
    file.rename(sub(".zip", "", file_zip, fixed = TRUE), file)
    unlink(file_zip)
  }
  raw <- NULL
  if (missing(make_db) && (!missing(partition) || !missing(format))) make_db <- TRUE
  if (missing(return_table) && make_db) return_table <- FALSE
  if (make_db) {
    raw <- arrow::read_delim_arrow(file, delim = "\t", escape_backslash = TRUE)
    if (is.null(partition)) {
      partition <- names(which.min(abs(20 - vapply(raw, function(x) length(unique(x)), 0))))
    } else if (is.list(partition)) {
      for (n in names(partition)) {
        raw[[n]] <- partition[[n]](raw)
      }
      partition <- names(partition)
    }
    arrow::write_dataset(raw, dbd, partitioning = partition, format = format)
  }
  if (!return_table && dir.exists(dbd)) {
    arrow::open_dataset(dbd, format = format)
  } else {
    if (return_table) {
      if (is.null(raw)) {
        arrow::read_delim_arrow(file, delim = "\t", escape_backslash = TRUE)
      } else {
        raw
      }
    } else {
      file
    }
  }
}
