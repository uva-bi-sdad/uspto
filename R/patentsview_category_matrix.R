#' Make Document-Class Matrix
#'
#' Create A document-class matrix from a patentsview bulk table.
#'
#' @param table A matrix-like object with columns specified by \code{class_id}, \code{doc_id}, and \code{rank}.
#' Alternatively, the path to a tab-separated file containing such a matrix, an \code{Arrow} dataset, or the name
#' of a table, to be passed to \code{\link{download_patentsview_bulk}}.
#' @param outFile Path to an rds file to save the results to.
#' @param class_id,doc_id,rank Column names of the class, document ID, and class rank to be pulled from \code{table}.
#' @param type Table name, used to set a default \code{class_id}.
#' @param ... Additional arguments to be passed to \code{\link{download_patentsview_bulk}},
#' if \code{table} is a table name.
#' @param sparse Logical; if \code{FALSE}, returns a regular, dense matrix.
#' @param overwrite Logical; if \code{TRUE}, overwrites an existing \code{outFile} rather than loading it.
#' @return A sparse matrix (or regular matrix if \code{sparse} is \code{FALSE}) with documents
#' in rows, classes in columns, and the class rank (sequence) as values.
#' @examples
#' table <- data.frame(
#'   patent_id = c("a", "a", "b"),
#'   class = c(1, 3, 2),
#'   sequence = c(1, 0, 0)
#' )
#' patentsview_class_matrix(table, class_id = "class")
#'
#' \dontrun{
#'
#' # get a matrix of WIPO class assignments
#' wipo_fields <- patentsview_class_matrix("wipo")
#'
#' # get a subset without creating the full matrix
#' wipo <- download_patentsview_bulk("wipo", make_db = TRUE)
#' wipo_fields_sub <- patentsview_class_matrix(dplyr::compute(dplyr::filter(
#'   wipo, patent_id %in% c("10000002", "10000015", "10000017")
#' )))
#' }
#' @export

patentsview_class_matrix <- function(table, outFile = NULL, class_id = NULL, doc_id = "patent_id", rank = "sequence",
                                     type = NULL, ..., sparse = TRUE, overwrite = FALSE) {
  m <- NULL
  if (!is.null(outFile)) {
    outFile <- sub("\\.[A-Za-z]{3}.*", ".rds", outFile)
    if (!overwrite && file.exists(outFile)) {
      m <- readRDS(outFile)
    }
  }
  if (is.null(m)) {
    if (is.character(table) && length(table) == 1) {
      type <- if (is.null(type)) basename(table) else type
      table <- if (file.exists(table)) {
        arrow::read_delim_arrow(table, delim = "\t", escape_backslash = TRUE)
      } else {
        download_patentsview_bulk(table, ..., make_db = TRUE, return_table = FALSE)
      }
    }
    if (!is.null(type)) {
      type <- substr(type, 1, 3)
      if (is.null(class_id)) {
        class_id <- switch(type,
          wip = "field_id",
          cpc = "group_id",
          ipc = "section",
          nbe = "subcategory_id",
          usp = "mainclass_id"
        )
      }
    } else if (is.null(class_id)) {
      class_id <- c("field_id", "group_id", "section", "subcategory_id", "mainclass_id")
      class_id <- class_id[class_id %in% names(table)]
      if (length(class_id) > 1) class_id <- class_id[[1]]
    }
    if (!length(class_id)) stop("specify a class_id or type from which it can be inferred", call. = FALSE)
    cols <- c(class_id, doc_id)
    if (!all(cols %in% names(table))) {
      stop("unrecognized column reference: ", paste(cols[!cols %in% names(table)], collapse = ", "))
    }
    if (!is.data.frame(table) && (!is.null(table$NewScan) || !is.null(table$GetColumnByName))) {
      if (!is.null(table$NewScan)) table <- table$NewScan()$Filter(arrow::Expression$scalar(TRUE))$Finish()$ToTable()
      classes <- sort(unique(table$GetColumnByName(class_id)))$as_vector()
      docs <- sort(unique(table$GetColumnByName(doc_id)))$as_vector()
      table <- as.data.frame(table)
    } else {
      if (inherits(table, "arrow_dplyr_query")) {
        stop(
          "enter a completed query (e.g., using dplyr::compute), or a data.frame",
          call. = FALSE
        )
      }
      classes <- sort(unique(table[[class_id]]))
      docs <- sort(unique(table[[doc_id]]))
    }
    m <- Matrix::sparseMatrix(
      as.integer(factor(table[[doc_id]], docs)), as.integer(factor(table[[class_id]], classes)),
      x = if (rank %in% names(table)) table[[rank]] + 1 else 1,
      dims = c(length(docs), length(classes)), dimnames = list(docs, classes)
    )
    if (!is.null(outFile)) saveRDS(m, outFile, compress = "xz")
  }
  if (sparse) m else as.matrix(m)
}
