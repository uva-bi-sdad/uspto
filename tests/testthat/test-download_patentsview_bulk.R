skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading data")

test_that("example works", {
  dir <- tempdir()
  file <- download_patentsview_bulk("nber_category", dir, return_table = FALSE)
  expect_true(file.exists(file))
  data <- download_patentsview_bulk("nber_category", dir)
  expect_true(!is.null(nrow(data)))
  db <- download_patentsview_bulk("nber_category", dir, partition = list(part = function(d) rep(1, nrow(d))))
  expect_true(!is.null(db$num_rows))
  data <- download_patentsview_bulk("nber_category", dir, make_db = TRUE, return_table = TRUE)
  expect_true(!is.null(nrow(data)))
  db2 <- download_patentsview_bulk("nber_category", paste0(dir, "/db"), make_db = TRUE)
  expect_true(!is.null(db2$num_rows))
})
