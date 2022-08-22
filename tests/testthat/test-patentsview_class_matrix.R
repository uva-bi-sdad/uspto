test_that("basic example works", {
  table <- data.frame(
    patent_id = c("a", "a", "b"),
    class = c(1, 3, 2),
    sequence = c(1, 0, 0)
  )
  expect_true(all(patentsview_class_matrix(table, class_id = "class") == c(2, 0, 0, 1, 1, 0)))
})

skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading data")

test_that("alternative inputs work", {
  dir <- tempdir()
  dcm <- patentsview_class_matrix("wipo", dir = dir)
  table <- download_patentsview_bulk("wipo", dir = dir, return_table = FALSE)
  filtered <- dplyr::compute(dplyr::filter(table, patent_id %in% c("10000002", "10000015", "10000017")))
  dcm <- patentsview_class_matrix(filtered, type = "wipo", dir = dir)
  expect_identical(dim(dcm), c(3L, 6L))
  file <- tempfile()
  dcmm <- patentsview_class_matrix(as.data.frame(filtered), type = "wipo", outFile = file)
  expect_identical(dcmm, dcm)
  expect_identical(patentsview_class_matrix(filtered), dcm)
  expect_identical(patentsview_class_matrix(outFile = file, sparse = FALSE), as.matrix(dcmm))
})
