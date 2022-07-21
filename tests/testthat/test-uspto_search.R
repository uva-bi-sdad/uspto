skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading search results")
file <- paste0(tempdir(), "/results.csv")

test_that("example works", {
  unlink(file)
  res <- uspto_search("20020801.pd. AND motorcycle", outFile = file)
  expect_true(!is.null(res) && file.exists(file))
  res$publicationReferenceDocumentNumber <- as.numeric(res$publicationReferenceDocumentNumber)
  res$pageCountDisplay <- as.integer(res$pageCountDisplay)
  res$familyCount <- as.integer(res$familyCount)
  expect_identical(res, read.csv(file))
})

test_that("load existing works", {
  expect_identical(read.csv(file), uspto_search("20020801.pd. AND motorcycle", outFile = file))
})
