skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading data")
outFile <- paste0(tempdir(), "/patents_latest.csv")

test_that("default works", {
  unlink(outFile)
  patents <- download_patents(outFile = outFile, limit = 5)
  expect_true(file.exists(outFile))
  expect_identical(patents$patentApplicationNumber, read.csv(outFile)$patentApplicationNumber)
})

test_that("number set works", {
  existing <- download_patents(outFile = outFile)
  patents <- download_patents(existing$patentApplicationNumber)
  expect_identical(patents$patentApplicationNumber, existing$patentApplicationNumber)
})
