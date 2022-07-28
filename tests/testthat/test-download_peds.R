skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading data")

test_that("example works", {
  outFile <- paste0(tempdir(), "/peds.json")
  unlink(outFile)
  download_peds("applId:13877637", outFile = outFile, verbose = FALSE)
  expect_true(file.exists(outFile))
  expect_identical(jsonlite::read_json(outFile)[[1]]$patentCaseMetadata$applicationNumberText$value, "13877637")
})
