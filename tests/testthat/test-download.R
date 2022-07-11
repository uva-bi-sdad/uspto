skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading data")

test_that("download office actions works", {
  outFile <- paste0(tempdir(), "/oa.json")
  unlink(outFile)
  oa <- download_office_actions(outFile = outFile, rows = 1, verbose = FALSE)
  expect_true(file.exists(outFile))
  expect_identical(oa, jsonlite::read_json(outFile))
})

test_that("download peds works", {
  outFile <- paste0(tempdir(), "/peds.json")
  unlink(outFile)
  download_peds("applId:13877637", outFile = outFile, verbose = FALSE)
  expect_true(file.exists(outFile))
  expect_identical(jsonlite::read_json(outFile)[["2013"]][[1]]$patentCaseMetadata$applicationNumberText$value, "13877637")
})
