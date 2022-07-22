skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading data")

test_that("example works", {
  outFile <- paste0(tempdir(), "/oa.json")
  unlink(outFile)
  oa <- download_office_actions(outFile = outFile, rows = 1, verbose = FALSE)
  expect_true(file.exists(outFile))
  expect_identical(oa, jsonlite::read_json(outFile))
})
