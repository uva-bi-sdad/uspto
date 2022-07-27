skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading data")

test_that("example works", {
  outFile <- paste0(tempdir(), "/oa.json.xz")
  unlink(outFile)
  oa <- download_office_actions(outFile = outFile, limit = 1, verbose = FALSE)
  expect_true(file.exists(outFile))
  expect_true(nrow(oa) == 1)
  expect_identical(
    oa$patentApplicationNumber,
    jsonlite::read_json(outFile)[[1]]$patentApplicationNumber
  )
})

test_that("batch works", {
  oa <- download_office_actions(limit = 2000, verbose = FALSE)
  outFile <- paste0(tempdir(), "/", Sys.Date(), ".json.xz")
  expect_true(file.exists(outFile))
  expect_true(nrow(oa) == 2000)
  expect_identical(
    oa$patentApplicationNumber,
    vapply(jsonlite::read_json(outFile), "[[", list(""), "patentApplicationNumber")
  )
})
