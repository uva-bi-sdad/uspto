skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading input")

test_that("example works", {
  application <- uspto_download("US-20040216465-A1")
  ref <- extract_inventors(application)
  expect_true(nrow(ref) == 2)
  expect_identical(ref, extract_inventors(download_peds("applId:10612573", verbose = FALSE)))
})
