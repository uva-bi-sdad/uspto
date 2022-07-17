skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading search results")

test_that("application search works", {
  dir <- paste0(tempdir(), "/ft_search/")
  unlink(list.files(dir, full.names = TRUE))
  res <- search_fulltext("ttl/syringe andnot (sew or thread$)", outdir = dir, limit = 100, verbose = FALSE)
  res$id <- as.numeric(res$id)
  outFile <- list.files(dir, full.names = TRUE)
  expect_true(length(outFile) > 0)
  expect_identical(read.csv(outFile[[1]]), res)
})

test_that("patent search works", {
  dir <- paste0(tempdir(), "/ft_search/")
  unlink(list.files(dir, full.names = TRUE))
  res <- search_fulltext("isd/1/8/2002 and motorcycle", FALSE, outdir = dir, verbose = FALSE)
  outFile <- list.files(dir, full.names = TRUE)
  expect_true(length(outFile) > 0)
  expect_identical(read.csv(outFile[[1]]), res)
})
