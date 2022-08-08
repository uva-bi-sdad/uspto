test_that("example works", {
  dir <- tempdir()
  info <- get_class_info(c(234, 429))
  expect_identical(names(info), c("234", "429"))
  expect_true(info$`429`$class == "429" && info$`429`$description != "")
  expect_identical(
    vapply(info$`234`, class, ""),
    c(class = "character", description = "character", subclasses = "data.frame")
  )
  expect_true(file.exists(paste0(dir, "/uspc234.json")))
  expect_identical(get_class_info(429), info$`429`)
})
