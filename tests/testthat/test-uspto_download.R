skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading documents")
dir <- tempdir()
unlink(dir)
dir.create(dir, FALSE, TRUE)
test_apps <- data.frame(
  guid = c("US-11391845-B2", "US-11393320-B2", "US-11391865-B2", "US-11385648-B2", "US-11385381-B2"),
  type = "USPAT"
)

test_that("main input works", {
  suppressMessages(expect_message(
    uspto_download(test_apps, outDir = dir, verbose = TRUE),
    paste0("retrieving ", nrow(test_apps), " documents; saving in ", paste0(normalizePath(dir, "/"), "/"))
  ))
  expect_true(all(file.exists(paste0(dir, "/", test_apps$guid, ".json"))))
})

test_that("alternate input", {
  documents <- uspto_download(test_apps$guid, outDir = dir, type = test_apps[1, "type"])
  expect_identical(test_apps$guid, documents$guid)
})
