skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading search results")
dir <- paste0(tempdir(), "/patentsview")
unlink(dir, TRUE)
dir.create(dir, FALSE)

test_that("example works", {
  res <- uspto_search_patentsview(list(
    "_and" = list(
      list(patent_date = "2002-01-08"), list("_text_any" = list(patent_abstract = "motorcycle"))
    )
  ), outDir = dir)
  expect_identical(res$patent_number, c("6336327", "6336328", "6336579"))
  file <- list.files(dir, full.names = TRUE)
  expect_identical(
    jsonlite::read_json(file, simplifyVector = TRUE)$patent_number,
    c("6336327", "6336328", "6336579")
  )
})

test_that("load existing works", {
  expect_identical(
    capture.output(res <- uspto_search_patentsview(list(
      "_and" = list(
        list(patent_date = "2002-01-08"), list("_text_any" = list(patent_abstract = "motorcycle"))
      )
    ), outDir = dir, verbose = TRUE), type = "message"),
    "reading existing results"
  )
})

test_that("GET works", {
  res <- uspto_search_patentsview(list(patent_number = 9260215), endpoint = "inventor", outDir = dir)
  expect_identical(
    uspto_search_patentsview(list(patent_number = 9260215), endpoint = "inventor", post = FALSE),
    res
  )
})

skip_if(Sys.getenv("PATENTSVIEW_KEY") == "", "no API key")

test_that("works with beta API", {
  res <- uspto_search_patentsview(list(patent_number = 9260215), endpoint = "patent", legacy = FALSE)
  expect_identical(res$patent_number, "9260215")
  res <- uspto_search_patentsview(list(inventor_id = 12320), endpoint = "inventor", legacy = FALSE, post = FALSE)
  expect_identical(res$inventor_id, 12320L)
  res <- uspto_search_patentsview(list(assignee_id = 2), endpoint = "assignee", legacy = FALSE)
  expect_identical(res$assignee_id, 2L)
  res <- uspto_search_patentsview(list(cpc_subsection_id = "A01"), endpoint = "cpc_subsection", legacy = FALSE)
  expect_identical(res$cpc_subsection_id, "A01")
  res <- uspto_search_patentsview(list(cpc_group_id = "A01B"), endpoint = "cpc_group", legacy = FALSE)
  expect_identical(res$cpc_group_id, "A01B")
  res <- uspto_search_patentsview(list(cpc_subgroup_id = "A01B1/00"), endpoint = "cpc_subgroup", legacy = FALSE)
  expect_identical(res$cpc_subgroup_id, "A01B1/00")
  res <- uspto_search_patentsview(list(uspc_mainclass_id = 100), endpoint = "uspc_mainclass", legacy = FALSE)
  expect_identical(res$uspc_mainclass_id, "100")
  res <- uspto_search_patentsview(list(uspc_subclass_id = "100/1"), endpoint = "uspc_subclass", legacy = FALSE)
  expect_identical(res$uspc_subclass_id, "100/1")
  res <- uspto_search_patentsview(list(nber_category_id = 1), endpoint = "nber_category", legacy = FALSE)
  expect_identical(res$nber_category_id, "1")
  res <- uspto_search_patentsview(list(nber_subcategory_id = 11), endpoint = "nber_subcategory", legacy = FALSE)
  expect_identical(res$nber_subcategory_id, "11")
  res <- uspto_search_patentsview(list(patent_number = 10966293), endpoint = "patent_citation", legacy = FALSE)
  expect_true(nrow(res) == 78 && all(res$patent_number == 10966293))
  res <- uspto_search_patentsview(list(patent_number = 10966293), endpoint = "application_citation", legacy = FALSE)
  expect_true(nrow(res) == 43 && all(res$patent_number == 10966293))
})
