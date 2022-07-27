test_that("works in the usual case", {
  expect_identical(
    c(id = NA, intext = "First M. Last", signed = NA),
    extract_examiner_name("questions can be directed to First M. Last, whose number is")
  )
})

test_that("fallback and list works", {
  expect_identical(
    data.frame(
      id = c("000", "NA1"), intext = c(NA, "First M. Last"), signed = c("First M. Last", NA),
      row.names = c("000", "NA1")
    ),
    extract_examiner_name(list(
      list(
        examinerEmployeeNumber = "000",
        bodyText = "body\nFirst M. Last\nPrimary Examiner\nfoot"
      ),
      "questions can be directed to First M. Last, whose number is"
    ))
  )
})
