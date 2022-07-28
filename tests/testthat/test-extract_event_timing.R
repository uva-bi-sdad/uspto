test_that("example works", {
  expect_identical(extract_event_timing(data.frame(
    date = c("2020-01-15", "2020-01-20"),
    event = c("FWDX", "CTNF")
  ))[[3]], data.frame(
    start = 2, startDate = as.Date("2020-01-15"), startEvent = "FWDX",
    end = 1, endDate = as.Date("2020-01-20"), endEvent = "CTNF",
    days = 5
  ))
})
