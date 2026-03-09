test_that("magpieReportAsQuitte can convert a report", {
  magclassReport <- superAggregateX(magclass::maxample("pop"), level = "regglo", aggr_type = "sum")

  quitteReport <- magpieReportAsQuitte(magclassReport)
  expect_true("quitte" %in% class(quitteReport))
  expect_true("World" %in% quitteReport$region)
})