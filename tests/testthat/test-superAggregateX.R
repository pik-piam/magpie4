test_that("superAggregateX results in the same aggregated data as superAggregate", {
  p <- magclass::maxample("pop")
  saResult <- superAggregate(p, "sum", level = "glo")
  saxResult <- superAggregateX(p, "sum", level = "glo")
  expect_equal(!!as.vector(saResult), !!as.vector(saxResult))
})

test_that("superAggregateX accepts a mapping name as a level", {
  withr::local_dir(withr::local_tempdir())

  tempMapping <- "RegionCode;NewRegionCode
AFR;REG1
CPA;REG2
EUR;REG1
FSU;REG2"
  writeLines(tempMapping, "mymapping.csv")
  p <- magclass::maxample("pop")[c("AFR", "CPA", "EUR", "FSU"), , ]
  saxResult <- superAggregateX(p, "sum", level = "mymapping.csv")
  expect_equal(getItems(saxResult, 1), c("REG1", "REG2"))
})

test_that("superAggregateX accepts a mapping with single regions and aggregated regions that overlap", {
  withr::local_dir(withr::local_tempdir())

  tempMapping <- "RegionCode;NewRegionCode
AFR;AFR
CPA;CPA
EUR;EUR
FSU;FSU
AFR;REG1
CPA;REG2
EUR;REG1
FSU;REG2
AFR;REG3
CPA;REG3"
  writeLines(tempMapping, "mymapping.csv")
  p <- magclass::maxample("pop")[c("AFR", "CPA", "EUR", "FSU"), , ]
  saxResult <- superAggregateX(p, "sum", level = "mymapping.csv")
  expect_equal(getItems(saxResult, 1), c(
    "AFR", "CPA", "EUR", "FSU",
    "REG1", "REG2",
    "REG3"
  ))
})

test_that("superAggregateX throws an error if no mapping was found", {
  p <- magclass::maxample("pop")
  expect_error(force(superAggregateX(p, "sum", level = "mymapping.csv")),
               "mymapping.csv is neither a valid level nor can a mapping with that name be found.")
})