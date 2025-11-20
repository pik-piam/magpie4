test_that("superAggregateX results in the same aggregated data as superAggregate", {
  expectEqualAggregations <- function(m, op, level) {
    weights <- m
    weights[, , ] <- 1
    saResult <- superAggregate(m, op, level = "glo", weight = weights)
    saxResult <- superAggregateX(m, op, level = "glo", weight = weights)

    # We can not use the magpie objects directly, as they
    # differ in the names of the dimensions and meta data
    expect_equal(!!as.vector(saResult), !!as.vector(saxResult))
  }

  p <- magclass::maxample("pop")
  psmall <- p / 1000000
  for (dataset in list(p, psmall)) {
    for (level in c("glo", "reg", "regglo")) {
      for (operation in c("sum", "mean", "weighted_mean")) {
        expectEqualAggregations(dataset, operation, level)
      }
    }
  }
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

test_that("superAggregateX takes care of aggregating to regional before applying the mapping", {
  withr::local_dir(withr::local_tempdir())

  tempMapping <- "RegionCode;NewRegionCode
AFR;REG1
CPA;REG2
EUR;REG1
FSU;REG2"
  writeLines(tempMapping, "mymapping.csv")

  p <- new.magpie(c("AFR.CO1", "CPA.CO2", "EUR.CO3", "FSU.CO4"), "", "value", c(1, 2, 3, 4))

  saxResult <- superAggregateX(p, "sum", level = "mymapping.csv")
  expect_equal(getItems(saxResult, 1), c("REG1", "REG2"))
})

test_that("superAggregateX throws an error if no mapping was found", {
  p <- magclass::maxample("pop")
  expect_error(force(superAggregateX(p, "sum", level = "mymapping.csv")),
               "mymapping.csv is neither a valid level nor can a mapping with that name be found.")
})
