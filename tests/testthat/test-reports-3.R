test_that("reportPriceFoodIndex works", {
  expectValidReport(expectReportSucceeds(reportPriceFoodIndex, baseyear = "y2010"))
  expectValidReport(expectReportSucceeds(reportPriceFoodIndex, baseyear = "y2020"))
})


test_that("reportProducerPriceIndex works", {
  expectValidReport(expectReportSucceeds(reportProducerPriceIndex))
})


test_that("reportExpenditureFoodIndex works", {
  expectValidReport(expectReportSucceeds(reportExpenditureFoodIndex))
})


test_that("reportPriceAgriculture works", {
  expectValidReport(expectReportSucceeds(reportPriceAgriculture))
})


test_that("reportPriceBioenergy works", {
  expectValidReport(expectReportSucceeds(reportPriceBioenergy))
})


test_that("reportPriceLand works", {
  expectValidReport(expectReportSucceeds(reportPriceLand))
})


test_that("reportPriceWater works", {
  expectValidReport(expectReportSucceeds(reportPriceWater))
})


test_that("reportValueTrade works", {
  expectValidReport(expectReportSucceeds(reportValueTrade))
})


test_that("reportProcessing works", {
  expectValidReport(expectReportSucceeds(reportProcessing, indicator = "primary_to_process"))
  expectValidReport(expectReportSucceeds(reportProcessing, indicator = "secondary_from_primary"))

})


test_that("reportAEI works", {
  expectValidReport(expectReportSucceeds(reportAEI))
})


test_that("reportWaterUsage works", {
  expectValidReport(expectReportSucceeds(reportWaterUsage, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportWaterUsage, detail = TRUE))
})


test_that("reportWaterAvailability works", {
  expectValidReport(expectReportSucceeds(reportWaterAvailability))
})


test_that("reportAAI works", {
  expectValidReport(expectReportSucceeds(reportAAI))
})


test_that("reportSOM works", {
  expectValidReport(expectReportSucceeds(reportSOM))
})


test_that("reportGrowingStock works", {
  expectValidReport(expectReportSucceeds(reportGrowingStock, indicator = "relative"))
  expectValidReport(expectReportSucceeds(reportGrowingStock, indicator = "absolute"))
})



test_that("reportSDG1 works", {
  expectValidReport(expectReportSucceeds(reportSDG1))
})


test_that("reportSDG2 works", {
  expectValidReport(expectReportSucceeds(reportSDG2))
})


test_that("reportSDG3 works", {
  expectValidReport(expectReportSucceeds(reportSDG3))
})


test_that("reportSDG6 works", {
  expectValidReport(expectReportSucceeds(reportSDG6))
})


test_that("reportSDG12 works", {
  expectValidReport(expectReportSucceeds(reportSDG12))
})


test_that("reportSDG15 works", {
  expectValidReport(expectReportSucceeds(reportSDG15))
})


test_that("reportPBwater works", {
  expectValidReport(expectReportSucceeds(reportPBwater, level = "regglo"))
})


test_that("reportPBland works", {
  skip("Requires additional files, infrastructure needs to be extended to support this")
  expectValidReport(expectReportSucceeds(reportPBland, level = "regglo"))
})


test_that("reportPBbiosphere works", {
  skip("Requires additional files, infrastructure needs to be extended to support this")
  expectValidReport(expectReportSucceeds(reportPBbiosphere, level = "regglo"))
})


test_that("reportPBnitrogen works", {
  skip("Requires additional files, infrastructure needs to be extended to support this")
  expectValidReport(expectReportSucceeds(reportPBnitrogen, level = "regglo"))
})
