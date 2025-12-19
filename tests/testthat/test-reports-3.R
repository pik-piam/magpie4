test_that("reportPriceFoodIndex works", {
  expectReportSucceeds(reportPriceFoodIndex, baseyear = 'y2010')
  expectReportSucceeds(reportPriceFoodIndex, baseyear = 'y2020')
})


test_that("reportProducerPriceIndex works", {
  expectReportSucceeds(reportProducerPriceIndex)
})


test_that("reportExpenditureFoodIndex works", {
  expectReportSucceeds(reportExpenditureFoodIndex)
})


test_that("reportPriceAgriculture works", {
  expectReportSucceeds(reportPriceAgriculture)
})


test_that("reportPriceBioenergy works", {
  expectReportSucceeds(reportPriceBioenergy)
})


test_that("reportPriceLand works", {
  expectReportSucceeds(reportPriceLand)
})


test_that("reportPriceWater works", {
  expectReportSucceeds(reportPriceWater)
})


test_that("reportValueTrade works", {
  expectReportSucceeds(reportValueTrade)
})


test_that("reportProcessing works", {
  expectReportSucceeds(reportProcessing, indicator = 'primary_to_process')
  expectReportSucceeds(reportProcessing, indicator = 'secondary_from_primary')

})


test_that("reportAEI works", {
  expectReportSucceeds(reportAEI)
})


test_that("reportWaterUsage works", {
  expectReportSucceeds(reportWaterUsage, detail = FALSE)
  expectReportSucceeds(reportWaterUsage, detail = TRUE)
})


test_that("reportWaterAvailability works", {
  expectReportSucceeds(reportWaterAvailability)
})


test_that("reportAAI works", {
  expectReportSucceeds(reportAAI)
})


test_that("reportSOM works", {
  expectReportSucceeds(reportSOM)
})


test_that("reportGrowingStock works", {
  expectReportSucceeds(reportGrowingStock, indicator = 'relative')
  expectReportSucceeds(reportGrowingStock, indicator = 'absolute')
})



test_that("reportSDG1 works", {
  expectReportSucceeds(reportSDG1)
})


test_that("reportSDG2 works", {
  expectReportSucceeds(reportSDG2)
})


test_that("reportSDG3 works", {
  expectReportSucceeds(reportSDG3)
})


test_that("reportSDG6 works", {
  expectReportSucceeds(reportSDG6)
})


test_that("reportSDG12 works", {
  expectReportSucceeds(reportSDG12)
})


test_that("reportSDG15 works", {
  expectReportSucceeds(reportSDG15)
})


test_that("reportPBwater works", {
  expectReportSucceeds(reportPBwater, level = 'regglo')
})


test_that("reportPBland works", {
  expectReportSucceeds(reportPBland, level = 'regglo')
})


test_that("reportPBbiosphere works", {
  expectReportSucceeds(reportPBbiosphere, level = 'regglo')
})


test_that("reportPBnitrogen works", {
  expectReportSucceeds(reportPBnitrogen, level = 'regglo')
})
