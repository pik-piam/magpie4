test_that("reportManure works", {
  expectValidReport(expectReportSucceeds(reportManure))
})


test_that("reportNetForestChange works", {
  expectValidReport(expectReportSucceeds(reportNetForestChange))
})


test_that("reportNitrogenBudgetCropland works", {
  expectValidReport(expectReportSucceeds(reportNitrogenBudgetCropland))
})


test_that("reportNitrogenBudgetPasture works", {
  expectValidReport(expectReportSucceeds(reportNitrogenBudgetPasture))
})


test_that("reportNitrogenEfficiencies works", {
  expectValidReport(expectReportSucceeds(reportNitrogenEfficiencies))
})


test_that("reportNitrogenPollution works", {
  expectValidReport(expectReportSucceeds(reportNitrogenPollution))
})


test_that("reportOutputPerWorker works", {
  expectValidReport(expectReportSucceeds(reportOutputPerWorker))
})


test_that("reportPBbiosphere works", {
  expectValidReport(expectReportSucceeds(reportPBbiosphere, level = "regglo"))
})


test_that("reportPBland works", {
  expectValidReport(expectReportSucceeds(reportPBland, level = "regglo"))
})


test_that("reportPBnitrogen works", {
  expectValidReport(expectReportSucceeds(reportPBnitrogen, level = "regglo"))
})


test_that("reportPBwater works", {
  expectValidReport(expectReportSucceeds(reportPBwater, level = "regglo"))
})


test_that("reportPeatland works", {
  expectValidReport(expectReportSucceeds(reportPeatland))
})


test_that("reportPlantationEstablishment works", {
  expectValidReport(expectReportSucceeds(reportPlantationEstablishment))
})


test_that("reportPopulation works", {
  expectValidReport(expectReportSucceeds(reportPopulation))
})


test_that("reportPriceAgriculture works", {
  expectValidReport(expectReportSucceeds(reportPriceAgriculture))
})


test_that("reportPriceBioenergy works", {
  expectValidReport(expectReportSucceeds(reportPriceBioenergy))
})


test_that("reportPriceElasticities works", {
  expectValidReport(expectReportSucceeds(reportPriceElasticities))
})


test_that("reportPriceFoodIndex works", {
  expectValidReport(expectReportSucceeds(reportPriceFoodIndex, baseyear = "y2010"))
  expectValidReport(expectReportSucceeds(reportPriceFoodIndex, baseyear = "y2020"))
})


test_that("reportPriceGHG works", {
  expectValidReport(expectReportSucceeds(reportPriceGHG))
})


test_that("reportPriceLand works", {
  expectValidReport(expectReportSucceeds(reportPriceLand))
})


test_that("reportPriceShock works", {
  expectEmptyOrValidReport(expectReportSucceeds(reportPriceShock))
})


test_that("reportPriceWater works", {
  expectValidReport(expectReportSucceeds(reportPriceWater))
})


test_that("reportPriceWoodyBiomass works", {
  expectValidReport(expectReportSucceeds(reportPriceWoodyBiomass))
})


test_that("reportProcessing works", {
  expectValidReport(expectReportSucceeds(reportProcessing, indicator = "primary_to_process"))
  expectValidReport(expectReportSucceeds(reportProcessing, indicator = "secondary_from_primary"))

})


test_that("reportProducerPriceIndex works", {
  expectValidReport(expectReportSucceeds(reportProducerPriceIndex))
})


test_that("reportProduction works", {
  expectValidReport(expectReportSucceeds(reportProduction, level = "regglo", detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProduction, level = "regglo", detail = TRUE))
})


test_that("reportProductionBioenergy works", {
  expectValidReport(expectReportSucceeds(reportProductionBioenergy, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProductionBioenergy, detail = TRUE))
})


test_that("reportProtein works", {
  expectValidReport(expectReportSucceeds(reportProtein, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProtein, detail = TRUE))
})
