test_that("reportManure works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportManure, fullDataName = !!fullDataName))
  }
})


test_that("reportNetForestChange works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportNetForestChange, fullDataName = !!fullDataName))
  }
})


test_that("reportNitrogenBudgetCropland works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportNitrogenBudgetCropland, fullDataName = !!fullDataName))
  }
})


test_that("reportNitrogenBudgetPasture works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportNitrogenBudgetPasture, fullDataName = !!fullDataName))
  }
})


test_that("reportNitrogenEfficiencies works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportNitrogenEfficiencies, fullDataName = !!fullDataName))
  }
})


test_that("reportNitrogenPollution works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportNitrogenPollution, fullDataName = !!fullDataName))
  }
})


test_that("reportOutputPerWorker works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportOutputPerWorker, fullDataName = !!fullDataName))
  }
})


test_that("reportPBbiosphere works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPBbiosphere, fullDataName = !!fullDataName, level = "regglo"))
  }
})


test_that("reportPBland works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPBland, fullDataName = !!fullDataName, level = "regglo"))
  }
})


test_that("reportPBnitrogen works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPBnitrogen, fullDataName = !!fullDataName, level = "regglo"))
  }
})


test_that("reportPBwater works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPBwater, fullDataName = !!fullDataName, level = "regglo"))
  }
})


test_that("reportPeatland works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPeatland, fullDataName = !!fullDataName))
  }
})


test_that("reportPlantationEstablishment works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPlantationEstablishment, fullDataName = !!fullDataName))
  }
})


test_that("reportPopulation works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPopulation, fullDataName = !!fullDataName))
  }
})


test_that("reportPriceAgriculture works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPriceAgriculture, fullDataName = !!fullDataName))
  }
})


test_that("reportPriceBioenergy works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPriceBioenergy, fullDataName = !!fullDataName))
  }
})


test_that("reportPriceElasticities works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPriceElasticities, fullDataName = !!fullDataName))
  }
})


test_that("reportPriceFoodIndex works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPriceFoodIndex, fullDataName = !!fullDataName, baseyear = "y2010"))
    expectValidReport(expectReportSucceeds(reportPriceFoodIndex, fullDataName = !!fullDataName, baseyear = "y2020"))
  }
})


test_that("reportPriceGHG works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPriceGHG, fullDataName = !!fullDataName))
  }
})


test_that("reportPriceLand works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPriceLand, fullDataName = !!fullDataName))
  }
})


test_that("reportPriceShock works", {
  for (fullDataName in oldAndCurrentData()) {
  expectEmptyOrValidReport(expectReportSucceeds(reportPriceShock, fullDataName = !!fullDataName))
  }
})


test_that("reportPriceWater works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPriceWater, fullDataName = !!fullDataName))
  }
})


test_that("reportPriceWoodyBiomass works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportPriceWoodyBiomass, fullDataName = !!fullDataName))
  }
})


test_that("reportProcessing works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportProcessing, fullDataName = !!fullDataName, indicator = "primary_to_process"))
    expectValidReport(expectReportSucceeds(reportProcessing, fullDataName = !!fullDataName, indicator = "secondary_from_primary"))

  }
})


test_that("reportProducerPriceIndex works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportProducerPriceIndex, fullDataName = !!fullDataName))
  }
})


test_that("reportProduction works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportProduction, fullDataName = !!fullDataName, level = "regglo", detail = FALSE))
    expectValidReport(expectReportSucceeds(reportProduction, fullDataName = !!fullDataName, level = "regglo", detail = TRUE))
  }
})


test_that("reportProductionBioenergy works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportProductionBioenergy, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportProductionBioenergy, fullDataName = !!fullDataName, detail = TRUE))
  }
})


test_that("reportProtein works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportProtein, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportProtein, fullDataName = !!fullDataName, detail = TRUE))
  }
})
