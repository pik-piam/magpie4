test_that("reportManure works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportManure, fullDataName = !!fullDataName))
})


test_that("reportNetForestChange works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportNetForestChange, fullDataName = !!fullDataName))
})


test_that("reportNitrogenBudgetCropland works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportNitrogenBudgetCropland, fullDataName = !!fullDataName))
})


test_that("reportNitrogenBudgetPasture works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportNitrogenBudgetPasture, fullDataName = !!fullDataName))
})


test_that("reportNitrogenEfficiencies works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportNitrogenEfficiencies, fullDataName = !!fullDataName))
})


test_that("reportNitrogenPollution works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportNitrogenPollution, fullDataName = !!fullDataName))
})


test_that("reportOutputPerWorker works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportOutputPerWorker, fullDataName = !!fullDataName))
})


test_that("reportPBbiosphere works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPBbiosphere, fullDataName = !!fullDataName, level = "regglo"))
})


test_that("reportPBland works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPBland, fullDataName = !!fullDataName, level = "regglo"))
})


test_that("reportPBnitrogen works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPBnitrogen, fullDataName = !!fullDataName, level = "regglo"))
})


test_that("reportPBwater works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPBwater, fullDataName = !!fullDataName, level = "regglo"))
})


test_that("reportPeatland works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPeatland, fullDataName = !!fullDataName))
})


test_that("reportPlantationEstablishment works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPlantationEstablishment, fullDataName = !!fullDataName))
})


test_that("reportPopulation works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPopulation, fullDataName = !!fullDataName))
})


test_that("reportPriceAgriculture works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceAgriculture, fullDataName = !!fullDataName))
})


test_that("reportPriceBioenergy works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceBioenergy, fullDataName = !!fullDataName))
})


test_that("reportPriceElasticities works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceElasticities, fullDataName = !!fullDataName))
})


test_that("reportPriceFoodIndex works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceFoodIndex, fullDataName = !!fullDataName, baseyear = "y2010"))
  expectValidReport(expectReportSucceeds(reportPriceFoodIndex, fullDataName = !!fullDataName, baseyear = "y2020"))
})


test_that("reportPriceGHG works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceGHG, fullDataName = !!fullDataName))
})


test_that("reportPriceLand works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceLand, fullDataName = !!fullDataName))
})


test_that("reportPriceShock works", {
  run_only_if_full_tests_requested()
  expectEmptyOrValidReport(expectReportSucceeds(reportPriceShock, fullDataName = !!fullDataName))
})


test_that("reportPriceWater works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceWater, fullDataName = !!fullDataName))
})


test_that("reportPriceWoodyBiomass works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceWoodyBiomass, fullDataName = !!fullDataName))
})


test_that("reportProcessing works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportProcessing,
                                         fullDataName = !!fullDataName,
                                         indicator = "primary_to_process"))
  expectValidReport(expectReportSucceeds(reportProcessing,
                                         fullDataName = !!fullDataName,
                                         indicator = "secondary_from_primary"))

})


test_that("reportProducerPriceIndex works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportProducerPriceIndex, fullDataName = !!fullDataName))
})


test_that("reportProduction works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportProduction,
                                         fullDataName = !!fullDataName,
                                         level = "regglo", detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProduction,
                                         fullDataName = !!fullDataName,
                                         level = "regglo", detail = TRUE))
})


test_that("reportProductionBioenergy works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportProductionBioenergy, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProductionBioenergy, fullDataName = !!fullDataName, detail = TRUE))
})


test_that("reportProtein works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportProtein, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProtein, fullDataName = !!fullDataName, detail = TRUE))
})
