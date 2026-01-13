test_that("reportManure works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportManure))
})


test_that("reportNetForestChange works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportNetForestChange))
})


test_that("reportNitrogenBudgetCropland works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportNitrogenBudgetCropland))
})


test_that("reportNitrogenBudgetPasture works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportNitrogenBudgetPasture))
})


test_that("reportNitrogenEfficiencies works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportNitrogenEfficiencies))
})


test_that("reportNitrogenPollution works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportNitrogenPollution))
})


test_that("reportOutputPerWorker works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportOutputPerWorker))
})


test_that("reportPBbiosphere works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPBbiosphere, level = "regglo"))
})


test_that("reportPBland works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPBland, level = "regglo"))
})


test_that("reportPBnitrogen works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPBnitrogen, level = "regglo"))
})


test_that("reportPBwater works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPBwater, level = "regglo"))
})


test_that("reportPeatland works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPeatland))
})


test_that("reportPlantationEstablishment works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPlantationEstablishment))
})


test_that("reportPopulation works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPopulation))
})


test_that("reportPriceAgriculture works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceAgriculture))
})


test_that("reportPriceBioenergy works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceBioenergy))
})


test_that("reportPriceElasticities works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceElasticities))
})


test_that("reportPriceFoodIndex works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceFoodIndex, baseyear = "y2010"))
  expectValidReport(expectReportSucceeds(reportPriceFoodIndex, baseyear = "y2020"))
})


test_that("reportPriceGHG works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceGHG))
})


test_that("reportPriceLand works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceLand))
})


test_that("reportPriceShock works", {
  run_only_if_full_tests_requested()
  expectEmptyOrValidReport(expectReportSucceeds(reportPriceShock))
})


test_that("reportPriceWater works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceWater))
})


test_that("reportPriceWoodyBiomass works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportPriceWoodyBiomass))
})


test_that("reportProcessing works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportProcessing,
                                         indicator = "primary_to_process"))
  expectValidReport(expectReportSucceeds(reportProcessing,
                                         indicator = "secondary_from_primary"))

})


test_that("reportProducerPriceIndex works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportProducerPriceIndex))
})


test_that("reportProduction works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportProduction,
                                         level = "regglo", detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProduction,
                                         level = "regglo", detail = TRUE))
})


test_that("reportProductionBioenergy works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportProductionBioenergy, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProductionBioenergy, detail = TRUE))
})


test_that("reportProtein works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportProtein, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProtein, detail = TRUE))
})
