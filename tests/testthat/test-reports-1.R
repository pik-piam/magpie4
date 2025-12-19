test_that("reportPopulation works", {
  expectValidReport(expectReportSucceeds(reportPopulation))
})


test_that("reportWorkingAgePopulation works", {
  expectValidReport(expectReportSucceeds(reportWorkingAgePopulation))
})


test_that("reportIncome works", {
  expectValidReport(expectReportSucceeds(reportIncome, type = 'ppp'))
  expectValidReport(expectReportSucceeds(reportIncome, type = 'mer'))

})

test_that("reportPriceGHG works", {
  expectValidReport(expectReportSucceeds(reportPriceGHG))
})


test_that("reportFoodExpenditure works", {
  expectValidReport(expectReportSucceeds(reportFoodExpenditure))
})


test_that("reportKcal works", {
  expectValidReport(expectReportSucceeds(reportKcal, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportKcal, detail = TRUE))
})


test_that("reportProtein works", {
  expectValidReport(expectReportSucceeds(reportProtein, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProtein, detail = TRUE))
})


test_that("reportIntakeDetailed works", {
  expectValidReport(expectReportSucceeds(reportIntakeDetailed, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportIntakeDetailed, detail = TRUE))
})


test_that("reportAnthropometrics works", {
  expectValidReport(expectReportSucceeds(reportAnthropometrics))
})


test_that("reportLivestockShare works", {
  expectValidReport(expectReportSucceeds(reportLivestockShare))
})


test_that("reportLivestockDemStructure works", {
  expectValidReport(expectReportSucceeds(reportLivestockDemStructure))
})


test_that("reportVegfruitShare works", {
  expectValidReport(expectReportSucceeds(reportVegfruitShare))
})


test_that("reportPriceShock works", {
  expectEmptyReport(expectReportSucceeds(reportPriceShock))
})


test_that("reportPriceElasticities works", {
  expectValidReport(expectReportSucceeds(reportPriceElasticities))
})


test_that("reportDemand works", {
  expectValidReport(expectReportSucceeds(reportDemand, detail = FALSE, level = "regglo"))
  expectValidReport(expectReportSucceeds(reportDemand, detail = TRUE, level = "regglo"))
})


test_that("reportDemandBioenergy works", {
  expectValidReport(expectReportSucceeds(reportDemandBioenergy, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportDemandBioenergy, detail = TRUE))
})


test_that("reportFeed works", {
  expectValidReport(expectReportSucceeds(reportFeed, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportFeed, detail = TRUE))
})


test_that("reportProduction works", {
  expectValidReport(expectReportSucceeds(reportProduction, level = "regglo", detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProduction, level = "regglo", detail = TRUE))
})


test_that("reportProductionBioenergy works", {
  expectValidReport(expectReportSucceeds(reportProductionBioenergy, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportProductionBioenergy, detail = TRUE))
})


test_that("reportTrade works", {
  expectValidReport(expectReportSucceeds(reportTrade, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportTrade, detail = TRUE))
})


test_that("reportLandUse works", {
  expectValidReport(expectReportSucceeds(reportLandUse, level = "regglo"))
})


test_that("reportLandUseChange works", {
  expectValidReport(expectReportSucceeds(reportLandUseChange))
})


test_that("reportNetForestChange works", {
  expectValidReport(expectReportSucceeds(reportNetForestChange))
})


test_that("reportPeatland works", {
  expectValidReport(expectReportSucceeds(reportPeatland))
})


test_that("reportLandConservation works", {
  expectValidReport(expectReportSucceeds(reportLandConservation))
})


test_that("reportCroparea works", {
  expectValidReport(expectReportSucceeds(reportCroparea, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportCroparea, detail = TRUE))
})
