test_that("reportPopulation works", {
  expectReportSucceeds(reportPopulation)
})


test_that("reportWorkingAgePopulation works", {
  expectReportSucceeds(reportWorkingAgePopulation)
})


test_that("reportIncome works", {
  expectReportSucceeds(reportIncome, type = 'ppp')
  expectReportSucceeds(reportIncome, type = 'mer')

})

test_that("reportPriceGHG works", {
  expectReportSucceeds(reportPriceGHG)
})


test_that("reportFoodExpenditure works", {
  expectReportSucceeds(reportFoodExpenditure)
})


test_that("reportKcal works", {
  expectReportSucceeds(reportKcal, detail = FALSE)
  expectReportSucceeds(reportKcal, detail = TRUE)
})


test_that("reportProtein works", {
  expectReportSucceeds(reportProtein, detail = FALSE)
  expectReportSucceeds(reportProtein, detail = TRUE)
})


test_that("reportIntakeDetailed works", {
  expectReportSucceeds(reportIntakeDetailed, detail = FALSE)
  expectReportSucceeds(reportIntakeDetailed, detail = TRUE)
})


test_that("reportAnthropometrics works", {
  expectReportSucceeds(reportAnthropometrics)
})


test_that("reportLivestockShare works", {
  expectReportSucceeds(reportLivestockShare)
})


test_that("reportLivestockDemStructure works", {
  expectReportSucceeds(reportLivestockDemStructure)
})


test_that("reportVegfruitShare works", {
  expectReportSucceeds(reportVegfruitShare)
})


test_that("reportPriceShock works", {
  expectReportSucceeds(reportPriceShock)
})


test_that("reportPriceElasticities works", {
  expectReportSucceeds(reportPriceElasticities)
})


test_that("reportDemand works", {
  expectReportSucceeds(reportDemand, detail = FALSE, level = "regglo")
  expectReportSucceeds(reportDemand, detail = TRUE, level = "regglo")
})


test_that("reportDemandBioenergy works", {
  expectReportSucceeds(reportDemandBioenergy, detail = FALSE)
  expectReportSucceeds(reportDemandBioenergy, detail = TRUE)
})


test_that("reportFeed works", {
  expectReportSucceeds(reportFeed, detail = FALSE)
  expectReportSucceeds(reportFeed, detail = TRUE)
})


test_that("reportProduction works", {
  expectReportSucceeds(reportProduction, level = "regglo", detail = FALSE)
  expectReportSucceeds(reportProduction, level = "regglo", detail = TRUE)
})


test_that("reportProductionBioenergy works", {
  expectReportSucceeds(reportProductionBioenergy, detail = FALSE)
  expectReportSucceeds(reportProductionBioenergy, detail = TRUE)
})


test_that("reportTrade works", {
  expectReportSucceeds(reportTrade, detail = FALSE)
  expectReportSucceeds(reportTrade, detail = TRUE)
})


test_that("reportLandUse works", {
  expectReportSucceeds(reportLandUse, level = "regglo")
})


test_that("reportLandUseChange works", {
  expectReportSucceeds(reportLandUseChange)
})


test_that("reportNetForestChange works", {
  expectReportSucceeds(reportNetForestChange)
})


test_that("reportPeatland works", {
  expectReportSucceeds(reportPeatland)
})


test_that("reportLandConservation works", {
  expectReportSucceeds(reportLandConservation)
})


test_that("reportCroparea works", {
  expectReportSucceeds(reportCroparea, detail = FALSE)
  expectReportSucceeds(reportCroparea, detail = TRUE)
})
