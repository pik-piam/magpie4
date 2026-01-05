test_that("reportDemand works", {
  expectValidReport(expectReportSucceeds(reportDemand, detail = FALSE, level = "regglo"))
  expectValidReport(expectReportSucceeds(reportDemand, detail = TRUE, level = "regglo"))
})


test_that("reportDemandBioenergy works", {
  expectValidReport(expectReportSucceeds(reportDemandBioenergy, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportDemandBioenergy, detail = TRUE))
})


test_that("reportEmissions works", {
  expectValidReport(expectReportSucceeds(reportEmissions))
})


test_that("reportEmissionsBeforeTechnicalMitigation works", {
  expectValidReport(expectReportSucceeds(reportEmissionsBeforeTechnicalMitigation))
})


test_that("reportExpenditureFoodIndex works", {
  expectValidReport(expectReportSucceeds(reportExpenditureFoodIndex))
})


test_that("reportExtraResidueEmissions works", {
  expectValidReport(expectReportSucceeds(reportExtraResidueEmissions, level = "regglo"))
})


test_that("reportFactorCostShares works", {
  expectValidReport(expectReportSucceeds(reportFactorCostShares, type = "requirements"))
  expectValidReport(expectReportSucceeds(reportFactorCostShares, type = "optimization"))
  expectValidReport(expectReportSucceeds(reportFactorCostShares, type = "accounting"))
})


test_that("reportFeed works", {
  expectValidReport(expectReportSucceeds(reportFeed, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportFeed, detail = TRUE))
})


test_that("reportFeedConversion works", {
  expectValidReport(expectReportSucceeds(reportFeedConversion))
})


test_that("reportFireEmissions works", {
  expectValidReport(expectReportSucceeds(reportFireEmissions, level = "regglo"))
})


test_that("reportFit works", {
  expectValidReport(expectReportSucceeds(reportFit, type = "R2", level = "grid"))
  expectValidReport(expectReportSucceeds(reportFit, type = "R2", level = "cell"))
})


test_that("reportFoodExpenditure works", {
  expectValidReport(expectReportSucceeds(reportFoodExpenditure))
})


test_that("reportForestYield works", {
  expectValidReport(expectReportSucceeds(reportForestYield))
})


test_that("reportGrasslandManagement works", {
  expectDisabledReport(expectReportSucceeds(reportGrasslandManagement))
})


test_that("reportGrasslandYields works", {
  expectDisabledReport(expectReportSucceeds(reportGrasslandYields))
})


test_that("reportGrassStats works", {
  expectDisabledReport(expectReportSucceeds(reportGrassStats))
})


test_that("reportGrowingStock works", {
  expectValidReport(expectReportSucceeds(reportGrowingStock, indicator = "relative"))
  expectValidReport(expectReportSucceeds(reportGrowingStock, indicator = "absolute"))
})


test_that("reportharvested_area_timber works", {
  expectValidReport(expectReportSucceeds(reportharvested_area_timber))
})


test_that("reportHourlyLaborCosts works", {
  expectValidReport(expectReportSucceeds(reportHourlyLaborCosts))
})


test_that("reportIncome works", {
  expectValidReport(expectReportSucceeds(reportIncome, type = "ppp"))
  expectValidReport(expectReportSucceeds(reportIncome, type = "mer"))

})

test_that("reportIntakeDetailed works", {
  expectValidReport(expectReportSucceeds(reportIntakeDetailed, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportIntakeDetailed, detail = TRUE))
})


test_that("reportKcal works", {
  expectValidReport(expectReportSucceeds(reportKcal, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportKcal, detail = TRUE))
})


test_that("reportLaborCostsEmpl works", {
  expectValidReport(expectReportSucceeds(reportLaborCostsEmpl))
})


test_that("reportLaborProductivity works", {
  expectEmptyReport(expectReportSucceeds(reportLaborProductivity))
})


test_that("reportLandConservation works", {
  expectValidReport(expectReportSucceeds(reportLandConservation))
})


test_that("reportLandUse works", {
  expectValidReport(expectReportSucceeds(reportLandUse, level = "regglo"))
})


test_that("reportLandUseChange works", {
  expectValidReport(expectReportSucceeds(reportLandUseChange))
})


test_that("reportLivestockDemStructure works", {
  expectValidReport(expectReportSucceeds(reportLivestockDemStructure))
})


test_that("reportLivestockShare works", {
  expectValidReport(expectReportSucceeds(reportLivestockShare))
})


test_that("reportLSUGrasslands works", {
  expectDisabledReport(expectReportSucceeds(reportLSUGrasslands))
})
