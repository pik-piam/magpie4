test_that("reportDemand works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportDemand,
                                         fullDataName = !!fullDataName,
                                         detail = FALSE, level = "regglo"))
  expectValidReport(expectReportSucceeds(reportDemand,
                                         fullDataName = !!fullDataName,
                                         detail = TRUE, level = "regglo"))
})


test_that("reportDemandBioenergy works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportDemandBioenergy, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportDemandBioenergy, fullDataName = !!fullDataName, detail = TRUE))
})


test_that("reportEmissions works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportEmissions, fullDataName = !!fullDataName))
})


test_that("reportEmissionsBeforeTechnicalMitigation works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportEmissionsBeforeTechnicalMitigation, fullDataName = !!fullDataName))
})


test_that("reportExpenditureFoodIndex works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportExpenditureFoodIndex, fullDataName = !!fullDataName))
})


test_that("reportExtraResidueEmissions works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportExtraResidueEmissions,
                                         fullDataName = !!fullDataName,
                                         level = "regglo"))
})


test_that("reportFactorCostShares works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFactorCostShares,
                                         fullDataName = !!fullDataName,
                                         type = "requirements"))
  expectValidReport(expectReportSucceeds(reportFactorCostShares,
                                         fullDataName = !!fullDataName,
                                         type = "optimization"))
  expectValidReport(expectReportSucceeds(reportFactorCostShares,
                                         fullDataName = !!fullDataName,
                                         type = "accounting"))
})


test_that("reportFeed works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFeed, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportFeed, fullDataName = !!fullDataName, detail = TRUE))
})


test_that("reportFeedConversion works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFeedConversion, fullDataName = !!fullDataName))
})


test_that("reportFireEmissions works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFireEmissions, fullDataName = !!fullDataName, level = "regglo"))
})


test_that("reportFit works", {
  run_only_if_full_tests_requested()
  for (type in c("R2", "MAE", "MPE", "MAPE")) {
    for (level in c("grid", "cell")) {
      expectValidReport(expectReportSucceeds(reportFit, fullDataName = !!fullDataName, type = !!type, level = !!level))
    }
  }
})


test_that("reportFoodExpenditure works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFoodExpenditure, fullDataName = !!fullDataName))
})


test_that("reportForestYield works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportForestYield, fullDataName = !!fullDataName))
})


test_that("reportGrasslandManagement works", {
  run_only_if_full_tests_requested()
  expectDisabledReport(expectReportSucceeds(reportGrasslandManagement, fullDataName = !!fullDataName))
})


test_that("reportGrasslandYields works", {
  run_only_if_full_tests_requested()
  expectDisabledReport(expectReportSucceeds(reportGrasslandYields, fullDataName = !!fullDataName))
})


test_that("reportGrassStats works", {
  run_only_if_full_tests_requested()
  expectDisabledReport(expectReportSucceeds(reportGrassStats, fullDataName = !!fullDataName))
})


test_that("reportGrowingStock works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportGrowingStock, fullDataName = !!fullDataName, indicator = "relative"))
  expectValidReport(expectReportSucceeds(reportGrowingStock, fullDataName = !!fullDataName, indicator = "absolute"))
})


test_that("reportharvested_area_timber works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportharvested_area_timber, fullDataName = !!fullDataName))
})


test_that("reportHourlyLaborCosts works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportHourlyLaborCosts, fullDataName = !!fullDataName))
})


test_that("reportIncome works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportIncome, fullDataName = !!fullDataName, type = "ppp"))
  expectValidReport(expectReportSucceeds(reportIncome, fullDataName = !!fullDataName, type = "mer"))

})

test_that("reportIntakeDetailed works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportIntakeDetailed, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportIntakeDetailed, fullDataName = !!fullDataName, detail = TRUE))
})


test_that("reportKcal works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportKcal, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportKcal, fullDataName = !!fullDataName, detail = TRUE))
})


test_that("reportLaborCostsEmpl works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLaborCostsEmpl, fullDataName = !!fullDataName))
})


test_that("reportLaborProductivity works", {
  run_only_if_full_tests_requested()
  expectEmptyOrValidReport(expectReportSucceeds(reportLaborProductivity, fullDataName = !!fullDataName))
})


test_that("reportLandConservation works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLandConservation, fullDataName = !!fullDataName))
})


test_that("reportLandUse works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLandUse, fullDataName = !!fullDataName, level = "regglo"))
})


test_that("reportLandUseChange works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLandUseChange, fullDataName = !!fullDataName))
})


test_that("reportLivestockDemStructure works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLivestockDemStructure, fullDataName = !!fullDataName))
})


test_that("reportLivestockShare works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLivestockShare, fullDataName = !!fullDataName))
})


test_that("reportLSUGrasslands works", {
  run_only_if_full_tests_requested()
  expectDisabledReport(expectReportSucceeds(reportLSUGrasslands, fullDataName = !!fullDataName))
})
