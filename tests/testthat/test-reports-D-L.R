test_that("reportDemand works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportDemand,
                                         detail = FALSE, level = "regglo"))
  expectValidReport(expectReportSucceeds(reportDemand,
                                         detail = TRUE, level = "regglo"))
})


test_that("reportDemandBioenergy works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportDemandBioenergy, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportDemandBioenergy, detail = TRUE))
})


test_that("reportEmissions works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportEmissions))
})


test_that("reportEmissionsBeforeTechnicalMitigation works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportEmissionsBeforeTechnicalMitigation))
})


test_that("reportExpenditureFoodIndex works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportExpenditureFoodIndex))
})


test_that("reportExtraResidueEmissions works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportExtraResidueEmissions,
                                         level = "regglo"))
})


test_that("reportFactorCostShares works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFactorCostShares,
                                         type = "requirements"))
  expectValidReport(expectReportSucceeds(reportFactorCostShares,
                                         type = "optimization"))
  expectValidReport(expectReportSucceeds(reportFactorCostShares,
                                         type = "accounting"))
})


test_that("reportFeed works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFeed, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportFeed, detail = TRUE))
})


test_that("reportFeedConversion works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFeedConversion))
})


test_that("reportFireEmissions works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFireEmissions, level = "regglo"))
})


test_that("reportFit works", {
  run_only_if_full_tests_requested()
  for (type in c("R2", "MAE", "MPE", "MAPE")) {
    for (level in c("grid", "cell")) {
      expectValidReport(expectReportSucceeds(reportFit, type = !!type, level = !!level))
    }
  }
})


test_that("reportFoodExpenditure works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFoodExpenditure))
})


test_that("reportForestYield works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportForestYield))
})


test_that("reportGrasslandManagement works", {
  run_only_if_full_tests_requested()
  expectDisabledReport(expectReportSucceeds(reportGrasslandManagement))
})


test_that("reportGrasslandYields works", {
  run_only_if_full_tests_requested()
  expectDisabledReport(expectReportSucceeds(reportGrasslandYields))
})


test_that("reportGrassStats works", {
  run_only_if_full_tests_requested()
  expectDisabledReport(expectReportSucceeds(reportGrassStats))
})


test_that("reportGrowingStock works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportGrowingStock, indicator = "relative"))
  expectValidReport(expectReportSucceeds(reportGrowingStock, indicator = "absolute"))
})


test_that("reportharvested_area_timber works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportharvested_area_timber))
})


test_that("reportHourlyLaborCosts works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportHourlyLaborCosts))
})


test_that("reportIncome works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportIncome, type = "ppp"))
  expectValidReport(expectReportSucceeds(reportIncome, type = "mer"))

})

test_that("reportIntakeDetailed works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportIntakeDetailed, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportIntakeDetailed, detail = TRUE))
})


test_that("reportKcal works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportKcal, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportKcal, detail = TRUE))
})


test_that("reportLaborCostsEmpl works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLaborCostsEmpl))
})


test_that("reportLaborProductivity works", {
  run_only_if_full_tests_requested()
  expectEmptyOrValidReport(expectReportSucceeds(reportLaborProductivity))
})


test_that("reportLandConservation works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLandConservation))
})


test_that("reportLandUse works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLandUse, level = "regglo"))
})


test_that("reportLandUseChange works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLandUseChange))
})


test_that("reportLivestockDemStructure works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLivestockDemStructure))
})


test_that("reportLivestockShare works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLivestockShare))
})


test_that("reportLSUGrasslands works", {
  run_only_if_full_tests_requested()
  expectDisabledReport(expectReportSucceeds(reportLSUGrasslands))
})
