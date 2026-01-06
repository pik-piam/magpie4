test_that("reportDemand works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportDemand, fullDataName = !!fullDataName, detail = FALSE, level = "regglo"))
    expectValidReport(expectReportSucceeds(reportDemand, fullDataName = !!fullDataName, detail = TRUE, level = "regglo"))
  }
})


test_that("reportDemandBioenergy works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportDemandBioenergy, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportDemandBioenergy, fullDataName = !!fullDataName, detail = TRUE))
  }
})


test_that("reportEmissions works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportEmissions, fullDataName = !!fullDataName))
  }
})


test_that("reportEmissionsBeforeTechnicalMitigation works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportEmissionsBeforeTechnicalMitigation, fullDataName = !!fullDataName))
  }
})


test_that("reportExpenditureFoodIndex works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportExpenditureFoodIndex, fullDataName = !!fullDataName))
  }
})


test_that("reportExtraResidueEmissions works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportExtraResidueEmissions, fullDataName = !!fullDataName, level = "regglo"))
  }
})


test_that("reportFactorCostShares works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportFactorCostShares, fullDataName = !!fullDataName, type = "requirements"))
    expectValidReport(expectReportSucceeds(reportFactorCostShares, fullDataName = !!fullDataName, type = "optimization"))
    expectValidReport(expectReportSucceeds(reportFactorCostShares, fullDataName = !!fullDataName, type = "accounting"))
  }
})


test_that("reportFeed works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportFeed, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportFeed, fullDataName = !!fullDataName, detail = TRUE))
  }
})


test_that("reportFeedConversion works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportFeedConversion, fullDataName = !!fullDataName))
  }
})


test_that("reportFireEmissions works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportFireEmissions, fullDataName = !!fullDataName, level = "regglo"))
  }
})


test_that("reportFit works", {
  for (fullDataName in oldAndCurrentData()) {
  for (type in c("R2", "MAE", "MPE", "MAP")) {
    for (level in c("grid", "cell")) {
        expectValidReport(expectReportSucceeds(reportFit, fullDataName = !!fullDataName, type = type, level = level))
    }
  }
  }
})


test_that("reportFoodExpenditure works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportFoodExpenditure, fullDataName = !!fullDataName))
  }
})


test_that("reportForestYield works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportForestYield, fullDataName = !!fullDataName))
  }
})


test_that("reportGrasslandManagement works", {
  for (fullDataName in oldAndCurrentData()) {
  expectDisabledReport(expectReportSucceeds(reportGrasslandManagement, fullDataName = !!fullDataName))
  }
})


test_that("reportGrasslandYields works", {
  for (fullDataName in oldAndCurrentData()) {
  expectDisabledReport(expectReportSucceeds(reportGrasslandYields, fullDataName = !!fullDataName))
  }
})


test_that("reportGrassStats works", {
  for (fullDataName in oldAndCurrentData()) {
  expectDisabledReport(expectReportSucceeds(reportGrassStats, fullDataName = !!fullDataName))
  }
})


test_that("reportGrowingStock works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportGrowingStock, fullDataName = !!fullDataName, indicator = "relative"))
    expectValidReport(expectReportSucceeds(reportGrowingStock, fullDataName = !!fullDataName, indicator = "absolute"))
  }
})


test_that("reportharvested_area_timber works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportharvested_area_timber, fullDataName = !!fullDataName))
  }
})


test_that("reportHourlyLaborCosts works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportHourlyLaborCosts, fullDataName = !!fullDataName))
  }
})


test_that("reportIncome works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportIncome, fullDataName = !!fullDataName, type = "ppp"))
    expectValidReport(expectReportSucceeds(reportIncome, fullDataName = !!fullDataName, type = "mer"))

  }
})

test_that("reportIntakeDetailed works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportIntakeDetailed, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportIntakeDetailed, fullDataName = !!fullDataName, detail = TRUE))
  }
})


test_that("reportKcal works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportKcal, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportKcal, fullDataName = !!fullDataName, detail = TRUE))
  }
})


test_that("reportLaborCostsEmpl works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportLaborCostsEmpl, fullDataName = !!fullDataName))
  }
})


test_that("reportLaborProductivity works", {
  for (fullDataName in oldAndCurrentData()) {
  expectEmptyOrValidReport(expectReportSucceeds(reportLaborProductivity, fullDataName = !!fullDataName))
  }
})


test_that("reportLandConservation works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportLandConservation, fullDataName = !!fullDataName))
  }
})


test_that("reportLandUse works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportLandUse, fullDataName = !!fullDataName, level = "regglo"))
  }
})


test_that("reportLandUseChange works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportLandUseChange, fullDataName = !!fullDataName))
  }
})


test_that("reportLivestockDemStructure works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportLivestockDemStructure, fullDataName = !!fullDataName))
  }
})


test_that("reportLivestockShare works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportLivestockShare, fullDataName = !!fullDataName))
  }
})


test_that("reportLSUGrasslands works", {
  for (fullDataName in oldAndCurrentData()) {
  expectDisabledReport(expectReportSucceeds(reportLSUGrasslands, fullDataName = !!fullDataName))
  }
})
