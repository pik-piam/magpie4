test_that("reportNitrogenBudgetCropland works", {
  expectValidReport(expectReportSucceeds(reportNitrogenBudgetCropland))
})


test_that("reportNitrogenBudgetPasture works", {
  expectValidReport(expectReportSucceeds(reportNitrogenBudgetPasture))
})


test_that("reportNitrogenEfficiencies works", {
  expectValidReport(expectReportSucceeds(reportNitrogenEfficiencies))
})


test_that("reportManure works", {
  expectValidReport(expectReportSucceeds(reportManure))
})


test_that("reportNitrogenPollution works", {
  expectValidReport(expectReportSucceeds(reportNitrogenPollution))
})


test_that("reportYields works", {
  expectValidReport(expectReportSucceeds(reportYields, detail = FALSE, physical = TRUE))
  expectValidReport(expectReportSucceeds(reportYields, detail = FALSE, physical = FALSE))
  expectValidReport(expectReportSucceeds(reportYields, detail = TRUE, physical = TRUE))
  expectValidReport(expectReportSucceeds(reportYields, detail = TRUE, physical = FALSE))
})


test_that("reportYieldsCropCalib works", {
  expectValidReport(expectReportSucceeds(reportYieldsCropCalib, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportYieldsCropCalib, detail = TRUE))
})


test_that("reportYieldsCropRaw works", {
  expectValidReport(expectReportSucceeds(reportYieldsCropRaw, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportYieldsCropRaw, detail = TRUE))
})


test_that("reportFeedConversion works", {
  expectValidReport(expectReportSucceeds(reportFeedConversion))
})


test_that("reportTau works", {
  expectValidReport(expectReportSucceeds(reportTau))
})


test_that("reportTc works", {
  expectValidReport(expectReportSucceeds(reportTc, level = "regglo"))
})


test_that("reportAgriResearchIntensity works", {
  expectValidReport(expectReportSucceeds(reportAgriResearchIntensity))
})


test_that("reportEmissions works", {
  expectValidReport(expectReportSucceeds(reportEmissions))
})


test_that("reportEmissionsBeforeTechnicalMitigation works", {
  expectValidReport(expectReportSucceeds(reportEmissionsBeforeTechnicalMitigation))
})


test_that("reportCosts works", {
  expectValidReport(expectReportSucceeds(reportCosts))
})


test_that("reportCostsPresolve works", {
  expectEmptyReport(expectReportSucceeds(reportCostsPresolve))
})


test_that("reportCostTransport works", {
  expectValidReport(expectReportSucceeds(reportCostTransport))
})


test_that("reportCostsFertilizer works", {
  expectValidReport(expectReportSucceeds(reportCostsFertilizer))
})


test_that("reportCostOverall works", {
  expectValidReport(expectReportSucceeds(reportCostOverall))
})


test_that("reportCostCapitalStocks works", {
  expectValidReport(expectReportSucceeds(reportCostCapitalStocks))
})


test_that("reportCostCapitalInvestment works", {
  expectValidReport(expectReportSucceeds(reportCostCapitalInvestment))
})


test_that("reportCostsInputFactors works", {
  expectValidReport(expectReportSucceeds(reportCostsInputFactors))
})


test_that("reportCostsAccounting works", {
  expectValidReport(expectReportSucceeds(reportCostsAccounting))
})


test_that("reportCostsWithoutIncentives works", {
  expectValidReport(expectReportSucceeds(reportCostsWithoutIncentives))
})


test_that("reportAgGDP works", {
  expectValidReport(expectReportSucceeds(reportAgGDP))
})


test_that("reportConsumVal works", {
  expectValidReport(expectReportSucceeds(reportConsumVal))
})
