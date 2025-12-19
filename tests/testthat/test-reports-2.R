test_that("reportNitrogenBudgetCropland works", {
  expectReportSucceeds(reportNitrogenBudgetCropland)
})


test_that("reportNitrogenBudgetPasture works", {
  expectReportSucceeds(reportNitrogenBudgetPasture)
})


test_that("reportNitrogenEfficiencies works", {
  expectReportSucceeds(reportNitrogenEfficiencies)
})


test_that("reportManure works", {
  expectReportSucceeds(reportManure)
})


test_that("reportNitrogenPollution works", {
  expectReportSucceeds(reportNitrogenPollution)
})


test_that("reportYields works", {
  expectReportSucceeds(reportYields, detail = FALSE, physical = TRUE)
  expectReportSucceeds(reportYields, detail = FALSE, physical = FALSE)
  expectReportSucceeds(reportYields, detail = TRUE, physical = TRUE)
})


test_that("reportYieldsCropCalib works", {
  expectReportSucceeds(reportYieldsCropCalib, detail = FALSE)
  expectReportSucceeds(reportYieldsCropCalib, detail = TRUE)
})


test_that("reportYieldsCropRaw works", {
  expectReportSucceeds(reportYieldsCropRaw, detail = FALSE)
  expectReportSucceeds(reportYieldsCropRaw, detail = TRUE)
})


test_that("reportFeedConversion works", {
  expectReportSucceeds(reportFeedConversion)
})


test_that("reportTau works", {
  expectReportSucceeds(reportTau)
})


test_that("reportTc works", {
  expectReportSucceeds(reportTc, level = "regglo")
})


test_that("reportAgriResearchIntensity works", {
  expectReportSucceeds(reportAgriResearchIntensity)
})


test_that("reportEmissions works", {
  expectReportSucceeds(reportEmissions)
})


test_that("reportEmissionsBeforeTechnicalMitigation works", {
  expectReportSucceeds(reportEmissionsBeforeTechnicalMitigation)
})


test_that("reportCosts works", {
  expectReportSucceeds(reportCosts)
})


test_that("reportCostsPresolve works", {
  expectReportSucceeds(reportCostsPresolve)
})


test_that("reportCostTransport works", {
  expectReportSucceeds(reportCostTransport)
})


test_that("reportCostsFertilizer works", {
  expectReportSucceeds(reportCostsFertilizer)
})


test_that("reportCostOverall works", {
  expectReportSucceeds(reportCostOverall)
})


test_that("reportCostCapitalStocks works", {
  expectReportSucceeds(reportCostCapitalStocks)
})


test_that("reportCostCapitalInvestment works", {
  expectReportSucceeds(reportCostCapitalInvestment)
})


test_that("reportCostsInputFactors works", {
  expectReportSucceeds(reportCostsInputFactors)
})


test_that("reportCostsAccounting works", {
  expectReportSucceeds(reportCostsAccounting)
})


test_that("reportCostsWithoutIncentives works", {
  expectReportSucceeds(reportCostsWithoutIncentives)
})


test_that("reportAgGDP works", {
  expectReportSucceeds(reportAgGDP)
})


test_that("reportConsumVal works", {
  expectReportSucceeds(reportConsumVal)
})
