test_that("reportAAI works", {
  expectValidReport(expectReportSucceeds(reportAAI))
})


test_that("reportAEI works", {
  expectValidReport(expectReportSucceeds(reportAEI))
})


test_that("reportAgEmployment works", {
  expectValidReport(expectReportSucceeds(reportAgEmployment, type = "absolute", detail = TRUE))
  expectValidReport(expectReportSucceeds(reportAgEmployment, type = "absolute", detail = FALSE))
  expectValidReport(expectReportSucceeds(reportAgEmployment, type = "share", detail = TRUE))
  expectValidReport(expectReportSucceeds(reportAgEmployment, type = "share", detail = FALSE))
})


test_that("reportAgGDP works", {
  expectValidReport(expectReportSucceeds(reportAgGDP))
})


test_that("reportAgriResearchIntensity works", {
  expectValidReport(expectReportSucceeds(reportAgriResearchIntensity))
})


test_that("reportAnthropometrics works", {
  expectValidReport(expectReportSucceeds(reportAnthropometrics))
})


test_that("reportBII works", {
  expectValidReport(expectReportSucceeds(reportBII))
})


test_that("reportBioplasticDemand works", {
  expectValidReport(expectReportSucceeds(reportBioplasticDemand))
})


test_that("reportCarbonstock works", {
  expectValidReport(expectReportSucceeds(reportCarbonstock))
})


test_that("reportConsumVal works", {
  expectValidReport(expectReportSucceeds(reportConsumVal))
})


test_that("reportCostCapitalInvestment works", {
  expectValidReport(expectReportSucceeds(reportCostCapitalInvestment))
})


test_that("reportCostCapitalStocks works", {
  expectValidReport(expectReportSucceeds(reportCostCapitalStocks))
})


test_that("reportCostOverall works", {
  expectValidReport(expectReportSucceeds(reportCostOverall))
})


test_that("reportCosts works", {
  expectValidReport(expectReportSucceeds(reportCosts))
})


test_that("reportCostsAccounting works", {
  expectValidReport(expectReportSucceeds(reportCostsAccounting))
})


test_that("reportCostsFertilizer works", {
  expectValidReport(expectReportSucceeds(reportCostsFertilizer))
})


test_that("reportCostsInputFactors works", {
  expectValidReport(expectReportSucceeds(reportCostsInputFactors))
})


test_that("reportCostsMACCS works", {
  expectValidReport(expectReportSucceeds(reportCostsMACCS))
})


test_that("reportCostsPresolve works", {
  expectEmptyReport(expectReportSucceeds(reportCostsPresolve))
})


test_that("reportCostsWholesale works", {
  expectEmptyReport(expectReportSucceeds(reportCostsWholesale))
})


test_that("reportCostsWithoutIncentives works", {
  expectValidReport(expectReportSucceeds(reportCostsWithoutIncentives))
})


test_that("reportCostTransport works", {
  expectValidReport(expectReportSucceeds(reportCostTransport))
})


test_that("reportCroparea works", {
  expectValidReport(expectReportSucceeds(reportCroparea, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportCroparea, detail = TRUE))
})


test_that("reportCropDiversity works", {
  expectValidReport(expectReportSucceeds(reportCropDiversity))
})
