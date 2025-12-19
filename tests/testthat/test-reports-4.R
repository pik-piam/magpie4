test_that("reportForestYield works", {
  expectValidReport(expectReportSucceeds(reportForestYield))
})


test_that("reportharvested_area_timber works", {
  expectValidReport(expectReportSucceeds(reportharvested_area_timber))
})


test_that("reportPlantationEstablishment works", {
  expectValidReport(expectReportSucceeds(reportPlantationEstablishment))
})


test_that("reportRotationLength works", {
  expectValidReport(expectReportSucceeds(reportRotationLength))
})


test_that("reportTimber works", {
  expectValidReport(expectReportSucceeds(reportTimber))
})


test_that("reportBII works", {
  skip("Requires additional files, infrastructure needs to be extended to support this")
  expectValidReport(expectReportSucceeds(reportBII))
})


test_that("reportCropDiversity works", {
  expectValidReport(expectReportSucceeds(reportCropDiversity))
})


test_that("reportPriceWoodyBiomass works", {
  expectValidReport(expectReportSucceeds(reportPriceWoodyBiomass))
})


test_that("reportCarbonstock works", {
  expectValidReport(expectReportSucceeds(reportCarbonstock))
})


test_that("reportGrasslandManagement works", {
  expectDisabledReport(expectReportSucceeds(reportGrasslandManagement))
})


test_that("reportGrassStats works", {
  expectDisabledReport(expectReportSucceeds(reportGrassStats))
})


test_that("reportGrasslandYields works", {
  expectDisabledReport(expectReportSucceeds(reportGrasslandYields))
})


test_that("reportLSUGrasslands works", {
  expectDisabledReport(expectReportSucceeds(reportLSUGrasslands))
})


test_that("reportAgEmployment works", {
  expectValidReport(expectReportSucceeds(reportAgEmployment, type = "absolute", detail = TRUE))
  expectValidReport(expectReportSucceeds(reportAgEmployment, type = "absolute", detail = FALSE))
  expectValidReport(expectReportSucceeds(reportAgEmployment, type = "share", detail = TRUE))
  expectValidReport(expectReportSucceeds(reportAgEmployment, type = "share", detail = FALSE))
})


test_that("reportHourlyLaborCosts works", {
  expectValidReport(expectReportSucceeds(reportHourlyLaborCosts))
})


test_that("reportRelativeHourlyLaborCosts works", {
  expectValidReport(expectReportSucceeds(reportRelativeHourlyLaborCosts))
})


test_that("reportTotalHoursWorked works", {
  expectValidReport(expectReportSucceeds(reportTotalHoursWorked))
})


test_that("reportOutputPerWorker works", {
  expectValidReport(expectReportSucceeds(reportOutputPerWorker))
})


test_that("reportValueMaterialDemand works", {
  expectValidReport(expectReportSucceeds(reportValueMaterialDemand))
})


test_that("reportFactorCostShares works", {
  expectValidReport(expectReportSucceeds(reportFactorCostShares, type = "requirements"))
  expectValidReport(expectReportSucceeds(reportFactorCostShares, type = "optimization"))
  expectValidReport(expectReportSucceeds(reportFactorCostShares, type = "accounting"))
})


test_that("reportWageDevelopment works", {
  expectValidReport(expectReportSucceeds(reportWageDevelopment,  baseYear = 2000))
  expectValidReport(expectReportSucceeds(reportWageDevelopment,  baseYear = 2010))
  expectValidReport(expectReportSucceeds(reportWageDevelopment,  baseYear = 2020))
})


test_that("reportWaterIndicators works", {
  expectValidReport(expectReportSucceeds(reportWaterIndicators))
})


test_that("reportBioplasticDemand works", {
  expectValidReport(expectReportSucceeds(reportBioplasticDemand))
})


test_that("reportCostsMACCS works", {
  expectValidReport(expectReportSucceeds(reportCostsMACCS))
})


test_that("reportLaborCostsEmpl works", {
  expectValidReport(expectReportSucceeds(reportLaborCostsEmpl))
})


test_that("reportLaborProductivity works", {
  expectEmptyReport(expectReportSucceeds(reportLaborProductivity))
})


test_that("reportRuralDemandShares works", {
  expectEmptyReport(expectReportSucceeds(reportRuralDemandShares, type = "tradOnly"))
})


test_that("reportCostsWholesale works", {
  expectEmptyReport(expectReportSucceeds(reportCostsWholesale))
})


test_that("reportFit works", {
  skip("Requires additional files, infrastructure needs to be extended to support this")
  expectValidReport(expectReportSucceeds(reportFit, type = "R2", level = "grid"))
  expectValidReport(expectReportSucceeds(reportFit, type = "R2", level = "cell"))
})


test_that("reportExtraResidueEmissions works", {
  expectValidReport(expectReportSucceeds(reportExtraResidueEmissions, level = "regglo"))
})


test_that("reportFireEmissions works", {
  expectValidReport(expectReportSucceeds(reportFireEmissions, level = "regglo"))
})

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
