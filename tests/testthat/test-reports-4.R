test_that("reportForestYield works", {
  expectReportSucceeds(reportForestYield)
})


test_that("reportharvested_area_timber works", {
  expectReportSucceeds(reportharvested_area_timber)
})


test_that("reportPlantationEstablishment works", {
  expectReportSucceeds(reportPlantationEstablishment)
})


test_that("reportRotationLength works", {
  expectReportSucceeds(reportRotationLength)
})


test_that("reportTimber works", {
  expectReportSucceeds(reportTimber)
})


test_that("reportBII works", {
  expectReportSucceeds(reportBII)
})


test_that("reportCropDiversity works", {
  expectReportSucceeds(reportCropDiversity)
})


test_that("reportPriceWoodyBiomass works", {
  expectReportSucceeds(reportPriceWoodyBiomass)
})


test_that("reportCarbonstock works", {
  expectReportSucceeds(reportCarbonstock)
})


test_that("reportGrasslandManagement works", {
  expectReportSucceeds(reportGrasslandManagement)
})


test_that("reportGrassStats works", {
  expectReportSucceeds(reportGrassStats)
})


test_that("reportGrasslandYields works", {
  expectReportSucceeds(reportGrasslandYields)
})


test_that("reportLSUGrasslands works", {
  expectReportSucceeds(reportLSUGrasslands)
})


test_that("reportAgEmployment works", {
  expectReportSucceeds(reportAgEmployment, type = 'absolute', detail = TRUE)
  expectReportSucceeds(reportAgEmployment, type = 'absolute', detail = FALSE)
  expectReportSucceeds(reportAgEmployment, type = 'share', detail = TRUE)
  expectReportSucceeds(reportAgEmployment, type = 'share', detail = FALSE)
})


test_that("reportHourlyLaborCosts works", {
  expectReportSucceeds(reportHourlyLaborCosts)
})


test_that("reportRelativeHourlyLaborCosts works", {
  expectReportSucceeds(reportRelativeHourlyLaborCosts)
})


test_that("reportTotalHoursWorked works", {
  expectReportSucceeds(reportTotalHoursWorked)
})


test_that("reportOutputPerWorker works", {
  expectReportSucceeds(reportOutputPerWorker)
})


test_that("reportValueMaterialDemand works", {
  expectReportSucceeds(reportValueMaterialDemand)
})


test_that("reportFactorCostShares works", {
  expectReportSucceeds(reportFactorCostShares, type = 'requirements')
  expectReportSucceeds(reportFactorCostShares, type = 'optimization')
  expectReportSucceeds(reportFactorCostShares, type = 'accounting')
})


test_that("reportWageDevelopment works", {
  expectReportSucceeds(reportWageDevelopment,  baseYear = 2000)
  expectReportSucceeds(reportWageDevelopment,  baseYear = 2010)
  expectReportSucceeds(reportWageDevelopment,  baseYear = 2020)
})


test_that("reportWaterIndicators works", {
  expectReportSucceeds(reportWaterIndicators)
})


test_that("reportBioplasticDemand works", {
  expectReportSucceeds(reportBioplasticDemand)
})


test_that("reportCostsMACCS works", {
  expectReportSucceeds(reportCostsMACCS)
})


test_that("reportLaborCostsEmpl works", {
  expectReportSucceeds(reportLaborCostsEmpl)
})


test_that("reportLaborProductivity works", {
  expectReportSucceeds(reportLaborProductivity)
})


test_that("reportRuralDemandShares works", {
  expectReportSucceeds(reportRuralDemandShares,  type = 'tradOnly')
})


test_that("reportCostsWholesale works", {
  expectReportSucceeds(reportCostsWholesale)
})


test_that("reportFit works", {
  expectReportSucceeds(reportFit, type = 'R2',level = 'grid')
  expectReportSucceeds(reportFit, type = 'R2', level = 'cell')
})


test_that("reportExtraResidueEmissions works", {
  expectReportSucceeds(reportExtraResidueEmissions, level = 'regglo')
})


test_that("reportFireEmissions works", {
  expectReportSucceeds(reportFireEmissions, level = 'regglo')
})
