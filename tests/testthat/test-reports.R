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


test_that("reportPriceFoodIndex works", {
  expectReportSucceeds(reportPriceFoodIndex, baseyear = 'y2010')
  expectReportSucceeds(reportPriceFoodIndex, baseyear = 'y2020')
})


test_that("reportProducerPriceIndex works", {
  expectReportSucceeds(reportProducerPriceIndex)
})


test_that("reportExpenditureFoodIndex works", {
  expectReportSucceeds(reportExpenditureFoodIndex)
})


test_that("reportPriceAgriculture works", {
  expectReportSucceeds(reportPriceAgriculture)
})


test_that("reportPriceBioenergy works", {
  expectReportSucceeds(reportPriceBioenergy)
})


test_that("reportPriceLand works", {
  expectReportSucceeds(reportPriceLand)
})


test_that("reportPriceWater works", {
  expectReportSucceeds(reportPriceWater)
})


test_that("reportValueTrade works", {
  expectReportSucceeds(reportValueTrade)
})


test_that("reportProcessing works", {
  expectReportSucceeds(reportProcessing, indicator = 'primary_to_process')
  expectReportSucceeds(reportProcessing, indicator = 'secondary_from_primary')

})


test_that("reportAEI works", {
  expectReportSucceeds(reportAEI)
})


test_that("reportWaterUsage works", {
  expectReportSucceeds(reportWaterUsage, detail = FALSE)
  expectReportSucceeds(reportWaterUsage, detail = TRUE)
})


test_that("reportWaterAvailability works", {
  expectReportSucceeds(reportWaterAvailability)
})


test_that("reportAAI works", {
  expectReportSucceeds(reportAAI)
})


test_that("reportSOM works", {
  expectReportSucceeds(reportSOM)
})


test_that("reportGrowingStock works", {
  expectReportSucceeds(reportGrowingStock, indicator = 'relative')
  expectReportSucceeds(reportGrowingStock, indicator = 'absolute')
})



test_that("reportSDG1 works", {
  expectReportSucceeds(reportSDG1)
})


test_that("reportSDG2 works", {
  expectReportSucceeds(reportSDG2)
})


test_that("reportSDG3 works", {
  expectReportSucceeds(reportSDG3)
})


test_that("reportSDG6 works", {
  expectReportSucceeds(reportSDG6)
})


test_that("reportSDG12 works", {
  expectReportSucceeds(reportSDG12)
})


test_that("reportSDG15 works", {
  expectReportSucceeds(reportSDG15)
})


test_that("reportPBwater works", {
  expectReportSucceeds(reportPBwater, level = 'regglo')
})


test_that("reportPBland works", {
  expectReportSucceeds(reportPBland, level = 'regglo')
})


test_that("reportPBbiosphere works", {
  expectReportSucceeds(reportPBbiosphere, level = 'regglo')
})


test_that("reportPBnitrogen works", {
  expectReportSucceeds(reportPBnitrogen, level = 'regglo')
})


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
