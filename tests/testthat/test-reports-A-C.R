test_that("reportAAI works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportAAI, fullDataName = !!fullDataName))
  }
})


test_that("reportAEI works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportAEI, fullDataName = !!fullDataName))
  }
})


test_that("reportAgEmployment works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportAgEmployment,
                                           fullDataName = !!fullDataName,
                                           type = "absolute", detail = TRUE))
    expectValidReport(expectReportSucceeds(reportAgEmployment,
                                           fullDataName = !!fullDataName,
                                           type = "absolute", detail = FALSE))
    expectValidReport(expectReportSucceeds(reportAgEmployment,
                                           fullDataName = !!fullDataName,
                                           type = "share", detail = TRUE))
    expectValidReport(expectReportSucceeds(reportAgEmployment,
                                           fullDataName = !!fullDataName,
                                           type = "share", detail = FALSE))
  }
})


test_that("reportAgGDP works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportAgGDP, fullDataName = !!fullDataName))
  }
})


test_that("reportAgriResearchIntensity works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportAgriResearchIntensity, fullDataName = !!fullDataName))
  }
})


test_that("reportAnthropometrics works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportAnthropometrics, fullDataName = !!fullDataName))
  }
})


test_that("reportBII works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportBII, fullDataName = !!fullDataName))
  }
})


test_that("reportBioplasticDemand works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportBioplasticDemand, fullDataName = !!fullDataName))
  }
})


test_that("reportCarbonstock works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCarbonstock, fullDataName = !!fullDataName))
  }
})


test_that("reportConsumVal works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportConsumVal, fullDataName = !!fullDataName))
  }
})


test_that("reportCostCapitalInvestment works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCostCapitalInvestment, fullDataName = !!fullDataName))
  }
})


test_that("reportCostCapitalStocks works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCostCapitalStocks, fullDataName = !!fullDataName))
  }
})


test_that("reportCostOverall works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCostOverall, fullDataName = !!fullDataName))
  }
})


test_that("reportCosts works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCosts, fullDataName = !!fullDataName))
  }
})


test_that("reportCostsAccounting works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCostsAccounting, fullDataName = !!fullDataName))
  }
})


test_that("reportCostsFertilizer works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCostsFertilizer, fullDataName = !!fullDataName))
  }
})


test_that("reportCostsInputFactors works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCostsInputFactors, fullDataName = !!fullDataName))
  }
})


test_that("reportCostsMACCS works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCostsMACCS, fullDataName = !!fullDataName))
  }
})


test_that("reportCostsPresolve works", {
  for (fullDataName in oldAndCurrentData()) {
    expectEmptyOrValidReport(expectReportSucceeds(reportCostsPresolve, fullDataName = !!fullDataName))
  }
})


test_that("reportCostsWholesale works", {
  for (fullDataName in oldAndCurrentData()) {
    expectEmptyOrValidReport(expectReportSucceeds(reportCostsWholesale, fullDataName = !!fullDataName))
  }
})


test_that("reportCostsWithoutIncentives works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCostsWithoutIncentives, fullDataName = !!fullDataName))
  }
})


test_that("reportCostTransport works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCostTransport, fullDataName = !!fullDataName))
  }
})


test_that("reportCroparea works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCroparea, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportCroparea, fullDataName = !!fullDataName, detail = TRUE))
  }
})


test_that("reportCropDiversity works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportCropDiversity, fullDataName = !!fullDataName))
  }
})
