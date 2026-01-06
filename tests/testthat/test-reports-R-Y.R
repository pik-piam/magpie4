test_that("reportRelativeHourlyLaborCosts works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportRelativeHourlyLaborCosts, fullDataName = !!fullDataName))
  }
})


test_that("reportRotationLength works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportRotationLength, fullDataName = !!fullDataName))
  }
})


test_that("reportRuralDemandShares works", {
  for (fullDataName in oldAndCurrentData()) {
    expectEmptyOrValidReport(expectReportSucceeds(reportRuralDemandShares, 
                                                  fullDataName = !!fullDataName,
                                                  type = "tradOnly"))
  }
})


test_that("reportSDG1 works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportSDG1, fullDataName = !!fullDataName))
  }
})


test_that("reportSDG12 works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportSDG12, fullDataName = !!fullDataName))
  }
})


test_that("reportSDG15 works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportSDG15, fullDataName = !!fullDataName))
  }
})


test_that("reportSDG2 works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportSDG2, fullDataName = !!fullDataName))
  }
})


test_that("reportSDG3 works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportSDG3, fullDataName = !!fullDataName))
  }
})


test_that("reportSDG6 works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportSDG6, fullDataName = !!fullDataName))
  }
})


test_that("reportSOM works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportSOM, fullDataName = !!fullDataName))
  }
})


test_that("reportTau works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportTau, fullDataName = !!fullDataName))
  }
})


test_that("reportTc works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportTc, fullDataName = !!fullDataName, level = "regglo"))
  }
})


test_that("reportTimber works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportTimber, fullDataName = !!fullDataName))
  }
})


test_that("reportTotalHoursWorked works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportTotalHoursWorked, fullDataName = !!fullDataName))
  }
})


test_that("reportTrade works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportTrade, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportTrade, fullDataName = !!fullDataName, detail = TRUE))
  }
})


test_that("reportValueMaterialDemand works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportValueMaterialDemand, fullDataName = !!fullDataName))
  }
})


test_that("reportValueTrade works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportValueTrade, fullDataName = !!fullDataName))
  }
})


test_that("reportVegfruitShare works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportVegfruitShare, fullDataName = !!fullDataName))
  }
})


test_that("reportWageDevelopment works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportWageDevelopment, fullDataName = !!fullDataName,  baseYear = 2000))
    expectValidReport(expectReportSucceeds(reportWageDevelopment, fullDataName = !!fullDataName,  baseYear = 2010))
    expectValidReport(expectReportSucceeds(reportWageDevelopment, fullDataName = !!fullDataName,  baseYear = 2020))
  }
})


test_that("reportWaterAvailability works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportWaterAvailability, fullDataName = !!fullDataName))
  }
})


test_that("reportWaterIndicators works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportWaterIndicators, fullDataName = !!fullDataName))
  }
})


test_that("reportWaterUsage works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportWaterUsage, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportWaterUsage, fullDataName = !!fullDataName, detail = TRUE))
  }
})


test_that("reportWorkingAgePopulation works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportWorkingAgePopulation, fullDataName = !!fullDataName))
  }
})


test_that("reportYields works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportYields,
                                           fullDataName = !!fullDataName,
                                           detail = FALSE, physical = TRUE))
    expectValidReport(expectReportSucceeds(reportYields,
                                           fullDataName = !!fullDataName,
                                           detail = FALSE, physical = FALSE))
    expectValidReport(expectReportSucceeds(reportYields,
                                           fullDataName = !!fullDataName,
                                           detail = TRUE, physical = TRUE))
    expectValidReport(expectReportSucceeds(reportYields,
                                           fullDataName = !!fullDataName,
                                           detail = TRUE, physical = FALSE))
  }
})


test_that("reportYieldsCropCalib works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportYieldsCropCalib, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportYieldsCropCalib, fullDataName = !!fullDataName, detail = TRUE))
  }
})


test_that("reportYieldsCropRaw works", {
  for (fullDataName in oldAndCurrentData()) {
    expectValidReport(expectReportSucceeds(reportYieldsCropRaw, fullDataName = !!fullDataName, detail = FALSE))
    expectValidReport(expectReportSucceeds(reportYieldsCropRaw, fullDataName = !!fullDataName, detail = TRUE))
  }
})
