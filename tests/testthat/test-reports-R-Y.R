test_that("reportRelativeHourlyLaborCosts works", {
  expectValidReport(expectReportSucceeds(reportRelativeHourlyLaborCosts))
})


test_that("reportRotationLength works", {
  expectValidReport(expectReportSucceeds(reportRotationLength))
})


test_that("reportRuralDemandShares works", {
  expectEmptyOrValidReport(expectReportSucceeds(reportRuralDemandShares, type = "tradOnly"))
})


test_that("reportSDG1 works", {
  expectValidReport(expectReportSucceeds(reportSDG1))
})


test_that("reportSDG12 works", {
  expectValidReport(expectReportSucceeds(reportSDG12))
})


test_that("reportSDG15 works", {
  expectValidReport(expectReportSucceeds(reportSDG15))
})


test_that("reportSDG2 works", {
  expectValidReport(expectReportSucceeds(reportSDG2))
})


test_that("reportSDG3 works", {
  expectValidReport(expectReportSucceeds(reportSDG3))
})


test_that("reportSDG6 works", {
  expectValidReport(expectReportSucceeds(reportSDG6))
})


test_that("reportSOM works", {
  expectValidReport(expectReportSucceeds(reportSOM))
})


test_that("reportTau works", {
  expectValidReport(expectReportSucceeds(reportTau))
})


test_that("reportTc works", {
  expectValidReport(expectReportSucceeds(reportTc, level = "regglo"))
})


test_that("reportTimber works", {
  expectValidReport(expectReportSucceeds(reportTimber))
})


test_that("reportTotalHoursWorked works", {
  expectValidReport(expectReportSucceeds(reportTotalHoursWorked))
})


test_that("reportTrade works", {
  expectValidReport(expectReportSucceeds(reportTrade, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportTrade, detail = TRUE))
})


test_that("reportValueMaterialDemand works", {
  expectValidReport(expectReportSucceeds(reportValueMaterialDemand))
})


test_that("reportValueTrade works", {
  expectValidReport(expectReportSucceeds(reportValueTrade))
})


test_that("reportVegfruitShare works", {
  expectValidReport(expectReportSucceeds(reportVegfruitShare))
})


test_that("reportWageDevelopment works", {
  expectValidReport(expectReportSucceeds(reportWageDevelopment,  baseYear = 2000))
  expectValidReport(expectReportSucceeds(reportWageDevelopment,  baseYear = 2010))
  expectValidReport(expectReportSucceeds(reportWageDevelopment,  baseYear = 2020))
})


test_that("reportWaterAvailability works", {
  expectValidReport(expectReportSucceeds(reportWaterAvailability))
})


test_that("reportWaterIndicators works", {
  expectValidReport(expectReportSucceeds(reportWaterIndicators))
})


test_that("reportWaterUsage works", {
  expectValidReport(expectReportSucceeds(reportWaterUsage, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportWaterUsage, detail = TRUE))
})


test_that("reportWorkingAgePopulation works", {
  expectValidReport(expectReportSucceeds(reportWorkingAgePopulation))
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
