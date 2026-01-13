test_that("reportRelativeHourlyLaborCosts works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportRelativeHourlyLaborCosts, fullDataName = !!fullDataName))
})


test_that("reportRotationLength works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportRotationLength, fullDataName = !!fullDataName))
})


test_that("reportRuralDemandShares works", {
  run_only_if_full_tests_requested()
  expectEmptyOrValidReport(expectReportSucceeds(reportRuralDemandShares,
                                                fullDataName = !!fullDataName,
                                                type = "tradOnly"))
})


test_that("reportSDG1 works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportSDG1, fullDataName = !!fullDataName))
})


test_that("reportSDG12 works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportSDG12, fullDataName = !!fullDataName))
})


test_that("reportSDG15 works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportSDG15, fullDataName = !!fullDataName))
})


test_that("reportSDG2 works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportSDG2, fullDataName = !!fullDataName))
})


test_that("reportSDG3 works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportSDG3, fullDataName = !!fullDataName))
})


test_that("reportSDG6 works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportSDG6, fullDataName = !!fullDataName))
})


test_that("reportSOM works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportSOM, fullDataName = !!fullDataName))
})


test_that("reportTau works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportTau, fullDataName = !!fullDataName))
})


test_that("reportTc works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportTc, fullDataName = !!fullDataName, level = "regglo"))
})


test_that("reportTimber works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportTimber, fullDataName = !!fullDataName))
})


test_that("reportTotalHoursWorked works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportTotalHoursWorked, fullDataName = !!fullDataName))
})


test_that("reportTrade works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportTrade, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportTrade, fullDataName = !!fullDataName, detail = TRUE))
})


test_that("reportValueMaterialDemand works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportValueMaterialDemand, fullDataName = !!fullDataName))
})


test_that("reportValueTrade works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportValueTrade, fullDataName = !!fullDataName))
})


test_that("reportVegfruitShare works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportVegfruitShare, fullDataName = !!fullDataName))
})


test_that("reportWageDevelopment works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportWageDevelopment, fullDataName = !!fullDataName,  baseYear = 2000))
  expectValidReport(expectReportSucceeds(reportWageDevelopment, fullDataName = !!fullDataName,  baseYear = 2010))
  expectValidReport(expectReportSucceeds(reportWageDevelopment, fullDataName = !!fullDataName,  baseYear = 2020))
})


test_that("reportWaterAvailability works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportWaterAvailability, fullDataName = !!fullDataName))
})


test_that("reportWaterIndicators works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportWaterIndicators, fullDataName = !!fullDataName))
})


test_that("reportWaterUsage works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportWaterUsage, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportWaterUsage, fullDataName = !!fullDataName, detail = TRUE))
})


test_that("reportWorkingAgePopulation works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportWorkingAgePopulation, fullDataName = !!fullDataName))
})


test_that("reportYields works", {
  run_only_if_full_tests_requested()
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
})


test_that("reportYieldsCropCalib works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportYieldsCropCalib, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportYieldsCropCalib, fullDataName = !!fullDataName, detail = TRUE))
})


test_that("reportYieldsCropRaw works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportYieldsCropRaw, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportYieldsCropRaw, fullDataName = !!fullDataName, detail = TRUE))
})
