# The magpie4 report functions are tested in separate files in order to
# benefit from the testthat test parallelization, which works on a per-file
# basis. The separation into four files worked well at the time of creating
# the tests but may have to be changed in the future.

test_that("reportAAI works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAAI, fullDataName = !!fullDataName))
})


test_that("reportAEI works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAEI, fullDataName = !!fullDataName))
})


test_that("reportAgEmployment works", {
  run_only_if_full_tests_requested()
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
})


test_that("reportAgGDP works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAgGDP, fullDataName = !!fullDataName))
})


test_that("reportAgriResearchIntensity works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAgriResearchIntensity, fullDataName = !!fullDataName))
})


test_that("reportAnthropometrics works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAnthropometrics, fullDataName = !!fullDataName))
})


test_that("reportBII works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportBII, fullDataName = !!fullDataName))
})


test_that("reportBioplasticDemand works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportBioplasticDemand, fullDataName = !!fullDataName))
})


test_that("reportCarbonstock works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCarbonstock, fullDataName = !!fullDataName))
})


test_that("reportConsumVal works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportConsumVal, fullDataName = !!fullDataName))
})


test_that("reportCostCapitalInvestment works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostCapitalInvestment, fullDataName = !!fullDataName))
})


test_that("reportCostCapitalStocks works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostCapitalStocks, fullDataName = !!fullDataName))
})


test_that("reportCostOverall works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostOverall, fullDataName = !!fullDataName))
})


test_that("reportCosts works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCosts, fullDataName = !!fullDataName))
})


test_that("reportCostsAccounting works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostsAccounting, fullDataName = !!fullDataName))
})


test_that("reportCostsFertilizer works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostsFertilizer, fullDataName = !!fullDataName))
})


test_that("reportCostsInputFactors works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostsInputFactors, fullDataName = !!fullDataName))
})


test_that("reportCostsMACCS works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostsMACCS, fullDataName = !!fullDataName))
})


test_that("reportCostsPresolve works", {
  run_only_if_full_tests_requested()
  expectEmptyOrValidReport(expectReportSucceeds(reportCostsPresolve, fullDataName = !!fullDataName))
})


test_that("reportCostsWholesale works", {
  run_only_if_full_tests_requested()
  expectEmptyOrValidReport(expectReportSucceeds(reportCostsWholesale, fullDataName = !!fullDataName))
})


test_that("reportCostsWithoutIncentives works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostsWithoutIncentives, fullDataName = !!fullDataName))
})


test_that("reportCostTransport works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostTransport, fullDataName = !!fullDataName))
})


test_that("reportCroparea works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCroparea, fullDataName = !!fullDataName, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportCroparea, fullDataName = !!fullDataName, detail = TRUE))
})


test_that("reportCropDiversity works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCropDiversity, fullDataName = !!fullDataName))
})
