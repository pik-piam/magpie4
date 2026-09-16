# The magpie4 report functions are tested in separate files in order to
# benefit from the testthat test parallelization, which works on a per-file
# basis. The separation into four files worked well at the time of creating
# the tests but may have to be changed in the future.

test_that("reportAAI works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAAI))
})


test_that("reportAEI works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAEI))
})


test_that("reportAgEmployment works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAgEmployment,
                                         type = "absolute", detail = TRUE))
  expectValidReport(expectReportSucceeds(reportAgEmployment,
                                         type = "absolute", detail = FALSE))
  expectValidReport(expectReportSucceeds(reportAgEmployment,
                                         type = "share", detail = TRUE))
  expectValidReport(expectReportSucceeds(reportAgEmployment,
                                         type = "share", detail = FALSE))
})


test_that("reportAgGDP works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAgGDP))
})


test_that("reportAgriResearchIntensity works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAgriResearchIntensity))
})


test_that("reportAnthropometrics works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportAnthropometrics))
})


test_that("reportBII works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportBII))
})


test_that("reportBiogasFeedstock works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportBiogasFeedstock))
})


test_that("reportBioplasticDemand works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportBioplasticDemand))
})


test_that("reportCarbonstock works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCarbonstock))
})


test_that("reportCarbonstock legacy pool is an additive carbon child", {
  run_only_if_full_tests_requested()
  pool <- "Resources|Carbon|+|Legacy clearing pool (Mt C)"

  # legacyEmis = TRUE: pool is a + child and Resources|Carbon closes over all four + children.
  x <- expectValidReport(expectReportSucceeds(reportCarbonstock, legacyEmis = TRUE))
  expect_true(pool %in% getNames(x))
  children <- paste0("Resources|Carbon|+|",
                     c("Soil", "Litter", "Vegetation", "Legacy clearing pool"), " (Mt C)")
  totalT <- collapseNames(x[, , "Resources|Carbon (Mt C)"])
  expect_lt(max(abs(totalT - dimSums(x[, , children], dim = 3)), na.rm = TRUE), 1e-6)

  # legacyEmis = FALSE: no pool child; total closes over the three model-native pools (backward compatible).
  y <- expectValidReport(expectReportSucceeds(reportCarbonstock, legacyEmis = FALSE))
  expect_false(pool %in% getNames(y))
  base <- paste0("Resources|Carbon|+|", c("Soil", "Litter", "Vegetation"), " (Mt C)")
  totalF <- collapseNames(y[, , "Resources|Carbon (Mt C)"])
  expect_lt(max(abs(totalF - dimSums(y[, , base], dim = 3)), na.rm = TRUE), 1e-6)

  # the pool inflates the total by exactly the pool (reframe made visible in the headline total).
  expect_lt(max(abs(totalT - totalF - collapseNames(x[, , pool])), na.rm = TRUE), 1e-6)
})


test_that("reportConsumVal works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportConsumVal))
})


test_that("reportCostCapitalInvestment works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostCapitalInvestment))
})


test_that("reportCostCapitalStocks works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostCapitalStocks))
})


test_that("reportCostOverall works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostOverall))
})


test_that("reportCosts works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCosts))
})


test_that("reportCostsAccounting works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostsAccounting))
})


test_that("reportCostsFertilizer works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostsFertilizer))
})


test_that("reportCostsInputFactors works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostsInputFactors))
})


test_that("reportCostsMACCS works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostsMACCS))
})


test_that("reportCostsPresolve works", {
  run_only_if_full_tests_requested()
  expectEmptyOrValidReport(expectReportSucceeds(reportCostsPresolve))
})


test_that("reportCostsWholesale works", {
  run_only_if_full_tests_requested()
  expectEmptyOrValidReport(expectReportSucceeds(reportCostsWholesale))
})


test_that("reportCostsWithoutIncentives works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostsWithoutIncentives))
})


test_that("reportCostTransport works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCostTransport))
})


test_that("reportCroparea works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCroparea, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportCroparea, detail = TRUE))
})


test_that("reportCropDiversity works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCropDiversity))
})


test_that("reportCropResidues2ndBE works", {
  skip("requires too much memory currently")
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportCropResidues2ndBE))
})
