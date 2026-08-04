test_that("reportDemand works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportDemand,
                                         detail = FALSE, level = "regglo"))
  expectValidReport(expectReportSucceeds(reportDemand,
                                         detail = TRUE, level = "regglo"))
})


test_that("reportDemandBioenergy works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportDemandBioenergy, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportDemandBioenergy, detail = TRUE))
})


test_that("reportEmissions works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportEmissions))
})


test_that("reportEmissions legacy clearing is additive", {
  run_only_if_full_tests_requested()
  # legacyEmis = TRUE folds a legacy-clearing correction into Land-use Change.
  # expectReportSucceeds wraps in expect_no_warning, so a broken additivity check fails here too.
  x <- expectValidReport(expectReportSucceeds(reportEmissions, legacyEmis = TRUE))

  net <- "Emissions|CO2|Land|Land-use Change|+|Legacy clearing (Mt CO2/yr)"
  sto <- "Emissions|CO2|Land|Land-use Change|Legacy clearing|+|Storage (Mt CO2/yr)"
  rel <- "Emissions|CO2|Land|Land-use Change|Legacy clearing|+|Release (Mt CO2/yr)"
  expect_true(net %in% getNames(x))
  # net = storage + release, by construction
  expect_lt(max(abs(x[, , net] - (x[, , sto] + x[, , rel])), na.rm = TRUE), 1e-6)

  # Land-use Change closes over its + children including the new legacy clearing child
  children <- paste0("Emissions|CO2|Land|Land-use Change|+|",
                     c("Deforestation", "Forest degradation", "Other land conversion", "Regrowth",
                       "Peatland", "Soil", "Residual", "Timber", "Wood Harvest",
                       "Legacy clearing"), " (Mt CO2/yr)")
  luc <- x[, , "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"]
  expect_lt(max(abs(luc - dimSums(x[, , children], dim = 3)), na.rm = TRUE), 1e-3)
})


test_that("reportEmissions legacyEmis=FALSE omits the legacy reframe", {
  run_only_if_full_tests_requested()
  # Backward-compat: with legacyEmis=FALSE the legacy-clearing reframe is fully disabled, so
  # output is the raw instantaneous accounting - no legacy variables are emitted and the
  # Land-use Change total closes over its original children (no legacy correction folded in).
  x <- expectValidReport(expectReportSucceeds(reportEmissions, legacyEmis = FALSE))

  # (a) no legacy-clearing variables at all (yearly or cumulative)
  expect_false(any(grepl("Legacy clearing", getNames(x), fixed = TRUE)))

  # (b) Land-use Change closes over its original (pre-legacy) children
  children <- paste0("Emissions|CO2|Land|Land-use Change|+|",
                     c("Deforestation", "Forest degradation", "Other land conversion", "Regrowth",
                       "Peatland", "Soil", "Residual", "Timber", "Wood Harvest"), " (Mt CO2/yr)")
  luc <- x[, , "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"]
  expect_lt(max(abs(luc - dimSums(x[, , children], dim = 3)), na.rm = TRUE), 1e-3)
})


test_that("reportEmissionsBeforeTechnicalMitigation works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportEmissionsBeforeTechnicalMitigation))
})


test_that("reportExpenditureFoodIndex works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportExpenditureFoodIndex))
})


test_that("reportExtraResidueEmissions works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportExtraResidueEmissions,
                                         level = "regglo"))
})


test_that("reportFactorCostShares works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFactorCostShares,
                                         type = "requirements"))
  expectValidReport(expectReportSucceeds(reportFactorCostShares,
                                         type = "optimization"))
  expectValidReport(expectReportSucceeds(reportFactorCostShares,
                                         type = "accounting"))
})


test_that("reportFeed works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFeed, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportFeed, detail = TRUE))
})


test_that("reportFeedConversion works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFeedConversion))
})


test_that("reportFireEmissions works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFireEmissions, level = "regglo"))
})


test_that("reportFit works", {
  run_only_if_full_tests_requested()
  for (type in c("R2", "MAE", "MPE", "MAPE")) {
    for (level in c("grid", "cell")) {
      expectValidReport(expectReportSucceeds(reportFit, type = !!type, level = !!level))
    }
  }
})


test_that("reportFoodExpenditure works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportFoodExpenditure))
})


test_that("reportForestYield works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportForestYield))
})


test_that("reportGrowingStock works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportGrowingStock, indicator = "relative"))
  expectValidReport(expectReportSucceeds(reportGrowingStock, indicator = "absolute"))
})


test_that("reportharvested_area_timber works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportharvested_area_timber))
})


test_that("reportHourlyLaborCosts works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportHourlyLaborCosts))
})


test_that("reportIncome works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportIncome, type = "ppp"))
  expectValidReport(expectReportSucceeds(reportIncome, type = "mer"))

})

test_that("reportIntakeDetailed works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportIntakeDetailed, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportIntakeDetailed, detail = TRUE))
})


test_that("reportKcal works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportKcal, detail = FALSE))
  expectValidReport(expectReportSucceeds(reportKcal, detail = TRUE))
})


test_that("reportLaborCostsEmpl works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLaborCostsEmpl))
})


test_that("reportLaborProductivity works", {
  run_only_if_full_tests_requested()
  expectEmptyOrValidReport(expectReportSucceeds(reportLaborProductivity))
})


test_that("reportLandConservation works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLandConservation))
})


test_that("reportLandUse works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLandUse, level = "regglo"))
})


test_that("reportLandUseChange works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLandUseChange))
})


test_that("reportLivestockDemStructure works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLivestockDemStructure))
})


test_that("reportLivestockShare works", {
  run_only_if_full_tests_requested()
  expectValidReport(expectReportSucceeds(reportLivestockShare))
})
