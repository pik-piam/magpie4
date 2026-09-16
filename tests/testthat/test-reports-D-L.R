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


test_that("reportEmissions legacy yearly integrates to the cumulative series", {
  run_only_if_full_tests_requested()
  # The cumulative legacy series is the timestep-weighted integral of the yearly one (both at the model
  # timesteps), so integrating the yearly Legacy clearing lines reproduces the reported cumulative variables.
  gdx <- fullDataGdxPath("magpie-default")
  skip_if_not(file.exists(gdx), "gdx file not available")
  x   <- reportEmissions(gdx, legacyEmis = TRUE)
  reg <- if ("GLO" %in% getItems(x, dim = 1)) "GLO" else getItems(x, dim = 1)[1]
  w   <- as.numeric(m_yeardiff(gdx)[, getYears(x), ])   # model timestep widths (the weights the report itself uses)
  for (child in c("+|Legacy clearing", "Legacy clearing|+|Storage", "Legacy clearing|+|Release")) {
    y  <- as.numeric(x[reg, , paste0("Emissions|CO2|Land|Land-use Change|", child, " (Mt CO2/yr)")])
    cc <- as.numeric(x[reg, , paste0("Emissions|CO2|Land|Cumulative|Land-use Change|", child, " (Gt CO2)")])
    y[1] <- 0
    expect_lt(max(abs(cumsum(y * w) / 1000 - cc)), 1e-6)
  }
})


test_that("reportEmissions Gross Positive/Negative partition the net LUC flux", {
  run_only_if_full_tests_requested()
  # Gross Positive/Negative are INDEPENDENT sums of the source/sink children (mirroring the REMIND co2luc
  # Pos/Neg split), so their sum reproduces the net Land-use Change flux only when every signed child is
  # classified. A child added to the report but not grouped here breaks it - catching the inconsistency in
  # magpie4, not only downstream in the REMIND coupling. Positive stays a source, Negative a sink.
  vv <- function(x, v) as.vector(x[, , paste0("Emissions|CO2|Land|", v)])
  cases <- list(
    list(p = "Land-use Change|Gross Positive (Mt CO2/yr)",
         n = "Land-use Change|Gross Negative (Mt CO2/yr)",
         t = "+|Land-use Change (Mt CO2/yr)", tol = 1e-3),
    list(p = "Cumulative|Land-use Change|Gross Positive (Gt CO2)",
         n = "Cumulative|Land-use Change|Gross Negative (Gt CO2)",
         t = "Cumulative|+|Land-use Change (Gt CO2)", tol = 1e-6)
  )
  for (legacy in c(TRUE, FALSE)) {
    x <- expectValidReport(expectReportSucceeds(reportEmissions, legacyEmis = legacy))
    for (cs in cases) {
      pos <- vv(x, cs$p)
      neg <- vv(x, cs$n)
      net <- vv(x, cs$t)
      # sum of the two gross halves reproduces the net flux
      expect_lt(max(abs(pos + neg - net), na.rm = TRUE), cs$tol)
      # gross source is non-negative, gross sink is non-positive
      expect_gte(min(pos, na.rm = TRUE), -cs$tol)
      expect_lte(max(neg, na.rm = TRUE), cs$tol)
    }
  }
})


test_that("reportEmissions legacyEmis=FALSE emits a zero legacy reframe", {
  run_only_if_full_tests_requested()
  # Backward-compat: with legacyEmis=FALSE the reframe is off (raw instantaneous accounting). The legacy
  # net/Storage/Release lines are still emitted - exactly zero - so downstream mappings never break on a
  # missing variable, and the Land-use Change total is unchanged (the zero legacy child does not move it).
  x <- expectValidReport(expectReportSucceeds(reportEmissions, legacyEmis = FALSE))

  # (a) legacy-clearing net/Storage/Release lines are present but exactly zero (yearly and cumulative);
  # the "Excl Legacy clearing" memo stays gated off, so exclude it from this check.
  legacyVars <- grep("Legacy clearing", getNames(x), fixed = TRUE, value = TRUE)
  legacyVars <- legacyVars[!grepl("Excl Legacy clearing", legacyVars, fixed = TRUE)]
  expect_gt(length(legacyVars), 0)
  expect_lt(max(abs(x[, , legacyVars]), na.rm = TRUE), 1e-9)

  # (b) Land-use Change closes over its original (pre-legacy) children
  children <- paste0("Emissions|CO2|Land|Land-use Change|+|",
                     c("Deforestation", "Forest degradation", "Other land conversion", "Regrowth",
                       "Peatland", "Soil", "Residual", "Timber", "Wood Harvest"), " (Mt CO2/yr)")
  luc <- x[, , "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"]
  expect_lt(max(abs(luc - dimSums(x[, , children], dim = 3)), na.rm = TRUE), 1e-3)
})


test_that("legacyEmissions net flux reconciles with the pool stock", {
  run_only_if_full_tests_requested()
  # emission/stock consistency (Jan's review): the legacy net correction equals minus the change in the
  # slash-pool carbon stock, so the pool reported by reportCarbonstock reconciles the reframed emissions.
  gdx <- fullDataGdxPath("magpie-default")
  skip_if_not(file.exists(gdx), "gdx file not available")
  le  <- legacyEmissions(gdx, level = "glo", unit = "element")
  expect_true(all(c("legacy_net", "legacy_storage", "legacy_release", "legacy_stock") %in% getNames(le)))

  net   <- le[, , "legacy_net"]
  stock <- le[, , "legacy_stock"]
  # Restrict to the reported window: pre-firstYear priming years have net zeroed on purpose while the pool
  # keeps its primed build-up (the documented non-mass-conserving legacy credit), so the identity only holds
  # from the first reported clearing year onward.
  allYr     <- getYears(net, as.integer = TRUE)
  firstYear <- min(allYr[as.vector(dimSums(abs(net), dim = c(1, 3))) > 0])
  yr <- allYr[allYr >= firstYear]
  n  <- length(yr)
  # net[t] == stock[t] - stock[t+1] (forward change) at the returned annual temporal resolution, across the Koeppen sum
  lhs <- net[, yr[-n], ]
  rhs <- stock[, yr[-n], ] - setYears(stock[, yr[-1], ], yr[-n])
  expect_lt(max(abs(lhs - rhs)), 1e-6 * (max(abs(stock)) + 1))
  # net = storage + release, by construction
  expect_lt(max(abs(net - (le[, , "legacy_storage"] + le[, , "legacy_release"]))), 1e-6)
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
