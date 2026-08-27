#' @title legacyEmissions
#' @description Reporting-side legacy-clearing tail for land-clearing CO2. MAgPIE books the aboveground carbon of
#' clearing (deforestation + other-land conversion) instantly; bookkeeping models (BLUE, OSCAR, Houghton &
#' Castanheira) spread it over a slash/deadwood decay tail. This re-spreads the pulse over a first-order-decay
#' (FOD) pool, as \code{\link{carbonLTS}} does for harvested wood products (HWP), and returns the net correction
#' to the raw flux, its Storage/Release parts, and the pool carbon stock.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file file name to write the output to with write.magpie
#' @param level aggregation level: "cell", "reg", "glo", "regglo", or any superAggregateX level
#' @param unit "element" (Mt C/yr) or "gas" (Mt CO2/yr)
#' @param cumulative report annually (FALSE) or cumulatively from \code{baseyear} (TRUE)
#' @param baseyear baseyear for cumulative emissions (default 1995)
#' @param priming assumed pre-firstYear clearing history that sets the pool's initial stock: "hist" (default, the
#' bookkeeping-ensemble LUC-CO2 shape), "ramp" (linear 0.3->1.0), "half" (flat 0.5x) or "peak" (flat 1x). The only
#' non-mass-conserving element - it credits pre-firstYear clearing MAgPIE never modelled (cf. carbonLTS's 1970 seed).
#' @param primingStart first year of the priming window (default 1850; results insensitive below ~1950).
#' @param harvestSlashFrac fraction of the wood-harvest aboveground flux added as dead slash (default 0, OFF);
#' a coarse sensitivity switch that does NOT net out the HWP carbon carbonLTS already defers.
#' @param a0Biome immediate/combustion fraction per Koeppen main group (A tropical, B arid, C temperate, D boreal,
#' E polar). Default A 0.25 / B 0.25 / C 0.20 / D 0.15 / E 0.15, from Houghton et al. 2012's clearing-year burn
#' partition (doi:10.5194/bg-9-5125-2012).
#' @param halfLifeBiome slash/deadwood decay half-life (years) per Koeppen group. Default A 4 / B 8 / C 10 / D 20 /
#' E 25, from coarse-woody-debris decay data (Chambers et al. 2000, Harmon et al. 2020, Russell et al. 2014).
#'
#' @return MAgPIE object (region x year x name) with "legacy_net" (delta to add to Land-use Change),
#' "legacy_storage" (<= 0, deferred emission), "legacy_release" (>= 0, decay outflow) and "legacy_stock"
#' (the slash-pool carbon level, incl. the primed initial stock). legacy_net = legacy_storage + legacy_release
#' = -diff(legacy_stock) by construction. Mt C or CO2 (per \code{unit}); cumulative fluxes are rebased to
#' \code{baseyear} as in \code{\link{carbonLTS}} (legacy_stock, a level, is returned only for cumulative = FALSE).
#'
#' @details The pulse is aboveground (vegc+litc) clearing CO2 only - deforestation + other-land conversion; soil
#' carbon and degradation are excluded. It is interpolated from MAgPIE's 5/10-yr steps to annual temporal resolution,
#' split by Koeppen group (fast decay in the tropics, slow in the boreal) with a separate pool per group, then summed;
#' the fraction a0 is released immediately and the rest (1-a0) decays with the group half-life (see the code for
#' the recursion). y1995 has no reported clearing flux, so the pulse starts at the first reported year, anchored to
#' the mean of the first few.
#'
#' Caveats: (1) reporting reframe only - the model still emits instantaneously and the carbon price is unchanged;
#' raw emissions stay available via legacyEmis = FALSE. (2) not mass-conserving: the priming credit is a one-off
#' initial pool; being a linear decay it is proportional to the priming amplitude, so half = exactly 0.5x peak and
#' the default hist sits near peak. (3) a residual scope gap to bookkeeping remains. The cumulative legacy series
#' is the timestep-weighted integral of the yearly one (both at the model timesteps); carbon still in the pool at
#' 2100 never enters the reported window.
#'
#' @author Florian Humpenoeder
#' @importFrom stats approx
#' @importFrom magclass time_interpolate
#' @importFrom memoise memoise
#' @examples
#' \dontrun{
#' x <- legacyEmissions(gdx)
#' }
#'
legacyEmissions <- function(gdx,
                            file = NULL,
                            level = "cell",
                            unit = "element",
                            cumulative = FALSE,
                            baseyear = 1995,
                            priming = "hist",
                            primingStart = 1850,
                            harvestSlashFrac = 0,
                            a0Biome = c(A = 0.25, B = 0.25, C = 0.20, D = 0.15, E = 0.15),
                            halfLifeBiome = c(A = 4, B = 8, C = 10, D = 20, E = 25)) {

  # The expensive emisCO2 read + FOD is done by the memoised core legacyClearingPool (shared with
  # reportCarbonstock); unit and cumulative are cheap wrapper transforms on the annual Mt C pool.
  legacy <- legacyClearingPool(gdx, level, priming, primingStart, harvestSlashFrac, a0Biome, halfLifeBiome)

  # cumulative transform at the MODEL timesteps (same convention as reportEmissions' raw emissions), so that
  # integrating the yearly legacy series with timestep weights reproduces the cumulative one. legacy_stock is a
  # level, so it is dropped in cumulative mode.
  if (cumulative) {
    legacy    <- legacy[, , "legacy_stock", invert = TRUE]
    timesteps <- m_yeardiff(gdx)
    legacy    <- legacy[, getYears(timesteps), ]
    legacy[, "y1995", ] <- 0
    legacy <- legacy * timesteps[, getYears(legacy), ]
    legacy <- as.magpie(apply(legacy, c(1, 3), cumsum))
    legacy <- legacy - setYears(legacy[, baseyear, ], NULL)
  }

  if (unit == "gas") {
    legacy <- legacy * 44 / 12
  }

  out(legacy, file)
}

#' @title legacyClearingPool
#' @description Memoised core of \code{\link{legacyEmissions}}: the expensive shared computation (cell-level
#' emisCO2 clearing read + Köppen first-order-decay convolution) returning the annual slash/deadwood pool at
#' \code{level} in Mt C (net/storage/release/stock). Both \code{\link{reportEmissions}} (fluxes) and
#' \code{\link{reportCarbonstock}} (stock) reach it through \code{legacyEmissions}, so memoising it (like
#' \code{\link{land}}) means repeated calls in one process share the result; a \code{\link{getReport}}
#' pre-warm additionally lets the parallel report workers inherit it copy-on-write. Cleared by
#' \code{\link{clearCacheMagpie4}}. Unit/cumulative transforms live in the \code{legacyEmissions} wrapper.
#' @param gdx GDX file
#' @param level aggregation level (see legacyEmissions)
#' @param priming,primingStart,harvestSlashFrac,a0Biome,halfLifeBiome see \code{\link{legacyEmissions}}
#' @return MAgPIE object (region x annual year x name), Mt C, with names legacy_net, legacy_storage,
#' legacy_release and legacy_stock
#' @author Florian Humpenoeder
#' @importFrom memoise memoise is.memoised forget
#' @keywords internal
legacyClearingPool <- memoise(function(gdx,
                                       level = "regglo",
                                       priming = "hist",
                                       primingStart = 1850,
                                       harvestSlashFrac = 0,
                                       a0Biome = c(A = 0.25, B = 0.25, C = 0.20, D = 0.15, E = 0.15),
                                       halfLifeBiome = c(A = 4, B = 8, C = 10, D = 20, E = 25)) {

  # Read the instantaneous clearing pulse (aboveground only), the SAME flux reportEmissions books 100% at the
  # year of clearing. Work in Mt C throughout (like carbonLTS); the wrapper converts to gas.
  co2 <- emisCO2(gdx, level = "cell", unit = "element", sum_land = FALSE, sum_cpool = FALSE)

  # other-land subset (matches reportEmissions' Other-land-conversion child)
  landItems <- getItems(co2, dim = "land")
  if ("other" %in% landItems) {
    otherSet <- "other"
  } else {
    otherSet <- c("other_othernat", "other_youngsecdf")
  }

  # Aboveground clearing pulse P(t): Deforestation (all pools) + Other-land conversion (otherSet), summed over
  # land types and c_pools; select "Above Ground Carbon" only (exclude soil).
  defo <- dimSums(co2[, , "lu_deforestation"][, , "Above Ground Carbon"], dim = c("land", "c_pools"))
  olc  <- dimSums(co2[, , "lu_other_conversion"][, , otherSet][, , "Above Ground Carbon"],
                  dim = c("land", "c_pools"))
  pulse <- collapseNames(defo) + collapseNames(olc)

  # optional dead-slash fraction of the wood-harvest aboveground flux (default OFF; see @param warning)
  if (harvestSlashFrac > 0) {
    harv  <- dimSums(co2[, , "lu_harvest"][, , "Above Ground Carbon"], dim = c("land", "c_pools"))
    pulse <- pulse + harvestSlashFrac * collapseNames(harv)
  }

  # First REPORTED clearing year (emisCO2 returns NA at the y1995 base year: no stock-difference flux). Drop
  # leading NA/zero years; anchor the pre-firstYear priming to the early-period clearing level.
  yearTotal <- as.numeric(dimSums(pulse, dim = 1))
  names(yearTotal) <- getYears(pulse)
  reported <- which(is.finite(yearTotal) & yearTotal > 0)
  if (length(reported) == 0) {
    # No positive clearing flux anywhere in the gdx (e.g. a degenerate no-clearing scenario): nothing to
    # defer, so return an all-zero correction over the model years. Keeps reportEmissions(legacyEmis = TRUE)
    # additive and crash-free rather than aborting the whole emissions report.
    message("legacyEmissions: no positive clearing flux in gdx - returning a zero legacy correction.")
    zero <- new.magpie(getCells(pulse), getYears(pulse),
                       c("legacy_net", "legacy_storage", "legacy_release", "legacy_stock"), fill = 0)
    if (level != "cell") {
      zero <- superAggregateX(zero, aggr_type = "sum", level = level)
    }
    return(zero)
  }
  firstYear   <- as.integer(sub("y", "", names(yearTotal)[reported[1]]))
  anchorYears <- names(yearTotal)[reported[seq_len(min(3, length(reported)))]]
  lastYear    <- 2150

  # Empirical historical-clearing prior for priming = "hist": bookkeeping-ensemble global LUC-CO2 (BLUE, OSCAR,
  # H&C2023, GCB), normalised to its 2000-2010 mean (= the model's early-period anchor), decadal 1850-2000. Same
  # record MAgPIE's LUC-CO2 cloud is compared against, so the prior is internally consistent with the comparison.
  histYear <- seq(1850, 2000, 10)
  histFrac <- c(0.33, 0.34, 0.41, 0.47, 0.57, 0.65, 0.68, 0.74, 0.86, 0.91, 1.04, 0.93, 0.85, 0.79, 0.94, 1.00)

  # FOD convolution of ONE cell-level sub-pulse: interpolate to annual, clamp negatives at cell level (the only
  # nonlinearity), aggregate to `level`, then run the linear priming + decay pool (exact at regglo, ~15x cheaper
  # than the cell recursion). Returns the legacy delta (priming years zeroed) plus the pool level legacy_stock.
  slashPool <- function(subPulse, a0, halfLife) {
    pc          <- subPulse[, getYears(subPulse, as.integer = TRUE) >= firstYear, ]
    pcAnchor    <- setYears(dimSums(pc[, anchorYears, ], dim = 2) / length(anchorYears), NULL)
    modelAnnual <- paste0("y", firstYear:lastYear)
    missing     <- setdiff(modelAnnual, getYears(pc))
    pcAnnual <- time_interpolate(pc, interpolated_year = missing,
                                 integrate_interpolated_years = TRUE, extrapolation_type = "linear")
    pcAnnual[pcAnnual < 0] <- 0
    pcAnnual <- pcAnnual[, modelAnnual, ]

    # aggregate to `level` AFTER the (nonlinear) clamp; everything below is linear, so this is exact
    pcAnnual <- aggregatePulse(pcAnnual)
    pcAnchor <- aggregatePulse(pcAnchor)

    primingYears <- paste0("y", primingStart:(firstYear - 1))
    fracVec <- switch(priming,
                      "hist" = approx(histYear, histFrac, xout = primingStart:(firstYear - 1), rule = 2)$y,
                      "peak" = rep(1.0, length(primingYears)),
                      "half" = rep(0.5, length(primingYears)),
                      "ramp" = seq(0.3, 1.0, length.out = length(primingYears)),
                      rep(0.5, length(primingYears)))
    primeMag <- new.magpie(getCells(pcAnnual), primingYears, getNames(pcAnnual), fill = 0)
    for (j in seq_along(primingYears)) {
      primeMag[, primingYears[j], ] <- fracVec[j] * pcAnchor
    }
    pcFull <- mbind(primeMag, pcAnnual)

    alpha       <- exp(-log(2) / halfLife)        # per-year retention (identical convention to carbonLTS)
    inflowToAdd <- (1 - a0) * pcFull              # slash-pool inflow (the deferred mass)
    stock <- pcFull
    stock[, , ] <- 0
    yrs <- getYears(stock)
    stopifnot(length(yrs) >= 2)
    for (i in 1:(length(yrs) - 1)) {
      stock[, i + 1, ] <- alpha * stock[, i, ] + inflowToAdd[, i, ]
    }
    outflow <- stock - alpha * stock              # = (1 - alpha) * stock
    net     <- -inflowToAdd + outflow

    # Invariant that keeps emissions and stocks consistent: the net delta equals minus the forward change in
    # the pool carbon stock, so reporting `stock` as a carbon line reconciles the two by construction.
    nY      <- length(yrs)
    stockT  <- stock[, yrs[-nY], ]
    stockT1 <- setYears(stock[, yrs[-1], ], yrs[-nY])
    stopifnot(max(abs(net[, yrs[-nY], ] - (stockT - stockT1))) < 1e-6 * (max(abs(pcFull)) + 1e-9))

    legacyDelta <- mbind(setNames(net,          "legacy_net"),
                         setNames(-inflowToAdd, "legacy_storage"),
                         setNames(outflow,      "legacy_release"))
    # priming years are pool-loading scaffold, not reported fluxes -> zero (leaves the primed in-window release
    # unmatched by in-window storage = the documented non-mass-conserving legacy credit). The pool level keeps
    # its primed years, so legacy_stock at firstYear is the inherited initial stock.
    legacyDelta[, getYears(legacyDelta, as.integer = TRUE) < firstYear, ] <- 0
    mbind(legacyDelta, setNames(stock, "legacy_stock"))
  }

  # Aggregate a cell-level sub-pulse to the requested level before the convolution (identity for level "cell").
  aggregatePulse <- function(subPulse) {
    if (level == "cell") {
      return(subPulse)
    }
    superAggregateX(subPulse, aggr_type = "sum", level = level)
  }

  # Split the clearing pulse by Koeppen main group and run a separate slash pool per group, then sum (decay is
  # fast in the tropics, slow in the boreal; a single global pool would mis-time the large tropical share).
  climateClass <- readGDX(gdx, "pm_climate_class", react = "silent")
  if (is.null(climateClass)) {
    # Fallback for a gdx without the Koeppen classification (should not occur for standard runs): a single
    # global bookkeeping-consistent pool (a0 = 0.2, half-life 8 yr). Message, not crash.
    message("legacyEmissions: pm_climate_class not in gdx - using a single global slash pool (a0=0.2, half-life=8 yr).")
    legacy <- slashPool(pulse, 0.2, 8)
  } else {
    grp <- substr(getItems(climateClass, dim = 3), 1, 1)   # first Koeppen letter = main group (A/B/C/D/E)
    legacy <- NULL
    for (g in unique(grp)) {
      if (!(g %in% names(halfLifeBiome)) || !(g %in% names(a0Biome))) {
        warning("legacyEmissions: no biome parameters for Koeppen group '", g, "'; skipped.")
        next
      }
      fracG    <- setYears(dimSums(climateClass[, , getItems(climateClass, dim = 3)[grp == g]], dim = 3), NULL)
      legacyG  <- slashPool(pulse * fracG, a0Biome[[g]], halfLifeBiome[[g]])
      legacy   <- if (is.null(legacy)) legacyG else legacy + legacyG
    }
  }

  # annual pool at `level`, Mt C; the wrapper applies unit + cumulative transforms
  legacy
})

# Pre-warm the memoised legacy pool in the current process (called by getReport before the parallel report
# fork): refresh the cache for THIS gdx, then fill it so forked workers inherit it copy-on-write instead of
# each recomputing the cell-level emisCO2 read. Wrapped in try() so a pre-warm failure never aborts the report.
warmLegacyPool <- function(gdx, level = "regglo") {
  if (is.memoised(legacyClearingPool)) {
    forget(legacyClearingPool)
  }
  try(invisible(legacyEmissions(gdx, level = level)), silent = TRUE)
}
