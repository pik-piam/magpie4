#' @title legacyEmissions
#' @description Reporting-side "legacy clearing tail" for land-clearing CO2. MAgPIE books the
#' aboveground carbon of land clearing (deforestation + other-land conversion to agriculture) 100%
#' instantaneously in the year of clearing, whereas bookkeeping models (BLUE, OSCAR, Houghton & Castanheira)
#' carry a Houghton-style legacy slash-and-deadwood decay tail. This function re-spreads the instantaneous
#' clearing pulse over a first-order-decay (FOD) slash/deadwood pool, mirroring the harvested-wood-products
#' (HWP) convolution in \code{\link{carbonLTS}}. It returns ONLY the net correction ("delta") to be added
#' on top of the raw (unchanged) instantaneous flux, plus its Storage/Release parts.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any aggregation level defined in superAggregateX
#' @param unit "element" or "gas"; "element": Mt C/yr; "gas": Mt CO2/yr
#' @param cumulative Logical; report the legacy correction annually (FALSE) or cumulative (TRUE).
#' The starting point for cumulative emissions is y1995.
#' @param baseyear Baseyear used for cumulative emissions (default = 1995)
#' @param priming Pre-firstYear clearing history used to load the decay tail. One of "hist" (default; an
#' EMPIRICAL historical-clearing prior - the bookkeeping-ensemble global LUC-CO2 (BLUE, OSCAR, H&C2023, GCB),
#' normalised to its 2000-2010 mean and interpolated onto the priming window: rises from ~0.33x (1850) to
#' ~0.65x (1900), plateaus near 1x from the 1930s with a 1960-80 dip - i.e. the SAME sources MAgPIE's LUC-CO2
#' cloud is compared against, so the prior is internally consistent with the comparison), "ramp" (the older
#' stylised linear 0.3 -> 1.0, which under-states early-20th-C clearing), "half" (flat 0.5x, conservative low
#' bound) or "peak" (flat 1x, aggressive upper bound). This priming is the ONLY non-mass-conserving element
#' (see details): it credits real pre-firstYear clearing MAgPIE never modelled. NOTE: it is a pre-firstYear
#' LUC-HISTORY assumption argued from the bookkeeping/deforestation-history record, NOT tuned to a comparison
#' cloud. "hist" gives a larger, flatter early tail than "ramp" (real early clearing was higher).
#' @param primingStart First year of the pre-firstYear priming window (default 1850, matching the empirical
#' "hist" record; earlier is clamped to the 1850 level). The result is insensitive to this below ~1950 (with
#' slash half-lives of ~4-20 yr, pre-1955 clearing carries little weight by 2020), so "how many years back" is
#' not a real tuning knob.
#' @param harvestSlashFrac Fraction of the wood-harvest aboveground flux added to the pulse as dead on-site
#' slash. Default 0 (harvest excluded). WARNING: enabling this is the riskiest term - the merchantable
#' product fraction is ALREADY deferred by carbonLTS (HWP) and woodfuel is released immediately by design,
#' so only the genuinely-dead residue with no standing-regrowth counterpart is poolable. A defensible
#' enabled value (~0.15-0.30 of gross harvest) must first net out the HWP-captured product carbon; the raw
#' term implemented here does NOT net it and is provided only as a coarse sensitivity switch.
#' @param a0Biome Named numeric over Koeppen main groups (A tropical, B arid, C temperate, D boreal/
#' continental, E polar): immediate/combustion fraction per group. The clearing pulse is ALWAYS split by
#' Koeppen main group (from \code{pm_climate_class} in the gdx) and a SEPARATE first-order-decay pool is run
#' per group with group-specific (a0Biome, halfLifeBiome), then summed - the FOD is linear, so this is an exact
#' "mixture of exponentials". This resolves the fact that deadwood/coarse-woody-debris decay is fast in the
#' tropics and slow in the boreal, so a single global half-life would mis-time the ~60 percent tropical share
#' of clearing. (If a gdx lacks \code{pm_climate_class}, a single global fallback pool a0=0.2 / half-life=8 yr
#' is used with a message.)
#' Default A 0.25 / B 0.25 / C 0.20 / D 0.15 / E 0.15. Anchored to Houghton et al. 2012's bookkeeping
#' clearing-year partition (~0.20 of total cleared aboveground C burned immediately, ~0.70 to the decaying
#' slash pool; doi:10.5194/bg-9-5125-2012), graded by biome per deforestation-fire evidence: tropical raised
#' (slash-and-burn; GFED killed-biomass combustion completeness higher, van der Werf et al. 2010), boreal
#' lowered (stem/bole carbon largely survives stand-replacing fire, de Groot et al. 2009). NOT plot-scale
#' combustion completeness (~0.5-0.6, burned-fuel-only denominator) - this is the whole-cleared-stand fraction.
#' @param halfLifeBiome Named numeric over Koeppen main groups: slash/deadwood FOD half-life (years) per
#' group. Default A 4 / B 8 / C 10 / D 20 / E 25 yr, anchored to
#' coarse-woody-debris decomposition data (tropical: Chambers et al. 2000 Amazon k~0.19/yr -> ~4 yr; temperate:
#' Russell et al. 2014 / Mackensen et al. 2003 / Harmon et al. 2020 ~10-18 yr; boreal: Yatskov et al. 2003 /
#' Shorohova & Kapitsa 2014 ~14-50 yr large-bole, shaded down for the fine+coarse slash mix), Q10~2.5
#' temperature scaling (Harmon et al. 2020). Deliberately SHORTER than a composite bookkeeping tail (whose
#' multi-decadal part is product- and soil-pool dominated, both handled elsewhere here); brackets BLUE's slash
#' e-folding (5-15 yr) and H&C2023's ~5-8 yr temperate slash. Boreal (20 yr) is the least constrained (range
#' 15-30). Anchors adversarially cross-checked (Crossref/OpenAlex); Chambers 2000, Harmon 2020 and
#' Russell 2014 are the load-bearing half-life references.
#'
#' @return MAgPIE object (region x year x name) with three variables: "legacy_net" (the delta to add to
#' Land-use Change), "legacy_storage" (= minus the pool inflow, <= 0, deferred emission) and
#' "legacy_release" (= FOD outflow, >= 0). legacy_net = legacy_storage + legacy_release by
#' construction. Annual values in Mt (C or CO2, per \code{unit}); for \code{cumulative = TRUE} cumulated-Mt
#' rebased to \code{baseyear} (the caller divides by 1000 for Gt), exactly as \code{\link{carbonLTS}}.
#'
#' @details The pulse P(t) is the instantaneous ABOVEGROUND (vegc+litc) clearing CO2 that MAgPIE currently
#' books at 100 percent in year t: \code{lu_deforestation} (all forest/tree pools) plus
#' \code{lu_other_conversion} (over the other-land subset), read the SAME way \code{reportEmissions} reads
#' its Deforestation and Other-land-conversion children - EXCEPT that only the "Above Ground Carbon" pool
#' is taken (the reported lines sum above+below ground). Soil carbon (below-ground) is deliberately EXCLUDED
#' - it is already released gradually by module 59 and reported separately as \code{lu_som}; convolving it
#' here would double-delay it. Forest degradation is EXCLUDED - it is a land-neutral clear-and-regrow cycle
#' whose gross emission is matched by a deepened Regrowth line, so convolving one side without the other
#' would inject a spurious tail (and it is flat, so the trend correction is ~0 anyway).
#'
#' The MAgPIE base year (y1995) carries no reported clearing flux (emisCO2 masks year 1 to NA), so the
#' pulse is taken from the first REPORTED clearing year onward, and the pre-1995 priming level is anchored
#' to the mean of the first up-to-three reported model years (a representative early-period clearing level,
#' robust to a single anomalous year), NOT the base-year value. NB: a genuine y1995 gross clearing flux IS
#' recoverable (deforestation+OLC = reduction x age-density, only masked by emisCO2's year-1 NA line), but
#' it is a base-year UNDER-estimate (~15 percent of the y2000 pulse) because y1995 is the calibration
#' snapshot and the 1-yr first step reduces almost no forest/other area - so it is NOT a good priming
#' anchor. The alternative "y1995 = solved stock minus initial carbon stock" recovers the model's NET y1995
#' emission, which MAgPIE itself flags as "not meaningful" (it is dominated by the aboveground-only init vs
#' soil-inclusive solved-stock pool-scope mismatch, plus the age-resolved-vs-mature density convention),
#' NOT a clearing flux - hence the early-period mean is the more defensible pre-1995 proxy.
#'
#' FOD pool (IPCC first-order decay, as in \code{\link{carbonLTS}}): alpha = exp(-ln2/halfLife); the pool
#' inflow is the deferred fraction (1 - a0) * P (mass-conserving beta = 1, so a0 is exactly the
#' immediately-released fraction and the pool eventually releases the full deferred mass - this is a
#' deliberate divergence from carbonLTS's within-year beta = (1-alpha)/k HWP discretisation). The recursion
#' stock[t+1] = alpha * stock[t] + (1 - a0) * P[t] runs on an ANNUAL grid (the model pulse is
#' time-interpolated to annual first, because alpha is a per-year retention); outflow(t) = (1-alpha)*stock(t).
#' The reframed booking is a0*P + outflow, so the delta reported here is
#' legacy_net = outflow - (1 - a0) * P. In cumulative mode the pool is integrated on the annual grid
#' (like carbonLTS's Timber child) then rebased to y1995; do not "correct" this to model-timestep weights.
#'
#' HONEST CAVEATS: (1) This is a REPORTING reframe only - the model still emits the pulse instantaneously and
#' the carbon price still acts on the instantaneous flux; nothing in the optimisation changes. (2) It is NOT
#' mass-conserving: the pre-1995 priming injects a legacy decay tail (a "legacy credit" of ~+15 percent
#' [ramp, default] of cumulative 1995-2100 LUC, bracketed ~+6 percent [half] to ~+21 percent [peak]) that
#' credits real clearing MAgPIE never modelled - analogous to the 1970 seed in carbonLTS's HWP pool. (3) The kernel (a0, halfLife) and priming
#' MUST be sourced from bookkeeping/LUC-history response functions, NOT tuned to any comparison cloud; keep
#' the RAW instantaneous flux as the default so the choice stays auditable. (4) It is necessary-but-not-
#' sufficient: a residual scope/naming gap remains (MAgPIE "Deforestation" ~250 vs bookkeeping ~7000-9000
#' Mt CO2/yr), which this reframe reshapes the trend of but cannot close.
#'
#' @author Florian Humpenoeder
#' @importFrom magclass dimSums collapseNames setYears setNames getYears getNames getItems getCells
#' new.magpie mbind as.magpie time_interpolate
#' @importFrom stats approx
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

  # ---------------------------------------------------------------------------------------------------
  # Read the instantaneous clearing pulse (aboveground only), the SAME flux reportEmissions books 100%
  # at the year of clearing. Work in Mt C throughout (like carbonLTS) and convert to gas at the end.
  # Read at cell level and aggregate LAST (FOD is linear, so GLO == sum of regions/cells).
  co2 <- emisCO2(gdx, level = "cell", unit = "element", sum_land = FALSE, sum_cpool = FALSE)

  # other-land subset (matches reportEmissions' Other-land-conversion child)
  landItems <- getItems(co2, dim = "land")
  if ("other" %in% landItems) {
    otherSet <- "other"
  } else {
    otherSet <- c("other_othernat", "other_youngsecdf")
  }

  # Aboveground clearing pulse P(t): Deforestation (all pools) + Other-land conversion (otherSet),
  # summed over land types and c_pools; select "Above Ground Carbon" only (exclude soil).
  defo <- dimSums(co2[, , "lu_deforestation"][, , "Above Ground Carbon"],
                  dim = c("land", "c_pools"))
  olc  <- dimSums(co2[, , "lu_other_conversion"][, , otherSet][, , "Above Ground Carbon"],
                  dim = c("land", "c_pools"))
  pulse <- collapseNames(defo) + collapseNames(olc)

  # optional dead-slash fraction of the wood-harvest aboveground flux (default OFF; see @param warning)
  if (harvestSlashFrac > 0) {
    harv  <- dimSums(co2[, , "lu_harvest"][, , "Above Ground Carbon"], dim = c("land", "c_pools"))
    pulse <- pulse + harvestSlashFrac * collapseNames(harv)
  }

  # ---------------------------------------------------------------------------------------------------
  # Find the first REPORTED clearing year (emisCO2 returns NA at the y1995 base year: no stock-difference
  # flux). Drop leading NA/zero years; anchor the pre-firstYear priming to the early-period clearing level.
  yearTotal <- as.numeric(dimSums(pulse, dim = 1))
  names(yearTotal) <- getYears(pulse)
  reported <- which(is.finite(yearTotal) & yearTotal > 0)
  if (length(reported) == 0) {
    # No positive clearing flux anywhere in the gdx (e.g. a degenerate no-clearing scenario). There
    # is nothing to defer, so the reframe is a no-op: return an all-zero correction over the model
    # years. This keeps reportEmissions(legacyEmis = TRUE, the default) additive and crash-free
    # rather than aborting the whole emissions report. Unit/level/cumulative transforms are all
    # no-ops on zeros but applied for a consistent return shape.
    message("legacyEmissions: no positive clearing flux in gdx - returning a zero legacy correction.")
    zero <- new.magpie(getCells(pulse), getYears(pulse),
                       c("legacy_net", "legacy_storage", "legacy_release"), fill = 0)
    if (level != "cell") zero <- superAggregateX(zero, aggr_type = "sum", level = level)
    if (unit == "gas") zero <- zero * 44 / 12
    return(out(zero, file))
  }
  firstYear   <- as.integer(sub("y", "", names(yearTotal)[reported[1]]))
  anchorYears <- names(yearTotal)[reported[seq_len(min(3, length(reported)))]]

  # ---------------------------------------------------------------------------------------------------
  # FOD slash/deadwood convolution of ONE (sub)pulse. Builds the annual pulse series firstYear:lastYear
  # (FOD needs annual steps - alpha is a per-year retention), prepends the pre-firstYear priming history,
  # runs the IPCC first-order-decay pool (cf. carbonLTS; mass-conserving beta = 1, so a0 is exactly the
  # immediately-released fraction), and returns the legacy delta (net/storage/release) with the priming
  # years zeroed. Closes over firstYear / anchorYears / priming / primingStart / lastYear. Called once
  # (scalar mode) or once per Koeppen main-group on the fraction-weighted sub-pulse (biome mode); the FOD
  # is linear, so the sum of the per-group results is exact - a "mixture of exponentials".
  lastYear <- 2150

  # Empirical historical-clearing prior for priming = "hist" (the default). Bookkeeping-ensemble global
  # LUC-CO2 (BLUE, OSCAR, H&C2023, GCB), normalised to its 2000-2010 mean (= the model's early-period anchor),
  # decadal 1850-2000. This is the SAME record MAgPIE's LUC-CO2 validation cloud is compared against, so the
  # priming prior is internally consistent with the comparison rather than a stylised guess. Note the real
  # early-20th-C level is much higher and flatter than the old "ramp" (1900 ~0.65 vs 0.33), with a mid-century
  # plateau and a 1960-80 dip. Values verify against validation.rds (Emissions|CO2|Land|+|Land-use Change, World).
  histYear <- seq(1850, 2000, 10)
  histFrac <- c(0.33, 0.34, 0.41, 0.47, 0.57, 0.65, 0.68, 0.74, 0.86, 0.91, 1.04, 0.93, 0.85, 0.79, 0.94, 1.00)

  fod <- function(pulseCell, a0, halfLife) {
    pc       <- pulseCell[, getYears(pulseCell, as.integer = TRUE) >= firstYear, ]
    pcAnchor <- setYears(dimSums(pc[, anchorYears, ], dim = 2) / length(anchorYears), NULL)
    modelAnnual <- paste0("y", firstYear:lastYear)
    missing     <- setdiff(modelAnnual, getYears(pc))
    pcAnnual <- time_interpolate(pc, interpolated_year = missing,
                                 integrate_interpolated_years = TRUE, extrapolation_type = "linear")
    pcAnnual[pcAnnual < 0] <- 0
    pcAnnual <- pcAnnual[, modelAnnual, ]

    primingYears <- paste0("y", primingStart:(firstYear - 1))
    fracVec <- switch(priming,
                      "hist" = approx(histYear, histFrac, xout = primingStart:(firstYear - 1), rule = 2)$y,
                      "peak" = rep(1.0, length(primingYears)),
                      "half" = rep(0.5, length(primingYears)),
                      "ramp" = seq(0.3, 1.0, length.out = length(primingYears)),
                      rep(0.5, length(primingYears)))
    primeMag <- new.magpie(getCells(pcAnnual), primingYears, getNames(pcAnnual), fill = 0)
    for (j in seq_along(primingYears)) primeMag[, primingYears[j], ] <- fracVec[j] * pcAnchor
    pcFull <- mbind(primeMag, pcAnnual)

    alpha       <- exp(-log(2) / halfLife)        # per-year retention (identical convention to carbonLTS)
    inflowToAdd <- (1 - a0) * pcFull              # slash-pool inflow (the deferred mass)
    stock <- pcFull
    stock[, , ] <- 0
    yrs <- getYears(stock)
    for (i in 1:(length(yrs) - 1)) stock[, i + 1, ] <- alpha * stock[, i, ] + inflowToAdd[, i, ]
    outflow <- stock - alpha * stock              # = (1 - alpha) * stock

    aa <- mbind(setNames(-1 * inflowToAdd + outflow, "legacy_net"),
                setNames(-1 * inflowToAdd,           "legacy_storage"),
                setNames(outflow,                    "legacy_release"))
    # priming years are pool-loading scaffold, not reported fluxes -> zero (leaves the primed in-window
    # release unmatched by in-window storage = the documented non-mass-conserving legacy credit)
    aa[, getYears(aa, as.integer = TRUE) < firstYear, ] <- 0
    aa
  }

  # Split the clearing pulse by Koeppen main group (A tropical / B arid / C temperate / D boreal-continental /
  # E polar) and run a SEPARATE slash/deadwood decay pool per group, then sum. The FOD is linear, so the sum
  # of the per-group results is exact - a "mixture of exponentials". CWD decay is fast in the tropics
  # (~4 yr half-life) and slow in the boreal (~20 yr), and the immediate/combustion fraction a0 is graded
  # tropical > temperate > boreal (see a0Biome / halfLifeBiome for values and provenance); a single global
  # pool would mis-time the ~60 percent tropical share of clearing.
  cc <- readGDX(gdx, "pm_climate_class", react = "silent")
  if (is.null(cc)) {
    # Fallback for a gdx without the Koeppen classification (should not occur for standard MAgPIE runs, where
    # module 52 always provides it): a single global bookkeeping-consistent pool - a0 = 0.2 (Houghton et al.
    # 2012 clearing-year burn fraction), half-life 8 yr (a tropical-temperate slash blend). Message, not crash.
    message("legacyEmissions: pm_climate_class not in gdx - using a single global slash pool (a0=0.2, half-life=8 yr).")
    a <- fod(pulse, 0.2, 8)
  } else {
    grp <- substr(getItems(cc, dim = 3), 1, 1)      # first Koeppen letter = main group (A/B/C/D/E)
    a <- NULL
    for (g in unique(grp)) {
      if (!(g %in% names(halfLifeBiome)) || !(g %in% names(a0Biome))) {
        warning("legacyEmissions: no biome parameters for Koeppen group '", g, "'; skipped.")
        next
      }
      fracG <- setYears(dimSums(cc[, , getItems(cc, dim = 3)[grp == g]], dim = 3), NULL)
      aG    <- fod(pulse * fracG, a0Biome[[g]], halfLifeBiome[[g]])
      a     <- if (is.null(a)) aG else a + aG
    }
  }

  # ---------------------------------------------------------------------------------------------------
  # cumulative transform (identical to carbonLTS): annual timestep weight, zero 1995, cumsum, rebase.
  if (cumulative) {
    years <- getYears(a, as.integer = TRUE)
    imYears <- new.magpie("GLO", years, NULL)
    imYears[, , ] <- c(1, diff(years))
    a[, "y1995", ] <- 0
    a <- a * imYears[, getYears(a), ]
    a <- as.magpie(apply(a, c(1, 3), cumsum))
    a <- a - setYears(a[, baseyear, ], NULL)
  }

  # aggregate LAST (FOD is linear -> GLO == sum of regions), then unit conversion
  if (level != "cell") {
    a <- superAggregateX(a, aggr_type = "sum", level = level)
  }
  if (unit == "gas") {
    a <- a * 44 / 12
  }

  out(a, file)
}
