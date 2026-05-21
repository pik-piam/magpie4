#' @title productEmissions
#' @description Calculates GHG emissions by product, allocating emissions from different
#' sources (land-use change, enteric fermentation, manure management, etc.) to specific
#' agricultural products based on cropland shares, livestock feed demand, and other factors.
#' This is an improved version with better code style and documentation.
#'
#' @export
#'
#' @param gdx GDX file
#' @param unit "element", "gas", "GWP100AR5", "GWP100AR6", "GWP*AR5", or "GWP*AR6"
#'    "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr
#'    "gas":     co2_c in Mt CO2/yr, n2o_n in Mt N2O/yr, ch4 in Mt CH4/yr
#'    "GWP":     co2_c in Mt CO2eq/yr, n2o_n in Mt CO2eq/yr, ch4 in Mt CO2eq/yr
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global)
#' @param perTonne If TRUE, returns emissions intensity (per tonne of production); if FALSE, returns total emissions
#' @return GHG emissions as MAgPIE object (Unit depends on \code{unit} parameter)
#' @author David M Chen
#' @seealso \code{\link{Emissions}}, \code{\link{emisCO2}}
#' @importFrom magclass collapseNames mbind dimSums dimOrder add_dimension add_columns setNames getItems getYears new.magpie
#' @importFrom magpie4 Emissions emisCO2 production croparea land
#' @examples
#' \dontrun{
#'   x <- productEmissions(gdx, unit = "GWP100AR6", level = "reg")
#'   xIntensity <- productEmissions(gdx, unit = "GWP100AR6", perTonne = TRUE)
#' }
#'
productEmissions <- function(gdx, unit = "GWP100AR6", level = "reg", perTonne = TRUE) {

  # Validate inputs
  if (!level %in% c("reg", "glo", "regglo")) {
    stop("Only regional level aggregations available: 'reg', 'glo', or 'regglo'")
  }

  # ==============================================================================
  # CO2 EMISSIONS BY PRODUCT
  # ==============================================================================
  # CO2 emissions from land-use change and degradation are allocated to products
  # based on cropland and pasture area shares
  # TODO: Include regrowth, wood storage, peatland emissions

  # Get CO2 emissions at cellular level (for spatial allocation)
  emis <- emisCO2(gdx, level = "cell", unit = "element")

  # Get cropland and pasture areas for allocation
  cropArea <- croparea(gdx, level = "cell", product_aggr = FALSE, water_aggr = TRUE)
  pastArea <- land(gdx, level = "cell")[, , "past"]
  getItems(pastArea, dim = 3) <- "pasture"  # rename to match kve set / trade naming
  agArea <- mbind(cropArea, pastArea)

  # Calculate agricultural land shares (for proportional allocation)
  agAreaTotal <- dimSums(agArea, dim = 3)
  agAreaTotal[agAreaTotal == 0] <- 1  # avoid division by zero
  ratioAg <- agArea / agAreaTotal
  ratioAg[is.na(ratioAg)] <- 0

  # Allocate land-use change emissions (lu_som_luc) to products by agricultural land share
  lucEmis <- emis[, , "lu_som_luc"] * ratioAg
  lucEmis <- add_dimension(lucEmis, dim = 3.2, add = "pollutants", nm = "co2_c")

  # Degradation emissions allocated only to cropland (not pasture)
  cropAreaTotal <- dimSums(cropArea, dim = 3)
  cropAreaTotal[cropAreaTotal == 0] <- 1  # avoid division by zero
  ratioCrop <- cropArea / cropAreaTotal
  ratioCrop[is.na(ratioCrop)] <- 0

  degradEmis <- emis[, , "lu_degrad"] * ratioCrop
  degradEmis <- add_dimension(degradEmis, dim = 3.2, add = "pollutants", nm = "co2_c")

  # Combine CO2 emissions
  cByProduct <- mbind(lucEmis, degradEmis)

  # Aggregate from cellular to regional level
  cByProduct <- dimSums(cByProduct, dim = 1.2)

  # ==============================================================================
  # CH4 EMISSIONS BY PRODUCT
  # ==============================================================================

  # Get CH4 emissions by category
  a <- collapseNames(Emissions(gdx, level = "reg", type = "ch4", unit = "element", 
                               subcategories = TRUE), collapsedim = 2)

  # --- Enteric fermentation: allocate to ruminants (livst_rum) and dairy (livst_milk) ---
  # Read feed composition data
  kconc <- readGDX(gdx, "k_conc53")      # concentrate feed items
  knoconc <- readGDX(gdx, "k_noconc53")  # non-concentrate feed items
  attr <- readGDX(gdx, "fm_attributes")  # feed attributes (gross energy, etc.)
  demFeed <- collapseNames(readGDX(gdx, "ov_dem_feed")[, , "level"])

  # Calculate enteric fermentation emissions for ruminants and dairy
  # Based on IPCC 2006 methodology with simplified emission factors
  # Emission factor: 0.03 for concentrate, 0.065 for non-concentrate
  emisRum <- dimSums(demFeed[, , list(kap = "livst_rum")][, , list(kall = kconc)] *
                      attr[, , "ge"][, , kconc], dim = 3.2) * 0.03 +
            dimSums(demFeed[, , list(kap = "livst_rum")][, , list(kall = knoconc)] *
                      attr[, , "ge"][, , knoconc], dim = 3.2) * 0.065
                      
  emisMilk <- dimSums(demFeed[, , list(kap = "livst_milk")][, , list(kall = kconc)] *
                       attr[, , "ge"][, , kconc], dim = 3.2) * 0.03 +
             dimSums(demFeed[, , list(kap = "livst_milk")][, , list(kall = knoconc)] *
                       attr[, , "ge"][, , knoconc], dim = 3.2) * 0.065

  # Calculate shares to split enteric fermentation between ruminants and dairy
  emisTotal <- collapseNames(emisRum + emisMilk, collapsedim = c(2, 3))
  emisTotal[emisTotal == 0] <- 1  # avoid division by zero
  emisShr <- collapseNames(emisRum, collapsedim = 2) / emisTotal
  emisShr <- add_columns(emisShr, dim = 3, addnm = "livst_milk")
  emisShr[, , "livst_milk"] <- 1 - emisShr[, , "livst_rum"]

  ch4EntFerm <- a[, , "ent_ferm"] * emisShr

  # --- Animal waste management systems (AWMS): allocate by livestock type ---
  awmsCh4Ef <- readGDX(gdx, "f53_ef_ch4_awms")  # CH4 emission factors for AWMS
  manure <- collapseNames(readGDX(gdx, "ov_manure")[, , "confinement.nr.level"])

  # Calculate CH4 from AWMS by livestock type
  awmsCh4 <- collapseNames(manure * awmsCh4Ef[, getYears(manure), ])
  awmsCh4Total <- dimSums(awmsCh4, dim = 3)
  awmsCh4Total[awmsCh4Total == 0] <- 1  # avoid division by zero
  awmsCh4Shr <- awmsCh4 / awmsCh4Total

  ch4Awms <- a[, , "awms"] * awmsCh4Shr

  # --- Rice cultivation: direct attribution ---
  ch4Rice <- add_dimension(a[, , "rice"], nm = "rice_pro", add = "k", dim = 3.2)

  # --- Residue burning: allocate by crop residue amounts ---
  resBurn <- collapseNames(readGDX(gdx, "ov_res_ag_burn")[, , "dm.level"])
  resBurnTotal <- dimSums(resBurn, dim = 3)
  resBurnTotal[resBurnTotal == 0] <- 1  # avoid division by zero
  resBurnShr <- resBurn / resBurnTotal
  resBurnShr[is.na(resBurnShr)] <- 0

  ch4ResBurn <- a[, , "resid_burn"] * resBurnShr

  # --- Peatland CH4: allocate by crop area in peatland cells ---
  ch4peatland <- a[, , "peatland"]
  cropCluster <- croparea(gdx, level = "cell", product_aggr = FALSE, water_aggr = TRUE)
  peatCluster <- PeatlandArea(gdx, level = "cell")[, , "degrad"]

  # Get crop area only in cells with degraded peatland
  # Weight crop area by the fraction of cell that is degraded peatland
  cellArea <- dimSums(land(gdx, level = "cell"), dim = 3)
  cellArea[cellArea == 0] <- 1  # avoid division by zero
  peatFrac <- peatCluster / cellArea
  peatFrac[is.na(peatFrac)] <- 0

  # Crop area weighted by peatland presence
  cropInPeat <- cropCluster * peatFrac

  # Aggregate to regional level and calculate shares
  cropInPeatReg <- dimSums(cropInPeat, dim = 1.2)
  cropInPeatTotal <- dimSums(cropInPeatReg, dim = 3)
  cropInPeatTotal[cropInPeatTotal == 0] <- 1  # avoid division by zero
  cropInPeatShr <- cropInPeatReg / cropInPeatTotal
  cropInPeatShr[is.na(cropInPeatShr)] <- 0

  # Allocate peatland CH4 to crops
  ch4Peat <- ch4peatland * cropInPeatShr
  ch4Peat <- collapseNames(ch4Peat, collapsedim = "data")

  # Combine all CH4 emissions by product
  ch4ByProduct <- mbind(ch4EntFerm, ch4Awms, ch4Rice, ch4ResBurn, ch4Peat)
  ch4ByProduct <- add_dimension(ch4ByProduct, dim = 3.2, add = "pollutants", nm = "ch4")
  ch4ByProduct <- complete_magpie(ch4ByProduct, fill = 0)

  # ==============================================================================
  # N2O AND OTHER NITROGEN EMISSIONS BY PRODUCT
  # ==============================================================================

  # Get N2O emissions by category (with subcategories for source allocation)
  n2oEmis <- Emissions(gdx, level = "reg", type = "n2o_n", unit = "element", 
                       subcategories = TRUE)

  # Keep only the aggregate n2o_n (not direct/indirect split) and collapse the pollutant dimension
  n2oEmis <- collapseNames(n2oEmis[, , "n2o_n"])

  # --- Cropland N inputs: allocate by nitrogen withdrawals ---
  # Includes: manure to cropland, inorganic fertilizer to cropland, residues, SOM
  nWith <- NitrogenBudgetWithdrawals(gdx, kcr = "kcr", net = TRUE, level = "reg")
  nWithTot <- dimSums(nWith, dim = 3)
  nWithTot[nWithTot == 0] <- 1  # avoid division by zero
  nWithShr <- nWith / nWithTot

  # Get available subcategories
  cropInputs <- c("man_crop", "inorg_fert_crop", "resid", "SOM")
  nInputs <- n2oEmis[, , cropInputs] * nWithShr

  # --- Inorg on pasture and Manure on pasture: allocate directly to pasture ---
  nFertPast <- n2oEmis[, , c("inorg_fert_past", "man_past")]
  nFertPast <- add_dimension(nFertPast, nm = "pasture", add = "k", dim = 3.2)

  # --- Residue burning: same shares as CH4 ---
  nResBurn <- n2oEmis[, , "resid_burn"] * resBurnShr

  # --- AWMS in confinement: allocate by livestock type ---
  manConf <- collapseNames(readGDX(gdx, "ov_manure_confinement")[, , "nr.level"])
  efConf <- collapseNames(readGDX(gdx, "f51_ef3_confinement")[, , "n2o_n_direct"])

  confNTotal <- dimSums(manConf * efConf, dim = 3)
  confNTotal[confNTotal == 0] <- 1  # avoid division by zero
  confNShr <- dimSums((manConf * efConf), dim = 3.2) / confNTotal

  # Get AWMS emissions and allocate by livestock type
  awmsN2o <- n2oEmis[, , "awms"]
  nAwmsConf <- awmsN2o * confNShr


  # --- Peatland N2O emissions: same as CH4, by crop area in peatland cells ---
  nPeat <- n2oEmis[, , "peatland"] * collapseNames(cropInPeatShr, 2)


  # Combine all N emissions by product (only non-NULL components)
  nByProduct <- mbind(nInputs, nFertPast, nResBurn, nAwmsConf, nPeat)
  nByProduct <- add_dimension(nByProduct, dim = 3.2, add = "pollutants", nm = "n2o_n")

  # ==============================================================================
  # COMBINE ALL GHG EMISSIONS BY PRODUCT
  # ==============================================================================
  # TODO: Add peatland emissions (N2O and CH4) - currently not allocated by product
  # 
  out <- mbind(cByProduct, nByProduct, ch4ByProduct)
  out <- dimSums(out, dim = 3.1)  # sum over emission subcategories

  # ==============================================================================
  # CALCULATE EMISSIONS INTENSITY (PER TONNE) IF REQUESTED
  # ==============================================================================

  if (perTonne) {
    prod <- production(gdx, level = "reg", product_aggr = FALSE, attributes = "dm")
    # prod has "pasture" — keep as-is to match kve set / trade naming

    # Avoid division by zero
    prod[prod == 0] <- NA
    out <- out / prod[, , getNames(out, dim = 2)]  # Mt emissions / Mt production
    out[is.na(out)] <- 0
  }

  # ==============================================================================
  # UNIT CONVERSION
  # ==============================================================================

  if (unit == "gas") {
    # Define conversion factors (elemental to gas form)
    conversionFactors <- c(
      "n2o_n" = 44 / 28,          # Mt N/yr to Mt N2O/yr
      "n2o_n_direct" = 44 / 28,   # Mt N/yr to Mt N2O/yr
      "n2o_n_indirect" = 44 / 28, # Mt N/yr to Mt N2O/yr
      "ch4" = 1,                  # no conversion needed
      "co2_c" = 44 / 12,          # Mt C/yr to Mt CO2/yr
      "no3_n" = 62 / 14,          # Mt N/yr to Mt NO3/yr
      "nh3_n" = 17 / 14,          # Mt N/yr to Mt NH3/yr
      "no2_n" = 46 / 14           # Mt N/yr to Mt NO2/yr
    )

    # Apply conversions only for pollutants that exist in output
    out <- out * conversionFactors[getNames(out, dim = "pollutants")]

    # Update pollutant names (remove _c and _n suffixes)
    getNames(out, dim = "pollutants") <- sub(pattern = "_c", replacement = "",
                                             x = getNames(out, dim = "pollutants"))
    getNames(out, dim = "pollutants") <- sub(pattern = "_n", replacement = "",
                                             x = getNames(out, dim = "pollutants"))
  }

  if (unit %in% c("GWP*AR5", "GWP*AR6")) {
    # Apply GWP* metric (Lynch et al. 2020 Environmental Research Letters)
    # Accounts for different persistence of CH4 vs CO2 in atmosphere
    years <- getYears(out, as.integer = TRUE)
    outTmp <- out
    for (t in years) {
      tBefore <- t - 20
      if (!tBefore %in% years) tBefore <- years[which.min(abs(years - tBefore))]
      out[, t, "ch4"] <- 4 * outTmp[, t, "ch4"] - 3.75 * outTmp[, tBefore, "ch4"]
    }
  }

  # GWP100 and GWP* for AR5 (IPCC Fifth Assessment Report)
  if (unit %in% c("GWP100AR5", "GWP*AR5")) {
    unitConversion <- out
    unitConversion[, , ] <- 1
    unitConversion[, , "n2o_n"] <- 44 / 28 * 265   # Mt N/yr to Mt CO2eq/yr (GWP100=265)
    unitConversion[, , "ch4"] <- 1 * 28             # Mt CH4 to Mt CO2eq/yr (GWP100=28)
    unitConversion[, , "co2_c"] <- 44 / 12          # Mt C/yr to Mt CO2/yr

    out <- out * unitConversion[, , getItems(out, dim = "pollutants")]

    # Update pollutant names
    getNames(out, dim = "pollutants") <- sub(pattern = "_c", replacement = "", 
                                            x = getNames(out, dim = "pollutants"))
    getNames(out, dim = "pollutants") <- sub(pattern = "_n", replacement = "", 
                                            x = getNames(out, dim = "pollutants"))
  }

  # GWP100 and GWP* for AR6 (IPCC Sixth Assessment Report)
  if (unit %in% c("GWP100AR6", "GWP*AR6")) {
    unitConversion <- out
    unitConversion[, , ] <- 1
    unitConversion[, , "n2o_n"] <- 44 / 28 * 273   # Mt N/yr to Mt CO2eq/yr (GWP100=273)
    unitConversion[, , "ch4"] <- 1 * 27             # Mt CH4 to Mt CO2eq/yr (GWP100=27)
    unitConversion[, , "co2_c"] <- 44 / 12          # Mt C/yr to Mt CO2/yr

    out <- out * unitConversion[, , getItems(out, dim = "pollutants")]

    # Update pollutant names
    getNames(out, dim = "pollutants") <- sub(pattern = "_c", replacement = "", 
                                            x = getNames(out, dim = "pollutants"))
    getNames(out, dim = "pollutants") <- sub(pattern = "_n", replacement = "", 
                                            x = getNames(out, dim = "pollutants"))
  }

  # Clean up any remaining NA values
  out[is.na(out)] <- 0

  if (level %in% c("reg", "glo", "regglo") || isCustomAggregation(level)) {
    out <- gdxAggregate(gdx, out, to = level)
  } else {
    stop("Aggregation level not supported: ", level)
  }

  return(out)
}
