#' @title reportEmissions
#' @description reports GHG emissions
#'
#' @export
#'
#' @param gdx GDX file
#' @param storage Accounting for long term carbon storage in wood products. Default is TRUE
#' @return GHG emissions as MAgPIE object (Unit: Mt CO2/yr, Mt N2O/yr and Mt CH4/yr)
#' @author Florian Humpenoeder, Benjamin Leon Bodirsky
#' @examples
#'
#' \dontrun{
#' x <- reportEmissions(gdx)
#' }
#'
reportEmissions <- function(gdx, storage = TRUE) {

  # luc is mostly positive (deforestation), regrowth is mostly negative (regrowth/afforestation). There are, however, some cases that behave differently.
  # luc: cropland-to-pasture conversion causes negative emissions in soilc
  # regrowth: Litter carbon can decrease in case of afforestation/regrowth because the starting level of litter carbon is always pasture litc. If pasture litc is higher than natveg litc, this results in positive emissions.

  a <- emisCO2(gdx, level = "cell", unit = "gas", lowpass = 3, sum_land = F, sum_cpool = F)

  total <- dimSums(a[, , "total"], dim = 3)
  climatechange <- dimSums(a[, , "cc"], dim = 3)
  lu_tot <- dimSums(a[, , "lu"], dim = 3)
  luc <- dimSums(a[, , "lu_luc"], dim = 3)
  degrad <- dimSums(a[, , "lu_degrad"], dim = 3)
  regrowth <- collapseNames(dimSums(a[, , "lu_regrowth"][, , c("forestry_plant", "forestry_ndc", "forestry_aff", "secdforest", "other")], dim = "c_pools"), collapsedim = "type")
  emis_wood_products <- carbonLTS(gdx, unit = "gas")[, getYears(total), ]


  # Above Ground / Below Ground Carbon
  total_pools <- collapseNames(dimSums(a[, , "total"], dim = c("land")))
  climate_pools <- collapseNames(dimSums(a[, , "cc"], dim = c("land")))
  lu_pools <- collapseNames(dimSums(a[, , "lu"], dim = c("land")))

  # wood products
  if (!is.null(emis_wood_products) && storage) {
    # calc storage and decay
    inflow <- collapseNames(emis_wood_products[, , "annual_inflow"])
    outflow <- collapseNames(emis_wood_products[, , "annual_outflow"])
    storage <- collapseNames(inflow + outflow)
    emis_wood <- collapseNames(emis_wood_products[, , "emis_wood"]) #-1 removed in carbonLTS_IPCC.R
    emis_woodfuel <- collapseNames(emis_wood_products[, , "emis_woodfuel"]) #-1 removed in carbonLTS_IPCC.R
    wood <- emis_woodfuel + storage # emis_wood is already accounted for in storage!
    emis_building_inflow <- collapseNames(emis_wood_products[, , "building_inflow"])
    emis_building_outflow <- collapseNames(emis_wood_products[, , "building_outflow"])
    emis_building_net <- collapseNames(emis_building_inflow + emis_building_outflow) ## inflow is negative
    # recalculate top categories
    luc <- luc + degrad + inflow
    lu_tot <- luc + dimSums(regrowth, dim = 3) + storage
    total <- lu_tot + climatechange
    # check
    if (abs(sum(total - (lu_tot + climatechange), na.rm = TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    if (abs(sum(lu_tot - (luc + dimSums(regrowth, dim = 3) + collapseNames(storage)), na.rm = TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    # assign proper names

    getNames(wood) <- "Emissions|CO2|Land|Land-use Change|Wood Harvest (Mt CO2/yr)"
    getNames(emis_woodfuel) <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Short Lived Products (Mt CO2/yr)"
    getNames(storage) <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage (Mt CO2/yr)" # carbon stored in wood products + release from wood products
    getNames(inflow) <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|+|Inflow (Mt CO2/yr)" # carbon stored in wood products
    getNames(outflow) <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|+|Outflow (Mt CO2/yr)" # slow release from wood products
    getNames(emis_building_net) <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Buildings (Mt CO2/yr)" # carbon stored in wood products + release from wood products
    getNames(emis_building_inflow) <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Buildings|+|Inflow (Mt CO2/yr)" # carbon stored in wood products
    getNames(emis_building_outflow) <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Buildings|+|Outflow (Mt CO2/yr)" # slow release from wood products
  } else {
    wood <- emis_woodfuel <- storage <- inflow <- outflow <- emis_building_net <- emis_building_inflow <- emis_building_outflow <- NULL
  }

  # Don't apply lowpass filter on peatland emissions
  peatland <- PeatlandEmissions(gdx, unit = "gas")
  if (!is.null(peatland)) {
    peatland <- collapseNames(peatland[, , "co2"])
    total <- total + peatland
    getNames(peatland) <- "Emissions|CO2|Land|+|Peatland (Mt CO2/yr)"
  }

  x <- mbind(
    setNames(total, "Emissions|CO2|Land (Mt CO2/yr)"),
    peatland,
    setNames(lu_tot, "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"), # includes land-use change and regrowth of vegetation
    setNames(luc, "Emissions|CO2|Land|Land-use Change|+|Gross LUC (Mt CO2/yr)"), # land-use change
    setNames(degrad, "Emissions|CO2|Land|Land-use Change|Gross LUC|+|Forest Degradation (Mt CO2/yr)"), # Forest Degradation
    setNames(dimSums(regrowth, dim = 3), "Emissions|CO2|Land|Land-use Change|+|Regrowth (Mt CO2/yr)"), # regrowth of vegetation
    setNames(collapseNames(regrowth[, , "forestry_aff"]), "Emissions|CO2|Land|Land-use Change|Regrowth|CO2-price AR (Mt CO2/yr)"), # regrowth of vegetation
    setNames(collapseNames(regrowth[, , "forestry_ndc"]), "Emissions|CO2|Land|Land-use Change|Regrowth|NPI_NDC AR (Mt CO2/yr)"), # regrowth of vegetation
    setNames(collapseNames(regrowth[, , "forestry_plant"]), "Emissions|CO2|Land|Land-use Change|Regrowth|Timber Plantations (Mt CO2/yr)"), # regrowth of vegetation
    setNames(collapseNames(regrowth[, , "secdforest"]), "Emissions|CO2|Land|Land-use Change|Regrowth|Secondary Forest (Mt CO2/yr)"), # regrowth of vegetation
    setNames(collapseNames(regrowth[, , "other"]), "Emissions|CO2|Land|Land-use Change|Regrowth|Other Land (Mt CO2/yr)"), # regrowth of vegetation
    wood,
    emis_woodfuel,
    storage,
    inflow, # carbon stored in wood products
    outflow, # slow release from wood products
    emis_building_net,
    emis_building_inflow,
    emis_building_outflow,
    setNames(climatechange, "Emissions|CO2|Land|+|Climate Change (Mt CO2/yr)"), # emissions from the terrestrial biosphere
    setNames(total_pools, paste0("Emissions|CO2|Land|++|", getNames(total_pools), " (Mt CO2/yr)")), # emissions from the terrestrial biosphere
    setNames(lu_pools, paste0("Emissions|CO2|Land|Land-use Change|++|", getNames(lu_pools), " (Mt CO2/yr)")), # emissions from the terrestrial biosphere
    setNames(climate_pools, paste0("Emissions|CO2|Land|Climate Change|++|", getNames(climate_pools), " (Mt CO2/yr)"))
  ) # emissions from the terrestrial biosphere

  # CO2 annual lowpass=0
  a <- emisCO2(gdx, level = "cell", unit = "gas", lowpass = 0, sum_land = F, sum_cpool = F)

  total <- dimSums(a[, , "total"], dim = 3)
  climatechange <- dimSums(a[, , "cc"], dim = 3)
  lu_tot <- dimSums(a[, , "lu"], dim = 3)

  peatland <- PeatlandEmissions(gdx, unit = "gas")
  if (!is.null(peatland)) {
    peatland <- collapseNames(peatland[, , "co2"])
    total <- total + peatland
    getNames(peatland) <- "Emissions|CO2|Land RAW|+|Peatland (Mt CO2/yr)"
  }

  x <- mbind(
    x, setNames(total, "Emissions|CO2|Land RAW (Mt CO2/yr)"),
    peatland,
    setNames(lu_tot, "Emissions|CO2|Land RAW|+|Land-use Change RAW (Mt CO2/yr)"), # includes land-use change and regrowth of vegetation
    setNames(climatechange, "Emissions|CO2|Land RAW|+|Climate Change RAW (Mt CO2/yr)")
  ) # emissions from the terrestrial biosphere

  # CO2 cumulative lowpass=3
  a <- emisCO2(gdx, level = "cell", unit = "gas", lowpass = 3, sum_land = F, sum_cpool = F, cumulative = TRUE) / 1000

  total <- dimSums(a[, , "total"], dim = 3)
  climatechange <- dimSums(a[, , "cc"], dim = 3)
  lu_tot <- dimSums(a[, , "lu"], dim = 3)
  luc <- dimSums(a[, , "lu_luc"], dim = 3)
  degrad <- dimSums(a[, , "lu_degrad"], dim = 3)
  regrowth <- collapseNames(dimSums(a[, , "lu_regrowth"][, , c("forestry_plant", "forestry_ndc", "forestry_aff", "secdforest", "other")], dim = "c_pools"), collapsedim = "type")
  emis_wood_products <- carbonLTS(gdx, unit = "gas", cumulative = TRUE)[, getYears(total), ]

  # wood products
  if (!is.null(emis_wood_products) && storage) {
    emis_wood_products <- emis_wood_products / 1000 ## Can't divide by 1000 during cumulative calc as in def runs its NULL and NULL/1000 is numeric(0)
    # calc storage and decay
    inflow <- collapseNames(emis_wood_products[, , "annual_inflow"])
    outflow <- collapseNames(emis_wood_products[, , "annual_outflow"])
    storage <- collapseNames(inflow + outflow)
    emis_wood <- collapseNames(emis_wood_products[, , "emis_wood"]) #-1 removed in carbonLTS_IPCC.R
    emis_woodfuel <- collapseNames(emis_wood_products[, , "emis_woodfuel"]) #-1 removed in carbonLTS_IPCC.R
    wood <- emis_woodfuel + storage # emis_wood is already accounted for in storage!
    emis_building_inflow <- collapseNames(emis_wood_products[, , "building_inflow"])
    emis_building_outflow <- collapseNames(emis_wood_products[, , "building_outflow"])
    emis_building_net <- collapseNames(emis_building_inflow + emis_building_outflow) ## inflow is negative
    # recalculate top categories
    luc <- luc + degrad + inflow
    lu_tot <- luc + dimSums(regrowth, dim = 3) + storage
    total <- lu_tot + climatechange
    # check
    if (abs(sum(total - (lu_tot + climatechange), na.rm = TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    if (abs(sum(lu_tot - (luc + dimSums(regrowth, dim = 3) + collapseNames(storage)), na.rm = TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    # assign proper names

    getNames(wood) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest (Gt CO2)"
    getNames(emis_woodfuel) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Short Lived Products (Gt CO2)"
    getNames(storage) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage (Gt CO2)" # carbon stored in wood products + release from wood products
    getNames(inflow) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|+|Inflow (Gt CO2)" # carbon stored in wood products
    getNames(outflow) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|+|Outflow (Gt CO2)" # slow release from wood products
    getNames(emis_building_net) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Buildings (Gt CO2)" # carbon stored in wood products + release from wood products
    getNames(emis_building_inflow) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Buildings|+|Inflow (Gt CO2)" # carbon stored in wood products
    getNames(emis_building_outflow) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Buildings|+|Outflow (Gt CO2)" # slow release from wood products
  } else {
    wood <- emis_woodfuel <- storage <- inflow <- outflow <- emis_building_net <- emis_building_inflow <- emis_building_outflow <- NULL
  }

  # Don't apply lowpass filter on peatland emissions
  peatland <- PeatlandEmissions(gdx, unit = "gas", cumulative = TRUE)
  if (!is.null(peatland)) {
    peatland <- collapseNames(peatland[, , "co2"]) / 1000
    total <- total + peatland
    getNames(peatland) <- "Emissions|CO2|Land|Cumulative|+|Peatland (Gt CO2)"
  }

  x <- mbind(
    x, setNames(total, "Emissions|CO2|Land|Cumulative (Gt CO2)"),
    peatland,
    setNames(lu_tot, "Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)"), # includes land-use change and regrowth of vegetation
    setNames(luc, "Emissions|CO2|Land|Cumulative|Land-use Change|+|Gross LUC (Gt CO2)"), # land-use change
    setNames(degrad, "Emissions|CO2|Land|Cumulative|Land-use Change|Gross LUC|+|Forest Degradation (Mt CO2/yr)"), # Forest Degradation
    setNames(dimSums(regrowth, dim = 3), "Emissions|CO2|Land|Cumulative|Land-use Change|+|Regrowth (Gt CO2)"), # regrowth of vegetation
    setNames(collapseNames(regrowth[, , "forestry_aff"]), "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|CO2-price AR (Gt CO2)"), # regrowth of vegetation
    setNames(collapseNames(regrowth[, , "forestry_ndc"]), "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|NPI_NDC AR (Gt CO2)"), # regrowth of vegetation
    setNames(collapseNames(regrowth[, , "forestry_plant"]), "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|Timber Plantations (Gt CO2)"), # regrowth of vegetation
    setNames(collapseNames(regrowth[, , "secdforest"]), "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|Secondary Forest (Gt CO2)"), # regrowth of vegetation
    setNames(collapseNames(regrowth[, , "other"]), "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|Other Land (Gt CO2)"), # regrowth of vegetation
    wood,
    emis_woodfuel,
    storage,
    inflow, # carbon stored in wood products
    outflow, # slow release from wood products
    emis_building_net,
    emis_building_inflow,
    emis_building_outflow,
    setNames(climatechange, "Emissions|CO2|Land|Cumulative|+|Climate Change (Gt CO2)")
  ) # emissions from the terrestrial biosphere

  x <- superAggregateX(x, level = "regglo", aggr_type = "sum")

  # N2O, NOx, NH3
  n_emissions <- c("n2o_n", "nh3_n", "no2_n", "no3_n", "n2o_n_direct", "n2o_n_indirect")
  total <- Emissions(gdx, level = "regglo", type = n_emissions, unit = "gas", subcategories = TRUE, inorg_fert_split = TRUE)

  for (emi in getNames(total, dim = 2)) {
    prefix <- paste0("Emissions|", reportingnames(emi), "|Land")
    a <- total[, , emi]

    emi2 <- emi
    if (emi2 %in% c("n2o_direct", "n2o_indirect")) {
      emi2 <- "n2o"
    }
    emi2 <- reportingnames(emi2)

    agricult <- c("SOM", "inorg_fert", "man_crop", "awms", "resid", "man_past", "rice", "ent_ferm")
    x <- mbind(
      x, setNames(
        dimSums(a[, , agricult], dim = 3),
        paste0(prefix, "|+|Agriculture (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , c("resid_burn")], dim = 3),
        paste0(prefix, "|Biomass Burning|+|Burning of Crop Residues (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , "awms"], dim = 3),
        paste0(prefix, "|Agriculture|+|Animal Waste Management (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , c("inorg_fert", "man_crop", "resid", "SOM", "rice", "man_past")], dim = 3),
        paste0(prefix, "|Agriculture|+|Agricultural Soils (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , c("inorg_fert", "rice")], dim = 3),
        paste0(prefix, "|Agriculture|Agricultural Soils|+|Inorganic Fertilizers (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , c("inorg_fert_crop", "rice")], dim = 3),
        paste0(prefix, "|Agriculture|Agricultural Soils|Inorganic Fertilizers|+|Cropland (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , c("inorg_fert_past")], dim = 3),
        paste0(prefix, "|Agriculture|Agricultural Soils|Inorganic Fertilizers|+|Pasture (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , c("man_crop")], dim = 3),
        paste0(prefix, "|Agriculture|Agricultural Soils|+|Manure applied to Croplands (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , c("resid")], dim = 3),
        paste0(prefix, "|Agriculture|Agricultural Soils|+|Decay of Crop Residues (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , c("SOM")], dim = 3),
        paste0(prefix, "|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss (Mt ", emi2, "/yr)")
      ),
      #               setNames(dimSums(a[,,c("rice")],dim=3),
      #                     paste0(prefix,"|Agriculture|Agricultural Soils|+|Lower N2O emissions of rice (Mt ",emi2,"/yr)")),
      setNames(
        dimSums(a[, , c("man_past")], dim = 3),
        paste0(prefix, "|Agriculture|Agricultural Soils|+|Pasture (Mt ", emi2, "/yr)")
      )
    )
  }

  peatland <- PeatlandEmissions(gdx, unit = "gas", level = "regglo")
  if (!is.null(peatland)) {
    peatland <- collapseNames(peatland[, , "n2o"])
    getNames(peatland) <- "Emissions|N2O|Land|+|Peatland (Mt N2O/yr)"
    total_n2o <- dimSums(total[, , "n2o"], dim = 3) + peatland
    getNames(total_n2o) <- "Emissions|N2O|Land (Mt N2O/yr)"
  } else {
    total_n2o <- NULL
  }
  x <- mbind(x, total_n2o)
  x <- mbind(x, peatland)

  # CH4
  a <- collapseNames(Emissions(gdx, level = "regglo", type = "ch4", unit = "gas", subcategories = TRUE), collapsedim = 2)

  peatland <- PeatlandEmissions(gdx, unit = "gas", level = "regglo")
  if (!is.null(peatland)) {
    peatland <- collapseNames(peatland[, , "ch4"])
    getNames(peatland) <- "Emissions|CH4|Land|+|Peatland (Mt CH4/yr)"
    total_ch4 <- dimSums(a, dim = 3) + peatland
    getNames(total_ch4) <- "Emissions|CH4|Land (Mt CH4/yr)"
  } else {
    total_ch4 <- NULL
  }
  x <- mbind(x, total_ch4)
  x <- mbind(x, peatland)

  x <- mbind(
    x, setNames(dimSums(a, dim = 3), "Emissions|CH4|Land|+|Agriculture (Mt CH4/yr)"),
    setNames(dimSums(a[, , c("rice")], dim = 3), "Emissions|CH4|Land|Agriculture|+|Rice (Mt CH4/yr)"),
    setNames(dimSums(a[, , c("awms")], dim = 3), "Emissions|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr)"),
    setNames(dimSums(a[, , c("ent_ferm")], dim = 3), "Emissions|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr)")
  )

  # CH4 GWP
  a <- collapseNames(Emissions(gdx, level = "regglo", type = "ch4", unit = "GWP", subcategories = TRUE), collapsedim = 2)
  # todo: add peatland CH4
  x <- mbind(
    x, setNames(dimSums(a, dim = 3), "Emissions|CH4_GWP100|Land|+|Agriculture (Mt CO2e/yr)"),
    setNames(dimSums(a[, , c("rice")], dim = 3), "Emissions|CH4_GWP100|Land|Agriculture|+|Rice (Mt CO2e/yr)"),
    setNames(dimSums(a[, , c("awms")], dim = 3), "Emissions|CH4_GWP100|Land|Agriculture|+|Animal waste management (Mt CO2e/yr)"),
    setNames(dimSums(a[, , c("ent_ferm")], dim = 3), "Emissions|CH4_GWP100|Land|Agriculture|+|Enteric fermentation (Mt CO2e/yr)")
  )

  # CH4 GWP*
  a <- collapseNames(Emissions(gdx, level = "regglo", type = "ch4", unit = "GWP*", subcategories = TRUE), collapsedim = 2)
  # todo: add peatland CH4
  x <- mbind(
    x, setNames(dimSums(a, dim = 3), "Emissions|CH4_GWP*|Land|+|Agriculture (Mt CO2we/yr)"),
    setNames(dimSums(a[, , c("rice")], dim = 3), "Emissions|CH4_GWP*|Land|Agriculture|+|Rice (Mt CO2we/yr)"),
    setNames(dimSums(a[, , c("awms")], dim = 3), "Emissions|CH4_GWP*|Land|Agriculture|+|Animal waste management (Mt CO2we/yr)"),
    setNames(dimSums(a[, , c("ent_ferm")], dim = 3), "Emissions|CH4_GWP*|Land|Agriculture|+|Enteric fermentation (Mt CO2we/yr)")
  )

  return(x)
}
