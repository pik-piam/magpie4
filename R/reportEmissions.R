#' @title reportEmissions
#' @description reports GHG emissions
#'
#' @export
#'
#' @param gdx GDX file
#' @param storage_wood Accounting for long term carbon storage in wood products. Default is TRUE
#' @return GHG emissions as MAgPIE object (Unit: Mt CO2/yr, Mt N2O/yr and Mt CH4/yr)
#' @author Florian Humpenoeder, Benjamin Leon Bodirsky
#' @examples
#'
#' \dontrun{
#' x <- reportEmissions(gdx)
#' }
#'
reportEmissions <- function(gdx, storage_wood = TRUE) {

  # luc is mostly positive (deforestation), regrowth is mostly negative (regrowth/afforestation). There are, however, some cases that behave differently.
  # luc: cropland-to-pasture conversion causes negative emissions in soilc
  # regrowth: Litter carbon can decrease in case of afforestation/regrowth because the starting level of litter carbon is always pasture litc. If pasture litc is higher than natveg litc, this results in positive emissions.

  a <- emisCO2(gdx, level = "regglo", unit = "gas", lowpass = 3, sum_land = F, sum_cpool = F)

  #climatechange <- dimSums(a[, , "cc"], dim = 3)
  climatechange <- landCarbonSink(gdx, level="regglo")
  if(!is.null(climatechange)) getNames(climatechange) <- "Emissions|CO2|Land|+|Indirect (Mt CO2/yr)" # indirect human-induced CO2 emissions: environmental change, climate change, natural effects
  lu_tot <- dimSums(a[, , "lu"], dim = 3)
  if(!is.null(climatechange)) {
    total <- lu_tot + climatechange
  } else {
    total <- lu_tot
  }
  luc <- dimSums(a[, , "lu_luc"], dim = 3)
  degrad <- dimSums(a[, , "lu_degrad"], dim = 3)
  regrowth <- collapseNames(dimSums(a[, , "lu_regrowth"][, , c("forestry_plant", "forestry_ndc", "forestry_aff", "secdforest", "other")], dim = "c_pools"), collapsedim = "type")
  emis_wood_products <- carbonLTS(gdx, level = "regglo", unit = "gas")[, getYears(total), ]


  # Above Ground / Below Ground Carbon
  #total_pools <- collapseNames(dimSums(a[, , "total"], dim = c("land")))
  #climate_pools <- collapseNames(dimSums(a[, , "cc"], dim = c("land")))
  lu_pools <- collapseNames(dimSums(a[, , "lu"], dim = c("land")))

  ## Include degradation in gross emissions
  luc <- luc + degrad

  # wood products
  if (!is.null(emis_wood_products) && storage_wood) {
    # calc storage and decay
    ## All categories
    inflow <- collapseNames(emis_wood_products[, , "annual_inflow"])
    outflow <- collapseNames(emis_wood_products[, , "annual_outflow"])
    storage <- collapseNames(inflow + outflow)

    ## Wood products (not including constr wood)
    emis_wood <- collapseNames(emis_wood_products[, , "emis_wood"]) #-1 removed in carbonLTS_IPCC.R
    emis_woodfuel <- collapseNames(emis_wood_products[, , "emis_woodfuel"]) #-1 removed in carbonLTS_IPCC.R
    emis_constr_wood <- collapseNames(emis_wood_products[, , "emis_constr_wood"]) #-1 removed in carbonLTS_IPCC.R

    ## purely industrial roundwood
    emis_wood_inflow <- collapseNames(emis_wood_products[, , "wood_inflow"])
    emis_wood_outflow <- collapseNames(emis_wood_products[, , "wood_outflow"])
    emis_wood_net <- collapseNames(emis_wood_inflow + emis_wood_outflow) ## inflow is negative

    ## Building materials
    emis_building_inflow <- collapseNames(emis_wood_products[, , "building_inflow"])
    emis_building_outflow <- collapseNames(emis_wood_products[, , "building_outflow"])
    emis_building_net <- collapseNames(emis_building_inflow + emis_building_outflow) ## inflow is negative

    # sum of net emissions from industrial roundwood and building material
    storage <- emis_wood_net + emis_building_net
    # total emissions from wood harvest
    wood <- emis_woodfuel + storage  # emis_wood is already accounted for in storage!

    # recalculate top categories
    luc <- luc - (emis_wood + emis_woodfuel + emis_constr_wood) #take away all wood-related emissions
    lu_tot <- luc + dimSums(regrowth, dim = 3) + wood #add wood-related emissions and removals
    if(!is.null(climatechange)) {
      total <- lu_tot + climatechange
    } else {
      total <- lu_tot
    }

    # check
    #if (abs(sum(total - (lu_tot + climatechange), na.rm = TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    if (abs(sum(lu_tot - (luc + dimSums(regrowth, dim = 3) + collapseNames(wood)), na.rm = TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    # assign proper names

    getNames(wood)                  <- "Emissions|CO2|Land|Land-use Change|+|Wood Harvest (Mt CO2/yr)"
    getNames(emis_woodfuel)         <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Short Lived Products (Mt CO2/yr)"
    getNames(storage)               <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage (Mt CO2/yr)" # carbon stored in wood products + release from wood products
    # getNames(inflow)                <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|+|Inflow (Mt CO2/yr)" # carbon stored in wood products
    # getNames(outflow)               <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|+|Outflow (Mt CO2/yr)" # slow release from wood products
    getNames(emis_wood_net)         <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|+|Industrial roundwood (Mt CO2/yr)" # carbon stored in Industrial roundwood + release from Industrial roundwood
    getNames(emis_wood_inflow)      <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Industrial roundwood|Inflow (Mt CO2/yr)" # carbon stored in Industrial roundwood
    getNames(emis_wood_outflow)     <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Industrial roundwood|Outflow (Mt CO2/yr)" # slow release from Industrial roundwood
    getNames(emis_building_net)     <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|+|Buildings (Mt CO2/yr)" # carbon stored in wood buildings + release from wood buildings
    getNames(emis_building_inflow)  <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Buildings|Inflow (Mt CO2/yr)" # carbon stored in wood buildings
    getNames(emis_building_outflow) <- "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Buildings|Outflow (Mt CO2/yr)" # slow release from wood buildings
  } else {
    wood <- emis_woodfuel <- storage <- inflow <- outflow <- emis_wood_net <- emis_wood_inflow <- emis_wood_outflow <- emis_building_net <- emis_building_inflow <- emis_building_outflow <- NULL
  }

  # Don't apply lowpass filter on peatland emissions
  peatland <- PeatlandEmissions(gdx, level = "regglo", unit = "gas")
  if (!is.null(peatland)) {
    peatland <- collapseNames(peatland[, , "co2"])
    total <- total + peatland
    lu_tot <- lu_tot + peatland
    getNames(peatland) <- "Emissions|CO2|Land|Land-use Change|+|Peatland (Mt CO2/yr)"
  }

  x <- mbind(
    setNames(total, "Emissions|CO2|Land (Mt CO2/yr)"), # All human-induced land-related CO2 emissions
    setNames(lu_tot, "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"), # direct human-induced CO2 emissions, includes land-use change, land management and regrowth of vegetation
    peatland,
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
#    inflow,
#    outflow,
    emis_wood_net,
    emis_wood_inflow,
    emis_wood_outflow,
    emis_building_net,
    emis_building_inflow,
    emis_building_outflow,
    climatechange,
    #setNames(total_pools, paste0("Emissions|CO2|Land|++|", getNames(total_pools), " (Mt CO2/yr)")),
    setNames(lu_pools, paste0("Emissions|CO2|Land|Land-use Change|++|", getNames(lu_pools), " (Mt CO2/yr)"))
    #setNames(climate_pools, paste0("Emissions|CO2|Land|Indirect|++|", getNames(climate_pools), " (Mt CO2/yr)"))
  )

  # CO2 annual lowpass=0
  a <- emisCO2(gdx, level = "regglo", unit = "gas", lowpass = 0, sum_land = F, sum_cpool = F)

  #climatechange <- dimSums(a[, , "cc"], dim = 3)
  climatechange <- landCarbonSink(gdx, level="regglo")
  if(!is.null(climatechange)) getNames(climatechange) <- "Emissions|CO2|Land RAW|+|Indirect RAW (Mt CO2/yr)" # indirect human-induced CO2 emissions: environmental change, climate change, natural effects
  lu_tot <- dimSums(a[, , "lu"], dim = 3)
  if(!is.null(climatechange)) {
    total <- lu_tot + climatechange
  } else {
    total <- lu_tot
  }

  peatland <- PeatlandEmissions(gdx, level = "regglo", unit = "gas")
  if (!is.null(peatland)) {
    peatland <- collapseNames(peatland[, , "co2"])
    total <- total + peatland
    lu_tot <- lu_tot + peatland
  }

  x <- mbind(
    x, setNames(total, "Emissions|CO2|Land RAW (Mt CO2/yr)"), # All human-induced land-related CO2 emissions
    setNames(lu_tot, "Emissions|CO2|Land RAW|+|Land-use Change RAW (Mt CO2/yr)"), # direct human-induced CO2 emissions, includes land-use change, land management and regrowth of vegetation
    climatechange
  )

  # CO2 cumulative lowpass=3
  a <- emisCO2(gdx, level = "regglo", unit = "gas", lowpass = 3, sum_land = F, sum_cpool = F, cumulative = TRUE) / 1000

  #climatechange <- dimSums(a[, , "cc"], dim = 3)
  climatechange <- landCarbonSink(gdx, level="regglo", cumulative = TRUE)
  if(!is.null(climatechange)) {
    climatechange <- climatechange / 1000
    getNames(climatechange) <- "Emissions|CO2|Land|Cumulative|+|Indirect (Gt CO2)" # indirect human-induced emissions: environmental change, climate change, natural effects
  }
  lu_tot <- dimSums(a[, , "lu"], dim = 3)
  if(!is.null(climatechange)) {
    total <- lu_tot + climatechange
  } else {
    total <- lu_tot
  }
  luc <- dimSums(a[, , "lu_luc"], dim = 3)
  degrad <- dimSums(a[, , "lu_degrad"], dim = 3)
  regrowth <- collapseNames(dimSums(a[, , "lu_regrowth"][, , c("forestry_plant", "forestry_ndc", "forestry_aff", "secdforest", "other")], dim = "c_pools"), collapsedim = "type")
  emis_wood_products <- carbonLTS(gdx, level = "regglo", unit = "gas", cumulative = TRUE)[, getYears(total), ]

  ## Include degradation in gross emissions
  luc <- luc + degrad

  # wood products
  if (!is.null(emis_wood_products) && storage_wood) {
    emis_wood_products <- emis_wood_products / 1000 ## Can't divide by 1000 during cumulative calc as in def runs its NULL and NULL/1000 is numeric(0)
    # calc storage and decay
    ## All categories
    inflow <- collapseNames(emis_wood_products[, , "annual_inflow"])
    outflow <- collapseNames(emis_wood_products[, , "annual_outflow"])
    storage <- collapseNames(inflow + outflow)

    ## Wood products (not including constr wood)
    emis_wood <- collapseNames(emis_wood_products[, , "emis_wood"]) #-1 removed in carbonLTS_IPCC.R
    emis_woodfuel <- collapseNames(emis_wood_products[, , "emis_woodfuel"]) #-1 removed in carbonLTS_IPCC.R
    emis_constr_wood <- collapseNames(emis_wood_products[, , "emis_constr_wood"]) #-1 removed in carbonLTS_IPCC.R

    ## What did we emit by burning and what did we save in storage
    wood <- emis_woodfuel + storage  # emis_wood is already accounted for in storage!

    ## purely industrial roundwood
    emis_wood_inflow <- collapseNames(emis_wood_products[, , "wood_inflow"])
    emis_wood_outflow <- collapseNames(emis_wood_products[, , "wood_outflow"])
    emis_wood_net <- collapseNames(emis_wood_inflow + emis_wood_outflow) ## inflow is negative

    ## Building materials
    emis_building_inflow <- collapseNames(emis_wood_products[, , "building_inflow"])
    emis_building_outflow <- collapseNames(emis_wood_products[, , "building_outflow"])
    emis_building_net <- collapseNames(emis_building_inflow + emis_building_outflow) ## inflow is negative

    # sum of net emissions from industrial roundwood and building material
    storage <- emis_wood_net + emis_building_net
    # total emissions from wood harvest
    wood <- emis_woodfuel + storage  # emis_wood is already accounted for in storage!

    # recalculate top categories
    luc <- luc - (emis_wood + emis_woodfuel + emis_constr_wood) #take away all wood-related emissions
    lu_tot <- luc + dimSums(regrowth, dim = 3) + wood #add wood-related emissions and removals
    if(!is.null(climatechange)) {
      total <- lu_tot + climatechange
    } else {
      total <- lu_tot
    }

    # check
    #if (abs(sum(total - (lu_tot + climatechange), na.rm = TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    if (abs(sum(lu_tot - (luc + dimSums(regrowth, dim = 3) + collapseNames(wood)), na.rm = TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")

    # assign proper names
    getNames(wood)                  <- "Emissions|CO2|Land|Cumulative|Land-use Change|+|Wood Harvest (Gt CO2)"
    getNames(emis_woodfuel)         <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Short Lived Products (Gt CO2)"
    getNames(storage)               <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage (Gt CO2)" # carbon stored in wood products + release from wood products
    # getNames(inflow)                <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Inflow (Gt CO2)" # carbon stored in wood products
    # getNames(outflow)               <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Outflow (Gt CO2)" # slow release from wood products
    getNames(emis_wood_net)         <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|+|Industrial roundwood (Gt CO2)" # carbon stored in Industrial roundwood + release from Industrial roundwood
    getNames(emis_wood_inflow)      <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Industrial roundwood|Inflow (Gt CO2)" # carbon stored in Industrial roundwood
    getNames(emis_wood_outflow)     <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Industrial roundwood|Outflow (Gt CO2)" # slow release from Industrial roundwood
    getNames(emis_building_net)     <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|+|Buildings (Gt CO2)" # carbon stored in wood buildings + release from wood buildings
    getNames(emis_building_inflow)  <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Buildings|Inflow (Gt CO2)" # carbon stored in wood buildings
    getNames(emis_building_outflow) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Buildings|Outflow (Gt CO2)" # slow release from wood buildings
  } else {
    wood <- emis_woodfuel <- storage <- inflow <- outflow <- emis_wood_net <- emis_wood_inflow <- emis_wood_outflow <- emis_building_net <- emis_building_inflow <- emis_building_outflow <- NULL
  }

  # Don't apply lowpass filter on peatland emissions
  peatland <- PeatlandEmissions(gdx, level = "regglo", unit = "gas", cumulative = TRUE)
  if (!is.null(peatland)) {
    peatland <- collapseNames(peatland[, , "co2"]) / 1000
    total <- total + peatland
    lu_tot <- lu_tot + peatland
    getNames(peatland) <- "Emissions|CO2|Land|Cumulative|Land-use Change|+|Peatland (Gt CO2)"
  }

  x <- mbind(
    x, setNames(total, "Emissions|CO2|Land|Cumulative (Gt CO2)"),
    setNames(lu_tot, "Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)"), # includes land-use change and regrowth of vegetation
    peatland,
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
#    inflow,
#    outflow,
    emis_wood_net,
    emis_wood_inflow,
    emis_wood_outflow,
    emis_building_net,
    emis_building_inflow,
    emis_building_outflow,
    climatechange
  )

  #x <- superAggregateX(x, level = "regglo", aggr_type = "sum")

  # N2O, NOx, NH3
  n_emissions <- c("n2o_n", "nh3_n", "no2_n", "no3_n", "n2o_n_direct", "n2o_n_indirect")
  total <- Emissions(gdx, level = "regglo", type = n_emissions, unit = "gas", subcategories = TRUE, inorg_fert_split = TRUE)

  #Add peatland emissions if missing
  if(!"peatland" %in% getNames(total,dim=1)) {
    peatland <- PeatlandEmissions(gdx, unit = "gas", level = "regglo")
    if (!is.null(peatland)) {
      peatland <- collapseNames(peatland[, , "n2o"])
      getNames(peatland) <- "peatland.n2o"
    } else {
      peatland <- NULL
    }
    total <- mbind(total, peatland)
  }


  for (emi in getNames(total, dim = 2)) {
    prefix <- paste0("Emissions|", reportingnames(emi), "|Land")
    a <- total[, , emi]

    emi2 <- emi
    if (emi2 %in% c("n2o_direct", "n2o_indirect")) {
      emi2 <- "n2o"
    }
    emi2 <- reportingnames(emi2)

    agricult <- c("SOM", "inorg_fert", "man_crop", "awms", "resid", "man_past", "rice")
    burn <- "resid_burn"
    if (emi == "n2o") peatland <- "peatland" else peatland <- NULL

    #subset, aggregate, rename and combine
    x <- mbind(
      x,
      setNames(
        dimSums(a[, , c(agricult,burn,peatland)], dim = 3),
        paste0(prefix, " (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , agricult], dim = 3),
        paste0(prefix, "|+|Agriculture (Mt ", emi2, "/yr)")
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
      ),
      setNames(
        dimSums(a[, , c(burn)], dim = 3),
        paste0(prefix, "|+|Biomass Burning (Mt ", emi2, "/yr)")
      ),
      setNames(
        dimSums(a[, , c("resid_burn")], dim = 3),
        paste0(prefix, "|Biomass Burning|+|Burning of Crop Residues (Mt ", emi2, "/yr)")
      )
    )

    #Add peatland N2O emissions
    if(emi == "n2o") {
      x <- mbind(
        x,
        setNames(
          dimSums(a[, , c(peatland)], dim = 3),
          paste0(prefix, "|+|Peatland (Mt ", emi2, "/yr)")
        ),
        setNames(
          dimSums(a[, , c("peatland")], dim = 3),
          paste0(prefix, "|Peatland|+|Managed (Mt ", emi2, "/yr)")
        )
      )
    }
  }


  # CH4
  agricult_ch4 <- c("rice", "awms", "ent_ferm")
  burn_ch4 <- c("resid_burn")
  peatland_ch4 <- "peatland"

  #combine all CH4 emissions in one object
  a <- collapseNames(Emissions(gdx, level = "regglo", type = "ch4", unit = "gas", subcategories = TRUE), collapsedim = 2)
  #Add peatland emissions if missing
  if(!"peatland" %in% getNames(a,dim=1)) {
    peatland <- PeatlandEmissions(gdx, unit = "gas", level = "regglo")
    if (!is.null(peatland)) {
      peatland <- setNames(collapseNames(peatland[, , "ch4"]),"peatland")
    } else {
      peatland <- NULL
    }
    a <- mbind(a,peatland)
  }

  #subset, aggregate, rename and combine CH4 emissions
  x <- mbind(x,
             setNames(dimSums(a[,,c(agricult_ch4,burn_ch4,peatland_ch4)], dim = 3), "Emissions|CH4|Land (Mt CH4/yr)"),
             setNames(dimSums(a[,,agricult_ch4], dim = 3), "Emissions|CH4|Land|+|Agriculture (Mt CH4/yr)"),
             setNames(dimSums(a[, , c("rice")], dim = 3), "Emissions|CH4|Land|Agriculture|+|Rice (Mt CH4/yr)"),
             setNames(dimSums(a[, , c("awms")], dim = 3), "Emissions|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr)"),
             setNames(dimSums(a[, , c("ent_ferm")], dim = 3), "Emissions|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr)"),
             setNames(dimSums(a[, , c(burn_ch4)], dim = 3),"Emissions|CH4|Land|+|Biomass Burning (Mt CH4/yr)"),
             setNames(dimSums(a[, , c("resid_burn")], dim = 3),"Emissions|CH4|Land|Biomass Burning|+|Burning of Crop Residues (Mt CH4/yr)"),
             setNames(dimSums(a[, , c(peatland_ch4)], dim = 3),"Emissions|CH4|Land|+|Peatland (Mt CH4/yr)"),
             setNames(dimSums(a[, , c("peatland")], dim = 3),"Emissions|CH4|Land|Peatland|+|Managed (Mt CH4/yr)")
  )

  #####
  # Append N2O GWP100AR6

  appendEmissionN2O <- function(.unit) {
    agriculture <- c("SOM", "inorg_fert", "man_crop", "awms", "resid", "man_past", "rice")

    emissions <- Emissions(gdx, level = "regglo", type = "n2o_n", unit = .unit, subcategories = TRUE)
    emissions <- collapseNames(emissions, collapsedim = 2)

    .createReport <- function(.emission, .name = NULL) {
      t <- dimSums(emissions[, , .emission], dim = 3)
      n <- paste0("Emissions|N2O_", .unit, "|Land", .name, " (Mt CO2e/yr)")
      return(setNames(t, n))
    }

    #nolint start
    .x <- mbind(
      .createReport(c(agriculture, "resid_burn", "peatland")),
      .createReport(c(agriculture),                                                   "|+|Agriculture"),
      .createReport(c("awms"),                                                        "|Agriculture|+|Animal Waste Management"),
      .createReport(c("inorg_fert", "man_crop", "resid", "SOM", "rice", "man_past"),  "|Agriculture|+|Agricultural Soils"),
      .createReport(c("inorg_fert", "rice"),                                          "|Agriculture|Agricultural Soils|+|Inorganic Fertilizers"),
      .createReport(c("inorg_fert_crop", "rice"),                                     "|Agriculture|Agricultural Soils|Inorganic Fertilizers|+|Cropland"),
      .createReport(c("inorg_fert_past"),                                             "|Agriculture|Agricultural Soils|Inorganic Fertilizers|+|Pasture"),
      .createReport(c("man_crop"),                                                    "|Agriculture|Agricultural Soils|+|Manure applied to Croplands"),
      .createReport(c("resid"),                                                       "|Agriculture|Agricultural Soils|+|Decay of Crop Residues"),
      .createReport(c("SOM"),                                                         "|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss"),
      .createReport(c("man_past"),                                                    "|Agriculture|Agricultural Soils|+|Pasture"),
      .createReport(c("resid_burn"),                                                  "|+|Biomass Burning"),
      .createReport(c("resid_burn"),                                                  "|Biomass Burning|+|Burning of Crop Residues"),
      .createReport(c("peatland"),                                                    "|+|Peatland"),
      .createReport(c("peatland"),                                                    "|Peatland|+|Managed")
    )
    #nolint end

    return(.x)
  }

  x <- mbind(x, appendEmissionN2O("GWP100AR6"))

  #####
  # Append CH4 GWP100AR6 and GWP*AR6 to output object

  appendEmissionCH4 <- function(.unit) {

    emissions <- Emissions(gdx, level = "regglo", type = "ch4", unit = .unit, subcategories = TRUE)
    emissions <- collapseNames(emissions, collapsedim = 2)

    .createReport <- function(.emission, .name = NULL) {
      t <- dimSums(emissions[, , .emission], dim = 3)
      n <- paste0("Emissions|CH4_", .unit, "|Land", .name, " (Mt CO2e/yr)")
      return(setNames(t, n))
    }

    #nolint start
    .x <- mbind(
      .createReport(c("rice", "awms", "ent_ferm", "resid_burn", "peatland")),
      .createReport(c("rice", "awms", "ent_ferm"),                            "|+|Agriculture"),
      .createReport(c("rice"),                                                "|Agriculture|+|Rice"),
      .createReport(c("awms"),                                                "|Agriculture|+|Animal waste management"),
      .createReport(c("ent_ferm"),                                            "|Agriculture|+|Enteric fermentation"),
      .createReport(c("resid_burn"),                                          "|+|Biomass Burning"),
      .createReport(c("resid_burn"),                                          "|Biomass Burning|+|Burning of Crop Residues"),
      .createReport(c("peatland"),                                            "|+|Peatland"),
      .createReport(c("peatland"),                                            "|Peatland|+|Managed")
    )
    #nolint end

    return(.x)
  }

  x <- mbind(x,
    appendEmissionCH4("GWP100AR6"),
    appendEmissionCH4("GWP*AR6"))


  #####
  # Append total yearly CO2e (for GWP100AR6)

  appendTotalGWP <- function(.unit) {
    reports <- c(paste0("Emissions|CH4_", .unit, "|Land (Mt CO2e/yr)"),
                 paste0("Emissions|N2O_", .unit, "|Land (Mt CO2e/yr)"),
                 "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)")

    total <- dimSums(x[, , reports], dim = 3) * 0.001 # Mt to Gt CO2e
    total <- setNames(total, paste0("Emissions|", .unit, "|Land (Gt CO2e/yr)"))

    return(total)
  }

  x <- mbind(x, appendTotalGWP("GWP100AR6"))

  #####
  # Append total cumulative CO2e (for GWP100AR6)

  appendCumGWP <- function(.unit) {

    years <- getYears(x, as.integer = TRUE)

    # accumulate flow reports (CH4, N2O)
    flows <- x[, , c(paste0("Emissions|CH4_", .unit, "|Land (Mt CO2e/yr)"),
                     paste0("Emissions|N2O_", .unit, "|Land (Mt CO2e/yr)"))]
    flows <- flows[, years, ]
    flows[, c("y1995", "y2000"), ] <- 0

    flows <- time_interpolate(flows, interpolated_year = min(years):max(years))
    flows <- as.magpie(apply(flows, c(1, 3), cumsum))
    flows <- flows[, years, ]

    # accumulate stock reports (CO2)
    stock <- x[, , "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"]
    stock <- stock[, years, ]
    stock[, c("y1995", "y2000"), ] <- 0

    im_years <- m_yeardiff(gdx)[, years, ]
    stock <- stock * im_years
    stock <- as.magpie(apply(stock, c(1, 3), cumsum))

    # combine accounting of stocks and flows
    all <- setNames(stock, NULL) + dimSums(flows, dim = 3)
    all <- all * 0.001  # Mt to Gt CO2e
    all <- setNames(all, paste0("Emissions|", .unit, "|Land|Cumulative (Gt CO2e)"))

    all[, "y1995", ] <- NA

    return(all)
  }

  x <- mbind(x, appendCumGWP("GWP100AR6"))

  return(x)
}
