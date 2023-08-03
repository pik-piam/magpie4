#' @title reportEmissions
#' @description reports GHG emissions
#'
#' @export
#'
#' @param gdx GDX file
#' @param storageWood Accounting for long term carbon storage in wood products. Default is TRUE.
#' @param grassi Replace MAgPIE's internal accounting of indirect emissions from land-use change with the estimates of Grassi et al.
#' @return GHG emissions as MAgPIE object (Unit: Mt CO2/yr, Mt N2O/yr, and Mt CH4/yr, for cumulative emissions Gt CO2)
#' @author Florian Humpenoeder, Benjamin Leon Bodirsky, Michael Crawford
#' @examples
#'
#' \dontrun{
#' x <- reportEmissions(gdx)
#' }
#' 
#' @section Tier-1 variables:
#' low pass filter = 3
#' Name | Unit | Meta
#' ---|---|---
#' Emissions\|CO2\|+\|Land | Mt CO2/yr | direct and indirect human-induced CO2 emissions from land use
#' Emissions\|CO2\|Land\|+\|Indirect | Mt CO2/yr | indirect human-induced CO2 emissions from land use (land carbon sink); based on estimates from Grassi et al 2021
#' Emissions\|CO2\|Land\|+\|Land-use Change | Mt CO2/yr | direct human-induced CO2 emissions from land use change, harvest and regrowth
#' Emissions\|CO2\|Land\|Land-use Change\|+\|Regrowth | Mt CO2/yr | negative CO2 emissions from regrowth
#' @section Tier-2 variables:
#' raw data; no low pass filter applied
#' Name | Unit | Meta
#' ---|---|---
#' Emissions\|CO2\|+\|Land RAW | Mt CO2/yr | direct and indirect human-induced CO2 emissions from land use
#' Emissions\|CO2\|Land\|+\|Indirect RAW | Mt CO2/yr | indirect human-induced CO2 emissions from land use (land carbon sink); based on estimates from Grassi et al 2021
#' Emissions\|CO2\|Land\|+\|Land-use Change RAW | Mt CO2/yr | direct human-induced CO2 emissions from land use change, harvest and regrowth
#' @md
#'
reportEmissions <- function(gdx, storageWood = TRUE, grassi = TRUE) {
    
    # -----------------------------------------------------------------------------------------------------------------
    # Calculate CO2 emissions from a MAgPIE .gdx file
    
    # landuseChange is mostly positive (deforestation) and regrowth is mostly negative (regrowth/afforestation),
    #   but there are some cases that behave differently.
    
    # landuseChange: cropland-to-pasture conversion causes negative emissions in soilc
    
    # regrowth: Litter carbon can decrease in case of afforestation/regrowth because the starting 
    #   level of litter carbon is always pasture litc. If pasture litc is higher than natveg 
    #   litc, this results in positive emissions.
    .calcCO2 <- function(.lowpass = 3, .cumulative = FALSE, .raw = FALSE) {
        
        co2 <- emisCO2(gdx, 
                       level = "regglo", unit = "gas", sum_land = FALSE, sum_cpool = FALSE, 
                       lowpass = .lowpass, cumulative = .cumulative)
        
        # Replace indirect emissions from climate change on managed land with the Grassi coefficients?
        if (grassi) {
          climateChange <- landCarbonSink(gdx, level = "regglo", cumulative = .cumulative)
        } else {
          climateChange <- dimSums(co2[, , "cc"], dim = 3)
        }
        
        # If cumulative, convert Mt CO2 to Gt CO2
        if (.cumulative) {
            co2 <- co2 / 1000 
            climateChange <- climateChange / 1000
        }  

        landuseTotal <- dimSums(co2[, , "lu"], dim = 3)
        total <- landuseTotal + climateChange
        
        if (!.raw) {
            
            landuseChange <- dimSums(co2[, , "lu_luc"], dim = 3)
            degradation   <- dimSums(co2[, , "lu_degrad"], dim = 3)
            landuseChange <- landuseChange + degradation # Include degradation into gross emissions
            
            vegetation <- c("forestry_plant", "forestry_ndc", "forestry_aff", "secdforest", "other")
            regrowth   <- collapseNames(dimSums(co2[, , "lu_regrowth"][, , vegetation], dim = "c_pools"), collapsedim = "type")
            
            # Above Ground / Below Ground Carbon
            totalPools   <- collapseNames(dimSums(co2[, , "total"], dim = c("land")))
            climatePools <- collapseNames(dimSums(co2[, , "cc"],    dim = c("land")))
            landusePools <- collapseNames(dimSums(co2[, , "lu"],    dim = c("land")))
            
            # Calculate wood products, if forestry module was activated and desired
            emisWoodProducts <- carbonLTS(gdx, level = "regglo", unit = "gas", cumulative = .cumulative)[, getYears(total), ]
            
            if (!is.null(emisWoodProducts) && storageWood) {
                
                if (.cumulative) {
                    # Can't divide by 1000 during cumulative calc as in def runs its NULL and NULL/1000 is numeric(0)
                    emisWoodProducts <- emisWoodProducts / 1000
                }
                
                # All categories
                inflow  <- collapseNames(emisWoodProducts[, , "annual_inflow"])
                outflow <- collapseNames(emisWoodProducts[, , "annual_outflow"])
                storage <- collapseNames(inflow + outflow)
                
                # Wood products (not including constr wood)
                emisWood        <- collapseNames(emisWoodProducts[, , "emisWood"])       # -1 removed in carbonLTS_IPCC.R
                emisWoodFuel    <- collapseNames(emisWoodProducts[, , "emisWoodFuel"])   # -1 removed in carbonLTS_IPCC.R
                emisConstrWood  <- collapseNames(emisWoodProducts[, , "emisConstrWood"]) # -1 removed in carbonLTS_IPCC.R
                
                # Purely industrial roundwood
                emisWoodInflow  <- collapseNames(emisWoodProducts[, , "wood_inflow"])
                emisWoodOutflow <- collapseNames(emisWoodProducts[, , "wood_outflow"])
                emisWoodNet     <- collapseNames(emisWoodInflow + emisWoodOutflow) # inflow is negative
                
                # Building materials
                emisBuildingInflow  <- collapseNames(emisWoodProducts[, , "building_inflow"])
                emisBuildingOutflow <- collapseNames(emisWoodProducts[, , "building_outflow"])
                emisBuildingNet     <- collapseNames(emisBuildingInflow + emisBuildingOutflow) # inflow is negative
                
                ### NOTE TO FLORIAN -- STORAGE IS OVERWRITTEN HERE
                # Sum of net emissions from industrial roundwood and building material
                storage <- emisWoodNet + emisBuildingNet
                
                # Total emissions from wood harvest
                wood <- emisWoodFuel + storage  # emisWood is already accounted for in storage!
                
                # Recalculate top-level categories
                landuseChange <- landuseChange - (emisWood + emisWoodFuel + emisConstrWood) # Subtract all wood-related emissions
                landuseTotal  <- landuseChange + dimSums(regrowth, dim = 3) + wood # Add wood-related emissions and removals
                total <- landuseTotal + climateChange
                
                # Consistency check
                if (abs(sum(landuseTotal - 
                            (landuseChange + dimSums(regrowth, dim = 3) + collapseNames(wood)), 
                            na.rm = TRUE)) > 0.1) {
                    warning("Emission subcategories do not add up to total! Check the code.")
                }
                
            } else {
                
                wood                <- NULL
                emisWoodFuel        <- NULL
                storage             <- NULL
                inflow              <- NULL
                outflow             <- NULL
                emisWoodNet         <- NULL
                emisWoodInflow      <- NULL
                emisWoodOutflow     <- NULL
                emisBuildingNet     <- NULL
                emisBuildingInflow  <- NULL
                emisBuildingOutflow <- NULL
                
            }
        }
        
        # Never apply lowpass filter on peatland emissions
        peatland <- PeatlandEmissions(gdx, level = "regglo", unit = "gas", cumulative = .cumulative)
        if (!is.null(peatland)) {
            peatland <- collapseNames(peatland[, , "co2"])
            if (.cumulative) {
                peatland <- peatland / 1000
            }
            
            total        <- total + peatland
            landuseTotal <- landuseTotal + peatland
        }
        
        # Generate return list
        if (.raw) {
            .x <- list(
                total         = total,
                landuseTotal  = landuseTotal,
                climateChange = climateChange
            )
            
        } else {
            .x <- list(
                total                      = total, 
                landuseTotal               = landuseTotal, 
                climateChange              = climateChange, 
                landuseChange              = landuseChange,
                degradation                = degradation, 
                totalRegrowth              = dimSums(regrowth, dim = 3),
                regrowthAffCO2Price        = collapseNames(regrowth[, , "forestry_aff"]),
                regrowthAffNPI_NDC         = collapseNames(regrowth[, , "forestry_ndc"]),
                regrowthPlantations        = collapseNames(regrowth[, , "forestry_plant"]),
                regrowthSecondaryForests   = collapseNames(regrowth[, , "secdforest"]),
                regrowthOther              = collapseNames(regrowth[, , "other"]),
                wood                       = wood,
                emisWoodFuel               = emisWoodFuel,
                storage                    = storage,
                # inflow                    = inflow,
                # outlow                    = outflow,
                emisWoodNet                = emisWoodNet,
                emisWoodInflow             = emisWoodInflow,
                emisWoodOutflow            = emisWoodOutflow,
                emisBuildingNet            = emisBuildingNet,
                emisBuildingInflow         = emisBuildingInflow,
                emisBuildingOutflow        = emisBuildingOutflow, 
                peatland                   = peatland,
                totalPools                 = totalPools,
                climatePools               = climatePools,
                landusePools               = landusePools
            )
        }
        
        return(.x)
    }
    
    
    # -----------------------------------------------------------------------------------------------------------------
    # Yearly CO2 emissions, lowpass = 3
    
    yearlyCO2 <- .calcCO2(.lowpass = 3, .cumulative = FALSE)
    
    # nolint
    emissionsReport <- with(yearlyCO2, 
      mbind(
        setNames(total,                     "Emissions|CO2|Land (Mt CO2/yr)"), # All human-induced land-related CO2 emissions
        setNames(landuseTotal,              "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"), # direct human-induced CO2 emissions, includes land-use change, land management and regrowth of vegetation
        setNames(climateChange,             "Emissions|CO2|Land|+|Indirect (Mt CO2/yr)"), # indirect human-induced CO2 emissions: environmental change, climate change, natural effects
        setNames(landuseChange,             "Emissions|CO2|Land|Land-use Change|+|Gross LUC (Mt CO2/yr)"), # land-use change
        setNames(degradation,               "Emissions|CO2|Land|Land-use Change|Gross LUC|+|Forest Degradation (Mt CO2/yr)"), # forest Degradation
        setNames(totalRegrowth,             "Emissions|CO2|Land|Land-use Change|+|Regrowth (Mt CO2/yr)"), # regrowth of vegetation
        setNames(regrowthAffCO2Price,       "Emissions|CO2|Land|Land-use Change|Regrowth|CO2-price AR (Mt CO2/yr)"), # regrowth of vegetation
        setNames(regrowthAffNPI_NDC,        "Emissions|CO2|Land|Land-use Change|Regrowth|NPI_NDC AR (Mt CO2/yr)"), # regrowth of vegetation
        setNames(regrowthPlantations,       "Emissions|CO2|Land|Land-use Change|Regrowth|Timber Plantations (Mt CO2/yr)"), # regrowth of vegetation
        setNames(regrowthSecondaryForests,  "Emissions|CO2|Land|Land-use Change|Regrowth|Secondary Forest (Mt CO2/yr)"), # regrowth of vegetation
        setNames(regrowthOther,             "Emissions|CO2|Land|Land-use Change|Regrowth|Other Land (Mt CO2/yr)"), # regrowth of vegetation
        setNames(peatland,                  "Emissions|CO2|Land|Land-use Change|+|Peatland (Mt CO2/yr)"),
        # setNames(totalPools,                paste0("Emissions|CO2|Land|Land-use Change|++|", getNames(totalPools), " (Mt CO2/yr)")),
        setNames(landusePools,              paste0("Emissions|CO2|Land|Land-use Change|++|", getNames(landusePools), " (Mt CO2/yr)"))
        # setNames(climatePools,              paste0("Emissions|CO2|Land|Land-use Change|++|", getNames(climatePools), " (Mt CO2/yr)"))
      )
    )
    
    # Only attempt to append wood-related reports if the forestry module was activated
    if (!is.null(yearlyCO2$wood)) {
        emissionsReport <- with(yearlyCO2, 
          mbind(
            emissionsReport,
            setNames(wood,                      "Emissions|CO2|Land|Land-use Change|+|Wood Harvest (Mt CO2/yr)"),
            setNames(emisWoodFuel,              "Emissions|CO2|Land|Land-use Change|Wood Harvest|Short Lived Products (Mt CO2/yr)"),
            setNames(storage,                   "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage (Mt CO2/yr)"), # carbon stored in wood products + release from wood products
            # setNames(inflow,                  "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|+|Inflow (Mt CO2/yr)"), # carbon stored in wood products
            # setNames(outlow,                  "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|+|Outflow (Mt CO2/yr)"), # slow release from wood products
            setNames(emisWoodNet,               "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|+|Industrial roundwood (Mt CO2/yr)"), # carbon stored in Industrial roundwood + release from Industrial roundwood
            setNames(emisWoodInflow,            "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Industrial roundwood|Inflow (Mt CO2/yr)"), # carbon stored in Industrial roundwood
            setNames(emisWoodOutflow,           "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Industrial roundwood|Outflow (Mt CO2/yr)"), # slow release from Industrial roundwood
            setNames(emisBuildingNet,           "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|+|Buildings (Mt CO2/yr)"), # carbon stored in wood buildings + release from wood buildings
            setNames(emisBuildingInflow,        "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Buildings|Inflow (Mt CO2/yr)"), # carbon stored in wood buildings
            setNames(emisBuildingOutflow,       "Emissions|CO2|Land|Land-use Change|Wood Harvest|Storage|Buildings|Outflow (Mt CO2/yr)"), # slow release from wood buildings
          )
        )
    } 
    # end nolint
    
    
    # -----------------------------------------------------------------------------------------------------------------
    # RAW yearly CO2 emissions, lowpass = 0
    
    rawYearlyCO2 <- .calcCO2(.lowpass = 0, .cumulative = FALSE, .raw = TRUE)
    
    # nolint
    emissionsReport <- with(rawYearlyCO2,
      mbind(
          emissionsReport, 
          setNames(total,         "Emissions|CO2|Land RAW (Mt CO2/yr)"), # All human-induced land-related CO2 emissions
          setNames(landuseTotal,  "Emissions|CO2|Land RAW|+|Land-use Change RAW (Mt CO2/yr)"), # direct human-induced CO2 emissions, includes land-use change, land management and regrowth of vegetation
          setNames(climateChange, "Emissions|CO2|Land RAW|+|Indirect RAW (Mt CO2/yr)") # indirect human-induced CO2 emissions: environmental change, climate change, natural effects
      )
    )
    # end nolint
    
    
    # -----------------------------------------------------------------------------------------------------------------
    # Cumulative CO2 emissions, lowpass = 3
    
    cumulativeCO2 <- .calcCO2(.lowpass = 3, .cumulative = TRUE)
    
    # nolint
    emissionsReport <- with(cumulativeCO2, 
      mbind(
          emissionsReport,
          setNames(total,                     "Emissions|CO2|Land|Cumulative (Gt CO2)"), # All human-induced land-related CO2 emissions
          setNames(landuseTotal,              "Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)"), # direct human-induced CO2 emissions, includes land-use change, land management and regrowth of vegetation
          setNames(climateChange,             "Emissions|CO2|Land|Cumulative|+|Indirect (Gt CO2)"), # indirect human-induced CO2 emissions: environmental change, climate change, natural effects
          setNames(landuseChange,             "Emissions|CO2|Land|Cumulative|Land-use Change|+|Gross LUC (Gt CO2)"), # land-use change
          setNames(degradation,               "Emissions|CO2|Land|Cumulative|Land-use Change|Gross LUC|+|Forest Degradation (Mt CO2/yr)"), # forest Degradation
          setNames(totalRegrowth,             "Emissions|CO2|Land|Cumulative|Land-use Change|+|Regrowth (Gt CO2)"), # regrowth of vegetation
          setNames(regrowthAffCO2Price,       "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|CO2-price AR (Gt CO2)"), # regrowth of vegetation
          setNames(regrowthAffNPI_NDC,        "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|NPI_NDC AR (Gt CO2)"), # regrowth of vegetation
          setNames(regrowthPlantations,       "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|Timber Plantations (Gt CO2)"), # regrowth of vegetation
          setNames(regrowthSecondaryForests,  "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|Secondary Forest (Gt CO2)"), # regrowth of vegetation
          setNames(regrowthOther,             "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|Other Land (Gt CO2)"), # regrowth of vegetation
          setNames(peatland,                  "Emissions|CO2|Land|Cumulative|Land-use Change|+|Peatland (Gt CO2)")
      )
    )
    
    if (!is.null(cumulativeCO2$wood)) {
        emissionsReport <- with(cumulativeCO2, 
          mbind(
              emissionsReport,
              setNames(wood,                      "Emissions|CO2|Land|Cumulative|Land-use Change|+|Wood Harvest (Gt CO2)"),
              setNames(emisWoodFuel,              "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Short Lived Products (Gt CO2)"),
              setNames(storage,                   "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage (Gt CO2)"), # carbon stored in wood products + release from wood products
              # setNames(inflow,                  "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Inflow (Gt CO2)"), # carbon stored in wood products
              # setNames(outlow,                  "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Outflow (Gt CO2)"), # slow release from wood products
              setNames(emisWoodNet,               "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|+|Industrial roundwood (Gt CO2)"), # carbon stored in Industrial roundwood + release from Industrial roundwood
              setNames(emisWoodInflow,            "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Industrial roundwood|Inflow (Gt CO2)"), # carbon stored in Industrial roundwood
              setNames(emisWoodOutflow,           "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Industrial roundwood|Outflow (Gt CO2)"), # slow release from Industrial roundwood
              setNames(emisBuildingNet,           "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|+|Buildings (Gt CO2)"), # carbon stored in wood buildings + release from wood buildings
              setNames(emisBuildingInflow,        "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Buildings|Inflow (Gt CO2)"), # carbon stored in wood buildings
              setNames(emisBuildingOutflow,       "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|Storage|Buildings|Outflow (Gt CO2)") # slow release from wood 
          )
        )
    }
    # end nolint  
    

    # -----------------------------------------------------------------------------------------------------------------
    # N2O, NOx, NH3 emissions reporting
    
    .generateNitrogenReport <- function(.type) {
        
        .createReport <- function(.emission, .source, .name = NULL) {
            type <- getItems(.emission, dim = 3.2)
            
            unit <- type
            if (unit %in% c("n2o_direct", "n2o_indirect")) {
                unit <- "n2o"
            }
            unit <- reportingnames(unit)
            
            t <- dimSums(.emission[, , .source], dim = 3)
            n <- paste0("Emissions|", reportingnames(type), "|Land", .name, " (Mt ", unit, "/yr)")
            return(setNames(t, n))
        }
         
        nEmissions <- Emissions(gdx, level = "regglo", type = .type, unit = "gas", subcategories = TRUE, inorg_fert_split = TRUE)

        agriculture <- c("SOM", "inorg_fert", "man_crop", "awms", "resid", "man_past", "rice")
        burn <- c("resid_burn")
        peatland_n2o <- c("peatland")
        
        # For backwards compatibility, add peatland N2O emissions if they were not already returned by Emissions.R 
        if (.type == "n2o_n" && !("peatland" %in% getItems(nEmissions, dim = 3.1))) {
            
            peatlandEmissions <- PeatlandEmissions(gdx, unit = "gas", level = "regglo")
            if (!is.null(peatlandEmissions)) {
                peatlandEmissions <- collapseNames(peatlandEmissions[, , "n2o"])
                getNames(peatlandEmissions) <- "peatland.n2o"
            } 

            nEmissions <- mbind(nEmissions, peatlandEmissions)
        }
        

        # nolint
        .x <- mbind(
            .createReport(nEmissions, c(agriculture, burn, peatland_n2o)), 
            .createReport(nEmissions, agriculture,                                                     "|+|Agriculture"), 
            .createReport(nEmissions, "awms",                                                          "|Agriculture|+|Animal Waste Management"), 
            .createReport(nEmissions, c("inorg_fert", "man_crop", "resid", "SOM", "rice", "man_past"), "|Agriculture|+|Agricultural Soils"), 
            .createReport(nEmissions, c("inorg_fert", "rice"),                                         "|Agriculture|Agricultural Soils|+|Inorganic Fertilizers"), 
            .createReport(nEmissions, c("inorg_fert_crop", "rice"),                                    "|Agriculture|Agricultural Soils|Inorganic Fertilizers|+|Cropland"), 
            .createReport(nEmissions, c("inorg_fert_past"),                                            "|Agriculture|Agricultural Soils|Inorganic Fertilizers|+|Pasture"), 
            .createReport(nEmissions, c("man_crop"),                                                   "|Agriculture|Agricultural Soils|+|Manure applied to Croplands"), 
            .createReport(nEmissions, c("resid"),                                                      "|Agriculture|Agricultural Soils|+|Decay of Crop Residues"), 
            .createReport(nEmissions, c("SOM"),                                                        "|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss"), 
            # .createReport(nEmissions, c("rice"),                                                       "|Agriculture|Agricultural Soils|+|Lower N2O emissions of rice"), 
            .createReport(nEmissions, c("man_past"),                                                   "|Agriculture|Agricultural Soils|+|Pasture"), 
            .createReport(nEmissions, burn,                                                            "|+|Biomass Burning"), 
            .createReport(nEmissions, c("resid_burn"),                                                 "|Biomass Burning|+|Burning of Crop Residues")
        )
        # end nolint
            
        # Old versions of MAgPIE may not include peatlands
        if ("peatland" %in% getItems(nEmissions, dim = 3.1)) {

            .x <- mbind(
                .x,
                .createReport(nEmissions, peatland_n2o,  "|+|Peatland"),
                .createReport(nEmissions, c("peatland"), "|Peatland|+|Managed")
            )
            
        }
        
        return(.x)
        
    }
    
    nEmisTypes <- c("n2o_n", "nh3_n", "no2_n", "no3_n", "n2o_n_direct", "n2o_n_indirect")
    nitrogenEmissions <- mbind(lapply(X = nEmisTypes, FUN = .generateNitrogenReport))
    emissionsReport <- mbind(emissionsReport, nitrogenEmissions)
    

    # -----------------------------------------------------------------------------------------------------------------
    # CH4 emissions reporting
    
    agricult_ch4 <- c("rice", "awms", "ent_ferm")
    burn_ch4 <- c("resid_burn")
    peatland_ch4 <- "peatland"
    
    # combine all CH4 emissions in one object
    ch4 <- collapseNames(Emissions(gdx, level = "regglo", type = "ch4", unit = "gas", subcategories = TRUE), collapsedim = 2)
    
    # Add peatland emissions if missing
    if (!"peatland" %in% getNames(ch4, dim = 1)) {
        
        peatlandEmissions <- PeatlandEmissions(gdx, unit = "gas", level = "regglo")
        if (!is.null(peatlandEmissions)) {
            peatlandEmissions <- setNames(collapseNames(peatlandEmissions[, , "ch4"]), "peatland")
        }
        ch4 <- mbind(ch4, peatlandEmissions)
    }
    
    # nolint
    emissionsReport <- mbind(
        emissionsReport,
        setNames(dimSums(ch4[, , c(agricult_ch4, burn_ch4, peatland_ch4)], dim = 3), "Emissions|CH4|Land (Mt CH4/yr)"),
        setNames(dimSums(ch4[, , agricult_ch4], dim = 3),                            "Emissions|CH4|Land|+|Agriculture (Mt CH4/yr)"),
        setNames(dimSums(ch4[, , c("rice")], dim = 3),                               "Emissions|CH4|Land|Agriculture|+|Rice (Mt CH4/yr)"),
        setNames(dimSums(ch4[, , c("awms")], dim = 3),                               "Emissions|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr)"),
        setNames(dimSums(ch4[, , c("ent_ferm")], dim = 3),                           "Emissions|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr)"),
        setNames(dimSums(ch4[, , c(burn_ch4)], dim = 3),                             "Emissions|CH4|Land|+|Biomass Burning (Mt CH4/yr)"),
        setNames(dimSums(ch4[, , c("resid_burn")], dim = 3),                         "Emissions|CH4|Land|Biomass Burning|+|Burning of Crop Residues (Mt CH4/yr)"),
        setNames(dimSums(ch4[, , c(peatland_ch4)], dim = 3),                         "Emissions|CH4|Land|+|Peatland (Mt CH4/yr)"),
        setNames(dimSums(ch4[, , c("peatland")], dim = 3),                           "Emissions|CH4|Land|Peatland|+|Managed (Mt CH4/yr)")
    )
    # end nolint
    

    # -----------------------------------------------------------------------------------------------------------------
    # N2O GWP100AR6 emissions reporting
    
    .generateGWPN2O <- function(.unit) {
        
        agriculture <- c("SOM", "inorg_fert", "man_crop", "awms", "resid", "man_past", "rice")
        
        emissions <- Emissions(gdx, level = "regglo", type = "n2o_n", unit = .unit, subcategories = TRUE)
        emissions <- collapseNames(emissions, collapsedim = 2)
        
        .createReport <- function(.emission, .name = NULL) {
            t <- dimSums(emissions[, , .emission], dim = 3)
            n <- paste0("Emissions|N2O_", .unit, "|Land", .name, " (Mt CO2e/yr)")
            return(setNames(t, n))
        }
        
        # nolint
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
        # end nolint
        
        return(.x)
    }
    
    emissionsReport <- mbind(
      emissionsReport, 
      .generateGWPN2O("GWP100AR6")
    )
    
    
    # -----------------------------------------------------------------------------------------------------------------
    # CH4 GWP100AR6 and GWP*AR6 emissions reporting
    
    .generateGWPCH4 <- function(.unit) {
        
        emissions <- Emissions(gdx, level = "regglo", type = "ch4", unit = .unit, subcategories = TRUE)
        emissions <- collapseNames(emissions, collapsedim = 2)
        
        .createReport <- function(.emission, .name = NULL) {
            t <- dimSums(emissions[, , .emission], dim = 3)
            n <- paste0("Emissions|CH4_", .unit, "|Land", .name, " (Mt CO2e/yr)")
            return(setNames(t, n))
        }
        
        # nolint
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
        # nolint end
        
        return(.x)
    }
    
    emissionsReport <- mbind(
        emissionsReport,
        .generateGWPCH4("GWP100AR6"),
        .generateGWPCH4("GWP*AR6")
    )
    
    
    # -----------------------------------------------------------------------------------------------------------------
    # Total yearly CO2e (for GWP100AR6) emissions reporting
    
    .generateTotalGWP <- function(.unit) {
        reports <- c(paste0("Emissions|CH4_", .unit, "|Land (Mt CO2e/yr)"),
                     paste0("Emissions|N2O_", .unit, "|Land (Mt CO2e/yr)"),
                     "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)")
        
        total <- dimSums(emissionsReport[, , reports], dim = 3) * 0.001 # Mt to Gt CO2e
        total <- setNames(total, paste0("Emissions|", .unit, "|Land (Gt CO2e/yr)"))
        
        return(total)
    }
    
    emissionsReport <- mbind(
        emissionsReport, 
        .generateTotalGWP("GWP100AR6")
    )
    
    
    # -----------------------------------------------------------------------------------------------------------------
    # Total cumulative CO2e (for GWP100AR6) emissions reporting
    
    .generateCumulativeGWP <- function(.unit) {
        
        years <- getYears(emissionsReport, as.integer = TRUE)
        
        # accumulate flow reports (CH4, N2O)
        flows <- emissionsReport[, , c(paste0("Emissions|CH4_", .unit, "|Land (Mt CO2e/yr)"),
                                       paste0("Emissions|N2O_", .unit, "|Land (Mt CO2e/yr)"))]
        flows <- flows[, years, ]
        flows[, c("y1995", "y2000"), ] <- 0
        
        flows <- time_interpolate(flows, interpolated_year = min(years):max(years))
        flows <- as.magpie(apply(flows, c(1, 3), cumsum))
        flows <- flows[, years, ]
        
        # accumulate stock reports (CO2)
        stock <- emissionsReport[, , "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"]
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
    
    emissionsReport <- mbind(
        emissionsReport, 
        .generateCumulativeGWP("GWP100AR6")
    )
    
    return(emissionsReport)

}
