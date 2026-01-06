#' @title reportEmissions
#' @description reports GHG emissions
#'
#' @export
#'
#' @param gdx GDX file
#' @param storageWood Accounting for long term carbon storage in wood products. Default is TRUE.
#' @return GHG emissions as MAgPIE object (Unit: Mt CO2/yr, Mt N2O/yr, and Mt CH4/yr, for cumulative emissions Gt CO2)
#' @author Florian Humpenoeder, Benjamin Leon Bodirsky, Michael Crawford
#' @examples
#' \dontrun{
#' x <- reportEmissions(gdx)
#' }
#'
#'
#' @section CO2 land-use change emissions (yearly):
#' Name | Unit | Meta
#' ---|---|---
#' Emissions\|CO2\|Land | Mt CO2/yr | Net CO2 flux from land use and land management, including environmental effects on managed land
#' Emissions\|CO2\|Land\|+\|Indirect | Mt CO2/yr | Carbon sink on managed land from environmental change (CO2 fertilization, climate, N deposition); managed land proxy following Grassi et al. 2021
#' Emissions\|CO2\|Land\|+\|Land-use Change | Mt CO2/yr | Net CO2 flux from land-use change, harvest and regrowth
#' Emissions\|CO2\|Land\|Land-use Change\|+\|Deforestation | Mt CO2/yr | CO2 emissions from permanent deforestation
#' Emissions\|CO2\|Land\|Land-use Change\|Deforestation\|+\|Primary forests | Mt CO2/yr | CO2 emissions from deforestation of primary forests
#' Emissions\|CO2\|Land\|Land-use Change\|Deforestation\|+\|Secondary forests | Mt CO2/yr | CO2 emissions from deforestation of secondary forests
#' Emissions\|CO2\|Land\|Land-use Change\|Deforestation\|+\|Forestry plantations | Mt CO2/yr | CO2 emissions from deforestation of forestry plantations
#' Emissions\|CO2\|Land\|Land-use Change\|Deforestation\|+\|Cropland Tree Cover | Mt CO2/yr | CO2 emissions from removal of trees on cropland
#' Emissions\|CO2\|Land\|Land-use Change\|+\|Forest degradation | Mt CO2/yr | CO2 emissions from forest degradation and shifting cultivation
#' Emissions\|CO2\|Land\|Land-use Change\|Forest degradation\|+\|Primary forests | Mt CO2/yr | CO2 emissions from degradation of primary forests
#' Emissions\|CO2\|Land\|Land-use Change\|Forest degradation\|+\|Secondary forests | Mt CO2/yr | CO2 emissions from degradation of secondary forests
#' Emissions\|CO2\|Land\|Land-use Change\|+\|Other land conversion | Mt CO2/yr | CO2 emissions from conversion of other natural land
#' Emissions\|CO2\|Land\|Land-use Change\|+\|Regrowth | Mt CO2/yr | CO2 removals from forest regrowth (negative values)
#' Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|CO2-price AR | Mt CO2/yr | CO2 removals from afforestation/reforestation driven by CO2 price
#' Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|NPI_NDC AR | Mt CO2/yr | CO2 removals from afforestation/reforestation under national policies
#' Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|Timber Plantations | Mt CO2/yr | CO2 removals from timber plantation regrowth
#' Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|Secondary Forest | Mt CO2/yr | CO2 removals from secondary forest regrowth
#' Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|Cropland Tree Cover | Mt CO2/yr | CO2 removals from trees on cropland
#' Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|Other Land | Mt CO2/yr | CO2 removals from regrowth on other land
#' Emissions\|CO2\|Land\|Land-use Change\|+\|Peatland | Mt CO2/yr | Net CO2 flux from managed peatlands
#' Emissions\|CO2\|Land\|Land-use Change\|Peatland\|+\|Positive | Mt CO2/yr | CO2 emissions from drained peatlands
#' Emissions\|CO2\|Land\|Land-use Change\|Peatland\|+\|Negative | Mt CO2/yr | CO2 removals from rewetted peatlands
#' Emissions\|CO2\|Land\|Land-use Change\|+\|Soil | Mt CO2/yr | Net CO2 flux from soil organic matter changes
#' Emissions\|CO2\|Land\|Land-use Change\|Soil\|++\|Emissions | Mt CO2/yr | CO2 emissions from soil carbon loss
#' Emissions\|CO2\|Land\|Land-use Change\|Soil\|++\|Withdrawals | Mt CO2/yr | CO2 removals from soil carbon accumulation
#' Emissions\|CO2\|Land\|Land-use Change\|Soil\|+\|Land Conversion | Mt CO2/yr | Net soil CO2 flux from land-use conversion
#' Emissions\|CO2\|Land\|Land-use Change\|Soil\|+\|Cropland management | Mt CO2/yr | Net soil CO2 flux from cropland management changes
#' Emissions\|CO2\|Land\|Land-use Change\|Soil\|+\|Soil Carbon Management | Mt CO2/yr | Net soil CO2 flux from explicit soil carbon management
#' Emissions\|CO2\|Land\|Land-use Change\|+\|Wood Harvest | Mt CO2/yr | CO2 emissions from wood harvest
#' Emissions\|CO2\|Land\|Land-use Change\|Wood Harvest\|+\|Timber Plantations | Mt CO2/yr | CO2 emissions from harvest in timber plantations
#' Emissions\|CO2\|Land\|Land-use Change\|Wood Harvest\|+\|Primary Forest | Mt CO2/yr | CO2 emissions from harvest in primary forests
#' Emissions\|CO2\|Land\|Land-use Change\|Wood Harvest\|+\|Secondary Forest | Mt CO2/yr | CO2 emissions from harvest in secondary forests
#' Emissions\|CO2\|Land\|Land-use Change\|Wood Harvest\|+\|Other Land | Mt CO2/yr | CO2 emissions from harvest on other land
#' Emissions\|CO2\|Land\|Land-use Change\|+\|Timber | Mt CO2/yr | Net CO2 flux from harvested wood products (storage minus release)
#' Emissions\|CO2\|Land\|Land-use Change\|Timber\|+\|Storage in HWP | Mt CO2/yr | CO2 stored in harvested wood products (negative values)
#' Emissions\|CO2\|Land\|Land-use Change\|Timber\|+\|Release from HWP | Mt CO2/yr | CO2 released from decay of harvested wood products
#' Emissions\|CO2\|Land\|Land-use Change\|+\|Residual | Mt CO2/yr | Residual CO2 flux not captured in other categories
#' Emissions\|CO2\|Land\|Land-use Change\|Residual\|+\|Positive | Mt CO2/yr | Positive residual CO2 flux
#' Emissions\|CO2\|Land\|Land-use Change\|Residual\|+\|Negative | Mt CO2/yr | Negative residual CO2 flux
#' Emissions\|CO2\|Land\|++\|Above Ground Carbon | Mt CO2/yr | CO2 flux from above ground carbon pools
#' Emissions\|CO2\|Land\|++\|Below Ground Carbon | Mt CO2/yr | CO2 flux from below ground carbon pools
#'
#' @section CO2 land carbon sink (yearly):
#' Name | Unit | Meta
#' ---|---|---
#' Emissions\|CO2\|Land Carbon Sink\|Grassi\|Managed Land\|Managed Forest | Mt CO2/yr | Carbon sink in managed forests following Grassi et al. 2021
#' Emissions\|CO2\|Land Carbon Sink\|LPJmL | Mt CO2/yr | Total carbon sink from LPJmL vegetation model
#' Emissions\|CO2\|Land Carbon Sink\|LPJmL\|+\|Managed Land | Mt CO2/yr | Carbon sink on managed land from LPJmL
#' Emissions\|CO2\|Land Carbon Sink\|LPJmL\|+\|Unmanaged Land | Mt CO2/yr | Carbon sink on unmanaged land from LPJmL
#'
#' @section CO2 land-use change emissions (cumulative):
#' Name | Unit | Meta
#' ---|---|---
#' Emissions\|CO2\|Land\|Cumulative | Gt CO2 | Cumulative net CO2 flux from land use and land management, including environmental effects on managed land
#' Emissions\|CO2\|Land\|Cumulative\|+\|Indirect | Gt CO2 | Cumulative carbon sink on managed land from environmental change
#' Emissions\|CO2\|Land\|Cumulative\|+\|Land-use Change | Gt CO2 | Cumulative net CO2 flux from land-use change, harvest and regrowth
#' Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Deforestation | Gt CO2 | Cumulative CO2 emissions from deforestation and degradation
#' Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Regrowth | Gt CO2 | Cumulative CO2 removals from regrowth
#' Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Other land conversion | Gt CO2 | Cumulative CO2 emissions from other land conversion
#' Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Peatland | Gt CO2 | Cumulative net CO2 flux from peatland
#' Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Soil | Gt CO2 | Cumulative net CO2 flux from soil
#' Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Wood Harvest | Gt CO2 | Cumulative CO2 emissions from wood harvest
#' Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Timber | Gt CO2 | Cumulative net CO2 flux from harvested wood products
#' Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Residual | Gt CO2 | Cumulative residual CO2 flux
#'
#' @section N2O emissions variables:
#' Name | Unit | Meta
#' ---|---|---
#' Emissions\|N2O\|Land | Mt N2O/yr | Total N2O emissions from agriculture, forestry and other land use (IPCC category 3)
#' Emissions\|N2O\|Land\|+\|Agriculture | Mt N2O/yr | N2O emissions from the agriculture sector
#' Emissions\|N2O\|Land\|Agriculture\|+\|Animal Waste Management | Mt N2O/yr | N2O emissions from animal waste management systems
#' Emissions\|N2O\|Land\|Agriculture\|+\|Agricultural Soils | Mt N2O/yr | N2O emissions from agricultural soils
#' Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|+\|Inorganic Fertilizers | Mt N2O/yr | N2O emissions from inorganic fertilizer application
#' Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|Inorganic Fertilizers\|+\|Cropland | Mt N2O/yr | N2O emissions from fertilizer on cropland
#' Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|Inorganic Fertilizers\|+\|Pasture | Mt N2O/yr | N2O emissions from fertilizer on pasture
#' Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|+\|Manure applied to Croplands | Mt N2O/yr | N2O emissions from manure applied to croplands
#' Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|+\|Decay of Crop Residues | Mt N2O/yr | N2O emissions from decay of crop residues
#' Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|+\|Soil Organic Matter Loss | Mt N2O/yr | N2O emissions from soil organic matter loss
#' Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|+\|Pasture | Mt N2O/yr | N2O emissions from pasture soils (manure deposited by grazing animals)
#' Emissions\|N2O\|Land\|+\|Peatland | Mt N2O/yr | N2O emissions from managed peatlands
#' Emissions\|N2O\|Land\|Peatland\|+\|Managed | Mt N2O/yr | N2O emissions from managed peatlands (excluding intact)
#' Emissions\|N2O\|Land\|+\|Biomass Burning | Mt N2O/yr | N2O emissions from biomass burning
#' Emissions\|N2O\|Land\|Biomass Burning\|+\|Burning of Crop Residues | Mt N2O/yr | N2O emissions from burning of crop residues
#'
#' @section CH4 emissions variables:
#' Name | Unit | Meta
#' ---|---|---
#' Emissions\|CH4\|Land | Mt CH4/yr | Total CH4 emissions from agriculture, forestry and other land use
#' Emissions\|CH4\|Land\|+\|Agriculture | Mt CH4/yr | CH4 emissions from agriculture sector
#' Emissions\|CH4\|Land\|Agriculture\|+\|Rice | Mt CH4/yr | CH4 emissions from flooded rice cultivation
#' Emissions\|CH4\|Land\|Agriculture\|+\|Animal waste management | Mt CH4/yr | CH4 emissions from animal waste management systems
#' Emissions\|CH4\|Land\|Agriculture\|+\|Enteric fermentation | Mt CH4/yr | CH4 emissions from enteric fermentation of livestock
#' Emissions\|CH4\|Land\|+\|Peatland | Mt CH4/yr | CH4 emissions from managed peatlands
#' Emissions\|CH4\|Land\|Peatland\|+\|Managed | Mt CH4/yr | CH4 emissions from managed peatlands (excluding intact)
#' Emissions\|CH4\|Land\|+\|Biomass Burning | Mt CH4/yr | CH4 emissions from biomass burning
#' Emissions\|CH4\|Land\|Biomass Burning\|+\|Burning of Crop Residues | Mt CH4/yr | CH4 emissions from burning of crop residues
#'
#' @section GWP emissions variables:
#' Name | Unit | Meta
#' ---|---|---
#' Emissions\|GWP100AR6\|Land | Gt CO2e/yr | Total GHG emissions from land use in CO2-equivalents using AR6 GWP100
#' Emissions\|GWP100AR6\|Land\|Cumulative | Gt CO2e | Cumulative total GHG emissions from land use
#' Emissions\|CH4_GWP100AR6\|Land | Mt CO2e/yr | CH4 emissions in CO2-equivalents using AR6 GWP100 (factor 27)
#' Emissions\|N2O_GWP100AR6\|Land | Mt CO2e/yr | N2O emissions in CO2-equivalents using AR6 GWP100 (factor 273)
#' @md
#'
reportEmissions <- function(gdx, storageWood = TRUE) {

    # -----------------------------------------------------------------------------------------------------------------
    # All transformations (lowpass filter, cumulative) will be applied to this single dataset
    co2_raw <- emisCO2(gdx, level = "regglo", unit = "gas", sum_land = FALSE, sum_cpool = FALSE)

    # -----------------------------------------------------------------------------------------------------------------
    # Helper function for applying lowpass filter

    .applyLowpassFilter <- function(data, lowpass = 0) {
        if (is.null(lowpass) || lowpass == 0) {
            return(data)
        }

        yrFix <- as.numeric(readGDX(gdx, "sm_fix_SSP2"))
        years <- getYears(data, as.integer = TRUE)
        yrsHist <- years[years > 1995 & years <= yrFix]
        yrsFut  <- years[years >= yrFix]

        # Apply lowpass filter (not applied on 1st time step, applied separately on historic and future period)
        filtered <- mbind(data[, 1995, ],
                          lowpass(data[, yrsHist, ], i = lowpass),
                          lowpass(data[, yrsFut, ],  i = lowpass)[, -1, ])

        return(filtered)
    }

    # -----------------------------------------------------------------------------------------------------------------
    # Calculate CO2 emissions with different transformations

    .calcCO2 <- function(.lowpass = 0, .cumulative = FALSE, .landCarbonSink = "grassi") {

        # Start with raw CO2 data (already calculated above)
        co2 <- co2_raw

        # Apply lowpass filter ONLY to CO2 emissions if requested and not cumulative
        if (.lowpass > 0 && !.cumulative) {
            co2 <- .applyLowpassFilter(co2, lowpass = .lowpass)
        }

        # Handle land carbon sink
        if (.landCarbonSink == "grassi") {
            # Use Grassi data WITHOUT filtering (as in original)
            eClimateChange <- landCarbonSink(gdx, level = "regglo", cumulative = .cumulative)
        } else {
            eClimateChange <- dimSums(co2[, , "cc"], dim = 3)
        }

        # If cumulative, apply timestep weighting and cumulative sum to raw data
        if (.cumulative) {
            # Always use raw data for cumulative calculation
            co2 <- co2_raw

            # Apply timestep weighting and cumulative sum
            timesteps <- m_yeardiff(gdx)
            co2[, "y1995", ] <- 0
            co2 <- co2 * timesteps[, getYears(co2), ]
            co2 <- as.magpie(apply(co2, c(1, 3), cumsum))
            co2 <- co2 - setYears(co2[, 1995, ], NULL)

            # Convert to Gt
            co2 <- co2 / 1000
            eClimateChange <- eClimateChange / 1000
        }

        eLanduseChange <- dimSums(co2[, , "lu"], dim = 3)
        totalNetFlux   <- eLanduseChange + eClimateChange

        # Emission from deforestation, degradation, other land conversion, harvest and SOM
        deforestation    <- collapseNames(dimSums(co2[, , "lu_deforestation"],    dim = "c_pools"))
        degradation      <- collapseNames(dimSums(co2[, , "lu_degrad"],           dim = "c_pools"))
        other_conversion <- collapseNames(dimSums(co2[, , "lu_other_conversion"], dim = "c_pools"))
        regrowth         <- collapseNames(dimSums(co2[, , "lu_regrowth"],         dim = "c_pools"))
        harvest          <- collapseNames(dimSums(co2[, , "lu_harvest"],          dim = "c_pools"))
        som              <- collapseNames(dimSums(co2[, , "lu_som"],              dim = "c_pools"))
        residual         <- collapseNames(dimSums(co2[, , "residual"],            dim = "c_pools"))
        somLu            <- collapseNames(dimSums(co2[, , "lu_som_luc"],          dim = "c_pools"))
        somMa            <- collapseNames(dimSums(co2[, , "lu_som_man"],          dim = "c_pools"))
        somMaTc          <- collapseNames(dimSums(co2[, , "lu_som_man_tc"],       dim = "c_pools"))
        somMaOt          <- collapseNames(dimSums(co2[, , "lu_som_man_ot"],       dim = "c_pools"))
        somScm           <- collapseNames(dimSums(co2[, , "lu_som_scm"],          dim = "c_pools"))

        # Split SOM into negative and positive emissions
        somLu_neg <- somLu_pos <- somLu
        somLu_neg[somLu_neg >= 0] <- 0
        somLu_pos[somLu_pos <= 0] <- 0

        somMa_neg <- somMa_pos <- somMa
        somMa_neg[somMa_neg >= 0] <- 0
        somMa_pos[somMa_pos <= 0] <- 0

        somMaTc_neg <- somMaTc_pos <- somMaTc
        somMaTc_neg[somMaTc_neg >= 0] <- 0
        somMaTc_pos[somMaTc_pos <= 0] <- 0

        somMaOt_neg <- somMaOt_pos <- somMaOt
        somMaOt_neg[somMaOt_neg >= 0] <- 0
        somMaOt_pos[somMaOt_pos <= 0] <- 0

        somScm_neg <- somScm_pos <- somScm
        somScm_neg[somScm_neg >= 0] <- 0
        somScm_pos[somScm_pos <= 0] <- 0

        som_neg <- somLu_neg + somMa_neg + somScm_neg
        som_pos <- somLu_pos + somMa_pos + somScm_pos

        # Split residual into negative and positive emissions
        residual_neg <- residual
        residual_neg[residual_neg >= 0] <- 0

        residual_pos <- residual
        residual_pos[residual_pos <= 0] <- 0

        regrowth_aff <- NULL
        s32_aff_plantation <- readGDX(gdx, "s32_aff_plantation")
        if (s32_aff_plantation == 0) {
            regrowth_aff <- mbind(regrowth_aff, setNames(new.magpie(getRegions(regrowth), getYears(regrowth), NULL, fill = 0, sets = getSets(regrowth)), "aff_plant"))
            regrowth_aff <- mbind(regrowth_aff, setNames(regrowth[, , "forestry_aff"], "aff_natveg"))
        } else if (s32_aff_plantation == 1) {
            regrowth_aff <- mbind(regrowth_aff, setNames(regrowth[, , "forestry_aff"], "aff_plant"))
            regrowth_aff <- mbind(regrowth_aff, setNames(new.magpie(getRegions(regrowth), getYears(regrowth), NULL, fill = 0, sets = getSets(regrowth)), "aff_natveg"))
        }

        # Above Ground / Below Ground Carbon
        totalPools   <- collapseNames(dimSums(co2[, , "total"], dim = "land"))
        climatePools <- collapseNames(dimSums(co2[, , "cc"],    dim = "land"))
        landusePools <- collapseNames(dimSums(co2[, , "lu"],    dim = "land"))

        # Calculate carbon storage in wood products - NO FILTER APPLIED
        emisWoodProducts <- carbonLTS(gdx, level = "regglo", unit = "gas", cumulative = .cumulative)[, getYears(totalNetFlux), ]

        if (!is.null(emisWoodProducts) && storageWood) {

            if (.cumulative) {
                # Convert to Gt for cumulative
                emisWoodProducts <- emisWoodProducts / 1000
            }

            # Purely industrial roundwood
            emisWoodInflow  <- collapseNames(emisWoodProducts[, , "wood_inflow"]) # inflow is negative emissions
            emisWoodOutflow <- collapseNames(emisWoodProducts[, , "wood_outflow"])
            emisWoodNet     <- collapseNames(emisWoodInflow + emisWoodOutflow)

            # Building materials
            emisBuildingInflow  <- collapseNames(emisWoodProducts[, , "building_inflow"]) # inflow is negative emissions
            emisBuildingOutflow <- collapseNames(emisWoodProducts[, , "building_outflow"])
            emisBuildingNet     <- collapseNames(emisBuildingInflow + emisBuildingOutflow)

            # Adjust top-categories
            eLanduseChange <- eLanduseChange + emisWoodNet + emisBuildingNet
            totalNetFlux   <- eLanduseChange + eClimateChange

        } else {

            dummy <- totalNetFlux
            dummy[, , ] <- 0
            emisWoodNet         <- dummy
            emisWoodInflow      <- dummy
            emisWoodOutflow     <- dummy
            emisBuildingNet     <- dummy
            emisBuildingInflow  <- dummy
            emisBuildingOutflow <- dummy

        }

        # use PeatlandEmissions for reporting to exclude emissions from intact peatlands
        # NO lowpass filter applied to peatland emissions (as in original)
        peatland <- PeatlandEmissions(gdx, cumulative = .cumulative, unit = "gas", level = "regglo", intact = FALSE)
        if (is.null(peatland)) {
            peatland <- new.magpie(getCells(co2), getYears(co2), "peatland", fill = 0)
        } else {
            peatland <- setNames(dimSums(peatland[, , c("co2", "doc")], dim = 3), "peatland")
        }

        if (.cumulative) {
            peatland <- peatland / 1000
        }

        totalNetFlux   <- totalNetFlux + peatland
        eLanduseChange <- eLanduseChange + peatland

        # Split Peatland into negative and positive emissions
        peatland_neg <- peatland
        peatland_neg[peatland_neg >= 0] <- 0

        peatland_pos <- peatland
        peatland_pos[peatland_pos <= 0] <- 0

        # generate return list
        .x <- list(
            totalNetFlux        = totalNetFlux,
            eLanduseChange      = eLanduseChange,
            eClimateChange      = eClimateChange,
            deforestation       = deforestation,
            other_conversion    = other_conversion,
            degradation         = degradation,
            regrowth            = regrowth,
            regrowth_aff        = regrowth_aff,
            harvest             = harvest,
            som                 = som,
            som_pos             = som_pos,
            som_neg             = som_neg,
            somLu               = somLu,
            somLu_pos           = somLu_pos,
            somLu_neg           = somLu_neg,
            somMa               = somMa,
            somMa_pos           = somMa_pos,
            somMa_neg           = somMa_neg,
            somMaTc             = somMaTc,
            somMaTc_pos         = somMaTc_pos,
            somMaTc_neg         = somMaTc_neg,
            somMaOt             = somMaOt,
            somMaOt_pos         = somMaOt_pos,
            somMaOt_neg         = somMaOt_neg,
            somScm              = somScm,
            somScm_pos          = somScm_pos,
            somScm_neg          = somScm_neg,
            residual            = residual,
            residual_pos        = residual_pos,
            residual_neg        = residual_neg,
            emisWoodNet         = emisWoodNet,
            emisWoodInflow      = emisWoodInflow,
            emisWoodOutflow     = emisWoodOutflow,
            emisBuildingNet     = emisBuildingNet,
            emisBuildingInflow  = emisBuildingInflow,
            emisBuildingOutflow = emisBuildingOutflow,
            peatland            = peatland,
            peatland_pos        = peatland_pos,
            peatland_neg        = peatland_neg,
            totalPools          = totalPools,
            climatePools        = climatePools,
            landusePools        = landusePools
        )

        return(.x)
    }

    # -----------------------------------------------------------------------------------------------------------------
    # Calculated indirect emissions from land-use change from a .gdx file

    .calcLandCarbonSink <- function(.lowpass = 0, .cumulative = FALSE) {
        # Use the pre-calculated co2_raw instead of calling emisCO2 again
        co2 <- co2_raw

        # Apply lowpass filter if requested and not cumulative
        if (.lowpass > 0 && !.cumulative) {
            co2 <- .applyLowpassFilter(co2, lowpass = .lowpass)
        }

        # Apply cumulative transformation if requested
        if (.cumulative) {
            timesteps <- m_yeardiff(gdx)
            co2[, "y1995", ] <- 0
            co2 <- co2 * timesteps[, getYears(co2), ]
            co2 <- as.magpie(apply(co2, c(1, 3), cumsum))
            co2 <- co2 - setYears(co2[, 1995, ], NULL)
            co2 <- co2 / 1000  # Convert to Gt
        }

        # Estimate of land-carbon sink from LPJmL
        LPJmlLCS <- co2[, , "cc", drop = TRUE]
        # NOTE: NOT summing above and belowground carbon here anymore
        # LPJmlLCS <- dimSums(LPJmlLCS, dim = 3.2) # This line is removed

        if ("other" %in% getNames(LPJmlLCS)) {
            otherSet <- "other"
        } else {
            otherSet <- c("other_othernat", "other_youngsecdf")
        }

        # Estimate of land-carbon sink from Grassi et al. (2021)
        grassiLandCarbonSink <- landCarbonSink(gdx, level = "regglo", cumulative = .cumulative)

        # If cumulative, convert Mt CO2 to Gt CO2
        if (.cumulative) {
            grassiLandCarbonSink <- grassiLandCarbonSink / 1000
        }

        # Managed land summation groupings
        managedAgCrop <- c("crop_area", "crop_fallow", "crop_treecover")
        managedAgPast <- c("past")
        managedAg     <- c(managedAgCrop, managedAgPast)
        managedForest <- c("secdforest", "forestry_aff", "forestry_ndc", "forestry_plant")
        managedLand   <- c(managedAg, managedForest, "urban")

        # Unmanaged land summation groupings
        unmanagedLand <- c("primforest", otherSet)

        # Total includes urban land
        totalLandCarbonSink <- c(managedLand, unmanagedLand)

        # Extract above and below ground carbon separately
        LPJmlLCS_above <- LPJmlLCS[, , "Above Ground Carbon", drop = TRUE]
        LPJmlLCS_below <- LPJmlLCS[, , "Below Ground Carbon", drop = TRUE]
        LPJmlLCS_total <- dimSums(LPJmlLCS, dim = 3.2)

        .x <- list(
            # Grassi data (no above/below split available)
            grassiLandCarbonSink       = grassiLandCarbonSink,

            # Total (summed above + below)
            LPJmlLandCarbonSink        = dimSums(LPJmlLCS_total[, , totalLandCarbonSink], dim = 3),
            managedLand                = dimSums(LPJmlLCS_total[, , managedLand], dim = 3),
            managedAg                  = dimSums(LPJmlLCS_total[, , managedAg], dim = 3),
            managedAgCrop              = dimSums(LPJmlLCS_total[, , managedAgCrop], dim = 3),
            managedAgCropArea          = LPJmlLCS_total[, , "crop_area", drop = TRUE],
            managedAgCropFallow        = LPJmlLCS_total[, , "crop_fallow", drop = TRUE],
            managedAgCropTreeCover     = LPJmlLCS_total[, , "crop_treecover", drop = TRUE],
            managedAgPast              = LPJmlLCS_total[, , "past", drop = TRUE],
            managedForest              = dimSums(LPJmlLCS_total[, , managedForest], dim = 3),
            managedForestSecdForest    = LPJmlLCS_total[, , "secdforest", drop = TRUE],
            managedForestForestryAff   = LPJmlLCS_total[, , "forestry_aff", drop = TRUE],
            managedForestForestryNDC   = LPJmlLCS_total[, , "forestry_ndc", drop = TRUE],
            managedForestForestryPlant = LPJmlLCS_total[, , "forestry_plant", drop = TRUE],
            managedUrban               = LPJmlLCS_total[, , "urban", drop = TRUE],
            unmanagedLand              = dimSums(LPJmlLCS_total[, , unmanagedLand], dim = 3),
            unmanagedLandPrimForest    = LPJmlLCS_total[, , "primforest", drop = TRUE],
            unmanagedLandOther         = dimSums(LPJmlLCS_total[, , otherSet, drop = TRUE], dim = 3),

            # Above Ground Carbon
            LPJmlLandCarbonSinkAboveGround        = dimSums(LPJmlLCS_above[, , totalLandCarbonSink], dim = 3),
            managedLandAboveGround                = dimSums(LPJmlLCS_above[, , managedLand], dim = 3),
            managedAgAboveGround                  = dimSums(LPJmlLCS_above[, , managedAg], dim = 3),
            managedAgCropAboveGround              = dimSums(LPJmlLCS_above[, , managedAgCrop], dim = 3),
            managedAgCropAreaAboveGround          = LPJmlLCS_above[, , "crop_area", drop = TRUE],
            managedAgCropFallowAboveGround        = LPJmlLCS_above[, , "crop_fallow", drop = TRUE],
            managedAgCropTreeCoverAboveGround     = LPJmlLCS_above[, , "crop_treecover", drop = TRUE],
            managedAgPastAboveGround              = LPJmlLCS_above[, , "past", drop = TRUE],
            managedForestAboveGround              = dimSums(LPJmlLCS_above[, , managedForest], dim = 3),
            managedForestSecdForestAboveGround    = LPJmlLCS_above[, , "secdforest", drop = TRUE],
            managedForestForestryAffAboveGround   = LPJmlLCS_above[, , "forestry_aff", drop = TRUE],
            managedForestForestryNDCAboveGround   = LPJmlLCS_above[, , "forestry_ndc", drop = TRUE],
            managedForestForestryPlantAboveGround = LPJmlLCS_above[, , "forestry_plant", drop = TRUE],
            managedUrbanAboveGround               = LPJmlLCS_above[, , "urban", drop = TRUE],
            unmanagedLandAboveGround              = dimSums(LPJmlLCS_above[, , unmanagedLand], dim = 3),
            unmanagedLandPrimForestAboveGround    = LPJmlLCS_above[, , "primforest", drop = TRUE],
            unmanagedLandOtherAboveGround         = dimSums(LPJmlLCS_above[, , otherSet, drop = TRUE], dim = 3),

            # Below Ground Carbon
            LPJmlLandCarbonSinkBelowGround        = dimSums(LPJmlLCS_below[, , totalLandCarbonSink], dim = 3),
            managedLandBelowGround                = dimSums(LPJmlLCS_below[, , managedLand], dim = 3),
            managedAgBelowGround                  = dimSums(LPJmlLCS_below[, , managedAg], dim = 3),
            managedAgCropBelowGround              = dimSums(LPJmlLCS_below[, , managedAgCrop], dim = 3),
            managedAgCropAreaBelowGround          = LPJmlLCS_below[, , "crop_area", drop = TRUE],
            managedAgCropFallowBelowGround        = LPJmlLCS_below[, , "crop_fallow", drop = TRUE],
            managedAgCropTreeCoverBelowGround     = LPJmlLCS_below[, , "crop_treecover", drop = TRUE],
            managedAgPastBelowGround              = LPJmlLCS_below[, , "past", drop = TRUE],
            managedForestBelowGround              = dimSums(LPJmlLCS_below[, , managedForest], dim = 3),
            managedForestSecdForestBelowGround    = LPJmlLCS_below[, , "secdforest", drop = TRUE],
            managedForestForestryAffBelowGround   = LPJmlLCS_below[, , "forestry_aff", drop = TRUE],
            managedForestForestryNDCBelowGround   = LPJmlLCS_below[, , "forestry_ndc", drop = TRUE],
            managedForestForestryPlantBelowGround = LPJmlLCS_below[, , "forestry_plant", drop = TRUE],
            managedUrbanBelowGround               = LPJmlLCS_below[, , "urban", drop = TRUE],
            unmanagedLandBelowGround              = dimSums(LPJmlLCS_below[, , unmanagedLand], dim = 3),
            unmanagedLandPrimForestBelowGround    = LPJmlLCS_below[, , "primforest", drop = TRUE],
            unmanagedLandOtherBelowGround         = dimSums(LPJmlLCS_below[, , otherSet, drop = TRUE], dim = 3)
        )

        return(.x)
    }

    # -----------------------------------------------------------------------------------------------------------------
    # Generate reports

    emissionsReport <- NULL

    # -----------------------------------------------------------------------------------------------------------------
    # Yearly CO2 emissions

    yearlyCO2 <- .calcCO2(.lowpass = 0, .cumulative = FALSE)

    if ("other" %in% getNames(yearlyCO2$regrowth)) {
        otherSet <- "other"
    } else {
        otherSet <- c("other_othernat", "other_youngsecdf")
    }

    # nolint start
    emissionsReport <- with(yearlyCO2, mbind(
        emissionsReport,

        setNames(totalNetFlux,   "Emissions|CO2|Land (Mt CO2/yr)"),
        setNames(eClimateChange, "Emissions|CO2|Land|+|Indirect (Mt CO2/yr)"),
        setNames(eLanduseChange, "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"),

        # Gross emissions - Deforestation
        setNames(dimSums(deforestation, dim = 3),     "Emissions|CO2|Land|Land-use Change|+|Deforestation (Mt CO2/yr)"),
        setNames(deforestation[, , "primforest"],     "Emissions|CO2|Land|Land-use Change|Deforestation|+|Primary forests (Mt CO2/yr)"),
        setNames(deforestation[, , "crop_treecover"], "Emissions|CO2|Land|Land-use Change|Deforestation|+|Cropland Tree Cover (Mt CO2/yr)"),
        setNames(deforestation[, , "secdforest"],     "Emissions|CO2|Land|Land-use Change|Deforestation|+|Secondary forests (Mt CO2/yr)"),
        setNames(deforestation[, , "forestry_plant"], "Emissions|CO2|Land|Land-use Change|Deforestation|+|Forestry plantations (Mt CO2/yr)"),

        # Gross emissions - Degradataion
        setNames(dimSums(degradation, dim = 3),       "Emissions|CO2|Land|Land-use Change|+|Forest degradation (Mt CO2/yr)"),
        setNames(degradation[, , "primforest"],       "Emissions|CO2|Land|Land-use Change|Forest degradation|+|Primary forests (Mt CO2/yr)"),
        setNames(degradation[, , "secdforest"],       "Emissions|CO2|Land|Land-use Change|Forest degradation|+|Secondary forests (Mt CO2/yr)"),

        # Gross emissions - Other conversion
        setNames(dimSums(other_conversion[, , otherSet], dim = 3), "Emissions|CO2|Land|Land-use Change|+|Other land conversion (Mt CO2/yr)"),

        # Regrowth
        setNames(dimSums(regrowth, dim = 3),          "Emissions|CO2|Land|Land-use Change|+|Regrowth (Mt CO2/yr)"),
        setNames(regrowth[, , "forestry_aff"],        "Emissions|CO2|Land|Land-use Change|Regrowth|+|CO2-price AR (Mt CO2/yr)"),
        setNames(regrowth_aff[, , "aff_natveg"],      "Emissions|CO2|Land|Land-use Change|Regrowth|CO2-price AR|+|Natural Forest (Mt CO2/yr)"),
        setNames(regrowth_aff[, , "aff_plant"],       "Emissions|CO2|Land|Land-use Change|Regrowth|CO2-price AR|+|Plantation (Mt CO2/yr)"),
        setNames(regrowth[, , "forestry_ndc"],        "Emissions|CO2|Land|Land-use Change|Regrowth|+|NPI_NDC AR (Mt CO2/yr)"),
        setNames(regrowth[, , "forestry_plant"],      "Emissions|CO2|Land|Land-use Change|Regrowth|+|Timber Plantations (Mt CO2/yr)"),
        setNames(regrowth[, , "crop_treecover"],      "Emissions|CO2|Land|Land-use Change|Regrowth|+|Cropland Tree Cover (Mt CO2/yr)"),
        setNames(regrowth[, , "secdforest"],          "Emissions|CO2|Land|Land-use Change|Regrowth|+|Secondary Forest (Mt CO2/yr)"),
        setNames(dimSums(regrowth[, , otherSet], dim = 3), "Emissions|CO2|Land|Land-use Change|Regrowth|+|Other Land (Mt CO2/yr)"),

        # Gross emissions - Peatland
        setNames(peatland,                            "Emissions|CO2|Land|Land-use Change|+|Peatland (Mt CO2/yr)"),
        setNames(peatland_pos,                        "Emissions|CO2|Land|Land-use Change|Peatland|+|Positive (Mt CO2/yr)"),
        setNames(peatland_neg,                        "Emissions|CO2|Land|Land-use Change|Peatland|+|Negative (Mt CO2/yr)"),

        # SOM
        setNames(dimSums(som, dim = 3),               "Emissions|CO2|Land|Land-use Change|+|Soil (Mt CO2/yr)"),
        setNames(dimSums(som_pos, dim = 3),           "Emissions|CO2|Land|Land-use Change|Soil|++|Emissions (Mt CO2/yr)"),
        setNames(dimSums(som_neg, dim = 3),           "Emissions|CO2|Land|Land-use Change|Soil|++|Withdrawals (Mt CO2/yr)"),

        # SOM-LU
        setNames(dimSums(somLu, dim = 3),             "Emissions|CO2|Land|Land-use Change|Soil|+|Land Conversion (Mt CO2/yr)"),
        setNames(dimSums(somLu_pos, dim = 3),         "Emissions|CO2|Land|Land-use Change|Soil|Land Conversion|+|Emissions (Mt CO2/yr)"),
        setNames(dimSums(somLu_neg, dim = 3),         "Emissions|CO2|Land|Land-use Change|Soil|Land Conversion|+|Withdrawals (Mt CO2/yr)"),

        # SOM-MA
        setNames(dimSums(somMa, dim = 3),             "Emissions|CO2|Land|Land-use Change|Soil|+|Cropland management (Mt CO2/yr)"),
        setNames(dimSums(somMa_pos, dim = 3),         "Emissions|CO2|Land|Land-use Change|Soil|Cropland management|+|Emissions (Mt CO2/yr)"),
        setNames(dimSums(somMa_neg, dim = 3),         "Emissions|CO2|Land|Land-use Change|Soil|Cropland management|+|Withdrawals (Mt CO2/yr)"),

        # SOM-SCM
        setNames(dimSums(somScm, dim = 3),            "Emissions|CO2|Land|Land-use Change|Soil|+|Soil Carbon Management (Mt CO2/yr)"),
        setNames(dimSums(somScm_pos, dim = 3),        "Emissions|CO2|Land|Land-use Change|Soil|Soil Carbon Management|+|Emissions (Mt CO2/yr)"),
        setNames(dimSums(somScm_neg, dim = 3),        "Emissions|CO2|Land|Land-use Change|Soil|Soil Carbon Management|+|Withdrawals (Mt CO2/yr)"),

        # Residual
        setNames(dimSums(residual, dim = 3),          "Emissions|CO2|Land|Land-use Change|+|Residual (Mt CO2/yr)"),
        setNames(dimSums(residual_pos, dim = 3),      "Emissions|CO2|Land|Land-use Change|Residual|+|Positive (Mt CO2/yr)"),
        setNames(dimSums(residual_neg, dim = 3),      "Emissions|CO2|Land|Land-use Change|Residual|+|Negative (Mt CO2/yr)"),

        # Carbon pools
        setNames(totalPools,                           paste0("Emissions|CO2|Land|++|", getNames(totalPools), " (Mt CO2/yr)")),
        setNames(climatePools,                         paste0("Emissions|CO2|Land|Indirect|++|", getNames(climatePools), " (Mt CO2/yr)")),
        setNames(landusePools,                         paste0("Emissions|CO2|Land|Land-use Change|++|", getNames(landusePools), " (Mt CO2/yr)"))

    ))

    # Only attempt to append wood-related reports if the forestry module was activated
    if (!is.null(yearlyCO2$harvest)) {

        emissionsReport <- with(yearlyCO2, mbind(
            emissionsReport,

            # Wood harvest CO2 emissions
            setNames(dimSums(harvest, dim = 3),               "Emissions|CO2|Land|Land-use Change|+|Wood Harvest (Mt CO2/yr)"),
            setNames(harvest[, , "forestry_plant"],           "Emissions|CO2|Land|Land-use Change|Wood Harvest|+|Timber Plantations (Mt CO2/yr)"),
            setNames(harvest[, , "primforest"],               "Emissions|CO2|Land|Land-use Change|Wood Harvest|+|Primary Forest (Mt CO2/yr)"),
            setNames(harvest[, , "secdforest"],               "Emissions|CO2|Land|Land-use Change|Wood Harvest|+|Secondary Forest (Mt CO2/yr)"),
            setNames(dimSums(harvest[, , otherSet], dim = 3), "Emissions|CO2|Land|Land-use Change|Wood Harvest|+|Other Land (Mt CO2/yr)"),

            # Carbon released from and stored in HWP
            setNames(emisWoodNet + emisBuildingNet,           "Emissions|CO2|Land|Land-use Change|+|Timber (Mt CO2/yr)"),
            setNames(emisWoodInflow + emisBuildingInflow,     "Emissions|CO2|Land|Land-use Change|Timber|+|Storage in HWP (Mt CO2/yr)"),
            setNames(emisWoodInflow,                          "Emissions|CO2|Land|Land-use Change|Timber|Storage in HWP|+|Industrial Roundwood (Mt CO2/yr)"),
            setNames(emisBuildingInflow,                      "Emissions|CO2|Land|Land-use Change|Timber|Storage in HWP|+|Buildings (Mt CO2/yr)"),
            setNames(emisWoodOutflow + emisBuildingOutflow,   "Emissions|CO2|Land|Land-use Change|Timber|+|Release from HWP (Mt CO2/yr)"),
            setNames(emisWoodOutflow,                         "Emissions|CO2|Land|Land-use Change|Timber|Release from HWP|+|Industrial Roundwood (Mt CO2/yr)"),
            setNames(emisBuildingOutflow,                     "Emissions|CO2|Land|Land-use Change|Timber|Release from HWP|+|Buildings (Mt CO2/yr)")
        ))

    }

    checkEmis <- emissionsReport[, , "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"] -
        dimSums(emissionsReport[, , c("Emissions|CO2|Land|Land-use Change|+|Deforestation (Mt CO2/yr)",
                                      "Emissions|CO2|Land|Land-use Change|+|Forest degradation (Mt CO2/yr)",
                                      "Emissions|CO2|Land|Land-use Change|+|Other land conversion (Mt CO2/yr)",
                                      "Emissions|CO2|Land|Land-use Change|+|Regrowth (Mt CO2/yr)",
                                      "Emissions|CO2|Land|Land-use Change|+|Peatland (Mt CO2/yr)",
                                      "Emissions|CO2|Land|Land-use Change|+|Soil (Mt CO2/yr)",
                                      "Emissions|CO2|Land|Land-use Change|+|Residual (Mt CO2/yr)",
                                      "Emissions|CO2|Land|Land-use Change|+|Timber (Mt CO2/yr)",
                                      "Emissions|CO2|Land|Land-use Change|+|Wood Harvest (Mt CO2/yr)")], dim = 3)

    if (any(abs(checkEmis) > 1e-03, na.rm = TRUE)) {
        warning("CO2 emission sub-categories do not add up to total")
    }

    # nolint end

    # -----------------------------------------------------------------------------------------------------------------
    # Yearly indirect CO2 emissions from land-use change (land-carbon sink) reporting

    landCarbonSink <- .calcLandCarbonSink(.lowpass = 0)

    # nolint start
    emissionsReport <- with(landCarbonSink, mbind(
        emissionsReport,

        # Total (summed above + below)
        setNames(grassiLandCarbonSink,       "Emissions|CO2|Land Carbon Sink|Grassi|Managed Land|Managed Forest (Mt CO2/yr)"),
        setNames(LPJmlLandCarbonSink,        "Emissions|CO2|Land Carbon Sink|LPJmL (Mt CO2/yr)"),
        setNames(managedLand,                "Emissions|CO2|Land Carbon Sink|LPJmL|+|Managed Land (Mt CO2/yr)"),
        setNames(managedAg,                  "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|+|Agricultural Land (Mt CO2/yr)"),
        setNames(managedAgCrop,              "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|Agricultural land|+|Cropland (Mt CO2/yr)"),
        setNames(managedAgCropArea,          "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|Agricultural land|Cropland|+|Croparea (Mt CO2/yr)"),
        setNames(managedAgCropFallow,        "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|Agricultural land|Cropland|+|Fallow (Mt CO2/yr)"),
        setNames(managedAgCropTreeCover,     "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|Agricultural land|Cropland|+|Tree Cover (Mt CO2/yr)"),
        setNames(managedAgPast,              "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|Agricultural land|+|Pasture (Mt CO2/yr)"),
        setNames(managedForest,              "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|+|Managed Forest (Mt CO2/yr)"),
        setNames(managedForestSecdForest,    "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|Managed Forest|+|Secondary Forest (Mt CO2/yr)"),
        setNames(managedForestForestryAff,   "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|Managed Forest|+|CO2-price AR (Mt CO2/yr)"),
        setNames(managedForestForestryNDC,   "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|Managed Forest|+|NPI_NDC AR (Mt CO2/yr)"),
        setNames(managedForestForestryPlant, "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|Managed Forest|+|Timber Plantations (Mt CO2/yr)"),
        setNames(managedUrban,               "Emissions|CO2|Land Carbon Sink|LPJmL|Managed Land|+|Urban Land (Mt CO2/yr)"),
        setNames(unmanagedLand,              "Emissions|CO2|Land Carbon Sink|LPJmL|+|Unmanaged Land (Mt CO2/yr)"),
        setNames(unmanagedLandPrimForest,    "Emissions|CO2|Land Carbon Sink|LPJmL|Unmanaged Land|+|Primary Forest (Mt CO2/yr)"),
        setNames(unmanagedLandOther,         "Emissions|CO2|Land Carbon Sink|LPJmL|Unmanaged Land|+|Other Land (Mt CO2/yr)"),

        # Above Ground Carbon
        setNames(LPJmlLandCarbonSinkAboveGround,        "Emissions|CO2|Land Carbon Sink|LPJmL|++|Above Ground Carbon (Mt CO2/yr)"),
        setNames(managedLandAboveGround,                "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|+|Managed Land (Mt CO2/yr)"),
        setNames(managedAgAboveGround,                  "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|+|Agricultural Land (Mt CO2/yr)"),
        setNames(managedAgCropAboveGround,              "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|Agricultural land|+|Cropland (Mt CO2/yr)"),
        setNames(managedAgCropAreaAboveGround,          "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|Agricultural land|Cropland|+|Croparea (Mt CO2/yr)"),
        setNames(managedAgCropFallowAboveGround,        "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|Agricultural land|Cropland|+|Fallow (Mt CO2/yr)"),
        setNames(managedAgCropTreeCoverAboveGround,     "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|Agricultural land|Cropland|+|Tree Cover (Mt CO2/yr)"),
        setNames(managedAgPastAboveGround,              "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|Agricultural land|+|Pasture (Mt CO2/yr)"),
        setNames(managedForestAboveGround,              "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|+|Managed Forest (Mt CO2/yr)"),
        setNames(managedForestSecdForestAboveGround,    "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|Managed Forest|+|Secondary Forest (Mt CO2/yr)"),
        setNames(managedForestForestryAffAboveGround,   "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|Managed Forest|+|CO2-price AR (Mt CO2/yr)"),
        setNames(managedForestForestryNDCAboveGround,   "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|Managed Forest|+|NPI_NDC AR (Mt CO2/yr)"),
        setNames(managedForestForestryPlantAboveGround, "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|Managed Forest|+|Timber Plantations (Mt CO2/yr)"),
        setNames(managedUrbanAboveGround,               "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Managed Land|+|Urban Land (Mt CO2/yr)"),
        setNames(unmanagedLandAboveGround,              "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|+|Unmanaged Land (Mt CO2/yr)"),
        setNames(unmanagedLandPrimForestAboveGround,    "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Unmanaged Land|+|Primary Forest (Mt CO2/yr)"),
        setNames(unmanagedLandOtherAboveGround,         "Emissions|CO2|Land Carbon Sink|LPJmL|Above Ground Carbon|Unmanaged Land|+|Other Land (Mt CO2/yr)"),

        # Below Ground Carbon
        setNames(LPJmlLandCarbonSinkBelowGround,        "Emissions|CO2|Land Carbon Sink|LPJmL|++|Below Ground Carbon (Mt CO2/yr)"),
        setNames(managedLandBelowGround,                "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|+|Managed Land (Mt CO2/yr)"),
        setNames(managedAgBelowGround,                  "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|+|Agricultural Land (Mt CO2/yr)"),
        setNames(managedAgCropBelowGround,              "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|Agricultural land|+|Cropland (Mt CO2/yr)"),
        setNames(managedAgCropAreaBelowGround,          "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|Agricultural land|Cropland|+|Croparea (Mt CO2/yr)"),
        setNames(managedAgCropFallowBelowGround,        "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|Agricultural land|Cropland|+|Fallow (Mt CO2/yr)"),
        setNames(managedAgCropTreeCoverBelowGround,     "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|Agricultural land|Cropland|+|Tree Cover (Mt CO2/yr)"),
        setNames(managedAgPastBelowGround,              "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|Agricultural land|+|Pasture (Mt CO2/yr)"),
        setNames(managedForestBelowGround,              "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|+|Managed Forest (Mt CO2/yr)"),
        setNames(managedForestSecdForestBelowGround,    "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|Managed Forest|+|Secondary Forest (Mt CO2/yr)"),
        setNames(managedForestForestryAffBelowGround,   "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|Managed Forest|+|CO2-price AR (Mt CO2/yr)"),
        setNames(managedForestForestryNDCBelowGround,   "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|Managed Forest|+|NPI_NDC AR (Mt CO2/yr)"),
        setNames(managedForestForestryPlantBelowGround, "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|Managed Forest|+|Timber Plantations (Mt CO2/yr)"),
        setNames(managedUrbanBelowGround,               "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Managed Land|+|Urban Land (Mt CO2/yr)"),
        setNames(unmanagedLandBelowGround,              "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|+|Unmanaged Land (Mt CO2/yr)"),
        setNames(unmanagedLandPrimForestBelowGround,    "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Unmanaged Land|+|Primary Forest (Mt CO2/yr)"),
        setNames(unmanagedLandOtherBelowGround,         "Emissions|CO2|Land Carbon Sink|LPJmL|Below Ground Carbon|Unmanaged Land|+|Other Land (Mt CO2/yr)")
    ))
    # nolint end

    # -----------------------------------------------------------------------------------------------------------------
    # Cumulative CO2 emissions, lowpass = 0

    cumulativeCO2 <- .calcCO2(.lowpass = 0, .cumulative = TRUE)

    if ("other" %in% getNames(cumulativeCO2$regrowth)) {
        otherSet <- "other"
    } else {
        otherSet <- c("other_othernat", "other_youngsecdf")
    }

    # nolint start
    emissionsReport <- with(cumulativeCO2, mbind(
        emissionsReport,

        setNames(totalNetFlux,                        "Emissions|CO2|Land|Cumulative (Gt CO2)"),
        setNames(eClimateChange,                      "Emissions|CO2|Land|Cumulative|+|Indirect (Gt CO2)"),
        setNames(eLanduseChange,                      "Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)"),

        # Gross emissions - Deforestation
        setNames(dimSums(deforestation, dim = 3) + dimSums(degradation, dim = 3), "Emissions|CO2|Land|Cumulative|Land-use Change|+|Deforestation (Gt CO2)"),

        # Gross emissions - Deforestation: Permanent deforestation
        setNames(dimSums(deforestation, dim = 3),     "Emissions|CO2|Land|Cumulative|Land-use Change|Deforestation|+|Permanent deforestation (Gt CO2)"),
        setNames(deforestation[, , "primforest"],     "Emissions|CO2|Land|Cumulative|Land-use Change|Deforestation|Permanent deforestation|+|Primary forests (Gt CO2)"),
        setNames(deforestation[, , "crop_treecover"], "Emissions|CO2|Land|Cumulative|Land-use Change|Deforestation|Permanent deforestation|+|Cropland Tree Cover (Gt CO2)"),
        setNames(deforestation[, , "secdforest"],     "Emissions|CO2|Land|Cumulative|Land-use Change|Deforestation|Permanent deforestation|+|Secondary forests (Gt CO2)"),
        setNames(deforestation[, , "forestry_plant"], "Emissions|CO2|Land|Cumulative|Land-use Change|Deforestation|Permanent deforestation|+|Forestry plantations (Gt CO2)"),

        # Gross emissions - Deforestation: Degradation/Shifting cultivation
        setNames(dimSums(degradation, dim = 3),       "Emissions|CO2|Land|Cumulative|Land-use Change|Deforestation|+|Forest degradation (Gt CO2)"),
        setNames(degradation[, , "primforest"],       "Emissions|CO2|Land|Cumulative|Land-use Change|Deforestation|Forest degradation|+|Primary forests (Gt CO2)"),
        setNames(degradation[, , "secdforest"],       "Emissions|CO2|Land|Cumulative|Land-use Change|Deforestation|Forest degradation|+|Secondary forests (Gt CO2)"),

        # Gross emissions - Other conversion
        setNames(dimSums(other_conversion[, , otherSet], dim = 3), "Emissions|CO2|Land|Cumulative|Land-use Change|+|Other land conversion (Gt CO2)"),

        # Regrowth
        setNames(dimSums(regrowth, dim = 3),          "Emissions|CO2|Land|Cumulative|Land-use Change|+|Regrowth (Gt CO2)"),
        setNames(regrowth[, , "forestry_aff"],        "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|+|CO2-price AR (Gt CO2)"),
        setNames(regrowth_aff[, , "aff_natveg"],      "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|CO2-price AR|+|Natural Forest (Gt CO2)"),
        setNames(regrowth_aff[, , "aff_plant"],       "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|CO2-price AR|+|Plantation (Gt CO2)"),
        setNames(regrowth[, , "forestry_ndc"],        "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|+|NPI_NDC AR (Gt CO2)"),
        setNames(regrowth[, , "forestry_plant"],      "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|+|Timber Plantations (Gt CO2)"),
        setNames(regrowth[, , "crop_treecover"],      "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|+|Cropland Tree Cover (Gt CO2)"),
        setNames(regrowth[, , "secdforest"],          "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|+|Secondary Forest (Gt CO2)"),
        setNames(dimSums(regrowth[, , otherSet], dim = 3), "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|+|Other Land (Gt CO2)"),

        # Gross emissions - Peatland
        setNames(peatland,                            "Emissions|CO2|Land|Cumulative|Land-use Change|+|Peatland (Gt CO2)"),

        # SOM
        setNames(dimSums(som, dim = 3),               "Emissions|CO2|Land|Cumulative|Land-use Change|+|Soil (Gt CO2)"),
        setNames(dimSums(som_pos, dim = 3),           "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|++|Emissions (Gt CO2)"),
        setNames(dimSums(som_neg, dim = 3),           "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|++|Withdrawals (Gt CO2)"),

        # SOM-LU
        setNames(dimSums(somLu, dim = 3),             "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|+|Land Conversion (Gt CO2)"),
        setNames(dimSums(somLu_pos, dim = 3),         "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Land Conversion|+|Emissions (Gt CO2)"),
        setNames(dimSums(somLu_neg, dim = 3),         "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Land Conversion|+|Withdrawals (Gt CO2)"),

        # SOM-MA
        setNames(dimSums(somMa, dim = 3),             "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|+|Cropland management (Gt CO2)"),
        setNames(dimSums(somMa_pos, dim = 3),         "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Cropland management|++|Emissions (Gt CO2)"),
        setNames(dimSums(somMa_neg, dim = 3),         "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Cropland management|++|Withdrawals (Gt CO2)"),
        # SOM-MA-treecover
        setNames(dimSums(somMaTc, dim = 3),           "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Cropland management|+|Treecover (Gt CO2/yr)"),
        setNames(dimSums(somMaTc_pos, dim = 3),       "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Cropland management|Treecover|+|Emissions (Gt CO2/yr)"),
        setNames(dimSums(somMaTc_neg, dim = 3),       "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Cropland management|Treecover|+|Withdrawals (Gt CO2/yr)"),
        # SOM-MA-other Management
        setNames(dimSums(somMaOt, dim = 3),           "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Cropland management|+|Other than treecover (Gt CO2/yr)"),
        setNames(dimSums(somMaOt_pos, dim = 3),       "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Cropland management|Other than treecover|+|Emissions (Gt CO2/yr)"),
        setNames(dimSums(somMaOt_neg, dim = 3),       "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Cropland management|Other than treecover|+|Withdrawals (Gt CO2/yr)"),

        # SOM-SCM
        setNames(dimSums(somScm, dim = 3),            "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|+|Soil Carbon Management (Gt CO2)"),
        setNames(dimSums(somScm_pos, dim = 3),        "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Soil Carbon Management|+|Emissions (Gt CO2)"),
        setNames(dimSums(somScm_neg, dim = 3),        "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Soil Carbon Management|+|Withdrawals (Gt CO2)"),

        # residual
        setNames(dimSums(residual, dim = 3),          "Emissions|CO2|Land|Cumulative|Land-use Change|+|Residual (Gt CO2)"),

        # Carbon pools
        setNames(totalPools,                          paste0("Emissions|CO2|Land|Cumulative|++|", getNames(totalPools), " (Gt CO2)")),
        setNames(climatePools,                        paste0("Emissions|CO2|Land|Cumulative|Indirect|++|", getNames(climatePools), " (Gt CO2)")),
        setNames(landusePools,                        paste0("Emissions|CO2|Land|Cumulative|Land-use Change|++|", getNames(landusePools), " (Gt CO2)"))

    ))

    # Only attempt to append wood-related reports if the forestry module was activated
    if (!is.null(cumulativeCO2$harvest)) {

        emissionsReport <- with(cumulativeCO2, mbind(
            emissionsReport,

            # Wood harvest CO2 emissions
            setNames(dimSums(harvest, dim = 3),               "Emissions|CO2|Land|Cumulative|Land-use Change|+|Wood Harvest (Gt CO2)"),
            setNames(harvest[, , "forestry_plant"],           "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|+|Timber Plantations (Gt CO2)"),
            setNames(harvest[, , "primforest"],               "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|+|Primary Forest (Gt CO2)"),
            setNames(harvest[, , "secdforest"],               "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|+|Secondary Forest (Gt CO2)"),
            setNames(dimSums(harvest[, , otherSet], dim = 3), "Emissions|CO2|Land|Cumulative|Land-use Change|Wood Harvest|+|Other Land (Gt CO2)"),

            # Carbon released from and stored in HWP
            setNames(emisWoodNet + emisBuildingNet,           "Emissions|CO2|Land|Cumulative|Land-use Change|+|Timber (Gt CO2)"),
            setNames(emisWoodInflow + emisBuildingInflow,     "Emissions|CO2|Land|Cumulative|Land-use Change|Timber|+|Storage in HWP (Gt CO2)"),
            setNames(emisWoodInflow,                          "Emissions|CO2|Land|Cumulative|Land-use Change|Timber|Storage in HWP|+|Industrial Roundwood (Gt CO2)"),
            setNames(emisBuildingInflow,                      "Emissions|CO2|Land|Cumulative|Land-use Change|Timber|Storage in HWP|+|Buildings (Gt CO2)"),
            setNames(emisWoodOutflow + emisBuildingOutflow,   "Emissions|CO2|Land|Cumulative|Land-use Change|Timber|+|Release from HWP (Gt CO2)"),
            setNames(emisWoodOutflow,                         "Emissions|CO2|Land|Cumulative|Land-use Change|Timber|Release from HWP|+|Industrial Roundwood (Gt CO2)"),
            setNames(emisBuildingOutflow,                     "Emissions|CO2|Land|Cumulative|Land-use Change|Timber|Release from HWP|+|Buildings (Gt CO2)")
        ))

    }

    checkEmis <- emissionsReport[, , "Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)"] -
        dimSums(emissionsReport[, , c("Emissions|CO2|Land|Cumulative|Land-use Change|+|Deforestation (Gt CO2)",
                                      "Emissions|CO2|Land|Cumulative|Land-use Change|+|Other land conversion (Gt CO2)",
                                      "Emissions|CO2|Land|Cumulative|Land-use Change|+|Regrowth (Gt CO2)",
                                      "Emissions|CO2|Land|Cumulative|Land-use Change|+|Peatland (Gt CO2)",
                                      "Emissions|CO2|Land|Cumulative|Land-use Change|+|Soil (Gt CO2)",
                                      "Emissions|CO2|Land|Cumulative|Land-use Change|+|Residual (Gt CO2)",
                                      "Emissions|CO2|Land|Cumulative|Land-use Change|+|Timber (Gt CO2)",
                                      "Emissions|CO2|Land|Cumulative|Land-use Change|+|Wood Harvest (Gt CO2)")], dim = 3)

    if (any(abs(checkEmis) > 1e-03, na.rm = TRUE)) {
        warning("CO2 emission sub-categories do not add up to total")
    }

    # nolint end

    # -----------------------------------------------------------------------------------------------------------------
    # Cumulative indirect CO2 emissions from land-use change (land-carbon sink) reporting

    cumulativeLandCarbonSink <- .calcLandCarbonSink(.lowpass = 0, .cumulative = TRUE)

    # nolint start
    emissionsReport <- with(cumulativeLandCarbonSink, mbind(
        emissionsReport,

        # Total (summed above + below)
        setNames(grassiLandCarbonSink,       "Emissions|CO2|Land Carbon Sink|Cumulative|Grassi (Gt CO2)"),
        setNames(LPJmlLandCarbonSink,        "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL (Gt CO2)"),
        setNames(managedLand,                "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|+|Managed Land (Gt CO2)"),
        setNames(managedAg,                  "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|+|Agricultural Land (Gt CO2)"),
        setNames(managedAgCrop,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|Agricultural land|+|Cropland (Gt CO2)"),
        setNames(managedAgCropArea,          "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|Agricultural land|Cropland|+|Croparea (Gt CO2)"),
        setNames(managedAgCropFallow,        "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|Agricultural land|Cropland|+|Fallow (Gt CO2)"),
        setNames(managedAgCropTreeCover,     "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|Agricultural land|Cropland|+|Tree Cover (Gt CO2)"),
        setNames(managedAgPast,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|Agricultural land|+|Pasture (Gt CO2)"),
        setNames(managedForest,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|+|Managed Forest (Gt CO2)"),
        setNames(managedForestSecdForest,    "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|Managed Forest|+|Secondary Forest (Gt CO2)"),
        setNames(managedForestForestryAff,   "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|Managed Forest|+|CO2-price AR (Gt CO2)"),
        setNames(managedForestForestryNDC,   "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|Managed Forest|+|NPI_NDC AR (Gt CO2)"),
        setNames(managedForestForestryPlant, "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|Managed Forest|+|Timber Plantations (Gt CO2)"),
        setNames(managedUrban,               "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Managed Land|+|Urban Land (Gt CO2)"),
        setNames(unmanagedLand,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|+|Unmanaged Land (Gt CO2)"),
        setNames(unmanagedLandPrimForest,    "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Unmanaged Land|+|Primary Forest (Gt CO2)"),
        setNames(unmanagedLandOther,         "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Unmanaged Land|+|Other Land (Gt CO2)"),

        # Above Ground Carbon
        setNames(LPJmlLandCarbonSinkAboveGround,        "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|++|Above Ground Carbon (Gt CO2)"),
        setNames(managedLandAboveGround,                "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|+|Managed Land (Gt CO2)"),
        setNames(managedAgAboveGround,                  "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|+|Agricultural Land (Gt CO2)"),
        setNames(managedAgCropAboveGround,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|Agricultural land|+|Cropland (Gt CO2)"),
        setNames(managedAgCropAreaAboveGround,          "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|Agricultural land|Cropland|+|Croparea (Gt CO2)"),
        setNames(managedAgCropFallowAboveGround,        "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|Agricultural land|Cropland|+|Fallow (Gt CO2)"),
        setNames(managedAgCropTreeCoverAboveGround,     "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|Agricultural land|Cropland|+|Tree Cover (Gt CO2)"),
        setNames(managedAgPastAboveGround,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|Agricultural land|+|Pasture (Gt CO2)"),
        setNames(managedForestAboveGround,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|+|Managed Forest (Gt CO2)"),
        setNames(managedForestSecdForestAboveGround,    "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|Managed Forest|+|Secondary Forest (Gt CO2)"),
        setNames(managedForestForestryAffAboveGround,   "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|Managed Forest|+|CO2-price AR (Gt CO2)"),
        setNames(managedForestForestryNDCAboveGround,   "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|Managed Forest|+|NPI_NDC AR (Gt CO2)"),
        setNames(managedForestForestryPlantAboveGround, "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|Managed Forest|+|Timber Plantations (Gt CO2)"),
        setNames(managedUrbanAboveGround,               "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Managed Land|+|Urban Land (Gt CO2)"),
        setNames(unmanagedLandAboveGround,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|+|Unmanaged Land (Gt CO2)"),
        setNames(unmanagedLandPrimForestAboveGround,    "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Unmanaged Land|+|Primary Forest (Gt CO2)"),
        setNames(unmanagedLandOtherAboveGround,         "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Above Ground Carbon|Unmanaged Land|+|Other Land (Gt CO2)"),

        # Below Ground Carbon
        setNames(LPJmlLandCarbonSinkBelowGround,        "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|++|Below Ground Carbon (Gt CO2)"),
        setNames(managedLandBelowGround,                "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|+|Managed Land (Gt CO2)"),
        setNames(managedAgBelowGround,                  "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|+|Agricultural Land (Gt CO2)"),
        setNames(managedAgCropBelowGround,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|Agricultural land|+|Cropland (Gt CO2)"),
        setNames(managedAgCropAreaBelowGround,          "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|Agricultural land|Cropland|+|Croparea (Gt CO2)"),
        setNames(managedAgCropFallowBelowGround,        "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|Agricultural land|Cropland|+|Fallow (Gt CO2)"),
        setNames(managedAgCropTreeCoverBelowGround,     "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|Agricultural land|Cropland|+|Tree Cover (Gt CO2)"),
        setNames(managedAgPastBelowGround,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|Agricultural land|+|Pasture (Gt CO2)"),
        setNames(managedForestBelowGround,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|+|Managed Forest (Gt CO2)"),
        setNames(managedForestSecdForestBelowGround,    "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|Managed Forest|+|Secondary Forest (Gt CO2)"),
        setNames(managedForestForestryAffBelowGround,   "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|Managed Forest|+|CO2-price AR (Gt CO2)"),
        setNames(managedForestForestryNDCBelowGround,   "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|Managed Forest|+|NPI_NDC AR (Gt CO2)"),
        setNames(managedForestForestryPlantBelowGround, "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|Managed Forest|+|Timber Plantations (Gt CO2)"),
        setNames(managedUrbanBelowGround,               "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Managed Land|+|Urban Land (Gt CO2)"),
        setNames(unmanagedLandBelowGround,              "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|+|Unmanaged Land (Gt CO2)"),
        setNames(unmanagedLandPrimForestBelowGround,    "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Unmanaged Land|+|Primary Forest (Gt CO2)"),
        setNames(unmanagedLandOtherBelowGround,         "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Below Ground Carbon|Unmanaged Land|+|Other Land (Gt CO2)")
    ))

    # nolint end

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

    agriculture <- c("SOM", "inorg_fert", "man_crop", "awms", "resid", "man_past", "rice")
    burn <- c("resid_burn")
    peatland_n2o <- c("peatland")

    nEmissions <- Emissions(gdx, level = "regglo", type = .type, unit = "gas", subcategories = TRUE, inorg_fert_split = TRUE)
    # use PeatlandEmissions for reporting to exclude emissions from intact peatlands
    if (.type %in% c("n2o_n", "n2o_n_direct")) {
      if ("peatland" %in% getItems(nEmissions, dim = 3.1)) nEmissions <- nEmissions[, , "peatland", invert = TRUE]
      peatlandN2O <- PeatlandEmissions(gdx, unit = "gas", level = "regglo", intact = FALSE)
      if (is.null(peatlandN2O)) {
        nEmissions <- add_columns(nEmissions, addnm = "peatland", dim = "emis_source", fill = 0)
      } else {
        nEmissions <- add_columns(nEmissions, addnm = "peatland", dim = "emis_source", fill = 0)
        nEmissions[, , "peatland"] <- collapseNames(peatlandN2O[, , "n2o"])
      }
    }

    # nolint start
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
    # nolint end

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

  ### add dummy variables with zero emissions for Emissions|("BC", "CO", "CO2", "OC", "SO2", "VOC)|AFOLU|Agriculture
  dummy <- emissionsReport[,,1]
  dummy[,,] <- 0
  emissionsReport <- mbind(emissionsReport,
                           setNames(dummy, "Emissions|BC|AFOLU|Agriculture (Mt BC/yr)"),
                           setNames(dummy, "Emissions|CO|AFOLU|Agriculture (Mt CO/yr)"),
                           setNames(dummy, "Emissions|CO2|AFOLU|Agriculture (Mt CO2/yr)"),
                           setNames(dummy, "Emissions|OC|AFOLU|Agriculture (Mt OC/yr)"),
                           setNames(dummy, "Emissions|SO2|AFOLU|Agriculture (Mt SO2/yr)"),
                           setNames(dummy, "Emissions|VOC|AFOLU|Agriculture (Mt VOC/yr)"))

  # -----------------------------------------------------------------------------------------------------------------
  # CH4 emissions reporting

  agricult_ch4 <- c("rice", "awms", "ent_ferm")
  burn_ch4 <- c("resid_burn")
  peatland_ch4 <- "peatland"

  # combine all CH4 emissions in one object
  ch4 <- collapseNames(Emissions(gdx, level = "regglo", type = "ch4", unit = "gas", subcategories = TRUE), collapsedim = 2)
  # use PeatlandEmissions for reporting to exclude emissions from intact peatlands
  if ("peatland" %in% getNames(ch4, dim = 1)) ch4 <- ch4[, , "peatland", invert = TRUE]
  peatlandCH4 <- PeatlandEmissions(gdx, unit = "gas", level = "regglo", intact = FALSE)
  if (is.null(peatlandCH4)) {
    ch4 <- add_columns(ch4, addnm = "peatland", dim = "emis_source", fill = 0)
  } else {
    peatlandCH4 <- setNames(collapseNames(peatlandCH4[, , "ch4"]), "peatland")
    ch4 <- mbind(ch4, peatlandCH4)
  }

  # nolint start
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
  # nolint end

  # -----------------------------------------------------------------------------------------------------------------
  # CH4 GWP100AR6 emissions reporting

  ch4_GWP100AR6 <- ch4 * 27

  # nolint start
  emissionsReport <- mbind(
    emissionsReport,
    setNames(dimSums(ch4_GWP100AR6[, , c(agricult_ch4, burn_ch4, peatland_ch4)], dim = 3), "Emissions|CH4_GWP100AR6|Land (Mt CO2e/yr)"),
    setNames(dimSums(ch4_GWP100AR6[, , agricult_ch4], dim = 3),                            "Emissions|CH4_GWP100AR6|Land|+|Agriculture (Mt CO2e/yr)"),
    setNames(dimSums(ch4_GWP100AR6[, , c("rice")], dim = 3),                               "Emissions|CH4_GWP100AR6|Land|Agriculture|+|Rice (Mt CO2e/yr)"),
    setNames(dimSums(ch4_GWP100AR6[, , c("awms")], dim = 3),                               "Emissions|CH4_GWP100AR6|Land|Agriculture|+|Animal waste management (Mt CO2e/yr)"),
    setNames(dimSums(ch4_GWP100AR6[, , c("ent_ferm")], dim = 3),                           "Emissions|CH4_GWP100AR6|Land|Agriculture|+|Enteric fermentation (Mt CO2e/yr)"),
    setNames(dimSums(ch4_GWP100AR6[, , c(burn_ch4)], dim = 3),                             "Emissions|CH4_GWP100AR6|Land|+|Biomass Burning (Mt CO2e/yr)"),
    setNames(dimSums(ch4_GWP100AR6[, , c("resid_burn")], dim = 3),                         "Emissions|CH4_GWP100AR6|Land|Biomass Burning|+|Burning of Crop Residues (Mt CO2e/yr)"),
    setNames(dimSums(ch4_GWP100AR6[, , c(peatland_ch4)], dim = 3),                         "Emissions|CH4_GWP100AR6|Land|+|Peatland (Mt CO2e/yr)"),
    setNames(dimSums(ch4_GWP100AR6[, , c("peatland")], dim = 3),                           "Emissions|CH4_GWP100AR6|Land|Peatland|+|Managed (Mt CO2e/yr)")
  )
  # nolint end

  # -----------------------------------------------------------------------------------------------------------------
  # N2O GWP100AR6 emissions reporting

  .generateGWPN2O <- function(.unit) {

    agriculture <- c("SOM", "inorg_fert", "man_crop", "awms", "resid", "man_past", "rice")

    emissions <- Emissions(gdx, level = "regglo", type = "n2o_n", unit = .unit, subcategories = TRUE)
    emissions <- collapseNames(emissions, collapsedim = 2)

    # disaggregate N2O emissions to croparea by crop using Nitrogen withdrawals as weight
    withdrawalN  <- collapseNames(NitrogenBudgetWithdrawals(gdx, kcr = "kcr", net = TRUE, level = "regglo"))
    emisCroparea <- dimSums(emissions[, , c("SOM", "inorg_fert", "man_crop", "awms", "resid", "rice")], dim = 3)
    mapping <- data.frame(kall = getItems(withdrawalN, dim = 3),
                          d3 = rep("NULL", length(getItems(withdrawalN, dim = 3))))
    emisCropareabycrop <- toolAggregate(x = emisCroparea, rel = mapping,
                                        weight = withdrawalN,
                                        from = "d3", to = "kall", dim = 3)
    emisCropareabycrop <- reporthelper(emisCropareabycrop, dim = 3.1,
                                       level_zero_name = "Emissions|N2O_GWP100AR6|Land|Agriculture|Croparea",
                                       detail = TRUE, sort = FALSE, partly = FALSE, version = NULL)
    getItems(emisCropareabycrop, dim = 3) <- paste0(getItems(emisCropareabycrop, dim = 3), " (Mt CO2e/yr)")

    .createReport <- function(.emission, .name = NULL) {
      t <- dimSums(emissions[, , .emission], dim = 3)
      n <- paste0("Emissions|N2O_", .unit, "|Land", .name, " (Mt CO2e/yr)")
      return(setNames(t, n))
    }

    # nolint start
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
      .createReport(c("peatland"),                                                    "|Peatland|+|Managed"),
      emisCropareabycrop
    )
    # nolint end

    return(.x)
  }

  emissionsReport <- mbind(
    emissionsReport,
    .generateGWPN2O("GWP100AR6")
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
