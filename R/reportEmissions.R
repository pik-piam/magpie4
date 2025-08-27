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
reportEmissions <- function(gdx, storageWood = TRUE) {
  # -----------------------------------------------------------------------------------------------------------------
  # Calculate CO2 emissions from a MAgPIE .gdx file

  # landuseChange is mostly positive (deforestation) and regrowth is mostly negative (regrowth/afforestation),
  #   but there are some cases that behave differently.

  # landuseChange: cropland-to-pasture conversion causes negative emissions in soilc

  # regrowth: Litter carbon can decrease in case of afforestation/regrowth because the starting
  #   level of litter carbon is always pasture litc. If pasture litc is higher than natveg
  #   litc, this results in positive emissions.

  .calcCO2 <- function(.lowpass = 3, .cumulative = FALSE, .raw = FALSE, .landCarbonSink = "grassi") {

    co2 <- emisCO2(gdx,
                   level = "regglo", unit = "gas", sum_land = FALSE, sum_cpool = FALSE,
                   lowpass = .lowpass, cumulative = .cumulative)

    if (.landCarbonSink == "grassi") {
      # To ensure consistency with national forest inventories, we replace our estimates of indirect emissions
      # from climate change (on all MAgPIE land-use types) with the emission land-carbon sink of
      # Grassi et al. (2021), which includes only managed forest. At the moment this is default behavior.
      eClimateChange <- landCarbonSink(gdx, level = "regglo", cumulative = .cumulative)
    } else {
      eClimateChange <- dimSums(co2[, , "cc"], dim = 3)
    }

    # If cumulative, convert Mt CO2 to Gt CO2
    if (.cumulative) {
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
    somScm           <- collapseNames(dimSums(co2[, , "lu_som_scm"],          dim = "c_pools"))

    # Split SOM into negative and positive emissions
    somLu_neg <- somLu_pos <- somLu
    somLu_neg[somLu_neg >= 0] <- 0
    somLu_pos[somLu_pos <= 0] <- 0

    somMa_neg <- somMa_pos <- somMa
    somMa_neg[somMa_neg >= 0] <- 0
    somMa_pos[somMa_pos <= 0] <- 0

    somScm_neg <- somScm_pos <- somScm
    somScm_neg[somScm_neg >= 0] <- 0
    somScm_pos[somScm_pos <= 0] <- 0

    som_neg <- som_pos <- som
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

    # Calculate carbon storage in wood products, if forestry module was activated and desired
    emisWoodProducts <- carbonLTS(gdx, level = "regglo", unit = "gas", cumulative = .cumulative)[, getYears(totalNetFlux), ]

    if (!is.null(emisWoodProducts) && storageWood) {

      if (.cumulative) {
        # Can't divide by 1000 during cumulative calc as in def runs its NULL and NULL/1000 is numeric(0)
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
    # No lowpass filter applied to peatland emissions
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
  # Generate reports

  emissionsReport <- NULL

  # -----------------------------------------------------------------------------------------------------------------
  # Yearly CO2 emissions, lowpass = 3

  yearlyCO2 <- .calcCO2(.lowpass = 3, .cumulative = FALSE)

  if ("other" %in% getNames(yearlyCO2$regrowth)) {
    otherSet <- "other"
  } else {
    otherSet <- c("other_othernat", "other_youngsecdf")
  }

  # nolint start
  emissionsReport <- with(yearlyCO2, mbind(
    emissionsReport,

    setNames(totalNetFlux,                        "Emissions|CO2|Land (Mt CO2/yr)"), # all human-induced land-related CO2 emissions
    setNames(eClimateChange,                      "Emissions|CO2|Land|+|Indirect (Mt CO2/yr)"), # indirect human-induced CO2 emissions: environmental change, climate change, natural effects
    setNames(eLanduseChange,                      "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"), # direct human-induced CO2 emissions, includes land-use change, land management and regrowth of vegetation

    # Deforestation
    setNames(dimSums(deforestation, dim = 3) + dimSums(degradation, dim = 3), "Emissions|CO2|Land|Land-use Change|+|Deforestation (Mt CO2/yr)"),

    # Gross emissions - Deforestation: Permanent deforestation
    setNames(dimSums(deforestation, dim = 3),     "Emissions|CO2|Land|Land-use Change|Deforestation|+|Permanent deforestation (Mt CO2/yr)"),
    setNames(deforestation[, , "primforest"],     "Emissions|CO2|Land|Land-use Change|Deforestation|Permanent deforestation|+|Primary forests (Mt CO2/yr)"),
    setNames(deforestation[, , "crop_treecover"], "Emissions|CO2|Land|Land-use Change|Deforestation|Permanent deforestation|+|Cropland Tree Cover (Mt CO2/yr)"),
    setNames(deforestation[, , "secdforest"],     "Emissions|CO2|Land|Land-use Change|Deforestation|Permanent deforestation|+|Secondary forests (Mt CO2/yr)"),
    setNames(deforestation[, , "forestry_plant"], "Emissions|CO2|Land|Land-use Change|Deforestation|Permanent deforestation|+|Forestry plantations (Mt CO2/yr)"),

    # Gross emissions - Deforestation: Degradation/Shifting cultivation
    setNames(dimSums(degradation, dim = 3),       "Emissions|CO2|Land|Land-use Change|Deforestation|+|Forest degradation (Mt CO2/yr)"),
    setNames(degradation[, , "primforest"],       "Emissions|CO2|Land|Land-use Change|Deforestation|Forest degradation|+|Primary forests (Mt CO2/yr)"),
    setNames(degradation[, , "secdforest"],       "Emissions|CO2|Land|Land-use Change|Deforestation|Forest degradation|+|Secondary forests (Mt CO2/yr)"),

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
      # Note: Carbon sequestration due to regrowth of timber plantations and natural forest after harvest are reported under `Land-use Change|Regrowth|+|Timber Plantations` and `Land-use Change|Regrowth|+|Secondary Forest`.
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
  # RAW yearly CO2 emissions, lowpass = 0

  rawYearlyCO2 <- .calcCO2(.lowpass = 0, .cumulative = FALSE)

  if ("other" %in% getNames(rawYearlyCO2$regrowth)) {
    otherSet <- "other"
  } else {
    otherSet <- c("other_othernat", "other_youngsecdf")
  }

  # nolint start
  emissionsReport <- with(rawYearlyCO2, mbind(
    emissionsReport,

    setNames(totalNetFlux,   "Emissions|CO2|Land RAW (Mt CO2/yr)"),
    setNames(eClimateChange, "Emissions|CO2|Land RAW|+|Indirect (Mt CO2/yr)"),
    setNames(eLanduseChange, "Emissions|CO2|Land RAW|+|Land-use Change (Mt CO2/yr)"),

    # Gross emissions - Deforestation
    setNames(dimSums(deforestation, dim = 3),     "Emissions|CO2|Land RAW|Land-use Change|+|Deforestation (Mt CO2/yr)"),
    setNames(deforestation[, , "primforest"],     "Emissions|CO2|Land RAW|Land-use Change|Deforestation|+|Primary forests (Mt CO2/yr)"),
    setNames(deforestation[, , "crop_treecover"], "Emissions|CO2|Land RAW|Land-use Change|Deforestation|+|Cropland Tree Cover (Mt CO2/yr)"),
    setNames(deforestation[, , "secdforest"],     "Emissions|CO2|Land RAW|Land-use Change|Deforestation|+|Secondary forests (Mt CO2/yr)"),
    setNames(deforestation[, , "forestry_plant"], "Emissions|CO2|Land RAW|Land-use Change|Deforestation|+|Forestry plantations (Mt CO2/yr)"),

    # Gross emissions - Degradataion
    setNames(dimSums(degradation, dim = 3),       "Emissions|CO2|Land RAW|Land-use Change|+|Forest degradation (Mt CO2/yr)"),
    setNames(degradation[, , "primforest"],       "Emissions|CO2|Land RAW|Land-use Change|Forest degradation|+|Primary forests (Mt CO2/yr)"),
    setNames(degradation[, , "secdforest"],       "Emissions|CO2|Land RAW|Land-use Change|Forest degradation|+|Secondary forests (Mt CO2/yr)"),

    # Gross emissions - Other conversion
    setNames(dimSums(other_conversion[, , otherSet], dim = 3), "Emissions|CO2|Land RAW|Land-use Change|+|Other land conversion (Mt CO2/yr)"),

    # Regrowth
    setNames(dimSums(regrowth, dim = 3),          "Emissions|CO2|Land RAW|Land-use Change|+|Regrowth (Mt CO2/yr)"),
    setNames(regrowth[, , "forestry_aff"],        "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|CO2-price AR (Mt CO2/yr)"),
    setNames(regrowth_aff[, , "aff_natveg"],      "Emissions|CO2|Land RAW|Land-use Change|Regrowth|CO2-price AR|+|Natural Forest (Mt CO2/yr)"),
    setNames(regrowth_aff[, , "aff_plant"],       "Emissions|CO2|Land RAW|Land-use Change|Regrowth|CO2-price AR|+|Plantation (Mt CO2/yr)"),
    setNames(regrowth[, , "forestry_ndc"],        "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|NPI_NDC AR (Mt CO2/yr)"),
    setNames(regrowth[, , "forestry_plant"],      "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|Timber Plantations (Mt CO2/yr)"),
    setNames(regrowth[, , "crop_treecover"],      "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|Cropland Tree Cover (Mt CO2/yr)"),
    setNames(regrowth[, , "secdforest"],          "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|Secondary Forest (Mt CO2/yr)"),
    setNames(dimSums(regrowth[, , otherSet], dim = 3), "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|Other Land (Mt CO2/yr)"),

    # Gross emissions - Peatland
    setNames(peatland,                            "Emissions|CO2|Land RAW|Land-use Change|+|Peatland (Mt CO2/yr)"),
    setNames(peatland_pos,                        "Emissions|CO2|Land RAW|Land-use Change|Peatland|+|Positive (Mt CO2/yr)"),
    setNames(peatland_neg,                        "Emissions|CO2|Land RAW|Land-use Change|Peatland|+|Negative (Mt CO2/yr)"),

    # SOM
    setNames(dimSums(som, dim = 3),               "Emissions|CO2|Land RAW|Land-use Change|+|Soil (Mt CO2/yr)"),
    setNames(dimSums(som_pos, dim = 3),           "Emissions|CO2|Land RAW|Land-use Change|Soil|++|Emissions (Mt CO2/yr)"),
    setNames(dimSums(som_neg, dim = 3),           "Emissions|CO2|Land RAW|Land-use Change|Soil|++|Withdrawals (Mt CO2/yr)"),

    # SOM-LU
    setNames(dimSums(somLu, dim = 3),             "Emissions|CO2|Land RAW|Land-use Change|Soil|+|Land Conversion (Mt CO2/yr)"),
    setNames(dimSums(somLu_pos, dim = 3),         "Emissions|CO2|Land RAW|Land-use Change|Soil|Land Conversion|+|Emissions (Mt CO2/yr)"),
    setNames(dimSums(somLu_neg, dim = 3),         "Emissions|CO2|Land RAW|Land-use Change|Soil|Land Conversion|+|Withdrawals (Mt CO2/yr)"),

    # SOM-MA
    setNames(dimSums(somMa, dim = 3),             "Emissions|CO2|Land RAW|Land-use Change|Soil|+|Cropland management (Mt CO2/yr)"),
    setNames(dimSums(somMa_pos, dim = 3),         "Emissions|CO2|Land RAW|Land-use Change|Soil|Cropland management|+|Emissions (Mt CO2/yr)"),
    setNames(dimSums(somMa_neg, dim = 3),         "Emissions|CO2|Land RAW|Land-use Change|Soil|Cropland management|+|Withdrawals (Mt CO2/yr)"),

    # SOM-SCM
    setNames(dimSums(somScm, dim = 3),            "Emissions|CO2|Land RAW|Land-use Change|Soil|+|Soil Carbon Management (Mt CO2/yr)"),
    setNames(dimSums(somScm_pos, dim = 3),        "Emissions|CO2|Land RAW|Land-use Change|Soil|Soil Carbon Management|+|Emissions (Mt CO2/yr)"),
    setNames(dimSums(somScm_neg, dim = 3),        "Emissions|CO2|Land RAW|Land-use Change|Soil|Soil Carbon Management|+|Withdrawals (Mt CO2/yr)"),

    # Residual
    setNames(dimSums(residual, dim = 3),          "Emissions|CO2|Land RAW|Land-use Change|+|Residual (Mt CO2/yr)"),
    setNames(dimSums(residual_pos, dim = 3),      "Emissions|CO2|Land RAW|Land-use Change|Residual|+|Positive (Mt CO2/yr)"),
    setNames(dimSums(residual_neg, dim = 3),      "Emissions|CO2|Land RAW|Land-use Change|Residual|+|Negative (Mt CO2/yr)"),

    # Carbon pools
    setNames(totalPools,                           paste0("Emissions|CO2|Land RAW|++|", getNames(totalPools), " (Mt CO2/yr)")),
    setNames(climatePools,                         paste0("Emissions|CO2|Land RAW|Indirect|++|", getNames(climatePools), " (Mt CO2/yr)")),
    setNames(landusePools,                         paste0("Emissions|CO2|Land RAW|Land-use Change|++|", getNames(landusePools), " (Mt CO2/yr)"))

  ))

  # Only attempt to append wood-related reports if the forestry module was activated
  if (!is.null(rawYearlyCO2$harvest)) {

    emissionsReport <- with(rawYearlyCO2, mbind(
      emissionsReport,

      # Wood harvest CO2 emissions
      setNames(dimSums(harvest, dim = 3),               "Emissions|CO2|Land RAW|Land-use Change|+|Wood Harvest (Mt CO2/yr)"),
      setNames(harvest[, , "forestry_plant"],           "Emissions|CO2|Land RAW|Land-use Change|Wood Harvest|+|Timber Plantations (Mt CO2/yr)"),
      setNames(harvest[, , "primforest"],               "Emissions|CO2|Land RAW|Land-use Change|Wood Harvest|+|Primary Forest (Mt CO2/yr)"),
      setNames(harvest[, , "secdforest"],               "Emissions|CO2|Land RAW|Land-use Change|Wood Harvest|+|Secondary Forest (Mt CO2/yr)"),
      setNames(dimSums(harvest[, , otherSet], dim = 3), "Emissions|CO2|Land RAW|Land-use Change|Wood Harvest|+|Other Land (Mt CO2/yr)"),

      # Carbon released from and stored in HWP
      # Note: Carbon sequestration due to regrowth of timber plantations and natural forest after harvest are reported under `Land-use Change|Regrowth|+|Timber Plantations` and `Land-use Change|Regrowth|+|Secondary Forest`.
      setNames(emisWoodNet + emisBuildingNet,           "Emissions|CO2|Land RAW|Land-use Change|+|Timber (Mt CO2/yr)"),
      setNames(emisWoodInflow + emisBuildingInflow,     "Emissions|CO2|Land RAW|Land-use Change|Timber|+|Storage in HWP (Mt CO2/yr)"),
      setNames(emisWoodInflow,                          "Emissions|CO2|Land RAW|Land-use Change|Timber|Storage in HWP|+|Industrial Roundwood (Mt CO2/yr)"),
      setNames(emisBuildingInflow,                      "Emissions|CO2|Land RAW|Land-use Change|Timber|Storage in HWP|+|Buildings (Mt CO2/yr)"),
      setNames(emisWoodOutflow + emisBuildingOutflow,   "Emissions|CO2|Land RAW|Land-use Change|Timber|+|Release from HWP (Mt CO2/yr)"),
      setNames(emisWoodOutflow,                         "Emissions|CO2|Land RAW|Land-use Change|Timber|Release from HWP|+|Industrial Roundwood (Mt CO2/yr)"),
      setNames(emisBuildingOutflow,                     "Emissions|CO2|Land RAW|Land-use Change|Timber|Release from HWP|+|Buildings (Mt CO2/yr)")
    ))

  }

  checkEmis <- emissionsReport[, , "Emissions|CO2|Land RAW|+|Land-use Change (Mt CO2/yr)"] -
    dimSums(emissionsReport[, , c("Emissions|CO2|Land RAW|Land-use Change|+|Deforestation (Mt CO2/yr)",
                                  "Emissions|CO2|Land RAW|Land-use Change|+|Forest degradation (Mt CO2/yr)",
                                  "Emissions|CO2|Land RAW|Land-use Change|+|Other land conversion (Mt CO2/yr)",
                                  "Emissions|CO2|Land RAW|Land-use Change|+|Regrowth (Mt CO2/yr)",
                                  "Emissions|CO2|Land RAW|Land-use Change|+|Peatland (Mt CO2/yr)",
                                  "Emissions|CO2|Land RAW|Land-use Change|+|Soil (Mt CO2/yr)",
                                  "Emissions|CO2|Land RAW|Land-use Change|+|Residual (Mt CO2/yr)",
                                  "Emissions|CO2|Land RAW|Land-use Change|+|Timber (Mt CO2/yr)",
                                  "Emissions|CO2|Land RAW|Land-use Change|+|Wood Harvest (Mt CO2/yr)")], dim = 3)

  if (any(abs(checkEmis) > 1e-03, na.rm = TRUE)) {
    warning("CO2 emission sub-categories do not add up to total")
  }

  # nolint end


  # -----------------------------------------------------------------------------------------------------------------
  # Cumulative CO2 emissions, lowpass = 0

  cumulativeCO2 <- .calcCO2(.lowpass = 0, .cumulative = TRUE)

  # nolint start
  emissionsReport <- with(cumulativeCO2, mbind(
    emissionsReport,

    setNames(totalNetFlux,                        "Emissions|CO2|Land|Cumulative (Gt CO2)"), # all human-induced land-related CO2 emissions
    setNames(eClimateChange,                      "Emissions|CO2|Land|Cumulative|+|Indirect (Gt CO2)"), # indirect human-induced CO2 emissions: environmental change, climate change, natural effects
    setNames(eLanduseChange,                      "Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)"), # direct human-induced CO2 emissions, includes land-use change, land management and regrowth of vegetation

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
    setNames(regrowth[, , "forestry_aff"],        "Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|+|CO2-price AR (Gt CO2/yr)"),
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
    setNames(dimSums(somMa_pos, dim = 3),         "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Cropland management|+|Emissions (Gt CO2)"),
    setNames(dimSums(somMa_neg, dim = 3),         "Emissions|CO2|Land|Cumulative|Land-use Change|Soil|Cropland management|+|Withdrawals (Gt CO2)"),

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
      # Note: Carbon sequestration due to regrowth of timber plantations and natural forest after harvest are reported under `Land-use Change|Regrowth|+|Timber Plantations` and `Land-use Change|Regrowth|+|Secondary Forest`.
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
  # Calculated indirect emissions from land-use change from a .gdx file

  .calcLandCarbonSink <- function(.lowpass = 3, .cumulative = FALSE) {
    # Estimate of land-carbon sink from LPJmL
    co2 <- emisCO2(gdx,
                   level = "regglo", unit = "gas", sum_land = FALSE, sum_cpool = FALSE,
                   lowpass = .lowpass, cumulative = .cumulative)

    # Estimate of land-carbon sink from LPJmL
    LPJmlLCS <- co2[, , "cc", drop = TRUE]
    LPJmlLCS <- dimSums(LPJmlLCS, dim = 3.2) # Sum above- and belowground carbon

    if ("other" %in% getNames(LPJmlLCS)) {
      otherSet <- "other"
    } else {
      otherSet <- c("other_othernat", "other_youngsecdf")
    }

    # Estimate of land-carbon sink from Grassi et al. (2021)
    grassiLandCarbonSink <- landCarbonSink(gdx, level = "regglo", cumulative = .cumulative)

    # If cumulative, convert Mt CO2 to Gt CO2
    if (.cumulative) {
      LPJmlLCS  <- LPJmlLCS / 1000
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

    .x <- list(grassiLandCarbonSink       = grassiLandCarbonSink,
      LPJmlLandCarbonSink        = dimSums(LPJmlLCS[, , totalLandCarbonSink], dim = 3),
      managedLand                = dimSums(LPJmlLCS[, , managedLand], dim = 3),
      managedAg                  = dimSums(LPJmlLCS[, , managedAg], dim = 3),
      managedAgCrop              = dimSums(LPJmlLCS[, , managedAgCrop], dim = 3),
      managedAgCropArea          = LPJmlLCS[, , "crop_area", drop = TRUE],
      managedAgCropFallow        = LPJmlLCS[, , "crop_fallow", drop = TRUE],
      managedAgCropTreeCover     = LPJmlLCS[, , "crop_treecover", drop = TRUE],
      managedAgPast              = LPJmlLCS[, , "past", drop = TRUE],
      managedForest              = dimSums(LPJmlLCS[, , managedForest], dim = 3),
      managedForestSecdForest    = LPJmlLCS[, , "secdforest", drop = TRUE],
      managedForestForestryAff   = LPJmlLCS[, , "forestry_aff", drop = TRUE],
      managedForestForestryNDC   = LPJmlLCS[, , "forestry_ndc", drop = TRUE],
      managedForestForestryPlant = LPJmlLCS[, , "forestry_plant", drop = TRUE],
      managedUrban               = LPJmlLCS[, , "urban", drop = TRUE],
      unmanagedLand              = dimSums(LPJmlLCS[, , unmanagedLand], dim = 3),
      unmanagedLandPrimForest    = LPJmlLCS[, , "primforest", drop = TRUE],
      unmanagedLandOther         = dimSums(LPJmlLCS[, , otherSet, drop = TRUE], dim = 3)
    )

    return(.x)
  }


  # -----------------------------------------------------------------------------------------------------------------
  # Yearly indirect CO2 emissions from land-use change (land-carbon sink) reporting

  landCarbonSink <- .calcLandCarbonSink()

  # nolint start
  emissionsReport <- with(landCarbonSink, mbind(
    emissionsReport,

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
    setNames(unmanagedLandOther,         "Emissions|CO2|Land Carbon Sink|LPJmL|Unmanaged Land|+|Other Land (Mt CO2/yr)")
  ))
  # nolint end


  # -----------------------------------------------------------------------------------------------------------------
  # Cumulative indirect CO2 emissions from land-use change (land-carbon sink) reporting

  cumulativeLandCarbonSink <- .calcLandCarbonSink(.cumulative = TRUE)

  # nolint start
  emissionsReport <- with(cumulativeLandCarbonSink, mbind(
    emissionsReport,

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
    setNames(unmanagedLandOther,         "Emissions|CO2|Land Carbon Sink|Cumulative|LPJmL|Unmanaged Land|+|Other Land (Gt CO2)")
  ))
  # nolint end

  # -----------------------------------------------------------------------------------------------------------------
  # Yearly RAW indirect CO2 emissions from land-use change (land-carbon sink) reporting

  landCarbonSinkRAW <- .calcLandCarbonSink(.lowpass = 0, .cumulative = FALSE)

  emissionsReport <- with(landCarbonSinkRAW, mbind(
    emissionsReport,

    setNames(LPJmlLandCarbonSink,        "Emissions|CO2|Land Carbon Sink RAW|LPJmL (Mt CO2/yr)"),
    setNames(managedLand,                "Emissions|CO2|Land Carbon Sink RAW|LPJmL|+|Managed Land (Mt CO2/yr)"),
    setNames(managedAg,                  "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|+|Agricultural Land (Mt CO2/yr)"),
    setNames(managedAgCrop,              "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|Agricultural land|+|Cropland (Mt CO2/yr)"),
    setNames(managedAgCropArea,          "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|Agricultural land|Cropland|+|Croparea (Mt CO2/yr)"),
    setNames(managedAgCropFallow,        "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|Agricultural land|Cropland|+|Fallow (Mt CO2/yr)"),
    setNames(managedAgCropTreeCover,     "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|Agricultural land|Cropland|+|Tree Cover (Mt CO2/yr)"),
    setNames(managedAgPast,              "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|Agricultural land|+|Pasture (Mt CO2/yr)"),
    setNames(managedForest,              "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|+|Managed Forest (Mt CO2/yr)"),
    setNames(managedForestSecdForest,    "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|Managed Forest|+|Secondary Forest (Mt CO2/yr)"),
    setNames(managedForestForestryAff,   "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|Managed Forest|+|CO2-price AR (Mt CO2/yr)"),
    setNames(managedForestForestryNDC,   "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|Managed Forest|+|NPI_NDC AR (Mt CO2/yr)"),
    setNames(managedForestForestryPlant, "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|Managed Forest|+|Timber Plantations (Mt CO2/yr)"),
    setNames(managedUrban,               "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Managed Land|+|Urban Land (Mt CO2/yr)"),
    setNames(unmanagedLand,              "Emissions|CO2|Land Carbon Sink RAW|LPJmL|+|Unmanaged Land (Mt CO2/yr)"),
    setNames(unmanagedLandPrimForest,    "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Unmanaged Land|+|Primary Forest (Mt CO2/yr)"),
    setNames(unmanagedLandOther,         "Emissions|CO2|Land Carbon Sink RAW|LPJmL|Unmanaged Land|+|Other Land (Mt CO2/yr)")
  ))

  # -----------------------------------------------------------------------------------------------------------------
  # Cumulative RAW indirect CO2 emissions from land-use change (land-carbon sink) reporting

  cumulativeLandCarbonSinkRAW <- .calcLandCarbonSink(.lowpass = 0, .cumulative = TRUE)

  emissionsReport <- with(cumulativeLandCarbonSinkRAW, mbind(
    emissionsReport,

    setNames(grassiLandCarbonSink,       "Emissions|CO2|Land Carbon Sink RAW|Cumulative|Grassi (Gt CO2)"),
    setNames(LPJmlLandCarbonSink,        "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL (Gt CO2)"),
    setNames(managedLand,                "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|+|Managed Land (Gt CO2)"),
    setNames(managedAg,                  "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|+|Agricultural Land (Gt CO2)"),
    setNames(managedAgCrop,              "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|Agricultural land|+|Cropland (Gt CO2)"),
    setNames(managedAgCropArea,          "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|Agricultural land|Cropland|+|Croparea (Gt CO2)"),
    setNames(managedAgCropFallow,        "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|Agricultural land|Cropland|+|Fallow (Gt CO2)"),
    setNames(managedAgCropTreeCover,     "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|Agricultural land|Cropland|+|Tree Cover (Gt CO2)"),
    setNames(managedAgPast,              "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|Agricultural land|+|Pasture (Gt CO2)"),
    setNames(managedForest,              "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|+|Managed Forest (Gt CO2)"),
    setNames(managedForestSecdForest,    "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|Managed Forest|+|Secondary Forest (Gt CO2)"),
    setNames(managedForestForestryAff,   "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|Managed Forest|+|CO2-price AR (Gt CO2)"),
    setNames(managedForestForestryNDC,   "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|Managed Forest|+|NPI_NDC AR (Gt CO2)"),
    setNames(managedForestForestryPlant, "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|Managed Forest|+|Timber Plantations (Gt CO2)"),
    setNames(managedUrban,               "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Managed Land|+|Urban Land (Gt CO2)"),
    setNames(unmanagedLand,              "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|+|Unmanaged Land (Gt CO2)"),
    setNames(unmanagedLandPrimForest,    "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Unmanaged Land|+|Primary Forest (Gt CO2)"),
    setNames(unmanagedLandOther,         "Emissions|CO2|Land Carbon Sink RAW|Cumulative|LPJmL|Unmanaged Land|+|Other Land (Gt CO2)")
  ))

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
