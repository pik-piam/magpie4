#' @title emisCO2
#' @description reads detailed CO2 emissions out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation;
#'                "reg" (regional),
#'                "glo" (global),
#'                "regglo" (regional and global) or
#'                any other aggregation level defined in superAggregate
#' @param unit "element" or "gas";
#'               "element": co2_c in Mt C/yr
#'               "gas": co2_c Mt CO2/yr
#' @param sum_cpool aggregate carbon pools (TRUE), below ground (soilc) and
#'                  above ground (vegc and litc) will be reported, if FALSE
#' @param sum_land TRUE (default) or FALSE. Sum over land types (TRUE)
#'                 or report land-type specific emissions (FALSE).
#' @param cumulative Logical; Determines if emissions are reported annually
#'                  (FALSE) or cumulative (TRUE). The starting point for
#'                  cumulative emissions is y1995.
#' @param baseyear Baseyear used for cumulative emissions (default = 1995)
#' @param lowpass number of lowpass filter iterations (default = 3)
#' @return CO2 emissions as MAgPIE object (unit depends on \code{unit})
#' @author Florian Humpenoeder, Michael Crawford
#' @importFrom magclass dimSums add_dimension getSets getCells getNames add_columns
#'  collapseNames collapseDim nyears getYears setYears getItems new.magpie as.magpie
#' @examples
#' \dontrun{
#' x <- emisCO2(gdx)
#' }
emisCO2 <- function(gdx, file = NULL, level = "cell", unit = "gas",
                    sum_cpool = TRUE, sum_land = TRUE,
                    cumulative = FALSE, baseyear = 1995, lowpass = 3) {
  ###
  # CONSTANTS -------------------------------------------------------------------------------------------------------

  timestepLength <- timePeriods(gdx)
  years <- getYears(timestepLength, as.integer = TRUE)
  agPools <- c("litc", "vegc")
  ageClasses <- readGDX(gdx, "ac")

  ###
  # FUNCTIONS -------------------------------------------------------------------------------------------------------

  ###
  # if the ac dimension exists, sum over it
  .dimSumAC <- function(x) {
    if (dimExists(dim = "ac", x = x)) {
      x <- dimSums(x, dim = "ac")
    }
    return(x)
  }

  ###
  # p29_treecover contains the information before optimization. This is contrary to e.g. p35_secdforest,
  # which is identical to the variable itself. We can thus calculate expansion and reduction based on this variable.
  .changeAC <- function(beforeOpt, afterOpt, mode = "net") {

    x <- beforeOpt - afterOpt

    if (mode == "net") {
      x <- x
    } else if (mode == "expansion") {
      x[x > 0] <- 0
      x <- -x
    } else if (mode == "reduction") {
      x[x < 0] <- 0
    }

    return(x)
  }

  ###
  # compose list of total area per land type, compartment
  composeAreas <- function() {
    # --- non-age class land types
    # land    <- readGDX(gdx, "ov_land", select = list(type = "level"))
    a    <- land(gdx, level = "cell", subcategories = c("crop", "other", "forestry"))

    cropArea   <- a[, , "crop_area"]
    cropFallow <- a[, , "crop_fallow"]
    pasture    <- a[, , "past"]
    urban      <- a[, , "urban"]
    primforest <- a[, , "primforest"]

    # --- age class land types, excl forestry
    secdforest <- readGDX(gdx, "ov35_secdforest", select = list(type = "level"))
    secdforest <- add_dimension(secdforest, dim = 3.1, add = "land", nm = "secdforest")

    other <- readGDX(gdx, "ov_land_other", select = list(type = "level"), react = "silent")
    if (!is.null(other)) {
      getSets(other)["d3.1"] <- "land"
      getNames(other, dim = 1) <- paste("other", getNames(other, dim = 1), sep = "_")
    } else {
      othernat <- readGDX(gdx, "ov35_other", select = list(type = "level"))
      youngsecdf <- othernat
      youngsecdf[, , ] <- 0
      othernat <- add_dimension(othernat, dim = 3.1, add = "land", nm = "other_othernat")
      youngsecdf <- add_dimension(youngsecdf, dim = 3.1, add = "land", nm = "other_youngsecdf")
      other <- mbind(othernat, youngsecdf)
    }

    # --- forestry
    forestry <- landForestry(gdx, level = "cell")
    getSets(forestry)["d3.1"] <- "land"
    getNames(forestry, dim = 1) <- paste("forestry", getNames(forestry, dim = 1), sep = "_")

    cropTreecover <- readGDX(gdx, "ov29_treecover", select = list(type = "level"), react = "silent")
    if (is.null(cropTreecover)) {
      cropTreecover <- secdforest
      cropTreecover[, , ] <- 0
      getNames(cropTreecover, dim = 1) <- "crop_treecover"
    } else {
      cropTreecover <- add_dimension(cropTreecover, dim = 3.1, add = "land", nm = "crop_treecover")
    }

    if (abs(sum(collapseNames(cropArea) + collapseNames(cropFallow) + dimSums(cropTreecover, dim = 3)
                - dimSums(a[, , c("crop_area", "crop_fallow", "crop_treecover")], dim = 3))) > 1e-6){
      warning("Difference in crop land detected")
    }

    areas <- list(cropArea      = cropArea,
                  cropFallow    = cropFallow,
                  cropTreecover = cropTreecover,
                  pasture       = pasture,
                  forestry      = forestry,
                  primforest    = primforest,
                  secdforest    = secdforest,
                  other         = other,
                  urban         = urban)

    return(areas)

  }

  ###
  # compose list of carbon-densities per land-use type, compartment

  composeDensities <- function() {
    # currently, for ac types all the soil carbon densities are assumed equal
    .addSOM <- function(x, soilc) {

      getSets(x)["d3.3"] <- "c_pools"
      x <- add_columns(x, addnm = "soilc", dim = "c_pools", fill = 1)
      soilc <- x[, , "soilc"] * soilc
      x <- mbind(x[, , "soilc", invert = TRUE], soilc)

      return(x)
    }

    # --- non-age class carbon densities
    carbonDensities <- readGDX(gdx, "fm_carbon_density")[, years, ]
    croparea          <- carbonDensities[, , "crop"][, , c("vegc", "litc")]
    names(dimnames(croparea)) <- c("j.region", "t", "land.ag_pools")
    getNames(croparea, dim = "land") <- "crop_area"
    fallow            <- croparea
    getNames(fallow, dim = "land") <- "crop_fallow"
    pasture           <- carbonDensities[, , "past"]
    urban             <- carbonDensities[, , "urban"]
    primforest        <- carbonDensities[, , "primforest"]

    # --- age class carbon densities, excl forestry

    secdforest <- readGDX(gdx, "pm_carbon_density_secdforest_ac", "pm_carbon_density_ac",
                          format = "first_found")[, years, ]
    secdforest <- add_dimension(secdforest, dim = 3.1, add = "land", nm = "secdforest")

    other <- readGDX(gdx, "ov_land_other", select = list(type = "level"), react = "silent")
    if (!is.null(other)) {
      other <- readGDX(gdx, "p35_carbon_density_other", react = "silent")
      getSets(other)["d3.1"] <- "land"
      other <- other[, years, ]
      getNames(other, dim = "land") <- paste("other", getNames(other, dim = "land"), sep = "_")
    } else {
      other <- readGDX(gdx, "pm_carbon_density_ac")[, years, ]
      other <- add_dimension(other, dim = 3.1, add = "land", nm = "other")
    }

    croptree <- readGDX(gdx, "p29_carbon_density_ac", react = "silent")
    if (!is.null(croptree)) {
      croptree <- add_dimension(croptree[, years, ], dim = 3.1, add = "land", nm = "crop_treecover")
    } else {
      croptree <- secdforest
      croptree[, , ] <- 0
      getNames(croptree, dim = 1) <- "crop_treecover"
    }

    # --- forestry
    forestry <- readGDX(gdx, "p32_carbon_density_ac", react = "silent")
    getSets(forestry)["d3.1"] <- "land"
    getNames(forestry, dim = 1) <- paste("forestry", getNames(forestry, dim = 1), sep = "_")


    dynSom <- !is.null(readGDX(gdx, "ov59_som_pool", react = "silent"))
    if (!dynSom) {
      # --- SOM emissions for static realization
      # --- cropland and pasture
      topsoilCarbonDensities <- readGDX(gdx, "i59_topsoilc_density")[, years, ] # i59, not f59 as in dynamic realization
      subsoilCarbonDensities <- readGDX(gdx, "i59_subsoilc_density")[, years, ]

      cropareaSOM <- topsoilCarbonDensities + subsoilCarbonDensities
      cropareaSOM <- add_dimension(cropareaSOM, dim = 3, add = "c_pools", nm = "soilc")
      getSets(croparea)["d3.2"] <- "c_pools"
      croparea <- add_columns(croparea, addnm = "soilc", dim = 3.2, fill = 0)
      croparea[, , "soilc"] <- cropareaSOM

      fallowSOM <- topsoilCarbonDensities + subsoilCarbonDensities
      fallowSOM <- add_dimension(fallowSOM, dim = 3, add = "c_pools", nm = "soilc")
      getSets(fallow)["d3.2"] <- "c_pools"
      fallow <- add_columns(fallow, addnm = "soilc", dim = 3.2, fill = 0)
      fallow[, , "soilc"] <- fallowSOM

      # --- all other types
      carbonDensities       <- readGDX(gdx, "fm_carbon_density")[, years, ]

      pasture[, , "soilc"]    <- carbonDensities[, , "past"][, , "soilc"]
      urban[, , "soilc"]      <- carbonDensities[, , "urban"][, , "soilc"]
      primforest[, , "soilc"] <- carbonDensities[, , "primforest"][, , "soilc"]

      croptreeSOM <- collapseDim(carbonDensities[, , "secdforest"][, , "soilc"], dim = "land")
      croptree    <- .addSOM(croptree, croptreeSOM)

      secdforestSOM <- collapseDim(carbonDensities[, , "secdforest"][, , "soilc"], dim = "land")
      secdforest    <- .addSOM(secdforest, secdforestSOM)

      if (all(c("other_othernat", "other_youngsecdf") %in% getNames(other, dim = "land"))) {
        othernatSOM <- collapseDim(carbonDensities[, , "other"][, , "soilc"], dim = "land")
        othernatSOM <- add_dimension(othernatSOM, dim = 3.1, add = "land", nm = "other_othernat")
        youngsecdfSOM <- collapseDim(carbonDensities[, , "secdforest"][, , "soilc"], dim = "land")
        youngsecdfSOM <- add_dimension(youngsecdfSOM, dim = 3.1, add = "land", nm = "other_youngsecdf")
        otherSOM <- mbind(othernatSOM, youngsecdfSOM)
      } else {
        otherSOM <- collapseDim(carbonDensities[, , "other"][, , "soilc"], dim = "land")
      }
      other    <- .addSOM(other, otherSOM)

      forestrySOM <- collapseDim(carbonDensities[, , "forestry"][, , "soilc"], dim = "land")
      forestry    <- .addSOM(forestry, forestrySOM)
    } else {
      # --- SOM emissions for dynamic realization (cellpool_jan23) - only dummy values
      # (legacy effects and management effects on densities are in contrast to the main calculation idea
      # of emisCO2, so that soil emissions are calculated in emisSOC for dynSom)

      # --- all natural types
      carbonDensities         <- readGDX(gdx, "fm_carbon_density")[, years, ]
      names(dimnames(croparea))[[3]] <- "land.c_pools"
      croparea <- add_columns(croparea, addnm = "soilc", dim = 3.2, fill = 0)
      croparea[, , "soilc"]   <- carbonDensities[, , "other"][, , "soilc"]
      getSets(fallow)["d3.2"] <- "c_pools"
      fallow   <- add_columns(fallow, addnm = "soilc", dim = 3.2, fill = 0)
      fallow[, , "soilc"]     <- carbonDensities[, , "other"][, , "soilc"]
      pasture[, , "soilc"]    <- carbonDensities[, , "other"][, , "soilc"]
      urban[, , "soilc"]      <- carbonDensities[, , "other"][, , "soilc"]
      primforest[, , "soilc"] <- carbonDensities[, , "other"][, , "soilc"]
      forestry    <- .addSOM(forestry,   collapseDim(carbonDensities[, , "other"][, , "soilc"], dim = "land"))
      other       <- .addSOM(other,      collapseDim(carbonDensities[, , "other"][, , "soilc"], dim = "land"))
      secdforest  <- .addSOM(secdforest, collapseDim(carbonDensities[, , "other"][, , "soilc"], dim = "land"))
      croptree    <- .addSOM(croptree,   collapseDim(carbonDensities[, , "other"][, , "soilc"], dim = "land"))
    }

    densities <- list(cropArea      = croparea,
                      cropFallow    = fallow,
                      cropTreecover = croptree,
                      pasture       = pasture,
                      forestry      = forestry,
                      primforest    = primforest,
                      secdforest    = secdforest,
                      other         = other,
                      urban         = urban)
  }

  ###
  # net emissions (total, cc, area, interaction)
  calculateMainEmissions <- function(areas, densities) {
    ###
    # calculate difference in carbon density between time steps
    # e.g. carbon density 1995 - carbon density 2000, ...
    .tDiff <- function(density) {

      diff <- density
      diff[, , ] <- 0
      for (t in 2:nyears(density)) {
        diff[, t, ] <- setYears(density[, t - 1, ], getYears(density[, t, ])) - density[, t, ]
      }

      return(diff)
    }

    # --- Total stocks (should be almost equal to magpie4::carbonstock)
    totalStock <- Map(function(area, density) {
      t <- area * density
      t <- .dimSumAC(t)
    }, areas, densities)

    # --- Total emissions
    emisNet <- Map(function(area, density) {
      t <- .tDiff(area * density)
      t <- .dimSumAC(t)
    }, areas, densities)

    # --- Climate change / carbon density effect
    emisCC <- Map(function(area, density) {
      t <- area * .tDiff(density)
      t <- .dimSumAC(t)
    }, areas, densities)

    # --- Area change effect
    emisArea <- Map(function(area, density) {
      t <- .tDiff(area) * density
      t <- .dimSumAC(t)
    }, areas, densities)

    # --- Interaction effect
    emisInteract <- Map(function(area, density) {
      t <- .tDiff(area) * .tDiff(density)
      t <- .dimSumAC(t)
    }, areas, densities)

    mainEmissions <- list(totalStock   = totalStock,
                          emisNet      = emisNet,
                          emisCC       = emisCC,
                          emisArea     = emisArea,
                          emisInteract = emisInteract)

    return(mainEmissions)
  }

  ###
  # aboveground emissions from wood harvesting, deforestation, conversion of other land, and land degradation
  calculateGrossEmissions <- function(areas, densities) {

    .grossEmissionsHelper <- function(densityMtC, reductionMha, harvestMha = NULL, degradMha = NULL) {

      deforMha <- reductionMha
      if (!is.null(harvestMha)) {
        deforMha <- deforMha - harvestMha
      }
      emisDeforMtC <- densityMtC * deforMha

      emisharvestMtC <- NULL
      if (!is.null(harvestMha)) {
        emisharvestMtC <- densityMtC * harvestMha
      }

      emisDegradMtC <- NULL
      if (!is.null(degradMha)) {
        emisDegradMtC <- densityMtC * degradMha
      }

      t <- list(emisDeforMtC   = emisDeforMtC,
                emisharvestMtC = emisharvestMtC,
                emisDegradMtC  = emisDegradMtC)

      t <- lapply(X = t, FUN = .dimSumAC)

      return(t)
    }

    # --- Primary forest
    densityMtC   <- densities$primforest[, , agPools]
    reductionMha <- readGDX(gdx, "ov35_primforest_reduction", select = list(type = "level"))
    harvestMha   <- readGDX(gdx, "ov35_hvarea_primforest", select = list(type = "level"), react = "silent")
    degradMha    <- readGDX(gdx, "p35_disturbance_loss_primf", react = "silent")

    emisPrimforest <- .grossEmissionsHelper(densityMtC    = densityMtC,
                                            reductionMha  = reductionMha,
                                            harvestMha    = harvestMha,
                                            degradMha     = degradMha)

    # --- Secondary forest
    densityMtC   <- densities$secdforest[, , agPools]
    reductionMha <- readGDX(gdx, "ov35_secdforest_reduction", select = list(type = "level"))
    harvestMha   <- readGDX(gdx, "ov35_hvarea_secdforest", select = list(type = "level"), react = "silent")
    degradMha    <- readGDX(gdx, "p35_disturbance_loss_secdf", react = "silent")

    emisSecdforest <- .grossEmissionsHelper(densityMtC    = densityMtC,
                                            reductionMha  = reductionMha,
                                            harvestMha    = harvestMha,
                                            degradMha     = degradMha)

    # --- Other land
    densityMtC   <- densities$other[, , agPools]
    p35_maturesecdf <- readGDX(gdx, "p35_maturesecdf", react = "silent")
    if (!is.null(p35_maturesecdf)) {
      areaBefore <- readGDX(gdx, "p35_land_other")
      areaAfter <- readGDX(gdx, "ov_land_other", select = list(type = "level"))
      reductionMha <- .changeAC(areaBefore, areaAfter, mode = "reduction")
      getSets(reductionMha)["d3.1"] <- "land"
      getNames(reductionMha, dim = 1) <- paste("other", getNames(reductionMha, dim = 1), sep = "_")
      harvestMha   <- readGDX(gdx, "ov35_hvarea_other", select = list(type = "level"), react = "silent")
      getSets(harvestMha)["d3.1"] <- "land"
      getNames(harvestMha, dim = 1) <- paste("other", getNames(harvestMha, dim = 1), sep = "_")
    } else {
      reductionMha <- readGDX(gdx, "ov35_other_reduction", select = list(type = "level"), react = "silent")
      reductionMha <- add_dimension(reductionMha, dim = 3.1, add = "land", c("other_othernat", "other_youngsecdf"))
      reductionMha[, , "other_youngsecdf"] <- 0
      harvestMha   <- readGDX(gdx, "ov35_hvarea_other", select = list(type = "level"), react = "silent")
      harvestMha <- add_dimension(harvestMha, dim = 3.1, add = "land", c("other_othernat", "other_youngsecdf"))
      harvestMha[, , "other_youngsecdf"] <- 0
    }

    emisOther <- .grossEmissionsHelper(densityMtC    = densityMtC,
                                       reductionMha  = reductionMha,
                                       harvestMha    = harvestMha)

    # --- Plantations
    densityMtC   <- densities$forestry[, , agPools]
    reductionMha <- readGDX(gdx, "ov32_land_reduction", select = list(type = "level"), react = "silent")
    getSets(reductionMha)["d3.1"] <- "land"
    getNames(reductionMha, dim = 1) <- c("forestry_aff", "forestry_ndc", "forestry_plant")

    # Only plantations are subject to harvesting
    harvestMha <- readGDX(gdx, "ov32_hvarea_forestry", select = list(type = "level"), react = "silent")
    harvestMha <- add_dimension(harvestMha, dim = 3.1, add = "land", nm = "forestry_plant")
    harvestMha <- add_columns(harvestMha, addnm = "forestry_aff", dim = "land", fill = 0)
    harvestMha <- add_columns(harvestMha, addnm = "forestry_ndc", dim = "land", fill = 0)

    emisPlantations <- .grossEmissionsHelper(densityMtC    = densityMtC,
                                             reductionMha  = reductionMha,
                                             harvestMha    = harvestMha)

    # --- Tree cover on cropland
    densityMtC         <- densities$cropTreecover[, , agPools]
    areaAfterOptimMha  <- areas$cropTreecover
    areaBeforeOptimMha <- readGDX(gdx, "p29_treecover", react = "silent")
    if (is.null(areaBeforeOptimMha)) areaBeforeOptimMha <- areaAfterOptimMha
    reductionMha       <- .changeAC(areaBeforeOptimMha, areaAfterOptimMha, mode = "reduction")

    emiscroptree <- .grossEmissionsHelper(densityMtC   = densityMtC,
                                          reductionMha = reductionMha)

    # --- Reformulate to deforestation, harvest, degradation, and other conversion
    grossEmissionsLand <- list(emisPrimforest  = emisPrimforest,
                               emisSecdforest  = emisSecdforest,
                               emisOther       = emisOther,
                               emisPlantations = emisPlantations,
                               emiscroptree = emiscroptree)

    emisDeforestation <- mbind(lapply(X = grossEmissionsLand, FUN = function(x) x$emisDeforMtC))
    emisHarvest       <- mbind(lapply(X = grossEmissionsLand, FUN = function(x) x$emisharvestMtC))
    emisDegrad        <- mbind(lapply(X = grossEmissionsLand, FUN = function(x) x$emisDegradMtC))

    # --- Deforestation on other is considered other_conversion

    emisOtherLand <- emisDeforestation[, , c("other_othernat", "other_youngsecdf")]
    emisDeforestation[, , "other_othernat"] <- 0
    emisDeforestation[, , "other_youngsecdf"] <- 0

    grossEmissions <- list(emisHarvest       = emisHarvest,
                           emisDeforestation = emisDeforestation,
                           emisDegrad        = emisDegrad,
                           emisOtherLand     = emisOtherLand)

    return(grossEmissions)
  }

  ###
  # aboveground negative emissions from regrowth
  calculateRegrowthEmissions <- function(areas, densities) {
    ###
    # Calculate expansion of ac land types in Mha over time
    # reduction is used as an appropriate template
    .expansionAc <- function(expansion, reduction) {

      newArea <- reduction
      newArea[, , ] <- 0

      t <- timestepLength
      t[1] <- 5
      t <- t / 5

      for (y in getYears(newArea)) {
        currentEstablishmentAC <- ageClasses[1:t[, y, ]]
        newArea[, y, currentEstablishmentAC] <- expansion[, y, ] / t[, y, ]
      }

      if (any(abs(dimSums(dimSums(newArea, dim = "ac") - expansion, dim = 1)) > 1e-6)) {
        warning("Differences in expansion_ac detected within magpie4::emisCO2")
      }

      return(newArea)
    }

    ###
    # Shift density from year t to year t+1
    .tShift <- function(density) {

      diff <- density
      diff[, , ] <- 0
      for (t in 2:nyears(density)) {
        diff[, t, ] <- setYears(density[, t - 1, ], getYears(density[, t, ]))
      }

      return(diff)
    }

    ###
    # Calculate growth in Mha per age class from year t to t+1, accounting for
    # accumulation in acx.
    .acGrow <- function(x) {

      a <- x
      a[, , ] <- 0
      ac <- getNames(x, dim = "ac")
      year <- getYears(x, as.integer = TRUE)

      for (t in 1:nyears(x)) {

        if (t == 1) {
          shifter <- 1
        } else {
          shifter <- (year[t] - year[t - 1]) / 5
        }

        # include acx
        acIndex <- (1 + shifter):length(ac)
        acSub <- ac[acIndex]
        acSubBefore <- ac[acIndex - shifter]
        a[, t, acSub] <- setNames(x[, t, acSubBefore], getNames(x[, t, acSub]))

        # accounting for accumulation in acx
        acx2 <- NULL
        for (i in seq_along(ac)) {
          if (i > length(ac) - shifter)
            acx2 <- c(acx2, ac[i])
        }

        a[, t, "acx"] <- a[, t, "acx"] + dimSums(x[, t, acx2], dim = "ac")

      }

      return(a)
    }

    ###
    # Calculate the effects of disturbance, if applicable, on the establishment age classes
    .disturbACest <- function(x) {

      x2 <- x
      x2[, , ] <- 0
      x2 <- add_dimension(x2, dim = 3.1, add = "ac", ageClasses)

      t <- timestepLength / 5
      for (y in getYears(x2)) {
        currentEstablishmentAC <- ageClasses[1:t[, y, ]]
        x2[, y, currentEstablishmentAC] <- dimSums(x[, y, ], dim = 3) / t[, y, ]
      }

      return(x2)
    }

    ###
    # Calculate emissions in MtC from regrowth of ac types
    .regrowth <- function(densityAg, area, expansion,
                          disturbanceLoss = NULL, disturbanceLossAcEst = NULL, recoveredForest = NULL,
                          youngSecdfAcEst = NULL) {
      # --- Mha loss due to the disturbance process
      disturbed <- 0
      disturbACest <- 0
      if (!is.null(disturbanceLoss) && !is.null(disturbanceLossAcEst)) {
        disturbed <- .disturbACest(disturbanceLossAcEst) - disturbanceLoss
        disturbACest <- .disturbACest(disturbanceLossAcEst)
      }

      areaYoungSecdf <- 0
      if (!is.null(youngSecdfAcEst)) {
        areaYoungSecdf <- youngSecdfAcEst
      }

      areaBefore <- .tShift(area) + disturbed + areaYoungSecdf
      areaAfter  <- .acGrow(areaBefore)
      emisRegrowth <- (areaBefore - areaAfter) * densityAg

      # extra regrowth emissions within longer time steps
      emisRegrowthIntraTimestep <- (0 - (expansion + areaYoungSecdf + disturbACest)) * densityAg
      emisRegrowth <- emisRegrowth + emisRegrowthIntraTimestep

      # emissions from shifting between other land and forest (negative in secdforest and positive in other land)
      emisRecover <- 0
      if (!is.null(recoveredForest)) {
        emisRecover <- recoveredForest * densityAg
      }

      emisRegrowth <- emisRegrowth + emisRecover

      return(emisRegrowth)

    }

    # --- Secondary forests
    densityAg  <- densities$secdforest[, , agPools]
    area       <- areas$secdforest

    reduction <- readGDX(gdx, "ov35_secdforest_reduction", select = list(type = "level"))
    expansion <- readGDX(gdx, "ov35_secdforest_expansion", select = list(type = "level"))
    expansion <- .expansionAc(expansion, reduction)

    disturbanceLoss           <- readGDX(gdx, "p35_disturbance_loss_secdf")
    primforestDisturbanceLoss <- readGDX(gdx, "p35_disturbance_loss_primf")
    disturbanceLossAcEst      <- dimSums(disturbanceLoss, dim = 3) + dimSums(primforestDisturbanceLoss, dim = 3)

    recoveredForest <- readGDX(gdx, "p35_maturesecdf", "p35_recovered_forest", format = "first_found") * -1

    regrowthEmisSecdforest <- .regrowth(densityAg            = densityAg,
                                        area                 = area,
                                        expansion            = expansion,
                                        disturbanceLoss      = disturbanceLoss,
                                        disturbanceLossAcEst = disturbanceLossAcEst,
                                        recoveredForest      = recoveredForest)

    # --- Other land
    densityAg <- densities$other[, , agPools]
    area      <- areas$other

    p35_forest_recovery_area <- readGDX(gdx, "p35_forest_recovery_area", react = "silent")
    if (!is.null(p35_forest_recovery_area)) {

      reduction <- readGDX(gdx, "ov35_other_reduction", select = list(type = "level"), react = "silent")
      expansion <- readGDX(gdx, "ov35_other_expansion", select = list(type = "level"), react = "silent")
      expansion <- .expansionAc(expansion, reduction)
      getNames(expansion, dim = 1) <- paste("other", getNames(expansion, dim = 1), sep = "_")
      getSets(expansion)["d3.1"] <- "land"

      youngSecdf <- expansion
      youngSecdf[, , ] <- 0
      youngSecdf[, , "other_othernat"] <- readGDX(gdx, "p35_forest_recovery_area") * -1
      youngSecdf[, , "other_youngsecdf"] <- readGDX(gdx, "p35_forest_recovery_area")

      recoveredForest <- expansion
      recoveredForest[, , ] <- 0
      recoveredForest[, , "other_youngsecdf"] <- readGDX(gdx, "p35_maturesecdf", react = "silent")

    } else {
      reduction <- readGDX(gdx, "ov35_other_reduction", select = list(type = "level"))
      expansion <- readGDX(gdx, "ov35_other_expansion", select = list(type = "level"))
      expansion <- .expansionAc(expansion, reduction)
      expansion <- add_dimension(expansion, dim = 3.1, add = "land", c("other_othernat", "other_youngsecdf"))
      expansion[, , "other_youngsecdf"] <- 0

      youngSecdf <- NULL
      recoveredForest <- expansion
      recoveredForest[, , "other_othernat"] <- readGDX(gdx, "p35_recovered_forest", react = "silent")
    }

    regrowthEmisOther <- .regrowth(densityAg       = densityAg,
                                   area            = area,
                                   expansion       = expansion,
                                   recoveredForest = recoveredForest,
                                   youngSecdfAcEst = youngSecdf)

    # --- Forestry
    densityAg <- densities$forestry[, , agPools]
    area      <- areas$forestry

    reduction <- readGDX(gdx, "ov32_land_reduction", select = list(type = "level"), react = "silent")
    expansion <- readGDX(gdx, "ov32_land_expansion", select = list(type = "level"), react = "silent")
    expansion <- .expansionAc(expansion, reduction)
    getSets(expansion)["d3.1"] <- "land"

    disturbanceLoss      <- readGDX(gdx, "p32_disturbance_loss_ftype32")
    getSets(disturbanceLoss)["d3.1"] <- "land"
    disturbanceLossAcEst <- dimSums(disturbanceLoss, dim = 3.2)
    getSets(disturbanceLossAcEst)["d3.1"] <- "land"

    forestryNames <- c("forestry_aff", "forestry_ndc", "forestry_plant")
    getNames(reduction, dim = 1)            <- forestryNames
    getNames(expansion, dim = 1)            <- forestryNames
    getNames(disturbanceLoss, dim = 1)      <- forestryNames
    getNames(disturbanceLossAcEst, dim = 1) <- forestryNames

    regrowthEmisForestry <- .regrowth(densityAg            = densityAg,
                                      area                 = area,
                                      expansion            = expansion,
                                      disturbanceLoss      = disturbanceLoss,
                                      disturbanceLossAcEst = disturbanceLossAcEst)

    # --- Tree cover on cropland
    densityAg <- densities$cropTreecover[, , agPools]
    areaAfterOptimMha  <- areas$cropTreecover
    areaBeforeOptimMha <- readGDX(gdx, "p29_treecover", react = "silent")
    if (is.null(areaBeforeOptimMha)) areaBeforeOptimMha <- areaAfterOptimMha
    expansion <- .changeAC(areaBeforeOptimMha, areaAfterOptimMha, mode = "expansion")

    regrowthEmiscroptree <- .regrowth(densityAg = densityAg,
                                      area      = areaAfterOptimMha,
                                      expansion = expansion)

    regrowth <- mbind(regrowthEmisSecdforest,
                      regrowthEmisOther,
                      regrowthEmisForestry,
                      regrowthEmiscroptree)


    regrowth <- dimSums(regrowth, dim = "ac")

    return(regrowth)
  }

  ###
  # CALCULATIONS ----------------------------------------------------------------------------------------------------

  # --- prepare input objects
  totalStock <- carbonstock(gdx, level = "cell", sum_cpool = FALSE, sum_land = FALSE,
                            subcategories = c("crop", "forestry", "other"))
  template <- totalStock
  template[, , ] <- 0

  areas     <- composeAreas()
  densities <- composeDensities()

  # --- calculate main emissions
  mainEmissions  <- calculateMainEmissions(areas, densities)

  # --- calculate individual emissions
  grossEmissions <- calculateGrossEmissions(areas = areas, densities = densities)
  regrowth       <- calculateRegrowthEmissions(areas = areas, densities = densities)

  ###
  # PREPARE OUTPUT OBJECT -------------------------------------------------------------------------------------------

  # ensure dimensionality of magpie objects is the same
  .expandTypes <- function(emissions, t = template) {
    t[, , getItems(emissions, dim = 3)] <- t[, , getItems(emissions, dim = 3)] + emissions
    return(t)
  }

  emisRegrowth      <- .expandTypes(regrowth)

  grossEmissions    <- lapply(grossEmissions, .expandTypes)
  emisDeforestation <- grossEmissions$emisDeforestation
  emisDegrad        <- grossEmissions$emisDegrad
  emisOtherLand     <- grossEmissions$emisOtherLand
  emisHarvest       <- grossEmissions$emisHarvest

  subcomponents <- emisRegrowth + emisDeforestation + emisDegrad + emisOtherLand + emisHarvest

  # --- net emissions
  mainEmissions   <- lapply(mainEmissions, function(x) Reduce(mbind, x))
  totalStockCheck <- mainEmissions$totalStock
  emisNet         <- mainEmissions$emisNet
  emisCC          <- mainEmissions$emisCC
  emisArea        <- mainEmissions$emisArea
  emisInteract    <- mainEmissions$emisInteract

  # As without cc there could be no interaction emissions, we assign these to emisCC
  emisCC <- emisCC + emisInteract

  # --- include SOM into subcomponents
  dynSom <- !is.null(readGDX(gdx, "ov59_som_pool", react = "silent"))
  if (!dynSom) {
    # For static, we take SOM directly from the main emissions
    emisSOM <- .expandTypes(emisArea[, , "soilc"])
    subcomponents <- subcomponents + emisSOM
  } else {
    landTypes <- getNames(emisArea[, , "soilc"], dim = "land")
    areas     <- composeAreas()
    cropDecomp     <- mbind(areas$cropArea, areas$cropFallow,
                            dimSums(areas$cropTreecover, dim = "ac"))
    cropDecomp     <- suppressMessages(
      toolConditionalReplace(cropDecomp / dimSums(cropDecomp, dim = 3), "is.na()", 1 / 3))
    forestryDecomp <- dimSums(areas$forestry, dim = 3.2)
    forestryDecomp <- suppressMessages(
      toolConditionalReplace(forestryDecomp / dimSums(forestryDecomp, dim = 3), "is.na()", 1 / 3))
    otherDecomp    <- dimSums(areas$other, dim = 3.2)
    otherDecomp    <- suppressMessages(
      toolConditionalReplace(otherDecomp / dimSums(otherDecomp, dim = 3), "is.na()", 1 / 2))

    weights        <- mbind(cropDecomp, forestryDecomp, otherDecomp)
    weights        <- add_columns(weights, addnm = setdiff(landTypes, getNames(weights)), dim = 3.1, fill = 1)
    weights        <- add_dimension(weights, dim = 3.1, add = "landSOC", nm = "dummy")
    getNames(weights, dim = 1) <- gsub("_(.*)", "", getNames(weights, dim = 2))

    emisSOC   <- emisSOC(gdx, sumLand = FALSE)
    emisSOC   <- collapseDim(emisSOC * weights[, getYears(emisSOC), ], dim = 3.2)

    emisSOM   <- .expandTypes(emisArea[, , "soilc"]) # getting structure
    emisSOM[] <- 0
    emisCcSOM <- emisLuSOM <- emisMaSOM <- emisScmSOM <- emisSOM
    emisCcSOM[, getYears(emisSOC), "soilc"]   <- emisSOC[, , "ccEmisFull"] + emisSOC[, , "ccEmisSub"]
    emisCC[, , "soilc"] <- emisCcSOM[, , "soilc"]
    emisLuSOM[, getYears(emisSOC), "soilc"]   <- emisSOC[, , "luEmisFull"]
    emisMaSOM[, getYears(emisSOC), "soilc"]   <- emisSOC[, , "maEmisFull"]
    emisScmSOM[, getYears(emisSOC), "soilc"]  <- emisSOC[, , "scmEmisFull"]
    emisSOM <- emisLuSOM + emisMaSOM + emisScmSOM
    emisArea[, , "soilc"] <- emisSOM[, , "soilc"]
    emisNet[, , "soilc"]  <- emisSOM[, , "soilc"] + emisCcSOM[, , "soilc"]
    subcomponents <- subcomponents + emisSOM
  }


  # --- calculation residual land-use change emissions
  # These are emissions unaccounted for within the sub-component emissions calculations
  # In theory, this quantity should be zero. In practice, this quantity should be very small.
  emisResidual <- emisArea - subcomponents

  # assign proper names
  emisNet           <- add_dimension(emisNet, dim = 3.3,           nm = "total", add = "type")
  emisCC            <- add_dimension(emisCC, dim = 3.3,            nm = "cc", add = "type")
  emisArea          <- add_dimension(emisArea, dim = 3.3,          nm = "lu", add = "type")
  emisResidual      <- add_dimension(emisResidual, dim = 3.3,      nm = "residual", add = "type")
  emisRegrowth      <- add_dimension(emisRegrowth, dim = 3.3,      nm = "lu_regrowth", add = "type")
  emisHarvest       <- add_dimension(emisHarvest, dim = 3.3,       nm = "lu_harvest", add = "type")
  emisDeforestation <- add_dimension(emisDeforestation, dim = 3.3, nm = "lu_deforestation", add = "type")
  emisDegrad        <- add_dimension(emisDegrad, dim = 3.3,        nm = "lu_degrad", add = "type")
  emisOtherLand     <- add_dimension(emisOtherLand, dim = 3.3,     nm = "lu_other_conversion", add = "type")
  emisSOM           <- add_dimension(emisSOM, dim = 3.3,           nm = "lu_som", add = "type")
  if (dynSom) {
    emisLuSOM     <- add_dimension(emisLuSOM, dim = 3.3,           nm = "lu_som_luc", add = "type")
    emisMaSOM     <- add_dimension(emisMaSOM, dim = 3.3,           nm = "lu_som_man", add = "type")
    emisScmSOM    <- add_dimension(emisScmSOM, dim = 3.3,          nm = "lu_som_scm", add = "type")
  } else {
    dummy   <- collapseNames(emisSOM)
    dummy[] <- 0
    emisLuSOM     <- add_dimension(collapseNames(emisSOM), dim = 3.3, nm = "lu_som_luc", add = "type")
    emisMaSOM     <- add_dimension(dummy, dim = 3.3, nm = "lu_som_man", add = "type")
    emisScmSOM    <- add_dimension(dummy, dim = 3.3, nm = "lu_som_scm", add = "type")
  }

  # --- bind together into return object

  output <- mbind(emisNet, emisCC, emisArea, emisResidual,
                  emisRegrowth, emisDeforestation, emisDegrad, emisOtherLand, emisHarvest, emisSOM,
                  emisLuSOM, emisMaSOM, emisScmSOM)

  # --- no data in y1995, correct for timestep length
  output[, 1, ] <- NA
  output <- output / timestepLength

  ###
  # various checks for output validation
  if (dynSom) {
    totalStockCheck[, , "soilc"] <- emisSOC[, , "totalStock"]
  }

  .validateCalculation <- function(totalStock, totalStockCheck, output) {
    # --- Ensure independent output of carbonstock is nearly equivalent to own calculation
    if (any(abs(totalStock - totalStockCheck) > 1e-03, na.rm = TRUE)) {
      if (any(abs(dimSums(totalStock[, , "soilc"], dim = 3) -
                  dimSums(totalStockCheck[, , "soilc"], dim = 3)) > 1e-03, na.rm = TRUE)
          || !dynSom) { # if dynSom is on only check the sums over all land types here
        warning("Stocks calculated in magpie4::emisCO2 differ from magpie4::carbonstock")
      }
    }

    # --- Ensure that area - subcomponent residual is nearly zero
    # Croparea, fallow and past are not accounted for in grossEmissions
    residual <- output[, , c("crop_area", "crop_fallow", "past"), invert = TRUE][, , "residual"]
    if (any(abs(residual) > 1e-03, na.rm = TRUE)) {
      # round(dimSums(residual,dim=c(1)),6)#[,,"soilc"]
      warning("Inappropriately high residuals in land use sub-components in magpie4::emisCO2")
    }

    # --- Ensure that total net emissions are additive of cc, lu, and interaction (now included in cc)
    totalEmissions <- output[, , "total"]
    componentEmissions <- dimSums(output[, , c("cc", "lu")], dim = 3.3)
    if (any(abs(totalEmissions - componentEmissions) > 1e-03, na.rm = TRUE)) {
      warning("Inapprpopriately high residuals in main emissions in magpie4::emisCO2")
    }

    # --- Ensure that gross emissions are all positive
    grossEmissions <- output[, , c("lu_deforestation", "lu_degrad", "lu_other_conversion", "lu_harvest")]
    if (any(grossEmissions < -1e-03, na.rm = TRUE)) {
      warning("Gross emissions are less than zero in magpie4::emisCO2")
    }
  }

  # --- validate return object
  .validateCalculation(totalStock, totalStockCheck, output)

  # --- sum pools?
  # "Caution. Interpretation of land-type specific emissions for soilc is
  # tricky because soil carbon is moved between land types in case of land-use
  # change. For instance, in case of forest-to-cropland conversion the
  # remaining fraction of soil carbon is moved from forest to cropland,
  # which will result in very high soilc emissions from forest and very high
  # negative soilc emissions from cropland.
  if (sum_cpool) {
    output <- dimSums(output, dim = "c_pools")
  } else {
    below <- dimSums(output[, , "soilc"], dim = "c_pools")
    below <- add_dimension(below, dim = 3.2, nm = "Below Ground Carbon", add = "c_pools")

    above <- dimSums(output[, , c("vegc", "litc")], dim = "c_pools")
    above <- add_dimension(above, dim = 3.2, nm = "Above Ground Carbon", add = "c_pools")

    output <- mbind(below, above)
  }

  # --- sum land?
  if (sum_land) {
    output <- dimSums(output, dim = c("land"))
  }

  # --- unit conversion?
  if (unit == "gas") {
    output <- output * 44 / 12 # Mt C/yr to Mt CO2/yr
  }

  # --- lowpass filter?
  yrFix <- as.numeric(readGDX(gdx, "sm_fix_SSP2"))
  yrsHist <- years[years > 1995 & years <= yrFix]
  yrsFut  <- years[years >= yrFix]

  # apply lowpass filter (not applied on 1st time step, applied separately on historic and future period)
  if (!is.null(lowpass)) {
    output <- mbind(output[, 1995, ],
                    lowpass(output[, yrsHist, ], i = lowpass),
                    lowpass(output[, yrsFut, ],  i = lowpass)[, -1, ])
  }

  # --- cumulative?
  if (cumulative) {
    years <- m_yeardiff(gdx)
    output[, "y1995", ] <- 0
    output <- output * years[, getYears(output), ]
    output <- as.magpie(apply(output, c(1, 3), cumsum))
    output <- output - setYears(output[, baseyear, ], NULL)
  }

  # --- aggregate over regions?
  if (level != "cell") {
    output <- superAggregate(output, aggr_type = "sum", level = level, na.rm = FALSE)
  }

  out(output, file)
}
