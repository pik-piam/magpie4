#' @title emisSOC
#' @description Reads detailed CO2 soil emissions out of a MAgPIE gdx file using
#'              Shapley-style decomposition with interaction terms
#'
#' @details
#' This function uses a structural decomposition approach (Shapley decomposition)
#' to attribute soil carbon emissions to different drivers and their interactions.
#' The framework is based on counterfactual analysis where all possible combinations
#' of driver states are evaluated following the inclusion-exclusion principle.
#'
#' For four drivers, the total emission is decomposed as:
#'
#' totEmissions = main_effects + first_order_interactions +
#'                second_order_interactions + third_order_interactions + residual
#'
#' This creates 16 counterfactual scenarios (2^4) to isolate individual and combined
#' effects. Main effects capture changes when one driver varies while others stay
#' constant. Interaction terms capture synergies where driver combinations amplify
#' or dampen effects beyond simple addition.
#'
#' Drivers included:
#' - Climate (C): Changes in reference soil carbon density
#' - Land-use (LU): Transitions between land types
#' - Management (M): Changes in crop type shares, fallow, and treecover
#'   * Can be split into treecover vs other management in attribution
#' - Soil Carbon Management (SCM): Enhanced soil carbon practices
#'
#' Legacy effects are incorporated using exponential decay (85% remaining per year)
#' over 100 years. Interaction terms are proportionally attributed back to main
#' drivers in the final output.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param sumLand TRUE (default) or FALSE. Sum over land types (TRUE)
#'                 or report land-type specific emissions (FALSE).
#'
#' @return CO2 emissions as MAgPIE object
#' @author Kristine Karstens
#'
#' @importFrom magclass dimSums add_dimension getSets getCells getNames add_columns
#'  collapseNames collapseDim nyears getYears setYears getItems new.magpie as.magpie
#' @importFrom madrat toolConditionalReplace
#' @examples
#' \dontrun{
#' x <- emisSOC(gdx)
#' }
emisSOC <- function(gdx, file = NULL, sumLand = FALSE) {

  years     <- gdx2::readGDX(gdx, "t")
  yeardiff  <- magpie4::timePeriods(gdx)
  landtypes <- gdx2::readGDX(gdx, "land")
  kcr       <- gdx2::readGDX(gdx, "kcr")

  ##### loading all parameters START #####
  managementRatio <- gdx2::readGDX(gdx, "i59_cratio")
  fallowRatio     <- gdx2::readGDX(gdx, "i59_cratio_fallow")
  treecoverRatio  <- gdx2::readGDX(gdx, "i59_cratio_treecover")
  scmRatio        <- gdx2::readGDX(gdx, "i59_scm_ratio", "i59_cratio_scm", react = "silent")
  if (is.null(scmRatio)) scmRatio <- 0
  lossrate        <- gdx2::readGDX(gdx, "i59_lossrate")
  ##### loading all parameters END ####

  ##### loading all variables START ####

  # climate change information
  refPNVTopsoil <- gdx2::readGDX(gdx, "f59_topsoilc_density")[, years, ]
  if (all(refPNVTopsoil[, years[1], ] == refPNVTopsoil[, years[length(years)], ])) {
    cc <- FALSE
  } else {
    cc <- TRUE
  }

  # land-use change information
  luTransition <- gdx2::readGDX(gdx, "ov_lu_transitions", select = list(type = "level"))
  names(dimnames(luTransition))[[3]] <- "landFrom.landTo"
  getNames(luTransition, dim = "landFrom") <- paste0("from_", getNames(luTransition, dim = "landFrom"))

  # management change information
  croparea  <- gdx2::readGDX(gdx, "ov_area", select = list(type = "level"))
  fallow    <- gdx2::readGDX(gdx, "ov_fallow", select = list(type = "level"))
  treecover <- gdx2::readGDX(gdx, "ov_treecover", select = list(type = "level"))
  croparea  <- add_columns(croparea, dim = 3.1, addnm = "fallow", fill = 0)
  croparea  <- add_columns(croparea, dim = 3.1, addnm = "treecover", fill = 0)
  croparea[, , "fallow.rainfed"] <- fallow
  croparea[, , "treecover.rainfed"] <- treecover
  cropareaShares <- croparea / dimSums(croparea, dim = 3)
  cropareaShares <- suppressMessages(toolConditionalReplace(cropareaShares, "is.na()", replaceby = 0))

  # soil carbon management (scm) change information
  scmShare <- gdx2::readGDX(gdx, "i59_scm_target", react = "silent")
  if (is.null(scmShare)) scmShare <- new.magpie(getCells(croparea), years, fill = 0)
  if (all(scmShare == 0)) {
    scm <- FALSE
  } else {
    scm <- TRUE
  }

  # previous soil carbon densities
  oldSocActual <- gdx2::readGDX(gdx, "p59_carbon_density")[, years, ]

  ##### loading all variables END ####

  ##### calculating soil stock START ####

  # define helper function calculating soil carbon stock given drivers
  # (reimplementing calculations of cellpool_jan23 realization)
  .socStock <- function(refPNVTopsoil,
                        luTransition,
                        cropareaShares,
                        scmShare,
                        oldSocActual,
                        lossrate) {

    socTargetCropland  <-
      (dimSums(cropareaShares[, , kcr] * managementRatio, dim = 3) *
       (1 + scmShare * (scmRatio - 1)) +
       dimSums(cropareaShares[, , "fallow"], dim = 3) * fallowRatio +
       dimSums(cropareaShares[, , "treecover"], dim = 3) * treecoverRatio) *
      dimSums(luTransition[, , "crop"], dim = "landFrom") * refPNVTopsoil

    socTargetNoncropland <- dimSums(luTransition, dim = "landFrom")[, , "crop", invert = TRUE] * refPNVTopsoil

    socTarget <- mbind(setNames(collapseNames(socTargetCropland), "crop"),
                       collapseNames(socTargetNoncropland))

    getNames(oldSocActual) <- paste0("from_", getNames(oldSocActual))
    getSets(oldSocActual)[3]  <- "landFrom"
    socActual <- lossrate * socTarget +
      (1 - lossrate) *
        dimSums(oldSocActual * luTransition, dim = "landFrom")
    socActual <- collapseNames(socActual)

    return(socActual)
  }

  # calculating soil stock over time
  stockPost <- .socStock(refPNVTopsoil  = refPNVTopsoil,
                         luTransition   = luTransition,
                         cropareaShares = cropareaShares,
                         scmShare       = scmShare,
                         oldSocActual   = oldSocActual,
                         lossrate       = lossrate)

  stockMAgPIE <- gdx2::readGDX(gdx, "ov59_som_pool", select = list(type = "level"))
  if (any(abs(stockMAgPIE - stockPost) > 10^-5)) {
    warning("Re-calculation of soil stocks in post processing failed.")
  }

  ##### calculating soil stock END ####

  ##### Math on emissions START ####
  # This follows the Shapley decomposition / Structural Decomposition Analysis approach
  # with systematic counterfactual analysis to attribute emissions to drivers and interactions
  #
  # For 4 drivers (C, LU, M, SCM), we calculate 2^4 = 16 counterfactual states
  # Each emission component is derived using the inclusion-exclusion principle:
  #
  # totEmis_t = Delta_C * M_t * LU_t * SCM_t +           > climate change emission
  #             C_t * (Delta_LU * M_t * SCM_t +          > land-use change emissions
  #                    LU_t * (Delta_M * SCM_t +         > management change emission
  #                           M_t * Delta_SCM -          > SCM CDR (negative) emission
  #                           Delta_M * Delta_SCM) -     > interaction term management and SCM
  #                    Delta_LU * Delta_M * SCM_t -      > interaction term land use and management
  #                    Delta_LU * M_t * Delta_SCM +      > interaction term land-use and SCM
  #                    Delta_LU * Delta_M * Delta_SCM) - > 2nd order interaction terms
  #             Delta_C * Delta_LU * M_t * SCM_t -       > interaction term climate and land-use
  #             Delta_C * LU_t * Delta_M * SCM_t -       > interaction term climate and management
  #             Delta_C * LU_t * M_t * Delta_SCM +       > interaction term climate and scm
  #             Delta_C * Delta_LU * Delta_M * SCM_t +   > 2nd order interaction terms
  #             Delta_C * Delta_LU * M_t * Delta_SCM +   > 2nd order interaction terms
  #             Delta_C * LU_t * Delta_M * Delta_SCM -   > 2nd order interaction terms
  #             Delta_C * Delta_LU * Delta_M * Delta_SCM > 3rd order interaction terms
  ##### Math on emissions END ####

  ##### calculating soil emissions for time step START ####

  # defining soil emission functions
  # based on the drivers: climate, land-use change,
  #                       cropland management,
  #                       soil carbon management on cropland
  # uses Shapley decomposition with systematic counterfactual analysis.
  .getEmis <- function(year, yeardiff,
                       refPNVTopsoil,
                       luTransition,
                       cropareaShares,
                       scmShare,
                       oldSocActual,
                       lossrate) {

    # Helper: Create no-transition land-use matrix (all land stays in place)
    luNoTransition   <- luTransition[, year, ]
    luNoTransition[] <- 0
    staying          <- paste0(paste0("from_", landtypes), ".", landtypes)
    luNoTransition[, , staying] <- dimSums(luTransition[, year, ], dim = "landTo")

    # Helper: Map stock from old LU configuration to current LU
    .mapStock2FutureLu <- function(stock, luTrans) {
      oldLand           <- dimSums(luTrans, dim = "landTo")
      getNames(oldLand) <- gsub("from_", "", getNames(oldLand))
      temp              <- suppressMessages(toolConditionalReplace(stock / oldLand, "is.na()", 0))
      getNames(temp)    <- paste0("from_", getNames(temp))
      getSets(temp, fulldim = FALSE)[3] <- "landFrom"
      return(dimSums(temp * luTrans, dim = "landFrom"))
    }

    # ===== PRE-CALCULATE ALL 16 STOCKS (including COUNTERFACTUALs) =====
    # Each stock represents a different combination of drivers at time t vs tm1
    # Naming: drivers at tm1 are listed (e.g., "cliLu" means climate and landuse at tm1)

    # Prepare input states
    cliT   <- refPNVTopsoil[, year, ]
    cliTm1 <- setYears(refPNVTopsoil[, year - yeardiff, ], year)
    luT    <- luTransition[, year, ]
    luTm1  <- luNoTransition
    maT    <- cropareaShares[, year, ]
    maTm1  <- setYears(cropareaShares[, year - yeardiff, ], year)
    scmT   <- scmShare[, year, ]
    scmTm1 <- setYears(scmShare[, year - yeardiff, ], year)
    oldSoc <- oldSocActual[, year, ]
    loss   <- lossrate[, year, ]
    # special effect: treecover share

    # Extract treecover share from cropareaShares
    tcTm1 <- setYears(cropareaShares[, year - yeardiff, "treecover"], year)
    nonTree <- cropareaShares[, year, ][, , "treecover", invert = TRUE]
    nonTree <- collapseNames(nonTree / dimSums(nonTree, dim = 3) *
                      (1 - dimSums(tcTm1, dim = 3)))
    nonTree <- suppressMessages(toolConditionalReplace(nonTree, "is.na()", replaceby = 0))
    matcTm1 <- mbind(nonTree, tcTm1)

    # All drivers at current time (t)
    actual <- .socStock(cliT, luT, maT, scmT, oldSoc, loss)

    # Single drivers at tm1
    cli <- .socStock(cliTm1, luT, maT, scmT, oldSoc, loss)
    lu  <- .mapStock2FutureLu(.socStock(cliT, luTm1, maT, scmT, oldSoc, loss), luT)
    ma  <- .socStock(cliT, luT, maTm1, scmT, oldSoc, loss)
    scm <- .socStock(cliT, luT, maT, scmTm1, oldSoc, loss)
    tc  <- .socStock(cliT, luT, matcTm1, scmT, oldSoc, loss)

    # Two drivers at tm1
    cliLu   <- .mapStock2FutureLu(.socStock(cliTm1, luTm1, maT, scmT, oldSoc, loss), luT)
    cliMa   <- .socStock(cliTm1, luT, maTm1, scmT, oldSoc, loss)
    cliScm  <- .socStock(cliTm1, luT, maT, scmTm1, oldSoc, loss)
    luMa    <- .mapStock2FutureLu(.socStock(cliT, luTm1, maTm1, scmT, oldSoc, loss), luT)
    luScm   <- .mapStock2FutureLu(.socStock(cliT, luTm1, maT, scmTm1, oldSoc, loss), luT)
    maScm   <- .socStock(cliT, luT, maTm1, scmTm1, oldSoc, loss)

    # Three drivers at tm1
    cliLuMa  <- .mapStock2FutureLu(.socStock(cliTm1, luTm1, maTm1, scmT, oldSoc, loss), luT)
    cliLuScm <- .mapStock2FutureLu(.socStock(cliTm1, luTm1, maT, scmTm1, oldSoc, loss), luT)
    cliMaScm <- .socStock(cliTm1, luT, maTm1, scmTm1, oldSoc, loss)
    luMaScm  <- .mapStock2FutureLu(.socStock(cliT, luTm1, maTm1, scmTm1, oldSoc, loss), luT)

    # All four drivers at tm1 (previous)
    previous <- .mapStock2FutureLu(.socStock(cliTm1, luTm1, maTm1, scmTm1, oldSoc, loss), luT)

    # Actual previous stock (mapped to current LU)
    useOldSocActual           <- oldSocActual
    getNames(useOldSocActual) <- paste0("from_", getNames(useOldSocActual))
    getSets(useOldSocActual, fulldim = FALSE)[3] <- "landFrom"
    tm1Actual <- dimSums(useOldSocActual[, year, ] * luTransition[, year, ], dim = "landFrom")

    # ===== CALCULATE EMISSIONS USING PRE-CALCULATED STOCKS =====
    # Total emission
    totEmis <- -(actual - tm1Actual)

    # Main effects (single driver changes)
    ccEmis  <- -(actual - cli)
    luEmis  <- -(actual - lu)
    maEmis  <- -(actual - ma)
    scmEmis <- -(actual - scm)
    #singled out effect of treecover as subcomponent of management
    tcEmis  <- -(actual - tc)

    # First-order interactions (2 drivers)
    iaEmisCliLu  <- -(actual + cliLu  - cli - lu)
    iaEmisCliMa  <- -(actual + cliMa  - cli - ma)
    iaEmisCliScm <- -(actual + cliScm - cli - scm)
    iaEmisLuMa   <- -(actual + luMa   - lu  - ma)
    iaEmisLuScm  <- -(actual + luScm  - lu  - scm)
    iaEmisMaScm  <- -(actual + maScm  - ma  - scm)

    # Second-order interactions (3 drivers)
    iaEmisCliLuMa  <- -(actual - cliLuMa  + cliLu + cliMa  + luMa  - cli - lu - ma)
    iaEmisCliLuScm <- -(actual - cliLuScm + cliLu + cliScm + luScm - cli - lu - scm)
    iaEmisCliMaScm <- -(actual - cliMaScm + cliMa + cliScm + maScm - cli - ma - scm)
    iaEmisLuMaScm  <- -(actual - luMaScm  + luMa  + luScm  + maScm - lu  - ma - scm)

    # Third-order interaction (all 4 drivers)
    iaEmisCliLuMaScm <- -(actual + previous - cli - lu - ma - scm +
                            cliLu + cliMa + cliScm + luMa + luScm + maScm -
                            cliLuMa - cliLuScm - cliMaScm - luMaScm)

    # ===== BUNDLE RESULTS =====
    out <- mbind(
      add_dimension(totEmis,           add = "emis", nm = "totEmis"),
      add_dimension(ccEmis,            add = "emis", nm = "ccEmis"),
      add_dimension(luEmis,            add = "emis", nm = "luEmis"),
      add_dimension(maEmis,            add = "emis", nm = "maEmis"),
      add_dimension(scmEmis,           add = "emis", nm = "scmEmis"),
      add_dimension(-iaEmisCliLu,      add = "emis", nm = "iaEmisCliLu"),
      add_dimension(-iaEmisCliMa,      add = "emis", nm = "iaEmisCliMa"),
      add_dimension(-iaEmisCliScm,     add = "emis", nm = "iaEmisCliScm"),
      add_dimension(-iaEmisLuMa,       add = "emis", nm = "iaEmisLuMa"),
      add_dimension(-iaEmisLuScm,      add = "emis", nm = "iaEmisLuScm"),
      add_dimension(-iaEmisMaScm,      add = "emis", nm = "iaEmisMaScm"),
      add_dimension(iaEmisCliLuMa,     add = "emis", nm = "iaEmisCliLuMa"),
      add_dimension(iaEmisCliLuScm,    add = "emis", nm = "iaEmisCliLuScm"),
      add_dimension(iaEmisCliMaScm,    add = "emis", nm = "iaEmisCliMaScm"),
      add_dimension(iaEmisLuMaScm,     add = "emis", nm = "iaEmisLuMaScm"),
      add_dimension(-iaEmisCliLuMaScm, add = "emis", nm = "iaEmisCliLuMaScm")
    )

    # Calculate residual
    residual <- collapseNames(out[, , "totEmis"] -
                                dimSums(out[, , "totEmis", invert = TRUE], dim = 3.1))
    
    # Add residual emissions, and the additional info on treecover emissions (not part of the Shapley decomposition)
    out <- mbind(out, add_dimension(tcEmis,   add = "emis", nm = "tcEmis"), 
                      add_dimension(residual, add = "emis", nm = "resEmis"))

    return(out)
  }

  # calculating emission per time step
  emis <- NULL
  for (i in seq_along(years)) {
    if (i == 1) next
    temp <-  .getEmis(as.numeric(gsub("y", "", years[i])),
                      as.numeric(yeardiff[, i, ]),
                      refPNVTopsoil  = refPNVTopsoil,
                      luTransition   = luTransition,
                      cropareaShares = cropareaShares,
                      scmShare       = scmShare,
                      oldSocActual   = oldSocActual,
                      lossrate       = lossrate)
    emis <- mbind(emis, temp)
  }

  ##### calculating soil emissions for time step END ####

  ##### calculating soil emissions incl. legacy interaction START ####

  # defining the time period to cover legacy effects
  legacyPeriod <- 100
  yearsNum <- as.numeric(gsub("y", "", years))
  pastTimesteps <- rep(NA, length(years))
  for (i in seq_along(years)) {
    # Identify past timesteps that are part of the legacy period of each time step
    validIndices <- which(yearsNum >= yearsNum[i] - legacyPeriod & yearsNum < yearsNum[i])
    if (length(validIndices) > 0) {
      # count number of timesteps to include in legacy calculations
      pastTimesteps[i] <- length(validIndices)
    }
  }
  legacySteps <- new.magpie("GLO", years, "legacysteps", pastTimesteps)
  loss <- function(diff) {
    return(1 - 0.85^diff)
  }

  # calculating emissions with legacy effects
  emisWithLegacy <- NULL
  for (i in seq_along(years)) {
    if (i == 1) next
    for (j in c(1:legacySteps[, years[i], ])) {
      if (j == 1) {
        emisTemp    <- emis[, years[i], ]        # emissions of current timestep
      } else {                                   # legacy emissions from past timesteps
        legacyloss  <- loss(sum(yeardiff[, (i - j + 1):i, ])) -
                         loss(sum(yeardiff[, (i - j + 1):(i - 1), ]))
        pastloss    <- loss(yeardiff[, (i - j + 1), ])
        emisTemp    <-
          emisTemp + toolConditionalReplace(emis[, years[i - j + 1], ] * legacyloss / pastloss, "is.na()", 0)
      }
    }
    emisWithLegacy <- mbind(emisWithLegacy, setYears(emisTemp, years[i]))
  }

  ignoreDim <- c("totEmis", "resEmis", "tcEmis")
  totEmisBottomUp  <- dimSums(emisWithLegacy[, , ignoreDim, invert = TRUE], dim = c(1, 3))
  totEmisStockDiff <- dimSums(emis[, , c("totEmis")], dim = c(1, 3))

  # Checking if emissions differ too much (mismatches by end of the century still greater than 1%)
  if (any(abs((totEmisBottomUp - totEmisStockDiff) /
                totEmisStockDiff)[, (length(years) - 6):length(years) - 1, ] > 0.01)) {
    warning("Attribution emissions including legacy effects to different drivers in post processing failed.")
  }

  ##### calculating soil emissions incl. legacy interaction END ####

  ##### calculating soil emissions attributing interaction and residuals START ####

  # attributing interaction effects by splitting them upon components
  # components are not spread equally: LU interaction effects between MA and SCM are always
  # fully attributed to LU as otherwise MA and SCM effects would enter !crop land-use types
  emisTemp    <- dimSums(emisWithLegacy, dim = 3.2)

  ccEmisAttr  <- c(ccEmis = 1, iaEmisCliLu = 1 / 2, iaEmisCliMa = 1 / 2, iaEmisCliScm = 1 / 2,
                   iaEmisCliLuMa = 1 / 2, iaEmisCliLuScm = 1 / 2, iaEmisCliMaScm = 1 / 3,
                   iaEmisCliLuMaScm = 1 / 2)
  luEmisAttr  <- c(luEmis = 1, iaEmisCliLu = 1 / 2, iaEmisLuMa = 1, iaEmisLuScm = 1,
                   iaEmisCliLuMa = 1 / 2, iaEmisCliLuScm = 1 / 2, iaEmisLuMaScm = 1,
                   iaEmisCliLuMaScm = 1 / 2)
  maEmisAttr  <- c(maEmis = 1, iaEmisCliMa = 1 / 2, iaEmisLuMa = 0, iaEmisMaScm = 1 / 2,
                   iaEmisCliLuMa = 0, iaEmisCliMaScm = 1 / 3, iaEmisLuMaScm = 0,
                   iaEmisCliLuMaScm = 0)
  scmEmisAttr <- c(scmEmis = 1, iaEmisCliScm = 1 / 2, iaEmisLuScm = 0, iaEmisMaScm = 1 / 2,
                   iaEmisCliLuScm = 0, iaEmisCliMaScm = 1 / 3, iaEmisLuMaScm = 0,
                   iaEmisCliLuMaScm = 0)

  emisAttr    <- mbind(add_dimension(as.magpie(ccEmisAttr), dim = 3.1, add = "emis", nm = "ccEmisFull"),
                       add_dimension(as.magpie(luEmisAttr), dim = 3.1, add = "emis", nm = "luEmisFull"),
                       add_dimension(as.magpie(maEmisAttr), dim = 3.1, add = "emis", nm = "maEmisFull"),
                       add_dimension(as.magpie(scmEmisAttr), dim = 3.1, add = "emis", nm = "scmEmisFull"))

  emisFullAttributed <- dimSums(emisAttr * emisTemp[, , ignoreDim, invert = TRUE], dim = 3.2)

  # calculating difference after reallocating interaction terms
  checkInteractionAttribution <-
    dimSums(emisFullAttributed, dim = 3.1) -
    dimSums(emisTemp[, , ignoreDim, invert = TRUE], dim = 3.1)

  if (any(round(checkInteractionAttribution, 10) != 0)) {
    warning("Splitting interaction terms to different drivers in post processing failed.")
  }

  # clearing up numerics before distributing residuals
  nonCrop <- setdiff(getNames(emisWithLegacy, dim = 2), "crop")
  emisWithLegacy[, , "scmEmis"][, , nonCrop] <- 0 # zero by design
  emisWithLegacy[, , "maEmis"][, , nonCrop]  <- 0 # zero by design
  if (!cc) emisFullAttributed[, , "ccEmisFull"] <- 0
  if (!scm) emisFullAttributed[, , "scmEmisFull"] <- 0

  # calculating difference after reallocating interaction terms
  emisTot   <- collapseNames(emis[, , "totEmis"])
  residuals <- collapseNames(dimSums(emisTot, dim = 3) - dimSums(emisFullAttributed, dim = 3))

  # attributing residual effects by using absolute shares of emissions components
  weights   <- abs(emisFullAttributed) / dimSums(abs(emisFullAttributed), dim = 3.1)
  weights   <- toolConditionalReplace(weights, c("is.na()", "is.nan()"), 0)
  weights   <- toolConditionalReplace(weights / dimSums(weights, dim = 3), c("is.na()", "is.nan()"), 0) # normalize

  # checking if there are only non-existing weights for residuals under a treshhold
  if (any(dimSums(weights, dim = 3) < 10^-10  & residuals > 10^-10)) {
    warning("Weighting of residual emissions is not working, as there are cells with non-zero residuals,
            but all weights are zero")
  }

  emisFullAttributed <- emisFullAttributed + weights * residuals

  checkResiduals <- dimSums(emisTot, dim = 3) - dimSums(emisFullAttributed, dim = 3)
  if (any(round(checkResiduals, 10) != 0)) {
    warning("Attribution residual effects to different drivers in post processing failed.")
  }

  ##### calculating soil emissions attributing interaction and residuals END ####

  ##### calculating land-use specific soil emissions START ####
  if (!sumLand) {
    # calculating land-ise weights for c("ccEmisFull", "maEmisFull", "scmEmisFull") first
    distributeFirst <- c("ccEmisFull", "maEmisFull", "scmEmisFull")
    weights <- emisFullAttributed * emisTot
    weights[] <- 0
    weights[, , c("maEmisFull", "scmEmisFull")][, , "crop"] <- 1
    if (cc) {
      weights[, , "ccEmisFull"] <- emisWithLegacy[, , "ccEmis"] / dimSums(emisWithLegacy[, , "ccEmis"], dim = 3.2)
      weights <- toolConditionalReplace(weights, c("is.na()", "is.nan()", "is.infinite()"), 0)
      zeroWeights <- (dimSums(weights[, , "ccEmisFull"], dim = 3.2) < 10^-10)
      weights[, , "ccEmisFull"][zeroWeights] <- (emisTot / dimSums(emisTot, dim = 3))[zeroWeights]
      # Replacing NAs with zeroes again for the cases where all emissions are zero
      weights <- toolConditionalReplace(weights, c("is.na()", "is.nan()", "is.infinite()"), 0)
    } else {
      weights[, , "ccEmisFull"] <- 0
    }
    # checking if there are only non-existing weights for emission categories
    if (any(dimSums(weights[, , distributeFirst], dim = 3.2) < 10^-10  &
              emisFullAttributed[, , distributeFirst] > 10^-10)) {
      warning("Weighting of land-use types is not working, as there are cells with non-zero emissions,
            but all weights are zero")
    }

    emisTemp <- emisFullAttributed[, , distributeFirst] * weights[, , distributeFirst]

    # calculating the land-type specific LU emissions as difference ot total to all other components
    emisLu   <- emisTot - dimSums(emisTemp, dim = 3.1)
    emisFullAttributed  <- mbind(emisTemp,
                                 add_dimension(emisLu, dim = 3.1, add = "emis", nm = "luEmisFull"))

    checkSplitLandtypes <- emisTot - dimSums(emisFullAttributed, dim = 3.1)
    if (any(round(checkSplitLandtypes, 10) != 0)) {
      warning(paste0("Split of emissions to land-use types not fully in line with total emission calculation.\n",
                     "Please take the land-use types specific soil sub-emission with care."))
    }
  }
  ##### calculating land-use specific soil emissions END ####

  ##### Split management into treecover vs other management START ####
  matcEmisRaw <- collapseNames(emisWithLegacy[, , "tcEmis"]) # management tree cover emissions
  maotEmisRaw <- collapseNames(emisWithLegacy[, , "maEmis"]) - matcEmisRaw # other management emissions
  tcWeight    <- abs(matcEmisRaw) / (abs(maotEmisRaw) + abs(matcEmisRaw) + 1e-10)
  otWeight    <- abs(maotEmisRaw) / (abs(maotEmisRaw) + abs(matcEmisRaw) + 1e-10)

  maEmisDiff <- collapseNames(emisFullAttributed[, , "maEmisFull"] - emisWithLegacy[, , "maEmis"])
  matcEmisFull <- matcEmisRaw + maEmisDiff * tcWeight
  maotEmisFull <- maotEmisRaw + maEmisDiff * otWeight

  # checking if split fo management emissions worked correctly
  checkMaSplit <- emisFullAttributed[, , "maEmisFull"] - (matcEmisFull + maotEmisFull)
  if (any(abs(checkMaSplit) > 1e-8)) {
    warning("Treecover/other management split does not sum to total management emissions.")
  }
  ##### Split management into treecover vs other management END ####

  ##### adding subsoil emissions, binding and returning START ####
  refPNVSubsoil <- gdx2::readGDX(gdx, "i59_subsoilc_density")[, years, ]
  land          <- gdx2::readGDX(gdx, "ov_land", select = list(type = "level"))
  if (sumLand) land <- dimSums(land, dim = 3)
  subStock      <- refPNVSubsoil * land
  ccEmisSub     <- -(subStock[, years[-1], ] -  setYears(subStock[, years[-length(years)], ], years[-1]))

  if (sumLand) emisTot <- dimSums(emisTot, dim = 3)

  out <- mbind(emisFullAttributed,
               add_dimension(ccEmisSub,           dim = 3.1, add = "emis", nm = "ccEmisSub"),
               add_dimension(maotEmisFull,        dim = 3.1, add = "emis", nm = "maotEmisFull"),
               add_dimension(matcEmisFull,        dim = 3.1, add = "emis", nm = "matcEmisFull"),
               add_dimension(emisTot + ccEmisSub, dim = 3.1, add = "emis", nm = "totalEmis"))

  # testing finally
  ignoreDim  <- c("totalEmis", "maotEmisFull", "matcEmisFull")
  checkFinal <- out[, , "totalEmis"] - dimSums(out[, , ignoreDim, invert = TRUE], dim = 3.1)
  if (any(round(checkFinal, 10) != 0)) {
    warning("Emissions finally reported are off.")
  }

  # adding stock information
  out <- add_columns(out, addnm = "y1995", dim = 2, NA)
  out <- out[, sort(getYears(out)), ]
  stock <- stockPost + subStock
  if (sumLand) stock <- dimSums(stock, dim = 3)
  out <- mbind(out, add_dimension(stock, dim = 3.1, add = "emis", nm = "totalStock"))
  out(out, file)
  ##### adding subsoil emissions, binding and returning END ####
}
