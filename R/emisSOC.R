#' @title emisSOC
#' @description reads detailed CO2 soil emissions out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param sumLand TRUE (default) or FALSE. Sum over land types (TRUE)
#'                 or report land-type specific emissions (FALSE).
#'
#' @return CO2 emissions as MAgPIE object
#' @author Krstine Karstens
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
  cropareaShares <- suppressMessages(
                                     toolConditionalReplace(cropareaShares, "is.na()", replaceby = 0))

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

  # defining soil stock function
  # (reimplementing calculations of cellpool_jan23 realization=
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
  # defining emissions functions based on the idea of disentangling for each time step
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
  .getEmis <- function(year, yeardiff,
                       refPNVTopsoil,
                       luTransition,
                       cropareaShares,
                       scmShare,
                       oldSocActual,
                       lossrate) {
    #### total emissions ####
    t <- # current time step
      .socStock(refPNVTopsoil  = refPNVTopsoil[, year, ],
                luTransition   = luTransition[, year, ],
                cropareaShares = cropareaShares[, year, ],
                scmShare       = scmShare[, year, ],
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])
    # tm1 <- # time step before (t minus 1 = tm1)
    #   .socStock(refPNVTopsoil  = refPNVTopsoil[, year - yeardiff, ],
    #             luTransition   = luTransition[, year - yeardiff, ],
    #             cropareaShares = cropareaShares[, year - yeardiff, ],
    #             scmShare       = scmShare[, year - yeardiff, ],
    #             oldSocActual   = oldSocActual[, year - yeardiff, ],
    #             lossrate       = lossrate[, year - yeardiff, ])

    useOldSocActual <- oldSocActual
    getNames(useOldSocActual) <- paste0("from_", getNames(useOldSocActual))
    getSets(useOldSocActual, fulldim = FALSE)[3]  <- "landFrom"
    tm1 <- dimSums(useOldSocActual[, year, ] * luTransition[, year, ], dim = "landFrom")

    # totEmis  <- t - setYears(tm1, year)
    totEmis <- -(t - tm1)

    #### climate change ####
    ctm1 <- # only climate for time step before (tm1)
      .socStock(refPNVTopsoil  = setYears(refPNVTopsoil[, year - yeardiff, ], year),
                luTransition   = luTransition[, year, ],
                cropareaShares = cropareaShares[, year, ],
                scmShare       = scmShare[, year, ],
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])
    ccEmis <- -(t - ctm1)

    #### land-use change ####
    luNoTransition <- luTransition[, year, ]
    luNoTransition[] <- 0
    staying <- paste0(paste0("from_", landtypes), ".", landtypes)
    luNoTransition[, , staying] <- dimSums(luTransition[, year, ], dim = "landTo")

    lutm1 <- # only land use for time step before (tm1)
      .socStock(refPNVTopsoil  = refPNVTopsoil[, year, ],
                luTransition   = luNoTransition,
                cropareaShares = cropareaShares[, year, ],
                scmShare       = scmShare[, year, ],
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    .mapStock2FutureLu <- function(stock, luTrans) {
      oldLand <- dimSums(luTrans, dim = "landTo")
      getNames(oldLand) <- gsub("from_", "", getNames(oldLand))
      temp <- suppressMessages(toolConditionalReplace(stock / oldLand, "is.na()", 0))
      getNames(temp) <- paste0("from_", getNames(temp))
      getSets(temp, fulldim = FALSE)[3]  <- "landFrom"
      return(dimSums(temp * luTrans, dim = "landFrom"))
    }
    luEmis <- -(t - .mapStock2FutureLu(lutm1, luTransition[, year, ]))

    #### management change ####
    mtm1 <- # only management for time step before (tm1)
      .socStock(refPNVTopsoil  = refPNVTopsoil[, year, ],
                luTransition   = luTransition[, year, ],
                cropareaShares = setYears(cropareaShares[, year - yeardiff, ], year),
                scmShare       = scmShare[, year, ],
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])
    maEmis <- -(t - mtm1)

    #### SCM change ####
    scmtm1 <- # only scm for time step before (tm1)
      .socStock(refPNVTopsoil  = refPNVTopsoil[, year, ],
                luTransition   = luTransition[, year, ],
                cropareaShares = cropareaShares[, year, ],
                scmShare       = setYears(scmShare[, year - yeardiff, ], year),
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])
    scmEmis <- -(t - scmtm1)

    #### 1st order interaction M+SCM term ####
    mtm1scmtm1 <- # only management and scm for time step before (tm1)
      .socStock(refPNVTopsoil  = refPNVTopsoil[, year, ],
                luTransition   = luTransition[, year, ],
                cropareaShares = setYears(cropareaShares[, year - yeardiff, ], year),
                scmShare       = setYears(scmShare[, year - yeardiff, ], year),
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisMaScm <- -(t + mtm1scmtm1 - mtm1 - scmtm1)

    #### 1st order interaction LU+SCM term ####
    lutm1scmtm1 <- # only land use and scm for time step before (tm1)
      .socStock(refPNVTopsoil  = refPNVTopsoil[, year, ],
                luTransition   = luNoTransition,
                cropareaShares = cropareaShares[, year, ],
                scmShare       = setYears(scmShare[, year - yeardiff, ], year),
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisLuScm <- -(t + .mapStock2FutureLu(lutm1scmtm1, luTransition[, year, ]) -
                       .mapStock2FutureLu(lutm1, luTransition[, year, ]) - scmtm1)

    #### 1st order interaction LU+M term ####
    lutm1mtm1 <- # only land use and management for time step before (tm1)
      .socStock(refPNVTopsoil  = refPNVTopsoil[, year, ],
                luTransition   = luNoTransition,
                cropareaShares = setYears(cropareaShares[, year - yeardiff, ], year),
                scmShare       = scmShare[, year, ],
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisLuMa <- -(t + .mapStock2FutureLu(lutm1mtm1, luTransition[, year, ]) -
                      .mapStock2FutureLu(lutm1, luTransition[, year, ]) - mtm1)

    #### 1st order interaction C+LU term ####
    ctm1lutm1 <- # climate and land use for time step before (tm1)
      .socStock(refPNVTopsoil  = setYears(refPNVTopsoil[, year - yeardiff, ], year),
                luTransition   = luNoTransition,
                cropareaShares = cropareaShares[, year, ],
                scmShare       = scmShare[, year, ],
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisCliLu <- -(t + .mapStock2FutureLu(ctm1lutm1, luTransition[, year, ]) -
                       .mapStock2FutureLu(lutm1, luTransition[, year, ]) - ctm1)

    #### 1st order interaction C+M term ####
    ctm1mtm1 <- # climte  and  management for time step before (tm1)
      .socStock(refPNVTopsoil  = setYears(refPNVTopsoil[, year - yeardiff, ], year),
                luTransition   = luTransition[, year, ],
                cropareaShares = setYears(cropareaShares[, year - yeardiff, ], year),
                scmShare       = scmShare[, year, ],
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisCliMa <- -(t + ctm1mtm1 - mtm1 - ctm1)

    #### 1st order interaction C+SCM term ####
    ctm1scmtm1 <- # climte and scm for time step before (tm1)
      .socStock(refPNVTopsoil  = setYears(refPNVTopsoil[, year - yeardiff, ], year),
                luTransition   = luTransition[, year, ],
                cropareaShares = cropareaShares[, year, ],
                scmShare       = setYears(scmShare[, year - yeardiff, ], year),
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisCliScm <- -(t + ctm1scmtm1 - scmtm1 - ctm1)

    #### 2nd order interaction LU+M+SCM term ####
    lutm1mtm1scmtm1 <- # land-use, management and scm for time step before (tm1)
      .socStock(refPNVTopsoil  = refPNVTopsoil[, year, ],
                luTransition   = luNoTransition,
                cropareaShares = setYears(cropareaShares[, year - yeardiff, ], year),
                scmShare       = setYears(scmShare[, year - yeardiff, ], year),
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisLuMaScm <- -(t - .mapStock2FutureLu(lutm1mtm1scmtm1, luTransition[, year, ]) +
                         .mapStock2FutureLu(lutm1mtm1, luTransition[, year, ]) +
                         .mapStock2FutureLu(lutm1scmtm1, luTransition[, year, ]) + mtm1scmtm1 -
                         .mapStock2FutureLu(lutm1, luTransition[, year, ]) - mtm1 - scmtm1)

    #### 2nd order interaction C+LU+M term ####
    ctm1lutm1mtm1 <- # climate, land-use and management for time step before (tm1)
      .socStock(refPNVTopsoil  = setYears(refPNVTopsoil[, year - yeardiff, ], year),
                luTransition   = luNoTransition,
                cropareaShares = setYears(cropareaShares[, year - yeardiff, ], year),
                scmShare       = scmShare[, year, ],
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisCliLuMa <- -(t -  .mapStock2FutureLu(ctm1lutm1mtm1, luTransition[, year, ]) +
                         .mapStock2FutureLu(lutm1mtm1, luTransition[, year, ]) +
                         .mapStock2FutureLu(ctm1lutm1, luTransition[, year, ]) + ctm1mtm1 -
                         .mapStock2FutureLu(lutm1, luTransition[, year, ]) - mtm1 - ctm1)

    #### 2nd order interaction C+LU+SCM term ####
    ctm1lutm1scmtm1 <- # climate, land-use and scm for time step before (tm1)
      .socStock(refPNVTopsoil  = setYears(refPNVTopsoil[, year - yeardiff, ], year),
                luTransition   = luNoTransition,
                cropareaShares = cropareaShares[, year, ],
                scmShare       = setYears(scmShare[, year - yeardiff, ], year),
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisCliLuScm <- -(t -  .mapStock2FutureLu(ctm1lutm1scmtm1, luTransition[, year, ]) +
                          .mapStock2FutureLu(lutm1scmtm1, luTransition[, year, ]) +
                          .mapStock2FutureLu(ctm1lutm1, luTransition[, year, ]) + ctm1scmtm1 -
                          .mapStock2FutureLu(lutm1, luTransition[, year, ]) - scmtm1 - ctm1)

    #### 2nd order interaction C+M+SCM term ####
    ctm1mtm1scmtm1 <- # climate, management and scm for time step before (tm1)
      .socStock(refPNVTopsoil  = setYears(refPNVTopsoil[, year - yeardiff, ], year),
                luTransition   = luTransition[, year, ],
                cropareaShares = setYears(cropareaShares[, year - yeardiff, ], year),
                scmShare       = setYears(scmShare[, year - yeardiff, ], year),
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisCliMaScm <- -(t - ctm1mtm1scmtm1 +
                          mtm1scmtm1 + ctm1scmtm1 + ctm1mtm1 -
                          scmtm1 - mtm1 - ctm1)

    #### 3rd order interaction C+LU+M+SCM term ####
    ctm1lutm1mtm1scmtm1 <- # climte, land-use, management and scm for time step before (tm1)
      .socStock(refPNVTopsoil  = setYears(refPNVTopsoil[, year - yeardiff, ], year),
                luTransition   = luNoTransition,
                cropareaShares = setYears(cropareaShares[, year - yeardiff, ], year),
                scmShare       = setYears(scmShare[, year - yeardiff, ], year),
                oldSocActual   = oldSocActual[, year, ],
                lossrate       = lossrate[, year, ])

    iaEmisCliLuMaScm <- -(t + .mapStock2FutureLu(ctm1lutm1mtm1scmtm1, luTransition[, year, ]) -
                            ctm1 - .mapStock2FutureLu(lutm1, luTransition[, year, ]) - mtm1 - scmtm1 +
                            .mapStock2FutureLu(ctm1lutm1, luTransition[, year, ]) + ctm1mtm1 + ctm1scmtm1 +
                            .mapStock2FutureLu(lutm1mtm1, luTransition[, year, ]) +
                            .mapStock2FutureLu(lutm1scmtm1, luTransition[, year, ]) + mtm1scmtm1 -
                            .mapStock2FutureLu(lutm1mtm1scmtm1, luTransition[, year, ]) - ctm1mtm1scmtm1 -
                            .mapStock2FutureLu(ctm1lutm1mtm1, luTransition[, year, ]) -
                            .mapStock2FutureLu(ctm1lutm1scmtm1, luTransition[, year, ]))

    #### Binding ####
    out <- mbind(add_dimension(totEmis, add = "emis", nm = "totEmis"),
                 add_dimension(ccEmis,  add = "emis", nm = "ccEmis"),
                 add_dimension(luEmis,  add = "emis", nm = "luEmis"),
                 add_dimension(maEmis,  add = "emis", nm = "maEmis"),
                 add_dimension(scmEmis, add = "emis", nm = "scmEmis"),
                 add_dimension(-iaEmisCliLu,      add = "emis", nm = "iaEmisCliLu"),
                 add_dimension(-iaEmisCliMa,      add = "emis", nm = "iaEmisCliMa"),
                 add_dimension(-iaEmisCliScm,     add = "emis", nm = "iaEmisCliScm"),
                 add_dimension(-iaEmisLuMa,       add = "emis", nm = "iaEmisLuMa"),
                 add_dimension(-iaEmisLuScm,      add = "emis", nm = "iaEmisLuScm"),
                 add_dimension(-iaEmisMaScm,      add = "emis", nm = "iaEmisMaScm"),
                 add_dimension(iaEmisCliLuMa,    add = "emis", nm = "iaEmisCliLuMa"),
                 add_dimension(iaEmisCliLuScm,   add = "emis", nm = "iaEmisCliLuScm"),
                 add_dimension(iaEmisCliMaScm,   add = "emis", nm = "iaEmisCliMaScm"),
                 add_dimension(iaEmisLuMaScm,    add = "emis", nm = "iaEmisLuMaScm"),
                 add_dimension(-iaEmisCliLuMaScm, add = "emis", nm = "iaEmisCliLuMaScm"))

    #### residual effect from legacy ####
    residual <- collapseNames(out[, , "totEmis"] -
                                dimSums(out[, , c("totEmis"), invert = TRUE], dim = 3.1))
    out      <- mbind(out, add_dimension(residual, add = "emis", nm = "resEmis"))

    #### Return ####
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
  loss <- function(diff, lossrate) {
    return(1 - lossrate^diff)
  }

  # calculating emissions with legacy effects
  emisWithLegacy <- NULL
  for (i in seq_along(years)) {
    if (i == 1) next
    for (j in c(1:legacySteps[, years[i], ])) {
      if (j == 1) {
        emisTemp    <- emis[, years[i], ]        # emissions of current timestep
      } else {                                   # legacy emissions from past timesteps
        legacyloss  <- loss(sum(yeardiff[, (i - j + 1):i, ]), lossrate) -
                         loss(sum(yeardiff[, (i - j + 1):(i - 1), ]), lossrate)
        pastloss    <- loss(yeardiff[, (i - j + 1), ], lossrate)
        emisTemp    <-
          emisTemp + toolConditionalReplace(emis[, years[i - j + 1], ] * legacyloss / pastloss, "is.na()", 0)
      }
    }
    emisWithLegacy <- mbind(emisWithLegacy, setYears(emisTemp, years[i]))
  }

  ignoreDim <- c("totEmis", "resEmis")
  totEmisBottomUp  <- dimSums(emisWithLegacy[, , ignoreDim, invert = TRUE], dim = c(1, 3))
  totEmisStockDiff <- dimSums(emis[, , c("totEmis")], dim = c(1, 3))

  # Checking if emissions differ too much (mismatches by end of the century still greater than 1%)
  if (any(abs((totEmisBottomUp - totEmisStockDiff) /
                totEmisStockDiff)[, (length(years) - 6):length(years) - 1, ] > 0.01)) {
    warning("Attribution emissions including legacy effects to different drivers in post processing failed.")
  }
  ##### calculating soil emissions incl. legacy interaction END ####

  ##### calculating soil emissions attributing interaction and residuals START ####

  # attributing interaction effects by splitting them  upon components
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

  ##### adding subsoil emissions, binding and returning START ####
  refPNVSubsoil <- gdx2::readGDX(gdx, "i59_subsoilc_density")[, years, ]
  land          <- gdx2::readGDX(gdx, "ov_land", select = list(type = "level"))
  if (sumLand) land <- dimSums(land, dim = 3)
  subStock      <- refPNVSubsoil * land
  ccEmisSub     <- -(subStock[, years[-1], ] -  setYears(subStock[, years[-length(years)], ], years[-1]))

  if (sumLand) emisTot <- dimSums(emisTot, dim = 3)

  out <- mbind(emisFullAttributed,
               add_dimension(ccEmisSub,           dim = 3.1, add = "emis", nm = "ccEmisSub"),
               add_dimension(emisTot + ccEmisSub, dim = 3.1, add = "emis", nm = "totalEmis"))

  # testing finally
  checkFinal <- out[, , "totalEmis"] - dimSums(out[, , "totalEmis", invert = TRUE], dim = 3.1)
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
