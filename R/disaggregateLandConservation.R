#' disaggregateLandConservation
#'
#' Read land conservation data from a fulldata.gdx and disaggregate to high resolution (0.5 deg).
#'
#' @param gdx character, path to a fulldata.gdx
#' @param cfg list, config data usually obtained with gms::loadConfig("config.yml")
#' @param mapping dataframe, must include columns cell and cluster
#' @param wdpaHr magclass, World Database on Protected Areas data in high resolution
#' @param conservationPrioHr magclass, high resolution data on conservation priority areas
#' @return magclass, high resolution land conservation area
#'
#' @author Patrick v. Jeetze, Pascal Sauer
#' @export
disaggregateLandConservation <- function(gdx, cfg, mapping, wdpaHr, conservationPrioHr) {
  landConsvLr <- gdx2::readGDX(gdx, "pm_land_conservation", react = "silent")
  landConsvLr <- dimSums(landConsvLr, dim = 3.2)

  # create full time series
  landConsvHr <- new.magpie(mapping[, "cell"], getYears(landConsvLr), getItems(wdpaHr, dim = 3.2),
                            fill = 0, sets = c("x.y.iso", "year", "data"))

  consvIso <- gdx2::readGDX(gdx, "policy_countries22")
  consvIso <- consvIso[consvIso %in% getItems(wdpaHr, dim = 1.3)]
  if (length(consvIso) == 0) {
    warning("No countries selected in land conservation disaggregation. Results may be erroneous")
  }

  baseProtectSelect <- cfg$gms$c22_base_protect
  baseProtectNoselect <- cfg$gms$c22_base_protect_noselect

  if (!all(c(baseProtectSelect, baseProtectNoselect) %in% "none")) {

    if (baseProtectNoselect != "none") {
      landConsvHr[, getYears(landConsvHr), ] <- collapseDim(wdpaHr[, nyears(wdpaHr), baseProtectNoselect], dim = 3.1)
      landConsvHr[, getYears(wdpaHr), ] <- collapseDim(wdpaHr[, , baseProtectNoselect], dim = 3.1)
    }
    if (baseProtectSelect != "none") {
      landConsvHr[consvIso, , ] <- collapseDim(wdpaHr[consvIso, nyears(wdpaHr), baseProtectSelect], dim = 3.1)
    } else {
      landConsvHr[consvIso, , ] <- 0
    }
  }

  consvSelect <- cfg$gms$c22_protect_scenario
  consvNoselect <- cfg$gms$c22_protect_scenario_noselect

  if (!all(c(consvSelect, consvNoselect) %in% "none")) {
    if (!is.null(conservationPrioHr)) {
      consvPrioAll <- conservationPrioHr
      consvPrioHr <- new.magpie(cells_and_regions = mapping[, "cell"],
                                names = getNames(consvPrioAll, dim = 2), fill = 0,
                                sets = c("x.y.iso", "year", "data"))

      if (consvNoselect != "none") {
        consvPrioHr <- collapseDim(consvPrioAll[, , consvNoselect], dim = 3.1)
      }
      if (consvSelect != "none") {
        consvPrioHr[consvIso, , ] <- collapseDim(consvPrioAll[consvIso, , consvSelect], dim = 3.1)
      } else {
        consvPrioHr[consvIso, , ] <- 0
      }
      # future conservation only pertains to natveg
      consvPrioHr[, , c("crop", "past", "forestry", "urban")] <- 0
      consvFader <- gdx2::readGDX(gdx, "p22_conservation_fader", format = "first_found")
      consvPrioHr <- consvPrioHr * consvFader[, getYears(landConsvHr), ]

      # add conservation priority areas
      landConsvHr <- (landConsvHr + consvPrioHr)
    }
  }
  # Due to internal constraints and compensation (e.g. NDC forest conservation)
  # the actual land conservation can sometimes be smaller than the land
  # conservation in the input data (this can especially happen also if
  # land restoration is switched off). Therefore a scaling is applied here separately
  # for grassland and natural vegetation
  natveg <- c("primforest", "secdforest", "other")
  consvSumLr <- mbind(landConsvLr[, , "past"],
                      setNames(dimSums(landConsvLr[, , natveg], dim = 3), "natveg"))
  consvSumHrAgg <- mbind(
    toolAggregate(landConsvHr[, , "past"], mapping, from = "cell", to = "cluster"),
    toolAggregate(setNames(dimSums(landConsvHr[, , natveg], dim = 3), "natveg"),
                  mapping, from = "cell", to = "cluster")
  )
  consvScaling <- consvSumLr / consvSumHrAgg
  consvScaling[is.na(consvScaling) | is.infinite(consvScaling)] <- 1
  consvScaling <- toolAggregate(consvScaling, mapping, from = "cluster", to = "cell")
  landConsvHr[, , "past"] <- consvScaling[, , "past"] * landConsvHr[, , "past"]
  landConsvHr[, , natveg] <- consvScaling[, , "natveg"] * landConsvHr[, , natveg]
  return(landConsvHr)
}
