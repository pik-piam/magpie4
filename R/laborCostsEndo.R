#' @title laborCostsEndo
#' @description reads MAgPIE endogenous labor costs for crop and livestock production from gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param products products for which labor costs should be reported ("kcr" or "kli",
#' for other products use factorCosts())
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("grid" or "iso", for regional/global
#' use factorCosts())
#' @return MAgPIE object containing labor costs [million US$17]
#' @author Debbora Leip
#' @importFrom magclass add_dimension getNames mbind
#' @importFrom madrat toolAggregate toolCountryFill
#' @examples
#' \dontrun{
#' x <- laborCostsEndo(gdx)
#' }
#'
laborCostsEndo <- function(gdx, products = "kcr", file = NULL, level = "grid") {

  if (!(level %in% c("iso", "grid"))) {
    stop("This function is only for iso and grid level. For regional/global results use the function factorCosts()")
  }

  if (!is.null(readGDX(gdx, "i38_fac_req", react = "silent"))) { # new factor requirements implementation
    if (products == "kcr") {
      if (!is.null(readGDX(gdx, "ov38_laborhours_need", react = "silent"))) {  # sticky labor realization
        laborHoursNeed <- readGDX(gdx, "ov38_laborhours_need", select = list(type = "level"))
        hourlyCosts    <- readGDX(gdx, "pm_hourly_costs")
        hourlyCosts    <- gdxAggregate(gdx, hourlyCosts, to = "cell", absolute = FALSE)

        years <- intersect(getYears(laborHoursNeed), getYears(hourlyCosts))
        costsPerOutput <- laborHoursNeed * hourlyCosts[, , "scenario", drop = TRUE]
        scale <- FALSE # in this realization, productivity changes are already incorporated directly

      } else { # sticky and per ton
        factorRequirements <- gdxAggregate(gdx, readGDX(gdx, "i38_fac_req"), to = "iso", absolute = FALSE)

        if (!is.null(readGDX(gdx, "p38_capital_cost_shares_iso", react = "silent"))) {
          laborShare <- 1 - readGDX(gdx, "p38_capital_cost_shares_iso")
        } else {
          laborShare <- readGDX(gdx, c("pm_factor_cost_shares", "pm_cost_share_crops", "p38_cost_share"),
                              react = "silent", format = "first_found")[, , "labor"]
          laborShare <- gdxAggregate(gdx, laborShare, to = "iso", absolute = FALSE)
        }

        years <- intersect(getYears(factorRequirements), getYears(laborShare))
        costsPerOutput <- factorRequirements[, years, ] * laborShare[, years, , drop = TRUE]
        scale <- TRUE # these are baseline costs per output, scaling for productivity/wage scenarios below

      }
    } else if (products == "kli") {
      if (!is.null(readGDX(gdx, "i70_fac_req_livst", react = "silent"))) {
        facReqLivst <- readGDX(gdx, "i70_fac_req_livst")
      } else {
        regression   <- readGDX(gdx, "i70_cost_regr")[, , "fish", invert = TRUE]
        sysToKli     <- readGDX(gdx, "sys_to_kli")
        productivity <- toolAggregate(readGDX(gdx, "i70_livestock_productivity"),
                                      from = "sys", to = "kli", rel = sysToKli, dim = 3)
        facReqLivst  <- (regression[, , "cost_regr_a", drop = TRUE] + 
                            regression[, , "cost_regr_b", drop = TRUE] * productivity)
      }
      facReqLivst <- gdxAggregate(gdx, facReqLivst, to = "iso", absolute = FALSE)

      if (!is.null(readGDX(gdx, "p38_capital_cost_shares_iso", react = "silent"))) {
        laborShare <- 1 - readGDX(gdx, "p38_capital_cost_shares_iso")
      } else {
        laborShare <- readGDX(gdx, c("pm_factor_cost_shares", "p70_cost_share_livst"),
                             react = "silent", format = "first_found")[, , "labor"]
        laborShare <- gdxAggregate(gdx, laborShare, to = "iso", absolute = FALSE)
      }

      years <- intersect(getYears(facReqLivst), getYears(laborShare))
      costsPerOutput <- facReqLivst[, years, ] * laborShare[, years, , drop = TRUE]
      scale <- TRUE # these are baseline costs per output, scaling for productivity/wage scenarios below
    } else {
      stop("This function only calculates labor costs for crops and livstock. For other products use factorCosts()")
    }

    # in case of scenarios affecting labor productivity or hourly labor costs
    if (!is.null(readGDX(gdx, "pm_productivity_gain_from_wages", react = "silent")) && scale) {
        productivityGain <- readGDX(gdx, "pm_productivity_gain_from_wages")
        hourlyCosts <- readGDX(gdx, "pm_hourly_costs")
        scalingFactor <- collapseDim((1 / productivityGain) * (hourlyCosts[, , "scenario"] / hourlyCosts[, , "baseline"]))
        scalingFactor <- gdxAggregate(gdx, scalingFactor, to = "iso", absolute = FALSE)
        costsPerOutput <- costsPerOutput * scalingFactor
    }
  } else { # no calculations for old factor costs implementation
    costsPerOutput <- NULL
  }

  # get production and calculate labor costs as production * costs per output, with some special handling for iso level results in case of sticky labor 
  if (!is.null(readGDX(gdx, "ov38_laborhours_need", react = "silent")) && level == "iso") { # for sticky labor costsPerOutput on cluster level
    prod <- production(gdx, level = "grid", products = products)
    costsPerOutput <- gdxAggregate(gdx, costsPerOutput, to = "grid", absolute = FALSE)
    laborCosts <- prod * costsPerOutput
    laborCosts <- gdxAggregate(gdx, laborCosts, to = "iso", absolute = TRUE)
    laborCosts <- toolCountryFill(laborCosts, fill = 0)
  } else {
    prod <- production(gdx, level = level, products = products)
    if (level == "iso") prod <- toolCountryFill(prod, fill = 0)
    if (level == "grid") costsPerOutput <- gdxAggregate(gdx, costsPerOutput, to = "grid", absolute = FALSE)
    laborCosts <- prod * costsPerOutput
  }
  

  ## if labor cost shares on iso level are used, the results here will not add up to the regional labor costs calculated in 
  ## factorCosts, because the latter uses the regional labor cost share (for which we use constant factor costs of the last 
  ## historical year as aggregation weight in MAgpIE). To ensure that the results are consistent, we use the results on iso
  ## level to disagregate the regional labor costs calculated with factorCosts.
  if (!is.null(laborCosts)) {
    weightCropTypes <- laborCosts / dimSums(laborCosts, dim = 3)
    weightCropTypes[is.na(weightCropTypes)] <- 0   
    laborCostsReg <- factorCosts(gdx, products = products, level = "reg")[, , "labor_costs", drop = TRUE]
    laborCosts <- gdxAggregate(gdx, laborCostsReg, weight = dimSums(laborCosts, dim = 3), to = level, absolute = TRUE)
    laborCosts <- weightCropTypes * laborCosts
  } 

  out(laborCosts, file)
}
