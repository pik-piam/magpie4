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
#' @importFrom luscale superAggregate
#' @importFrom madrat toolAggregate
#' @examples
#' \dontrun{
#' x <- laborCostsEndo(gdx)
#' }
#'
laborCostsEndo <- function(gdx, products = "kcr", file = NULL, level = "grid") {

  if (!(level %in% c("iso", "grid"))) {
    stop("This function is only for iso and grid level. For regional/global results use the function factorCosts()")
  }

  prod <- production(gdx, level = level, products = products)
  if (level == "iso") prod <- toolCountryFill(prod, fill = 0)

  if (suppressWarnings(!is.null(readGDX(gdx, "i38_fac_req")))) { # new factor requirements implementation
    if (products == "kcr") {
      factorRequirements <- gdxAggregate(gdx, readGDX(gdx, "i38_fac_req"), to = "iso", absolute = FALSE)

      if (suppressWarnings(!is.null(readGDX(gdx, "p38_capital_cost_shares_iso")))) {
        laborShare <- 1 - readGDX(gdx, "p38_capital_cost_shares_iso", react = "silent")
      } else {
        laborShare <- readGDX(gdx, c("pm_factor_cost_shares", "pm_cost_share_crops", "p38_cost_share"),
                             react = "silent", format = "first_found")[, , "labor"]
        laborShare <- gdxAggregate(gdx, laborShare, to = "iso", absolute = FALSE)
      }

      years <- intersect(getYears(factorRequirements), getYears(laborShare))
      costsPerOutput <- factorRequirements[, years, ] * laborShare[, years, , drop = TRUE]
    } else if (products == "kli") {
      if (suppressWarnings(!is.null(readGDX(gdx, "i70_fac_req_livst")))) {
        facReqLivst <- readGDX(gdx, "i70_fac_req_livst", react = "silent", format = "first_found")
      } else {
        regression <- readGDX(gdx, "i70_cost_regr", react = "silent", format = "first_found")[, , "fish", invert = TRUE]
        sysToKli <- readGDX(gdx, "sys_to_kli", react = "silent", format = "first_found")
        productivity <- toolAggregate(readGDX(gdx, "i70_livestock_productivity", react = "silent",
                                              format = "first_found"), from = "sys", to = "kli", rel = sysToKli, dim = 3)
        facReqLivst <- (regression[, , "cost_regr_a", drop = TRUE] + regression[, , "cost_regr_b", drop = TRUE] *
                          productivity)
      }
      facReqLivst <- gdxAggregate(gdx, facReqLivst, to = "iso", absolute = FALSE)

      if (suppressWarnings(!is.null(readGDX(gdx, "p38_capital_cost_shares_iso")))) {
        laborShare <- 1 - readGDX(gdx, "p38_capital_cost_shares_iso", react = "silent")
      } else {
        laborShare <- readGDX(gdx, c("pm_factor_cost_shares", "p70_cost_share_livst"),
                             react = "silent", format = "first_found")[, , "labor"]
        laborShare <- gdxAggregate(gdx, laborShare, to = "iso", absolute = FALSE)
      }

      years <- intersect(getYears(facReqLivst), getYears(laborShare))
      costsPerOutput <- facReqLivst[, years, ] * laborShare[, years, , drop = TRUE]
    } else {
      stop("This function only calculates labor costs for crops and livstock. For other products use factorCosts()")
    }

    # in case of scenarios affecting labor productivity or hourly labor costs
    if (suppressWarnings(!is.null(readGDX(gdx, "pm_productivity_gain_from_wages")))) {
        productivityGain <- readGDX(gdx, "pm_productivity_gain_from_wages", react = "silent", format = "first_found")
        hourlyCosts <- readGDX(gdx, "pm_hourly_costs", react = "silent", format = "first_found")
        scale <- collapseDim((1 / productivityGain) * (hourlyCosts[, , "scenario"] / hourlyCosts[, , "baseline"]))
        scale <- gdxAggregate(gdx, scale, to = "iso", absolute = FALSE)
        costsPerOutput <- costsPerOutput * scale
    }

    # iso to grid if output should be on grid level
    costsPerOutput <- gdxAggregate(gdx, costsPerOutput, to = level, absolute = FALSE)
  } else {
    costsPerOutput <- NULL
  }

  laborCosts <- prod * costsPerOutput

  out(laborCosts, file)
}
