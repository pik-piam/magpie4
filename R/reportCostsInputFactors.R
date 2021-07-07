#' @title reportFactorCosts
#' @description reports MAgPIE factor costs (split into labor and capital for sticky realization)
#'
#' @param gdx GDX file
#' @return magpie object with factor costs
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' x <- reportCostsInputFactors(gdx)
#' }
#'
#' @importFrom magpiesets reporthelper summationhelper reportingnames
#' @importFrom magclass collapseDim mbind

reportCostsInputFactors <- function(gdx) {

  if (is.null(readGDX(gdx, "p38_capital_mobile_t", react = "silent"))) {
    factor_costs_crops <- readGDX(gdx, "ov_cost_prod", select = list(type = "level"))[, , findset("kcr")]
    factor_costs_crops <- superAggregate(factor_costs_crops, aggr_type = "sum", level = "regglo")
    factor_costs_lvst <- FactorCosts(gdx, products = "kli", level = "regglo")
    factor_costs_residues <- FactorCosts(gdx, products = "kres", level = "regglo")
    factor_costs_pasture <- FactorCosts(gdx, products = "pasture", level = "regglo")

    factor_costs <- mbind(factor_costs_crops, factor_costs_lvst, factor_costs_pasture, factor_costs_residues)
    factor_costs <- reporthelper(factor_costs, dim = 3.1, level_zero_name = "Costs Optimization|Input Factors", detail = FALSE)
    factor_costs <- summationhelper(factor_costs)
  } else {
    # reading data
    capital_costs_crops <- setNames(superAggregate(readGDX(gdx, "ov_cost_inv", select = list(type = "level")), aggr_type = "sum", level = "regglo"),
                                    paste0("Costs Optimization|Input Factors|Capital costs|+|", reportingnames("kcr")))
    labor_cost_crops <-   superAggregate(readGDX(gdx, "ov_cost_prod", select = list(type = "level"))[, , findset("kcr")], aggr_type = "sum", level = "regglo")

    costs_livst <- FactorCosts(gdx, products = "kli", level = "regglo")
    costs_fish <- FactorCosts(gdx, products = "fish", level = "regglo")
    costs_residues <- FactorCosts(gdx, products = "kres", level = "regglo")
    costs_pasture <- FactorCosts(gdx, products = "pasture", level = "regglo")

    # factor costs split by sector (kcr, livestock, fish, residues, pasture)
    factor_costs_crops <- setNames(dimSums(mbind(labor_cost_crops, capital_costs_crops), dim = 3.1),
                            paste0("Costs Optimization|Input Factors|+|", reportingnames("kcr")))
    factor_costs <- mbind(costs_livst[, , "factor_costs", drop = TRUE],
                          collapseDim(costs_fish[, , "factor_costs"], dim = 3.1),
                          costs_residues[, , "factor_costs", drop = TRUE],
                          collapseDim(costs_pasture[, , "factor_costs"], dim = 3.1))
    factor_costs <- reporthelper(factor_costs, dim = 3.1, level_zero_name = "Costs Optimization|Input Factors", detail = FALSE)
    factor_costs <- summationhelper(factor_costs)
    factor_costs <- mbind(factor_costs_crops, factor_costs)

    # Total labor costs and total capital costs
    capital_costs <- setNames(dimSums(mbind(capital_costs_crops,
                                            costs_livst[, , "capital_costs", drop = TRUE],
                                            collapseDim(costs_fish[, , "capital_costs"], dim = 3.1),
                                            costs_residues[, , "capital_costs", drop = TRUE],
                                            collapseDim(costs_pasture[, , "capital_costs"], dim = 3.1)), dim = 3.1),
                              "Costs Optimization|Input Factors|++|Capital costs")
    labor_costs <- setNames(dimSums(mbind(labor_cost_crops,
                                          costs_livst[, , "labor_costs", drop = TRUE],
                                          collapseDim(costs_fish[, , "labor_costs"], dim = 3.1),
                                          costs_residues[, , "labor_costs", drop = TRUE],
                                          collapseDim(costs_pasture[, , "labor_costs"], dim = 3.1)), dim = 3.1),
                            "Costs Optimization|Input Factors|++|Labor costs")
    factor_costs <- mbind(factor_costs, capital_costs, labor_costs)

    # labor costs split by sector (kcr, livestock, fish, residues, pasture)
    labor_costs <- mbind(labor_cost_crops,
                         costs_livst[, , "labor_costs", drop = TRUE],
                         collapseDim(costs_fish[, , "labor_costs"], dim = 3.1),
                         costs_residues[, , "labor_costs", drop = TRUE],
                         collapseDim(costs_pasture[, , "labor_costs"], dim = 3.1))
    labor_costs <- reporthelper(labor_costs, dim = 3.1, level_zero_name = "Costs Optimization|Input Factors|Labor costs", detail = FALSE)
    labor_costs <- summationhelper(labor_costs)

    # capital costs split by sector (kcr, livestock, residues, pasture)
    capital_costs <- mbind(costs_livst[, , "capital_costs", drop = TRUE],
                           collapseDim(costs_fish[, , "capital_costs"], dim = 3.1),
                           costs_residues[, , "capital_costs", drop = TRUE],
                           collapseDim(costs_pasture[, , "capital_costs"], dim = 3.1))
    capital_costs <- reporthelper(capital_costs, dim = 3.1, level_zero_name = "Costs Optimization|Input Factors|Capital costs", detail = FALSE)
    capital_costs <- summationhelper(capital_costs)
    capital_costs <- mbind(capital_costs_crops, capital_costs)

    factor_costs <- mbind(factor_costs, capital_costs, labor_costs)
  }

  getNames(factor_costs) <- paste0(getNames(factor_costs), " (million US$05/yr)")

  return(factor_costs)
}
