#' @title reportFactorCosts
#' @description reports MAgPIE factor costs (split into labor and capital for sticky realization)
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return magpie object with factor costs
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' x <- reportCostsInputFactors(gdx)
#' }
#'
#'
#' @section Input factor cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs\|Input Factors | million US$2017/yr | Total input factor costs (labor + capital)
#' Costs\|Input Factors\|+\|Crops | million US$2017/yr | Factor costs for crop production
#' Costs\|Input Factors\|+\|Livestock | million US$2017/yr | Factor costs for livestock production
#' Costs\|Input Factors\|+\|Fish | million US$2017/yr | Factor costs for fish production
#' Costs\|Input Factors\|+\|Pasture | million US$2017/yr | Factor costs for pasture management
#' Costs\|Input Factors\|+\|Residues | million US$2017/yr | Factor costs for residue handling
#' Costs\|Input Factors\|++\|Labor costs | million US$2017/yr | Total labor costs
#' Costs\|Input Factors\|++\|Capital costs | million US$2017/yr | Total capital costs
#' @md

#' @importFrom magpiesets reporthelper summationhelper reportingnames
#' @importFrom magclass collapseDim mbind
#' @export

reportCostsInputFactors <- function(gdx, level = "regglo") {

  # only crop factor costs need to be read in differently depending on version and realization
  costsLivst <- factorCosts(gdx, products = "kli", level = level)
  costsFish <- factorCosts(gdx, products = "fish", level = level)
  costsResidues <- factorCosts(gdx, products = "kres", level = level)
  costsPasture <- factorCosts(gdx, products = "pasture", level = level)

  # old factor cost variable naming
  if (suppressWarnings(!is.null(readGDX(gdx, "ov_cost_prod")))) {

    # non-sticky factor costs realization
    if (is.null(readGDX(gdx, "p38_capital_mobile", react = "silent"))) {
      costsCrops <- readGDX(gdx, "ov_cost_prod", select = list(type = "level"))[, , findset("kcr")]
      costsCrops <- superAggregateX(costsCrops, aggr_type = "sum", level = level)

      factorCosts <- mbind(costsCrops, costsLivst, costsFish, costsPasture, costsResidues)
      factorCosts <- reporthelper(factorCosts, dim = 3.1,
                                  level_zero_name = "Costs|Input Factors", detail = FALSE)
      factorCosts <- summationhelper(factorCosts)

    } else { # sticky factor costs realization

      # crop factor costs different depending on sticky free or dynamic
      if (all(readGDX(gdx, "ov_cost_inv")[, , "level"] == 0)) { # sticky free
        factorCostsCrops <- factorCosts(gdx, products = "kcr", level = level)
        laborCostsCrops <- factorCostsCrops[, , "labor_costs", drop = TRUE]
        capitalCostsCropsSum <- factorCostsCrops[, , "capital_costs", drop = TRUE]
        capitalCostsCrops <- reporthelper(factorCostsCrops[, , "capital_costs", drop = TRUE], dim = 3.1,
                                    level_zero_name = "Costs|Input Factors|Capital costs", detail = FALSE)
        capitalCostsCrops <- summationhelper(capitalCostsCrops)
        factorCostsCrops <- setNames(dimSums(factorCostsCrops[, , "factor_costs", drop = TRUE], dim = 3),
                                       paste0("Costs|Input Factors|+|", reportingnames("kcr")))
      } else { # sticky dynamic
        capitalCostsCrops <- setNames(superAggregateX(readGDX(gdx, "ov_cost_inv", select = list(type = "level")),
                                                             aggr_type = "sum", level = level),
                                    paste0("Costs|Input Factors|Capital costs|+|", reportingnames("kcr")))
        capitalCostsCropsSum <- capitalCostsCrops
        laborCostsCrops <- readGDX(gdx, "ov_cost_prod", select = list(type = "level"))[, , findset("kcr")]
        laborCostsCrops <- superAggregateX(laborCostsCrops, aggr_type = "sum", level = level)
        factorCostsCrops <- setNames(dimSums(mbind(laborCostsCrops, capitalCostsCrops), dim = 3.1),
                            paste0("Costs|Input Factors|+|", reportingnames("kcr")))
      }

      # factor costs split by sector (kcr, livestock, fish, residues, pasture)
      factorCosts <- mbind(costsLivst[, , "factor_costs", drop = TRUE],
                            collapseDim(costsFish[, , "factor_costs"], dim = 3.1),
                            costsResidues[, , "factor_costs", drop = TRUE],
                            collapseDim(costsPasture[, , "factor_costs"], dim = 3.1))
      factorCosts <- reporthelper(factorCosts, dim = 3.1,
                                  level_zero_name = "Costs|Input Factors", detail = FALSE)
      factorCosts <- summationhelper(factorCosts)
      factorCosts <- mbind(factorCostsCrops, factorCosts)

      # Total labor costs and total capital costs
      capitalCosts <- setNames(dimSums(mbind(capitalCostsCropsSum,
                                              costsLivst[, , "capital_costs", drop = TRUE],
                                              collapseDim(costsFish[, , "capital_costs"], dim = 3.1),
                                              costsResidues[, , "capital_costs", drop = TRUE],
                                              collapseDim(costsPasture[, , "capital_costs"], dim = 3.1)), dim = 3.1),
                                "Costs|Input Factors|++|Capital costs")
      laborCosts <- setNames(dimSums(mbind(laborCostsCrops,
                                            costsLivst[, , "labor_costs", drop = TRUE],
                                            collapseDim(costsFish[, , "labor_costs"], dim = 3.1),
                                            costsResidues[, , "labor_costs", drop = TRUE],
                                            collapseDim(costsPasture[, , "labor_costs"], dim = 3.1)), dim = 3.1),
                              "Costs|Input Factors|++|Labor costs")
      factorCosts <- mbind(factorCosts, capitalCosts, laborCosts)

      # labor costs split by sector (kcr, livestock, fish, residues, pasture)
      laborCosts <- mbind(laborCostsCrops,
                           costsLivst[, , "labor_costs", drop = TRUE],
                           collapseDim(costsFish[, , "labor_costs"], dim = 3.1),
                           costsResidues[, , "labor_costs", drop = TRUE],
                           collapseDim(costsPasture[, , "labor_costs"], dim = 3.1))
      laborCosts <- reporthelper(laborCosts, dim = 3.1,
                                 level_zero_name = "Costs|Input Factors|Labor costs", detail = FALSE)
      laborCosts <- summationhelper(laborCosts)

      # capital costs split by sector (kcr, livestock, residues, pasture)
      capitalCosts <- mbind(costsLivst[, , "capital_costs", drop = TRUE],
                             collapseDim(costsFish[, , "capital_costs"], dim = 3.1),
                             costsResidues[, , "capital_costs", drop = TRUE],
                             collapseDim(costsPasture[, , "capital_costs"], dim = 3.1))
      capitalCosts <- reporthelper(capitalCosts, dim = 3.1,
                                   level_zero_name = "Costs|Input Factors|Capital costs", detail = FALSE)
      capitalCosts <- summationhelper(capitalCosts)
      capitalCosts <- mbind(capitalCostsCrops, capitalCosts)

      factorCosts <- mbind(factorCosts, capitalCosts, laborCosts)
    }
  } else { # new factor cost variable naming
    costsCrops <- factorCosts(gdx, products = "kcr", level = level)

    costs <- mbind(costsCrops, costsLivst, costsFish, costsPasture, costsResidues)

    # factor costs split by sector (kcr, livestock, fish, residues, pasture)
    factorCosts <- costs[, , "factor_costs", drop = TRUE]
    getNames(factorCosts) <- paste0("Costs|Input Factors|+|", getNames(factorCosts))

    # Total labor costs and total capital costs
    capitalCosts <- setNames(dimSums(costs[, , "capital_costs"], dim = 3),
                              "Costs|Input Factors|++|Capital costs")
    laborCosts <- setNames(dimSums(costs[, , "labor_costs"], dim = 3),
                            "Costs|Input Factors|++|Labor costs")
    factorCosts <- mbind(factorCosts, capitalCosts, laborCosts)

    # labor costs split by sector (kcr, livestock, fish, residues, pasture)
    laborCosts <- costs[, , "labor_costs", drop = TRUE]
    getNames(laborCosts) <- paste0("Costs|Input Factors|Labor costs|+|", getNames(laborCosts))

    # capital costs split by sector (kcr, livestock, residues, pasture)
    capitalCosts <- costs[, , "capital_costs", drop = TRUE]
    getNames(capitalCosts) <- paste0("Costs|Input Factors|Capital costs|+|", getNames(capitalCosts))

    factorCosts <- mbind(factorCosts, capitalCosts, laborCosts)
  }

  getNames(factorCosts) <- paste0(getNames(factorCosts), " (million US$2017/yr)")

  return(factorCosts)
}
