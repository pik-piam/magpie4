#' @title factorCostShares
#' @description returns labor and capital cost share out of factor costs (i.e. labor + capital)
#'
#' @export
#'
#' @param gdx GDX file
#' @param type
#' \itemize{
#'   \item "requirements": shares from factor requirements
#'   \item "optimization": cost shares between labor and capital costs in optimization
#'   \item "accounting": cost shares based on accounting of labor and capital costs
#' }
#' @param products products for which cost shares should be reported, kcr or kli
#' @param level spatial aggregation to report employment ("reg", "glo" or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @return labor and capital cost share out of factor costs
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' x <- factorCostShares(gdx)
#' }

factorCostShares <- function(gdx, type = "optimization", products = "kcr", level = "reg", file = NULL) {

  if (type == "requirements") {

    if (products == "kcr") {

      if (is.null(readGDX(gdx, c("ov38_capital_need", "p38_capital_need"), react = "silent"))) { # per_ton impl.
        laborReq <- readGDX(gdx, "ov_cost_prod_crop", select = list(type = "level"))[, , "labor"]
        capitalReq <- readGDX(gdx, "ov_cost_prod_crop", select = list(type = "level"))[, , "capital"]
      } else {
        laborReq <- readGDX(gdx, "ov_cost_prod_crop", select = list(type = "level"))[, , "labor"]
        if (!is.null(readGDX(gdx, "ov38_laborhours_need", react = "silent"))) { # sticky_labor implementation
          capitalNeed <- dimSums(readGDX(gdx, "ov38_capital_need", select = list(type = "level")), dim = 3.2)
        } else if (!is.null(readGDX(gdx, "p38_capital_need", react = "silent"))) { # sticky implementation
          capitalNeed <- dimSums(readGDX(gdx, "p38_capital_need"), dim = 3.2)
        }
        production <- readGDX(gdx, "ov_prod", select = list(type = "level"))[, , getNames(capitalNeed)]
        interestRate <- readGDX(gdx, "pm_interest")[, getYears(production), ]
        depreciation <- readGDX(gdx, "s38_depreciation_rate")
        capitalReq <- dimSums(dimSums(capitalNeed * production, dim = 3) * (interestRate + depreciation), dim = 1.2)
      }
      x <- mbind(laborReq / collapseDim(laborReq + capitalReq),
                 setNames(capitalReq / (laborReq + capitalReq), "capital"))
      w <- setNames(laborReq + capitalReq, NULL)

    } else if (products == "kli") {
      x <- readGDX(gdx, c("pm_factor_cost_shares", "p70_cost_share_livst"), react = "silent", format = "first_found")
      w <- factorCosts(gdx, products = products, level = "reg")[, , "factor_costs", drop = TRUE]
    } else {
      stop("Product type not supported")
    }
    if (!is.null(x)) {
      w <- mbind(setNames(w, "labor"), setNames(w, "capital"))
      x <- superAggregateX(x, aggr_type = "weighted_mean", weight = w, level = level)
    }

  } else if (type == "optimization") {

    factorCosts <- collapseDim(factorCosts(gdx, products = products, level = "reg"))
    # if labor, capital, and factor costs are reported, shares can be calculated
    if (all(c("labor_costs", "capital_costs", "factor_costs") %in% getNames(factorCosts))) {
      x <- factorCosts[, , c("labor_costs", "capital_costs")] / factorCosts[, , "factor_costs", drop  = TRUE]
      w <- factorCosts[, , "factor_costs"]
      w <- mbind(setNames(w, "labor_costs"), setNames(w, "capital_costs"))
      x <- superAggregateX(x, aggr_type = "weighted_mean", weight = w, level = level)
    } else {
      x <- NULL
    }

  } else if (type == "accounting") {

    labor <- collapseDim(factorCosts(gdx, products = products, level = "reg"))[, , "labor_costs"]

    if (products == "kli") {
      capital <- collapseDim(factorCosts(gdx, products = products, level = "reg"))[, , "capital_costs"]
    } else if (products == "kcr") { # accounting of investments only for crops and only for sticky/sticky_labor
      investmentImmobile <- readGDX(gdx, "ov38_investment_immobile", select = list(type = "level"), react = "silent")
      investmentMobile <- readGDX(gdx, "ov38_investment_mobile", select = list(type = "level"), react = "silent")

      if (!is.null(investmentImmobile)) {
        t <- getYears(labor, as.integer = TRUE)
        tStep <- c(5, t[seq_len(length(t))[-1]] - t[seq_len(length(t) - 1)])
        tSm <- setNames(labor, "t")
        for (y in seq_len(length(getYears(tSm)))) {
          tSm[, y, ] <- tStep[y]
        }
        capital <- (dimSums(investmentImmobile, dim = 3) + dimSums(investmentMobile, dim = 3)) / tSm
        capital <- superAggregateX(capital, aggr_type = "sum", level = "reg")
      } else {
        capital <- collapseDim(factorCosts(gdx, products = products, level = "reg"))[, , "capital_costs"]
      }
    }

    if (!is.null(labor)) {
      x <- setNames(mbind(labor, capital), c("labor_costs", "capital_costs"))
      w <- dimSums(x, dim = 3)
      x <- x / w
      w <- mbind(setNames(w, "labor_costs"), setNames(w, "capital_costs"))
      x <- superAggregateX(x, aggr_type = "weighted_mean", weight = w, level = level)
    } else {
      x <- NULL
    }

  } else {
    stop("Type is not valid.")
  }

  out(x, file)
}
