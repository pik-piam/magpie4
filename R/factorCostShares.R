#' @title factorCostShares
#' @description returns labor and capital cost share out of factor costs (i.e. labor + capital)
#'
#' @export
#'
#' @param gdx GDX file
#' @param type
#' \itemize{
#'   \item "baseline": cost shares from USDA - GDP regression, start point of calculation in MAgPIE
#'   \item "optimization": cost shares between labor and capital costs in optimization
#'   \item "accounting": cost shares based on accounting of labor and capital costs
#' }
#' @param products products for which cost shares should be reported, kcr or kli
#' @param level spatial aggregation to report employment ("reg", "glo" or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @return labor and capital cost share out of factor costs
#' @author Debbora Leip
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- factorCostShares(gdx)
#' }

factorCostShares <- function(gdx, type = "optimization", products = "kcr", level = "reg", file = NULL) {

  if (type == "baseline") {

    if (products == "kcr") {
      x <- readGDX(gdx, "p38_cost_share", react = "silent")
      w <- factorCosts(gdx, products = products, level = "reg")[, , "factor_costs", drop = TRUE]
    } else if (products == "kli") {
      x <- readGDX(gdx, "p70_cost_share_livst", react = "silent")
      w <- factorCosts(gdx, products = products, level = "reg")[, , "factor_costs", drop = TRUE]
    } else {
      stop("Product type not supported")
    }
    if (!is.null(x)) {
      w <- mbind(setNames(w, "labor"), setNames(w, "capital"))
      x <- superAggregate(x, aggr_type = "weighted_mean", weight = w, level = level)
    }

  } else if (type == "optimization") {

    factorCosts <- collapseDim(factorCosts(gdx, products = products, level = "reg"))
    # if labor, capital, and factor costs are reported, shares can be calculated
    if (all(c("labor_costs", "capital_costs", "factor_costs") %in% getNames(factorCosts))) {
      x <- factorCosts[, , c("labor_costs", "capital_costs")] / factorCosts[, , "factor_costs", drop  = TRUE]
      w <- factorCosts[, , "factor_costs"]
      w <- mbind(setNames(w, "labor_costs"), setNames(w, "capital_costs"))
      x <- superAggregate(x, aggr_type = "weighted_mean", weight = w, level = level)
    } else {
      x <- NULL
    }

  } else if (type == "accounting") {

    labor <- collapseDim(factorCosts(gdx, products = products, level = "reg"))[, , "labor_costs"]

    if (products == "kli") {
      capital <- collapseDim(factorCosts(gdx, products = products, level = "reg"))[, , "capital_costs"]
    } else if (products == "kcr") { # accounting of investments only for crops
      t <- getYears(labor, as.integer = TRUE)
      tStep <- c(5, t[seq_len(length(t))[-1]] - t[seq_len(length(t) - 1)])
      tSm <- setNames(labor, "t")
      for (y in seq_len(length(getYears(tSm)))) {
        tSm[, y, ] <- tStep[y]
      }
      capital <- (dimSums(readGDX(gdx, "ov38_investment_immobile", select = list(type = "level")), dim = 3) +
          dimSums(readGDX(gdx, "ov38_investment_mobile", select = list(type = "level")), dim = 3)) / tSm
      capital <- superAggregate(capital, aggr_type = "sum", level = "reg")
    }

    if (!is.null(labor)) {
      x <- setNames(mbind(labor, capital), c("labor_costs", "capital_costs"))
      w <- dimSums(x, dim = 3)
      x <- x / w
      w <- mbind(setNames(w, "labor_costs"), setNames(w, "capital_costs"))
      x <- superAggregate(x, aggr_type = "weighted_mean", weight = w, level = level)
    } else {
      x <- NULL
    }

  } else {
    stop("Type is not valid.")
  }

  out(x, file)
}
